/* qoz_gc: precise mark/sweep with type descriptors.
 *
 * Owns the heap directly. Each call to qoz_gc_alloc returns malloc'd
 * memory and registers the allocation in a side table that records the
 * pointer, its TypeDesc, and a mark bit. Roots come from a shadow stack
 * the compiler emits at every function entry.
 *
 * Auto-collection triggers from qoz_gc_alloc once heap byte usage
 * crosses a growth threshold. Shutdown frees everything regardless.
 */

/* _GNU_SOURCE is defined at the top of every emitted translation
 * unit (see compiler/emit/emit.qoz). It must precede any system
 * header so glibc exposes pthread_getattr_np. Repeating the
 * define here is unnecessary. */

#include "gc.h"

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <setjmp.h>

#ifdef _WIN32
/* GetCurrentThreadStackLimits is Windows 8+. MinGW's windows.h
 * gates it behind _WIN32_WINNT. Pin the floor before the include. */
#  if !defined(_WIN32_WINNT) || _WIN32_WINNT < 0x0602
#    undef _WIN32_WINNT
#    define _WIN32_WINNT 0x0602
#  endif
#  include <windows.h>
#else
#  include <pthread.h>
#endif

/* state: 0 = empty (probe stops), 1 = live, 2 = tombstone (probe continues). */
typedef struct {
    void                       *ptr;
    const qoz_type_desc        *desc;
    int64_t                     size;
    uint8_t                     mark;
    uint8_t                     state;
} qoz_gc_slot;

typedef struct {
    qoz_gc_slot *slots;
    int64_t      nslots;
    int64_t      nlive;
    int64_t      ntomb;
} qoz_gc_table;

static qoz_gc_table g_table  = { NULL, 0, 0, 0 };

/* Heap-usage tracking. qoz_gc_run triggers automatically from
 * qoz_gc_alloc once g_bytes_live crosses g_bytes_threshold. After each
 * sweep, the threshold is reset to max(initial, 2 * live_after_sweep)
 * so steady-state working sets pay one collection per doubling. */
#define QOZ_GC_INITIAL_THRESHOLD (1 << 20)   /* 1 MiB */
static int64_t g_bytes_live      = 0;
static int64_t g_bytes_threshold = QOZ_GC_INITIAL_THRESHOLD;

/* Conservative C-stack scan supplements the shadow stack. Return values
 * from function calls live in registers between the callee return and
 * the caller's qoz_gc_push_root, where the precise scan would miss
 * them. At collection time, setjmp spills callee-saved registers into
 * a jmp_buf on the stack, and the scan walks from the current stack
 * pointer up to the top of the thread's stack region (queried via
 * pthread). The user-supplied anchor in qoz_init is recorded too but
 * the pthread bound is preferred because it covers frames above main
 * (dyld, _start, libc init) that may transit managed pointers during
 * argv setup. */
static void *g_stack_bottom = NULL;
static void *g_stack_top_bound = NULL;

#define ROOT_STACK_INIT 4096
static void   **g_roots     = NULL;
static int64_t  g_roots_top = 0;
static int64_t  g_roots_cap = 0;

static uint64_t hash_ptr(const void *p) {
    uint64_t x = (uint64_t)(uintptr_t)p;
    x ^= x >> 30; x *= 0xbf58476d1ce4e5b9ULL;
    x ^= x >> 27; x *= 0x94d049bb133111ebULL;
    x ^= x >> 31;
    return x;
}

static void table_init_if_needed(void) {
    if (g_table.slots) return;
    g_table.nslots = 1024;
    g_table.slots  = (qoz_gc_slot *)calloc((size_t)g_table.nslots, sizeof(qoz_gc_slot));
    g_table.nlive  = 0;
}

static qoz_gc_slot *find_slot(const void *ptr, int insert) {
    table_init_if_needed();
    int64_t mask = g_table.nslots - 1;
    int64_t i = (int64_t)(hash_ptr(ptr) & (uint64_t)mask);
    qoz_gc_slot *first_tomb = NULL;
    for (int64_t step = 0; step < g_table.nslots; step++) {
        qoz_gc_slot *s = &g_table.slots[(i + step) & mask];
        if (s->state == 0) {
            if (insert) return first_tomb ? first_tomb : s;
            return NULL;
        }
        if (s->state == 2) {
            if (insert && !first_tomb) first_tomb = s;
            continue;
        }
        if (s->ptr == ptr) return s;
    }
    return insert ? first_tomb : NULL;
}

static void table_grow(void) {
    int64_t      old_n     = g_table.nslots;
    qoz_gc_slot *old_slots = g_table.slots;
    g_table.nslots = old_n * 2;
    g_table.slots  = (qoz_gc_slot *)calloc((size_t)g_table.nslots, sizeof(qoz_gc_slot));
    g_table.nlive  = 0;
    g_table.ntomb  = 0;
    for (int64_t i = 0; i < old_n; i++) {
        if (old_slots[i].state == 1) {
            qoz_gc_slot *s = find_slot(old_slots[i].ptr, 1);
            if (s) {
                *s = old_slots[i];
                g_table.nlive++;
            }
        }
    }
    free(old_slots);
}

void *qoz_gc_alloc(int64_t size, const qoz_type_desc *desc) {
    if (g_bytes_live >= g_bytes_threshold) {
        qoz_gc_run();
    }
    void *p = calloc(1, (size_t)size);
    if (!p) return NULL;
    table_init_if_needed();
    if ((g_table.nlive + g_table.ntomb) * 2 >= g_table.nslots) table_grow();
    qoz_gc_slot *s = find_slot(p, 1);
    if (s) {
        uint8_t prev = s->state;
        s->ptr   = p;
        s->desc  = desc;
        s->size  = size;
        s->mark  = 0;
        s->state = 1;
        if (prev == 2) g_table.ntomb--;
        g_table.nlive++;
        g_bytes_live += size;
    }
    return p;
}

const qoz_type_desc *qoz_gc_desc_of(const void *ptr) {
    if (!ptr) return NULL;
    qoz_gc_slot *s = find_slot(ptr, 0);
    return (s && s->state == 1) ? s->desc : NULL;
}

void qoz_gc_push_root(void *root) {
    if (g_roots_top == g_roots_cap) {
        int64_t new_cap = g_roots_cap ? g_roots_cap * 2 : ROOT_STACK_INIT;
        g_roots = (void **)realloc(g_roots, (size_t)new_cap * sizeof(void *));
        g_roots_cap = new_cap;
    }
    g_roots[g_roots_top++] = root;
}

void qoz_gc_pop_roots(int64_t n) {
    g_roots_top -= n;
    if (g_roots_top < 0) g_roots_top = 0;
}

int64_t qoz_gc_shadow_top(void) { return g_roots_top; }

void qoz_gc_shadow_set_top(int64_t top) {
    if (top < 0) top = 0;
    if (top > g_roots_top) return;
    g_roots_top = top;
}

void qoz_gc_restore_shadow(int64_t *base) {
    qoz_gc_shadow_set_top(*base);
}

void qoz_gc_walk_shadow_roots(void (*cb)(void *arg, void *child), void *arg) {
    for (int64_t i = 0; i < g_roots_top; i++) {
        void *slot = g_roots[i];
        if (!slot) continue;
        void *p = *((void **)slot);
        if (p) cb(arg, p);
    }
}

void qoz_gc_clear_marks(void) {
    table_init_if_needed();
    for (int64_t i = 0; i < g_table.nslots; i++) g_table.slots[i].mark = 0;
}

bool qoz_gc_is_marked(const void *ptr) {
    qoz_gc_slot *s = find_slot(ptr, 0);
    return (s && s->state == 1) ? (s->mark != 0) : false;
}

int64_t qoz_gc_total_allocations(void) { return g_table.nlive; }

int qoz_gc_scan_object_callback(void *ptr,
                                void (*cb)(void *arg, void *child),
                                void *arg) {
    qoz_gc_slot *s = find_slot(ptr, 0);
    if (!s || s->state != 1 || !s->desc) return 0;
    const qoz_type_desc *desc = s->desc;
    switch (desc->kind) {
    case QOZ_DESC_LEAF:
        return 1;
    case QOZ_DESC_OFFSETS:
        for (int32_t i = 0; i < desc->nptrs; i++) {
            void *child = *((void **)((char *)ptr + desc->offsets[i]));
            if (child) cb(arg, child);
        }
        return 1;
    case QOZ_DESC_ADT: {
        int32_t tag = *((int32_t *)((char *)ptr + desc->tag_off));
        for (int32_t v = 0; v < desc->nvariants; v++) {
            if (desc->variants[v].tag == tag) {
                void *payload = (char *)ptr + desc->payload_off;
                for (int32_t i = 0; i < desc->variants[v].nptrs; i++) {
                    void *child = *((void **)((char *)payload + desc->variants[v].offsets[i]));
                    if (child) cb(arg, child);
                }
                break;
            }
        }
        return 1;
    }
    case QOZ_DESC_CONSERVATIVE:
    default:
        return 0;
    }
}

typedef struct {
    void  **items;
    int64_t top;
    int64_t cap;
} qoz_gc_mark_stack;

static void mark_stack_push(qoz_gc_mark_stack *ms, void *p) {
    if (ms->top == ms->cap) {
        int64_t new_cap = ms->cap ? ms->cap * 2 : 1024;
        ms->items = (void **)realloc(ms->items, (size_t)new_cap * sizeof(void *));
        ms->cap = new_cap;
    }
    ms->items[ms->top++] = p;
}

static void mark_callback(void *arg, void *child) {
    mark_stack_push((qoz_gc_mark_stack *)arg, child);
}

static void scan_conservative_range(qoz_gc_mark_stack *ms, void *lo, void *hi) {
    if (!lo || !hi) return;
    if (lo > hi) { void *t = lo; lo = hi; hi = t; }
    /* Align lo upward to pointer size. */
    uintptr_t alo = ((uintptr_t)lo + sizeof(void *) - 1) & ~(uintptr_t)(sizeof(void *) - 1);
    void **p = (void **)alo;
    void **e = (void **)hi;
    for (; p < e; p++) {
        void *cand = *p;
        if (!cand) continue;
        qoz_gc_slot *s = find_slot(cand, 0);
        if (s && s->state == 1) mark_stack_push(ms, cand);
    }
}

int64_t qoz_gc_mark_phase(void) {
    qoz_gc_clear_marks();
    qoz_gc_mark_stack ms = { NULL, 0, 0 };
    qoz_gc_walk_shadow_roots(mark_callback, &ms);
    /* Supplement with a conservative scan of the C stack so register-
     * resident return values and call temporaries are not missed. */
    {
        jmp_buf jb;
        (void)setjmp(jb);   /* spills callee-saved registers to jb */
        void *stack_top = (void *)&jb;
        void *upper = g_stack_top_bound ? g_stack_top_bound : g_stack_bottom;
        if (upper) scan_conservative_range(&ms, stack_top, upper);
    }

    int64_t marked = 0;
    while (ms.top > 0) {
        void *obj = ms.items[--ms.top];
        qoz_gc_slot *s = find_slot(obj, 0);
        if (!s || s->state != 1 || s->mark) continue;
        s->mark = 1;
        marked++;
        if (s->desc && s->desc->kind == QOZ_DESC_LEAF) continue;
        if (!qoz_gc_scan_object_callback(obj, mark_callback, &ms)) {
            /* No descriptor: conservative scan of the allocation. */
            int64_t nwords = s->size / (int64_t)sizeof(void *);
            void **words = (void **)obj;
            for (int64_t k = 0; k < nwords; k++) {
                if (words[k]) mark_stack_push(&ms, words[k]);
            }
        }
    }
    free(ms.items);
    return marked;
}

int64_t qoz_gc_sweep_phase(void) {
    int64_t freed = 0;
    table_init_if_needed();
    for (int64_t i = 0; i < g_table.nslots; i++) {
        qoz_gc_slot *s = &g_table.slots[i];
        if (s->state == 1 && !s->mark) {
            g_bytes_live -= s->size;
            free(s->ptr);
            s->ptr   = NULL;
            s->desc  = NULL;
            s->size  = 0;
            s->state = 2;
            g_table.nlive--;
            g_table.ntomb++;
            freed++;
        }
    }
    return freed;
}

int64_t qoz_gc_run(void) {
    qoz_gc_mark_phase();
    int64_t freed = qoz_gc_sweep_phase();
    int64_t doubled = g_bytes_live * 2;
    g_bytes_threshold = doubled > QOZ_GC_INITIAL_THRESHOLD
                        ? doubled : QOZ_GC_INITIAL_THRESHOLD;
    return freed;
}

/* Resolve the high end of the calling thread's stack so the
 * conservative scan in qoz_gc_collect knows where to stop. The
 * APIs are not portable across darwin, glibc, and Windows, so each
 * platform owns its own block. A platform with no resolver leaves
 * g_stack_top_bound NULL and the scan falls back to the anchor
 * passed at qoz_init. */
void qoz_gc_set_stack_bottom(void *anchor) {
    g_stack_bottom = anchor;
#if defined(_WIN32)
    /* Windows reserves the stack as a guarded virtual address
     * range. GetCurrentThreadStackLimits gives the reserve bounds,
     * but pages between the current SP and the high end include
     * guard pages that fault on read. Leave g_stack_top_bound NULL
     * and let the scan fall back to the anchor, which the shadow
     * stack already covers exhaustively. The conservative scan is
     * a supplement for register-resident pointers; the anchor's
     * coverage is sufficient on Windows for now. */
    (void)anchor;
#elif defined(__APPLE__)
    pthread_t self = pthread_self();
    void *addr = pthread_get_stackaddr_np(self);
    /* pthread_get_stackaddr_np returns the address one past the high
     * end of the stack on darwin. Subtract one word to land inside. */
    if (addr) g_stack_top_bound = (char *)addr - sizeof(void *);
#elif defined(__linux__) && defined(__GLIBC__)
    pthread_attr_t attr;
    if (pthread_getattr_np(pthread_self(), &attr) == 0) {
        void *base = NULL;
        size_t sz = 0;
        if (pthread_attr_getstack(&attr, &base, &sz) == 0 && base && sz > 0) {
            /* On Linux pthread_attr_getstack returns the LOW address of
             * the stack region; the high end is base+sz. */
            g_stack_top_bound = (char *)base + sz - sizeof(void *);
        }
        pthread_attr_destroy(&attr);
    }
#endif
}

int64_t qoz_gc_alloc_size(const void *ptr) {
    if (!ptr) return 0;
    qoz_gc_slot *s = find_slot(ptr, 0);
    return (s && s->state == 1) ? s->size : 0;
}

void qoz_gc_free(void *ptr) {
    if (!ptr) return;
    qoz_gc_slot *s = find_slot(ptr, 0);
    if (!s || s->state != 1) return;
    g_bytes_live -= s->size;
    free(s->ptr);
    s->ptr   = NULL;
    s->desc  = NULL;
    s->size  = 0;
    s->mark  = 0;
    s->state = 2;
    g_table.nlive--;
    g_table.ntomb++;
}

/* Free everything (called at process shutdown). */
void qoz_gc_shutdown(void) {
    if (!g_table.slots) return;
    for (int64_t i = 0; i < g_table.nslots; i++) {
        if (g_table.slots[i].state == 1) {
            free(g_table.slots[i].ptr);
            g_table.slots[i].ptr   = NULL;
            g_table.slots[i].state = 0;
        }
    }
    free(g_table.slots);
    g_table.slots  = NULL;
    g_table.nslots = 0;
    g_table.nlive  = 0;
    g_table.ntomb  = 0;
    g_bytes_live   = 0;
    free(g_roots);
    g_roots     = NULL;
    g_roots_top = 0;
    g_roots_cap = 0;
}
