/* embedded runtime: gc.h */
/* qoz_gc: precise mark/sweep collector with type descriptors.
 *
 * Each managed allocation is paired with a `qoz_type_desc *` that the
 * compiler emits when it lays out the type. The descriptor lists
 * pointer-field byte offsets inside the object so the collector can find
 * every outgoing reference without scanning conservatively.
 *
 * Roots come from a shadow stack the compiler maintains: at every
 * function entry pointer-typed locals are registered, at exit they are
 * popped via the __cleanup__ attribute.
 *
 * Auto-collection triggers from qoz_gc_alloc once heap byte usage
 * crosses a growth threshold. Shutdown frees everything regardless.
 */
#ifndef QOZ_GC_H
#define QOZ_GC_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef enum {
    QOZ_DESC_LEAF        = 0,  /* no outgoing pointers */
    QOZ_DESC_OFFSETS     = 1,  /* offsets[] lists pointer-field byte offsets */
    QOZ_DESC_ADT         = 2,  /* tag-dispatched: each variant has its own offset list */
    QOZ_DESC_CONSERVATIVE = 3, /* fall back to scanning every word (avoid when possible) */
} qoz_desc_kind;

typedef struct qoz_variant_desc {
    int32_t        tag;        /* matches the ADT's runtime discriminant */
    int32_t        nptrs;
    const int32_t *offsets;    /* byte offsets within the variant's payload area */
} qoz_variant_desc;

typedef struct qoz_type_desc {
    qoz_desc_kind             kind;
    int32_t                   size;     /* total object size in bytes */
    int32_t                   nptrs;    /* meaningful when kind == OFFSETS */
    const int32_t            *offsets;  /* meaningful when kind == OFFSETS */
    int32_t                   tag_off;  /* offset of the tag field; meaningful when kind == ADT */
    int32_t                   payload_off; /* offset of the payload union; meaningful when kind == ADT */
    int32_t                   nvariants;
    const qoz_variant_desc   *variants;
    const char               *name;     /* debug only */
} qoz_type_desc;

/* Allocate `size` bytes; record `desc` against the allocation so future
 * mark walks can find outgoing pointers. `desc` may be NULL, in which
 * case the allocation is treated as opaque (conservative scan fallback).
 */
void *qoz_gc_alloc(int64_t size, const qoz_type_desc *desc);

/* Look up the descriptor associated with an allocation. Returns NULL
 * when the pointer is not a known managed allocation or no descriptor
 * was registered. */
const qoz_type_desc *qoz_gc_desc_of(const void *ptr);

/* Shadow-stack root operations. The compiler emits push at every fn
 * entry for each pointer-typed local and pop at exit. */
void qoz_gc_push_root(void *root);
void qoz_gc_pop_roots(int64_t n);

/* Snapshot / restore the shadow-stack top. emit_fn declares a scoped
 * variable holding the entry snapshot and registers `qoz_gc_restore_shadow`
 * as a __cleanup__ attribute so every exit path (including early
 * returns) restores automatically. */
int64_t qoz_gc_shadow_top(void);
void    qoz_gc_shadow_set_top(int64_t top);
void    qoz_gc_restore_shadow(int64_t *base);

/* Walk all shadow-stack roots and invoke the callback for each non-NULL
 * pointer found at `*root_slot`. */
void qoz_gc_walk_shadow_roots(void (*cb)(void *arg, void *child),
                              void *arg);

/* Run a precise mark phase walking the shadow stack. Each reachable
 * allocation gets its mark bit set; everything else is left unmarked.
 * Returns the number of allocations marked. */
int64_t qoz_gc_mark_phase(void);

/* Free every unmarked allocation. Returns the number of allocations
 * freed. Call after qoz_gc_mark_phase. */
int64_t qoz_gc_sweep_phase(void);

/* Mark + sweep in one call. Returns the number of allocations freed. */
int64_t qoz_gc_run(void);

/* Record the bottom of the C stack (called once from qoz_init with the
 * address of a local in main). The conservative supplement scans from
 * the current top up to this anchor at every collection. */
void qoz_gc_set_stack_bottom(void *anchor);

/* Free all remaining allocations (process-shutdown hook). */
void qoz_gc_shutdown(void);

/* Allocation byte-size as recorded at qoz_gc_alloc time. Returns 0
 * for pointers not registered with the GC. */
int64_t qoz_gc_alloc_size(const void *ptr);

/* Explicitly free a single allocation (used by qoz_realloc to retire
 * the old buffer). Safe on already-freed or unknown pointers. */
void qoz_gc_free(void *ptr);

/* Diagnostic: total number of managed allocations currently registered
 * with the precise GC (used by the self-test). */
int64_t qoz_gc_total_allocations(void);

/* Diagnostic: reset all mark bits to 0. Called by the test harness
 * before counting reachability. */
void qoz_gc_clear_marks(void);

/* True when the allocation is currently marked. */
bool qoz_gc_is_marked(const void *ptr);

/* Walk the outgoing pointers of `ptr` using its registered descriptor,
 * invoking `cb(arg, child)` for each non-NULL pointer field. Returns
 * 1 when the scan happened (caller should skip conservative fallback),
 * 0 when no descriptor was registered. */
int qoz_gc_scan_object_callback(void *ptr,
                                void (*cb)(void *arg, void *child),
                                void *arg);

#endif

/* embedded runtime: qoz_runtime.h */
#ifndef QOZ_RUNTIME_H
#define QOZ_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <math.h>

typedef struct {
    const char *data;
    int64_t len;
    /* Allocation-start pointer that keeps the data buffer reachable
     * through tgc's exact-match scan. Equal to data for fresh allocations,
     * inherited from the source for slices, NULL for static literals. */
    const char *root;
} qoz_string;

void qoz_init(int *stack_anchor);
void qoz_shutdown(void);

/* Allocation */
void *qoz_alloc(int64_t size);
void *qoz_calloc(int64_t size);
void *qoz_realloc(void *ptr, int64_t size);

/* String helpers */
bool     qoz_string_eq(qoz_string a, qoz_string b);
uint64_t qoz_string_hash(qoz_string s);
bool     qoz_string_has_prefix(qoz_string s, qoz_string p);
bool     qoz_string_has_suffix(qoz_string s, qoz_string suf);
int64_t  qoz_string_index_byte(qoz_string s, int64_t byte);
int64_t  qoz_string_byte_at(qoz_string s, int64_t i);
qoz_string qoz_string_cat(qoz_string a, qoz_string b);
qoz_string qoz_string_slice(qoz_string s, int64_t from, int64_t to);
int64_t  qoz_string_parse_int(qoz_string s);

/* OS bridge */
void       qoz_set_argv(int argc, char **argv);
int64_t    qoz_os_argc(void);
qoz_string qoz_os_arg(int64_t i);
void       qoz_os_exit(int64_t code);
qoz_string qoz_os_getenv(qoz_string name);

/* Unrecoverable abort. Prints a one-line diagnostic to stderr (the
 * message, the source location if the call site provided one, and a
 * fixed prefix) and calls abort(). Intended for programmer bugs:
 * invariants that "cannot" fail, exhausted match arms reaching the
 * default, integer overflow on counter limits, etc. Expected errors
 * flow through Result<T, E> with the `try` operator; panic is not
 * recoverable. */
_Noreturn void qoz_panic(qoz_string msg);

/* File system */
qoz_string qoz_fs_read_file(qoz_string path);
bool       qoz_fs_write_file(qoz_string path, qoz_string content);
bool       qoz_fs_file_exists(qoz_string path);
/* List every regular file in `dir` whose name ends in ".qoz", sorted
 * by filename. Returns a single string with the filenames separated by
 * newlines (no trailing newline). Returns an empty string if the
 * directory cannot be opened. */
qoz_string qoz_fs_list_qoz_files(qoz_string dir);

/* Opaque-pointer access to a string's internal data pointer. Used by
 * Qoz code that needs to memcpy string bytes into a raw buffer (e.g.
 * std/strings/Strbuf::sb_append). */
void      *qoz_string_data(qoz_string s);

/* Construct a string that aliases the bytes at `buf` for `n` bytes.
 * The `root` field of the returned qoz_string is `buf`, so the GC
 * keeps the allocation reachable as long as the string is reachable.
 * `buf` must point at a GC-managed allocation. */
qoz_string qoz_string_alias(void *buf, int64_t n);

/* Copy `n` bytes from `src` to `dst`. Wrapper over libc memcpy so Qoz
 * can call it portably; on darwin memcpy is a fortify-source macro and
 * cannot be extern-declared by Qoz directly. */
void       qoz_bytes_copy(void *dst, void *src, int64_t n);

/* Print primitives. The generated code emits a sequence of these per
 * `fmt.println(args...)` call: one print per argument, separators in between,
 * and a newline at the end.
 */
void qoz_print_str(qoz_string s);
void qoz_print_cstr(const char *s);
void qoz_print_i64(int64_t v);
void qoz_print_i32(int32_t v);
void qoz_print_f64(double v);
void qoz_print_bool(bool v);
void qoz_print_sep(void);
void qoz_print_nl(void);
/* Print a string followed by a newline and flush stdout. Backs the
 * `fmt.println` stdlib function. */
void qoz_print_line(qoz_string s);

/* Format builder */
typedef struct { char *buf; int64_t len; int64_t cap; } qoz_strbuf;
void       qoz_strbuf_init(void *b);
void       qoz_strbuf_append_str(void *b, qoz_string s);
void       qoz_strbuf_append_cstr(void *b, const char *s);
void       qoz_strbuf_append_i64(void *b, int64_t v);
void       qoz_strbuf_append_f64(void *b, double v);
void       qoz_strbuf_append_bool(void *b, bool v);
qoz_string qoz_strbuf_finish(void *b);

#define QOZ_STR_LIT(s) ((qoz_string){ (s), (int64_t)(sizeof(s) - 1) })

/* Fork+exec a child, capture its stdout and stderr into Qoz strings,
 * and report the child's exit status. `argv` points to an array of `n`
 * qoz_string values; the first is the program to launch, the rest are
 * its arguments. The output pointers must point to valid storage; on
 * return they hold the captured streams. Exit code is the process's
 * raw exit value, or -1 if the spawn itself failed. */
void qoz_process_exec(qoz_string *argv, int64_t n,
                      int64_t *out_exit,
                      qoz_string *out_stdout,
                      qoz_string *out_stderr);

#endif

/* embedded runtime: gc.c */
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


#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <setjmp.h>
#include <pthread.h>

/* state: 0 = empty (probe stops); 1 = live; 2 = tombstone (probe continues) */
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

void qoz_gc_set_stack_bottom(void *anchor) {
    g_stack_bottom = anchor;
    pthread_t self = pthread_self();
    void *addr = pthread_get_stackaddr_np(self);
    size_t sz   = pthread_get_stacksize_np(self);
    /* pthread_get_stackaddr_np returns the address one past the high end
     * of the stack on darwin; subtract one word to land inside. */
    (void)sz;
    if (addr) g_stack_top_bound = (char *)addr - sizeof(void *);
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

/* embedded runtime: qoz_runtime.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <dirent.h>
#include <sys/types.h>

static int qoz_argc_val = 0;
static char **qoz_argv_val = NULL;

void qoz_set_argv(int argc, char **argv) {
    qoz_argc_val = argc;
    qoz_argv_val = argv;
}

int64_t qoz_os_argc(void) { return (int64_t)qoz_argc_val; }

qoz_string qoz_os_arg(int64_t i) {
    if (i < 0 || i >= (int64_t)qoz_argc_val) return (qoz_string){ NULL, 0 };
    const char *s = qoz_argv_val[i];
    return (qoz_string){ s, (int64_t)strlen(s) };
}

void qoz_os_exit(int64_t code) { exit((int)code); }

void qoz_panic(qoz_string msg) {
    fputs("qoz: panic: ", stderr);
    if (msg.len > 0) fwrite(msg.data, 1, (size_t)msg.len, stderr);
    fputc('\n', stderr);
    fflush(stderr);
    abort();
}

qoz_string qoz_os_getenv(qoz_string name) {
    char buf[1024];
    if (name.len < 0 || (size_t)name.len >= sizeof(buf)) return (qoz_string){ NULL, 0 };
    memcpy(buf, name.data, (size_t)name.len);
    buf[name.len] = 0;
    const char *v = getenv(buf);
    if (!v) return (qoz_string){ NULL, 0 };
    return (qoz_string){ v, (int64_t)strlen(v) };
}

static int qoz_copy_path_nul(qoz_string path, char *buf, size_t buflen) {
    if (path.len < 0 || (size_t)path.len >= buflen) return 0;
    memcpy(buf, path.data, (size_t)path.len);
    buf[path.len] = 0;
    return 1;
}

qoz_string qoz_fs_read_file(qoz_string path) {
    char buf[4096];
    if (!qoz_copy_path_nul(path, buf, sizeof(buf))) return (qoz_string){ NULL, -1 };
    FILE *f = fopen(buf, "rb");
    if (!f) return (qoz_string){ NULL, -1 };
    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return (qoz_string){ NULL, -1 }; }
    long n = ftell(f);
    if (n < 0) { fclose(f); return (qoz_string){ NULL, -1 }; }
    if (fseek(f, 0, SEEK_SET) != 0) { fclose(f); return (qoz_string){ NULL, -1 }; }
    char *data = (char *)qoz_alloc((int64_t)n);
    size_t got = fread(data, 1, (size_t)n, f);
    fclose(f);
    return (qoz_string){ data, (int64_t)got, data };
}

bool qoz_fs_file_exists(qoz_string path) {
    char buf[4096];
    if (!qoz_copy_path_nul(path, buf, sizeof(buf))) return false;
    FILE *f = fopen(buf, "rb");
    if (!f) return false;
    fclose(f);
    return true;
}

void *qoz_string_data(qoz_string s) {
    return (void *)s.data;
}

qoz_string qoz_string_alias(void *buf, int64_t n) {
    return (qoz_string){ (const char *)buf, n, (const char *)buf };
}

void qoz_bytes_copy(void *dst, void *src, int64_t n) {
    if (n > 0) memcpy(dst, src, (size_t)n);
}

bool qoz_fs_write_file(qoz_string path, qoz_string content) {
    char buf[4096];
    if (!qoz_copy_path_nul(path, buf, sizeof(buf))) return false;
    FILE *f = fopen(buf, "wb");
    if (!f) return false;
    size_t wrote = 0;
    if (content.len > 0) wrote = fwrite(content.data, 1, (size_t)content.len, f);
    fclose(f);
    return wrote == (size_t)content.len;
}

static int qoz_str_lex_less(const void *a, const void *b) {
    return strcmp(*(const char *const *)a, *(const char *const *)b);
}

qoz_string qoz_fs_list_qoz_files(qoz_string dir) {
    char path[4096];
    if (!qoz_copy_path_nul(dir, path, sizeof(path))) return (qoz_string){ NULL, 0 };
    DIR *d = opendir(path);
    if (!d) return (qoz_string){ NULL, 0 };

    const char **names = NULL;
    int64_t count = 0;
    int64_t cap = 0;

    struct dirent *ent;
    while ((ent = readdir(d)) != NULL) {
        const char *name = ent->d_name;
        size_t nlen = strlen(name);
        if (nlen < 4) continue;
        if (memcmp(name + nlen - 4, ".qoz", 4) != 0) continue;
        if (count == cap) {
            cap = cap == 0 ? 8 : cap * 2;
            names = (const char **)realloc((void *)names, (size_t)cap * sizeof(*names));
        }
        char *dup = (char *)malloc(nlen + 1);
        memcpy(dup, name, nlen + 1);
        names[count++] = dup;
    }
    closedir(d);

    if (count == 0) {
        free((void *)names);
        return (qoz_string){ NULL, 0 };
    }
    qsort(names, (size_t)count, sizeof(*names), qoz_str_lex_less);

    int64_t total = 0;
    for (int64_t i = 0; i < count; i++) total += (int64_t)strlen(names[i]);
    total += count - 1;

    char *out = (char *)qoz_alloc(total);
    int64_t off = 0;
    for (int64_t i = 0; i < count; i++) {
        if (i > 0) { out[off++] = '\n'; }
        size_t nlen = strlen(names[i]);
        memcpy(out + off, names[i], nlen);
        off += (int64_t)nlen;
        free((void *)names[i]);
    }
    free((void *)names);
    return (qoz_string){ out, total, out };
}

bool qoz_string_has_prefix(qoz_string s, qoz_string p) {
    if (p.len > s.len) return false;
    return memcmp(s.data, p.data, (size_t)p.len) == 0;
}

bool qoz_string_has_suffix(qoz_string s, qoz_string suf) {
    if (suf.len > s.len) return false;
    return memcmp(s.data + s.len - suf.len, suf.data, (size_t)suf.len) == 0;
}

qoz_string qoz_string_cat(qoz_string a, qoz_string b) {
    int64_t n = (a.len < 0 ? 0 : a.len) + (b.len < 0 ? 0 : b.len);
    char *buf = (char *)qoz_alloc(n);
    if (a.len > 0) memcpy(buf,         a.data, (size_t)a.len);
    if (b.len > 0) memcpy(buf + a.len, b.data, (size_t)b.len);
    return (qoz_string){ buf, n, buf };
}

int64_t qoz_string_byte_at(qoz_string s, int64_t i) {
    if (i < 0 || i >= s.len) return -1;
    return (int64_t)(unsigned char)s.data[i];
}

int64_t qoz_string_index_byte(qoz_string s, int64_t byte) {
    for (int64_t i = 0; i < s.len; i++) {
        if ((unsigned char)s.data[i] == (unsigned char)byte) return i;
    }
    return -1;
}

qoz_string qoz_string_slice(qoz_string s, int64_t from, int64_t to) {
    if (from < 0) from = 0;
    if (to > s.len) to = s.len;
    if (from > to) from = to;
    return (qoz_string){ s.data + from, to - from, s.root };
}

void qoz_strbuf_init(void *bv) {
    qoz_strbuf *b = (qoz_strbuf *)bv;
    b->buf = NULL;
    b->len = 0;
    b->cap = 0;
}

static void qoz_strbuf_grow(qoz_strbuf *b, int64_t needed) {
    int64_t new_cap = b->cap == 0 ? 64 : b->cap;
    while (new_cap < b->len + needed) new_cap *= 2;
    b->buf = (char *)qoz_realloc(b->buf, new_cap);
    b->cap = new_cap;
}

void qoz_strbuf_append_str(void *bv, qoz_string s) {
    qoz_strbuf *b = (qoz_strbuf *)bv;
    if (s.len <= 0) return;
    if (b->len + s.len > b->cap) qoz_strbuf_grow(b, s.len);
    memcpy(b->buf + b->len, s.data, (size_t)s.len);
    b->len += s.len;
}

void qoz_strbuf_append_cstr(void *bv, const char *s) {
    qoz_string ss = { s, (int64_t)strlen(s) };
    qoz_strbuf_append_str(bv, ss);
}

void qoz_strbuf_append_i64(void *bv, int64_t v) {
    qoz_strbuf *b = (qoz_strbuf *)bv;
    char tmp[32];
    int n = snprintf(tmp, sizeof(tmp), "%" PRId64, v);
    if (n > 0) {
        if (b->len + n > b->cap) qoz_strbuf_grow(b, n);
        memcpy(b->buf + b->len, tmp, (size_t)n);
        b->len += n;
    }
}

void qoz_strbuf_append_f64(void *bv, double v) {
    qoz_strbuf *b = (qoz_strbuf *)bv;
    char tmp[64];
    int n = snprintf(tmp, sizeof(tmp), "%g", v);
    if (n > 0) {
        if (b->len + n > b->cap) qoz_strbuf_grow(b, n);
        memcpy(b->buf + b->len, tmp, (size_t)n);
        b->len += n;
    }
}

void qoz_strbuf_append_bool(void *bv, bool v) {
    qoz_string s = v ? (qoz_string){ "true", 4 } : (qoz_string){ "false", 5 };
    qoz_strbuf_append_str(bv, s);
}

qoz_string qoz_strbuf_finish(void *bv) {
    qoz_strbuf *b = (qoz_strbuf *)bv;
    return (qoz_string){ b->buf, b->len, b->buf };
}

int64_t qoz_string_parse_int(qoz_string s) {
    char buf[64];
    if (s.len < 0 || (size_t)s.len >= sizeof(buf)) return 0;
    memcpy(buf, s.data, (size_t)s.len);
    buf[s.len] = 0;
    return (int64_t)strtoll(buf, NULL, 10);
}

void qoz_init(int *stack_anchor) {
    /* gc.c owns the heap and auto-collects from qoz_gc_alloc once the
     * live-byte threshold is crossed. The shadow stack registers every
     * pointer-typed parameter and local; a conservative C-stack scan
     * supplements that so register-resident return values are reached
     * during the mark phase. */
    qoz_gc_set_stack_bottom(stack_anchor);
}

void qoz_shutdown(void) {
    qoz_gc_shutdown();
}

void *qoz_alloc(int64_t size) {
    /* Used by string / Vec / Map data buffers and other arrays that do
     * not carry a per-element type descriptor. Tracked by gc.c so the
     * shutdown sweep frees them. */
    return qoz_gc_alloc(size, NULL);
}

void *qoz_calloc(int64_t size) {
    /* qoz_gc_alloc already zero-fills. */
    return qoz_gc_alloc(size, NULL);
}

void *qoz_realloc(void *ptr, int64_t size) {
    if (ptr == NULL) return qoz_gc_alloc(size, NULL);
    /* For now treat realloc as alloc-and-copy. The GC tracks the new
     * allocation; the old one will be freed by the next sweep (or at
     * shutdown). Vec/Map are the common callers and they always retain
     * a reference to the new pointer, so this is safe. */
    int64_t old_size = qoz_gc_alloc_size(ptr);
    void *p = qoz_gc_alloc(size, NULL);
    if (p != NULL && old_size > 0) {
        int64_t copy = old_size < size ? old_size : size;
        memcpy(p, ptr, (size_t)copy);
    }
    qoz_gc_free(ptr);
    return p;
}

bool qoz_string_eq(qoz_string a, qoz_string b) {
    if (a.len != b.len) return false;
    if (a.len == 0) return true;
    return memcmp(a.data, b.data, (size_t)a.len) == 0;
}

uint64_t qoz_string_hash(qoz_string s) {
    /* FNV-1a 64-bit. */
    uint64_t h = 14695981039346656037ULL;
    for (int64_t i = 0; i < s.len; i++) {
        h ^= (uint8_t)s.data[i];
        h *= 1099511628211ULL;
    }
    return h;
}

void qoz_print_str(qoz_string s) {
    fwrite(s.data, 1, (size_t)s.len, stdout);
}

void qoz_print_cstr(const char *s) {
    fputs(s, stdout);
}

void qoz_print_i64(int64_t v) {
    printf("%" PRId64, v);
}

void qoz_print_i32(int32_t v) {
    printf("%" PRId32, v);
}

void qoz_print_f64(double v) {
    printf("%g", v);
}

void qoz_print_bool(bool v) {
    fputs(v ? "true" : "false", stdout);
}

void qoz_print_sep(void) {
    fputc(' ', stdout);
}

void qoz_print_nl(void) {
    fputc('\n', stdout);
    fflush(stdout);
}

void qoz_print_line(qoz_string s) {
    if (s.len > 0) fwrite(s.data, 1, (size_t)s.len, stdout);
    fputc('\n', stdout);
    fflush(stdout);
}

#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>

/* Drain a file descriptor into a newly-allocated qoz_string. The pipe
 * is read in chunks; the buffer doubles when full. Returns a string
 * whose `len` is the byte count and whose `data` and `root` point to a
 * GC allocation, so the returned value stays reachable while in scope. */
static qoz_string qoz_drain_fd(int fd) {
    int64_t cap = 4096;
    char *buf = (char *)qoz_alloc(cap);
    int64_t n = 0;
    for (;;) {
        if (n + 1024 > cap) {
            int64_t new_cap = cap * 2;
            char *nb = (char *)qoz_alloc(new_cap);
            memcpy(nb, buf, (size_t)n);
            buf = nb;
            cap = new_cap;
        }
        ssize_t got = read(fd, buf + n, (size_t)(cap - n));
        if (got <= 0) break;
        n += (int64_t)got;
    }
    close(fd);
    return (qoz_string){ buf, n, buf };
}

void qoz_process_exec(qoz_string *argv, int64_t n,
                      int64_t *out_exit,
                      qoz_string *out_stdout,
                      qoz_string *out_stderr) {
    *out_exit = -1;
    *out_stdout = (qoz_string){ NULL, 0 };
    *out_stderr = (qoz_string){ NULL, 0 };

    if (n <= 0 || argv == NULL) return;

    /* Build a NUL-terminated char** for execvp from the input qoz_string
     * array. Strings inside argv may not be NUL-terminated (they can
     * alias slices), so each one is copied into its own buffer. */
    char **cargv = (char **)qoz_alloc((int64_t)((n + 1) * (int64_t)sizeof(char *)));
    for (int64_t i = 0; i < n; i++) {
        int64_t len = argv[i].len;
        char *s = (char *)qoz_alloc(len + 1);
        if (len > 0) memcpy(s, argv[i].data, (size_t)len);
        s[len] = '\0';
        cargv[i] = s;
    }
    cargv[n] = NULL;

    int out_pipe[2] = { -1, -1 };
    int err_pipe[2] = { -1, -1 };
    if (pipe(out_pipe) != 0) return;
    if (pipe(err_pipe) != 0) { close(out_pipe[0]); close(out_pipe[1]); return; }

    pid_t pid = fork();
    if (pid < 0) {
        close(out_pipe[0]); close(out_pipe[1]);
        close(err_pipe[0]); close(err_pipe[1]);
        return;
    }
    if (pid == 0) {
        /* Child. */
        dup2(out_pipe[1], 1);
        dup2(err_pipe[1], 2);
        close(out_pipe[0]); close(out_pipe[1]);
        close(err_pipe[0]); close(err_pipe[1]);
        execvp(cargv[0], cargv);
        /* If exec fails the child exits 127, matching POSIX convention
         * for "command not found". */
        _exit(127);
    }

    /* Parent. Close the write ends; read until EOF so the child can
     * make progress when its pipes are full. */
    close(out_pipe[1]);
    close(err_pipe[1]);
    *out_stdout = qoz_drain_fd(out_pipe[0]);
    *out_stderr = qoz_drain_fd(err_pipe[0]);

    int status = 0;
    while (waitpid(pid, &status, 0) < 0) {
        if (errno != EINTR) { *out_exit = -1; return; }
    }
    if (WIFEXITED(status)) {
        *out_exit = (int64_t)WEXITSTATUS(status);
    } else {
        *out_exit = -1;
    }
}

/* user code */
typedef struct qoz_Vec__qoz_string qoz_Vec__qoz_string;
typedef struct qoz_Map__qoz_string__bool qoz_Map__qoz_string__bool;
typedef struct qoz_Vec__qoz_Expr qoz_Vec__qoz_Expr;
typedef struct qoz_Vec__qoz_RecordFieldLit qoz_Vec__qoz_RecordFieldLit;
typedef struct qoz_Vec__qoz_Stmt qoz_Vec__qoz_Stmt;
typedef struct qoz_Vec__qoz_MatchArm qoz_Vec__qoz_MatchArm;
typedef struct qoz_Vec__qoz_Pending qoz_Vec__qoz_Pending;
typedef struct qoz_Vec__qoz_Decl qoz_Vec__qoz_Decl;
typedef struct qoz_Vec__qoz_Token qoz_Vec__qoz_Token;
typedef struct qoz_Vec__qoz_TypeExpr qoz_Vec__qoz_TypeExpr;
typedef struct qoz_Vec__qoz_Pattern qoz_Vec__qoz_Pattern;
typedef struct qoz_Vec__qoz_ClosureParam qoz_Vec__qoz_ClosureParam;
typedef struct qoz_Vec__qoz_StructField qoz_Vec__qoz_StructField;
typedef struct qoz_Vec__qoz_FnParam qoz_Vec__qoz_FnParam;
typedef struct qoz_Vec__qoz_VariantDecl qoz_Vec__qoz_VariantDecl;
typedef struct qoz_Map__qoz_string__qoz_Decl qoz_Map__qoz_string__qoz_Decl;
typedef struct qoz_Map__qoz_string__qoz_string qoz_Map__qoz_string__qoz_string;
typedef struct qoz_Vec__qoz_TypeError qoz_Vec__qoz_TypeError;
typedef struct qoz_Map__int64_t__qoz_TypeExpr qoz_Map__int64_t__qoz_TypeExpr;
typedef struct qoz_Vec__qoz_Ty qoz_Vec__qoz_Ty;
typedef struct qoz_Vec__qoz_Binding qoz_Vec__qoz_Binding;
typedef struct qoz_Map__qoz_string__qoz_Ty qoz_Map__qoz_string__qoz_Ty;
typedef struct qoz_Vec__qoz_Instantiation qoz_Vec__qoz_Instantiation;
typedef struct qoz_Map__qoz_string__qoz_TypeExpr qoz_Map__qoz_string__qoz_TypeExpr;
typedef struct qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr;
typedef struct qoz_Vec__qoz_Vec__qoz_StructField qoz_Vec__qoz_Vec__qoz_StructField;
typedef struct qoz_Slot__qoz_string__bool qoz_Slot__qoz_string__bool;
typedef struct qoz_Slot__qoz_string__qoz_Decl qoz_Slot__qoz_string__qoz_Decl;
typedef struct qoz_Slot__qoz_string__qoz_string qoz_Slot__qoz_string__qoz_string;
typedef struct qoz_Slot__int64_t__qoz_TypeExpr qoz_Slot__int64_t__qoz_TypeExpr;
typedef struct qoz_Slot__qoz_string__qoz_Ty qoz_Slot__qoz_string__qoz_Ty;
typedef struct qoz_Slot__qoz_string__qoz_TypeExpr qoz_Slot__qoz_string__qoz_TypeExpr;
typedef struct qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr;
typedef struct qoz_Option__qoz_TypeExpr qoz_Option__qoz_TypeExpr;
typedef struct qoz_Option__qoz_string qoz_Option__qoz_string;
typedef struct qoz_Option__bool qoz_Option__bool;
typedef struct qoz_Option__qoz_Decl qoz_Option__qoz_Decl;
typedef struct qoz_Option__qoz_Ty qoz_Option__qoz_Ty;
typedef struct qoz_Option__qoz_Vec__qoz_TypeExpr qoz_Option__qoz_Vec__qoz_TypeExpr;

typedef struct qoz_Pending qoz_Pending;
typedef struct qoz_Loaded qoz_Loaded;
typedef struct qoz_ProcessResult qoz_ProcessResult;
typedef struct qoz_Strbuf qoz_Strbuf;
typedef struct qoz_TokenKind qoz_TokenKind;
typedef struct qoz_Token qoz_Token;
typedef struct qoz_Lexer qoz_Lexer;
typedef struct qoz_Span qoz_Span;
typedef struct qoz_UnaryOp qoz_UnaryOp;
typedef struct qoz_BinaryOp qoz_BinaryOp;
typedef struct qoz_AssignOp qoz_AssignOp;
typedef struct qoz_TypeExpr qoz_TypeExpr;
typedef struct qoz_Pattern qoz_Pattern;
typedef struct qoz_ClosureParam qoz_ClosureParam;
typedef struct qoz_RecordFieldLit qoz_RecordFieldLit;
typedef struct qoz_MatchArm qoz_MatchArm;
typedef struct qoz_Expr qoz_Expr;
typedef struct qoz_Stmt qoz_Stmt;
typedef struct qoz_FnParam qoz_FnParam;
typedef struct qoz_StructField qoz_StructField;
typedef struct qoz_VariantPayloadKind qoz_VariantPayloadKind;
typedef struct qoz_VariantDecl qoz_VariantDecl;
typedef struct qoz_LinkKind qoz_LinkKind;
typedef struct qoz_Attribute qoz_Attribute;
typedef struct qoz_Decl qoz_Decl;
typedef struct qoz_File qoz_File;
typedef struct qoz_Parser qoz_Parser;
typedef struct qoz_ParseOutput qoz_ParseOutput;
typedef struct qoz_TypeError qoz_TypeError;
typedef struct qoz_TyContext qoz_TyContext;
typedef struct qoz_Binding qoz_Binding;
typedef struct qoz_Env qoz_Env;
typedef struct qoz_CoverSet qoz_CoverSet;
typedef struct qoz_Instantiation qoz_Instantiation;
typedef struct qoz_Emitter qoz_Emitter;
typedef struct qoz_CaptureScope qoz_CaptureScope;
typedef struct qoz_StmtScope qoz_StmtScope;
typedef struct qoz_MainRetKind qoz_MainRetKind;
typedef struct qoz_IntInfo qoz_IntInfo;
typedef struct qoz_FloatInfo qoz_FloatInfo;
typedef struct qoz_Ty qoz_Ty;

struct qoz_Pending {
    qoz_string path;
    qoz_string pkg;
};

static bool qoz_eq_Pending(qoz_Pending a, qoz_Pending b) {
    return qoz_string_eq(a.path, b.path) && qoz_string_eq(a.pkg, b.pkg);
}

static uint64_t qoz_hash_Pending(qoz_Pending v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.path));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.pkg));
    return h;
}

static const int32_t qoz_Pending_offsets[] = { (int32_t)(offsetof(struct qoz_Pending, path) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Pending, path) + offsetof(qoz_string, root)), (int32_t)(offsetof(struct qoz_Pending, pkg) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Pending, pkg) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_Pending_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Pending), 4, qoz_Pending_offsets, 0, 0, 0, NULL, "Pending" };

struct qoz_Vec__qoz_Decl {
    qoz_Decl** data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Decl(qoz_Vec__qoz_Decl a, qoz_Vec__qoz_Decl b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Decl(qoz_Vec__qoz_Decl v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Decl_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Decl, data) };
static const qoz_type_desc qoz_Vec__qoz_Decl_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Decl), 1, qoz_Vec__qoz_Decl_offsets, 0, 0, 0, NULL, "Vec__qoz_Decl" };

struct qoz_File {
    qoz_string path;
    qoz_Vec__qoz_Decl decls;
};

static bool qoz_eq_File(qoz_File a, qoz_File b) {
    return qoz_string_eq(a.path, b.path) && qoz_eq_Vec__qoz_Decl(a.decls, b.decls);
}

static uint64_t qoz_hash_File(qoz_File v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.path));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_Decl(v.decls));
    return h;
}

static const int32_t qoz_File_offsets[] = { (int32_t)(offsetof(struct qoz_File, path) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_File, path) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_File, decls) };
static const qoz_type_desc qoz_File_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_File), 3, qoz_File_offsets, 0, 0, 0, NULL, "File" };

struct qoz_Vec__qoz_string {
    qoz_string* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_string(qoz_Vec__qoz_string a, qoz_Vec__qoz_string b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_string(qoz_Vec__qoz_string v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_string_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_string, data) };
static const qoz_type_desc qoz_Vec__qoz_string_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_string), 1, qoz_Vec__qoz_string_offsets, 0, 0, 0, NULL, "Vec__qoz_string" };

struct qoz_Loaded {
    qoz_File file;
    qoz_Vec__qoz_string sources;
};

static bool qoz_eq_Loaded(qoz_Loaded a, qoz_Loaded b) {
    return qoz_eq_File(a.file, b.file) && qoz_eq_Vec__qoz_string(a.sources, b.sources);
}

static uint64_t qoz_hash_Loaded(qoz_Loaded v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_File(v.file));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.sources));
    return h;
}

static const int32_t qoz_Loaded_offsets[] = { (int32_t)(offsetof(struct qoz_Loaded, file) + (offsetof(struct qoz_File, path) + offsetof(qoz_string, data))), (int32_t)(offsetof(struct qoz_Loaded, file) + (offsetof(struct qoz_File, path) + offsetof(qoz_string, root))), (int32_t)(offsetof(struct qoz_Loaded, file) + offsetof(struct qoz_File, decls)), (int32_t)offsetof(struct qoz_Loaded, sources) };
static const qoz_type_desc qoz_Loaded_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Loaded), 4, qoz_Loaded_offsets, 0, 0, 0, NULL, "Loaded" };

struct qoz_ProcessResult {
    int64_t exit_code;
    qoz_string stdout;
    qoz_string stderr;
};

static bool qoz_eq_ProcessResult(qoz_ProcessResult a, qoz_ProcessResult b) {
    return a.exit_code == b.exit_code && qoz_string_eq(a.stdout, b.stdout) && qoz_string_eq(a.stderr, b.stderr);
}

static uint64_t qoz_hash_ProcessResult(qoz_ProcessResult v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.exit_code);
    h = h * 31 + (uint64_t)(qoz_string_hash(v.stdout));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.stderr));
    return h;
}

static const int32_t qoz_ProcessResult_offsets[] = { (int32_t)(offsetof(struct qoz_ProcessResult, stdout) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_ProcessResult, stdout) + offsetof(qoz_string, root)), (int32_t)(offsetof(struct qoz_ProcessResult, stderr) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_ProcessResult, stderr) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_ProcessResult_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_ProcessResult), 4, qoz_ProcessResult_offsets, 0, 0, 0, NULL, "ProcessResult" };

struct qoz_Strbuf {
    uint8_t* buf;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Strbuf(qoz_Strbuf a, qoz_Strbuf b) {
    return a.buf == b.buf && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Strbuf(qoz_Strbuf v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.buf);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Strbuf_offsets[] = { (int32_t)offsetof(struct qoz_Strbuf, buf) };
static const qoz_type_desc qoz_Strbuf_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Strbuf), 1, qoz_Strbuf_offsets, 0, 0, 0, NULL, "Strbuf" };

struct qoz_Token {
    qoz_TokenKind* kind;
    qoz_string text;
    int64_t line;
    int64_t col;
};

static bool qoz_eq_Token(qoz_Token a, qoz_Token b) {
    return a.kind == b.kind && qoz_string_eq(a.text, b.text) && a.line == b.line && a.col == b.col;
}

static uint64_t qoz_hash_Token(qoz_Token v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.kind);
    h = h * 31 + (uint64_t)(qoz_string_hash(v.text));
    h = h * 31 + (uint64_t)(v.line);
    h = h * 31 + (uint64_t)(v.col);
    return h;
}

static const int32_t qoz_Token_offsets[] = { (int32_t)offsetof(struct qoz_Token, kind), (int32_t)(offsetof(struct qoz_Token, text) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Token, text) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_Token_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Token), 3, qoz_Token_offsets, 0, 0, 0, NULL, "Token" };

struct qoz_Lexer {
    qoz_string src;
    int64_t pos;
    int64_t line;
    int64_t col;
};

static bool qoz_eq_Lexer(qoz_Lexer a, qoz_Lexer b) {
    return qoz_string_eq(a.src, b.src) && a.pos == b.pos && a.line == b.line && a.col == b.col;
}

static uint64_t qoz_hash_Lexer(qoz_Lexer v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.src));
    h = h * 31 + (uint64_t)(v.pos);
    h = h * 31 + (uint64_t)(v.line);
    h = h * 31 + (uint64_t)(v.col);
    return h;
}

static const int32_t qoz_Lexer_offsets[] = { (int32_t)(offsetof(struct qoz_Lexer, src) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Lexer, src) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_Lexer_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Lexer), 2, qoz_Lexer_offsets, 0, 0, 0, NULL, "Lexer" };

struct qoz_Span {
    qoz_string file;
    int64_t line;
    int64_t col;
};

static bool qoz_eq_Span(qoz_Span a, qoz_Span b) {
    return qoz_string_eq(a.file, b.file) && a.line == b.line && a.col == b.col;
}

static uint64_t qoz_hash_Span(qoz_Span v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.file));
    h = h * 31 + (uint64_t)(v.line);
    h = h * 31 + (uint64_t)(v.col);
    return h;
}

static const int32_t qoz_Span_offsets[] = { (int32_t)(offsetof(struct qoz_Span, file) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Span, file) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_Span_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Span), 2, qoz_Span_offsets, 0, 0, 0, NULL, "Span" };

struct qoz_ClosureParam {
    qoz_string name;
    qoz_TypeExpr* ty;
};

static bool qoz_eq_ClosureParam(qoz_ClosureParam a, qoz_ClosureParam b) {
    return qoz_string_eq(a.name, b.name) && a.ty == b.ty;
}

static uint64_t qoz_hash_ClosureParam(qoz_ClosureParam v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.name));
    h = h * 31 + (uint64_t)(v.ty);
    return h;
}

static const int32_t qoz_ClosureParam_offsets[] = { (int32_t)(offsetof(struct qoz_ClosureParam, name) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_ClosureParam, name) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_ClosureParam, ty) };
static const qoz_type_desc qoz_ClosureParam_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_ClosureParam), 3, qoz_ClosureParam_offsets, 0, 0, 0, NULL, "ClosureParam" };

struct qoz_RecordFieldLit {
    qoz_string name;
    qoz_Expr* value;
};

static bool qoz_eq_RecordFieldLit(qoz_RecordFieldLit a, qoz_RecordFieldLit b) {
    return qoz_string_eq(a.name, b.name) && a.value == b.value;
}

static uint64_t qoz_hash_RecordFieldLit(qoz_RecordFieldLit v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.name));
    h = h * 31 + (uint64_t)(v.value);
    return h;
}

static const int32_t qoz_RecordFieldLit_offsets[] = { (int32_t)(offsetof(struct qoz_RecordFieldLit, name) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_RecordFieldLit, name) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_RecordFieldLit, value) };
static const qoz_type_desc qoz_RecordFieldLit_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_RecordFieldLit), 3, qoz_RecordFieldLit_offsets, 0, 0, 0, NULL, "RecordFieldLit" };

struct qoz_MatchArm {
    qoz_Pattern* pat;
    qoz_Expr* body;
    bool has_guard;
    qoz_Expr* guard;
};

static bool qoz_eq_MatchArm(qoz_MatchArm a, qoz_MatchArm b) {
    return a.pat == b.pat && a.body == b.body && a.has_guard == b.has_guard && a.guard == b.guard;
}

static uint64_t qoz_hash_MatchArm(qoz_MatchArm v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.pat);
    h = h * 31 + (uint64_t)(v.body);
    h = h * 31 + (uint64_t)(v.has_guard);
    h = h * 31 + (uint64_t)(v.guard);
    return h;
}

static const int32_t qoz_MatchArm_offsets[] = { (int32_t)offsetof(struct qoz_MatchArm, pat), (int32_t)offsetof(struct qoz_MatchArm, body), (int32_t)offsetof(struct qoz_MatchArm, guard) };
static const qoz_type_desc qoz_MatchArm_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_MatchArm), 3, qoz_MatchArm_offsets, 0, 0, 0, NULL, "MatchArm" };

struct qoz_FnParam {
    qoz_string name;
    qoz_TypeExpr* ty;
};

static bool qoz_eq_FnParam(qoz_FnParam a, qoz_FnParam b) {
    return qoz_string_eq(a.name, b.name) && a.ty == b.ty;
}

static uint64_t qoz_hash_FnParam(qoz_FnParam v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.name));
    h = h * 31 + (uint64_t)(v.ty);
    return h;
}

static const int32_t qoz_FnParam_offsets[] = { (int32_t)(offsetof(struct qoz_FnParam, name) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_FnParam, name) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_FnParam, ty) };
static const qoz_type_desc qoz_FnParam_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_FnParam), 3, qoz_FnParam_offsets, 0, 0, 0, NULL, "FnParam" };

struct qoz_StructField {
    qoz_string name;
    qoz_TypeExpr* ty;
};

static bool qoz_eq_StructField(qoz_StructField a, qoz_StructField b) {
    return qoz_string_eq(a.name, b.name) && a.ty == b.ty;
}

static uint64_t qoz_hash_StructField(qoz_StructField v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.name));
    h = h * 31 + (uint64_t)(v.ty);
    return h;
}

static const int32_t qoz_StructField_offsets[] = { (int32_t)(offsetof(struct qoz_StructField, name) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_StructField, name) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_StructField, ty) };
static const qoz_type_desc qoz_StructField_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_StructField), 3, qoz_StructField_offsets, 0, 0, 0, NULL, "StructField" };

struct qoz_Vec__qoz_TypeExpr {
    qoz_TypeExpr** data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_TypeExpr(qoz_Vec__qoz_TypeExpr a, qoz_Vec__qoz_TypeExpr b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_TypeExpr(qoz_Vec__qoz_TypeExpr v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_TypeExpr_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_TypeExpr, data) };
static const qoz_type_desc qoz_Vec__qoz_TypeExpr_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_TypeExpr), 1, qoz_Vec__qoz_TypeExpr_offsets, 0, 0, 0, NULL, "Vec__qoz_TypeExpr" };

struct qoz_Vec__qoz_StructField {
    qoz_StructField* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_StructField(qoz_Vec__qoz_StructField a, qoz_Vec__qoz_StructField b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_StructField(qoz_Vec__qoz_StructField v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_StructField_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_StructField, data) };
static const qoz_type_desc qoz_Vec__qoz_StructField_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_StructField), 1, qoz_Vec__qoz_StructField_offsets, 0, 0, 0, NULL, "Vec__qoz_StructField" };

struct qoz_VariantDecl {
    qoz_Span span;
    qoz_string name;
    qoz_VariantPayloadKind* kind;
    qoz_Vec__qoz_TypeExpr pos;
    qoz_Vec__qoz_StructField named;
};

static bool qoz_eq_VariantDecl(qoz_VariantDecl a, qoz_VariantDecl b) {
    return qoz_eq_Span(a.span, b.span) && qoz_string_eq(a.name, b.name) && a.kind == b.kind && qoz_eq_Vec__qoz_TypeExpr(a.pos, b.pos) && qoz_eq_Vec__qoz_StructField(a.named, b.named);
}

static uint64_t qoz_hash_VariantDecl(qoz_VariantDecl v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Span(v.span));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.name));
    h = h * 31 + (uint64_t)(v.kind);
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_TypeExpr(v.pos));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_StructField(v.named));
    return h;
}

static const int32_t qoz_VariantDecl_offsets[] = { (int32_t)(offsetof(struct qoz_VariantDecl, span) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(struct qoz_VariantDecl, span) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(struct qoz_VariantDecl, name) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_VariantDecl, name) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_VariantDecl, kind), (int32_t)offsetof(struct qoz_VariantDecl, pos), (int32_t)offsetof(struct qoz_VariantDecl, named) };
static const qoz_type_desc qoz_VariantDecl_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_VariantDecl), 7, qoz_VariantDecl_offsets, 0, 0, 0, NULL, "VariantDecl" };

struct qoz_Attribute {
    qoz_Span span;
    qoz_string name;
    qoz_string string_arg;
    bool has_arg;
};

static bool qoz_eq_Attribute(qoz_Attribute a, qoz_Attribute b) {
    return qoz_eq_Span(a.span, b.span) && qoz_string_eq(a.name, b.name) && qoz_string_eq(a.string_arg, b.string_arg) && a.has_arg == b.has_arg;
}

static uint64_t qoz_hash_Attribute(qoz_Attribute v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Span(v.span));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.name));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.string_arg));
    h = h * 31 + (uint64_t)(v.has_arg);
    return h;
}

static const int32_t qoz_Attribute_offsets[] = { (int32_t)(offsetof(struct qoz_Attribute, span) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(struct qoz_Attribute, span) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(struct qoz_Attribute, name) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Attribute, name) + offsetof(qoz_string, root)), (int32_t)(offsetof(struct qoz_Attribute, string_arg) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Attribute, string_arg) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_Attribute_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Attribute), 6, qoz_Attribute_offsets, 0, 0, 0, NULL, "Attribute" };

struct qoz_Vec__qoz_Token {
    qoz_Token* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Token(qoz_Vec__qoz_Token a, qoz_Vec__qoz_Token b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Token(qoz_Vec__qoz_Token v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Token_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Token, data) };
static const qoz_type_desc qoz_Vec__qoz_Token_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Token), 1, qoz_Vec__qoz_Token_offsets, 0, 0, 0, NULL, "Vec__qoz_Token" };

struct qoz_Parser {
    qoz_Vec__qoz_Token tokens;
    int64_t pos;
    qoz_string file;
    qoz_Vec__qoz_string errors;
    int64_t in_match_arm;
};

static bool qoz_eq_Parser(qoz_Parser a, qoz_Parser b) {
    return qoz_eq_Vec__qoz_Token(a.tokens, b.tokens) && a.pos == b.pos && qoz_string_eq(a.file, b.file) && qoz_eq_Vec__qoz_string(a.errors, b.errors) && a.in_match_arm == b.in_match_arm;
}

static uint64_t qoz_hash_Parser(qoz_Parser v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_Token(v.tokens));
    h = h * 31 + (uint64_t)(v.pos);
    h = h * 31 + (uint64_t)(qoz_string_hash(v.file));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.errors));
    h = h * 31 + (uint64_t)(v.in_match_arm);
    return h;
}

static const int32_t qoz_Parser_offsets[] = { (int32_t)offsetof(struct qoz_Parser, tokens), (int32_t)(offsetof(struct qoz_Parser, file) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Parser, file) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_Parser, errors) };
static const qoz_type_desc qoz_Parser_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Parser), 4, qoz_Parser_offsets, 0, 0, 0, NULL, "Parser" };

struct qoz_ParseOutput {
    qoz_File file;
    qoz_Vec__qoz_string errors;
};

static bool qoz_eq_ParseOutput(qoz_ParseOutput a, qoz_ParseOutput b) {
    return qoz_eq_File(a.file, b.file) && qoz_eq_Vec__qoz_string(a.errors, b.errors);
}

static uint64_t qoz_hash_ParseOutput(qoz_ParseOutput v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_File(v.file));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.errors));
    return h;
}

static const int32_t qoz_ParseOutput_offsets[] = { (int32_t)(offsetof(struct qoz_ParseOutput, file) + (offsetof(struct qoz_File, path) + offsetof(qoz_string, data))), (int32_t)(offsetof(struct qoz_ParseOutput, file) + (offsetof(struct qoz_File, path) + offsetof(qoz_string, root))), (int32_t)(offsetof(struct qoz_ParseOutput, file) + offsetof(struct qoz_File, decls)), (int32_t)offsetof(struct qoz_ParseOutput, errors) };
static const qoz_type_desc qoz_ParseOutput_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_ParseOutput), 4, qoz_ParseOutput_offsets, 0, 0, 0, NULL, "ParseOutput" };

struct qoz_TypeError {
    qoz_Span span;
    qoz_string message;
};

static bool qoz_eq_TypeError(qoz_TypeError a, qoz_TypeError b) {
    return qoz_eq_Span(a.span, b.span) && qoz_string_eq(a.message, b.message);
}

static uint64_t qoz_hash_TypeError(qoz_TypeError v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Span(v.span));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.message));
    return h;
}

static const int32_t qoz_TypeError_offsets[] = { (int32_t)(offsetof(struct qoz_TypeError, span) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(struct qoz_TypeError, span) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(struct qoz_TypeError, message) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_TypeError, message) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_TypeError_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_TypeError), 4, qoz_TypeError_offsets, 0, 0, 0, NULL, "TypeError" };

struct qoz_Map__qoz_string__qoz_Decl {
    qoz_Slot__qoz_string__qoz_Decl* slots;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Map__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl a, qoz_Map__qoz_string__qoz_Decl b) {
    return a.slots == b.slots && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Map__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.slots);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Map__qoz_string__qoz_Decl_offsets[] = { (int32_t)offsetof(struct qoz_Map__qoz_string__qoz_Decl, slots) };
static const qoz_type_desc qoz_Map__qoz_string__qoz_Decl_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Map__qoz_string__qoz_Decl), 1, qoz_Map__qoz_string__qoz_Decl_offsets, 0, 0, 0, NULL, "Map__qoz_string__qoz_Decl" };

struct qoz_Map__qoz_string__bool {
    qoz_Slot__qoz_string__bool* slots;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Map__qoz_string__bool(qoz_Map__qoz_string__bool a, qoz_Map__qoz_string__bool b) {
    return a.slots == b.slots && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Map__qoz_string__bool(qoz_Map__qoz_string__bool v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.slots);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Map__qoz_string__bool_offsets[] = { (int32_t)offsetof(struct qoz_Map__qoz_string__bool, slots) };
static const qoz_type_desc qoz_Map__qoz_string__bool_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Map__qoz_string__bool), 1, qoz_Map__qoz_string__bool_offsets, 0, 0, 0, NULL, "Map__qoz_string__bool" };

struct qoz_Map__qoz_string__qoz_string {
    qoz_Slot__qoz_string__qoz_string* slots;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Map__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string a, qoz_Map__qoz_string__qoz_string b) {
    return a.slots == b.slots && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Map__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.slots);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Map__qoz_string__qoz_string_offsets[] = { (int32_t)offsetof(struct qoz_Map__qoz_string__qoz_string, slots) };
static const qoz_type_desc qoz_Map__qoz_string__qoz_string_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Map__qoz_string__qoz_string), 1, qoz_Map__qoz_string__qoz_string_offsets, 0, 0, 0, NULL, "Map__qoz_string__qoz_string" };

struct qoz_Vec__qoz_TypeError {
    qoz_TypeError* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_TypeError(qoz_Vec__qoz_TypeError a, qoz_Vec__qoz_TypeError b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_TypeError(qoz_Vec__qoz_TypeError v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_TypeError_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_TypeError, data) };
static const qoz_type_desc qoz_Vec__qoz_TypeError_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_TypeError), 1, qoz_Vec__qoz_TypeError_offsets, 0, 0, 0, NULL, "Vec__qoz_TypeError" };

struct qoz_Map__int64_t__qoz_TypeExpr {
    qoz_Slot__int64_t__qoz_TypeExpr* slots;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Map__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr a, qoz_Map__int64_t__qoz_TypeExpr b) {
    return a.slots == b.slots && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Map__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.slots);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Map__int64_t__qoz_TypeExpr_offsets[] = { (int32_t)offsetof(struct qoz_Map__int64_t__qoz_TypeExpr, slots) };
static const qoz_type_desc qoz_Map__int64_t__qoz_TypeExpr_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Map__int64_t__qoz_TypeExpr), 1, qoz_Map__int64_t__qoz_TypeExpr_offsets, 0, 0, 0, NULL, "Map__int64_t__qoz_TypeExpr" };

struct qoz_TyContext {
    qoz_Map__qoz_string__qoz_Decl enums;
    qoz_Map__qoz_string__qoz_Decl structs;
    qoz_Map__qoz_string__qoz_Decl aliases;
    qoz_Map__qoz_string__qoz_Decl fns;
    qoz_Map__qoz_string__qoz_Decl externs;
    qoz_Map__qoz_string__bool packages;
    qoz_Map__qoz_string__qoz_string variant_of;
    qoz_Vec__qoz_TypeError errors;
    qoz_Map__qoz_string__bool type_params;
    qoz_Map__int64_t__qoz_TypeExpr expr_types;
};

static bool qoz_eq_TyContext(qoz_TyContext a, qoz_TyContext b) {
    return qoz_eq_Map__qoz_string__qoz_Decl(a.enums, b.enums) && qoz_eq_Map__qoz_string__qoz_Decl(a.structs, b.structs) && qoz_eq_Map__qoz_string__qoz_Decl(a.aliases, b.aliases) && qoz_eq_Map__qoz_string__qoz_Decl(a.fns, b.fns) && qoz_eq_Map__qoz_string__qoz_Decl(a.externs, b.externs) && qoz_eq_Map__qoz_string__bool(a.packages, b.packages) && qoz_eq_Map__qoz_string__qoz_string(a.variant_of, b.variant_of) && qoz_eq_Vec__qoz_TypeError(a.errors, b.errors) && qoz_eq_Map__qoz_string__bool(a.type_params, b.type_params) && qoz_eq_Map__int64_t__qoz_TypeExpr(a.expr_types, b.expr_types);
}

static uint64_t qoz_hash_TyContext(qoz_TyContext v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.enums));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.structs));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.aliases));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.fns));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.externs));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.packages));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_string(v.variant_of));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_TypeError(v.errors));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.type_params));
    h = h * 31 + (uint64_t)(qoz_hash_Map__int64_t__qoz_TypeExpr(v.expr_types));
    return h;
}

static const int32_t qoz_TyContext_offsets[] = { (int32_t)offsetof(struct qoz_TyContext, enums), (int32_t)offsetof(struct qoz_TyContext, structs), (int32_t)offsetof(struct qoz_TyContext, aliases), (int32_t)offsetof(struct qoz_TyContext, fns), (int32_t)offsetof(struct qoz_TyContext, externs), (int32_t)offsetof(struct qoz_TyContext, packages), (int32_t)offsetof(struct qoz_TyContext, variant_of), (int32_t)offsetof(struct qoz_TyContext, errors), (int32_t)offsetof(struct qoz_TyContext, type_params), (int32_t)offsetof(struct qoz_TyContext, expr_types) };
static const qoz_type_desc qoz_TyContext_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_TyContext), 10, qoz_TyContext_offsets, 0, 0, 0, NULL, "TyContext" };

struct qoz_Binding {
    qoz_string name;
    qoz_Ty* ty;
};

static bool qoz_eq_Binding(qoz_Binding a, qoz_Binding b) {
    return qoz_string_eq(a.name, b.name) && a.ty == b.ty;
}

static uint64_t qoz_hash_Binding(qoz_Binding v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.name));
    h = h * 31 + (uint64_t)(v.ty);
    return h;
}

static const int32_t qoz_Binding_offsets[] = { (int32_t)(offsetof(struct qoz_Binding, name) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Binding, name) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_Binding, ty) };
static const qoz_type_desc qoz_Binding_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Binding), 3, qoz_Binding_offsets, 0, 0, 0, NULL, "Binding" };

struct qoz_Vec__qoz_Binding {
    qoz_Binding* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Binding(qoz_Vec__qoz_Binding a, qoz_Vec__qoz_Binding b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Binding(qoz_Vec__qoz_Binding v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Binding_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Binding, data) };
static const qoz_type_desc qoz_Vec__qoz_Binding_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Binding), 1, qoz_Vec__qoz_Binding_offsets, 0, 0, 0, NULL, "Vec__qoz_Binding" };

struct qoz_Env {
    qoz_Vec__qoz_Binding bindings;
};

static bool qoz_eq_Env(qoz_Env a, qoz_Env b) {
    return qoz_eq_Vec__qoz_Binding(a.bindings, b.bindings);
}

static uint64_t qoz_hash_Env(qoz_Env v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_Binding(v.bindings));
    return h;
}

static const int32_t qoz_Env_offsets[] = { (int32_t)offsetof(struct qoz_Env, bindings) };
static const qoz_type_desc qoz_Env_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Env), 1, qoz_Env_offsets, 0, 0, 0, NULL, "Env" };

struct qoz_CoverSet {
    qoz_Map__qoz_string__bool bound;
};

static bool qoz_eq_CoverSet(qoz_CoverSet a, qoz_CoverSet b) {
    return qoz_eq_Map__qoz_string__bool(a.bound, b.bound);
}

static uint64_t qoz_hash_CoverSet(qoz_CoverSet v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.bound));
    return h;
}

static const int32_t qoz_CoverSet_offsets[] = { (int32_t)offsetof(struct qoz_CoverSet, bound) };
static const qoz_type_desc qoz_CoverSet_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_CoverSet), 1, qoz_CoverSet_offsets, 0, 0, 0, NULL, "CoverSet" };

struct qoz_Instantiation {
    qoz_string name;
    qoz_Vec__qoz_TypeExpr args;
    qoz_string mangled;
};

static bool qoz_eq_Instantiation(qoz_Instantiation a, qoz_Instantiation b) {
    return qoz_string_eq(a.name, b.name) && qoz_eq_Vec__qoz_TypeExpr(a.args, b.args) && qoz_string_eq(a.mangled, b.mangled);
}

static uint64_t qoz_hash_Instantiation(qoz_Instantiation v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.name));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_TypeExpr(v.args));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.mangled));
    return h;
}

static const int32_t qoz_Instantiation_offsets[] = { (int32_t)(offsetof(struct qoz_Instantiation, name) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Instantiation, name) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_Instantiation, args), (int32_t)(offsetof(struct qoz_Instantiation, mangled) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Instantiation, mangled) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_Instantiation_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Instantiation), 5, qoz_Instantiation_offsets, 0, 0, 0, NULL, "Instantiation" };

struct qoz_Vec__qoz_Instantiation {
    qoz_Instantiation* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Instantiation(qoz_Vec__qoz_Instantiation a, qoz_Vec__qoz_Instantiation b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Instantiation(qoz_Vec__qoz_Instantiation v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Instantiation_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Instantiation, data) };
static const qoz_type_desc qoz_Vec__qoz_Instantiation_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Instantiation), 1, qoz_Vec__qoz_Instantiation_offsets, 0, 0, 0, NULL, "Vec__qoz_Instantiation" };

struct qoz_Map__qoz_string__qoz_TypeExpr {
    qoz_Slot__qoz_string__qoz_TypeExpr* slots;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Map__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr a, qoz_Map__qoz_string__qoz_TypeExpr b) {
    return a.slots == b.slots && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Map__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.slots);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Map__qoz_string__qoz_TypeExpr_offsets[] = { (int32_t)offsetof(struct qoz_Map__qoz_string__qoz_TypeExpr, slots) };
static const qoz_type_desc qoz_Map__qoz_string__qoz_TypeExpr_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Map__qoz_string__qoz_TypeExpr), 1, qoz_Map__qoz_string__qoz_TypeExpr_offsets, 0, 0, 0, NULL, "Map__qoz_string__qoz_TypeExpr" };

struct qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr {
    qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr* slots;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Map__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr a, qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr b) {
    return a.slots == b.slots && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Map__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.slots);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr_offsets[] = { (int32_t)offsetof(struct qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr, slots) };
static const qoz_type_desc qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr), 1, qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr_offsets, 0, 0, 0, NULL, "Map__qoz_string__qoz_Vec__qoz_TypeExpr" };

struct qoz_Emitter {
    qoz_Strbuf out;
    qoz_Map__int64_t__qoz_TypeExpr expr_types;
    qoz_Vec__qoz_string current_tparams;
    qoz_Vec__qoz_TypeExpr current_targs;
    int64_t pos_fn_typedefs;
    int64_t pos_tuple_typedefs;
    int64_t pos_synth_fn_decls;
    qoz_Map__qoz_string__qoz_string variant_of;
    qoz_Map__qoz_string__bool is_enum;
    qoz_Map__qoz_string__qoz_Decl enum_decls;
    qoz_Map__qoz_string__qoz_Decl struct_decls;
    qoz_Map__qoz_string__qoz_Decl generic_decls;
    qoz_Map__qoz_string__qoz_Decl generic_fn_decls;
    qoz_Map__qoz_string__bool packages;
    qoz_Vec__qoz_Instantiation record_insts;
    qoz_Map__qoz_string__bool record_seen;
    qoz_Vec__qoz_Instantiation enum_insts;
    qoz_Map__qoz_string__bool enum_seen;
    qoz_Vec__qoz_Instantiation fn_insts;
    qoz_Map__qoz_string__bool fn_seen;
    qoz_Map__qoz_string__qoz_TypeExpr locals;
    qoz_Map__qoz_string__qoz_string externs;
    qoz_Map__qoz_string__qoz_TypeExpr fn_returns;
    qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr fn_params;
    int64_t match_counter;
    qoz_TypeExpr* current_ret_te;
    qoz_TypeExpr* match_hint;
    qoz_Map__qoz_string__qoz_string fn_typedefs;
    qoz_Map__qoz_string__qoz_string op_dispatch;
    qoz_Map__qoz_string__bool byval_helpers_emitted;
    qoz_Vec__qoz_string pending_prologue;
    qoz_string current_return_restore;
    qoz_Map__qoz_string__qoz_string tuple_typedefs;
    qoz_Vec__qoz_string tuple_typedef_order;
    qoz_Vec__qoz_string fn_typedef_order;
    int64_t closure_counter;
    qoz_Vec__qoz_string synth_fn_defs;
    qoz_Vec__qoz_string synth_fn_decls;
};

static bool qoz_eq_Emitter(qoz_Emitter a, qoz_Emitter b) {
    return qoz_eq_Strbuf(a.out, b.out) && qoz_eq_Map__int64_t__qoz_TypeExpr(a.expr_types, b.expr_types) && qoz_eq_Vec__qoz_string(a.current_tparams, b.current_tparams) && qoz_eq_Vec__qoz_TypeExpr(a.current_targs, b.current_targs) && a.pos_fn_typedefs == b.pos_fn_typedefs && a.pos_tuple_typedefs == b.pos_tuple_typedefs && a.pos_synth_fn_decls == b.pos_synth_fn_decls && qoz_eq_Map__qoz_string__qoz_string(a.variant_of, b.variant_of) && qoz_eq_Map__qoz_string__bool(a.is_enum, b.is_enum) && qoz_eq_Map__qoz_string__qoz_Decl(a.enum_decls, b.enum_decls) && qoz_eq_Map__qoz_string__qoz_Decl(a.struct_decls, b.struct_decls) && qoz_eq_Map__qoz_string__qoz_Decl(a.generic_decls, b.generic_decls) && qoz_eq_Map__qoz_string__qoz_Decl(a.generic_fn_decls, b.generic_fn_decls) && qoz_eq_Map__qoz_string__bool(a.packages, b.packages) && qoz_eq_Vec__qoz_Instantiation(a.record_insts, b.record_insts) && qoz_eq_Map__qoz_string__bool(a.record_seen, b.record_seen) && qoz_eq_Vec__qoz_Instantiation(a.enum_insts, b.enum_insts) && qoz_eq_Map__qoz_string__bool(a.enum_seen, b.enum_seen) && qoz_eq_Vec__qoz_Instantiation(a.fn_insts, b.fn_insts) && qoz_eq_Map__qoz_string__bool(a.fn_seen, b.fn_seen) && qoz_eq_Map__qoz_string__qoz_TypeExpr(a.locals, b.locals) && qoz_eq_Map__qoz_string__qoz_string(a.externs, b.externs) && qoz_eq_Map__qoz_string__qoz_TypeExpr(a.fn_returns, b.fn_returns) && qoz_eq_Map__qoz_string__qoz_Vec__qoz_TypeExpr(a.fn_params, b.fn_params) && a.match_counter == b.match_counter && a.current_ret_te == b.current_ret_te && a.match_hint == b.match_hint && qoz_eq_Map__qoz_string__qoz_string(a.fn_typedefs, b.fn_typedefs) && qoz_eq_Map__qoz_string__qoz_string(a.op_dispatch, b.op_dispatch) && qoz_eq_Map__qoz_string__bool(a.byval_helpers_emitted, b.byval_helpers_emitted) && qoz_eq_Vec__qoz_string(a.pending_prologue, b.pending_prologue) && qoz_string_eq(a.current_return_restore, b.current_return_restore) && qoz_eq_Map__qoz_string__qoz_string(a.tuple_typedefs, b.tuple_typedefs) && qoz_eq_Vec__qoz_string(a.tuple_typedef_order, b.tuple_typedef_order) && qoz_eq_Vec__qoz_string(a.fn_typedef_order, b.fn_typedef_order) && a.closure_counter == b.closure_counter && qoz_eq_Vec__qoz_string(a.synth_fn_defs, b.synth_fn_defs) && qoz_eq_Vec__qoz_string(a.synth_fn_decls, b.synth_fn_decls);
}

static uint64_t qoz_hash_Emitter(qoz_Emitter v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Strbuf(v.out));
    h = h * 31 + (uint64_t)(qoz_hash_Map__int64_t__qoz_TypeExpr(v.expr_types));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.current_tparams));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_TypeExpr(v.current_targs));
    h = h * 31 + (uint64_t)(v.pos_fn_typedefs);
    h = h * 31 + (uint64_t)(v.pos_tuple_typedefs);
    h = h * 31 + (uint64_t)(v.pos_synth_fn_decls);
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_string(v.variant_of));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.is_enum));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.enum_decls));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.struct_decls));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.generic_decls));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Decl(v.generic_fn_decls));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.packages));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_Instantiation(v.record_insts));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.record_seen));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_Instantiation(v.enum_insts));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.enum_seen));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_Instantiation(v.fn_insts));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.fn_seen));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_TypeExpr(v.locals));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_string(v.externs));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_TypeExpr(v.fn_returns));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_Vec__qoz_TypeExpr(v.fn_params));
    h = h * 31 + (uint64_t)(v.match_counter);
    h = h * 31 + (uint64_t)(v.current_ret_te);
    h = h * 31 + (uint64_t)(v.match_hint);
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_string(v.fn_typedefs));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_string(v.op_dispatch));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.byval_helpers_emitted));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.pending_prologue));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.current_return_restore));
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__qoz_string(v.tuple_typedefs));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.tuple_typedef_order));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.fn_typedef_order));
    h = h * 31 + (uint64_t)(v.closure_counter);
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.synth_fn_defs));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.synth_fn_decls));
    return h;
}

static const int32_t qoz_Emitter_offsets[] = { (int32_t)(offsetof(struct qoz_Emitter, out) + offsetof(struct qoz_Strbuf, buf)), (int32_t)offsetof(struct qoz_Emitter, expr_types), (int32_t)offsetof(struct qoz_Emitter, current_tparams), (int32_t)offsetof(struct qoz_Emitter, current_targs), (int32_t)offsetof(struct qoz_Emitter, variant_of), (int32_t)offsetof(struct qoz_Emitter, is_enum), (int32_t)offsetof(struct qoz_Emitter, enum_decls), (int32_t)offsetof(struct qoz_Emitter, struct_decls), (int32_t)offsetof(struct qoz_Emitter, generic_decls), (int32_t)offsetof(struct qoz_Emitter, generic_fn_decls), (int32_t)offsetof(struct qoz_Emitter, packages), (int32_t)offsetof(struct qoz_Emitter, record_insts), (int32_t)offsetof(struct qoz_Emitter, record_seen), (int32_t)offsetof(struct qoz_Emitter, enum_insts), (int32_t)offsetof(struct qoz_Emitter, enum_seen), (int32_t)offsetof(struct qoz_Emitter, fn_insts), (int32_t)offsetof(struct qoz_Emitter, fn_seen), (int32_t)offsetof(struct qoz_Emitter, locals), (int32_t)offsetof(struct qoz_Emitter, externs), (int32_t)offsetof(struct qoz_Emitter, fn_returns), (int32_t)offsetof(struct qoz_Emitter, fn_params), (int32_t)offsetof(struct qoz_Emitter, current_ret_te), (int32_t)offsetof(struct qoz_Emitter, match_hint), (int32_t)offsetof(struct qoz_Emitter, fn_typedefs), (int32_t)offsetof(struct qoz_Emitter, op_dispatch), (int32_t)offsetof(struct qoz_Emitter, byval_helpers_emitted), (int32_t)offsetof(struct qoz_Emitter, pending_prologue), (int32_t)(offsetof(struct qoz_Emitter, current_return_restore) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Emitter, current_return_restore) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_Emitter, tuple_typedefs), (int32_t)offsetof(struct qoz_Emitter, tuple_typedef_order), (int32_t)offsetof(struct qoz_Emitter, fn_typedef_order), (int32_t)offsetof(struct qoz_Emitter, synth_fn_defs), (int32_t)offsetof(struct qoz_Emitter, synth_fn_decls) };
static const qoz_type_desc qoz_Emitter_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Emitter), 34, qoz_Emitter_offsets, 0, 0, 0, NULL, "Emitter" };

struct qoz_CaptureScope {
    qoz_Map__qoz_string__bool bound;
};

static bool qoz_eq_CaptureScope(qoz_CaptureScope a, qoz_CaptureScope b) {
    return qoz_eq_Map__qoz_string__bool(a.bound, b.bound);
}

static uint64_t qoz_hash_CaptureScope(qoz_CaptureScope v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_hash_Map__qoz_string__bool(v.bound));
    return h;
}

static const int32_t qoz_CaptureScope_offsets[] = { (int32_t)offsetof(struct qoz_CaptureScope, bound) };
static const qoz_type_desc qoz_CaptureScope_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_CaptureScope), 1, qoz_CaptureScope_offsets, 0, 0, 0, NULL, "CaptureScope" };

struct qoz_StmtScope {
    int64_t start;
    qoz_Vec__qoz_string prologue;
};

static bool qoz_eq_StmtScope(qoz_StmtScope a, qoz_StmtScope b) {
    return a.start == b.start && qoz_eq_Vec__qoz_string(a.prologue, b.prologue);
}

static uint64_t qoz_hash_StmtScope(qoz_StmtScope v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.start);
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_string(v.prologue));
    return h;
}

static const int32_t qoz_StmtScope_offsets[] = { (int32_t)offsetof(struct qoz_StmtScope, prologue) };
static const qoz_type_desc qoz_StmtScope_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_StmtScope), 1, qoz_StmtScope_offsets, 0, 0, 0, NULL, "StmtScope" };

struct qoz_IntInfo {
    int64_t width;
    bool is_signed;
    bool untyped;
};

static bool qoz_eq_IntInfo(qoz_IntInfo a, qoz_IntInfo b) {
    return a.width == b.width && a.is_signed == b.is_signed && a.untyped == b.untyped;
}

static uint64_t qoz_hash_IntInfo(qoz_IntInfo v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.width);
    h = h * 31 + (uint64_t)(v.is_signed);
    h = h * 31 + (uint64_t)(v.untyped);
    return h;
}

static const qoz_type_desc qoz_IntInfo_desc = { QOZ_DESC_LEAF, (int32_t)sizeof(struct qoz_IntInfo), 0, NULL, 0, 0, 0, NULL, "IntInfo" };

struct qoz_FloatInfo {
    int64_t width;
    bool untyped;
};

static bool qoz_eq_FloatInfo(qoz_FloatInfo a, qoz_FloatInfo b) {
    return a.width == b.width && a.untyped == b.untyped;
}

static uint64_t qoz_hash_FloatInfo(qoz_FloatInfo v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.width);
    h = h * 31 + (uint64_t)(v.untyped);
    return h;
}

static const qoz_type_desc qoz_FloatInfo_desc = { QOZ_DESC_LEAF, (int32_t)sizeof(struct qoz_FloatInfo), 0, NULL, 0, 0, 0, NULL, "FloatInfo" };

struct qoz_Vec__qoz_Expr {
    qoz_Expr** data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Expr(qoz_Vec__qoz_Expr a, qoz_Vec__qoz_Expr b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Expr(qoz_Vec__qoz_Expr v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Expr_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Expr, data) };
static const qoz_type_desc qoz_Vec__qoz_Expr_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Expr), 1, qoz_Vec__qoz_Expr_offsets, 0, 0, 0, NULL, "Vec__qoz_Expr" };

struct qoz_Vec__qoz_RecordFieldLit {
    qoz_RecordFieldLit* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_RecordFieldLit(qoz_Vec__qoz_RecordFieldLit a, qoz_Vec__qoz_RecordFieldLit b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_RecordFieldLit(qoz_Vec__qoz_RecordFieldLit v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_RecordFieldLit_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_RecordFieldLit, data) };
static const qoz_type_desc qoz_Vec__qoz_RecordFieldLit_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_RecordFieldLit), 1, qoz_Vec__qoz_RecordFieldLit_offsets, 0, 0, 0, NULL, "Vec__qoz_RecordFieldLit" };

struct qoz_Vec__qoz_Stmt {
    qoz_Stmt** data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Stmt(qoz_Vec__qoz_Stmt a, qoz_Vec__qoz_Stmt b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Stmt(qoz_Vec__qoz_Stmt v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Stmt_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Stmt, data) };
static const qoz_type_desc qoz_Vec__qoz_Stmt_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Stmt), 1, qoz_Vec__qoz_Stmt_offsets, 0, 0, 0, NULL, "Vec__qoz_Stmt" };

struct qoz_Vec__qoz_MatchArm {
    qoz_MatchArm* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_MatchArm(qoz_Vec__qoz_MatchArm a, qoz_Vec__qoz_MatchArm b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_MatchArm(qoz_Vec__qoz_MatchArm v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_MatchArm_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_MatchArm, data) };
static const qoz_type_desc qoz_Vec__qoz_MatchArm_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_MatchArm), 1, qoz_Vec__qoz_MatchArm_offsets, 0, 0, 0, NULL, "Vec__qoz_MatchArm" };

struct qoz_Vec__qoz_Pending {
    qoz_Pending* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Pending(qoz_Vec__qoz_Pending a, qoz_Vec__qoz_Pending b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Pending(qoz_Vec__qoz_Pending v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Pending_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Pending, data) };
static const qoz_type_desc qoz_Vec__qoz_Pending_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Pending), 1, qoz_Vec__qoz_Pending_offsets, 0, 0, 0, NULL, "Vec__qoz_Pending" };

struct qoz_Vec__qoz_Pattern {
    qoz_Pattern** data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Pattern(qoz_Vec__qoz_Pattern a, qoz_Vec__qoz_Pattern b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Pattern(qoz_Vec__qoz_Pattern v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Pattern_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Pattern, data) };
static const qoz_type_desc qoz_Vec__qoz_Pattern_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Pattern), 1, qoz_Vec__qoz_Pattern_offsets, 0, 0, 0, NULL, "Vec__qoz_Pattern" };

struct qoz_Vec__qoz_ClosureParam {
    qoz_ClosureParam* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_ClosureParam(qoz_Vec__qoz_ClosureParam a, qoz_Vec__qoz_ClosureParam b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_ClosureParam(qoz_Vec__qoz_ClosureParam v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_ClosureParam_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_ClosureParam, data) };
static const qoz_type_desc qoz_Vec__qoz_ClosureParam_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_ClosureParam), 1, qoz_Vec__qoz_ClosureParam_offsets, 0, 0, 0, NULL, "Vec__qoz_ClosureParam" };

struct qoz_Vec__qoz_FnParam {
    qoz_FnParam* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_FnParam(qoz_Vec__qoz_FnParam a, qoz_Vec__qoz_FnParam b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_FnParam(qoz_Vec__qoz_FnParam v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_FnParam_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_FnParam, data) };
static const qoz_type_desc qoz_Vec__qoz_FnParam_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_FnParam), 1, qoz_Vec__qoz_FnParam_offsets, 0, 0, 0, NULL, "Vec__qoz_FnParam" };

struct qoz_Vec__qoz_VariantDecl {
    qoz_VariantDecl* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_VariantDecl(qoz_Vec__qoz_VariantDecl a, qoz_Vec__qoz_VariantDecl b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_VariantDecl(qoz_Vec__qoz_VariantDecl v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_VariantDecl_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_VariantDecl, data) };
static const qoz_type_desc qoz_Vec__qoz_VariantDecl_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_VariantDecl), 1, qoz_Vec__qoz_VariantDecl_offsets, 0, 0, 0, NULL, "Vec__qoz_VariantDecl" };

struct qoz_Vec__qoz_Ty {
    qoz_Ty** data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Ty(qoz_Vec__qoz_Ty a, qoz_Vec__qoz_Ty b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Ty(qoz_Vec__qoz_Ty v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Ty_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Ty, data) };
static const qoz_type_desc qoz_Vec__qoz_Ty_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Ty), 1, qoz_Vec__qoz_Ty_offsets, 0, 0, 0, NULL, "Vec__qoz_Ty" };

struct qoz_Map__qoz_string__qoz_Ty {
    qoz_Slot__qoz_string__qoz_Ty* slots;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Map__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty a, qoz_Map__qoz_string__qoz_Ty b) {
    return a.slots == b.slots && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Map__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.slots);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Map__qoz_string__qoz_Ty_offsets[] = { (int32_t)offsetof(struct qoz_Map__qoz_string__qoz_Ty, slots) };
static const qoz_type_desc qoz_Map__qoz_string__qoz_Ty_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Map__qoz_string__qoz_Ty), 1, qoz_Map__qoz_string__qoz_Ty_offsets, 0, 0, 0, NULL, "Map__qoz_string__qoz_Ty" };

struct qoz_Vec__qoz_Vec__qoz_StructField {
    qoz_Vec__qoz_StructField* data;
    int64_t len;
    int64_t cap;
};

static bool qoz_eq_Vec__qoz_Vec__qoz_StructField(qoz_Vec__qoz_Vec__qoz_StructField a, qoz_Vec__qoz_Vec__qoz_StructField b) {
    return a.data == b.data && a.len == b.len && a.cap == b.cap;
}

static uint64_t qoz_hash_Vec__qoz_Vec__qoz_StructField(qoz_Vec__qoz_Vec__qoz_StructField v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.data);
    h = h * 31 + (uint64_t)(v.len);
    h = h * 31 + (uint64_t)(v.cap);
    return h;
}

static const int32_t qoz_Vec__qoz_Vec__qoz_StructField_offsets[] = { (int32_t)offsetof(struct qoz_Vec__qoz_Vec__qoz_StructField, data) };
static const qoz_type_desc qoz_Vec__qoz_Vec__qoz_StructField_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Vec__qoz_Vec__qoz_StructField), 1, qoz_Vec__qoz_Vec__qoz_StructField_offsets, 0, 0, 0, NULL, "Vec__qoz_Vec__qoz_StructField" };

struct qoz_Slot__qoz_string__bool {
    qoz_string key;
    bool value;
    bool occupied;
    bool deleted;
};

static bool qoz_eq_Slot__qoz_string__bool(qoz_Slot__qoz_string__bool a, qoz_Slot__qoz_string__bool b) {
    return qoz_string_eq(a.key, b.key) && a.value == b.value && a.occupied == b.occupied && a.deleted == b.deleted;
}

static uint64_t qoz_hash_Slot__qoz_string__bool(qoz_Slot__qoz_string__bool v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.key));
    h = h * 31 + (uint64_t)(v.value);
    h = h * 31 + (uint64_t)(v.occupied);
    h = h * 31 + (uint64_t)(v.deleted);
    return h;
}

static const int32_t qoz_Slot__qoz_string__bool_offsets[] = { (int32_t)(offsetof(struct qoz_Slot__qoz_string__bool, key) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Slot__qoz_string__bool, key) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_Slot__qoz_string__bool_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Slot__qoz_string__bool), 2, qoz_Slot__qoz_string__bool_offsets, 0, 0, 0, NULL, "Slot__qoz_string__bool" };

struct qoz_Slot__qoz_string__qoz_Decl {
    qoz_string key;
    qoz_Decl* value;
    bool occupied;
    bool deleted;
};

static bool qoz_eq_Slot__qoz_string__qoz_Decl(qoz_Slot__qoz_string__qoz_Decl a, qoz_Slot__qoz_string__qoz_Decl b) {
    return qoz_string_eq(a.key, b.key) && a.value == b.value && a.occupied == b.occupied && a.deleted == b.deleted;
}

static uint64_t qoz_hash_Slot__qoz_string__qoz_Decl(qoz_Slot__qoz_string__qoz_Decl v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.key));
    h = h * 31 + (uint64_t)(v.value);
    h = h * 31 + (uint64_t)(v.occupied);
    h = h * 31 + (uint64_t)(v.deleted);
    return h;
}

static const int32_t qoz_Slot__qoz_string__qoz_Decl_offsets[] = { (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_Decl, key) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_Decl, key) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_Slot__qoz_string__qoz_Decl, value) };
static const qoz_type_desc qoz_Slot__qoz_string__qoz_Decl_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Slot__qoz_string__qoz_Decl), 3, qoz_Slot__qoz_string__qoz_Decl_offsets, 0, 0, 0, NULL, "Slot__qoz_string__qoz_Decl" };

struct qoz_Slot__qoz_string__qoz_string {
    qoz_string key;
    qoz_string value;
    bool occupied;
    bool deleted;
};

static bool qoz_eq_Slot__qoz_string__qoz_string(qoz_Slot__qoz_string__qoz_string a, qoz_Slot__qoz_string__qoz_string b) {
    return qoz_string_eq(a.key, b.key) && qoz_string_eq(a.value, b.value) && a.occupied == b.occupied && a.deleted == b.deleted;
}

static uint64_t qoz_hash_Slot__qoz_string__qoz_string(qoz_Slot__qoz_string__qoz_string v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.key));
    h = h * 31 + (uint64_t)(qoz_string_hash(v.value));
    h = h * 31 + (uint64_t)(v.occupied);
    h = h * 31 + (uint64_t)(v.deleted);
    return h;
}

static const int32_t qoz_Slot__qoz_string__qoz_string_offsets[] = { (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_string, key) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_string, key) + offsetof(qoz_string, root)), (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_string, value) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_string, value) + offsetof(qoz_string, root)) };
static const qoz_type_desc qoz_Slot__qoz_string__qoz_string_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Slot__qoz_string__qoz_string), 4, qoz_Slot__qoz_string__qoz_string_offsets, 0, 0, 0, NULL, "Slot__qoz_string__qoz_string" };

struct qoz_Slot__int64_t__qoz_TypeExpr {
    int64_t key;
    qoz_TypeExpr* value;
    bool occupied;
    bool deleted;
};

static bool qoz_eq_Slot__int64_t__qoz_TypeExpr(qoz_Slot__int64_t__qoz_TypeExpr a, qoz_Slot__int64_t__qoz_TypeExpr b) {
    return a.key == b.key && a.value == b.value && a.occupied == b.occupied && a.deleted == b.deleted;
}

static uint64_t qoz_hash_Slot__int64_t__qoz_TypeExpr(qoz_Slot__int64_t__qoz_TypeExpr v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(v.key);
    h = h * 31 + (uint64_t)(v.value);
    h = h * 31 + (uint64_t)(v.occupied);
    h = h * 31 + (uint64_t)(v.deleted);
    return h;
}

static const int32_t qoz_Slot__int64_t__qoz_TypeExpr_offsets[] = { (int32_t)offsetof(struct qoz_Slot__int64_t__qoz_TypeExpr, value) };
static const qoz_type_desc qoz_Slot__int64_t__qoz_TypeExpr_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Slot__int64_t__qoz_TypeExpr), 1, qoz_Slot__int64_t__qoz_TypeExpr_offsets, 0, 0, 0, NULL, "Slot__int64_t__qoz_TypeExpr" };

struct qoz_Slot__qoz_string__qoz_Ty {
    qoz_string key;
    qoz_Ty* value;
    bool occupied;
    bool deleted;
};

static bool qoz_eq_Slot__qoz_string__qoz_Ty(qoz_Slot__qoz_string__qoz_Ty a, qoz_Slot__qoz_string__qoz_Ty b) {
    return qoz_string_eq(a.key, b.key) && a.value == b.value && a.occupied == b.occupied && a.deleted == b.deleted;
}

static uint64_t qoz_hash_Slot__qoz_string__qoz_Ty(qoz_Slot__qoz_string__qoz_Ty v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.key));
    h = h * 31 + (uint64_t)(v.value);
    h = h * 31 + (uint64_t)(v.occupied);
    h = h * 31 + (uint64_t)(v.deleted);
    return h;
}

static const int32_t qoz_Slot__qoz_string__qoz_Ty_offsets[] = { (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_Ty, key) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_Ty, key) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_Slot__qoz_string__qoz_Ty, value) };
static const qoz_type_desc qoz_Slot__qoz_string__qoz_Ty_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Slot__qoz_string__qoz_Ty), 3, qoz_Slot__qoz_string__qoz_Ty_offsets, 0, 0, 0, NULL, "Slot__qoz_string__qoz_Ty" };

struct qoz_Slot__qoz_string__qoz_TypeExpr {
    qoz_string key;
    qoz_TypeExpr* value;
    bool occupied;
    bool deleted;
};

static bool qoz_eq_Slot__qoz_string__qoz_TypeExpr(qoz_Slot__qoz_string__qoz_TypeExpr a, qoz_Slot__qoz_string__qoz_TypeExpr b) {
    return qoz_string_eq(a.key, b.key) && a.value == b.value && a.occupied == b.occupied && a.deleted == b.deleted;
}

static uint64_t qoz_hash_Slot__qoz_string__qoz_TypeExpr(qoz_Slot__qoz_string__qoz_TypeExpr v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.key));
    h = h * 31 + (uint64_t)(v.value);
    h = h * 31 + (uint64_t)(v.occupied);
    h = h * 31 + (uint64_t)(v.deleted);
    return h;
}

static const int32_t qoz_Slot__qoz_string__qoz_TypeExpr_offsets[] = { (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_TypeExpr, key) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_TypeExpr, key) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_Slot__qoz_string__qoz_TypeExpr, value) };
static const qoz_type_desc qoz_Slot__qoz_string__qoz_TypeExpr_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Slot__qoz_string__qoz_TypeExpr), 3, qoz_Slot__qoz_string__qoz_TypeExpr_offsets, 0, 0, 0, NULL, "Slot__qoz_string__qoz_TypeExpr" };

struct qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr {
    qoz_string key;
    qoz_Vec__qoz_TypeExpr value;
    bool occupied;
    bool deleted;
};

static bool qoz_eq_Slot__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr a, qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr b) {
    return qoz_string_eq(a.key, b.key) && qoz_eq_Vec__qoz_TypeExpr(a.value, b.value) && a.occupied == b.occupied && a.deleted == b.deleted;
}

static uint64_t qoz_hash_Slot__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr v) {
    uint64_t h = 0;
    h = h * 31 + (uint64_t)(qoz_string_hash(v.key));
    h = h * 31 + (uint64_t)(qoz_hash_Vec__qoz_TypeExpr(v.value));
    h = h * 31 + (uint64_t)(v.occupied);
    h = h * 31 + (uint64_t)(v.deleted);
    return h;
}

static const int32_t qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr_offsets[] = { (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr, key) + offsetof(qoz_string, data)), (int32_t)(offsetof(struct qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr, key) + offsetof(qoz_string, root)), (int32_t)offsetof(struct qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr, value) };
static const qoz_type_desc qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr), 3, qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr_offsets, 0, 0, 0, NULL, "Slot__qoz_string__qoz_Vec__qoz_TypeExpr" };

typedef enum {
    qoz_TokenKind_TokIdent,
    qoz_TokenKind_TokInt,
    qoz_TokenKind_TokFloat,
    qoz_TokenKind_TokString,
    qoz_TokenKind_TokChar,
    qoz_TokenKind_TokPunct,
    qoz_TokenKind_TokKeyword,
    qoz_TokenKind_TokEOF,
} qoz_TokenKind_tag;

struct qoz_TokenKind {
    qoz_TokenKind_tag tag;
};

static const qoz_variant_desc qoz_TokenKind_variants[] = {
    { qoz_TokenKind_TokIdent, 0, NULL },
    { qoz_TokenKind_TokInt, 0, NULL },
    { qoz_TokenKind_TokFloat, 0, NULL },
    { qoz_TokenKind_TokString, 0, NULL },
    { qoz_TokenKind_TokChar, 0, NULL },
    { qoz_TokenKind_TokPunct, 0, NULL },
    { qoz_TokenKind_TokKeyword, 0, NULL },
    { qoz_TokenKind_TokEOF, 0, NULL },
};
static const qoz_type_desc qoz_TokenKind_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_TokenKind), 0, NULL, (int32_t)offsetof(struct qoz_TokenKind, tag), 0, 8, qoz_TokenKind_variants, "TokenKind" };

typedef enum {
    qoz_UnaryOp_UOpNeg,
    qoz_UnaryOp_UOpNot,
    qoz_UnaryOp_UOpDeref,
    qoz_UnaryOp_UOpAddr,
} qoz_UnaryOp_tag;

struct qoz_UnaryOp {
    qoz_UnaryOp_tag tag;
};

static const qoz_variant_desc qoz_UnaryOp_variants[] = {
    { qoz_UnaryOp_UOpNeg, 0, NULL },
    { qoz_UnaryOp_UOpNot, 0, NULL },
    { qoz_UnaryOp_UOpDeref, 0, NULL },
    { qoz_UnaryOp_UOpAddr, 0, NULL },
};
static const qoz_type_desc qoz_UnaryOp_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_UnaryOp), 0, NULL, (int32_t)offsetof(struct qoz_UnaryOp, tag), 0, 4, qoz_UnaryOp_variants, "UnaryOp" };

typedef enum {
    qoz_BinaryOp_BOpAdd,
    qoz_BinaryOp_BOpSub,
    qoz_BinaryOp_BOpMul,
    qoz_BinaryOp_BOpDiv,
    qoz_BinaryOp_BOpMod,
    qoz_BinaryOp_BOpEq,
    qoz_BinaryOp_BOpNe,
    qoz_BinaryOp_BOpLt,
    qoz_BinaryOp_BOpGt,
    qoz_BinaryOp_BOpLe,
    qoz_BinaryOp_BOpGe,
    qoz_BinaryOp_BOpAnd,
    qoz_BinaryOp_BOpOr,
    qoz_BinaryOp_BOpBitAnd,
    qoz_BinaryOp_BOpBitOr,
    qoz_BinaryOp_BOpBitXor,
    qoz_BinaryOp_BOpShl,
    qoz_BinaryOp_BOpShr,
    qoz_BinaryOp_BOpRange,
    qoz_BinaryOp_BOpRangeInclusive,
} qoz_BinaryOp_tag;

struct qoz_BinaryOp {
    qoz_BinaryOp_tag tag;
};

static const qoz_variant_desc qoz_BinaryOp_variants[] = {
    { qoz_BinaryOp_BOpAdd, 0, NULL },
    { qoz_BinaryOp_BOpSub, 0, NULL },
    { qoz_BinaryOp_BOpMul, 0, NULL },
    { qoz_BinaryOp_BOpDiv, 0, NULL },
    { qoz_BinaryOp_BOpMod, 0, NULL },
    { qoz_BinaryOp_BOpEq, 0, NULL },
    { qoz_BinaryOp_BOpNe, 0, NULL },
    { qoz_BinaryOp_BOpLt, 0, NULL },
    { qoz_BinaryOp_BOpGt, 0, NULL },
    { qoz_BinaryOp_BOpLe, 0, NULL },
    { qoz_BinaryOp_BOpGe, 0, NULL },
    { qoz_BinaryOp_BOpAnd, 0, NULL },
    { qoz_BinaryOp_BOpOr, 0, NULL },
    { qoz_BinaryOp_BOpBitAnd, 0, NULL },
    { qoz_BinaryOp_BOpBitOr, 0, NULL },
    { qoz_BinaryOp_BOpBitXor, 0, NULL },
    { qoz_BinaryOp_BOpShl, 0, NULL },
    { qoz_BinaryOp_BOpShr, 0, NULL },
    { qoz_BinaryOp_BOpRange, 0, NULL },
    { qoz_BinaryOp_BOpRangeInclusive, 0, NULL },
};
static const qoz_type_desc qoz_BinaryOp_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_BinaryOp), 0, NULL, (int32_t)offsetof(struct qoz_BinaryOp, tag), 0, 20, qoz_BinaryOp_variants, "BinaryOp" };

typedef enum {
    qoz_AssignOp_AOpSet,
    qoz_AssignOp_AOpAddSet,
    qoz_AssignOp_AOpSubSet,
    qoz_AssignOp_AOpMulSet,
    qoz_AssignOp_AOpDivSet,
    qoz_AssignOp_AOpModSet,
} qoz_AssignOp_tag;

struct qoz_AssignOp {
    qoz_AssignOp_tag tag;
};

static const qoz_variant_desc qoz_AssignOp_variants[] = {
    { qoz_AssignOp_AOpSet, 0, NULL },
    { qoz_AssignOp_AOpAddSet, 0, NULL },
    { qoz_AssignOp_AOpSubSet, 0, NULL },
    { qoz_AssignOp_AOpMulSet, 0, NULL },
    { qoz_AssignOp_AOpDivSet, 0, NULL },
    { qoz_AssignOp_AOpModSet, 0, NULL },
};
static const qoz_type_desc qoz_AssignOp_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_AssignOp), 0, NULL, (int32_t)offsetof(struct qoz_AssignOp, tag), 0, 6, qoz_AssignOp_variants, "AssignOp" };

typedef enum {
    qoz_TypeExpr_TENamed,
    qoz_TypeExpr_TEPtr,
    qoz_TypeExpr_TETuple,
    qoz_TypeExpr_TEUnit,
    qoz_TypeExpr_TEFn,
} qoz_TypeExpr_tag;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_string f1;
    qoz_Vec__qoz_TypeExpr f2;
} qoz_TypeExpr_TENamed_payload;

typedef struct {
    qoz_Span f0;
    qoz_TypeExpr* f1;
} qoz_TypeExpr_TEPtr_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_TypeExpr f1;
} qoz_TypeExpr_TETuple_payload;

typedef struct {
    qoz_Span f0;
} qoz_TypeExpr_TEUnit_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_TypeExpr f1;
    qoz_TypeExpr* f2;
} qoz_TypeExpr_TEFn_payload;

struct qoz_TypeExpr {
    qoz_TypeExpr_tag tag;
    union {
        qoz_TypeExpr_TENamed_payload TENamed;
        qoz_TypeExpr_TEPtr_payload TEPtr;
        qoz_TypeExpr_TETuple_payload TETuple;
        qoz_TypeExpr_TEUnit_payload TEUnit;
        qoz_TypeExpr_TEFn_payload TEFn;
    } payload;
};

static const int32_t qoz_TypeExpr_TENamed_offsets[] = { (int32_t)(offsetof(qoz_TypeExpr_TENamed_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_TypeExpr_TENamed_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_TypeExpr_TENamed_payload, f1), (int32_t)offsetof(qoz_TypeExpr_TENamed_payload, f2) };
static const int32_t qoz_TypeExpr_TEPtr_offsets[] = { (int32_t)(offsetof(qoz_TypeExpr_TEPtr_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_TypeExpr_TEPtr_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_TypeExpr_TEPtr_payload, f1) };
static const int32_t qoz_TypeExpr_TETuple_offsets[] = { (int32_t)(offsetof(qoz_TypeExpr_TETuple_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_TypeExpr_TETuple_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_TypeExpr_TETuple_payload, f1) };
static const int32_t qoz_TypeExpr_TEUnit_offsets[] = { (int32_t)(offsetof(qoz_TypeExpr_TEUnit_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_TypeExpr_TEUnit_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))) };
static const int32_t qoz_TypeExpr_TEFn_offsets[] = { (int32_t)(offsetof(qoz_TypeExpr_TEFn_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_TypeExpr_TEFn_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_TypeExpr_TEFn_payload, f1), (int32_t)offsetof(qoz_TypeExpr_TEFn_payload, f2) };
static const qoz_variant_desc qoz_TypeExpr_variants[] = {
    { qoz_TypeExpr_TENamed, 4, qoz_TypeExpr_TENamed_offsets },
    { qoz_TypeExpr_TEPtr, 3, qoz_TypeExpr_TEPtr_offsets },
    { qoz_TypeExpr_TETuple, 3, qoz_TypeExpr_TETuple_offsets },
    { qoz_TypeExpr_TEUnit, 2, qoz_TypeExpr_TEUnit_offsets },
    { qoz_TypeExpr_TEFn, 4, qoz_TypeExpr_TEFn_offsets },
};
static const qoz_type_desc qoz_TypeExpr_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_TypeExpr), 0, NULL, (int32_t)offsetof(struct qoz_TypeExpr, tag), (int32_t)offsetof(struct qoz_TypeExpr, payload), 5, qoz_TypeExpr_variants, "TypeExpr" };

typedef enum {
    qoz_Pattern_PatWild,
    qoz_Pattern_PatBind,
    qoz_Pattern_PatLitInt,
    qoz_Pattern_PatLitString,
    qoz_Pattern_PatLitBool,
    qoz_Pattern_PatVariant,
    qoz_Pattern_PatTuple,
} qoz_Pattern_tag;

typedef struct {
    qoz_Span f0;
} qoz_Pattern_PatWild_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
} qoz_Pattern_PatBind_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
} qoz_Pattern_PatLitInt_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
} qoz_Pattern_PatLitString_payload;

typedef struct {
    qoz_Span f0;
    bool f1;
} qoz_Pattern_PatLitBool_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_string f1;
    qoz_Vec__qoz_Pattern f2;
} qoz_Pattern_PatVariant_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_Pattern f1;
} qoz_Pattern_PatTuple_payload;

struct qoz_Pattern {
    qoz_Pattern_tag tag;
    union {
        qoz_Pattern_PatWild_payload PatWild;
        qoz_Pattern_PatBind_payload PatBind;
        qoz_Pattern_PatLitInt_payload PatLitInt;
        qoz_Pattern_PatLitString_payload PatLitString;
        qoz_Pattern_PatLitBool_payload PatLitBool;
        qoz_Pattern_PatVariant_payload PatVariant;
        qoz_Pattern_PatTuple_payload PatTuple;
    } payload;
};

static const int32_t qoz_Pattern_PatWild_offsets[] = { (int32_t)(offsetof(qoz_Pattern_PatWild_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Pattern_PatWild_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))) };
static const int32_t qoz_Pattern_PatBind_offsets[] = { (int32_t)(offsetof(qoz_Pattern_PatBind_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Pattern_PatBind_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Pattern_PatBind_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Pattern_PatBind_payload, f1) + offsetof(qoz_string, root)) };
static const int32_t qoz_Pattern_PatLitInt_offsets[] = { (int32_t)(offsetof(qoz_Pattern_PatLitInt_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Pattern_PatLitInt_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Pattern_PatLitInt_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Pattern_PatLitInt_payload, f1) + offsetof(qoz_string, root)) };
static const int32_t qoz_Pattern_PatLitString_offsets[] = { (int32_t)(offsetof(qoz_Pattern_PatLitString_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Pattern_PatLitString_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Pattern_PatLitString_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Pattern_PatLitString_payload, f1) + offsetof(qoz_string, root)) };
static const int32_t qoz_Pattern_PatLitBool_offsets[] = { (int32_t)(offsetof(qoz_Pattern_PatLitBool_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Pattern_PatLitBool_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))) };
static const int32_t qoz_Pattern_PatVariant_offsets[] = { (int32_t)(offsetof(qoz_Pattern_PatVariant_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Pattern_PatVariant_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Pattern_PatVariant_payload, f1), (int32_t)offsetof(qoz_Pattern_PatVariant_payload, f2) };
static const int32_t qoz_Pattern_PatTuple_offsets[] = { (int32_t)(offsetof(qoz_Pattern_PatTuple_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Pattern_PatTuple_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Pattern_PatTuple_payload, f1) };
static const qoz_variant_desc qoz_Pattern_variants[] = {
    { qoz_Pattern_PatWild, 2, qoz_Pattern_PatWild_offsets },
    { qoz_Pattern_PatBind, 4, qoz_Pattern_PatBind_offsets },
    { qoz_Pattern_PatLitInt, 4, qoz_Pattern_PatLitInt_offsets },
    { qoz_Pattern_PatLitString, 4, qoz_Pattern_PatLitString_offsets },
    { qoz_Pattern_PatLitBool, 2, qoz_Pattern_PatLitBool_offsets },
    { qoz_Pattern_PatVariant, 4, qoz_Pattern_PatVariant_offsets },
    { qoz_Pattern_PatTuple, 3, qoz_Pattern_PatTuple_offsets },
};
static const qoz_type_desc qoz_Pattern_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Pattern), 0, NULL, (int32_t)offsetof(struct qoz_Pattern, tag), (int32_t)offsetof(struct qoz_Pattern, payload), 7, qoz_Pattern_variants, "Pattern" };

typedef enum {
    qoz_Expr_EInt,
    qoz_Expr_EFloat,
    qoz_Expr_EString,
    qoz_Expr_EChar,
    qoz_Expr_EBool,
    qoz_Expr_ENil,
    qoz_Expr_EIdent,
    qoz_Expr_EPath,
    qoz_Expr_EUnary,
    qoz_Expr_EBinary,
    qoz_Expr_EAssign,
    qoz_Expr_ECall,
    qoz_Expr_EField,
    qoz_Expr_EIndex,
    qoz_Expr_ECast,
    qoz_Expr_ETry,
    qoz_Expr_ETuple,
    qoz_Expr_ERecord,
    qoz_Expr_EClosure,
    qoz_Expr_EBlock,
    qoz_Expr_EIf,
    qoz_Expr_EMatch,
    qoz_Expr_EWhile,
    qoz_Expr_EFor,
    qoz_Expr_EReturn,
    qoz_Expr_EDefer,
    qoz_Expr_ESizeOf,
    qoz_Expr_EArrayLit,
} qoz_Expr_tag;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
} qoz_Expr_EInt_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
} qoz_Expr_EFloat_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
} qoz_Expr_EString_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
} qoz_Expr_EChar_payload;

typedef struct {
    qoz_Span f0;
    bool f1;
} qoz_Expr_EBool_payload;

typedef struct {
    qoz_Span f0;
} qoz_Expr_ENil_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
} qoz_Expr_EIdent_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_string f1;
} qoz_Expr_EPath_payload;

typedef struct {
    qoz_Span f0;
    qoz_UnaryOp* f1;
    qoz_Expr* f2;
} qoz_Expr_EUnary_payload;

typedef struct {
    qoz_Span f0;
    qoz_BinaryOp* f1;
    qoz_Expr* f2;
    qoz_Expr* f3;
} qoz_Expr_EBinary_payload;

typedef struct {
    qoz_Span f0;
    qoz_AssignOp* f1;
    qoz_Expr* f2;
    qoz_Expr* f3;
} qoz_Expr_EAssign_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
    qoz_Vec__qoz_TypeExpr f2;
    qoz_Vec__qoz_Expr f3;
} qoz_Expr_ECall_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
    qoz_string f2;
} qoz_Expr_EField_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
    qoz_Expr* f2;
} qoz_Expr_EIndex_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
    qoz_TypeExpr* f2;
} qoz_Expr_ECast_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
} qoz_Expr_ETry_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_Expr f1;
} qoz_Expr_ETuple_payload;

typedef struct {
    qoz_Span f0;
    qoz_TypeExpr* f1;
    qoz_Vec__qoz_RecordFieldLit f2;
} qoz_Expr_ERecord_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_ClosureParam f1;
    qoz_TypeExpr* f2;
    qoz_Expr* f3;
} qoz_Expr_EClosure_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_Stmt f1;
    qoz_Expr* f2;
} qoz_Expr_EBlock_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
    qoz_Expr* f2;
    qoz_Expr* f3;
} qoz_Expr_EIf_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
    qoz_Vec__qoz_MatchArm f2;
} qoz_Expr_EMatch_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
    qoz_Expr* f2;
} qoz_Expr_EWhile_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_string f2;
    qoz_Expr* f3;
    qoz_Expr* f4;
} qoz_Expr_EFor_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
} qoz_Expr_EReturn_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
} qoz_Expr_EDefer_payload;

typedef struct {
    qoz_Span f0;
    qoz_TypeExpr* f1;
} qoz_Expr_ESizeOf_payload;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_Expr f1;
} qoz_Expr_EArrayLit_payload;

struct qoz_Expr {
    qoz_Expr_tag tag;
    union {
        qoz_Expr_EInt_payload EInt;
        qoz_Expr_EFloat_payload EFloat;
        qoz_Expr_EString_payload EString;
        qoz_Expr_EChar_payload EChar;
        qoz_Expr_EBool_payload EBool;
        qoz_Expr_ENil_payload ENil;
        qoz_Expr_EIdent_payload EIdent;
        qoz_Expr_EPath_payload EPath;
        qoz_Expr_EUnary_payload EUnary;
        qoz_Expr_EBinary_payload EBinary;
        qoz_Expr_EAssign_payload EAssign;
        qoz_Expr_ECall_payload ECall;
        qoz_Expr_EField_payload EField;
        qoz_Expr_EIndex_payload EIndex;
        qoz_Expr_ECast_payload ECast;
        qoz_Expr_ETry_payload ETry;
        qoz_Expr_ETuple_payload ETuple;
        qoz_Expr_ERecord_payload ERecord;
        qoz_Expr_EClosure_payload EClosure;
        qoz_Expr_EBlock_payload EBlock;
        qoz_Expr_EIf_payload EIf;
        qoz_Expr_EMatch_payload EMatch;
        qoz_Expr_EWhile_payload EWhile;
        qoz_Expr_EFor_payload EFor;
        qoz_Expr_EReturn_payload EReturn;
        qoz_Expr_EDefer_payload EDefer;
        qoz_Expr_ESizeOf_payload ESizeOf;
        qoz_Expr_EArrayLit_payload EArrayLit;
    } payload;
};

static const int32_t qoz_Expr_EInt_offsets[] = { (int32_t)(offsetof(qoz_Expr_EInt_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EInt_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Expr_EInt_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Expr_EInt_payload, f1) + offsetof(qoz_string, root)) };
static const int32_t qoz_Expr_EFloat_offsets[] = { (int32_t)(offsetof(qoz_Expr_EFloat_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EFloat_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Expr_EFloat_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Expr_EFloat_payload, f1) + offsetof(qoz_string, root)) };
static const int32_t qoz_Expr_EString_offsets[] = { (int32_t)(offsetof(qoz_Expr_EString_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EString_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Expr_EString_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Expr_EString_payload, f1) + offsetof(qoz_string, root)) };
static const int32_t qoz_Expr_EChar_offsets[] = { (int32_t)(offsetof(qoz_Expr_EChar_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EChar_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Expr_EChar_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Expr_EChar_payload, f1) + offsetof(qoz_string, root)) };
static const int32_t qoz_Expr_EBool_offsets[] = { (int32_t)(offsetof(qoz_Expr_EBool_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EBool_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))) };
static const int32_t qoz_Expr_ENil_offsets[] = { (int32_t)(offsetof(qoz_Expr_ENil_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_ENil_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))) };
static const int32_t qoz_Expr_EIdent_offsets[] = { (int32_t)(offsetof(qoz_Expr_EIdent_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EIdent_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Expr_EIdent_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Expr_EIdent_payload, f1) + offsetof(qoz_string, root)) };
static const int32_t qoz_Expr_EPath_offsets[] = { (int32_t)(offsetof(qoz_Expr_EPath_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EPath_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EPath_payload, f1) };
static const int32_t qoz_Expr_EUnary_offsets[] = { (int32_t)(offsetof(qoz_Expr_EUnary_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EUnary_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EUnary_payload, f1), (int32_t)offsetof(qoz_Expr_EUnary_payload, f2) };
static const int32_t qoz_Expr_EBinary_offsets[] = { (int32_t)(offsetof(qoz_Expr_EBinary_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EBinary_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EBinary_payload, f1), (int32_t)offsetof(qoz_Expr_EBinary_payload, f2), (int32_t)offsetof(qoz_Expr_EBinary_payload, f3) };
static const int32_t qoz_Expr_EAssign_offsets[] = { (int32_t)(offsetof(qoz_Expr_EAssign_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EAssign_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EAssign_payload, f1), (int32_t)offsetof(qoz_Expr_EAssign_payload, f2), (int32_t)offsetof(qoz_Expr_EAssign_payload, f3) };
static const int32_t qoz_Expr_ECall_offsets[] = { (int32_t)(offsetof(qoz_Expr_ECall_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_ECall_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_ECall_payload, f1), (int32_t)offsetof(qoz_Expr_ECall_payload, f2), (int32_t)offsetof(qoz_Expr_ECall_payload, f3) };
static const int32_t qoz_Expr_EField_offsets[] = { (int32_t)(offsetof(qoz_Expr_EField_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EField_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EField_payload, f1), (int32_t)(offsetof(qoz_Expr_EField_payload, f2) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Expr_EField_payload, f2) + offsetof(qoz_string, root)) };
static const int32_t qoz_Expr_EIndex_offsets[] = { (int32_t)(offsetof(qoz_Expr_EIndex_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EIndex_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EIndex_payload, f1), (int32_t)offsetof(qoz_Expr_EIndex_payload, f2) };
static const int32_t qoz_Expr_ECast_offsets[] = { (int32_t)(offsetof(qoz_Expr_ECast_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_ECast_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_ECast_payload, f1), (int32_t)offsetof(qoz_Expr_ECast_payload, f2) };
static const int32_t qoz_Expr_ETry_offsets[] = { (int32_t)(offsetof(qoz_Expr_ETry_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_ETry_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_ETry_payload, f1) };
static const int32_t qoz_Expr_ETuple_offsets[] = { (int32_t)(offsetof(qoz_Expr_ETuple_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_ETuple_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_ETuple_payload, f1) };
static const int32_t qoz_Expr_ERecord_offsets[] = { (int32_t)(offsetof(qoz_Expr_ERecord_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_ERecord_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_ERecord_payload, f1), (int32_t)offsetof(qoz_Expr_ERecord_payload, f2) };
static const int32_t qoz_Expr_EClosure_offsets[] = { (int32_t)(offsetof(qoz_Expr_EClosure_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EClosure_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EClosure_payload, f1), (int32_t)offsetof(qoz_Expr_EClosure_payload, f2), (int32_t)offsetof(qoz_Expr_EClosure_payload, f3) };
static const int32_t qoz_Expr_EBlock_offsets[] = { (int32_t)(offsetof(qoz_Expr_EBlock_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EBlock_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EBlock_payload, f1), (int32_t)offsetof(qoz_Expr_EBlock_payload, f2) };
static const int32_t qoz_Expr_EIf_offsets[] = { (int32_t)(offsetof(qoz_Expr_EIf_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EIf_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EIf_payload, f1), (int32_t)offsetof(qoz_Expr_EIf_payload, f2), (int32_t)offsetof(qoz_Expr_EIf_payload, f3) };
static const int32_t qoz_Expr_EMatch_offsets[] = { (int32_t)(offsetof(qoz_Expr_EMatch_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EMatch_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EMatch_payload, f1), (int32_t)offsetof(qoz_Expr_EMatch_payload, f2) };
static const int32_t qoz_Expr_EWhile_offsets[] = { (int32_t)(offsetof(qoz_Expr_EWhile_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EWhile_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EWhile_payload, f1), (int32_t)offsetof(qoz_Expr_EWhile_payload, f2) };
static const int32_t qoz_Expr_EFor_offsets[] = { (int32_t)(offsetof(qoz_Expr_EFor_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EFor_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Expr_EFor_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Expr_EFor_payload, f1) + offsetof(qoz_string, root)), (int32_t)(offsetof(qoz_Expr_EFor_payload, f2) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Expr_EFor_payload, f2) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Expr_EFor_payload, f3), (int32_t)offsetof(qoz_Expr_EFor_payload, f4) };
static const int32_t qoz_Expr_EReturn_offsets[] = { (int32_t)(offsetof(qoz_Expr_EReturn_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EReturn_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EReturn_payload, f1) };
static const int32_t qoz_Expr_EDefer_offsets[] = { (int32_t)(offsetof(qoz_Expr_EDefer_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EDefer_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EDefer_payload, f1) };
static const int32_t qoz_Expr_ESizeOf_offsets[] = { (int32_t)(offsetof(qoz_Expr_ESizeOf_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_ESizeOf_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_ESizeOf_payload, f1) };
static const int32_t qoz_Expr_EArrayLit_offsets[] = { (int32_t)(offsetof(qoz_Expr_EArrayLit_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Expr_EArrayLit_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Expr_EArrayLit_payload, f1) };
static const qoz_variant_desc qoz_Expr_variants[] = {
    { qoz_Expr_EInt, 4, qoz_Expr_EInt_offsets },
    { qoz_Expr_EFloat, 4, qoz_Expr_EFloat_offsets },
    { qoz_Expr_EString, 4, qoz_Expr_EString_offsets },
    { qoz_Expr_EChar, 4, qoz_Expr_EChar_offsets },
    { qoz_Expr_EBool, 2, qoz_Expr_EBool_offsets },
    { qoz_Expr_ENil, 2, qoz_Expr_ENil_offsets },
    { qoz_Expr_EIdent, 4, qoz_Expr_EIdent_offsets },
    { qoz_Expr_EPath, 3, qoz_Expr_EPath_offsets },
    { qoz_Expr_EUnary, 4, qoz_Expr_EUnary_offsets },
    { qoz_Expr_EBinary, 5, qoz_Expr_EBinary_offsets },
    { qoz_Expr_EAssign, 5, qoz_Expr_EAssign_offsets },
    { qoz_Expr_ECall, 5, qoz_Expr_ECall_offsets },
    { qoz_Expr_EField, 5, qoz_Expr_EField_offsets },
    { qoz_Expr_EIndex, 4, qoz_Expr_EIndex_offsets },
    { qoz_Expr_ECast, 4, qoz_Expr_ECast_offsets },
    { qoz_Expr_ETry, 3, qoz_Expr_ETry_offsets },
    { qoz_Expr_ETuple, 3, qoz_Expr_ETuple_offsets },
    { qoz_Expr_ERecord, 4, qoz_Expr_ERecord_offsets },
    { qoz_Expr_EClosure, 5, qoz_Expr_EClosure_offsets },
    { qoz_Expr_EBlock, 4, qoz_Expr_EBlock_offsets },
    { qoz_Expr_EIf, 5, qoz_Expr_EIf_offsets },
    { qoz_Expr_EMatch, 4, qoz_Expr_EMatch_offsets },
    { qoz_Expr_EWhile, 4, qoz_Expr_EWhile_offsets },
    { qoz_Expr_EFor, 8, qoz_Expr_EFor_offsets },
    { qoz_Expr_EReturn, 3, qoz_Expr_EReturn_offsets },
    { qoz_Expr_EDefer, 3, qoz_Expr_EDefer_offsets },
    { qoz_Expr_ESizeOf, 3, qoz_Expr_ESizeOf_offsets },
    { qoz_Expr_EArrayLit, 3, qoz_Expr_EArrayLit_offsets },
};
static const qoz_type_desc qoz_Expr_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Expr), 0, NULL, (int32_t)offsetof(struct qoz_Expr, tag), (int32_t)offsetof(struct qoz_Expr, payload), 28, qoz_Expr_variants, "Expr" };

typedef enum {
    qoz_Stmt_SLet,
    qoz_Stmt_SVar,
    qoz_Stmt_SExpr,
} qoz_Stmt_tag;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_TypeExpr* f2;
    qoz_Expr* f3;
} qoz_Stmt_SLet_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_TypeExpr* f2;
    qoz_Expr* f3;
} qoz_Stmt_SVar_payload;

typedef struct {
    qoz_Span f0;
    qoz_Expr* f1;
} qoz_Stmt_SExpr_payload;

struct qoz_Stmt {
    qoz_Stmt_tag tag;
    union {
        qoz_Stmt_SLet_payload SLet;
        qoz_Stmt_SVar_payload SVar;
        qoz_Stmt_SExpr_payload SExpr;
    } payload;
};

static const int32_t qoz_Stmt_SLet_offsets[] = { (int32_t)(offsetof(qoz_Stmt_SLet_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Stmt_SLet_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Stmt_SLet_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Stmt_SLet_payload, f1) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Stmt_SLet_payload, f2), (int32_t)offsetof(qoz_Stmt_SLet_payload, f3) };
static const int32_t qoz_Stmt_SVar_offsets[] = { (int32_t)(offsetof(qoz_Stmt_SVar_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Stmt_SVar_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Stmt_SVar_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Stmt_SVar_payload, f1) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Stmt_SVar_payload, f2), (int32_t)offsetof(qoz_Stmt_SVar_payload, f3) };
static const int32_t qoz_Stmt_SExpr_offsets[] = { (int32_t)(offsetof(qoz_Stmt_SExpr_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Stmt_SExpr_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Stmt_SExpr_payload, f1) };
static const qoz_variant_desc qoz_Stmt_variants[] = {
    { qoz_Stmt_SLet, 6, qoz_Stmt_SLet_offsets },
    { qoz_Stmt_SVar, 6, qoz_Stmt_SVar_offsets },
    { qoz_Stmt_SExpr, 3, qoz_Stmt_SExpr_offsets },
};
static const qoz_type_desc qoz_Stmt_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Stmt), 0, NULL, (int32_t)offsetof(struct qoz_Stmt, tag), (int32_t)offsetof(struct qoz_Stmt, payload), 3, qoz_Stmt_variants, "Stmt" };

typedef enum {
    qoz_VariantPayloadKind_VPKNone,
    qoz_VariantPayloadKind_VPKPositional,
    qoz_VariantPayloadKind_VPKNamed,
} qoz_VariantPayloadKind_tag;

struct qoz_VariantPayloadKind {
    qoz_VariantPayloadKind_tag tag;
};

static const qoz_variant_desc qoz_VariantPayloadKind_variants[] = {
    { qoz_VariantPayloadKind_VPKNone, 0, NULL },
    { qoz_VariantPayloadKind_VPKPositional, 0, NULL },
    { qoz_VariantPayloadKind_VPKNamed, 0, NULL },
};
static const qoz_type_desc qoz_VariantPayloadKind_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_VariantPayloadKind), 0, NULL, (int32_t)offsetof(struct qoz_VariantPayloadKind, tag), 0, 3, qoz_VariantPayloadKind_variants, "VariantPayloadKind" };

typedef enum {
    qoz_LinkKind_LinkLibrary,
    qoz_LinkKind_LinkFramework,
} qoz_LinkKind_tag;

struct qoz_LinkKind {
    qoz_LinkKind_tag tag;
};

static const qoz_variant_desc qoz_LinkKind_variants[] = {
    { qoz_LinkKind_LinkLibrary, 0, NULL },
    { qoz_LinkKind_LinkFramework, 0, NULL },
};
static const qoz_type_desc qoz_LinkKind_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_LinkKind), 0, NULL, (int32_t)offsetof(struct qoz_LinkKind, tag), 0, 2, qoz_LinkKind_variants, "LinkKind" };

typedef enum {
    qoz_Decl_DImport,
    qoz_Decl_DFn,
    qoz_Decl_DStruct,
    qoz_Decl_DEnum,
    qoz_Decl_DTypeAlias,
    qoz_Decl_DConst,
    qoz_Decl_DExternal,
    qoz_Decl_DLink,
} qoz_Decl_tag;

typedef struct {
    qoz_Span f0;
    qoz_Vec__qoz_string f1;
    qoz_string f2;
} qoz_Decl_DImport_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_Vec__qoz_string f2;
    qoz_Vec__qoz_FnParam f3;
    qoz_TypeExpr* f4;
    qoz_Expr* f5;
    qoz_string f6;
} qoz_Decl_DFn_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_Vec__qoz_string f2;
    qoz_Vec__qoz_StructField f3;
} qoz_Decl_DStruct_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_Vec__qoz_string f2;
    qoz_Vec__qoz_VariantDecl f3;
} qoz_Decl_DEnum_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_Vec__qoz_string f2;
    qoz_TypeExpr* f3;
} qoz_Decl_DTypeAlias_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_TypeExpr* f2;
    qoz_Expr* f3;
} qoz_Decl_DConst_payload;

typedef struct {
    qoz_Span f0;
    qoz_string f1;
    qoz_string f2;
    qoz_Vec__qoz_FnParam f3;
    qoz_TypeExpr* f4;
} qoz_Decl_DExternal_payload;

typedef struct {
    qoz_Span f0;
    qoz_LinkKind* f1;
    qoz_string f2;
} qoz_Decl_DLink_payload;

struct qoz_Decl {
    qoz_Decl_tag tag;
    union {
        qoz_Decl_DImport_payload DImport;
        qoz_Decl_DFn_payload DFn;
        qoz_Decl_DStruct_payload DStruct;
        qoz_Decl_DEnum_payload DEnum;
        qoz_Decl_DTypeAlias_payload DTypeAlias;
        qoz_Decl_DConst_payload DConst;
        qoz_Decl_DExternal_payload DExternal;
        qoz_Decl_DLink_payload DLink;
    } payload;
};

static const int32_t qoz_Decl_DImport_offsets[] = { (int32_t)(offsetof(qoz_Decl_DImport_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Decl_DImport_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Decl_DImport_payload, f1), (int32_t)(offsetof(qoz_Decl_DImport_payload, f2) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DImport_payload, f2) + offsetof(qoz_string, root)) };
static const int32_t qoz_Decl_DFn_offsets[] = { (int32_t)(offsetof(qoz_Decl_DFn_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Decl_DFn_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Decl_DFn_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DFn_payload, f1) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Decl_DFn_payload, f2), (int32_t)offsetof(qoz_Decl_DFn_payload, f3), (int32_t)offsetof(qoz_Decl_DFn_payload, f4), (int32_t)offsetof(qoz_Decl_DFn_payload, f5), (int32_t)(offsetof(qoz_Decl_DFn_payload, f6) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DFn_payload, f6) + offsetof(qoz_string, root)) };
static const int32_t qoz_Decl_DStruct_offsets[] = { (int32_t)(offsetof(qoz_Decl_DStruct_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Decl_DStruct_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Decl_DStruct_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DStruct_payload, f1) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Decl_DStruct_payload, f2), (int32_t)offsetof(qoz_Decl_DStruct_payload, f3) };
static const int32_t qoz_Decl_DEnum_offsets[] = { (int32_t)(offsetof(qoz_Decl_DEnum_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Decl_DEnum_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Decl_DEnum_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DEnum_payload, f1) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Decl_DEnum_payload, f2), (int32_t)offsetof(qoz_Decl_DEnum_payload, f3) };
static const int32_t qoz_Decl_DTypeAlias_offsets[] = { (int32_t)(offsetof(qoz_Decl_DTypeAlias_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Decl_DTypeAlias_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Decl_DTypeAlias_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DTypeAlias_payload, f1) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Decl_DTypeAlias_payload, f2), (int32_t)offsetof(qoz_Decl_DTypeAlias_payload, f3) };
static const int32_t qoz_Decl_DConst_offsets[] = { (int32_t)(offsetof(qoz_Decl_DConst_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Decl_DConst_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Decl_DConst_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DConst_payload, f1) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Decl_DConst_payload, f2), (int32_t)offsetof(qoz_Decl_DConst_payload, f3) };
static const int32_t qoz_Decl_DExternal_offsets[] = { (int32_t)(offsetof(qoz_Decl_DExternal_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Decl_DExternal_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)(offsetof(qoz_Decl_DExternal_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DExternal_payload, f1) + offsetof(qoz_string, root)), (int32_t)(offsetof(qoz_Decl_DExternal_payload, f2) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DExternal_payload, f2) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Decl_DExternal_payload, f3), (int32_t)offsetof(qoz_Decl_DExternal_payload, f4) };
static const int32_t qoz_Decl_DLink_offsets[] = { (int32_t)(offsetof(qoz_Decl_DLink_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, data))), (int32_t)(offsetof(qoz_Decl_DLink_payload, f0) + (offsetof(struct qoz_Span, file) + offsetof(qoz_string, root))), (int32_t)offsetof(qoz_Decl_DLink_payload, f1), (int32_t)(offsetof(qoz_Decl_DLink_payload, f2) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Decl_DLink_payload, f2) + offsetof(qoz_string, root)) };
static const qoz_variant_desc qoz_Decl_variants[] = {
    { qoz_Decl_DImport, 5, qoz_Decl_DImport_offsets },
    { qoz_Decl_DFn, 10, qoz_Decl_DFn_offsets },
    { qoz_Decl_DStruct, 6, qoz_Decl_DStruct_offsets },
    { qoz_Decl_DEnum, 6, qoz_Decl_DEnum_offsets },
    { qoz_Decl_DTypeAlias, 6, qoz_Decl_DTypeAlias_offsets },
    { qoz_Decl_DConst, 6, qoz_Decl_DConst_offsets },
    { qoz_Decl_DExternal, 8, qoz_Decl_DExternal_offsets },
    { qoz_Decl_DLink, 5, qoz_Decl_DLink_offsets },
};
static const qoz_type_desc qoz_Decl_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Decl), 0, NULL, (int32_t)offsetof(struct qoz_Decl, tag), (int32_t)offsetof(struct qoz_Decl, payload), 8, qoz_Decl_variants, "Decl" };

typedef enum {
    qoz_MainRetKind_MainRetUnit,
    qoz_MainRetKind_MainRetInt,
} qoz_MainRetKind_tag;

typedef struct {
    qoz_string f0;
} qoz_MainRetKind_MainRetInt_payload;

struct qoz_MainRetKind {
    qoz_MainRetKind_tag tag;
    union {
        qoz_MainRetKind_MainRetInt_payload MainRetInt;
    } payload;
};

static const int32_t qoz_MainRetKind_MainRetInt_offsets[] = { (int32_t)(offsetof(qoz_MainRetKind_MainRetInt_payload, f0) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_MainRetKind_MainRetInt_payload, f0) + offsetof(qoz_string, root)) };
static const qoz_variant_desc qoz_MainRetKind_variants[] = {
    { qoz_MainRetKind_MainRetUnit, 0, NULL },
    { qoz_MainRetKind_MainRetInt, 2, qoz_MainRetKind_MainRetInt_offsets },
};
static const qoz_type_desc qoz_MainRetKind_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_MainRetKind), 0, NULL, (int32_t)offsetof(struct qoz_MainRetKind, tag), (int32_t)offsetof(struct qoz_MainRetKind, payload), 2, qoz_MainRetKind_variants, "MainRetKind" };

typedef enum {
    qoz_Ty_TyInt,
    qoz_Ty_TyFloat,
    qoz_Ty_TyBool,
    qoz_Ty_TyChar,
    qoz_Ty_TyString,
    qoz_Ty_TyCstring,
    qoz_Ty_TyUnit,
    qoz_Ty_TyNil,
    qoz_Ty_TyError,
    qoz_Ty_TyPtr,
    qoz_Ty_TyAdt,
    qoz_Ty_TyRecord,
    qoz_Ty_TyFn,
    qoz_Ty_TyTuple,
    qoz_Ty_TyVar,
} qoz_Ty_tag;

typedef struct {
    qoz_IntInfo f0;
} qoz_Ty_TyInt_payload;

typedef struct {
    qoz_FloatInfo f0;
} qoz_Ty_TyFloat_payload;

typedef struct {
    qoz_Ty* f0;
} qoz_Ty_TyPtr_payload;

typedef struct {
    qoz_string f0;
    qoz_Vec__qoz_Ty f1;
} qoz_Ty_TyAdt_payload;

typedef struct {
    qoz_string f0;
    qoz_Vec__qoz_Ty f1;
} qoz_Ty_TyRecord_payload;

typedef struct {
    qoz_Vec__qoz_Ty f0;
    qoz_Ty* f1;
} qoz_Ty_TyFn_payload;

typedef struct {
    qoz_Vec__qoz_Ty f0;
} qoz_Ty_TyTuple_payload;

typedef struct {
    int64_t f0;
    qoz_string f1;
} qoz_Ty_TyVar_payload;

struct qoz_Ty {
    qoz_Ty_tag tag;
    union {
        qoz_Ty_TyInt_payload TyInt;
        qoz_Ty_TyFloat_payload TyFloat;
        qoz_Ty_TyPtr_payload TyPtr;
        qoz_Ty_TyAdt_payload TyAdt;
        qoz_Ty_TyRecord_payload TyRecord;
        qoz_Ty_TyFn_payload TyFn;
        qoz_Ty_TyTuple_payload TyTuple;
        qoz_Ty_TyVar_payload TyVar;
    } payload;
};

static const int32_t qoz_Ty_TyPtr_offsets[] = { (int32_t)offsetof(qoz_Ty_TyPtr_payload, f0) };
static const int32_t qoz_Ty_TyAdt_offsets[] = { (int32_t)(offsetof(qoz_Ty_TyAdt_payload, f0) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Ty_TyAdt_payload, f0) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Ty_TyAdt_payload, f1) };
static const int32_t qoz_Ty_TyRecord_offsets[] = { (int32_t)(offsetof(qoz_Ty_TyRecord_payload, f0) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Ty_TyRecord_payload, f0) + offsetof(qoz_string, root)), (int32_t)offsetof(qoz_Ty_TyRecord_payload, f1) };
static const int32_t qoz_Ty_TyFn_offsets[] = { (int32_t)offsetof(qoz_Ty_TyFn_payload, f0), (int32_t)offsetof(qoz_Ty_TyFn_payload, f1) };
static const int32_t qoz_Ty_TyTuple_offsets[] = { (int32_t)offsetof(qoz_Ty_TyTuple_payload, f0) };
static const int32_t qoz_Ty_TyVar_offsets[] = { (int32_t)(offsetof(qoz_Ty_TyVar_payload, f1) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Ty_TyVar_payload, f1) + offsetof(qoz_string, root)) };
static const qoz_variant_desc qoz_Ty_variants[] = {
    { qoz_Ty_TyInt, 0, NULL },
    { qoz_Ty_TyFloat, 0, NULL },
    { qoz_Ty_TyBool, 0, NULL },
    { qoz_Ty_TyChar, 0, NULL },
    { qoz_Ty_TyString, 0, NULL },
    { qoz_Ty_TyCstring, 0, NULL },
    { qoz_Ty_TyUnit, 0, NULL },
    { qoz_Ty_TyNil, 0, NULL },
    { qoz_Ty_TyError, 0, NULL },
    { qoz_Ty_TyPtr, 1, qoz_Ty_TyPtr_offsets },
    { qoz_Ty_TyAdt, 3, qoz_Ty_TyAdt_offsets },
    { qoz_Ty_TyRecord, 3, qoz_Ty_TyRecord_offsets },
    { qoz_Ty_TyFn, 2, qoz_Ty_TyFn_offsets },
    { qoz_Ty_TyTuple, 1, qoz_Ty_TyTuple_offsets },
    { qoz_Ty_TyVar, 2, qoz_Ty_TyVar_offsets },
};
static const qoz_type_desc qoz_Ty_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Ty), 0, NULL, (int32_t)offsetof(struct qoz_Ty, tag), (int32_t)offsetof(struct qoz_Ty, payload), 15, qoz_Ty_variants, "Ty" };

typedef enum {
    qoz_Option__qoz_TypeExpr_Some,
    qoz_Option__qoz_TypeExpr_None,
} qoz_Option__qoz_TypeExpr_tag;

typedef struct {
    qoz_TypeExpr* f0;
} qoz_Option__qoz_TypeExpr_Some_payload;

struct qoz_Option__qoz_TypeExpr {
    qoz_Option__qoz_TypeExpr_tag tag;
    union {
        qoz_Option__qoz_TypeExpr_Some_payload Some;
    } payload;
};

static const int32_t qoz_Option__qoz_TypeExpr_Some_offsets[] = { (int32_t)offsetof(qoz_Option__qoz_TypeExpr_Some_payload, f0) };
static const qoz_variant_desc qoz_Option__qoz_TypeExpr_variants[] = {
    { qoz_Option__qoz_TypeExpr_Some, 1, qoz_Option__qoz_TypeExpr_Some_offsets },
    { qoz_Option__qoz_TypeExpr_None, 0, NULL },
};
static const qoz_type_desc qoz_Option__qoz_TypeExpr_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Option__qoz_TypeExpr), 0, NULL, (int32_t)offsetof(struct qoz_Option__qoz_TypeExpr, tag), (int32_t)offsetof(struct qoz_Option__qoz_TypeExpr, payload), 2, qoz_Option__qoz_TypeExpr_variants, "Option__qoz_TypeExpr" };

static qoz_Option__qoz_TypeExpr *qoz_make_Option__qoz_TypeExpr_Some(qoz_TypeExpr* f0) {
    qoz_Option__qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_TypeExpr), &qoz_Option__qoz_TypeExpr_desc);
    p->tag = qoz_Option__qoz_TypeExpr_Some;
    p->payload.Some.f0 = f0;
    return p;
}

static qoz_Option__qoz_TypeExpr *qoz_make_Option__qoz_TypeExpr_None(void) {
    qoz_Option__qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_TypeExpr), &qoz_Option__qoz_TypeExpr_desc);
    p->tag = qoz_Option__qoz_TypeExpr_None;
    return p;
}

typedef enum {
    qoz_Option__qoz_string_Some,
    qoz_Option__qoz_string_None,
} qoz_Option__qoz_string_tag;

typedef struct {
    qoz_string f0;
} qoz_Option__qoz_string_Some_payload;

struct qoz_Option__qoz_string {
    qoz_Option__qoz_string_tag tag;
    union {
        qoz_Option__qoz_string_Some_payload Some;
    } payload;
};

static const int32_t qoz_Option__qoz_string_Some_offsets[] = { (int32_t)(offsetof(qoz_Option__qoz_string_Some_payload, f0) + offsetof(qoz_string, data)), (int32_t)(offsetof(qoz_Option__qoz_string_Some_payload, f0) + offsetof(qoz_string, root)) };
static const qoz_variant_desc qoz_Option__qoz_string_variants[] = {
    { qoz_Option__qoz_string_Some, 2, qoz_Option__qoz_string_Some_offsets },
    { qoz_Option__qoz_string_None, 0, NULL },
};
static const qoz_type_desc qoz_Option__qoz_string_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Option__qoz_string), 0, NULL, (int32_t)offsetof(struct qoz_Option__qoz_string, tag), (int32_t)offsetof(struct qoz_Option__qoz_string, payload), 2, qoz_Option__qoz_string_variants, "Option__qoz_string" };

static qoz_Option__qoz_string *qoz_make_Option__qoz_string_Some(qoz_string f0) {
    qoz_Option__qoz_string *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_string), &qoz_Option__qoz_string_desc);
    p->tag = qoz_Option__qoz_string_Some;
    p->payload.Some.f0 = f0;
    return p;
}

static qoz_Option__qoz_string *qoz_make_Option__qoz_string_None(void) {
    qoz_Option__qoz_string *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_string), &qoz_Option__qoz_string_desc);
    p->tag = qoz_Option__qoz_string_None;
    return p;
}

typedef enum {
    qoz_Option__bool_Some,
    qoz_Option__bool_None,
} qoz_Option__bool_tag;

typedef struct {
    bool f0;
} qoz_Option__bool_Some_payload;

struct qoz_Option__bool {
    qoz_Option__bool_tag tag;
    union {
        qoz_Option__bool_Some_payload Some;
    } payload;
};

static const qoz_variant_desc qoz_Option__bool_variants[] = {
    { qoz_Option__bool_Some, 0, NULL },
    { qoz_Option__bool_None, 0, NULL },
};
static const qoz_type_desc qoz_Option__bool_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Option__bool), 0, NULL, (int32_t)offsetof(struct qoz_Option__bool, tag), (int32_t)offsetof(struct qoz_Option__bool, payload), 2, qoz_Option__bool_variants, "Option__bool" };

static qoz_Option__bool *qoz_make_Option__bool_Some(bool f0) {
    qoz_Option__bool *p = qoz_gc_alloc(sizeof(qoz_Option__bool), &qoz_Option__bool_desc);
    p->tag = qoz_Option__bool_Some;
    p->payload.Some.f0 = f0;
    return p;
}

static qoz_Option__bool *qoz_make_Option__bool_None(void) {
    qoz_Option__bool *p = qoz_gc_alloc(sizeof(qoz_Option__bool), &qoz_Option__bool_desc);
    p->tag = qoz_Option__bool_None;
    return p;
}

typedef enum {
    qoz_Option__qoz_Decl_Some,
    qoz_Option__qoz_Decl_None,
} qoz_Option__qoz_Decl_tag;

typedef struct {
    qoz_Decl* f0;
} qoz_Option__qoz_Decl_Some_payload;

struct qoz_Option__qoz_Decl {
    qoz_Option__qoz_Decl_tag tag;
    union {
        qoz_Option__qoz_Decl_Some_payload Some;
    } payload;
};

static const int32_t qoz_Option__qoz_Decl_Some_offsets[] = { (int32_t)offsetof(qoz_Option__qoz_Decl_Some_payload, f0) };
static const qoz_variant_desc qoz_Option__qoz_Decl_variants[] = {
    { qoz_Option__qoz_Decl_Some, 1, qoz_Option__qoz_Decl_Some_offsets },
    { qoz_Option__qoz_Decl_None, 0, NULL },
};
static const qoz_type_desc qoz_Option__qoz_Decl_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Option__qoz_Decl), 0, NULL, (int32_t)offsetof(struct qoz_Option__qoz_Decl, tag), (int32_t)offsetof(struct qoz_Option__qoz_Decl, payload), 2, qoz_Option__qoz_Decl_variants, "Option__qoz_Decl" };

static qoz_Option__qoz_Decl *qoz_make_Option__qoz_Decl_Some(qoz_Decl* f0) {
    qoz_Option__qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_Decl), &qoz_Option__qoz_Decl_desc);
    p->tag = qoz_Option__qoz_Decl_Some;
    p->payload.Some.f0 = f0;
    return p;
}

static qoz_Option__qoz_Decl *qoz_make_Option__qoz_Decl_None(void) {
    qoz_Option__qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_Decl), &qoz_Option__qoz_Decl_desc);
    p->tag = qoz_Option__qoz_Decl_None;
    return p;
}

typedef enum {
    qoz_Option__qoz_Ty_Some,
    qoz_Option__qoz_Ty_None,
} qoz_Option__qoz_Ty_tag;

typedef struct {
    qoz_Ty* f0;
} qoz_Option__qoz_Ty_Some_payload;

struct qoz_Option__qoz_Ty {
    qoz_Option__qoz_Ty_tag tag;
    union {
        qoz_Option__qoz_Ty_Some_payload Some;
    } payload;
};

static const int32_t qoz_Option__qoz_Ty_Some_offsets[] = { (int32_t)offsetof(qoz_Option__qoz_Ty_Some_payload, f0) };
static const qoz_variant_desc qoz_Option__qoz_Ty_variants[] = {
    { qoz_Option__qoz_Ty_Some, 1, qoz_Option__qoz_Ty_Some_offsets },
    { qoz_Option__qoz_Ty_None, 0, NULL },
};
static const qoz_type_desc qoz_Option__qoz_Ty_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Option__qoz_Ty), 0, NULL, (int32_t)offsetof(struct qoz_Option__qoz_Ty, tag), (int32_t)offsetof(struct qoz_Option__qoz_Ty, payload), 2, qoz_Option__qoz_Ty_variants, "Option__qoz_Ty" };

static qoz_Option__qoz_Ty *qoz_make_Option__qoz_Ty_Some(qoz_Ty* f0) {
    qoz_Option__qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_Ty), &qoz_Option__qoz_Ty_desc);
    p->tag = qoz_Option__qoz_Ty_Some;
    p->payload.Some.f0 = f0;
    return p;
}

static qoz_Option__qoz_Ty *qoz_make_Option__qoz_Ty_None(void) {
    qoz_Option__qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_Ty), &qoz_Option__qoz_Ty_desc);
    p->tag = qoz_Option__qoz_Ty_None;
    return p;
}

typedef enum {
    qoz_Option__qoz_Vec__qoz_TypeExpr_Some,
    qoz_Option__qoz_Vec__qoz_TypeExpr_None,
} qoz_Option__qoz_Vec__qoz_TypeExpr_tag;

typedef struct {
    qoz_Vec__qoz_TypeExpr f0;
} qoz_Option__qoz_Vec__qoz_TypeExpr_Some_payload;

struct qoz_Option__qoz_Vec__qoz_TypeExpr {
    qoz_Option__qoz_Vec__qoz_TypeExpr_tag tag;
    union {
        qoz_Option__qoz_Vec__qoz_TypeExpr_Some_payload Some;
    } payload;
};

static const int32_t qoz_Option__qoz_Vec__qoz_TypeExpr_Some_offsets[] = { (int32_t)offsetof(qoz_Option__qoz_Vec__qoz_TypeExpr_Some_payload, f0) };
static const qoz_variant_desc qoz_Option__qoz_Vec__qoz_TypeExpr_variants[] = {
    { qoz_Option__qoz_Vec__qoz_TypeExpr_Some, 1, qoz_Option__qoz_Vec__qoz_TypeExpr_Some_offsets },
    { qoz_Option__qoz_Vec__qoz_TypeExpr_None, 0, NULL },
};
static const qoz_type_desc qoz_Option__qoz_Vec__qoz_TypeExpr_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_Option__qoz_Vec__qoz_TypeExpr), 0, NULL, (int32_t)offsetof(struct qoz_Option__qoz_Vec__qoz_TypeExpr, tag), (int32_t)offsetof(struct qoz_Option__qoz_Vec__qoz_TypeExpr, payload), 2, qoz_Option__qoz_Vec__qoz_TypeExpr_variants, "Option__qoz_Vec__qoz_TypeExpr" };

static qoz_Option__qoz_Vec__qoz_TypeExpr *qoz_make_Option__qoz_Vec__qoz_TypeExpr_Some(qoz_Vec__qoz_TypeExpr f0) {
    qoz_Option__qoz_Vec__qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_Vec__qoz_TypeExpr), &qoz_Option__qoz_Vec__qoz_TypeExpr_desc);
    p->tag = qoz_Option__qoz_Vec__qoz_TypeExpr_Some;
    p->payload.Some.f0 = f0;
    return p;
}

static qoz_Option__qoz_Vec__qoz_TypeExpr *qoz_make_Option__qoz_Vec__qoz_TypeExpr_None(void) {
    qoz_Option__qoz_Vec__qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_Option__qoz_Vec__qoz_TypeExpr), &qoz_Option__qoz_Vec__qoz_TypeExpr_desc);
    p->tag = qoz_Option__qoz_Vec__qoz_TypeExpr_None;
    return p;
}

static qoz_TokenKind *qoz_make_TokenKind_TokIdent(void) {
    qoz_TokenKind *p = qoz_gc_alloc(sizeof(qoz_TokenKind), &qoz_TokenKind_desc);
    p->tag = qoz_TokenKind_TokIdent;
    return p;
}

static qoz_TokenKind *qoz_make_TokenKind_TokInt(void) {
    qoz_TokenKind *p = qoz_gc_alloc(sizeof(qoz_TokenKind), &qoz_TokenKind_desc);
    p->tag = qoz_TokenKind_TokInt;
    return p;
}

static qoz_TokenKind *qoz_make_TokenKind_TokFloat(void) {
    qoz_TokenKind *p = qoz_gc_alloc(sizeof(qoz_TokenKind), &qoz_TokenKind_desc);
    p->tag = qoz_TokenKind_TokFloat;
    return p;
}

static qoz_TokenKind *qoz_make_TokenKind_TokString(void) {
    qoz_TokenKind *p = qoz_gc_alloc(sizeof(qoz_TokenKind), &qoz_TokenKind_desc);
    p->tag = qoz_TokenKind_TokString;
    return p;
}

static qoz_TokenKind *qoz_make_TokenKind_TokChar(void) {
    qoz_TokenKind *p = qoz_gc_alloc(sizeof(qoz_TokenKind), &qoz_TokenKind_desc);
    p->tag = qoz_TokenKind_TokChar;
    return p;
}

static qoz_TokenKind *qoz_make_TokenKind_TokPunct(void) {
    qoz_TokenKind *p = qoz_gc_alloc(sizeof(qoz_TokenKind), &qoz_TokenKind_desc);
    p->tag = qoz_TokenKind_TokPunct;
    return p;
}

static qoz_TokenKind *qoz_make_TokenKind_TokKeyword(void) {
    qoz_TokenKind *p = qoz_gc_alloc(sizeof(qoz_TokenKind), &qoz_TokenKind_desc);
    p->tag = qoz_TokenKind_TokKeyword;
    return p;
}

static qoz_TokenKind *qoz_make_TokenKind_TokEOF(void) {
    qoz_TokenKind *p = qoz_gc_alloc(sizeof(qoz_TokenKind), &qoz_TokenKind_desc);
    p->tag = qoz_TokenKind_TokEOF;
    return p;
}

static qoz_UnaryOp *qoz_make_UnaryOp_UOpNeg(void) {
    qoz_UnaryOp *p = qoz_gc_alloc(sizeof(qoz_UnaryOp), &qoz_UnaryOp_desc);
    p->tag = qoz_UnaryOp_UOpNeg;
    return p;
}

static qoz_UnaryOp *qoz_make_UnaryOp_UOpNot(void) {
    qoz_UnaryOp *p = qoz_gc_alloc(sizeof(qoz_UnaryOp), &qoz_UnaryOp_desc);
    p->tag = qoz_UnaryOp_UOpNot;
    return p;
}

static qoz_UnaryOp *qoz_make_UnaryOp_UOpDeref(void) {
    qoz_UnaryOp *p = qoz_gc_alloc(sizeof(qoz_UnaryOp), &qoz_UnaryOp_desc);
    p->tag = qoz_UnaryOp_UOpDeref;
    return p;
}

static qoz_UnaryOp *qoz_make_UnaryOp_UOpAddr(void) {
    qoz_UnaryOp *p = qoz_gc_alloc(sizeof(qoz_UnaryOp), &qoz_UnaryOp_desc);
    p->tag = qoz_UnaryOp_UOpAddr;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpAdd(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpAdd;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpSub(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpSub;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpMul(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpMul;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpDiv(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpDiv;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpMod(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpMod;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpEq(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpEq;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpNe(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpNe;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpLt(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpLt;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpGt(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpGt;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpLe(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpLe;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpGe(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpGe;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpAnd(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpAnd;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpOr(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpOr;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpBitAnd(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpBitAnd;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpBitOr(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpBitOr;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpBitXor(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpBitXor;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpShl(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpShl;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpShr(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpShr;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpRange(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpRange;
    return p;
}

static qoz_BinaryOp *qoz_make_BinaryOp_BOpRangeInclusive(void) {
    qoz_BinaryOp *p = qoz_gc_alloc(sizeof(qoz_BinaryOp), &qoz_BinaryOp_desc);
    p->tag = qoz_BinaryOp_BOpRangeInclusive;
    return p;
}

static qoz_AssignOp *qoz_make_AssignOp_AOpSet(void) {
    qoz_AssignOp *p = qoz_gc_alloc(sizeof(qoz_AssignOp), &qoz_AssignOp_desc);
    p->tag = qoz_AssignOp_AOpSet;
    return p;
}

static qoz_AssignOp *qoz_make_AssignOp_AOpAddSet(void) {
    qoz_AssignOp *p = qoz_gc_alloc(sizeof(qoz_AssignOp), &qoz_AssignOp_desc);
    p->tag = qoz_AssignOp_AOpAddSet;
    return p;
}

static qoz_AssignOp *qoz_make_AssignOp_AOpSubSet(void) {
    qoz_AssignOp *p = qoz_gc_alloc(sizeof(qoz_AssignOp), &qoz_AssignOp_desc);
    p->tag = qoz_AssignOp_AOpSubSet;
    return p;
}

static qoz_AssignOp *qoz_make_AssignOp_AOpMulSet(void) {
    qoz_AssignOp *p = qoz_gc_alloc(sizeof(qoz_AssignOp), &qoz_AssignOp_desc);
    p->tag = qoz_AssignOp_AOpMulSet;
    return p;
}

static qoz_AssignOp *qoz_make_AssignOp_AOpDivSet(void) {
    qoz_AssignOp *p = qoz_gc_alloc(sizeof(qoz_AssignOp), &qoz_AssignOp_desc);
    p->tag = qoz_AssignOp_AOpDivSet;
    return p;
}

static qoz_AssignOp *qoz_make_AssignOp_AOpModSet(void) {
    qoz_AssignOp *p = qoz_gc_alloc(sizeof(qoz_AssignOp), &qoz_AssignOp_desc);
    p->tag = qoz_AssignOp_AOpModSet;
    return p;
}

static qoz_TypeExpr *qoz_make_TypeExpr_TENamed(qoz_Span f0, qoz_Vec__qoz_string f1, qoz_Vec__qoz_TypeExpr f2) {
    qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_TypeExpr), &qoz_TypeExpr_desc);
    p->tag = qoz_TypeExpr_TENamed;
    p->payload.TENamed.f0 = f0;
    p->payload.TENamed.f1 = f1;
    p->payload.TENamed.f2 = f2;
    return p;
}

static qoz_TypeExpr *qoz_make_TypeExpr_TEPtr(qoz_Span f0, qoz_TypeExpr* f1) {
    qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_TypeExpr), &qoz_TypeExpr_desc);
    p->tag = qoz_TypeExpr_TEPtr;
    p->payload.TEPtr.f0 = f0;
    p->payload.TEPtr.f1 = f1;
    return p;
}

static qoz_TypeExpr *qoz_make_TypeExpr_TETuple(qoz_Span f0, qoz_Vec__qoz_TypeExpr f1) {
    qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_TypeExpr), &qoz_TypeExpr_desc);
    p->tag = qoz_TypeExpr_TETuple;
    p->payload.TETuple.f0 = f0;
    p->payload.TETuple.f1 = f1;
    return p;
}

static qoz_TypeExpr *qoz_make_TypeExpr_TEUnit(qoz_Span f0) {
    qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_TypeExpr), &qoz_TypeExpr_desc);
    p->tag = qoz_TypeExpr_TEUnit;
    p->payload.TEUnit.f0 = f0;
    return p;
}

static qoz_TypeExpr *qoz_make_TypeExpr_TEFn(qoz_Span f0, qoz_Vec__qoz_TypeExpr f1, qoz_TypeExpr* f2) {
    qoz_TypeExpr *p = qoz_gc_alloc(sizeof(qoz_TypeExpr), &qoz_TypeExpr_desc);
    p->tag = qoz_TypeExpr_TEFn;
    p->payload.TEFn.f0 = f0;
    p->payload.TEFn.f1 = f1;
    p->payload.TEFn.f2 = f2;
    return p;
}

static qoz_Pattern *qoz_make_Pattern_PatWild(qoz_Span f0) {
    qoz_Pattern *p = qoz_gc_alloc(sizeof(qoz_Pattern), &qoz_Pattern_desc);
    p->tag = qoz_Pattern_PatWild;
    p->payload.PatWild.f0 = f0;
    return p;
}

static qoz_Pattern *qoz_make_Pattern_PatBind(qoz_Span f0, qoz_string f1) {
    qoz_Pattern *p = qoz_gc_alloc(sizeof(qoz_Pattern), &qoz_Pattern_desc);
    p->tag = qoz_Pattern_PatBind;
    p->payload.PatBind.f0 = f0;
    p->payload.PatBind.f1 = f1;
    return p;
}

static qoz_Pattern *qoz_make_Pattern_PatLitInt(qoz_Span f0, qoz_string f1) {
    qoz_Pattern *p = qoz_gc_alloc(sizeof(qoz_Pattern), &qoz_Pattern_desc);
    p->tag = qoz_Pattern_PatLitInt;
    p->payload.PatLitInt.f0 = f0;
    p->payload.PatLitInt.f1 = f1;
    return p;
}

static qoz_Pattern *qoz_make_Pattern_PatLitString(qoz_Span f0, qoz_string f1) {
    qoz_Pattern *p = qoz_gc_alloc(sizeof(qoz_Pattern), &qoz_Pattern_desc);
    p->tag = qoz_Pattern_PatLitString;
    p->payload.PatLitString.f0 = f0;
    p->payload.PatLitString.f1 = f1;
    return p;
}

static qoz_Pattern *qoz_make_Pattern_PatLitBool(qoz_Span f0, bool f1) {
    qoz_Pattern *p = qoz_gc_alloc(sizeof(qoz_Pattern), &qoz_Pattern_desc);
    p->tag = qoz_Pattern_PatLitBool;
    p->payload.PatLitBool.f0 = f0;
    p->payload.PatLitBool.f1 = f1;
    return p;
}

static qoz_Pattern *qoz_make_Pattern_PatVariant(qoz_Span f0, qoz_Vec__qoz_string f1, qoz_Vec__qoz_Pattern f2) {
    qoz_Pattern *p = qoz_gc_alloc(sizeof(qoz_Pattern), &qoz_Pattern_desc);
    p->tag = qoz_Pattern_PatVariant;
    p->payload.PatVariant.f0 = f0;
    p->payload.PatVariant.f1 = f1;
    p->payload.PatVariant.f2 = f2;
    return p;
}

static qoz_Pattern *qoz_make_Pattern_PatTuple(qoz_Span f0, qoz_Vec__qoz_Pattern f1) {
    qoz_Pattern *p = qoz_gc_alloc(sizeof(qoz_Pattern), &qoz_Pattern_desc);
    p->tag = qoz_Pattern_PatTuple;
    p->payload.PatTuple.f0 = f0;
    p->payload.PatTuple.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EInt(qoz_Span f0, qoz_string f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EInt;
    p->payload.EInt.f0 = f0;
    p->payload.EInt.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EFloat(qoz_Span f0, qoz_string f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EFloat;
    p->payload.EFloat.f0 = f0;
    p->payload.EFloat.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EString(qoz_Span f0, qoz_string f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EString;
    p->payload.EString.f0 = f0;
    p->payload.EString.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EChar(qoz_Span f0, qoz_string f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EChar;
    p->payload.EChar.f0 = f0;
    p->payload.EChar.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EBool(qoz_Span f0, bool f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EBool;
    p->payload.EBool.f0 = f0;
    p->payload.EBool.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_ENil(qoz_Span f0) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_ENil;
    p->payload.ENil.f0 = f0;
    return p;
}

static qoz_Expr *qoz_make_Expr_EIdent(qoz_Span f0, qoz_string f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EIdent;
    p->payload.EIdent.f0 = f0;
    p->payload.EIdent.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EPath(qoz_Span f0, qoz_Vec__qoz_string f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EPath;
    p->payload.EPath.f0 = f0;
    p->payload.EPath.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EUnary(qoz_Span f0, qoz_UnaryOp* f1, qoz_Expr* f2) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EUnary;
    p->payload.EUnary.f0 = f0;
    p->payload.EUnary.f1 = f1;
    p->payload.EUnary.f2 = f2;
    return p;
}

static qoz_Expr *qoz_make_Expr_EBinary(qoz_Span f0, qoz_BinaryOp* f1, qoz_Expr* f2, qoz_Expr* f3) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EBinary;
    p->payload.EBinary.f0 = f0;
    p->payload.EBinary.f1 = f1;
    p->payload.EBinary.f2 = f2;
    p->payload.EBinary.f3 = f3;
    return p;
}

static qoz_Expr *qoz_make_Expr_EAssign(qoz_Span f0, qoz_AssignOp* f1, qoz_Expr* f2, qoz_Expr* f3) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EAssign;
    p->payload.EAssign.f0 = f0;
    p->payload.EAssign.f1 = f1;
    p->payload.EAssign.f2 = f2;
    p->payload.EAssign.f3 = f3;
    return p;
}

static qoz_Expr *qoz_make_Expr_ECall(qoz_Span f0, qoz_Expr* f1, qoz_Vec__qoz_TypeExpr f2, qoz_Vec__qoz_Expr f3) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_ECall;
    p->payload.ECall.f0 = f0;
    p->payload.ECall.f1 = f1;
    p->payload.ECall.f2 = f2;
    p->payload.ECall.f3 = f3;
    return p;
}

static qoz_Expr *qoz_make_Expr_EField(qoz_Span f0, qoz_Expr* f1, qoz_string f2) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EField;
    p->payload.EField.f0 = f0;
    p->payload.EField.f1 = f1;
    p->payload.EField.f2 = f2;
    return p;
}

static qoz_Expr *qoz_make_Expr_EIndex(qoz_Span f0, qoz_Expr* f1, qoz_Expr* f2) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EIndex;
    p->payload.EIndex.f0 = f0;
    p->payload.EIndex.f1 = f1;
    p->payload.EIndex.f2 = f2;
    return p;
}

static qoz_Expr *qoz_make_Expr_ECast(qoz_Span f0, qoz_Expr* f1, qoz_TypeExpr* f2) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_ECast;
    p->payload.ECast.f0 = f0;
    p->payload.ECast.f1 = f1;
    p->payload.ECast.f2 = f2;
    return p;
}

static qoz_Expr *qoz_make_Expr_ETry(qoz_Span f0, qoz_Expr* f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_ETry;
    p->payload.ETry.f0 = f0;
    p->payload.ETry.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_ETuple(qoz_Span f0, qoz_Vec__qoz_Expr f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_ETuple;
    p->payload.ETuple.f0 = f0;
    p->payload.ETuple.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_ERecord(qoz_Span f0, qoz_TypeExpr* f1, qoz_Vec__qoz_RecordFieldLit f2) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_ERecord;
    p->payload.ERecord.f0 = f0;
    p->payload.ERecord.f1 = f1;
    p->payload.ERecord.f2 = f2;
    return p;
}

static qoz_Expr *qoz_make_Expr_EClosure(qoz_Span f0, qoz_Vec__qoz_ClosureParam f1, qoz_TypeExpr* f2, qoz_Expr* f3) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EClosure;
    p->payload.EClosure.f0 = f0;
    p->payload.EClosure.f1 = f1;
    p->payload.EClosure.f2 = f2;
    p->payload.EClosure.f3 = f3;
    return p;
}

static qoz_Expr *qoz_make_Expr_EBlock(qoz_Span f0, qoz_Vec__qoz_Stmt f1, qoz_Expr* f2) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EBlock;
    p->payload.EBlock.f0 = f0;
    p->payload.EBlock.f1 = f1;
    p->payload.EBlock.f2 = f2;
    return p;
}

static qoz_Expr *qoz_make_Expr_EIf(qoz_Span f0, qoz_Expr* f1, qoz_Expr* f2, qoz_Expr* f3) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EIf;
    p->payload.EIf.f0 = f0;
    p->payload.EIf.f1 = f1;
    p->payload.EIf.f2 = f2;
    p->payload.EIf.f3 = f3;
    return p;
}

static qoz_Expr *qoz_make_Expr_EMatch(qoz_Span f0, qoz_Expr* f1, qoz_Vec__qoz_MatchArm f2) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EMatch;
    p->payload.EMatch.f0 = f0;
    p->payload.EMatch.f1 = f1;
    p->payload.EMatch.f2 = f2;
    return p;
}

static qoz_Expr *qoz_make_Expr_EWhile(qoz_Span f0, qoz_Expr* f1, qoz_Expr* f2) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EWhile;
    p->payload.EWhile.f0 = f0;
    p->payload.EWhile.f1 = f1;
    p->payload.EWhile.f2 = f2;
    return p;
}

static qoz_Expr *qoz_make_Expr_EFor(qoz_Span f0, qoz_string f1, qoz_string f2, qoz_Expr* f3, qoz_Expr* f4) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EFor;
    p->payload.EFor.f0 = f0;
    p->payload.EFor.f1 = f1;
    p->payload.EFor.f2 = f2;
    p->payload.EFor.f3 = f3;
    p->payload.EFor.f4 = f4;
    return p;
}

static qoz_Expr *qoz_make_Expr_EReturn(qoz_Span f0, qoz_Expr* f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EReturn;
    p->payload.EReturn.f0 = f0;
    p->payload.EReturn.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EDefer(qoz_Span f0, qoz_Expr* f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EDefer;
    p->payload.EDefer.f0 = f0;
    p->payload.EDefer.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_ESizeOf(qoz_Span f0, qoz_TypeExpr* f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_ESizeOf;
    p->payload.ESizeOf.f0 = f0;
    p->payload.ESizeOf.f1 = f1;
    return p;
}

static qoz_Expr *qoz_make_Expr_EArrayLit(qoz_Span f0, qoz_Vec__qoz_Expr f1) {
    qoz_Expr *p = qoz_gc_alloc(sizeof(qoz_Expr), &qoz_Expr_desc);
    p->tag = qoz_Expr_EArrayLit;
    p->payload.EArrayLit.f0 = f0;
    p->payload.EArrayLit.f1 = f1;
    return p;
}

static qoz_Stmt *qoz_make_Stmt_SLet(qoz_Span f0, qoz_string f1, qoz_TypeExpr* f2, qoz_Expr* f3) {
    qoz_Stmt *p = qoz_gc_alloc(sizeof(qoz_Stmt), &qoz_Stmt_desc);
    p->tag = qoz_Stmt_SLet;
    p->payload.SLet.f0 = f0;
    p->payload.SLet.f1 = f1;
    p->payload.SLet.f2 = f2;
    p->payload.SLet.f3 = f3;
    return p;
}

static qoz_Stmt *qoz_make_Stmt_SVar(qoz_Span f0, qoz_string f1, qoz_TypeExpr* f2, qoz_Expr* f3) {
    qoz_Stmt *p = qoz_gc_alloc(sizeof(qoz_Stmt), &qoz_Stmt_desc);
    p->tag = qoz_Stmt_SVar;
    p->payload.SVar.f0 = f0;
    p->payload.SVar.f1 = f1;
    p->payload.SVar.f2 = f2;
    p->payload.SVar.f3 = f3;
    return p;
}

static qoz_Stmt *qoz_make_Stmt_SExpr(qoz_Span f0, qoz_Expr* f1) {
    qoz_Stmt *p = qoz_gc_alloc(sizeof(qoz_Stmt), &qoz_Stmt_desc);
    p->tag = qoz_Stmt_SExpr;
    p->payload.SExpr.f0 = f0;
    p->payload.SExpr.f1 = f1;
    return p;
}

static qoz_VariantPayloadKind *qoz_make_VariantPayloadKind_VPKNone(void) {
    qoz_VariantPayloadKind *p = qoz_gc_alloc(sizeof(qoz_VariantPayloadKind), &qoz_VariantPayloadKind_desc);
    p->tag = qoz_VariantPayloadKind_VPKNone;
    return p;
}

static qoz_VariantPayloadKind *qoz_make_VariantPayloadKind_VPKPositional(void) {
    qoz_VariantPayloadKind *p = qoz_gc_alloc(sizeof(qoz_VariantPayloadKind), &qoz_VariantPayloadKind_desc);
    p->tag = qoz_VariantPayloadKind_VPKPositional;
    return p;
}

static qoz_VariantPayloadKind *qoz_make_VariantPayloadKind_VPKNamed(void) {
    qoz_VariantPayloadKind *p = qoz_gc_alloc(sizeof(qoz_VariantPayloadKind), &qoz_VariantPayloadKind_desc);
    p->tag = qoz_VariantPayloadKind_VPKNamed;
    return p;
}

static qoz_LinkKind *qoz_make_LinkKind_LinkLibrary(void) {
    qoz_LinkKind *p = qoz_gc_alloc(sizeof(qoz_LinkKind), &qoz_LinkKind_desc);
    p->tag = qoz_LinkKind_LinkLibrary;
    return p;
}

static qoz_LinkKind *qoz_make_LinkKind_LinkFramework(void) {
    qoz_LinkKind *p = qoz_gc_alloc(sizeof(qoz_LinkKind), &qoz_LinkKind_desc);
    p->tag = qoz_LinkKind_LinkFramework;
    return p;
}

static qoz_Decl *qoz_make_Decl_DImport(qoz_Span f0, qoz_Vec__qoz_string f1, qoz_string f2) {
    qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Decl), &qoz_Decl_desc);
    p->tag = qoz_Decl_DImport;
    p->payload.DImport.f0 = f0;
    p->payload.DImport.f1 = f1;
    p->payload.DImport.f2 = f2;
    return p;
}

static qoz_Decl *qoz_make_Decl_DFn(qoz_Span f0, qoz_string f1, qoz_Vec__qoz_string f2, qoz_Vec__qoz_FnParam f3, qoz_TypeExpr* f4, qoz_Expr* f5, qoz_string f6) {
    qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Decl), &qoz_Decl_desc);
    p->tag = qoz_Decl_DFn;
    p->payload.DFn.f0 = f0;
    p->payload.DFn.f1 = f1;
    p->payload.DFn.f2 = f2;
    p->payload.DFn.f3 = f3;
    p->payload.DFn.f4 = f4;
    p->payload.DFn.f5 = f5;
    p->payload.DFn.f6 = f6;
    return p;
}

static qoz_Decl *qoz_make_Decl_DStruct(qoz_Span f0, qoz_string f1, qoz_Vec__qoz_string f2, qoz_Vec__qoz_StructField f3) {
    qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Decl), &qoz_Decl_desc);
    p->tag = qoz_Decl_DStruct;
    p->payload.DStruct.f0 = f0;
    p->payload.DStruct.f1 = f1;
    p->payload.DStruct.f2 = f2;
    p->payload.DStruct.f3 = f3;
    return p;
}

static qoz_Decl *qoz_make_Decl_DEnum(qoz_Span f0, qoz_string f1, qoz_Vec__qoz_string f2, qoz_Vec__qoz_VariantDecl f3) {
    qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Decl), &qoz_Decl_desc);
    p->tag = qoz_Decl_DEnum;
    p->payload.DEnum.f0 = f0;
    p->payload.DEnum.f1 = f1;
    p->payload.DEnum.f2 = f2;
    p->payload.DEnum.f3 = f3;
    return p;
}

static qoz_Decl *qoz_make_Decl_DTypeAlias(qoz_Span f0, qoz_string f1, qoz_Vec__qoz_string f2, qoz_TypeExpr* f3) {
    qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Decl), &qoz_Decl_desc);
    p->tag = qoz_Decl_DTypeAlias;
    p->payload.DTypeAlias.f0 = f0;
    p->payload.DTypeAlias.f1 = f1;
    p->payload.DTypeAlias.f2 = f2;
    p->payload.DTypeAlias.f3 = f3;
    return p;
}

static qoz_Decl *qoz_make_Decl_DConst(qoz_Span f0, qoz_string f1, qoz_TypeExpr* f2, qoz_Expr* f3) {
    qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Decl), &qoz_Decl_desc);
    p->tag = qoz_Decl_DConst;
    p->payload.DConst.f0 = f0;
    p->payload.DConst.f1 = f1;
    p->payload.DConst.f2 = f2;
    p->payload.DConst.f3 = f3;
    return p;
}

static qoz_Decl *qoz_make_Decl_DExternal(qoz_Span f0, qoz_string f1, qoz_string f2, qoz_Vec__qoz_FnParam f3, qoz_TypeExpr* f4) {
    qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Decl), &qoz_Decl_desc);
    p->tag = qoz_Decl_DExternal;
    p->payload.DExternal.f0 = f0;
    p->payload.DExternal.f1 = f1;
    p->payload.DExternal.f2 = f2;
    p->payload.DExternal.f3 = f3;
    p->payload.DExternal.f4 = f4;
    return p;
}

static qoz_Decl *qoz_make_Decl_DLink(qoz_Span f0, qoz_LinkKind* f1, qoz_string f2) {
    qoz_Decl *p = qoz_gc_alloc(sizeof(qoz_Decl), &qoz_Decl_desc);
    p->tag = qoz_Decl_DLink;
    p->payload.DLink.f0 = f0;
    p->payload.DLink.f1 = f1;
    p->payload.DLink.f2 = f2;
    return p;
}

static qoz_MainRetKind *qoz_make_MainRetKind_MainRetUnit(void) {
    qoz_MainRetKind *p = qoz_gc_alloc(sizeof(qoz_MainRetKind), &qoz_MainRetKind_desc);
    p->tag = qoz_MainRetKind_MainRetUnit;
    return p;
}

static qoz_MainRetKind *qoz_make_MainRetKind_MainRetInt(qoz_string f0) {
    qoz_MainRetKind *p = qoz_gc_alloc(sizeof(qoz_MainRetKind), &qoz_MainRetKind_desc);
    p->tag = qoz_MainRetKind_MainRetInt;
    p->payload.MainRetInt.f0 = f0;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyInt(qoz_IntInfo f0) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyInt;
    p->payload.TyInt.f0 = f0;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyFloat(qoz_FloatInfo f0) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyFloat;
    p->payload.TyFloat.f0 = f0;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyBool(void) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyBool;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyChar(void) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyChar;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyString(void) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyString;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyCstring(void) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyCstring;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyUnit(void) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyUnit;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyNil(void) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyNil;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyError(void) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyError;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyPtr(qoz_Ty* f0) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyPtr;
    p->payload.TyPtr.f0 = f0;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyAdt(qoz_string f0, qoz_Vec__qoz_Ty f1) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyAdt;
    p->payload.TyAdt.f0 = f0;
    p->payload.TyAdt.f1 = f1;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyRecord(qoz_string f0, qoz_Vec__qoz_Ty f1) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyRecord;
    p->payload.TyRecord.f0 = f0;
    p->payload.TyRecord.f1 = f1;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyFn(qoz_Vec__qoz_Ty f0, qoz_Ty* f1) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyFn;
    p->payload.TyFn.f0 = f0;
    p->payload.TyFn.f1 = f1;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyTuple(qoz_Vec__qoz_Ty f0) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyTuple;
    p->payload.TyTuple.f0 = f0;
    return p;
}

static qoz_Ty *qoz_make_Ty_TyVar(int64_t f0, qoz_string f1) {
    qoz_Ty *p = qoz_gc_alloc(sizeof(qoz_Ty), &qoz_Ty_desc);
    p->tag = qoz_Ty_TyVar;
    p->payload.TyVar.f0 = f0;
    p->payload.TyVar.f1 = f1;
    return p;
}

qoz_string decl_kind_name(qoz_Decl* d);
qoz_string decl_name(qoz_Decl* d);
qoz_Vec__qoz_string prelude_segs(qoz_string path);
qoz_string resolve_import_path(qoz_Vec__qoz_string segs, qoz_string qoz_root);
qoz_Vec__qoz_string list_package_files(qoz_string dir);
qoz_Decl* rename_with_pkg(qoz_Decl* d, qoz_string pkg, qoz_Map__qoz_string__bool local_fns);
qoz_Expr* rewrite_ident_refs(qoz_Expr* e, qoz_string pkg, qoz_Map__qoz_string__bool local_fns);
qoz_Stmt* rewrite_ident_refs_stmt(qoz_Stmt* s, qoz_string pkg, qoz_Map__qoz_string__bool local_fns);
qoz_Loaded load_all(qoz_string entry, qoz_string qoz_root);
void process_one(qoz_Pending cur, qoz_Vec__qoz_Pending* queue, qoz_Map__qoz_string__bool* visited, qoz_Vec__qoz_Decl* aggregated_decls, qoz_Vec__qoz_string* sources, qoz_string qoz_root);
qoz_string cmd_build(qoz_string path, qoz_string qoz_root);
qoz_Vec__qoz_string clang_argv(qoz_string c_path, qoz_string bin_path);
void cmd_run(qoz_string path, qoz_string qoz_root);
extern void qoz_print_str(qoz_string);
extern void qoz_print_nl(void);
void fmt_println(qoz_string s);
void fmt_print(qoz_string s);
extern qoz_string qoz_fs_read_file(qoz_string);
extern bool qoz_fs_write_file(qoz_string, qoz_string);
extern bool qoz_fs_file_exists(qoz_string);
extern qoz_string qoz_fs_list_qoz_files(qoz_string);
extern int64_t qoz_os_argc(void);
extern qoz_string qoz_os_arg(int64_t);
extern void qoz_os_exit(int64_t);
extern void qoz_panic(qoz_string);
extern qoz_string qoz_os_getenv(qoz_string);
qoz_Vec__qoz_string os_args(void);
extern void qoz_process_exec(qoz_string*, int64_t, int64_t*, qoz_string*, qoz_string*);
qoz_ProcessResult os_process_exec(qoz_Vec__qoz_string argv);
extern void qoz_bytes_copy(void*, void*, int64_t);
extern void* qoz_string_data(qoz_string);
extern qoz_string qoz_string_alias(void*, int64_t);
void strings_sb_init(qoz_Strbuf* b);
void strings_sb_grow(qoz_Strbuf* b, int64_t needed);
void strings_sb_append(qoz_Strbuf* b, qoz_string s);
int64_t strings_sb_len(qoz_Strbuf* b);
void strings_sb_truncate(qoz_Strbuf* b, int64_t new_len);
qoz_string strings_sb_finish(qoz_Strbuf* b);
extern void qoz_strbuf_append_i64(void*, int64_t);
extern void qoz_strbuf_append_bool(void*, bool);
extern void qoz_strbuf_append_f64(void*, double);
void strings_sb_append_i64(qoz_Strbuf* b, int64_t v);
void strings_sb_append_bool(qoz_Strbuf* b, bool v);
void strings_sb_append_f64(qoz_Strbuf* b, double v);
qoz_string strings_sb_slice_copy(qoz_Strbuf* b, int64_t from, int64_t to);
extern bool qoz_string_eq(qoz_string, qoz_string);
extern uint64_t qoz_string_hash(qoz_string);
extern bool qoz_string_has_prefix(qoz_string, qoz_string);
extern bool qoz_string_has_suffix(qoz_string, qoz_string);
extern int64_t qoz_string_byte_at(qoz_string, int64_t);
extern int64_t qoz_string_index_byte(qoz_string, int64_t);
extern qoz_string qoz_string_slice(qoz_string, int64_t, int64_t);
extern int64_t qoz_string_parse_int(qoz_string);
extern qoz_string qoz_string_cat(qoz_string, qoz_string);
bool strings_eq(qoz_string* a, qoz_string* b);
uint64_t strings_hash(qoz_string* s);
bool strings_lt_raw(qoz_string a, qoz_string b);
bool strings_lt(qoz_string* a, qoz_string* b);
qoz_string strings_replace_all(qoz_string s, qoz_string needle, qoz_string replacement);
qoz_Vec__qoz_string strings_split(qoz_string s, int64_t sep);
qoz_Lexer tokenize_make_lexer(qoz_string src);
int64_t tokenize_lex_at(qoz_Lexer* l);
void tokenize_lex_advance(qoz_Lexer* l);
bool tokenize_is_space(int64_t c);
bool tokenize_is_digit(int64_t c);
bool tokenize_is_alpha(int64_t c);
bool tokenize_is_alnum(int64_t c);
bool tokenize_is_hex_digit(int64_t c);
bool tokenize_is_bin_digit(int64_t c);
bool tokenize_is_oct_digit(int64_t c);
bool tokenize_is_punct(int64_t c);
void tokenize_skip_whitespace_and_comments(qoz_Lexer* l);
bool tokenize_is_keyword(qoz_Map__qoz_string__bool* kw, qoz_string s);
qoz_Map__qoz_string__bool tokenize_build_keywords(void);
qoz_Token tokenize_lex_one(qoz_Lexer* l, qoz_Map__qoz_string__bool* kw);
bool tokenize_is_stmt_ender(qoz_Token t);
bool tokenize_is_line_continuation(qoz_Token t);
qoz_Vec__qoz_Token tokenize_run(qoz_string src);
qoz_Span ast_make_span(qoz_string file, int64_t line, int64_t col);
qoz_Span ast_span_of_expr(qoz_Expr* e);
qoz_Parser parse_make_parser(qoz_Vec__qoz_Token tokens, qoz_string file);
bool parse_at_eof(qoz_Parser* p);
qoz_Token parse_peek(qoz_Parser* p);
void parse_advance(qoz_Parser* p);
bool parse_is_kw(qoz_Token t, qoz_string kw);
bool parse_is_punct(qoz_Token t, qoz_string sym);
bool parse_is_ident(qoz_Token t);
bool parse_is_int_lit(qoz_Token t);
bool parse_is_float_lit(qoz_Token t);
bool parse_is_str_lit(qoz_Token t);
bool parse_is_char_lit(qoz_Token t);
qoz_Span parse_span_of(qoz_Parser* p, qoz_Token t);
void parse_record_error(qoz_Parser* p, qoz_string msg);
void parse_err_unexpected(qoz_Parser* p, qoz_string want, qoz_Token t);
void parse_expect_punct(qoz_Parser* p, qoz_string sym);
qoz_string parse_expect_ident(qoz_Parser* p);
qoz_TypeExpr* parse_parse_type(qoz_Parser* p);
qoz_Expr* parse_parse_expr(qoz_Parser* p);
qoz_Expr* parse_parse_logical_or(qoz_Parser* p);
qoz_Expr* parse_parse_logical_and(qoz_Parser* p);
qoz_Expr* parse_parse_bitor(qoz_Parser* p);
qoz_Expr* parse_parse_bitxor(qoz_Parser* p);
qoz_Expr* parse_parse_bitand(qoz_Parser* p);
qoz_BinaryOp* parse_bin_op_for(qoz_string sym);
bool parse_is_cmp_punct(qoz_Token t);
qoz_Expr* parse_parse_compare(qoz_Parser* p);
bool parse_is_double_gt_adjacent(qoz_Parser* p);
qoz_Expr* parse_parse_shift(qoz_Parser* p);
qoz_Expr* parse_parse_addsub(qoz_Parser* p);
qoz_Expr* parse_parse_muldiv(qoz_Parser* p);
qoz_Expr* parse_parse_unary(qoz_Parser* p);
qoz_Token parse_peek_at(qoz_Parser* p, int64_t offset);
bool parse_looks_like_call_type_args(qoz_Parser* p);
bool parse_is_token_eof(qoz_Token t);
bool parse_looks_like_record_literal(qoz_Parser* p);
qoz_Expr* parse_parse_record_literal(qoz_Parser* p, qoz_Span span, qoz_Vec__qoz_string type_path);
qoz_Expr* parse_parse_postfix(qoz_Parser* p);
bool parse_looks_like_closure(qoz_Parser* p);
qoz_Expr* parse_parse_closure(qoz_Parser* p);
qoz_string parse_c_escape_bytes(qoz_string src);
qoz_string parse_hex_digit(int64_t n);
qoz_string parse_hex2(int64_t b);
bool parse_is_interp_start_byte(int64_t b);
bool parse_is_interp_cont_byte(int64_t b);
int64_t parse_scan_interp_end(qoz_string body, int64_t start);
qoz_Expr* parse_build_interp_expr(qoz_string text, qoz_Span span);
bool parse_interp_text_valid(qoz_string text);
qoz_Expr* parse_build_string_lit_or_interp(qoz_string raw, qoz_Span span);
qoz_string parse_interp_var_name(qoz_Span span);
qoz_Vec__qoz_string parse_split_template_chunks(qoz_string template);
qoz_Expr* parse_interp_block(qoz_string template, qoz_Vec__qoz_Expr args, qoz_Span span);
qoz_Vec__qoz_Expr parse_append_args1(qoz_Expr* a);
qoz_Vec__qoz_Expr parse_append_args2(qoz_Expr* a, qoz_Expr* b);
qoz_Expr* parse_strings_call(qoz_string method, qoz_Span span, qoz_Vec__qoz_Expr args);
qoz_Expr* parse_parse_atom(qoz_Parser* p);
qoz_Expr* parse_parse_if(qoz_Parser* p);
qoz_Expr* parse_parse_while(qoz_Parser* p);
qoz_Expr* parse_parse_for(qoz_Parser* p);
qoz_Expr* parse_parse_match(qoz_Parser* p);
qoz_Expr* parse_parse_return(qoz_Parser* p);
qoz_Expr* parse_parse_defer(qoz_Parser* p);
qoz_Pattern* parse_parse_pattern(qoz_Parser* p);
qoz_Expr* parse_parse_block(qoz_Parser* p);
qoz_Vec__qoz_string parse_parse_path_segs(qoz_Parser* p);
qoz_Decl* parse_parse_import(qoz_Parser* p);
qoz_Vec__qoz_string parse_parse_type_params(qoz_Parser* p);
qoz_Vec__qoz_StructField parse_parse_struct_fields(qoz_Parser* p);
qoz_Decl* parse_parse_top_type(qoz_Parser* p);
qoz_Vec__qoz_VariantDecl parse_parse_variants(qoz_Parser* p);
qoz_Vec__qoz_FnParam parse_parse_fn_params(qoz_Parser* p);
qoz_Decl* parse_parse_top_let(qoz_Parser* p);
qoz_Decl* parse_parse_decl(qoz_Parser* p);
qoz_string parse_strip_quotes(qoz_string s);
qoz_Decl* parse_parse_external(qoz_Parser* p, qoz_string link_name);
qoz_Decl* parse_parse_top_let_with_op(qoz_Parser* p, qoz_string operator);
qoz_Decl* parse_parse_link_directive(qoz_Parser* p);
qoz_ParseOutput parse_run(qoz_Vec__qoz_Token tokens, qoz_string file);
qoz_TyContext check_make_ctx(void);
int64_t check_expr_id(qoz_Expr* e);
void check_record_error(qoz_TyContext* tc, qoz_Span span, qoz_string msg);
void check_register_decl(qoz_TyContext* tc, qoz_Decl* d);
void check_register_file(qoz_TyContext* tc, qoz_File f);
void check_report(qoz_TyContext* tc);
void check_summary(qoz_TyContext* tc);
qoz_Ty* check_resolve_type(qoz_TyContext* tc, qoz_TypeExpr* te);
qoz_Ty* check_resolve_named(qoz_TyContext* tc, qoz_Span span, qoz_Vec__qoz_string path, qoz_Vec__qoz_TypeExpr args);
qoz_Env check_env_make(void);
void check_env_define(qoz_Env* env, qoz_string name, qoz_Ty* t);
qoz_Ty* check_env_lookup(qoz_Env* env, qoz_string name);
bool check_env_has(qoz_Env* env, qoz_string name);
qoz_Ty* check_synth(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* e);
qoz_Ty* check_synth_inner(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* e);
qoz_Ty* check_synth_unary(qoz_TyContext* tc, qoz_Env* env, qoz_UnaryOp* op, qoz_Expr* rhs);
qoz_Ty* check_synth_field(qoz_TyContext* tc, qoz_Env* env, qoz_Span sp, qoz_Expr* base, qoz_string name);
qoz_Ty* check_field_type_of(qoz_TyContext* tc, qoz_Ty* bt, qoz_string name);
qoz_Ty* check_lookup_field(qoz_TyContext* tc, qoz_string struct_name, qoz_Vec__qoz_Ty tyargs, qoz_string field);
qoz_Ty* check_resolve_type_with_subst(qoz_TyContext* tc, qoz_TypeExpr* te, qoz_Vec__qoz_string params, qoz_Vec__qoz_Ty args);
qoz_Ty* check_apply_subst(qoz_Ty* t, qoz_Vec__qoz_string params, qoz_Vec__qoz_Ty args);
qoz_Ty* check_synth_index(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* base, qoz_Expr* idx);
bool check_is_strings_callee(qoz_Expr* callee, qoz_string method);
qoz_string check_sb_append_method_for(qoz_Ty* t);
qoz_string check_resolve_callee_fn(qoz_TyContext* tc, qoz_Expr* callee);
qoz_Ty* check_synth_call_full(qoz_TyContext* tc, qoz_Env* env, qoz_Span sp, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args);
qoz_Ty* check_synth_variant_ctor(qoz_TyContext* tc, qoz_string name);
qoz_Ty* check_synth_call_with_decl(qoz_TyContext* tc, qoz_Span sp, qoz_Decl* d, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Ty arg_tys);
bool check_unify(qoz_Ty* pattern, qoz_Ty* concrete, qoz_Map__qoz_string__qoz_Ty* env);
bool check_compatible_or_refine(qoz_Map__qoz_string__qoz_Ty* env, qoz_string name, qoz_Ty* prior, qoz_Ty* concrete);
bool check_unify_args(qoz_Vec__qoz_Ty a, qoz_Vec__qoz_Ty b, qoz_Map__qoz_string__qoz_Ty* env);
bool check_is_ty_error(qoz_Ty* t);
qoz_Ty* check_synth_ident(qoz_TyContext* tc, qoz_Env* env, qoz_Span span, qoz_string name);
qoz_Ty* check_synth_binary(qoz_TyContext* tc, qoz_Env* env, qoz_BinaryOp* op, qoz_Expr* l, qoz_Expr* r);
qoz_Ty* check_synth_block(qoz_TyContext* tc, qoz_Env* env, qoz_Vec__qoz_Stmt stmts, qoz_Expr* tail);
qoz_Ty* check_synth_if(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* c, qoz_Expr* t, qoz_Expr* f);
qoz_Ty* check_synth_match(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* scrut, qoz_Vec__qoz_MatchArm arms);
void check_check_match_exhaustiveness(qoz_TyContext* tc, qoz_Expr* scrut, qoz_Ty* scrut_ty, qoz_Vec__qoz_MatchArm arms);
qoz_Span check_scrut_span(qoz_Expr* scrut);
qoz_string check_enum_name_of_ty(qoz_Ty* t);
bool check_has_catch_all_with_variants(qoz_Vec__qoz_MatchArm arms, qoz_CoverSet* variants);
void check_collect_covered_variants(qoz_Vec__qoz_MatchArm arms, qoz_CoverSet* cs);
void check_bind_for_loop(qoz_Env* env, qoz_string b1, qoz_string b2, qoz_Ty* it_ty);
void check_env_truncate(qoz_Env* env, int64_t n);
void check_bind_pattern(qoz_TyContext* tc, qoz_Env* env, qoz_Pattern* pat, qoz_Ty* scrut);
void check_bind_variant_pattern(qoz_TyContext* tc, qoz_Env* env, qoz_Vec__qoz_string path, qoz_Vec__qoz_Pattern sub_pats, qoz_Ty* scrut);
qoz_File check_infer_calls(qoz_TyContext* tc, qoz_File f);
qoz_Decl* check_infer_calls_decl(qoz_TyContext* tc, qoz_Decl* d);
qoz_Expr* check_infer_calls_expr(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* e, qoz_Ty* expected);
qoz_Stmt* check_infer_calls_stmt(qoz_TyContext* tc, qoz_Env* env, qoz_Stmt* s);
qoz_TypeExpr* check_pick_record_type(qoz_TypeExpr* te, qoz_Ty* expected);
qoz_Ty* check_record_field_hint(qoz_TyContext* tc, qoz_TypeExpr* te, qoz_string field_name);
void check_collect_arg_hints(qoz_TyContext* tc, qoz_Expr* callee, int64_t n, qoz_Vec__qoz_Ty* out);
void check_fill_fn_param_hints(qoz_TyContext* tc, qoz_Decl* d, qoz_Vec__qoz_Ty* out);
void check_fill_variant_pos_hints(qoz_TyContext* tc, qoz_Decl* d, qoz_string variant_name, qoz_Vec__qoz_Ty* out);
bool check_is_ty_unit_or_error(qoz_Ty* t);
qoz_Expr* check_rewrite_call(qoz_TyContext* tc, qoz_Env* env, qoz_Span sp, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args, qoz_Ty* expected);
void check_check_fn_bodies(qoz_TyContext* tc, qoz_File f);
void check_set_type_params(qoz_TyContext* tc, qoz_Vec__qoz_string params);
void check_clear_type_params(qoz_TyContext* tc);
void check_validate_signatures(qoz_TyContext* tc, qoz_File f);
qoz_Emitter emit_make_emitter(void);
void emit_register_generics(qoz_Emitter* e, qoz_File file);
void emit_register_fn_instantiation(qoz_Emitter* e, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args);
qoz_string emit_generic_callee_name(qoz_Emitter* e, qoz_Expr* callee);
void emit_collect_type_instantiations(qoz_Emitter* e, qoz_File file);
void emit_walk_fn_body_with_subst(qoz_Emitter* e, qoz_Decl* d, qoz_Vec__qoz_TypeExpr args);
void emit_walk_struct_fields_with_subst(qoz_Emitter* e, qoz_Decl* d, qoz_Vec__qoz_TypeExpr args);
void emit_walk_enum_variants_with_subst(qoz_Emitter* e, qoz_Decl* d, qoz_Vec__qoz_TypeExpr args);
void emit_walk_decl_for_types(qoz_Emitter* e, qoz_Decl* d);
void emit_walk_typeexpr(qoz_Emitter* e, qoz_TypeExpr* te);
qoz_Option__qoz_TypeExpr* emit_literal_typeexpr(qoz_Expr* ex);
void emit_walk_let_value_for_array_hint(qoz_Emitter* e, qoz_TypeExpr* ty, qoz_Expr* val);
void emit_walk_expr_for_types(qoz_Emitter* e, qoz_Expr* ex);
void emit_register_instance(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_TypeExpr args);
qoz_string emit_mangle_inst(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_TypeExpr args);
qoz_string emit_mangle_type(qoz_Emitter* e, qoz_TypeExpr* t);
qoz_string emit_mangle_named(qoz_Emitter* e, qoz_Vec__qoz_string path, qoz_Vec__qoz_TypeExpr args);
qoz_string emit_primitive_c_name(qoz_string n);
qoz_TypeExpr* emit_substitute_type(qoz_Emitter* e, qoz_TypeExpr* t, qoz_Vec__qoz_string params, qoz_Vec__qoz_TypeExpr args);
qoz_Expr* emit_substitute_expr(qoz_Emitter* e, qoz_Expr* x, qoz_Vec__qoz_string params, qoz_Vec__qoz_TypeExpr args);
qoz_Stmt* emit_substitute_stmt(qoz_Emitter* e, qoz_Stmt* s, qoz_Vec__qoz_string params, qoz_Vec__qoz_TypeExpr args);
qoz_string emit_operator_first_param_type_name(qoz_TypeExpr* te);
void emit_register_variants(qoz_Emitter* e, qoz_File file);
qoz_string emit_strip_numeric_underscores(qoz_string s);
void emit_push(qoz_Emitter* e, qoz_string s);
qoz_string emit_render(qoz_Emitter* e);
qoz_string emit_register_fn_typedef(qoz_Emitter* e, qoz_Vec__qoz_TypeExpr params, qoz_TypeExpr* ret);
qoz_string emit_c_type_for(qoz_Emitter* e, qoz_TypeExpr* t);
qoz_string emit_register_tuple_typedef(qoz_Emitter* e, qoz_Vec__qoz_TypeExpr elems);
qoz_string emit_c_type_for_named_with_args(qoz_Emitter* e, qoz_Vec__qoz_string path, qoz_Vec__qoz_TypeExpr args);
void emit_collect_closure_captures(qoz_Emitter* e, qoz_Expr* ex, qoz_CaptureScope* sc, qoz_Vec__qoz_string* out);
qoz_string emit_result_mangle_for(qoz_Emitter* e, qoz_TypeExpr* te);
void emit_emit_try(qoz_Emitter* e, qoz_Span sp, qoz_Expr* value);
void emit_emit_tuple_lit(qoz_Emitter* e, qoz_Span sp, qoz_Vec__qoz_Expr elems);
void emit_emit_array_lit_with_hint(qoz_Emitter* e, qoz_Span sp, qoz_Vec__qoz_Expr elems, qoz_TypeExpr* hint);
void emit_emit_array_lit_using(qoz_Emitter* e, qoz_Span sp, qoz_Vec__qoz_Expr elems, qoz_TypeExpr* hint);
void emit_emit_closure_lifted(qoz_Emitter* e, qoz_Span sp, qoz_Vec__qoz_ClosureParam cps, qoz_TypeExpr* ret, qoz_Expr* body);
void emit_emit_expr(qoz_Emitter* e, qoz_Expr* ex);
qoz_string emit_infer_value_ctype(qoz_Emitter* e, qoz_TypeExpr* ty, qoz_Expr* value);
qoz_string emit_infer_expr_ctype(qoz_Emitter* e, qoz_Expr* ex);
void emit_ingest_stmt_locals(qoz_Emitter* e, qoz_Stmt* s);
qoz_string emit_bare_enum_for_pat(qoz_Emitter* e, qoz_Pattern* pat);
qoz_string emit_emit_die(qoz_Span sp, qoz_string msg);
qoz_TypeExpr* emit_infer_base_typeexpr(qoz_Emitter* e, qoz_Expr* base);
qoz_Span emit_span_of_expr(qoz_Expr* e);
qoz_TypeExpr* emit_field_typeexpr_for(qoz_Emitter* e, qoz_TypeExpr* base_te, qoz_string field, qoz_Span sp);
int64_t emit_tuple_field_index(qoz_string field);
qoz_Span emit_span_of_te(qoz_TypeExpr* te);
qoz_TypeExpr* emit_binding_te(qoz_Emitter* e, qoz_TypeExpr* declared, qoz_Expr* value);
int64_t emit_expr_id(qoz_Expr* e);
qoz_TypeExpr* emit_infer_value_te_cached(qoz_Emitter* e, qoz_Expr* v);
qoz_TypeExpr* emit_infer_value_te(qoz_Emitter* e, qoz_Expr* v);
qoz_TypeExpr* emit_single_named_te(qoz_Span sp, qoz_string name);
qoz_Vec__qoz_TypeExpr emit_infer_variant_type_args(qoz_Emitter* e, qoz_string enum_name, qoz_string variant, qoz_Vec__qoz_Expr args, qoz_Span sp);
void emit_unify_te(qoz_TypeExpr* pattern, qoz_TypeExpr* concrete, qoz_Map__qoz_string__qoz_TypeExpr* env);
bool emit_is_type_var_name(qoz_string n);
qoz_TypeExpr* emit_call_return_te(qoz_Emitter* e, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args, qoz_Span sp);
qoz_StmtScope emit_open_statement_scope(qoz_Emitter* e);
void emit_close_statement_scope(qoz_Emitter* e, qoz_StmtScope saved);
void emit_hoist_to_prologue(qoz_Emitter* e, int64_t start);
void emit_emit_stmt(qoz_Emitter* e, qoz_Stmt* s);
void emit_emit_stmt_inner(qoz_Emitter* e, qoz_Stmt* s);
qoz_Vec__qoz_Expr emit_collect_defers_in_stmts(qoz_Vec__qoz_Stmt stmts);
void emit_emit_defers_reverse(qoz_Emitter* e, qoz_Vec__qoz_Expr defers);
void emit_emit_cond(qoz_Emitter* e, qoz_Expr* cond);
void emit_emit_stmt_expr(qoz_Emitter* e, qoz_Expr* expr);
void emit_emit_branch_as_statement(qoz_Emitter* e, qoz_Expr* br);
bool emit_path_is(qoz_Expr* callee, qoz_string a, qoz_string b);
bool emit_ident_is(qoz_Expr* ex, qoz_string name);
qoz_string emit_qualified_call_name(qoz_Emitter* e, qoz_Expr* callee);
qoz_string emit_match_result_ctype_with_hint(qoz_Emitter* e, qoz_string enum_name, qoz_Vec__qoz_MatchArm arms);
qoz_string emit_match_result_ctype(qoz_Emitter* e, qoz_string enum_name, qoz_Vec__qoz_MatchArm arms);
qoz_string emit_arm_body_ctype(qoz_Emitter* e, qoz_string enum_name, qoz_Expr* body);
bool emit_same_enum_base(qoz_Emitter* e, qoz_string bare, qoz_string maybe_mangled);
void emit_bind_arm_locals(qoz_Emitter* e, qoz_string enum_name, qoz_Pattern* pat);
qoz_string emit_default_value_for(qoz_string ctype);
bool emit_is_int_ctype(qoz_string ctype);
void emit_emit_field(qoz_Emitter* e, qoz_Expr* base, qoz_string name);
qoz_string emit_field_access_op(qoz_Emitter* e, qoz_Expr* base);
void emit_walk_assign_for_map_helpers(qoz_Emitter* e, qoz_Expr* lhs);
qoz_TypeExpr* emit_base_type_for_walk(qoz_Emitter* e, qoz_Expr* ex);
qoz_TypeExpr* emit_field_typeexpr_for_no_die(qoz_Emitter* e, qoz_TypeExpr* base_te, qoz_string field, qoz_Span sp);
void emit_register_map_helper(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_TypeExpr args);
void emit_emit_assign(qoz_Emitter* e, qoz_Expr* lhs, qoz_Expr* rhs);
qoz_TypeExpr* emit_lvalue_hint(qoz_Emitter* e, qoz_Expr* lhs);
void emit_emit_index(qoz_Emitter* e, qoz_Expr* base, qoz_Expr* idx);
void emit_emit_hash_builtin(qoz_Emitter* e, qoz_Expr* arg);
void emit_emit_len_builtin(qoz_Emitter* e, qoz_Expr* arg);
void emit_emit_call(qoz_Emitter* e, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args);
qoz_TypeExpr* emit_callee_value_typeexpr(qoz_Emitter* e, qoz_Expr* callee);
void emit_ensure_unary_byval_dispatch_helper(qoz_Emitter* e, qoz_string fn_name, qoz_string operand_ct, qoz_string ret_ct);
qoz_string emit_unary_op_text(qoz_UnaryOp* op);
void emit_ensure_byval_dispatch_helper(qoz_Emitter* e, qoz_string fn_name, qoz_string operand_ct, qoz_string ret_ct);
qoz_string emit_binary_op_text(qoz_BinaryOp* op);
qoz_string emit_record_struct_name_of_ctype(qoz_Emitter* e, qoz_string ct);
void emit_emit_binary(qoz_Emitter* e, qoz_BinaryOp* op, qoz_Expr* lhs, qoz_Expr* rhs);
bool emit_is_logical_op(qoz_BinaryOp* op);
void emit_emit_binary_child(qoz_Emitter* e, qoz_Expr* ex, int64_t min_prec, bool force_paren_if_binary);
int64_t emit_binary_prec(qoz_BinaryOp* op);
void emit_emit_with_prec(qoz_Emitter* e, qoz_Expr* ex, int64_t min_prec);
qoz_string emit_binary_c_op(qoz_BinaryOp* op);
qoz_string emit_unary_c_op(qoz_UnaryOp* op);
bool emit_is_block(qoz_Expr* ex);
bool emit_is_nil_expr(qoz_Expr* ex);
void emit_emit_fn_body_block(qoz_Emitter* e, qoz_Expr* body, qoz_TypeExpr* ret_hint);
bool emit_is_unit_typeexpr(qoz_TypeExpr* te);
void emit_emit_fn(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_FnParam params, qoz_TypeExpr* ret, qoz_Expr* body);
bool emit_c_type_is_pointer(qoz_string ct);
void emit_emit_records_topological(qoz_Emitter* e, qoz_File f);
void emit_emit_record_visit(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_string names, qoz_Vec__qoz_Vec__qoz_StructField field_lists, qoz_Map__qoz_string__bool* visited);
qoz_Option__qoz_string* emit_value_field_dep(qoz_Emitter* e, qoz_TypeExpr* te);
bool emit_is_main(qoz_string name);
qoz_string emit_strip_runtime_includes(qoz_string src);
qoz_string emit_emit_program(qoz_File f, qoz_Map__int64_t__qoz_TypeExpr expr_types);
void emit_emit_enum(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_VariantDecl variants);
bool emit_variant_has_positional(qoz_VariantDecl v);
qoz_string emit_int_to_string(int64_t n);
void emit_emit_variant_ctor(qoz_Emitter* e, qoz_string enum_name, qoz_VariantDecl v);
void emit_collect_field_ptr_offsets(qoz_Emitter* e, qoz_string parent, qoz_string field, qoz_TypeExpr* te, qoz_Vec__qoz_string* out);
void emit_emit_record_desc(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_StructField fields);
qoz_Vec__qoz_string emit_collect_variant_ptr_offsets(qoz_Emitter* e, qoz_string enum_name, qoz_VariantDecl v);
void emit_emit_adt_desc(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_VariantDecl variants);
void emit_emit_struct(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_StructField fields);
void emit_emit_record_eq(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_StructField fields);
void emit_emit_record_hash(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_StructField fields);
void emit_emit_field_hash_expr(qoz_Emitter* e, qoz_TypeExpr* te, qoz_string fname);
void emit_emit_field_eq_expr(qoz_Emitter* e, qoz_TypeExpr* te, qoz_string fname);
void emit_emit_fn_proto(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_FnParam params, qoz_TypeExpr* ret);
void emit_emit_extern_proto(qoz_Emitter* e, qoz_string symbol, qoz_Vec__qoz_FnParam params, qoz_TypeExpr* ret);
bool emit_is_range_op(qoz_BinaryOp* op);
qoz_string emit_range_cmp_op(qoz_BinaryOp* op);
void emit_emit_block_tail_as_value(qoz_Emitter* e, qoz_Expr* tail);
bool emit_hint_is_cstring(qoz_TypeExpr* hint);
void emit_emit_value_with_hint(qoz_Emitter* e, qoz_Expr* value, qoz_TypeExpr* hint);
void emit_emit_match_as_expr_with_hint(qoz_Emitter* e, qoz_Span sp, qoz_Expr* scrut, qoz_Vec__qoz_MatchArm arms, qoz_TypeExpr* hint);
void emit_emit_ident_with_hint(qoz_Emitter* e, qoz_string name, qoz_TypeExpr* hint);
qoz_string emit_register_fn_thunk(qoz_Emitter* e, qoz_string fn_name, qoz_Vec__qoz_TypeExpr params, qoz_TypeExpr* ret);
void emit_emit_call_with_hint(qoz_Emitter* e, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args, qoz_TypeExpr* hint);
qoz_Vec__qoz_TypeExpr emit_hint_args_for_enum(qoz_TypeExpr* hint, qoz_string enum_name);
void emit_emit_record_lit_with_hint(qoz_Emitter* e, qoz_TypeExpr* te, qoz_Vec__qoz_RecordFieldLit fields, qoz_TypeExpr* hint);
qoz_string emit_type_lookup_key(qoz_Emitter* e, qoz_Vec__qoz_string path);
qoz_TypeExpr* emit_field_type_hint(qoz_Emitter* e, qoz_TypeExpr* te, qoz_string field_name);
qoz_TypeExpr* emit_pick_type_with_args(qoz_TypeExpr* te, qoz_TypeExpr* hint);
void emit_emit_for_loop_one(qoz_Emitter* e, qoz_string binding, qoz_string binding2, qoz_Expr* iter, qoz_Expr* body);
void emit_emit_vec_for(qoz_Emitter* e, qoz_string binding, qoz_Expr* iter, qoz_Expr* body);
qoz_TypeExpr* emit_vec_element_typeexpr(qoz_Emitter* e, qoz_Expr* iter);
void emit_emit_map_for(qoz_Emitter* e, qoz_string binding, qoz_string binding2, qoz_Expr* iter, qoz_Expr* body);
qoz_TypeExpr* emit_map_key_typeexpr(qoz_Emitter* e, qoz_TypeExpr* te);
qoz_TypeExpr* emit_map_val_typeexpr(qoz_Emitter* e, qoz_TypeExpr* te);
void emit_emit_branch_body_inline(qoz_Emitter* e, qoz_Expr* body);
qoz_string emit_find_enum_from_arms(qoz_Emitter* e, qoz_Vec__qoz_MatchArm arms);
bool emit_any_arm_has_guard(qoz_Vec__qoz_MatchArm arms);
bool emit_any_arm_is_literal(qoz_Vec__qoz_MatchArm arms);
void emit_emit_match_as_expr(qoz_Emitter* e, qoz_Span span, qoz_Expr* scrut, qoz_Vec__qoz_MatchArm arms);
void emit_emit_match_as_if_chain(qoz_Emitter* e, qoz_Span span, qoz_Expr* scrut, qoz_Vec__qoz_MatchArm arms);
void emit_emit_arm_in_chain_with_te(qoz_Emitter* e, qoz_string enum_name, qoz_string scrut_tmp, qoz_TypeExpr* scrut_te, qoz_string res_tmp, qoz_string mflag, qoz_MatchArm arm, bool is_void);
void emit_emit_arm_body_kind(qoz_Emitter* e, bool is_void, qoz_string res_tmp, qoz_Expr* body, qoz_TypeExpr* body_hint);
void emit_emit_match_arm_with_kind(qoz_Emitter* e, qoz_string enum_name, qoz_string scrut_tmp, qoz_string res_tmp, qoz_MatchArm arm, bool is_void);
qoz_string emit_enum_lookup_name(qoz_Emitter* e, qoz_Expr* scrut, qoz_string bare_enum);
qoz_string emit_enum_name_from_type(qoz_Emitter* e, qoz_TypeExpr* te, qoz_string bare_enum);
qoz_Vec__qoz_TypeExpr emit_variant_inst_args(qoz_Emitter* e, qoz_string enum_name, qoz_string bare);
qoz_TypeExpr* emit_variant_payload_typeexpr(qoz_Emitter* e, qoz_string enum_name, qoz_string variant, int64_t pos);
qoz_string emit_variant_payload_ctype(qoz_Emitter* e, qoz_string enum_name, qoz_string variant, int64_t pos);
qoz_string emit_strip_mangled(qoz_string name);
qoz_Vec__qoz_TypeExpr emit_mangled_args(qoz_Emitter* e, qoz_string mangled, qoz_string bare);
qoz_TypeExpr* emit_type_expr_from_mangled_part(qoz_string part, qoz_Span sp);
qoz_string emit_unmangle_primitive(qoz_string s);
void emit_emit_arm_body_with_hint(qoz_Emitter* e, qoz_string res_tmp, qoz_Expr* body, qoz_TypeExpr* hint);
qoz_TypeExpr* emit_match_body_hint(qoz_Emitter* e, qoz_string enum_name, qoz_Expr* body);
void emit_emit_main(qoz_Emitter* e, qoz_TypeExpr* ret, qoz_Expr* body);
qoz_MainRetKind* emit_main_return_kind(qoz_TypeExpr* ret);
void emit_emit_main_tail(qoz_Emitter* e, qoz_MainRetKind* kind, qoz_Expr* tail);
extern void* qoz_alloc(int64_t);
extern void* qoz_calloc(int64_t);
extern void* qoz_realloc(void*, int64_t);
qoz_Ty* ty_ty_int_(int64_t width, bool is_signed);
qoz_Ty* ty_ty_int_untyped(void);
qoz_Ty* ty_ty_float_(int64_t width);
qoz_Ty* ty_ty_float_untyped(void);
qoz_Ty* ty_ty_bool_(void);
qoz_Ty* ty_ty_char_(void);
qoz_Ty* ty_ty_string_(void);
qoz_Ty* ty_ty_cstring_(void);
qoz_Ty* ty_ty_unit_(void);
qoz_Ty* ty_ty_nil_(void);
qoz_Ty* ty_ty_error_(void);
qoz_Ty* ty_ty_ptr_(qoz_Ty* inner);
qoz_Ty* ty_ty_adt_(qoz_string name, qoz_Vec__qoz_Ty args);
qoz_Ty* ty_ty_record_(qoz_string name, qoz_Vec__qoz_Ty args);
qoz_Ty* ty_ty_fn_(qoz_Vec__qoz_Ty params, qoz_Ty* ret);
qoz_Ty* ty_ty_tuple_(qoz_Vec__qoz_Ty elems);
qoz_Ty* ty_ty_var_(qoz_string name);
bool ty_ty_eq(qoz_Ty* a, qoz_Ty* b);
qoz_TypeExpr* ty_ty_to_type_expr_at(qoz_Ty* t, qoz_Span sp);
qoz_Vec__qoz_string ty_ints_to_path(int64_t width, bool is_signed);
qoz_TypeExpr* ty_single_path_te(qoz_Span sp, qoz_string name);
qoz_TypeExpr* ty_named_with_args(qoz_Span sp, qoz_string name, qoz_Vec__qoz_Ty args);
bool ty_ty_args_eq(qoz_Vec__qoz_Ty a, qoz_Vec__qoz_Ty b);
qoz_string ty_ty_show(qoz_Ty* t);
bool ty_arg_is_nil_accepted_by(qoz_Ty* param);
bool ty_untyped_int_fits(qoz_Ty* param);
bool ty_untyped_float_fits(qoz_Ty* param);
bool ty_int_widens_to(qoz_IntInfo param_int, qoz_IntInfo arg_int);
bool ty_arg_passes_to_param(qoz_Ty* param, qoz_Ty* arg);
bool ty_same_constructor_assignable(qoz_Ty* param, qoz_Ty* arg);
bool ty_ty_assignable(qoz_Ty* param, qoz_Ty* arg);
qoz_Vec__qoz_string qoz_vec_make__qoz_string(void);
void qoz_vec_push__qoz_string(qoz_Vec__qoz_string* v, qoz_string x);
bool qoz_map_contains__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key);
qoz_Vec__qoz_Expr qoz_vec_make__qoz_Expr(void);
void qoz_vec_push__qoz_Expr(qoz_Vec__qoz_Expr* v, qoz_Expr* x);
qoz_Vec__qoz_RecordFieldLit qoz_vec_make__qoz_RecordFieldLit(void);
void qoz_vec_push__qoz_RecordFieldLit(qoz_Vec__qoz_RecordFieldLit* v, qoz_RecordFieldLit x);
qoz_Vec__qoz_Stmt qoz_vec_make__qoz_Stmt(void);
void qoz_vec_push__qoz_Stmt(qoz_Vec__qoz_Stmt* v, qoz_Stmt* x);
qoz_Vec__qoz_MatchArm qoz_vec_make__qoz_MatchArm(void);
void qoz_vec_push__qoz_MatchArm(qoz_Vec__qoz_MatchArm* v, qoz_MatchArm x);
qoz_Vec__qoz_Pending qoz_vec_make__qoz_Pending(void);
qoz_Map__qoz_string__bool qoz_map_make__qoz_string__bool(void);
qoz_Vec__qoz_Decl qoz_vec_make__qoz_Decl(void);
void qoz_vec_push__qoz_Pending(qoz_Vec__qoz_Pending* v, qoz_Pending x);
void qoz_map_set__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key, bool value);
void qoz_map_insert__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key, bool value);
void qoz_vec_push__qoz_Decl(qoz_Vec__qoz_Decl* v, qoz_Decl* x);
qoz_Option__bool* qoz_map_get__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key);
qoz_Vec__qoz_Token qoz_vec_make__qoz_Token(void);
void qoz_vec_push__qoz_Token(qoz_Vec__qoz_Token* v, qoz_Token x);
qoz_Vec__qoz_TypeExpr qoz_vec_make__qoz_TypeExpr(void);
void qoz_vec_push__qoz_TypeExpr(qoz_Vec__qoz_TypeExpr* v, qoz_TypeExpr* x);
qoz_Vec__qoz_ClosureParam qoz_vec_make__qoz_ClosureParam(void);
void qoz_vec_push__qoz_ClosureParam(qoz_Vec__qoz_ClosureParam* v, qoz_ClosureParam x);
qoz_Vec__qoz_Pattern qoz_vec_make__qoz_Pattern(void);
void qoz_vec_push__qoz_Pattern(qoz_Vec__qoz_Pattern* v, qoz_Pattern* x);
qoz_Vec__qoz_StructField qoz_vec_make__qoz_StructField(void);
void qoz_vec_push__qoz_StructField(qoz_Vec__qoz_StructField* v, qoz_StructField x);
qoz_Vec__qoz_VariantDecl qoz_vec_make__qoz_VariantDecl(void);
void qoz_vec_push__qoz_VariantDecl(qoz_Vec__qoz_VariantDecl* v, qoz_VariantDecl x);
qoz_Vec__qoz_FnParam qoz_vec_make__qoz_FnParam(void);
void qoz_vec_push__qoz_FnParam(qoz_Vec__qoz_FnParam* v, qoz_FnParam x);
qoz_Map__qoz_string__qoz_Decl qoz_map_make__qoz_string__qoz_Decl(void);
qoz_Map__qoz_string__qoz_string qoz_map_make__qoz_string__qoz_string(void);
qoz_Vec__qoz_TypeError qoz_vec_make__qoz_TypeError(void);
qoz_Map__int64_t__qoz_TypeExpr qoz_map_make__int64_t__qoz_TypeExpr(void);
void qoz_vec_push__qoz_TypeError(qoz_Vec__qoz_TypeError* v, qoz_TypeError x);
void qoz_map_set__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key, qoz_Decl* value);
void qoz_map_insert__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key, qoz_Decl* value);
void qoz_map_set__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key, qoz_string value);
void qoz_map_insert__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key, qoz_string value);
qoz_Vec__qoz_Ty qoz_vec_make__qoz_Ty(void);
void qoz_vec_push__qoz_Ty(qoz_Vec__qoz_Ty* v, qoz_Ty* x);
bool qoz_map_contains__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key);
qoz_Vec__qoz_Binding qoz_vec_make__qoz_Binding(void);
void qoz_vec_push__qoz_Binding(qoz_Vec__qoz_Binding* v, qoz_Binding x);
void qoz_map_set__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key, qoz_TypeExpr* value);
void qoz_map_insert__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key, qoz_TypeExpr* value);
qoz_Option__qoz_Decl* qoz_map_get__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key);
qoz_Option__qoz_string* qoz_map_get__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key);
qoz_Map__qoz_string__qoz_Ty qoz_map_make__qoz_string__qoz_Ty(void);
qoz_Option__qoz_Ty* qoz_map_get__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key);
void qoz_map_set__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key, qoz_Ty* value);
qoz_Vec__qoz_Instantiation qoz_vec_make__qoz_Instantiation(void);
qoz_Map__qoz_string__qoz_TypeExpr qoz_map_make__qoz_string__qoz_TypeExpr(void);
qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr qoz_map_make__qoz_string__qoz_Vec__qoz_TypeExpr(void);
void qoz_vec_push__qoz_Instantiation(qoz_Vec__qoz_Instantiation* v, qoz_Instantiation x);
void qoz_map_set__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key, qoz_TypeExpr* value);
void qoz_map_insert__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key, qoz_TypeExpr* value);
void qoz_map_set__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key, qoz_Vec__qoz_TypeExpr value);
void qoz_map_insert__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key, qoz_Vec__qoz_TypeExpr value);
bool qoz_map_contains__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key);
bool qoz_map_contains__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key);
qoz_Option__qoz_TypeExpr* qoz_map_get__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key);
qoz_Option__qoz_TypeExpr* qoz_map_get__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key);
qoz_Option__qoz_Vec__qoz_TypeExpr* qoz_map_get__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key);
qoz_Vec__qoz_Vec__qoz_StructField qoz_vec_make__qoz_Vec__qoz_StructField(void);
void qoz_vec_push__qoz_Vec__qoz_StructField(qoz_Vec__qoz_Vec__qoz_StructField* v, qoz_Vec__qoz_StructField x);
bool qoz_map_contains__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key);
void qoz_vec_grow__qoz_string(qoz_Vec__qoz_string* v);
int64_t qoz_map_probe__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key);
void qoz_vec_grow__qoz_Expr(qoz_Vec__qoz_Expr* v);
void qoz_vec_grow__qoz_RecordFieldLit(qoz_Vec__qoz_RecordFieldLit* v);
void qoz_vec_grow__qoz_Stmt(qoz_Vec__qoz_Stmt* v);
void qoz_vec_grow__qoz_MatchArm(qoz_Vec__qoz_MatchArm* v);
void qoz_vec_grow__qoz_Pending(qoz_Vec__qoz_Pending* v);
void qoz_map_grow__qoz_string__bool(qoz_Map__qoz_string__bool* m);
void qoz_map_insert_raw__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key, bool value);
void qoz_vec_grow__qoz_Decl(qoz_Vec__qoz_Decl* v);
void qoz_vec_grow__qoz_Token(qoz_Vec__qoz_Token* v);
void qoz_vec_grow__qoz_TypeExpr(qoz_Vec__qoz_TypeExpr* v);
void qoz_vec_grow__qoz_ClosureParam(qoz_Vec__qoz_ClosureParam* v);
void qoz_vec_grow__qoz_Pattern(qoz_Vec__qoz_Pattern* v);
void qoz_vec_grow__qoz_StructField(qoz_Vec__qoz_StructField* v);
void qoz_vec_grow__qoz_VariantDecl(qoz_Vec__qoz_VariantDecl* v);
void qoz_vec_grow__qoz_FnParam(qoz_Vec__qoz_FnParam* v);
void qoz_vec_grow__qoz_TypeError(qoz_Vec__qoz_TypeError* v);
void qoz_map_grow__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m);
void qoz_map_insert_raw__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key, qoz_Decl* value);
void qoz_map_grow__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m);
void qoz_map_insert_raw__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key, qoz_string value);
void qoz_vec_grow__qoz_Ty(qoz_Vec__qoz_Ty* v);
int64_t qoz_map_probe__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key);
void qoz_vec_grow__qoz_Binding(qoz_Vec__qoz_Binding* v);
void qoz_map_grow__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m);
void qoz_map_insert_raw__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key, qoz_TypeExpr* value);
int64_t qoz_map_probe__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key);
int64_t qoz_map_probe__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key);
void qoz_map_insert__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key, qoz_Ty* value);
void qoz_vec_grow__qoz_Instantiation(qoz_Vec__qoz_Instantiation* v);
void qoz_map_grow__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m);
void qoz_map_insert_raw__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key, qoz_TypeExpr* value);
void qoz_map_grow__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m);
void qoz_map_insert_raw__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key, qoz_Vec__qoz_TypeExpr value);
int64_t qoz_map_probe__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key);
int64_t qoz_map_probe__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key);
int64_t qoz_map_probe__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key);
void qoz_vec_grow__qoz_Vec__qoz_StructField(qoz_Vec__qoz_Vec__qoz_StructField* v);
void qoz_map_grow__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m);
void qoz_map_insert_raw__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key, qoz_Ty* value);

qoz_string decl_kind_name(qoz_Decl* d) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_1 = d; qoz_string _qoz_mv_1 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_1->tag) { case qoz_Decl_DImport: { _qoz_mv_1 = (QOZ_STR_LIT("import"));  break; } case qoz_Decl_DFn: { _qoz_mv_1 = (QOZ_STR_LIT("fn"));  break; } case qoz_Decl_DStruct: { _qoz_mv_1 = (QOZ_STR_LIT("struct"));  break; } case qoz_Decl_DEnum: { _qoz_mv_1 = (QOZ_STR_LIT("enum"));  break; } case qoz_Decl_DTypeAlias: { _qoz_mv_1 = (QOZ_STR_LIT("type_alias"));  break; } case qoz_Decl_DConst: { _qoz_mv_1 = (QOZ_STR_LIT("const"));  break; } case qoz_Decl_DExternal: { _qoz_mv_1 = (QOZ_STR_LIT("external"));  break; } case qoz_Decl_DLink: { _qoz_mv_1 = (QOZ_STR_LIT("link"));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_1;
}

qoz_string decl_name(qoz_Decl* d) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_2 = d; qoz_string _qoz_mv_2 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_2->tag) { case qoz_Decl_DImport: { _qoz_mv_2 = (QOZ_STR_LIT("<import>"));  break; } case qoz_Decl_DFn: { qoz_string n = _qoz_ms_2->payload.DFn.f1; _qoz_mv_2 = (n);  break; } case qoz_Decl_DStruct: { qoz_string n = _qoz_ms_2->payload.DStruct.f1; _qoz_mv_2 = (n);  break; } case qoz_Decl_DEnum: { qoz_string n = _qoz_ms_2->payload.DEnum.f1; _qoz_mv_2 = (n);  break; } case qoz_Decl_DTypeAlias: { qoz_string n = _qoz_ms_2->payload.DTypeAlias.f1; _qoz_mv_2 = (n);  break; } case qoz_Decl_DConst: { qoz_string n = _qoz_ms_2->payload.DConst.f1; _qoz_mv_2 = (n);  break; } case qoz_Decl_DExternal: { qoz_string n = _qoz_ms_2->payload.DExternal.f1; _qoz_mv_2 = (n);  break; } case qoz_Decl_DLink: { qoz_string n = _qoz_ms_2->payload.DLink.f2; _qoz_mv_2 = (n);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_2;
}

qoz_Vec__qoz_string prelude_segs(qoz_string path) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string v = qoz_vec_make__qoz_string(); qoz_string cur = QOZ_STR_LIT(""); int64_t i = 0; while (i < (path).len) { int64_t c = qoz_string_byte_at(path, i); if (c == 47) { qoz_vec_push__qoz_string(&v, cur); cur = QOZ_STR_LIT(""); }  else { cur = qoz_string_cat(cur, qoz_string_slice(path, i, i + 1)); } i = i + 1; } if ((cur).len > 0) { qoz_vec_push__qoz_string(&v, cur); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return v;
}

qoz_string resolve_import_path(qoz_Vec__qoz_string segs, qoz_string qoz_root) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if ((segs.len) < 1) { return QOZ_STR_LIT("");} qoz_string last = segs.data[(segs.len) - 1]; qoz_string joined = QOZ_STR_LIT(""); int64_t i = 0; while (i < (segs.len)) { if (i > 0) { joined = qoz_string_cat(joined, QOZ_STR_LIT("/")); } joined = qoz_string_cat(joined, segs.data[i]); i = i + 1; } qoz_string dir_rel = joined; qoz_string legacy_rel = qoz_string_cat(dir_rel, qoz_string_cat(QOZ_STR_LIT("/"), qoz_string_cat(last, QOZ_STR_LIT(".qoz")))); if (qoz_fs_file_exists(legacy_rel)) { return dir_rel;} if ((qoz_root).len > 0) { qoz_string abs_dir = qoz_string_cat(qoz_string_cat(qoz_root, QOZ_STR_LIT("/")), dir_rel); qoz_string abs_legacy = qoz_string_cat(qoz_string_cat(qoz_root, QOZ_STR_LIT("/")), legacy_rel); if (qoz_fs_file_exists(abs_legacy)) { return abs_dir;} } if (((segs.len) >= 2) && qoz_string_eq(segs.data[0], QOZ_STR_LIT("std"))) { qoz_string tmp = qoz_os_getenv(QOZ_STR_LIT("TMPDIR")); if ((tmp).len == 0) { tmp = QOZ_STR_LIT("/tmp"); } if (qoz_string_byte_at(tmp, (tmp).len - 1) == 47) { tmp = qoz_string_slice(tmp, 0, (tmp).len - 1); } qoz_string cached_legacy = qoz_string_cat(tmp, qoz_string_cat(QOZ_STR_LIT("/qoz-stage-a-stdlib/"), legacy_rel)); qoz_string cached_dir = qoz_string_cat(tmp, qoz_string_cat(QOZ_STR_LIT("/qoz-stage-a-stdlib/"), dir_rel)); if (qoz_fs_file_exists(cached_legacy)) { return cached_dir;} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return QOZ_STR_LIT("");
}

qoz_Vec__qoz_string list_package_files(qoz_string dir) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string out = qoz_vec_make__qoz_string(); qoz_string raw = qoz_fs_list_qoz_files(dir); if ((raw).len == 0) { return out;} qoz_Vec__qoz_string parts = strings_split(raw, 10); { qoz_Vec__qoz_string __col = parts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string name = __col.data[__i]; (void)name; if ((name).len > 0) { qoz_string full = qoz_string_cat(qoz_string_cat(dir, QOZ_STR_LIT("/")), name); qoz_vec_push__qoz_string(&out, full); } } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

qoz_Decl* rename_with_pkg(qoz_Decl* d, qoz_string pkg, qoz_Map__qoz_string__bool local_fns) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&d);
    if (qoz_string_eq(pkg, QOZ_STR_LIT(""))) { return d;} qoz_Decl* _qoz_ms_3 = d; qoz_Decl* _qoz_mv_3 = NULL; switch (_qoz_ms_3->tag) { case qoz_Decl_DFn: { qoz_Span sp = _qoz_ms_3->payload.DFn.f0; qoz_string n = _qoz_ms_3->payload.DFn.f1; qoz_Vec__qoz_string tp = _qoz_ms_3->payload.DFn.f2; qoz_Vec__qoz_FnParam fp = _qoz_ms_3->payload.DFn.f3; qoz_TypeExpr* rt = _qoz_ms_3->payload.DFn.f4; qoz_Expr* body = _qoz_ms_3->payload.DFn.f5; qoz_string attr = _qoz_ms_3->payload.DFn.f6; qoz_Decl* _qoz_bv_1;
    {
        qoz_Expr* new_body = rewrite_ident_refs(body, pkg, local_fns); qoz_gc_push_root(&new_body); _qoz_bv_1 = qoz_make_Decl_DFn(sp, qoz_string_cat(qoz_string_cat(pkg, QOZ_STR_LIT("_")), n), tp, fp, rt, new_body, attr);
    }
    _qoz_mv_3 = (_qoz_bv_1);  break; } case qoz_Decl_DExternal: { qoz_Span sp = _qoz_ms_3->payload.DExternal.f0; qoz_string n = _qoz_ms_3->payload.DExternal.f1; qoz_string sym = _qoz_ms_3->payload.DExternal.f2; qoz_Vec__qoz_FnParam fp = _qoz_ms_3->payload.DExternal.f3; qoz_TypeExpr* rt = _qoz_ms_3->payload.DExternal.f4; _qoz_mv_3 = (qoz_make_Decl_DExternal(sp, qoz_string_cat(qoz_string_cat(pkg, QOZ_STR_LIT("_")), n), sym, fp, rt));  break; } default: { _qoz_mv_3 = (d);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_3;
}

qoz_Expr* rewrite_ident_refs(qoz_Expr* e, qoz_string pkg, qoz_Map__qoz_string__bool local_fns) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_Expr* _qoz_ms_4 = e; qoz_Expr* _qoz_mv_4 = NULL; switch (_qoz_ms_4->tag) { case qoz_Expr_EInt: { _qoz_mv_4 = (e);  break; } case qoz_Expr_EFloat: { _qoz_mv_4 = (e);  break; } case qoz_Expr_EString: { _qoz_mv_4 = (e);  break; } case qoz_Expr_EChar: { _qoz_mv_4 = (e);  break; } case qoz_Expr_EBool: { _qoz_mv_4 = (e);  break; } case qoz_Expr_ENil: { _qoz_mv_4 = (e);  break; } case qoz_Expr_EIdent: { qoz_Span sp = _qoz_ms_4->payload.EIdent.f0; qoz_string name = _qoz_ms_4->payload.EIdent.f1; qoz_Expr* _qoz_bv_2;
    {
        if (qoz_map_contains__qoz_string__bool(&local_fns, name)) { return qoz_make_Expr_EIdent(sp, qoz_string_cat(qoz_string_cat(pkg, QOZ_STR_LIT("_")), name));} _qoz_bv_2 = e;
    }
    _qoz_mv_4 = (_qoz_bv_2);  break; } case qoz_Expr_EPath: { _qoz_mv_4 = (e);  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_4->payload.EUnary.f0; qoz_UnaryOp* op = _qoz_ms_4->payload.EUnary.f1; qoz_Expr* rhs = _qoz_ms_4->payload.EUnary.f2; _qoz_mv_4 = (qoz_make_Expr_EUnary(sp, op, rewrite_ident_refs(rhs, pkg, local_fns)));  break; } case qoz_Expr_EBinary: { qoz_Span sp = _qoz_ms_4->payload.EBinary.f0; qoz_BinaryOp* op = _qoz_ms_4->payload.EBinary.f1; qoz_Expr* l = _qoz_ms_4->payload.EBinary.f2; qoz_Expr* r = _qoz_ms_4->payload.EBinary.f3; _qoz_mv_4 = (qoz_make_Expr_EBinary(sp, op, rewrite_ident_refs(l, pkg, local_fns), rewrite_ident_refs(r, pkg, local_fns)));  break; } case qoz_Expr_EAssign: { qoz_Span sp = _qoz_ms_4->payload.EAssign.f0; qoz_AssignOp* op = _qoz_ms_4->payload.EAssign.f1; qoz_Expr* l = _qoz_ms_4->payload.EAssign.f2; qoz_Expr* r = _qoz_ms_4->payload.EAssign.f3; _qoz_mv_4 = (qoz_make_Expr_EAssign(sp, op, rewrite_ident_refs(l, pkg, local_fns), rewrite_ident_refs(r, pkg, local_fns)));  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_4->payload.ECall.f0; qoz_Expr* callee = _qoz_ms_4->payload.ECall.f1; qoz_Vec__qoz_TypeExpr ta = _qoz_ms_4->payload.ECall.f2; qoz_Vec__qoz_Expr args = _qoz_ms_4->payload.ECall.f3; qoz_Expr* _qoz_bv_3;
    {
        qoz_Vec__qoz_Expr na = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_Expr(&na, rewrite_ident_refs(a, pkg, local_fns)); } }_qoz_bv_3 = qoz_make_Expr_ECall(sp, rewrite_ident_refs(callee, pkg, local_fns), ta, na);
    }
    _qoz_mv_4 = (_qoz_bv_3);  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_4->payload.EField.f0; qoz_Expr* base = _qoz_ms_4->payload.EField.f1; qoz_string name = _qoz_ms_4->payload.EField.f2; _qoz_mv_4 = (qoz_make_Expr_EField(sp, rewrite_ident_refs(base, pkg, local_fns), name));  break; } case qoz_Expr_EIndex: { qoz_Span sp = _qoz_ms_4->payload.EIndex.f0; qoz_Expr* base = _qoz_ms_4->payload.EIndex.f1; qoz_Expr* idx = _qoz_ms_4->payload.EIndex.f2; _qoz_mv_4 = (qoz_make_Expr_EIndex(sp, rewrite_ident_refs(base, pkg, local_fns), rewrite_ident_refs(idx, pkg, local_fns)));  break; } case qoz_Expr_ECast: { qoz_Span sp = _qoz_ms_4->payload.ECast.f0; qoz_Expr* value = _qoz_ms_4->payload.ECast.f1; qoz_TypeExpr* t = _qoz_ms_4->payload.ECast.f2; _qoz_mv_4 = (qoz_make_Expr_ECast(sp, rewrite_ident_refs(value, pkg, local_fns), t));  break; } case qoz_Expr_ETry: { qoz_Span sp = _qoz_ms_4->payload.ETry.f0; qoz_Expr* v = _qoz_ms_4->payload.ETry.f1; _qoz_mv_4 = (qoz_make_Expr_ETry(sp, rewrite_ident_refs(v, pkg, local_fns)));  break; } case qoz_Expr_ETuple: { qoz_Span sp = _qoz_ms_4->payload.ETuple.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_4->payload.ETuple.f1; qoz_Expr* _qoz_bv_4;
    {
        qoz_Vec__qoz_Expr ne = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_Expr(&ne, rewrite_ident_refs(el, pkg, local_fns)); } }_qoz_bv_4 = qoz_make_Expr_ETuple(sp, ne);
    }
    _qoz_mv_4 = (_qoz_bv_4);  break; } case qoz_Expr_ERecord: { qoz_Span sp = _qoz_ms_4->payload.ERecord.f0; qoz_TypeExpr* te = _qoz_ms_4->payload.ERecord.f1; qoz_Vec__qoz_RecordFieldLit fields = _qoz_ms_4->payload.ERecord.f2; qoz_Expr* _qoz_bv_5;
    {
        qoz_Vec__qoz_RecordFieldLit nf = qoz_vec_make__qoz_RecordFieldLit(); { qoz_Vec__qoz_RecordFieldLit __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_RecordFieldLit f = __col.data[__i]; (void)f; qoz_vec_push__qoz_RecordFieldLit(&nf, ((qoz_RecordFieldLit){ .name = f.name, .value = rewrite_ident_refs(f.value, pkg, local_fns) })); } }_qoz_bv_5 = qoz_make_Expr_ERecord(sp, te, nf);
    }
    _qoz_mv_4 = (_qoz_bv_5);  break; } case qoz_Expr_EClosure: { qoz_Span sp = _qoz_ms_4->payload.EClosure.f0; qoz_Vec__qoz_ClosureParam cps = _qoz_ms_4->payload.EClosure.f1; qoz_TypeExpr* ret = _qoz_ms_4->payload.EClosure.f2; qoz_Expr* body = _qoz_ms_4->payload.EClosure.f3; _qoz_mv_4 = (qoz_make_Expr_EClosure(sp, cps, ret, rewrite_ident_refs(body, pkg, local_fns)));  break; } case qoz_Expr_EBlock: { qoz_Span sp = _qoz_ms_4->payload.EBlock.f0; qoz_Vec__qoz_Stmt stmts = _qoz_ms_4->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_4->payload.EBlock.f2; qoz_Expr* _qoz_bv_6;
    {
        qoz_Vec__qoz_Stmt ns = qoz_vec_make__qoz_Stmt(); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; qoz_vec_push__qoz_Stmt(&ns, rewrite_ident_refs_stmt(s, pkg, local_fns)); } }_qoz_bv_6 = qoz_make_Expr_EBlock(sp, ns, rewrite_ident_refs(tail, pkg, local_fns));
    }
    _qoz_mv_4 = (_qoz_bv_6);  break; } case qoz_Expr_EIf: { qoz_Span sp = _qoz_ms_4->payload.EIf.f0; qoz_Expr* c = _qoz_ms_4->payload.EIf.f1; qoz_Expr* t = _qoz_ms_4->payload.EIf.f2; qoz_Expr* f = _qoz_ms_4->payload.EIf.f3; _qoz_mv_4 = (qoz_make_Expr_EIf(sp, rewrite_ident_refs(c, pkg, local_fns), rewrite_ident_refs(t, pkg, local_fns), rewrite_ident_refs(f, pkg, local_fns)));  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_4->payload.EMatch.f0; qoz_Expr* scrut = _qoz_ms_4->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_4->payload.EMatch.f2; qoz_Expr* _qoz_bv_7;
    {
        qoz_Vec__qoz_MatchArm na = qoz_vec_make__qoz_MatchArm(); { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm a = __col.data[__i]; (void)a; qoz_vec_push__qoz_MatchArm(&na, ((qoz_MatchArm){ .pat = a.pat, .body = rewrite_ident_refs(a.body, pkg, local_fns), .has_guard = a.has_guard, .guard = a.guard })); } }_qoz_bv_7 = qoz_make_Expr_EMatch(sp, rewrite_ident_refs(scrut, pkg, local_fns), na);
    }
    _qoz_mv_4 = (_qoz_bv_7);  break; } case qoz_Expr_EWhile: { qoz_Span sp = _qoz_ms_4->payload.EWhile.f0; qoz_Expr* c = _qoz_ms_4->payload.EWhile.f1; qoz_Expr* b = _qoz_ms_4->payload.EWhile.f2; _qoz_mv_4 = (qoz_make_Expr_EWhile(sp, rewrite_ident_refs(c, pkg, local_fns), rewrite_ident_refs(b, pkg, local_fns)));  break; } case qoz_Expr_EFor: { qoz_Span sp = _qoz_ms_4->payload.EFor.f0; qoz_string b1 = _qoz_ms_4->payload.EFor.f1; qoz_string b2 = _qoz_ms_4->payload.EFor.f2; qoz_Expr* it = _qoz_ms_4->payload.EFor.f3; qoz_Expr* body = _qoz_ms_4->payload.EFor.f4; _qoz_mv_4 = (qoz_make_Expr_EFor(sp, b1, b2, rewrite_ident_refs(it, pkg, local_fns), rewrite_ident_refs(body, pkg, local_fns)));  break; } case qoz_Expr_EReturn: { qoz_Span sp = _qoz_ms_4->payload.EReturn.f0; qoz_Expr* v = _qoz_ms_4->payload.EReturn.f1; _qoz_mv_4 = (qoz_make_Expr_EReturn(sp, rewrite_ident_refs(v, pkg, local_fns)));  break; } case qoz_Expr_EDefer: { qoz_Span sp = _qoz_ms_4->payload.EDefer.f0; qoz_Expr* v = _qoz_ms_4->payload.EDefer.f1; _qoz_mv_4 = (qoz_make_Expr_EDefer(sp, rewrite_ident_refs(v, pkg, local_fns)));  break; } case qoz_Expr_ESizeOf: { _qoz_mv_4 = (e);  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_4->payload.EArrayLit.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_4->payload.EArrayLit.f1; qoz_Expr* _qoz_bv_8;
    {
        qoz_Vec__qoz_Expr ne = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_Expr(&ne, rewrite_ident_refs(el, pkg, local_fns)); } }_qoz_bv_8 = qoz_make_Expr_EArrayLit(sp, ne);
    }
    _qoz_mv_4 = (_qoz_bv_8);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_4;
}

qoz_Stmt* rewrite_ident_refs_stmt(qoz_Stmt* s, qoz_string pkg, qoz_Map__qoz_string__bool local_fns) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&s);
    qoz_Stmt* _qoz_ms_5 = s; qoz_Stmt* _qoz_mv_5 = NULL; switch (_qoz_ms_5->tag) { case qoz_Stmt_SLet: { qoz_Span sp = _qoz_ms_5->payload.SLet.f0; qoz_string name = _qoz_ms_5->payload.SLet.f1; qoz_TypeExpr* te = _qoz_ms_5->payload.SLet.f2; qoz_Expr* val = _qoz_ms_5->payload.SLet.f3; _qoz_mv_5 = (qoz_make_Stmt_SLet(sp, name, te, rewrite_ident_refs(val, pkg, local_fns)));  break; } case qoz_Stmt_SVar: { qoz_Span sp = _qoz_ms_5->payload.SVar.f0; qoz_string name = _qoz_ms_5->payload.SVar.f1; qoz_TypeExpr* te = _qoz_ms_5->payload.SVar.f2; qoz_Expr* val = _qoz_ms_5->payload.SVar.f3; _qoz_mv_5 = (qoz_make_Stmt_SVar(sp, name, te, rewrite_ident_refs(val, pkg, local_fns)));  break; } case qoz_Stmt_SExpr: { qoz_Span sp = _qoz_ms_5->payload.SExpr.f0; qoz_Expr* x = _qoz_ms_5->payload.SExpr.f1; _qoz_mv_5 = (qoz_make_Stmt_SExpr(sp, rewrite_ident_refs(x, pkg, local_fns)));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_5;
}

qoz_Loaded load_all(qoz_string entry, qoz_string qoz_root) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_Pending queue = qoz_vec_make__qoz_Pending(); qoz_Map__qoz_string__bool visited = qoz_map_make__qoz_string__bool(); qoz_Vec__qoz_Decl aggregated_decls = qoz_vec_make__qoz_Decl(); qoz_Vec__qoz_string sources = qoz_vec_make__qoz_string(); qoz_Vec__qoz_string prelude_paths = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&prelude_paths, QOZ_STR_LIT("std/option")); qoz_vec_push__qoz_string(&prelude_paths, QOZ_STR_LIT("std/result")); { qoz_Vec__qoz_string __col = prelude_paths; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string pp = __col.data[__i]; (void)pp; qoz_string resolved_dir = resolve_import_path(prelude_segs(pp), qoz_root); if (!qoz_string_eq(resolved_dir, QOZ_STR_LIT(""))) { qoz_Vec__qoz_string files = list_package_files(resolved_dir); { qoz_Vec__qoz_string __col = files; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string fpath = __col.data[__i]; (void)fpath; if (!qoz_map_contains__qoz_string__bool(&visited, fpath)) { qoz_vec_push__qoz_Pending(&queue, ((qoz_Pending){ .path = fpath, .pkg = QOZ_STR_LIT("") })); } } }} } }qoz_vec_push__qoz_Pending(&queue, ((qoz_Pending){ .path = entry, .pkg = QOZ_STR_LIT("") })); int64_t qi = 0; while (qi < (queue.len)) { qoz_Pending cur = queue.data[qi]; qi = qi + 1; if (!qoz_map_contains__qoz_string__bool(&visited, cur.path)) { qoz_map_set__qoz_string__bool(&visited, cur.path, true); process_one(cur, &queue, &visited, &aggregated_decls, &sources, qoz_root); } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Loaded){ .file = ((qoz_File){ .path = entry, .decls = aggregated_decls }), .sources = sources });
}

void process_one(qoz_Pending cur, qoz_Vec__qoz_Pending* queue, qoz_Map__qoz_string__bool* visited, qoz_Vec__qoz_Decl* aggregated_decls, qoz_Vec__qoz_string* sources, qoz_string qoz_root) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&queue);
    qoz_gc_push_root(&visited);
    qoz_gc_push_root(&aggregated_decls);
    qoz_gc_push_root(&sources);
    qoz_string src = qoz_fs_read_file(cur.path); if ((src).len < 0) { qoz_string _qoz_bv_9;
    {
        qoz_Strbuf _qoz_sb_240_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_240_21); strings_sb_append(&_qoz_sb_240_21, QOZ_STR_LIT("could not read ")); strings_sb_append(&_qoz_sb_240_21, cur.path); _qoz_bv_9 = strings_sb_finish(&_qoz_sb_240_21);
    }
    fmt_println(_qoz_bv_9); qoz_os_exit(1); } qoz_vec_push__qoz_string(sources, src); qoz_Vec__qoz_Token tokens = tokenize_run(src); qoz_ParseOutput parsed = parse_run(tokens, cur.path); if ((parsed.errors.len) > 0) { { qoz_Vec__qoz_string __col = parsed.errors; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string msg = __col.data[__i]; (void)msg; fmt_println(qoz_string_cat(qoz_string_cat(cur.path, QOZ_STR_LIT(": ")), msg)); } }qoz_os_exit(1); } qoz_File file = parsed.file; qoz_Map__qoz_string__bool local_fns = qoz_map_make__qoz_string__bool(); { qoz_Vec__qoz_Decl __col = file.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_6 = d; switch (_qoz_ms_6->tag) { case qoz_Decl_DFn: { qoz_string n = _qoz_ms_6->payload.DFn.f1; qoz_map_set__qoz_string__bool(&local_fns, n, true);  break; } case qoz_Decl_DExternal: { qoz_string n = _qoz_ms_6->payload.DExternal.f1; qoz_map_set__qoz_string__bool(&local_fns, n, true);  break; } default: { NULL;  break; } } 0; } }{ qoz_Vec__qoz_Decl __col = file.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_7 = d; switch (_qoz_ms_7->tag) { case qoz_Decl_DImport: { qoz_Vec__qoz_string ipath = _qoz_ms_7->payload.DImport.f1; qoz_string alias = _qoz_ms_7->payload.DImport.f2; {
        qoz_string resolved_dir = resolve_import_path(ipath, qoz_root); if (qoz_string_eq(resolved_dir, QOZ_STR_LIT(""))) { qoz_string _qoz_bv_10;
    {
        qoz_Strbuf _qoz_sb_267_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_267_29); strings_sb_append(&_qoz_sb_267_29, QOZ_STR_LIT("could not resolve import ")); strings_sb_append(&_qoz_sb_267_29, ipath.data[(ipath.len) - 1]); _qoz_bv_10 = strings_sb_finish(&_qoz_sb_267_29);
    }
    fmt_println(_qoz_bv_10); qoz_os_exit(1); } qoz_string pkg = alias; if (qoz_string_eq(pkg, QOZ_STR_LIT(""))) { pkg = ipath.data[(ipath.len) - 1]; } qoz_Vec__qoz_string files = list_package_files(resolved_dir); { qoz_Vec__qoz_string __col = files; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string fpath = __col.data[__i]; (void)fpath; if (!qoz_map_contains__qoz_string__bool(visited, fpath)) { qoz_vec_push__qoz_Pending(queue, ((qoz_Pending){ .path = fpath, .pkg = pkg })); } } }
    }
    0;  break; } default: { NULL;  break; } } 0; qoz_vec_push__qoz_Decl(aggregated_decls, rename_with_pkg(d, cur.pkg, local_fns)); } }
    return;
}

qoz_string cmd_build(qoz_string path, qoz_string qoz_root) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Loaded loaded = load_all(path, qoz_root); qoz_File file = loaded.file; qoz_TyContext tc = check_make_ctx(); check_register_file(&tc, file); check_validate_signatures(&tc, file); qoz_File inferred = check_infer_calls(&tc, file); check_check_fn_bodies(&tc, inferred); check_report(&tc); if ((tc.errors.len) > 0) { qoz_os_exit(1); } qoz_string c_source = emit_emit_program(inferred, tc.expr_types); qoz_Vec__qoz_string _keep_alive = loaded.sources; qoz_string out_path = qoz_string_cat(path, QOZ_STR_LIT(".c")); bool ok = qoz_fs_write_file(out_path, c_source); if (!ok) { qoz_string _qoz_bv_11;
    {
        qoz_Strbuf _qoz_sb_307_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_307_21); strings_sb_append(&_qoz_sb_307_21, QOZ_STR_LIT("could not write ")); strings_sb_append(&_qoz_sb_307_21, out_path); _qoz_bv_11 = strings_sb_finish(&_qoz_sb_307_21);
    }
    fmt_println(_qoz_bv_11); qoz_os_exit(1); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return out_path;
}

qoz_Vec__qoz_string clang_argv(qoz_string c_path, qoz_string bin_path) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string argv = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("clang")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-std=c11")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-pedantic")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-O3")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Wall")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Werror")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Wno-unused-function")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Wno-unused-variable")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Wno-unused-but-set-variable")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Wno-unused-const-variable")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Wno-parentheses-equality")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Wno-unused-value")); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-Wno-overlength-strings")); qoz_vec_push__qoz_string(&argv, c_path); qoz_vec_push__qoz_string(&argv, QOZ_STR_LIT("-o")); qoz_vec_push__qoz_string(&argv, bin_path); qoz_gc_shadow_set_top(_qoz_shadow_guard); return argv;
}

void cmd_run(qoz_string path, qoz_string qoz_root) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_string c_path = cmd_build(path, qoz_root); qoz_string bin_path = qoz_string_cat(path, QOZ_STR_LIT(".bin")); qoz_Vec__qoz_string cargv = clang_argv(c_path, bin_path); qoz_ProcessResult clang_result = os_process_exec(cargv); if (clang_result.exit_code != 0) { fmt_print(clang_result.stdout); fmt_print(clang_result.stderr); qoz_os_exit(1); } qoz_Vec__qoz_string run_argv = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&run_argv, bin_path); qoz_ProcessResult result = os_process_exec(run_argv); fmt_print(result.stdout); fmt_print(result.stderr); qoz_os_exit(result.exit_code); 
    return;
}

int main(int argc, char **argv) {
    qoz_set_argv(argc, argv);
    int qoz_stack_anchor;
    qoz_init(&qoz_stack_anchor);
    qoz_Vec__qoz_string args = os_args(); if ((args.len) < 2) { fmt_println(QOZ_STR_LIT("usage: qoz <subcommand> <path>")); fmt_println(QOZ_STR_LIT("       qoz build <path>   compile to <path>.c and clang to <path>.bin")); fmt_println(QOZ_STR_LIT("       qoz run   <path>   build, then execute and propagate exit code")); fmt_println(QOZ_STR_LIT("       qoz emit  <path>   write <path>.c only (no clang)")); qoz_os_exit(1); } qoz_string qoz_root = qoz_os_getenv(QOZ_STR_LIT("QOZ_ROOT")); qoz_string first = args.data[1]; if (qoz_string_eq(first, QOZ_STR_LIT("emit"))) { if ((args.len) < 3) { fmt_println(QOZ_STR_LIT("usage: qoz emit <path>")); qoz_os_exit(1); } qoz_string out = cmd_build(args.data[2], qoz_root); qoz_string _qoz_bv_12;
    {
        qoz_Strbuf _qoz_sb_380_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_380_21); strings_sb_append(&_qoz_sb_380_21, QOZ_STR_LIT("wrote ")); strings_sb_append(&_qoz_sb_380_21, out); _qoz_bv_12 = strings_sb_finish(&_qoz_sb_380_21);
    }
    fmt_println(_qoz_bv_12); qoz_os_exit(0); }  else { if (qoz_string_eq(first, QOZ_STR_LIT("build"))) { if ((args.len) < 3) { fmt_println(QOZ_STR_LIT("usage: qoz build <path>")); qoz_os_exit(1); } qoz_string path = args.data[2]; qoz_string c_path = cmd_build(path, qoz_root); qoz_string bin_path = qoz_string_cat(path, QOZ_STR_LIT(".bin")); qoz_Vec__qoz_string cargv = clang_argv(c_path, bin_path); qoz_ProcessResult r = os_process_exec(cargv); if (r.exit_code != 0) { fmt_print(r.stdout); fmt_print(r.stderr); qoz_os_exit(1); } qoz_os_exit(0); }  else { if (qoz_string_eq(first, QOZ_STR_LIT("run"))) { if ((args.len) < 3) { fmt_println(QOZ_STR_LIT("usage: qoz run <path>")); qoz_os_exit(1); } cmd_run(args.data[2], qoz_root); }  else { qoz_string out = cmd_build(first, qoz_root); qoz_string _qoz_bv_13;
    {
        qoz_Strbuf _qoz_sb_410_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_410_21); strings_sb_append(&_qoz_sb_410_21, QOZ_STR_LIT("wrote ")); strings_sb_append(&_qoz_sb_410_21, out); _qoz_bv_13 = strings_sb_finish(&_qoz_sb_410_21);
    }
    fmt_println(_qoz_bv_13); qoz_os_exit(0); } } } 
    qoz_shutdown();
    return 0;
}
void fmt_println(qoz_string s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_print_str(s); qoz_print_nl(); 
    return;
}

void fmt_print(qoz_string s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_print_str(s); 
    return;
}

qoz_Vec__qoz_string os_args(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string v = qoz_vec_make__qoz_string(); int64_t n = qoz_os_argc(); int64_t i = 0; while (i < n) { qoz_vec_push__qoz_string(&v, qoz_os_arg(i)); i = i + 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return v;
}

qoz_ProcessResult os_process_exec(qoz_Vec__qoz_string argv) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    int64_t exit_code = 0; qoz_string so = QOZ_STR_LIT(""); qoz_string se = QOZ_STR_LIT(""); qoz_process_exec(argv.data, argv.len, &exit_code, &so, &se); qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_ProcessResult){ .exit_code = exit_code, .stdout = so, .stderr = se });
}

void strings_sb_init(qoz_Strbuf* b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    b->buf = ((uint8_t*)NULL); b->len = 0; b->cap = 0; 
    return;
}

void strings_sb_grow(qoz_Strbuf* b, int64_t needed) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    int64_t new_cap = ((b->cap == 0) ? 64 : b->cap); while (new_cap < b->len + needed) { new_cap = new_cap * 2; } b->buf = ((uint8_t*)qoz_realloc(((void*)b->buf), new_cap)); b->cap = new_cap; 
    return;
}

void strings_sb_append(qoz_Strbuf* b, qoz_string s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    int64_t n = (s).len; if (n <= 0) { return;} if (b->len + n > b->cap) { strings_sb_grow(b, n); } int64_t dst = ((int64_t)((void*)b->buf)) + b->len; qoz_bytes_copy(((void*)dst), qoz_string_data(s), n); b->len = b->len + n; 
    return;
}

int64_t strings_sb_len(qoz_Strbuf* b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return b->len;
}

void strings_sb_truncate(qoz_Strbuf* b, int64_t new_len) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    if (new_len < 0) { return;} if (new_len > b->len) { return;} b->len = new_len; 
    return;
}

qoz_string strings_sb_finish(qoz_Strbuf* b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_alias(((void*)b->buf), b->len);
}

void strings_sb_append_i64(qoz_Strbuf* b, int64_t v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    qoz_strbuf_append_i64(((void*)b), v);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

void strings_sb_append_bool(qoz_Strbuf* b, bool v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    qoz_strbuf_append_bool(((void*)b), v);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

void strings_sb_append_f64(qoz_Strbuf* b, double v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    qoz_strbuf_append_f64(((void*)b), v);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

qoz_string strings_sb_slice_copy(qoz_Strbuf* b, int64_t from, int64_t to) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&b);
    int64_t lo = from; int64_t hi = to; if (lo < 0) { lo = 0; } if (hi > b->len) { hi = b->len; } if (lo > hi) { lo = hi; } int64_t n = hi - lo; void* buf = qoz_alloc(n); qoz_gc_push_root(&buf); if (n > 0) { int64_t src_off = ((int64_t)((void*)b->buf)) + lo; qoz_bytes_copy(buf, ((void*)src_off), n); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_alias(buf, n);
}

bool strings_eq(qoz_string* a, qoz_string* b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&a);
    qoz_gc_push_root(&b);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_eq(*a, *b);
}

uint64_t strings_hash(qoz_string* s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&s);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_hash(*s);
}

bool strings_lt_raw(qoz_string a, qoz_string b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    int64_t an = (a).len; int64_t bn = (b).len; int64_t i = 0; while ((i < an) && (i < bn)) { int64_t ca = qoz_string_byte_at(a, i); int64_t cb = qoz_string_byte_at(b, i); if (ca != cb) { return ca < cb;} i = i + 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return an < bn;
}

bool strings_lt(qoz_string* a, qoz_string* b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&a);
    qoz_gc_push_root(&b);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return strings_lt_raw(*a, *b);
}

qoz_string strings_replace_all(qoz_string s, qoz_string needle, qoz_string replacement) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    int64_t nn = (needle).len; if (nn == 0) { return s;} qoz_string out = QOZ_STR_LIT(""); int64_t i = 0; int64_t total = (s).len; while (i + nn <= total) { if (qoz_string_has_prefix(qoz_string_slice(s, i, total), needle)) { out = qoz_string_cat(out, replacement); i = i + nn; }  else { out = qoz_string_cat(out, qoz_string_slice(s, i, i + 1)); i = i + 1; } } if (i < total) { out = qoz_string_cat(out, qoz_string_slice(s, i, total)); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

qoz_Vec__qoz_string strings_split(qoz_string s, int64_t sep) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string out = qoz_vec_make__qoz_string(); int64_t total = (s).len; int64_t start = 0; int64_t i = 0; while (i < total) { if (qoz_string_byte_at(s, i) == sep) { qoz_vec_push__qoz_string(&out, qoz_string_slice(s, start, i)); start = i + 1; } i = i + 1; } qoz_vec_push__qoz_string(&out, qoz_string_slice(s, start, total)); qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

qoz_Lexer tokenize_make_lexer(qoz_string src) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Lexer){ .src = src, .pos = 0, .line = 1, .col = 1 });
}

int64_t tokenize_lex_at(qoz_Lexer* l) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&l);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_byte_at(l->src, l->pos);
}

void tokenize_lex_advance(qoz_Lexer* l) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&l);
    int64_t c = tokenize_lex_at(l); l->pos = l->pos + 1; if (c == 10) { l->line = l->line + 1; l->col = 1; }  else { l->col = l->col + 1; } 
    return;
}

bool tokenize_is_space(int64_t c) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return (((c == 32) || (c == 9)) || (c == 10)) || (c == 13);
}

bool tokenize_is_digit(int64_t c) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return (c >= 48) && (c <= 57);
}

bool tokenize_is_alpha(int64_t c) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return (((c >= 65) && (c <= 90)) || ((c >= 97) && (c <= 122))) || (c == 95);
}

bool tokenize_is_alnum(int64_t c) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return tokenize_is_alpha(c) || tokenize_is_digit(c);
}

bool tokenize_is_hex_digit(int64_t c) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return (tokenize_is_digit(c) || ((c >= 65) && (c <= 70))) || ((c >= 97) && (c <= 102));
}

bool tokenize_is_bin_digit(int64_t c) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return (c == 48) || (c == 49);
}

bool tokenize_is_oct_digit(int64_t c) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return (c >= 48) && (c <= 55);
}

bool tokenize_is_punct(int64_t c) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((((((((((((((((((((((c == 40) || (c == 41)) || (c == 123)) || (c == 125)) || (c == 91)) || (c == 93)) || (c == 44)) || (c == 59)) || (c == 46)) || (c == 58)) || (c == 61)) || (c == 43)) || (c == 45)) || (c == 42)) || (c == 47)) || (c == 37)) || (c == 60)) || (c == 62)) || (c == 33)) || (c == 38)) || (c == 124)) || (c == 64)) || (c == 63);
}

void tokenize_skip_whitespace_and_comments(qoz_Lexer* l) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&l);
    bool changed = true; while (changed) { changed = false; while ((l->pos < (l->src).len) && tokenize_is_space(tokenize_lex_at(l))) { tokenize_lex_advance(l); changed = true; } if (((l->pos + 1 < (l->src).len) && (tokenize_lex_at(l) == 47)) && (qoz_string_byte_at(l->src, l->pos + 1) == 47)) { while ((l->pos < (l->src).len) && (tokenize_lex_at(l) != 10)) { tokenize_lex_advance(l); } changed = true; } if (((l->pos + 1 < (l->src).len) && (tokenize_lex_at(l) == 47)) && (qoz_string_byte_at(l->src, l->pos + 1) == 42)) { tokenize_lex_advance(l); tokenize_lex_advance(l); int64_t depth = 1; while ((depth > 0) && (l->pos < (l->src).len)) { if (((l->pos + 1 < (l->src).len) && (tokenize_lex_at(l) == 47)) && (qoz_string_byte_at(l->src, l->pos + 1) == 42)) { tokenize_lex_advance(l); tokenize_lex_advance(l); depth = depth + 1; }  else if (((l->pos + 1 < (l->src).len) && (tokenize_lex_at(l) == 42)) && (qoz_string_byte_at(l->src, l->pos + 1) == 47)) { tokenize_lex_advance(l); tokenize_lex_advance(l); depth = depth - 1; }  else { tokenize_lex_advance(l); } } changed = true; } } 
    return;
}

bool tokenize_is_keyword(qoz_Map__qoz_string__bool* kw, qoz_string s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&kw);
    qoz_Option__bool* _qoz_ms_8 = qoz_map_get__qoz_string__bool(kw, s); bool _qoz_mv_8 = false; switch (_qoz_ms_8->tag) { case qoz_Option__bool_Some: { _qoz_mv_8 = (true);  break; } case qoz_Option__bool_None: { _qoz_mv_8 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_8;
}

qoz_Map__qoz_string__bool tokenize_build_keywords(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Map__qoz_string__bool m = qoz_map_make__qoz_string__bool(); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("let"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("var"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("type"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("if"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("else"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("match"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("while"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("for"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("in"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("return"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("defer"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("import"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("external"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("as"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("new"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("true"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("false"), true); qoz_map_set__qoz_string__bool(&m, QOZ_STR_LIT("nil"), true); qoz_gc_shadow_set_top(_qoz_shadow_guard); return m;
}

qoz_Token tokenize_lex_one(qoz_Lexer* l, qoz_Map__qoz_string__bool* kw) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&l);
    qoz_gc_push_root(&kw);
    tokenize_skip_whitespace_and_comments(l); if (l->pos >= (l->src).len) { return ((qoz_Token){ .kind = qoz_make_TokenKind_TokEOF(), .text = QOZ_STR_LIT(""), .line = l->line, .col = l->col });} int64_t start_pos = l->pos; int64_t start_line = l->line; int64_t start_col = l->col; int64_t c = tokenize_lex_at(l); if (tokenize_is_digit(c)) { if ((c == 48) && (l->pos + 1 < (l->src).len)) { int64_t n = qoz_string_byte_at(l->src, l->pos + 1); if ((n == 120) || (n == 88)) { tokenize_lex_advance(l); tokenize_lex_advance(l); while ((l->pos < (l->src).len) && (tokenize_is_hex_digit(tokenize_lex_at(l)) || (tokenize_lex_at(l) == 95))) { tokenize_lex_advance(l); } return ((qoz_Token){ .kind = qoz_make_TokenKind_TokInt(), .text = qoz_string_slice(l->src, start_pos, l->pos), .line = start_line, .col = start_col });} if ((n == 98) || (n == 66)) { tokenize_lex_advance(l); tokenize_lex_advance(l); while ((l->pos < (l->src).len) && (tokenize_is_bin_digit(tokenize_lex_at(l)) || (tokenize_lex_at(l) == 95))) { tokenize_lex_advance(l); } return ((qoz_Token){ .kind = qoz_make_TokenKind_TokInt(), .text = qoz_string_slice(l->src, start_pos, l->pos), .line = start_line, .col = start_col });} if ((n == 111) || (n == 79)) { tokenize_lex_advance(l); tokenize_lex_advance(l); while ((l->pos < (l->src).len) && (tokenize_is_oct_digit(tokenize_lex_at(l)) || (tokenize_lex_at(l) == 95))) { tokenize_lex_advance(l); } return ((qoz_Token){ .kind = qoz_make_TokenKind_TokInt(), .text = qoz_string_slice(l->src, start_pos, l->pos), .line = start_line, .col = start_col });} } bool is_float = false; while ((l->pos < (l->src).len) && (tokenize_is_digit(tokenize_lex_at(l)) || (tokenize_lex_at(l) == 95))) { tokenize_lex_advance(l); } if (((l->pos + 1 < (l->src).len) && (tokenize_lex_at(l) == 46)) && tokenize_is_digit(qoz_string_byte_at(l->src, l->pos + 1))) { is_float = true; tokenize_lex_advance(l); while ((l->pos < (l->src).len) && (tokenize_is_digit(tokenize_lex_at(l)) || (tokenize_lex_at(l) == 95))) { tokenize_lex_advance(l); } } if ((l->pos < (l->src).len) && ((tokenize_lex_at(l) == 101) || (tokenize_lex_at(l) == 69))) { is_float = true; tokenize_lex_advance(l); if ((l->pos < (l->src).len) && ((tokenize_lex_at(l) == 43) || (tokenize_lex_at(l) == 45))) { tokenize_lex_advance(l); } while ((l->pos < (l->src).len) && tokenize_is_digit(tokenize_lex_at(l))) { tokenize_lex_advance(l); } } qoz_TokenKind* kind = ((is_float) ? qoz_make_TokenKind_TokFloat() : qoz_make_TokenKind_TokInt()); qoz_gc_push_root(&kind); return ((qoz_Token){ .kind = kind, .text = qoz_string_slice(l->src, start_pos, l->pos), .line = start_line, .col = start_col });} if (c == 39) { tokenize_lex_advance(l); if ((l->pos < (l->src).len) && (tokenize_lex_at(l) == 92)) { tokenize_lex_advance(l); } if (l->pos < (l->src).len) { tokenize_lex_advance(l); } if ((l->pos < (l->src).len) && (tokenize_lex_at(l) == 39)) { tokenize_lex_advance(l); } return ((qoz_Token){ .kind = qoz_make_TokenKind_TokChar(), .text = qoz_string_slice(l->src, start_pos, l->pos), .line = start_line, .col = start_col });} if (tokenize_is_alpha(c)) { while ((l->pos < (l->src).len) && tokenize_is_alnum(tokenize_lex_at(l))) { tokenize_lex_advance(l); } qoz_string word = qoz_string_slice(l->src, start_pos, l->pos); qoz_TokenKind* k = qoz_make_TokenKind_TokIdent(); qoz_gc_push_root(&k); if (tokenize_is_keyword(kw, word)) { k = qoz_make_TokenKind_TokKeyword(); } return ((qoz_Token){ .kind = k, .text = word, .line = start_line, .col = start_col });} if (c == 34) { tokenize_lex_advance(l); while ((l->pos < (l->src).len) && (tokenize_lex_at(l) != 34)) { if ((tokenize_lex_at(l) == 92) && (l->pos + 1 < (l->src).len)) { tokenize_lex_advance(l); } tokenize_lex_advance(l); } if (l->pos < (l->src).len) { tokenize_lex_advance(l); } return ((qoz_Token){ .kind = qoz_make_TokenKind_TokString(), .text = qoz_string_slice(l->src, start_pos, l->pos), .line = start_line, .col = start_col });} if (tokenize_is_punct(c)) { tokenize_lex_advance(l); if (l->pos < (l->src).len) { int64_t n = tokenize_lex_at(l); bool two_char = ((((((((((c == 61) && (n == 61)) || ((c == 33) && (n == 61))) || ((c == 60) && (n == 61))) || ((c == 62) && (n == 61))) || ((c == 60) && (n == 60))) || ((c == 45) && (n == 62))) || ((c == 38) && (n == 38))) || ((c == 124) && (n == 124))) || ((c == 46) && (n == 46))) || ((c == 58) && (n == 58)); if (two_char) { tokenize_lex_advance(l); int64_t m = tokenize_lex_at(l); if (((c == 46) && (n == 46)) && ((m == 60) || (m == 61))) { tokenize_lex_advance(l); } } } return ((qoz_Token){ .kind = qoz_make_TokenKind_TokPunct(), .text = qoz_string_slice(l->src, start_pos, l->pos), .line = start_line, .col = start_col });} tokenize_lex_advance(l); return ((qoz_Token){ .kind = qoz_make_TokenKind_TokPunct(), .text = qoz_string_slice(l->src, start_pos, l->pos), .line = start_line, .col = start_col });
}

bool tokenize_is_stmt_ender(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_9 = t.kind; bool _qoz_mv_9 = false; switch (_qoz_ms_9->tag) { case qoz_TokenKind_TokIdent: { _qoz_mv_9 = (true);  break; } case qoz_TokenKind_TokInt: { _qoz_mv_9 = (true);  break; } case qoz_TokenKind_TokFloat: { _qoz_mv_9 = (true);  break; } case qoz_TokenKind_TokString: { _qoz_mv_9 = (true);  break; } case qoz_TokenKind_TokChar: { _qoz_mv_9 = (true);  break; } case qoz_TokenKind_TokKeyword: { bool _qoz_bv_14;
    {
        if (qoz_string_eq(t.text, QOZ_STR_LIT("true"))) { return true;} if (qoz_string_eq(t.text, QOZ_STR_LIT("false"))) { return true;} if (qoz_string_eq(t.text, QOZ_STR_LIT("nil"))) { return true;} if (qoz_string_eq(t.text, QOZ_STR_LIT("return"))) { return true;} _qoz_bv_14 = false;
    }
    _qoz_mv_9 = (_qoz_bv_14);  break; } case qoz_TokenKind_TokPunct: { bool _qoz_bv_15;
    {
        if (qoz_string_eq(t.text, QOZ_STR_LIT(")"))) { return true;} if (qoz_string_eq(t.text, QOZ_STR_LIT("]"))) { return true;} if (qoz_string_eq(t.text, QOZ_STR_LIT("}"))) { return true;} _qoz_bv_15 = false;
    }
    _qoz_mv_9 = (_qoz_bv_15);  break; } default: { _qoz_mv_9 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_9;
}

bool tokenize_is_line_continuation(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_10 = t.kind; bool _qoz_mv_10 = false; switch (_qoz_ms_10->tag) { case qoz_TokenKind_TokKeyword: { bool _qoz_bv_16;
    {
        if (qoz_string_eq(t.text, QOZ_STR_LIT("else"))) { return true;} if (qoz_string_eq(t.text, QOZ_STR_LIT("as"))) { return true;} if (qoz_string_eq(t.text, QOZ_STR_LIT("in"))) { return true;} _qoz_bv_16 = false;
    }
    _qoz_mv_10 = (_qoz_bv_16);  break; } case qoz_TokenKind_TokPunct: { bool _qoz_bv_17;
    {
        qoz_string s = t.text; if (qoz_string_eq(s, QOZ_STR_LIT("|"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("||"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("&"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("&&"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("+"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("-"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("*"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("/"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("%"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("^"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("<"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT(">"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("<="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT(">="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("<<"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("=="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("!="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("+="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("-="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("*="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("/="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("%="))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("->"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("."))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT(","))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT(":"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("?"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT(")"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("]"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("}"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT(".."))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("..<"))) { return true;} if (qoz_string_eq(s, QOZ_STR_LIT("..="))) { return true;} _qoz_bv_17 = false;
    }
    _qoz_mv_10 = (_qoz_bv_17);  break; } default: { _qoz_mv_10 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_10;
}

qoz_Vec__qoz_Token tokenize_run(qoz_string src) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_Token raw = qoz_vec_make__qoz_Token(); qoz_Lexer l = tokenize_make_lexer(src); qoz_Map__qoz_string__bool kw = tokenize_build_keywords(); bool running = true; while (running) { qoz_Token t = tokenize_lex_one(&l, &kw); qoz_TokenKind* _qoz_ms_11 = t.kind; switch (_qoz_ms_11->tag) { case qoz_TokenKind_TokEOF: { running = false;  break; } default: { qoz_vec_push__qoz_Token(&raw, t);  break; } } 0; } qoz_Vec__qoz_Token out = qoz_vec_make__qoz_Token(); int64_t paren_depth = 0; int64_t i = 0; while (i < (raw.len)) { qoz_Token cur = raw.data[i]; if ((i > 0) && (paren_depth == 0)) { qoz_Token prev = raw.data[i - 1]; if (((cur.line > prev.line) && tokenize_is_stmt_ender(prev)) && !tokenize_is_line_continuation(cur)) { qoz_vec_push__qoz_Token(&out, ((qoz_Token){ .kind = qoz_make_TokenKind_TokPunct(), .text = QOZ_STR_LIT(";"), .line = prev.line, .col = prev.col })); } } qoz_vec_push__qoz_Token(&out, cur); qoz_TokenKind* _qoz_ms_12 = cur.kind; switch (_qoz_ms_12->tag) { case qoz_TokenKind_TokPunct: { if (qoz_string_eq(cur.text, QOZ_STR_LIT("("))) { paren_depth = paren_depth + 1; }  else if (qoz_string_eq(cur.text, QOZ_STR_LIT("["))) { paren_depth = paren_depth + 1; }  else if (qoz_string_eq(cur.text, QOZ_STR_LIT(")"))) { paren_depth = paren_depth - 1; }  else if (qoz_string_eq(cur.text, QOZ_STR_LIT("]"))) { paren_depth = paren_depth - 1; } 0;  break; } default: { NULL;  break; } } 0; i = i + 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

qoz_Span ast_make_span(qoz_string file, int64_t line, int64_t col) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Span){ .file = file, .line = line, .col = col });
}

qoz_Span ast_span_of_expr(qoz_Expr* e) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_Expr* _qoz_ms_13 = e; qoz_Span _qoz_mv_13 = ((qoz_Span){0}); switch (_qoz_ms_13->tag) { case qoz_Expr_ENil: { qoz_Span sp = _qoz_ms_13->payload.ENil.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EInt: { qoz_Span sp = _qoz_ms_13->payload.EInt.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EFloat: { qoz_Span sp = _qoz_ms_13->payload.EFloat.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EString: { qoz_Span sp = _qoz_ms_13->payload.EString.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EChar: { qoz_Span sp = _qoz_ms_13->payload.EChar.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EBool: { qoz_Span sp = _qoz_ms_13->payload.EBool.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EIdent: { qoz_Span sp = _qoz_ms_13->payload.EIdent.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EPath: { qoz_Span sp = _qoz_ms_13->payload.EPath.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EReturn: { qoz_Span sp = _qoz_ms_13->payload.EReturn.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EDefer: { qoz_Span sp = _qoz_ms_13->payload.EDefer.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_ESizeOf: { qoz_Span sp = _qoz_ms_13->payload.ESizeOf.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_13->payload.EArrayLit.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_ETry: { qoz_Span sp = _qoz_ms_13->payload.ETry.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_ETuple: { qoz_Span sp = _qoz_ms_13->payload.ETuple.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_13->payload.EUnary.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_13->payload.EField.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EIndex: { qoz_Span sp = _qoz_ms_13->payload.EIndex.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_ECast: { qoz_Span sp = _qoz_ms_13->payload.ECast.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_ERecord: { qoz_Span sp = _qoz_ms_13->payload.ERecord.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EBlock: { qoz_Span sp = _qoz_ms_13->payload.EBlock.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EWhile: { qoz_Span sp = _qoz_ms_13->payload.EWhile.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_13->payload.EMatch.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EBinary: { qoz_Span sp = _qoz_ms_13->payload.EBinary.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EAssign: { qoz_Span sp = _qoz_ms_13->payload.EAssign.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_13->payload.ECall.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EIf: { qoz_Span sp = _qoz_ms_13->payload.EIf.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EClosure: { qoz_Span sp = _qoz_ms_13->payload.EClosure.f0; _qoz_mv_13 = (sp);  break; } case qoz_Expr_EFor: { qoz_Span sp = _qoz_ms_13->payload.EFor.f0; _qoz_mv_13 = (sp);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_13;
}

qoz_Parser parse_make_parser(qoz_Vec__qoz_Token tokens, qoz_string file) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Parser){ .tokens = tokens, .pos = 0, .file = file, .errors = qoz_vec_make__qoz_string(), .in_match_arm = 0 });
}

bool parse_at_eof(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return p->pos >= (p->tokens.len);
}

qoz_Token parse_peek(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    if (p->pos >= (p->tokens.len)) { return ((qoz_Token){ .kind = qoz_make_TokenKind_TokEOF(), .text = QOZ_STR_LIT(""), .line = 0, .col = 0 });} qoz_gc_shadow_set_top(_qoz_shadow_guard); return p->tokens.data[p->pos];
}

void parse_advance(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    p->pos = p->pos + 1; 
    return;
}

bool parse_is_kw(qoz_Token t, qoz_string kw) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_14 = t.kind; bool _qoz_mv_14 = false; switch (_qoz_ms_14->tag) { case qoz_TokenKind_TokKeyword: { _qoz_mv_14 = (qoz_string_eq(t.text, kw));  break; } default: { _qoz_mv_14 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_14;
}

bool parse_is_punct(qoz_Token t, qoz_string sym) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_15 = t.kind; bool _qoz_mv_15 = false; switch (_qoz_ms_15->tag) { case qoz_TokenKind_TokPunct: { _qoz_mv_15 = (qoz_string_eq(t.text, sym));  break; } default: { _qoz_mv_15 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_15;
}

bool parse_is_ident(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_16 = t.kind; bool _qoz_mv_16 = false; switch (_qoz_ms_16->tag) { case qoz_TokenKind_TokIdent: { _qoz_mv_16 = (true);  break; } default: { _qoz_mv_16 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_16;
}

bool parse_is_int_lit(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_17 = t.kind; bool _qoz_mv_17 = false; switch (_qoz_ms_17->tag) { case qoz_TokenKind_TokInt: { _qoz_mv_17 = (true);  break; } default: { _qoz_mv_17 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_17;
}

bool parse_is_float_lit(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_18 = t.kind; bool _qoz_mv_18 = false; switch (_qoz_ms_18->tag) { case qoz_TokenKind_TokFloat: { _qoz_mv_18 = (true);  break; } default: { _qoz_mv_18 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_18;
}

bool parse_is_str_lit(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_19 = t.kind; bool _qoz_mv_19 = false; switch (_qoz_ms_19->tag) { case qoz_TokenKind_TokString: { _qoz_mv_19 = (true);  break; } default: { _qoz_mv_19 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_19;
}

bool parse_is_char_lit(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_20 = t.kind; bool _qoz_mv_20 = false; switch (_qoz_ms_20->tag) { case qoz_TokenKind_TokChar: { _qoz_mv_20 = (true);  break; } default: { _qoz_mv_20 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_20;
}

qoz_Span parse_span_of(qoz_Parser* p, qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ast_make_span(p->file, t.line, t.col);
}

void parse_record_error(qoz_Parser* p, qoz_string msg) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_vec_push__qoz_string(&p->errors, msg); 
    return;
}

void parse_err_unexpected(qoz_Parser* p, qoz_string want, qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_string _qoz_bv_18;
    {
        qoz_Strbuf _qoz_sb_99_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_99_21); strings_sb_append(&_qoz_sb_99_21, QOZ_STR_LIT("expected ")); strings_sb_append(&_qoz_sb_99_21, want); strings_sb_append(&_qoz_sb_99_21, QOZ_STR_LIT(" at line ")); strings_sb_append_i64(&_qoz_sb_99_21, t.line); strings_sb_append(&_qoz_sb_99_21, QOZ_STR_LIT(" (got `")); strings_sb_append(&_qoz_sb_99_21, t.text); strings_sb_append(&_qoz_sb_99_21, QOZ_STR_LIT("`)")); _qoz_bv_18 = strings_sb_finish(&_qoz_sb_99_21);
    }
    parse_record_error(p, _qoz_bv_18); 
    return;
}

void parse_expect_punct(qoz_Parser* p, qoz_string sym) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token t = parse_peek(p); if (parse_is_punct(t, sym)) { parse_advance(p); }  else { qoz_string _qoz_bv_19;
    {
        qoz_Strbuf _qoz_sb_107_27 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_107_27); strings_sb_append(&_qoz_sb_107_27, QOZ_STR_LIT("`")); strings_sb_append(&_qoz_sb_107_27, sym); strings_sb_append(&_qoz_sb_107_27, QOZ_STR_LIT("`")); _qoz_bv_19 = strings_sb_finish(&_qoz_sb_107_27);
    }
    parse_err_unexpected(p, _qoz_bv_19, t); if (!parse_at_eof(p)) { parse_advance(p); } } 
    return;
}

qoz_string parse_expect_ident(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token t = parse_peek(p); if (parse_is_ident(t)) { parse_advance(p); return t.text;} parse_err_unexpected(p, QOZ_STR_LIT("identifier"), t); if (!parse_at_eof(p)) { parse_advance(p); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return QOZ_STR_LIT("<err>");
}

qoz_TypeExpr* parse_parse_type(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token t = parse_peek(p); qoz_Span span = parse_span_of(p, t); if (parse_is_punct(t, QOZ_STR_LIT("*"))) { parse_advance(p); qoz_TypeExpr* inner = parse_parse_type(p); qoz_gc_push_root(&inner); return qoz_make_TypeExpr_TEPtr(span, inner);} if (parse_is_punct(t, QOZ_STR_LIT("("))) { parse_advance(p); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { parse_advance(p); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("->"))) { parse_advance(p); qoz_TypeExpr* ret = parse_parse_type(p); qoz_gc_push_root(&ret); return qoz_make_TypeExpr_TEFn(span, qoz_vec_make__qoz_TypeExpr(), ret);} return qoz_make_TypeExpr_TEUnit(span);} qoz_Vec__qoz_TypeExpr elems = qoz_vec_make__qoz_TypeExpr(); qoz_vec_push__qoz_TypeExpr(&elems, parse_parse_type(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); qoz_vec_push__qoz_TypeExpr(&elems, parse_parse_type(p)); } parse_expect_punct(p, QOZ_STR_LIT(")")); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("->"))) { parse_advance(p); qoz_TypeExpr* ret = parse_parse_type(p); qoz_gc_push_root(&ret); return qoz_make_TypeExpr_TEFn(span, elems, ret);} if ((elems.len) == 1) { return elems.data[0];} return qoz_make_TypeExpr_TETuple(span, elems);} if (parse_is_ident(t)) { qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, t.text); parse_advance(p); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("."))) { parse_advance(p); qoz_Token next_tok = parse_peek(p); if (parse_is_ident(next_tok)) { qoz_vec_push__qoz_string(&path, next_tok.text); parse_advance(p); } } qoz_Vec__qoz_TypeExpr args = qoz_vec_make__qoz_TypeExpr(); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("<"))) { parse_advance(p); qoz_vec_push__qoz_TypeExpr(&args, parse_parse_type(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); qoz_vec_push__qoz_TypeExpr(&args, parse_parse_type(p)); } parse_expect_punct(p, QOZ_STR_LIT(">")); } return qoz_make_TypeExpr_TENamed(span, path, args);} parse_err_unexpected(p, QOZ_STR_LIT("type expression"), t); if (!parse_at_eof(p)) { parse_advance(p); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_TypeExpr_TEUnit(span);
}

qoz_Expr* parse_parse_expr(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_logical_or(p); qoz_gc_push_root(&lhs); qoz_Token t = parse_peek(p); qoz_Span span = parse_span_of(p, t); if (parse_is_punct(t, QOZ_STR_LIT("="))) { parse_advance(p); qoz_Expr* rhs = parse_parse_expr(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EAssign(span, qoz_make_AssignOp_AOpSet(), lhs, rhs);} if (parse_is_punct(t, QOZ_STR_LIT("+="))) { parse_advance(p); qoz_Expr* rhs = parse_parse_expr(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EAssign(span, qoz_make_AssignOp_AOpAddSet(), lhs, rhs);} if (parse_is_punct(t, QOZ_STR_LIT("-="))) { parse_advance(p); qoz_Expr* rhs = parse_parse_expr(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EAssign(span, qoz_make_AssignOp_AOpSubSet(), lhs, rhs);} if (parse_is_punct(t, QOZ_STR_LIT("..<"))) { parse_advance(p); qoz_Expr* rhs = parse_parse_logical_or(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EBinary(span, qoz_make_BinaryOp_BOpRange(), lhs, rhs);} if (parse_is_punct(t, QOZ_STR_LIT("..="))) { parse_advance(p); qoz_Expr* rhs = parse_parse_logical_or(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EBinary(span, qoz_make_BinaryOp_BOpRangeInclusive(), lhs, rhs);} qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_Expr* parse_parse_logical_or(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_logical_and(p); qoz_gc_push_root(&lhs); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("||"))) { qoz_Span span = parse_span_of(p, parse_peek(p)); parse_advance(p); qoz_Expr* rhs = parse_parse_logical_and(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, qoz_make_BinaryOp_BOpOr(), lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_Expr* parse_parse_logical_and(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_bitor(p); qoz_gc_push_root(&lhs); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("&&"))) { qoz_Span span = parse_span_of(p, parse_peek(p)); parse_advance(p); qoz_Expr* rhs = parse_parse_bitor(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, qoz_make_BinaryOp_BOpAnd(), lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_Expr* parse_parse_bitor(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_bitxor(p); qoz_gc_push_root(&lhs); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("|")) && (p->in_match_arm == 0)) { qoz_Span span = parse_span_of(p, parse_peek(p)); parse_advance(p); qoz_Expr* rhs = parse_parse_bitxor(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, qoz_make_BinaryOp_BOpBitOr(), lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_Expr* parse_parse_bitxor(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_bitand(p); qoz_gc_push_root(&lhs); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("^"))) { qoz_Span span = parse_span_of(p, parse_peek(p)); parse_advance(p); qoz_Expr* rhs = parse_parse_bitand(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, qoz_make_BinaryOp_BOpBitXor(), lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_Expr* parse_parse_bitand(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_compare(p); qoz_gc_push_root(&lhs); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("&")) && !parse_is_punct(parse_peek(p), QOZ_STR_LIT("&&"))) { qoz_Span span = parse_span_of(p, parse_peek(p)); parse_advance(p); qoz_Expr* rhs = parse_parse_compare(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, qoz_make_BinaryOp_BOpBitAnd(), lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_BinaryOp* parse_bin_op_for(qoz_string sym) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if (qoz_string_eq(sym, QOZ_STR_LIT("=="))) { return qoz_make_BinaryOp_BOpEq();} if (qoz_string_eq(sym, QOZ_STR_LIT("!="))) { return qoz_make_BinaryOp_BOpNe();} if (qoz_string_eq(sym, QOZ_STR_LIT("<="))) { return qoz_make_BinaryOp_BOpLe();} if (qoz_string_eq(sym, QOZ_STR_LIT(">="))) { return qoz_make_BinaryOp_BOpGe();} if (qoz_string_eq(sym, QOZ_STR_LIT("<"))) { return qoz_make_BinaryOp_BOpLt();} if (qoz_string_eq(sym, QOZ_STR_LIT(">"))) { return qoz_make_BinaryOp_BOpGt();} if (qoz_string_eq(sym, QOZ_STR_LIT("+"))) { return qoz_make_BinaryOp_BOpAdd();} if (qoz_string_eq(sym, QOZ_STR_LIT("-"))) { return qoz_make_BinaryOp_BOpSub();} if (qoz_string_eq(sym, QOZ_STR_LIT("*"))) { return qoz_make_BinaryOp_BOpMul();} if (qoz_string_eq(sym, QOZ_STR_LIT("/"))) { return qoz_make_BinaryOp_BOpDiv();} qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_BinaryOp_BOpMod();
}

bool parse_is_cmp_punct(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((((parse_is_punct(t, QOZ_STR_LIT("==")) || parse_is_punct(t, QOZ_STR_LIT("!="))) || parse_is_punct(t, QOZ_STR_LIT("<"))) || parse_is_punct(t, QOZ_STR_LIT(">"))) || parse_is_punct(t, QOZ_STR_LIT("<="))) || parse_is_punct(t, QOZ_STR_LIT(">="));
}

qoz_Expr* parse_parse_compare(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_shift(p); qoz_gc_push_root(&lhs); while (parse_is_cmp_punct(parse_peek(p))) { qoz_Token tok = parse_peek(p); qoz_BinaryOp* op = parse_bin_op_for(tok.text); qoz_gc_push_root(&op); qoz_Span span = parse_span_of(p, tok); parse_advance(p); qoz_Expr* rhs = parse_parse_shift(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, op, lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

bool parse_is_double_gt_adjacent(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    if (p->pos + 1 >= (p->tokens.len)) { return false;} qoz_Token a = p->tokens.data[p->pos]; qoz_Token b = p->tokens.data[p->pos + 1]; if (!parse_is_punct(a, QOZ_STR_LIT(">"))) { return false;} if (!parse_is_punct(b, QOZ_STR_LIT(">"))) { return false;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return (a.line == b.line) && (b.col == a.col + 1);
}

qoz_Expr* parse_parse_shift(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_addsub(p); qoz_gc_push_root(&lhs); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("<<")) || parse_is_double_gt_adjacent(p)) { qoz_Token tok = parse_peek(p); qoz_Span span = parse_span_of(p, tok); qoz_BinaryOp* op = ((parse_is_punct(tok, QOZ_STR_LIT("<<"))) ? qoz_make_BinaryOp_BOpShl() : qoz_make_BinaryOp_BOpShr()); qoz_gc_push_root(&op); if (parse_is_punct(tok, QOZ_STR_LIT("<<"))) { parse_advance(p); }  else { parse_advance(p); parse_advance(p); } qoz_Expr* rhs = parse_parse_addsub(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, op, lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_Expr* parse_parse_addsub(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_muldiv(p); qoz_gc_push_root(&lhs); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("+")) || parse_is_punct(parse_peek(p), QOZ_STR_LIT("-"))) { qoz_Token tok = parse_peek(p); qoz_BinaryOp* op = parse_bin_op_for(tok.text); qoz_gc_push_root(&op); qoz_Span span = parse_span_of(p, tok); parse_advance(p); qoz_Expr* rhs = parse_parse_muldiv(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, op, lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_Expr* parse_parse_muldiv(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* lhs = parse_parse_unary(p); qoz_gc_push_root(&lhs); while ((parse_is_punct(parse_peek(p), QOZ_STR_LIT("*")) || parse_is_punct(parse_peek(p), QOZ_STR_LIT("/"))) || parse_is_punct(parse_peek(p), QOZ_STR_LIT("%"))) { qoz_Token tok = parse_peek(p); qoz_BinaryOp* op = parse_bin_op_for(tok.text); qoz_gc_push_root(&op); qoz_Span span = parse_span_of(p, tok); parse_advance(p); qoz_Expr* rhs = parse_parse_unary(p); qoz_gc_push_root(&rhs); lhs = qoz_make_Expr_EBinary(span, op, lhs, rhs); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return lhs;
}

qoz_Expr* parse_parse_unary(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token tok = parse_peek(p); qoz_Span span = parse_span_of(p, tok); if (parse_is_punct(tok, QOZ_STR_LIT("-"))) { parse_advance(p); qoz_Expr* rhs = parse_parse_unary(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EUnary(span, qoz_make_UnaryOp_UOpNeg(), rhs);} if (parse_is_punct(tok, QOZ_STR_LIT("!"))) { parse_advance(p); qoz_Expr* rhs = parse_parse_unary(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EUnary(span, qoz_make_UnaryOp_UOpNot(), rhs);} if (parse_is_punct(tok, QOZ_STR_LIT("&"))) { parse_advance(p); qoz_Expr* rhs = parse_parse_unary(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EUnary(span, qoz_make_UnaryOp_UOpAddr(), rhs);} if (parse_is_punct(tok, QOZ_STR_LIT("*"))) { parse_advance(p); qoz_Expr* rhs = parse_parse_unary(p); qoz_gc_push_root(&rhs); return qoz_make_Expr_EUnary(span, qoz_make_UnaryOp_UOpDeref(), rhs);} qoz_gc_shadow_set_top(_qoz_shadow_guard); return parse_parse_postfix(p);
}

qoz_Token parse_peek_at(qoz_Parser* p, int64_t offset) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    if (p->pos + offset >= (p->tokens.len)) { return ((qoz_Token){ .kind = qoz_make_TokenKind_TokEOF(), .text = QOZ_STR_LIT(""), .line = 0, .col = 0 });} qoz_gc_shadow_set_top(_qoz_shadow_guard); return p->tokens.data[p->pos + offset];
}

bool parse_looks_like_call_type_args(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    if (!parse_is_punct(parse_peek_at(p, 0), QOZ_STR_LIT("<"))) { return false;} int64_t depth = 1; int64_t off = 1; while (depth > 0) { qoz_Token t = parse_peek_at(p, off); if (parse_is_token_eof(t)) { return false;} if (parse_is_punct(t, QOZ_STR_LIT("<"))) { depth = depth + 1; off = off + 1; }  else if (parse_is_punct(t, QOZ_STR_LIT(">"))) { depth = depth - 1; off = off + 1; }  else if ((((parse_is_punct(t, QOZ_STR_LIT(",")) || parse_is_punct(t, QOZ_STR_LIT("."))) || parse_is_punct(t, QOZ_STR_LIT("*"))) || parse_is_punct(t, QOZ_STR_LIT("&"))) || parse_is_punct(t, QOZ_STR_LIT("::"))) { off = off + 1; }  else if (parse_is_ident(t)) { off = off + 1; }  else { return false;} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return parse_is_punct(parse_peek_at(p, off), QOZ_STR_LIT("("));
}

bool parse_is_token_eof(qoz_Token t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_TokenKind* _qoz_ms_21 = t.kind; bool _qoz_mv_21 = false; switch (_qoz_ms_21->tag) { case qoz_TokenKind_TokEOF: { _qoz_mv_21 = (true);  break; } default: { _qoz_mv_21 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_21;
}

bool parse_looks_like_record_literal(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    if (!parse_is_punct(parse_peek_at(p, 0), QOZ_STR_LIT("{"))) { return false;} qoz_Token a = parse_peek_at(p, 1); qoz_Token b = parse_peek_at(p, 2); if (parse_is_punct(a, QOZ_STR_LIT(".."))) { return true;} if (parse_is_ident(a) && parse_is_punct(b, QOZ_STR_LIT(":"))) { return true;} if (parse_is_punct(a, QOZ_STR_LIT("}"))) { return true;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

qoz_Expr* parse_parse_record_literal(qoz_Parser* p, qoz_Span span, qoz_Vec__qoz_string type_path) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    parse_expect_punct(p, QOZ_STR_LIT("{")); qoz_Vec__qoz_RecordFieldLit fields = qoz_vec_make__qoz_RecordFieldLit(); while (!parse_is_punct(parse_peek(p), QOZ_STR_LIT("}")) && !parse_at_eof(p)) { int64_t start_pos = p->pos; qoz_string name = parse_expect_ident(p); parse_expect_punct(p, QOZ_STR_LIT(":")); qoz_Expr* value = parse_parse_expr(p); qoz_gc_push_root(&value); qoz_vec_push__qoz_RecordFieldLit(&fields, ((qoz_RecordFieldLit){ .name = name, .value = value })); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); } if (p->pos == start_pos) { parse_advance(p); } } parse_expect_punct(p, QOZ_STR_LIT("}")); qoz_TypeExpr* te = qoz_make_TypeExpr_TENamed(span, type_path, qoz_vec_make__qoz_TypeExpr()); qoz_gc_push_root(&te); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_ERecord(span, te, fields);
}

qoz_Expr* parse_parse_postfix(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Expr* e = parse_parse_atom(p); qoz_gc_push_root(&e); bool more = true; while (more) { qoz_Token tok = parse_peek(p); qoz_Span span = parse_span_of(p, tok); if (parse_is_punct(tok, QOZ_STR_LIT("{")) && parse_looks_like_record_literal(p)) { bool handled = false; qoz_Expr* _qoz_ms_22 = e; switch (_qoz_ms_22->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_22->payload.EIdent.f1; {
        qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, name); e = parse_parse_record_literal(p, span, path); handled = true; 
    }
    0;  break; } case qoz_Expr_EPath: { qoz_Vec__qoz_string segs = _qoz_ms_22->payload.EPath.f1; {
        e = parse_parse_record_literal(p, span, segs); handled = true; 
    }
    0;  break; } default: { NULL;  break; } } 0; if (!handled) { more = false; } }  else if (parse_is_punct(tok, QOZ_STR_LIT("<")) && parse_looks_like_call_type_args(p)) { parse_advance(p); qoz_Vec__qoz_TypeExpr type_args = qoz_vec_make__qoz_TypeExpr(); qoz_vec_push__qoz_TypeExpr(&type_args, parse_parse_type(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); qoz_vec_push__qoz_TypeExpr(&type_args, parse_parse_type(p)); } parse_expect_punct(p, QOZ_STR_LIT(">")); parse_expect_punct(p, QOZ_STR_LIT("(")); qoz_Vec__qoz_Expr args = qoz_vec_make__qoz_Expr(); if (!parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { qoz_vec_push__qoz_Expr(&args, parse_parse_expr(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); qoz_vec_push__qoz_Expr(&args, parse_parse_expr(p)); } } parse_expect_punct(p, QOZ_STR_LIT(")")); e = qoz_make_Expr_ECall(span, e, type_args, args); }  else if (parse_is_punct(tok, QOZ_STR_LIT("("))) { bool handled_sizeof = false; qoz_Expr* _qoz_ms_23 = e; switch (_qoz_ms_23->tag) { case qoz_Expr_EIdent: { qoz_string ename = _qoz_ms_23->payload.EIdent.f1; if (qoz_string_eq(ename, QOZ_STR_LIT("size_of"))) { parse_advance(p); qoz_TypeExpr* target = parse_parse_type(p); qoz_gc_push_root(&target); parse_expect_punct(p, QOZ_STR_LIT(")")); e = qoz_make_Expr_ESizeOf(span, target); handled_sizeof = true; } 0;  break; } default: { NULL;  break; } } 0; if (!handled_sizeof) { parse_advance(p); qoz_Vec__qoz_Expr args = qoz_vec_make__qoz_Expr(); if (!parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { qoz_vec_push__qoz_Expr(&args, parse_parse_expr(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); qoz_vec_push__qoz_Expr(&args, parse_parse_expr(p)); } } parse_expect_punct(p, QOZ_STR_LIT(")")); e = qoz_make_Expr_ECall(span, e, qoz_vec_make__qoz_TypeExpr(), args); } }  else if (parse_is_punct(tok, QOZ_STR_LIT("["))) { parse_advance(p); qoz_Expr* idx = parse_parse_expr(p); qoz_gc_push_root(&idx); parse_expect_punct(p, QOZ_STR_LIT("]")); e = qoz_make_Expr_EIndex(span, e, idx); }  else if (parse_is_punct(tok, QOZ_STR_LIT("."))) { parse_advance(p); qoz_Token name_tok = parse_peek(p); if (parse_is_ident(name_tok)) { parse_advance(p); e = qoz_make_Expr_EField(span, e, name_tok.text); }  else { parse_err_unexpected(p, QOZ_STR_LIT("field name after `.`"), name_tok); more = false; } }  else if (parse_is_kw(tok, QOZ_STR_LIT("as"))) { parse_advance(p); qoz_TypeExpr* target = parse_parse_type(p); qoz_gc_push_root(&target); e = qoz_make_Expr_ECast(span, e, target); }  else if (parse_is_punct(tok, QOZ_STR_LIT("?"))) { parse_advance(p); e = qoz_make_Expr_ETry(span, e); }  else { more = false; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return e;
}

bool parse_looks_like_closure(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    if (!parse_is_punct(parse_peek_at(p, 0), QOZ_STR_LIT("("))) { return false;} qoz_Token a = parse_peek_at(p, 1); qoz_Token b = parse_peek_at(p, 2); if (parse_is_punct(a, QOZ_STR_LIT(")"))) { if (parse_is_punct(b, QOZ_STR_LIT("->"))) { return true;} if (parse_is_punct(b, QOZ_STR_LIT(":"))) { return true;} return false;} if (parse_is_ident(a) && parse_is_punct(b, QOZ_STR_LIT(":"))) { return true;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

qoz_Expr* parse_parse_closure(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token open_tok = parse_peek(p); qoz_Span span = parse_span_of(p, open_tok); parse_advance(p); qoz_Vec__qoz_ClosureParam params = qoz_vec_make__qoz_ClosureParam(); if (!parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { bool more = true; while (more) { int64_t start_pos = p->pos; qoz_string name = parse_expect_ident(p); parse_expect_punct(p, QOZ_STR_LIT(":")); qoz_TypeExpr* ty = parse_parse_type(p); qoz_gc_push_root(&ty); qoz_vec_push__qoz_ClosureParam(&params, ((qoz_ClosureParam){ .name = name, .ty = ty })); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); }  else { more = false; } if (p->pos == start_pos) { parse_advance(p); more = false; } } } parse_expect_punct(p, QOZ_STR_LIT(")")); qoz_TypeExpr* ret_ty = qoz_make_TypeExpr_TEUnit(span); qoz_gc_push_root(&ret_ty); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(":"))) { parse_advance(p); ret_ty = parse_parse_type(p); } parse_expect_punct(p, QOZ_STR_LIT("->")); qoz_Expr* body = parse_parse_expr(p); qoz_gc_push_root(&body); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EClosure(span, params, ret_ty, body);
}

qoz_string parse_c_escape_bytes(qoz_string src) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_string out = QOZ_STR_LIT("\""); int64_t i = 0; int64_t n = (src).len; while (i < n) { int64_t b = qoz_string_byte_at(src, i); if (b == 92) { out = qoz_string_cat(out, QOZ_STR_LIT("\\\\")); }  else if (b == 34) { out = qoz_string_cat(out, QOZ_STR_LIT("\\\"")); }  else if (b == 10) { out = qoz_string_cat(out, QOZ_STR_LIT("\\n")); }  else if (b == 13) { out = qoz_string_cat(out, QOZ_STR_LIT("\\r")); }  else if (b == 9) { out = qoz_string_cat(out, QOZ_STR_LIT("\\t")); }  else if ((b < 32) || (b == 127)) { qoz_string _qoz_bv_20;
    {
        qoz_Strbuf _qoz_sb_625_36 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_625_36); strings_sb_append(&_qoz_sb_625_36, QOZ_STR_LIT("\\x")); strings_sb_append(&_qoz_sb_625_36, parse_hex2(b)); _qoz_bv_20 = strings_sb_finish(&_qoz_sb_625_36);
    }
    out = qoz_string_cat(out, _qoz_bv_20); }  else { out = qoz_string_cat(out, qoz_string_slice(src, i, i + 1)); } i = i + 1; } out = qoz_string_cat(out, QOZ_STR_LIT("\"")); qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

qoz_string parse_hex_digit(int64_t n) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if (n < 10) { qoz_string _qoz_bv_21;
    {
        qoz_Strbuf _qoz_sb_636_24 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_636_24); strings_sb_append_i64(&_qoz_sb_636_24, n); _qoz_bv_21 = strings_sb_finish(&_qoz_sb_636_24);
    }
    return _qoz_bv_21;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_slice(QOZ_STR_LIT("abcdef"), n - 10, n - 9);
}

qoz_string parse_hex2(int64_t b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    int64_t hi = b / 16 % 16; int64_t lo = b % 16; qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_cat(parse_hex_digit(hi), parse_hex_digit(lo));
}

bool parse_is_interp_start_byte(int64_t b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if (b == 95) { return true;} if ((b >= 65) && (b <= 90)) { return true;} if ((b >= 97) && (b <= 122)) { return true;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

bool parse_is_interp_cont_byte(int64_t b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if ((b == 95) || (b == 46)) { return true;} if ((b >= 65) && (b <= 90)) { return true;} if ((b >= 97) && (b <= 122)) { return true;} if ((b >= 48) && (b <= 57)) { return true;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

int64_t parse_scan_interp_end(qoz_string body, int64_t start) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    int64_t n = (body).len; if (start >= n) { return -1;} if (!parse_is_interp_start_byte(qoz_string_byte_at(body, start))) { return -1;} int64_t depth = 1; int64_t i = start; bool in_str = false; while (i < n) { int64_t c = qoz_string_byte_at(body, i); if (in_str) { if ((c == 92) && (i + 1 < n)) { i = i + 2; }  else { if (c == 34) { in_str = false; } i = i + 1; } }  else { if (c == 34) { in_str = true; }  else if (c == 123) { depth = depth + 1; }  else if (c == 125) { depth = depth - 1; if (depth == 0) { return i;} } i = i + 1; } } return -1;
}

qoz_Expr* parse_build_interp_expr(qoz_string text, qoz_Span span) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_Token tokens = tokenize_run(text); qoz_Parser sub = parse_make_parser(tokens, span.file); qoz_gc_shadow_set_top(_qoz_shadow_guard); return parse_parse_expr(&sub);
}

bool parse_interp_text_valid(qoz_string text) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_Token tokens = tokenize_run(text); qoz_Parser sub = parse_make_parser(tokens, QOZ_STR_LIT("")); (void)(parse_parse_expr(&sub)); if ((sub.errors.len) > 0) { return false;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return sub.pos == (sub.tokens.len);
}

qoz_Expr* parse_build_string_lit_or_interp(qoz_string raw, qoz_Span span) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    int64_t n = (raw).len; if (n < 2) { return qoz_make_Expr_EString(span, raw);} qoz_string body = qoz_string_slice(raw, 1, n - 1); int64_t bn = (body).len; bool needs_rewrite = false; int64_t i = 0; while (i < bn) { int64_t b = qoz_string_byte_at(body, i); if ((b == 92) && (i + 1 < bn)) { i = i + 2; }  else if (((b == 123) && (i + 1 < bn)) && (qoz_string_byte_at(body, i + 1) == 123)) { needs_rewrite = true; i = bn; }  else if (((b == 125) && (i + 1 < bn)) && (qoz_string_byte_at(body, i + 1) == 125)) { needs_rewrite = true; i = bn; }  else if ((b == 123) && (i + 1 < bn)) { int64_t end = parse_scan_interp_end(body, i + 1); if ((end >= 0) && parse_interp_text_valid(qoz_string_slice(body, i + 1, end))) { needs_rewrite = true; i = bn; }  else { i = i + 1; } }  else { i = i + 1; } } if (!needs_rewrite) { return qoz_make_Expr_EString(span, raw);} qoz_string template = QOZ_STR_LIT("\""); qoz_Vec__qoz_Expr args = qoz_vec_make__qoz_Expr(); i = 0; while (i < bn) { int64_t b = qoz_string_byte_at(body, i); if ((b == 92) && (i + 1 < bn)) { template = qoz_string_cat(template, qoz_string_slice(body, i, i + 2)); i = i + 2; }  else if (((b == 123) && (i + 1 < bn)) && (qoz_string_byte_at(body, i + 1) == 123)) { template = qoz_string_cat(template, QOZ_STR_LIT("{")); i = i + 2; }  else if (((b == 125) && (i + 1 < bn)) && (qoz_string_byte_at(body, i + 1) == 125)) { template = qoz_string_cat(template, QOZ_STR_LIT("}")); i = i + 2; }  else if ((b == 123) && (i + 1 < bn)) { int64_t end = parse_scan_interp_end(body, i + 1); qoz_string ident_text = ((end >= 0) ? qoz_string_slice(body, i + 1, end) : QOZ_STR_LIT("")); if ((end >= 0) && parse_interp_text_valid(ident_text)) { template = qoz_string_cat(template, QOZ_STR_LIT("{}")); qoz_vec_push__qoz_Expr(&args, parse_build_interp_expr(ident_text, span)); i = end + 1; }  else { template = qoz_string_cat(template, qoz_string_slice(body, i, i + 1)); i = i + 1; } }  else { template = qoz_string_cat(template, qoz_string_slice(body, i, i + 1)); i = i + 1; } } template = qoz_string_cat(template, QOZ_STR_LIT("\"")); if ((args.len) == 0) { return qoz_make_Expr_EString(span, template);} qoz_gc_shadow_set_top(_qoz_shadow_guard); return parse_interp_block(template, args, span);
}

qoz_string parse_interp_var_name(qoz_Span span) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_string _qoz_bv_22;
    {
        qoz_Strbuf _qoz_sb_791_41 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_791_41); strings_sb_append_i64(&_qoz_sb_791_41, span.line); _qoz_bv_22 = strings_sb_finish(&_qoz_sb_791_41);
    }
    qoz_string _qoz_bv_23;
    {
        qoz_Strbuf _qoz_sb_791_73 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_791_73); strings_sb_append_i64(&_qoz_sb_791_73, span.col); _qoz_bv_23 = strings_sb_finish(&_qoz_sb_791_73);
    }
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_cat(QOZ_STR_LIT("_qoz_sb_"), qoz_string_cat(_qoz_bv_22, qoz_string_cat(QOZ_STR_LIT("_"), _qoz_bv_23)));
}

qoz_Vec__qoz_string parse_split_template_chunks(qoz_string template) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string chunks = qoz_vec_make__qoz_string(); int64_t n = (template).len; if (n < 2) { return chunks;} qoz_string body = qoz_string_slice(template, 1, n - 1); int64_t bn = (body).len; int64_t i = 0; int64_t start = 0; while (i + 1 < bn) { if ((qoz_string_byte_at(body, i) == 123) && (qoz_string_byte_at(body, i + 1) == 125)) { qoz_vec_push__qoz_string(&chunks, qoz_string_slice(body, start, i)); i = i + 2; start = i; }  else { i = i + 1; } } qoz_vec_push__qoz_string(&chunks, qoz_string_slice(body, start, bn)); qoz_gc_shadow_set_top(_qoz_shadow_guard); return chunks;
}

qoz_Expr* parse_interp_block(qoz_string template, qoz_Vec__qoz_Expr args, qoz_Span span) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_string sb_name = parse_interp_var_name(span); qoz_Vec__qoz_string strbuf_path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&strbuf_path, QOZ_STR_LIT("Strbuf")); qoz_Vec__qoz_TypeExpr no_args = qoz_vec_make__qoz_TypeExpr(); qoz_TypeExpr* strbuf_te = qoz_make_TypeExpr_TENamed(span, strbuf_path, no_args); qoz_gc_push_root(&strbuf_te); qoz_Vec__qoz_string u8_path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&u8_path, QOZ_STR_LIT("u8")); qoz_TypeExpr* u8_te = qoz_make_TypeExpr_TENamed(span, u8_path, qoz_vec_make__qoz_TypeExpr()); qoz_gc_push_root(&u8_te); qoz_Expr* nil_as_u8_ptr = qoz_make_Expr_ECast(span, qoz_make_Expr_ENil(span), qoz_make_TypeExpr_TEPtr(span, u8_te)); qoz_gc_push_root(&nil_as_u8_ptr); qoz_Vec__qoz_RecordFieldLit fields = qoz_vec_make__qoz_RecordFieldLit(); qoz_vec_push__qoz_RecordFieldLit(&fields, ((qoz_RecordFieldLit){ .name = QOZ_STR_LIT("buf"), .value = nil_as_u8_ptr })); qoz_vec_push__qoz_RecordFieldLit(&fields, ((qoz_RecordFieldLit){ .name = QOZ_STR_LIT("len"), .value = qoz_make_Expr_EInt(span, QOZ_STR_LIT("0")) })); qoz_vec_push__qoz_RecordFieldLit(&fields, ((qoz_RecordFieldLit){ .name = QOZ_STR_LIT("cap"), .value = qoz_make_Expr_EInt(span, QOZ_STR_LIT("0")) })); qoz_Expr* init_value = qoz_make_Expr_ERecord(span, strbuf_te, fields); qoz_gc_push_root(&init_value); qoz_Vec__qoz_Stmt stmts = qoz_vec_make__qoz_Stmt(); qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SVar(span, sb_name, strbuf_te, init_value)); qoz_Expr* sb_ref = qoz_make_Expr_EUnary(span, qoz_make_UnaryOp_UOpAddr(), qoz_make_Expr_EIdent(span, sb_name)); qoz_gc_push_root(&sb_ref); qoz_Expr* init_call = parse_strings_call(QOZ_STR_LIT("sb_init"), span, parse_append_args1(sb_ref)); qoz_gc_push_root(&init_call); qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SExpr(span, init_call)); qoz_Vec__qoz_string chunks = parse_split_template_chunks(template); int64_t idx = 0; while (idx < (args.len)) { qoz_string chunk = chunks.data[idx]; if ((chunk).len > 0) { qoz_Expr* lit_arg = qoz_make_Expr_EString(span, qoz_string_cat(QOZ_STR_LIT("\""), qoz_string_cat(chunk, QOZ_STR_LIT("\"")))); qoz_gc_push_root(&lit_arg); qoz_Expr* append_lit = parse_strings_call(QOZ_STR_LIT("sb_append"), span, parse_append_args2(sb_ref, lit_arg)); qoz_gc_push_root(&append_lit); qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SExpr(span, append_lit)); } qoz_Expr* append_arg = parse_strings_call(QOZ_STR_LIT("sb_append"), span, parse_append_args2(sb_ref, args.data[idx])); qoz_gc_push_root(&append_arg); qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SExpr(span, append_arg)); idx = idx + 1; } qoz_string tail_chunk = chunks.data[(args.len)]; if ((tail_chunk).len > 0) { qoz_Expr* lit_arg = qoz_make_Expr_EString(span, qoz_string_cat(QOZ_STR_LIT("\""), qoz_string_cat(tail_chunk, QOZ_STR_LIT("\"")))); qoz_gc_push_root(&lit_arg); qoz_Expr* append_lit = parse_strings_call(QOZ_STR_LIT("sb_append"), span, parse_append_args2(sb_ref, lit_arg)); qoz_gc_push_root(&append_lit); qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SExpr(span, append_lit)); } qoz_Expr* finish_call = parse_strings_call(QOZ_STR_LIT("sb_finish"), span, parse_append_args1(sb_ref)); qoz_gc_push_root(&finish_call); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EBlock(span, stmts, finish_call);
}

qoz_Vec__qoz_Expr parse_append_args1(qoz_Expr* a) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&a);
    qoz_Vec__qoz_Expr v = qoz_vec_make__qoz_Expr(); qoz_vec_push__qoz_Expr(&v, a); qoz_gc_shadow_set_top(_qoz_shadow_guard); return v;
}

qoz_Vec__qoz_Expr parse_append_args2(qoz_Expr* a, qoz_Expr* b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&a);
    qoz_gc_push_root(&b);
    qoz_Vec__qoz_Expr v = qoz_vec_make__qoz_Expr(); qoz_vec_push__qoz_Expr(&v, a); qoz_vec_push__qoz_Expr(&v, b); qoz_gc_shadow_set_top(_qoz_shadow_guard); return v;
}

qoz_Expr* parse_strings_call(qoz_string method, qoz_Span span, qoz_Vec__qoz_Expr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Expr* callee = qoz_make_Expr_EField(span, qoz_make_Expr_EIdent(span, QOZ_STR_LIT("strings")), method); qoz_gc_push_root(&callee); qoz_Vec__qoz_TypeExpr type_args = qoz_vec_make__qoz_TypeExpr(); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_ECall(span, callee, type_args, args);
}

qoz_Expr* parse_parse_atom(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token t = parse_peek(p); qoz_Span span = parse_span_of(p, t); if (parse_is_punct(t, QOZ_STR_LIT("#"))) { parse_advance(p); qoz_Token name_tok = parse_peek(p); if (!parse_is_ident(name_tok)) { qoz_string _qoz_bv_24;
    {
        qoz_Strbuf _qoz_sb_896_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_896_29); strings_sb_append(&_qoz_sb_896_29, QOZ_STR_LIT("expected directive name after `#`, got `")); strings_sb_append(&_qoz_sb_896_29, name_tok.text); strings_sb_append(&_qoz_sb_896_29, QOZ_STR_LIT("`")); _qoz_bv_24 = strings_sb_finish(&_qoz_sb_896_29);
    }
    parse_record_error(p, _qoz_bv_24); return qoz_make_Expr_ENil(span);} parse_advance(p); parse_expect_punct(p, QOZ_STR_LIT("(")); qoz_Token arg_tok = parse_peek(p); if (!parse_is_str_lit(arg_tok)) { qoz_string _qoz_bv_25;
    {
        qoz_Strbuf _qoz_sb_903_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_903_29); strings_sb_append(&_qoz_sb_903_29, QOZ_STR_LIT("#")); strings_sb_append(&_qoz_sb_903_29, name_tok.text); strings_sb_append(&_qoz_sb_903_29, QOZ_STR_LIT(" expects a string literal path")); _qoz_bv_25 = strings_sb_finish(&_qoz_sb_903_29);
    }
    parse_record_error(p, _qoz_bv_25); return qoz_make_Expr_ENil(span);} parse_advance(p); parse_expect_punct(p, QOZ_STR_LIT(")")); if (!qoz_string_eq(name_tok.text, QOZ_STR_LIT("load_string"))) { qoz_string _qoz_bv_26;
    {
        qoz_Strbuf _qoz_sb_909_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_909_29); strings_sb_append(&_qoz_sb_909_29, QOZ_STR_LIT("unknown directive `#")); strings_sb_append(&_qoz_sb_909_29, name_tok.text); strings_sb_append(&_qoz_sb_909_29, QOZ_STR_LIT("` in expression position; supported: #load_string")); _qoz_bv_26 = strings_sb_finish(&_qoz_sb_909_29);
    }
    parse_record_error(p, _qoz_bv_26); return qoz_make_Expr_ENil(span);} qoz_string raw_path = arg_tok.text; int64_t pn = (raw_path).len; if (((pn >= 2) && (qoz_string_byte_at(raw_path, 0) == 34)) && (qoz_string_byte_at(raw_path, pn - 1) == 34)) { raw_path = qoz_string_slice(raw_path, 1, pn - 1); } qoz_string bytes = qoz_fs_read_file(raw_path); if ((bytes).len < 0) { qoz_string _qoz_bv_27;
    {
        qoz_Strbuf _qoz_sb_920_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_920_29); strings_sb_append(&_qoz_sb_920_29, QOZ_STR_LIT("#load_string: cannot read `")); strings_sb_append(&_qoz_sb_920_29, raw_path); strings_sb_append(&_qoz_sb_920_29, QOZ_STR_LIT("`")); _qoz_bv_27 = strings_sb_finish(&_qoz_sb_920_29);
    }
    parse_record_error(p, _qoz_bv_27); return qoz_make_Expr_ENil(span);} return qoz_make_Expr_EString(span, parse_c_escape_bytes(bytes));} if (parse_is_int_lit(t)) { parse_advance(p); return qoz_make_Expr_EInt(span, t.text);} if (parse_is_float_lit(t)) { parse_advance(p); return qoz_make_Expr_EFloat(span, t.text);} if (parse_is_str_lit(t)) { parse_advance(p); return parse_build_string_lit_or_interp(t.text, span);} if (parse_is_char_lit(t)) { parse_advance(p); return qoz_make_Expr_EChar(span, t.text);} if (parse_is_kw(t, QOZ_STR_LIT("true"))) { parse_advance(p); return qoz_make_Expr_EBool(span, true);} if (parse_is_kw(t, QOZ_STR_LIT("false"))) { parse_advance(p); return qoz_make_Expr_EBool(span, false);} if (parse_is_kw(t, QOZ_STR_LIT("nil"))) { parse_advance(p); return qoz_make_Expr_ENil(span);} if (parse_is_ident(t)) { parse_advance(p); return qoz_make_Expr_EIdent(span, t.text);} if (parse_is_punct(t, QOZ_STR_LIT("("))) { if (parse_looks_like_closure(p)) { return parse_parse_closure(p);} parse_advance(p); int64_t saved_match = p->in_match_arm; p->in_match_arm = 0; if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { parse_advance(p); p->in_match_arm = saved_match; return qoz_make_Expr_ETuple(span, qoz_vec_make__qoz_Expr());} qoz_Expr* first = parse_parse_expr(p); qoz_gc_push_root(&first); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { qoz_Vec__qoz_Expr elems = qoz_vec_make__qoz_Expr(); qoz_vec_push__qoz_Expr(&elems, first); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { parse_advance(p); p->in_match_arm = saved_match; return qoz_make_Expr_ETuple(span, elems);} qoz_vec_push__qoz_Expr(&elems, parse_parse_expr(p)); } parse_expect_punct(p, QOZ_STR_LIT(")")); p->in_match_arm = saved_match; return qoz_make_Expr_ETuple(span, elems);} p->in_match_arm = saved_match; parse_expect_punct(p, QOZ_STR_LIT(")")); return first;} if (parse_is_punct(t, QOZ_STR_LIT("["))) { parse_advance(p); qoz_Vec__qoz_Expr elems = qoz_vec_make__qoz_Expr(); if (!parse_is_punct(parse_peek(p), QOZ_STR_LIT("]"))) { qoz_vec_push__qoz_Expr(&elems, parse_parse_expr(p)); bool done = false; while (!done && parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("]"))) { done = true; }  else { qoz_vec_push__qoz_Expr(&elems, parse_parse_expr(p)); } } } parse_expect_punct(p, QOZ_STR_LIT("]")); return qoz_make_Expr_EArrayLit(span, elems);} if (parse_is_punct(t, QOZ_STR_LIT("{"))) { return parse_parse_block(p);} if (parse_is_kw(t, QOZ_STR_LIT("if"))) { return parse_parse_if(p);} if (parse_is_kw(t, QOZ_STR_LIT("while"))) { return parse_parse_while(p);} if (parse_is_kw(t, QOZ_STR_LIT("for"))) { return parse_parse_for(p);} if (parse_is_kw(t, QOZ_STR_LIT("match"))) { return parse_parse_match(p);} if (parse_is_kw(t, QOZ_STR_LIT("return"))) { return parse_parse_return(p);} if (parse_is_kw(t, QOZ_STR_LIT("defer"))) { return parse_parse_defer(p);} parse_err_unexpected(p, QOZ_STR_LIT("expression"), t); parse_advance(p); return qoz_make_Expr_ENil(span);
}

qoz_Expr* parse_parse_if(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); if (parse_is_kw(parse_peek(p), QOZ_STR_LIT("let"))) { parse_advance(p); qoz_Pattern* pat = parse_parse_pattern(p); qoz_gc_push_root(&pat); parse_expect_punct(p, QOZ_STR_LIT("=")); qoz_Expr* scrut = parse_parse_expr(p); qoz_gc_push_root(&scrut); qoz_Expr* then_b = parse_parse_block(p); qoz_gc_push_root(&then_b); qoz_Expr* else_b = qoz_make_Expr_EBlock(span, qoz_vec_make__qoz_Stmt(), qoz_make_Expr_ENil(span)); qoz_gc_push_root(&else_b); if (parse_is_kw(parse_peek(p), QOZ_STR_LIT("else"))) { parse_advance(p); if (parse_is_kw(parse_peek(p), QOZ_STR_LIT("if"))) { else_b = parse_parse_if(p); }  else { else_b = parse_parse_block(p); } } qoz_Vec__qoz_MatchArm arms = qoz_vec_make__qoz_MatchArm(); qoz_vec_push__qoz_MatchArm(&arms, ((qoz_MatchArm){ .pat = pat, .body = then_b, .has_guard = false, .guard = qoz_make_Expr_ENil(span) })); qoz_vec_push__qoz_MatchArm(&arms, ((qoz_MatchArm){ .pat = qoz_make_Pattern_PatWild(span), .body = else_b, .has_guard = false, .guard = qoz_make_Expr_ENil(span) })); return qoz_make_Expr_EMatch(span, scrut, arms);} qoz_Expr* cond = parse_parse_expr(p); qoz_gc_push_root(&cond); qoz_Expr* then_b = parse_parse_block(p); qoz_gc_push_root(&then_b); qoz_Expr* else_b = qoz_make_Expr_ENil(span); qoz_gc_push_root(&else_b); if (parse_is_kw(parse_peek(p), QOZ_STR_LIT("else"))) { parse_advance(p); if (parse_is_kw(parse_peek(p), QOZ_STR_LIT("if"))) { else_b = parse_parse_if(p); }  else { else_b = parse_parse_block(p); } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EIf(span, cond, then_b, else_b);
}

qoz_Expr* parse_parse_while(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); qoz_Expr* cond = parse_parse_expr(p); qoz_gc_push_root(&cond); qoz_Expr* body = parse_parse_block(p); qoz_gc_push_root(&body); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EWhile(span, cond, body);
}

qoz_Expr* parse_parse_for(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); qoz_string binding = parse_expect_ident(p); qoz_string binding2 = QOZ_STR_LIT(""); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); binding2 = parse_expect_ident(p); } if (!parse_is_kw(parse_peek(p), QOZ_STR_LIT("in"))) { parse_err_unexpected(p, QOZ_STR_LIT("`in`"), parse_peek(p)); }  else { parse_advance(p); } qoz_Expr* iter = parse_parse_expr(p); qoz_gc_push_root(&iter); qoz_Expr* body = parse_parse_block(p); qoz_gc_push_root(&body); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EFor(span, binding, binding2, iter, body);
}

qoz_Expr* parse_parse_match(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); qoz_Expr* scrutinee = parse_parse_expr(p); qoz_gc_push_root(&scrutinee); parse_expect_punct(p, QOZ_STR_LIT("{")); qoz_Vec__qoz_MatchArm arms = qoz_vec_make__qoz_MatchArm(); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(";"))) { parse_advance(p); } while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("|"))) { parse_advance(p); qoz_Vec__qoz_Pattern pats = qoz_vec_make__qoz_Pattern(); qoz_vec_push__qoz_Pattern(&pats, parse_parse_pattern(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("|"))) { parse_advance(p); qoz_vec_push__qoz_Pattern(&pats, parse_parse_pattern(p)); } bool has_guard = false; qoz_Expr* guard = qoz_make_Expr_ENil(span); qoz_gc_push_root(&guard); if (parse_is_kw(parse_peek(p), QOZ_STR_LIT("if"))) { parse_advance(p); has_guard = true; guard = parse_parse_expr(p); } if (!parse_is_punct(parse_peek(p), QOZ_STR_LIT("->"))) { parse_err_unexpected(p, QOZ_STR_LIT("`->` after pattern"), parse_peek(p)); }  else { parse_advance(p); } p->in_match_arm = p->in_match_arm + 1; qoz_Expr* body = parse_parse_expr(p); qoz_gc_push_root(&body); p->in_match_arm = p->in_match_arm - 1; { qoz_Vec__qoz_Pattern __col = pats; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Pattern* pat = __col.data[__i]; (void)pat; qoz_vec_push__qoz_MatchArm(&arms, ((qoz_MatchArm){ .pat = pat, .body = body, .has_guard = has_guard, .guard = guard })); } }while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(";"))) { parse_advance(p); } } parse_expect_punct(p, QOZ_STR_LIT("}")); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EMatch(span, scrutinee, arms);
}

qoz_Expr* parse_parse_return(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); qoz_Expr* value = qoz_make_Expr_ENil(span); qoz_gc_push_root(&value); if ((!parse_is_punct(parse_peek(p), QOZ_STR_LIT("}")) && !parse_is_punct(parse_peek(p), QOZ_STR_LIT(";"))) && !parse_at_eof(p)) { value = parse_parse_expr(p); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EReturn(span, value);
}

qoz_Expr* parse_parse_defer(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); qoz_Expr* body = parse_parse_expr(p); qoz_gc_push_root(&body); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EDefer(span, body);
}

qoz_Pattern* parse_parse_pattern(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token t = parse_peek(p); qoz_Span span = parse_span_of(p, t); if (parse_is_int_lit(t)) { parse_advance(p); return qoz_make_Pattern_PatLitInt(span, t.text);} if (parse_is_str_lit(t)) { parse_advance(p); return qoz_make_Pattern_PatLitString(span, t.text);} if (parse_is_kw(t, QOZ_STR_LIT("true"))) { parse_advance(p); return qoz_make_Pattern_PatLitBool(span, true);} if (parse_is_kw(t, QOZ_STR_LIT("false"))) { parse_advance(p); return qoz_make_Pattern_PatLitBool(span, false);} if (parse_is_ident(t)) { if (qoz_string_eq(t.text, QOZ_STR_LIT("_"))) { parse_advance(p); return qoz_make_Pattern_PatWild(span);} qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, t.text); parse_advance(p); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("."))) { parse_advance(p); qoz_Token part = parse_peek(p); if (parse_is_ident(part)) { qoz_vec_push__qoz_string(&path, part.text); parse_advance(p); } } qoz_Vec__qoz_Pattern pos = qoz_vec_make__qoz_Pattern(); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("("))) { parse_advance(p); if (!parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { qoz_vec_push__qoz_Pattern(&pos, parse_parse_pattern(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); qoz_vec_push__qoz_Pattern(&pos, parse_parse_pattern(p)); } } parse_expect_punct(p, QOZ_STR_LIT(")")); } if (((path.len) == 1) && ((pos.len) == 0)) { return qoz_make_Pattern_PatBind(span, path.data[0]);} return qoz_make_Pattern_PatVariant(span, path, pos);} parse_err_unexpected(p, QOZ_STR_LIT("pattern"), t); parse_advance(p); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Pattern_PatWild(span);
}

qoz_Expr* parse_parse_block(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token open = parse_peek(p); qoz_Span span = parse_span_of(p, open); parse_expect_punct(p, QOZ_STR_LIT("{")); qoz_Vec__qoz_Stmt stmts = qoz_vec_make__qoz_Stmt(); qoz_Expr* tail = qoz_make_Expr_ENil(span); qoz_gc_push_root(&tail); bool have_tail = false; bool done = false; while ((!done && !parse_is_punct(parse_peek(p), QOZ_STR_LIT("}"))) && !parse_at_eof(p)) { int64_t block_start_pos = p->pos; while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(";"))) { parse_advance(p); } if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("}"))) { done = true; }  else { qoz_Token t = parse_peek(p); qoz_Span stmt_span = parse_span_of(p, t); if (parse_is_kw(t, QOZ_STR_LIT("let"))) { parse_advance(p); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("("))) { parse_advance(p); qoz_Vec__qoz_string binds = qoz_vec_make__qoz_string(); if (!parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { qoz_vec_push__qoz_string(&binds, parse_expect_ident(p)); bool keep = true; while (keep && parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { keep = false; }  else { qoz_vec_push__qoz_string(&binds, parse_expect_ident(p)); } } } parse_expect_punct(p, QOZ_STR_LIT(")")); parse_expect_punct(p, QOZ_STR_LIT("=")); qoz_Expr* value = parse_parse_expr(p); qoz_gc_push_root(&value); int64_t n = (stmts.len); qoz_string _qoz_bv_28;
    {
        qoz_Strbuf _qoz_sb_1254_27 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1254_27); strings_sb_append(&_qoz_sb_1254_27, QOZ_STR_LIT("_qoz_tup_")); strings_sb_append_i64(&_qoz_sb_1254_27, n); _qoz_bv_28 = strings_sb_finish(&_qoz_sb_1254_27);
    }
    qoz_string tmp = _qoz_bv_28; qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SLet(stmt_span, tmp, qoz_make_TypeExpr_TEUnit(stmt_span), value)); int64_t bi = 0; { qoz_Vec__qoz_string __col = binds; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string bname = __col.data[__i]; (void)bname; if (!qoz_string_eq(bname, QOZ_STR_LIT("_"))) { qoz_string _qoz_bv_29;
    {
        qoz_Strbuf _qoz_sb_1259_37 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1259_37); strings_sb_append(&_qoz_sb_1259_37, QOZ_STR_LIT("_")); strings_sb_append_i64(&_qoz_sb_1259_37, bi); _qoz_bv_29 = strings_sb_finish(&_qoz_sb_1259_37);
    }
    qoz_string field = _qoz_bv_29; qoz_Expr* fld_expr = qoz_make_Expr_EField(stmt_span, qoz_make_Expr_EIdent(stmt_span, tmp), field); qoz_gc_push_root(&fld_expr); qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SLet(stmt_span, bname, qoz_make_TypeExpr_TEUnit(stmt_span), fld_expr)); } bi = bi + 1; } }}  else { qoz_string name = parse_expect_ident(p); qoz_TypeExpr* ann = qoz_make_TypeExpr_TEUnit(stmt_span); qoz_gc_push_root(&ann); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(":"))) { parse_advance(p); ann = parse_parse_type(p); } parse_expect_punct(p, QOZ_STR_LIT("=")); qoz_Expr* value = parse_parse_expr(p); qoz_gc_push_root(&value); qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SLet(stmt_span, name, ann, value)); } }  else if (parse_is_kw(t, QOZ_STR_LIT("var"))) { parse_advance(p); qoz_string name = parse_expect_ident(p); qoz_TypeExpr* ann = qoz_make_TypeExpr_TEUnit(stmt_span); qoz_gc_push_root(&ann); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(":"))) { parse_advance(p); ann = parse_parse_type(p); } parse_expect_punct(p, QOZ_STR_LIT("=")); qoz_Expr* value = parse_parse_expr(p); qoz_gc_push_root(&value); qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SVar(stmt_span, name, ann, value)); }  else { qoz_Expr* e = parse_parse_expr(p); qoz_gc_push_root(&e); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(";"))) { parse_advance(p); } if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("}"))) { tail = e; have_tail = true; }  else { qoz_vec_push__qoz_Stmt(&stmts, qoz_make_Stmt_SExpr(stmt_span, e)); } } } if ((p->pos == block_start_pos) && !done) { parse_advance(p); } } parse_expect_punct(p, QOZ_STR_LIT("}")); qoz_Expr* final_tail = qoz_make_Expr_ENil(span); qoz_gc_push_root(&final_tail); if (have_tail) { final_tail = tail; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Expr_EBlock(span, stmts, final_tail);
}

qoz_Vec__qoz_string parse_parse_path_segs(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Vec__qoz_string segs = qoz_vec_make__qoz_string(); qoz_Token first = parse_peek(p); if (parse_is_ident(first)) { qoz_vec_push__qoz_string(&segs, first.text); parse_advance(p); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("/"))) { parse_advance(p); qoz_Token part = parse_peek(p); if (parse_is_ident(part)) { qoz_vec_push__qoz_string(&segs, part.text); parse_advance(p); }  else { parse_err_unexpected(p, QOZ_STR_LIT("path segment"), part); } } }  else { parse_err_unexpected(p, QOZ_STR_LIT("path segment"), first); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return segs;
}

qoz_Decl* parse_parse_import(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); qoz_Vec__qoz_string path = parse_parse_path_segs(p); qoz_string alias = QOZ_STR_LIT(""); if (parse_is_kw(parse_peek(p), QOZ_STR_LIT("as"))) { parse_advance(p); alias = parse_expect_ident(p); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Decl_DImport(span, path, alias);
}

qoz_Vec__qoz_string parse_parse_type_params(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Vec__qoz_string params = qoz_vec_make__qoz_string(); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("<"))) { parse_advance(p); qoz_vec_push__qoz_string(&params, parse_expect_ident(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); qoz_vec_push__qoz_string(&params, parse_expect_ident(p)); } parse_expect_punct(p, QOZ_STR_LIT(">")); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return params;
}

qoz_Vec__qoz_StructField parse_parse_struct_fields(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Vec__qoz_StructField fields = qoz_vec_make__qoz_StructField(); parse_expect_punct(p, QOZ_STR_LIT("{")); while (!parse_is_punct(parse_peek(p), QOZ_STR_LIT("}")) && !parse_at_eof(p)) { int64_t start_pos = p->pos; qoz_string name = parse_expect_ident(p); parse_expect_punct(p, QOZ_STR_LIT(":")); qoz_TypeExpr* ty = parse_parse_type(p); qoz_gc_push_root(&ty); qoz_vec_push__qoz_StructField(&fields, ((qoz_StructField){ .name = name, .ty = ty })); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); } if (p->pos == start_pos) { parse_advance(p); } } parse_expect_punct(p, QOZ_STR_LIT("}")); qoz_gc_shadow_set_top(_qoz_shadow_guard); return fields;
}

qoz_Decl* parse_parse_top_type(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); qoz_string name = parse_expect_ident(p); qoz_Vec__qoz_string type_params = parse_parse_type_params(p); parse_expect_punct(p, QOZ_STR_LIT("=")); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("{"))) { qoz_Vec__qoz_StructField fields = parse_parse_struct_fields(p); return qoz_make_Decl_DStruct(span, name, type_params, fields);} if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("|"))) { qoz_Vec__qoz_VariantDecl variants = parse_parse_variants(p); return qoz_make_Decl_DEnum(span, name, type_params, variants);} qoz_TypeExpr* target = parse_parse_type(p); qoz_gc_push_root(&target); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Decl_DTypeAlias(span, name, type_params, target);
}

qoz_Vec__qoz_VariantDecl parse_parse_variants(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Vec__qoz_VariantDecl out = qoz_vec_make__qoz_VariantDecl(); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(";"))) { parse_advance(p); } while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("|"))) { qoz_Token bar_tok = parse_peek(p); qoz_Span v_span = parse_span_of(p, bar_tok); parse_advance(p); qoz_string vname = parse_expect_ident(p); qoz_VariantPayloadKind* kind = qoz_make_VariantPayloadKind_VPKNone(); qoz_gc_push_root(&kind); qoz_Vec__qoz_TypeExpr pos = qoz_vec_make__qoz_TypeExpr(); qoz_Vec__qoz_StructField named = qoz_vec_make__qoz_StructField(); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("("))) { kind = qoz_make_VariantPayloadKind_VPKPositional(); parse_advance(p); if (!parse_is_punct(parse_peek(p), QOZ_STR_LIT(")"))) { qoz_vec_push__qoz_TypeExpr(&pos, parse_parse_type(p)); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); qoz_vec_push__qoz_TypeExpr(&pos, parse_parse_type(p)); } } parse_expect_punct(p, QOZ_STR_LIT(")")); }  else if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("{"))) { kind = qoz_make_VariantPayloadKind_VPKNamed(); named = parse_parse_struct_fields(p); } qoz_vec_push__qoz_VariantDecl(&out, ((qoz_VariantDecl){ .span = v_span, .name = vname, .kind = kind, .pos = pos, .named = named })); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(";"))) { parse_advance(p); } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

qoz_Vec__qoz_FnParam parse_parse_fn_params(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Vec__qoz_FnParam params = qoz_vec_make__qoz_FnParam(); parse_expect_punct(p, QOZ_STR_LIT("(")); while (!parse_is_punct(parse_peek(p), QOZ_STR_LIT(")")) && !parse_at_eof(p)) { int64_t start_pos = p->pos; qoz_string name = parse_expect_ident(p); parse_expect_punct(p, QOZ_STR_LIT(":")); qoz_TypeExpr* ty = parse_parse_type(p); qoz_gc_push_root(&ty); qoz_vec_push__qoz_FnParam(&params, ((qoz_FnParam){ .name = name, .ty = ty })); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(","))) { parse_advance(p); } if (p->pos == start_pos) { parse_advance(p); } } parse_expect_punct(p, QOZ_STR_LIT(")")); qoz_gc_shadow_set_top(_qoz_shadow_guard); return params;
}

qoz_Decl* parse_parse_top_let(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); qoz_string name = parse_expect_ident(p); qoz_Vec__qoz_string type_params = parse_parse_type_params(p); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("("))) { qoz_Vec__qoz_FnParam params = parse_parse_fn_params(p); qoz_TypeExpr* ret_ty = qoz_make_TypeExpr_TEUnit(span); qoz_gc_push_root(&ret_ty); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(":"))) { parse_advance(p); ret_ty = parse_parse_type(p); } parse_expect_punct(p, QOZ_STR_LIT("=")); qoz_Expr* body = parse_parse_expr(p); qoz_gc_push_root(&body); return qoz_make_Decl_DFn(span, name, type_params, params, ret_ty, body, QOZ_STR_LIT(""));} qoz_TypeExpr* ann_ty = qoz_make_TypeExpr_TEUnit(span); qoz_gc_push_root(&ann_ty); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(":"))) { parse_advance(p); ann_ty = parse_parse_type(p); } parse_expect_punct(p, QOZ_STR_LIT("=")); qoz_Expr* value = parse_parse_expr(p); qoz_gc_push_root(&value); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Decl_DConst(span, name, ann_ty, value);
}

qoz_Decl* parse_parse_decl(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_string attr_link_name = QOZ_STR_LIT(""); qoz_string attr_operator = QOZ_STR_LIT(""); while (parse_is_punct(parse_peek(p), QOZ_STR_LIT("@"))) { parse_advance(p); qoz_string name = parse_expect_ident(p); qoz_string arg = QOZ_STR_LIT(""); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT("("))) { parse_advance(p); qoz_Token a = parse_peek(p); if (parse_is_str_lit(a)) { parse_advance(p); arg = parse_strip_quotes(a.text); }  else if (parse_is_ident(a)) { parse_advance(p); arg = a.text; } parse_expect_punct(p, QOZ_STR_LIT(")")); } if (qoz_string_eq(name, QOZ_STR_LIT("link_name"))) { attr_link_name = arg; }  else if (qoz_string_eq(name, QOZ_STR_LIT("operator"))) { attr_operator = arg; } } while (parse_is_punct(parse_peek(p), QOZ_STR_LIT(";"))) { parse_advance(p); } qoz_Token t = parse_peek(p); if (parse_is_kw(t, QOZ_STR_LIT("import"))) { return parse_parse_import(p);} if (parse_is_kw(t, QOZ_STR_LIT("type"))) { return parse_parse_top_type(p);} if (parse_is_kw(t, QOZ_STR_LIT("external"))) { return parse_parse_external(p, attr_link_name);} if (parse_is_kw(t, QOZ_STR_LIT("let"))) { return parse_parse_top_let_with_op(p, attr_operator);} if (parse_is_punct(t, QOZ_STR_LIT("#"))) { return parse_parse_link_directive(p);} parse_err_unexpected(p, QOZ_STR_LIT("top-level declaration"), t); parse_advance(p); qoz_Span span = parse_span_of(p, t); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Decl_DConst(span, QOZ_STR_LIT("<error>"), qoz_make_TypeExpr_TEUnit(span), qoz_make_Expr_ENil(span));
}

qoz_string parse_strip_quotes(qoz_string s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    int64_t n = (s).len; if (((n >= 2) && (qoz_string_byte_at(s, 0) == 34)) && (qoz_string_byte_at(s, n - 1) == 34)) { return qoz_string_slice(s, 1, n - 1);} qoz_gc_shadow_set_top(_qoz_shadow_guard); return s;
}

qoz_Decl* parse_parse_external(qoz_Parser* p, qoz_string link_name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token kw_tok = parse_peek(p); qoz_Span span = parse_span_of(p, kw_tok); parse_advance(p); if (!parse_is_kw(parse_peek(p), QOZ_STR_LIT("let"))) { parse_err_unexpected(p, QOZ_STR_LIT("`let` after `external`"), parse_peek(p)); }  else { parse_advance(p); } qoz_string name = parse_expect_ident(p); qoz_string symbol = ((qoz_string_eq(link_name, QOZ_STR_LIT(""))) ? name : link_name); qoz_Vec__qoz_FnParam params = parse_parse_fn_params(p); qoz_TypeExpr* ret_ty = qoz_make_TypeExpr_TEUnit(span); qoz_gc_push_root(&ret_ty); if (parse_is_punct(parse_peek(p), QOZ_STR_LIT(":"))) { parse_advance(p); ret_ty = parse_parse_type(p); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Decl_DExternal(span, name, symbol, params, ret_ty);
}

qoz_Decl* parse_parse_top_let_with_op(qoz_Parser* p, qoz_string operator) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Decl* d = parse_parse_top_let(p); qoz_gc_push_root(&d); if (qoz_string_eq(operator, QOZ_STR_LIT(""))) { return d;} qoz_Decl* _qoz_ms_24 = d; qoz_Decl* _qoz_mv_24 = NULL; switch (_qoz_ms_24->tag) { case qoz_Decl_DImport: { _qoz_mv_24 = (d);  break; } case qoz_Decl_DStruct: { _qoz_mv_24 = (d);  break; } case qoz_Decl_DEnum: { _qoz_mv_24 = (d);  break; } case qoz_Decl_DTypeAlias: { _qoz_mv_24 = (d);  break; } case qoz_Decl_DConst: { _qoz_mv_24 = (d);  break; } case qoz_Decl_DExternal: { _qoz_mv_24 = (d);  break; } case qoz_Decl_DLink: { _qoz_mv_24 = (d);  break; } case qoz_Decl_DFn: { qoz_Span s = _qoz_ms_24->payload.DFn.f0; qoz_string n = _qoz_ms_24->payload.DFn.f1; qoz_Vec__qoz_string tps = _qoz_ms_24->payload.DFn.f2; qoz_Vec__qoz_FnParam ps = _qoz_ms_24->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_24->payload.DFn.f4; qoz_Expr* body = _qoz_ms_24->payload.DFn.f5; _qoz_mv_24 = (qoz_make_Decl_DFn(s, n, tps, ps, ret, body, operator));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_24;
}

qoz_Decl* parse_parse_link_directive(qoz_Parser* p) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&p);
    qoz_Token hash_tok = parse_peek(p); qoz_Span span = parse_span_of(p, hash_tok); parse_advance(p); qoz_string name = parse_expect_ident(p); parse_expect_punct(p, QOZ_STR_LIT("(")); qoz_Token arg = parse_peek(p); qoz_string lib_name = QOZ_STR_LIT(""); if (parse_is_str_lit(arg)) { parse_advance(p); lib_name = parse_strip_quotes(arg.text); }  else { parse_err_unexpected(p, QOZ_STR_LIT("string literal"), arg); } parse_expect_punct(p, QOZ_STR_LIT(")")); qoz_LinkKind* kind = qoz_make_LinkKind_LinkLibrary(); qoz_gc_push_root(&kind); if (qoz_string_eq(name, QOZ_STR_LIT("link_framework"))) { kind = qoz_make_LinkKind_LinkFramework(); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Decl_DLink(span, kind, lib_name);
}

qoz_ParseOutput parse_run(qoz_Vec__qoz_Token tokens, qoz_string file) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Parser p = parse_make_parser(tokens, file); qoz_Vec__qoz_Decl decls = qoz_vec_make__qoz_Decl(); bool running = true; while (running && !parse_at_eof(&p)) { while (parse_is_punct(parse_peek(&p), QOZ_STR_LIT(";"))) { p.pos = p.pos + 1; } if (parse_at_eof(&p)) { running = false; }  else { int64_t start_pos = p.pos; qoz_vec_push__qoz_Decl(&decls, parse_parse_decl(&p)); if (p.pos == start_pos) { p.pos = p.pos + 1; } } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_ParseOutput){ .file = ((qoz_File){ .path = file, .decls = decls }), .errors = p.errors });
}

qoz_TyContext check_make_ctx(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_TyContext){ .enums = qoz_map_make__qoz_string__qoz_Decl(), .structs = qoz_map_make__qoz_string__qoz_Decl(), .aliases = qoz_map_make__qoz_string__qoz_Decl(), .fns = qoz_map_make__qoz_string__qoz_Decl(), .externs = qoz_map_make__qoz_string__qoz_Decl(), .packages = qoz_map_make__qoz_string__bool(), .variant_of = qoz_map_make__qoz_string__qoz_string(), .errors = qoz_vec_make__qoz_TypeError(), .type_params = qoz_map_make__qoz_string__bool(), .expr_types = qoz_map_make__int64_t__qoz_TypeExpr() });
}

int64_t check_expr_id(qoz_Expr* e) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((int64_t)((void*)e));
}

void check_record_error(qoz_TyContext* tc, qoz_Span span, qoz_string msg) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_vec_push__qoz_TypeError(&tc->errors, ((qoz_TypeError){ .span = span, .message = msg })); 
    return;
}

void check_register_decl(qoz_TyContext* tc, qoz_Decl* d) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_25 = d; switch (_qoz_ms_25->tag) { case qoz_Decl_DImport: { qoz_Vec__qoz_string path = _qoz_ms_25->payload.DImport.f1; qoz_string alias = _qoz_ms_25->payload.DImport.f2; {
        qoz_string name = alias; if (qoz_string_eq(name, QOZ_STR_LIT(""))) { if ((path.len) > 0) { name = path.data[(path.len) - 1]; } } qoz_map_set__qoz_string__bool(&tc->packages, name, true); 
    }
    0;  break; } case qoz_Decl_DEnum: { qoz_string name = _qoz_ms_25->payload.DEnum.f1; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_25->payload.DEnum.f3; {
        qoz_map_set__qoz_string__qoz_Decl(&tc->enums, name, d); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; qoz_map_set__qoz_string__qoz_string(&tc->variant_of, v.name, name); } }
    }
    0;  break; } case qoz_Decl_DStruct: { qoz_string name = _qoz_ms_25->payload.DStruct.f1; (qoz_map_set__qoz_string__qoz_Decl(&tc->structs, name, d), 0);  break; } case qoz_Decl_DTypeAlias: { qoz_string name = _qoz_ms_25->payload.DTypeAlias.f1; (qoz_map_set__qoz_string__qoz_Decl(&tc->aliases, name, d), 0);  break; } case qoz_Decl_DFn: { qoz_string name = _qoz_ms_25->payload.DFn.f1; (qoz_map_set__qoz_string__qoz_Decl(&tc->fns, name, d), 0);  break; } case qoz_Decl_DExternal: { qoz_string name = _qoz_ms_25->payload.DExternal.f1; (qoz_map_set__qoz_string__qoz_Decl(&tc->externs, name, d), 0);  break; } case qoz_Decl_DConst: { NULL;  break; } case qoz_Decl_DLink: { NULL;  break; } } 0; 
    return;
}

void check_register_file(qoz_TyContext* tc, qoz_File f) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    { qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; check_register_decl(tc, d); } }
    return;
}

void check_report(qoz_TyContext* tc) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    { qoz_Vec__qoz_TypeError __col = tc->errors; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeError e = __col.data[__i]; (void)e; qoz_string _qoz_bv_30;
    {
        qoz_Strbuf _qoz_sb_87_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_87_21); strings_sb_append(&_qoz_sb_87_21, e.span.file); strings_sb_append(&_qoz_sb_87_21, QOZ_STR_LIT(":")); strings_sb_append_i64(&_qoz_sb_87_21, e.span.line); strings_sb_append(&_qoz_sb_87_21, QOZ_STR_LIT(":")); strings_sb_append_i64(&_qoz_sb_87_21, e.span.col); strings_sb_append(&_qoz_sb_87_21, QOZ_STR_LIT(": ")); strings_sb_append(&_qoz_sb_87_21, e.message); _qoz_bv_30 = strings_sb_finish(&_qoz_sb_87_21);
    }
    fmt_println(_qoz_bv_30); } }
    return;
}

void check_summary(qoz_TyContext* tc) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_string _qoz_bv_31;
    {
        qoz_Strbuf _qoz_sb_92_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_92_17); strings_sb_append(&_qoz_sb_92_17, QOZ_STR_LIT("enums:    ")); strings_sb_append_i64(&_qoz_sb_92_17, (tc->enums.len)); _qoz_bv_31 = strings_sb_finish(&_qoz_sb_92_17);
    }
    fmt_println(_qoz_bv_31); qoz_string _qoz_bv_32;
    {
        qoz_Strbuf _qoz_sb_93_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_93_17); strings_sb_append(&_qoz_sb_93_17, QOZ_STR_LIT("structs:  ")); strings_sb_append_i64(&_qoz_sb_93_17, (tc->structs.len)); _qoz_bv_32 = strings_sb_finish(&_qoz_sb_93_17);
    }
    fmt_println(_qoz_bv_32); qoz_string _qoz_bv_33;
    {
        qoz_Strbuf _qoz_sb_94_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_94_17); strings_sb_append(&_qoz_sb_94_17, QOZ_STR_LIT("aliases:  ")); strings_sb_append_i64(&_qoz_sb_94_17, (tc->aliases.len)); _qoz_bv_33 = strings_sb_finish(&_qoz_sb_94_17);
    }
    fmt_println(_qoz_bv_33); qoz_string _qoz_bv_34;
    {
        qoz_Strbuf _qoz_sb_95_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_95_17); strings_sb_append(&_qoz_sb_95_17, QOZ_STR_LIT("fns:      ")); strings_sb_append_i64(&_qoz_sb_95_17, (tc->fns.len)); _qoz_bv_34 = strings_sb_finish(&_qoz_sb_95_17);
    }
    fmt_println(_qoz_bv_34); qoz_string _qoz_bv_35;
    {
        qoz_Strbuf _qoz_sb_96_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_96_17); strings_sb_append(&_qoz_sb_96_17, QOZ_STR_LIT("externs:  ")); strings_sb_append_i64(&_qoz_sb_96_17, (tc->externs.len)); _qoz_bv_35 = strings_sb_finish(&_qoz_sb_96_17);
    }
    fmt_println(_qoz_bv_35); qoz_string _qoz_bv_36;
    {
        qoz_Strbuf _qoz_sb_97_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_97_17); strings_sb_append(&_qoz_sb_97_17, QOZ_STR_LIT("packages: ")); strings_sb_append_i64(&_qoz_sb_97_17, (tc->packages.len)); _qoz_bv_36 = strings_sb_finish(&_qoz_sb_97_17);
    }
    fmt_println(_qoz_bv_36); qoz_string _qoz_bv_37;
    {
        qoz_Strbuf _qoz_sb_98_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_98_17); strings_sb_append(&_qoz_sb_98_17, QOZ_STR_LIT("errors:   ")); strings_sb_append_i64(&_qoz_sb_98_17, (tc->errors.len)); _qoz_bv_37 = strings_sb_finish(&_qoz_sb_98_17);
    }
    fmt_println(_qoz_bv_37); 
    return;
}

qoz_Ty* check_resolve_type(qoz_TyContext* tc, qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_26 = te; qoz_Ty* _qoz_mv_26 = NULL; switch (_qoz_ms_26->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_26 = (ty_ty_unit_());  break; } case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_26->payload.TEPtr.f1; _qoz_mv_26 = (ty_ty_ptr_(check_resolve_type(tc, inner)));  break; } case qoz_TypeExpr_TETuple: { _qoz_mv_26 = (ty_ty_error_());  break; } case qoz_TypeExpr_TENamed: { qoz_Span span = _qoz_ms_26->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_26->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_26->payload.TENamed.f2; _qoz_mv_26 = (check_resolve_named(tc, span, path, args));  break; } case qoz_TypeExpr_TEFn: { qoz_Vec__qoz_TypeExpr params = _qoz_ms_26->payload.TEFn.f1; qoz_TypeExpr* ret = _qoz_ms_26->payload.TEFn.f2; qoz_Ty* _qoz_bv_38;
    {
        qoz_Vec__qoz_Ty resolved = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_TypeExpr __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* p = __col.data[__i]; (void)p; qoz_vec_push__qoz_Ty(&resolved, check_resolve_type(tc, p)); } }_qoz_bv_38 = ty_ty_fn_(resolved, check_resolve_type(tc, ret));
    }
    _qoz_mv_26 = (_qoz_bv_38);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_26;
}

qoz_Ty* check_resolve_named(qoz_TyContext* tc, qoz_Span span, qoz_Vec__qoz_string path, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    if ((path.len) != 1) { check_record_error(tc, span, QOZ_STR_LIT("qualified type paths are not yet resolved in Stage B")); return ty_ty_error_();} qoz_string name = path.data[0]; if (qoz_map_contains__qoz_string__bool(&tc->type_params, name)) { return ty_ty_var_(name);} if (qoz_string_eq(name, QOZ_STR_LIT("i8"))) { return ty_ty_int_(8, true);} if (qoz_string_eq(name, QOZ_STR_LIT("i16"))) { return ty_ty_int_(16, true);} if (qoz_string_eq(name, QOZ_STR_LIT("i32"))) { return ty_ty_int_(32, true);} if (qoz_string_eq(name, QOZ_STR_LIT("i64"))) { return ty_ty_int_(64, true);} if (qoz_string_eq(name, QOZ_STR_LIT("u8"))) { return ty_ty_int_(8, false);} if (qoz_string_eq(name, QOZ_STR_LIT("u16"))) { return ty_ty_int_(16, false);} if (qoz_string_eq(name, QOZ_STR_LIT("u32"))) { return ty_ty_int_(32, false);} if (qoz_string_eq(name, QOZ_STR_LIT("u64"))) { return ty_ty_int_(64, false);} if (qoz_string_eq(name, QOZ_STR_LIT("f32"))) { return ty_ty_float_(32);} if (qoz_string_eq(name, QOZ_STR_LIT("f64"))) { return ty_ty_float_(64);} if (qoz_string_eq(name, QOZ_STR_LIT("bool"))) { return ty_ty_bool_();} if (qoz_string_eq(name, QOZ_STR_LIT("char"))) { return ty_ty_char_();} if (qoz_string_eq(name, QOZ_STR_LIT("string"))) { return ty_ty_string_();} if (qoz_string_eq(name, QOZ_STR_LIT("cstring"))) { return ty_ty_cstring_();} if (qoz_string_eq(name, QOZ_STR_LIT("unit"))) { return ty_ty_unit_();} if (qoz_string_eq(name, QOZ_STR_LIT("void"))) { return ty_ty_unit_();} qoz_Vec__qoz_Ty resolved_args = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_TypeExpr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_Ty(&resolved_args, check_resolve_type(tc, a)); } }if (qoz_map_contains__qoz_string__qoz_Decl(&tc->enums, name)) { return ty_ty_adt_(name, resolved_args);} if (qoz_map_contains__qoz_string__qoz_Decl(&tc->structs, name)) { return ty_ty_record_(name, resolved_args);} if (qoz_map_contains__qoz_string__qoz_Decl(&tc->aliases, name)) { return ty_ty_record_(name, resolved_args);} qoz_string _qoz_bv_39;
    {
        qoz_Strbuf _qoz_sb_158_28 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_158_28); strings_sb_append(&_qoz_sb_158_28, QOZ_STR_LIT("unknown type `")); strings_sb_append(&_qoz_sb_158_28, name); strings_sb_append(&_qoz_sb_158_28, QOZ_STR_LIT("`")); _qoz_bv_39 = strings_sb_finish(&_qoz_sb_158_28);
    }
    check_record_error(tc, span, _qoz_bv_39); qoz_gc_shadow_set_top(_qoz_shadow_guard); return ty_ty_error_();
}

qoz_Env check_env_make(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Env){ .bindings = qoz_vec_make__qoz_Binding() });
}

void check_env_define(qoz_Env* env, qoz_string name, qoz_Ty* t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&t);
    qoz_vec_push__qoz_Binding(&env->bindings, ((qoz_Binding){ .name = name, .ty = t })); 
    return;
}

qoz_Ty* check_env_lookup(qoz_Env* env, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&env);
    int64_t i = (env->bindings.len) - 1; while (i >= 0) { qoz_Binding b = env->bindings.data[i]; if (qoz_string_eq(b.name, name)) { return b.ty;} i = i - 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return ty_ty_error_();
}

bool check_env_has(qoz_Env* env, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&env);
    int64_t i = (env->bindings.len) - 1; while (i >= 0) { qoz_Binding b = env->bindings.data[i]; if (qoz_string_eq(b.name, name)) { return true;} i = i - 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

qoz_Ty* check_synth(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* e) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&e);
    qoz_Ty* t = check_synth_inner(tc, env, e); qoz_gc_push_root(&t); qoz_map_set__int64_t__qoz_TypeExpr(&tc->expr_types, check_expr_id(e), ty_ty_to_type_expr_at(t, ast_span_of_expr(e))); qoz_gc_shadow_set_top(_qoz_shadow_guard); return t;
}

qoz_Ty* check_synth_inner(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* e) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&e);
    qoz_Expr* _qoz_ms_27 = e; qoz_Ty* _qoz_mv_27 = NULL; switch (_qoz_ms_27->tag) { case qoz_Expr_EInt: { _qoz_mv_27 = (ty_ty_int_untyped());  break; } case qoz_Expr_EFloat: { _qoz_mv_27 = (ty_ty_float_untyped());  break; } case qoz_Expr_EString: { _qoz_mv_27 = (ty_ty_string_());  break; } case qoz_Expr_EChar: { _qoz_mv_27 = (ty_ty_char_());  break; } case qoz_Expr_EBool: { _qoz_mv_27 = (ty_ty_bool_());  break; } case qoz_Expr_ENil: { _qoz_mv_27 = (ty_ty_nil_());  break; } case qoz_Expr_EIdent: { qoz_Span span = _qoz_ms_27->payload.EIdent.f0; qoz_string name = _qoz_ms_27->payload.EIdent.f1; _qoz_mv_27 = (check_synth_ident(tc, env, span, name));  break; } case qoz_Expr_EPath: { _qoz_mv_27 = (ty_ty_error_());  break; } case qoz_Expr_EUnary: { qoz_UnaryOp* op = _qoz_ms_27->payload.EUnary.f1; qoz_Expr* rhs = _qoz_ms_27->payload.EUnary.f2; _qoz_mv_27 = (check_synth_unary(tc, env, op, rhs));  break; } case qoz_Expr_EBinary: { qoz_BinaryOp* op = _qoz_ms_27->payload.EBinary.f1; qoz_Expr* l = _qoz_ms_27->payload.EBinary.f2; qoz_Expr* r = _qoz_ms_27->payload.EBinary.f3; _qoz_mv_27 = (check_synth_binary(tc, env, op, l, r));  break; } case qoz_Expr_EAssign: { qoz_Expr* v = _qoz_ms_27->payload.EAssign.f3; qoz_Ty* _qoz_bv_40;
    {
        (void)(check_synth(tc, env, v)); _qoz_bv_40 = ty_ty_unit_();
    }
    _qoz_mv_27 = (_qoz_bv_40);  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_27->payload.ECall.f0; qoz_Expr* callee = _qoz_ms_27->payload.ECall.f1; qoz_Vec__qoz_TypeExpr ta = _qoz_ms_27->payload.ECall.f2; qoz_Vec__qoz_Expr args = _qoz_ms_27->payload.ECall.f3; _qoz_mv_27 = (check_synth_call_full(tc, env, sp, callee, ta, args));  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_27->payload.EField.f0; qoz_Expr* base = _qoz_ms_27->payload.EField.f1; qoz_string name = _qoz_ms_27->payload.EField.f2; _qoz_mv_27 = (check_synth_field(tc, env, sp, base, name));  break; } case qoz_Expr_EIndex: { qoz_Expr* base = _qoz_ms_27->payload.EIndex.f1; qoz_Expr* idx = _qoz_ms_27->payload.EIndex.f2; _qoz_mv_27 = (check_synth_index(tc, env, base, idx));  break; } case qoz_Expr_ECast: { qoz_TypeExpr* te = _qoz_ms_27->payload.ECast.f2; _qoz_mv_27 = (check_resolve_type(tc, te));  break; } case qoz_Expr_ETry: { qoz_Expr* v = _qoz_ms_27->payload.ETry.f1; qoz_Ty* _qoz_bv_41;
    {
        qoz_Ty* t = check_synth(tc, env, v); qoz_gc_push_root(&t); qoz_Ty* _qoz_ms_28 = t; qoz_Ty* _qoz_mv_28 = NULL; switch (_qoz_ms_28->tag) { case qoz_Ty_TyAdt: { qoz_string name = _qoz_ms_28->payload.TyAdt.f0; qoz_Vec__qoz_Ty args = _qoz_ms_28->payload.TyAdt.f1; qoz_Ty* _qoz_bv_42;
    {
        if (qoz_string_eq(name, QOZ_STR_LIT("Result")) && ((args.len) >= 1)) { return args.data[0];} _qoz_bv_42 = t;
    }
    _qoz_mv_28 = (_qoz_bv_42);  break; } default: { _qoz_mv_28 = (t);  break; } } _qoz_bv_41 = _qoz_mv_28;
    }
    _qoz_mv_27 = (_qoz_bv_41);  break; } case qoz_Expr_ETuple: { qoz_Vec__qoz_Expr elems = _qoz_ms_27->payload.ETuple.f1; qoz_Ty* _qoz_bv_43;
    {
        qoz_Vec__qoz_Ty elem_tys = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_Ty(&elem_tys, check_synth(tc, env, el)); } }_qoz_bv_43 = ty_ty_tuple_(elem_tys);
    }
    _qoz_mv_27 = (_qoz_bv_43);  break; } case qoz_Expr_ERecord: { qoz_TypeExpr* te = _qoz_ms_27->payload.ERecord.f1; _qoz_mv_27 = (check_resolve_type(tc, te));  break; } case qoz_Expr_EClosure: { qoz_Vec__qoz_ClosureParam cps = _qoz_ms_27->payload.EClosure.f1; qoz_TypeExpr* ret = _qoz_ms_27->payload.EClosure.f2; qoz_Expr* body = _qoz_ms_27->payload.EClosure.f3; qoz_Ty* _qoz_bv_44;
    {
        qoz_Vec__qoz_Ty pty = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_ClosureParam __col = cps; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_ClosureParam cp = __col.data[__i]; (void)cp; qoz_Ty* t = check_resolve_type(tc, cp.ty); qoz_gc_push_root(&t); qoz_vec_push__qoz_Ty(&pty, t); check_env_define(env, cp.name, t); } }(void)(check_synth(tc, env, body)); _qoz_bv_44 = ty_ty_fn_(pty, check_resolve_type(tc, ret));
    }
    _qoz_mv_27 = (_qoz_bv_44);  break; } case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_27->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_27->payload.EBlock.f2; _qoz_mv_27 = (check_synth_block(tc, env, stmts, tail));  break; } case qoz_Expr_EIf: { qoz_Expr* c = _qoz_ms_27->payload.EIf.f1; qoz_Expr* t = _qoz_ms_27->payload.EIf.f2; qoz_Expr* f = _qoz_ms_27->payload.EIf.f3; _qoz_mv_27 = (check_synth_if(tc, env, c, t, f));  break; } case qoz_Expr_EMatch: { qoz_Expr* scrut = _qoz_ms_27->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_27->payload.EMatch.f2; _qoz_mv_27 = (check_synth_match(tc, env, scrut, arms));  break; } case qoz_Expr_EWhile: { qoz_Expr* c = _qoz_ms_27->payload.EWhile.f1; qoz_Expr* b = _qoz_ms_27->payload.EWhile.f2; qoz_Ty* _qoz_bv_45;
    {
        (void)(check_synth(tc, env, c)); (void)(check_synth(tc, env, b)); _qoz_bv_45 = ty_ty_unit_();
    }
    _qoz_mv_27 = (_qoz_bv_45);  break; } case qoz_Expr_EFor: { qoz_string b1 = _qoz_ms_27->payload.EFor.f1; qoz_string b2 = _qoz_ms_27->payload.EFor.f2; qoz_Expr* it = _qoz_ms_27->payload.EFor.f3; qoz_Expr* b = _qoz_ms_27->payload.EFor.f4; qoz_Ty* _qoz_bv_46;
    {
        qoz_Ty* it_ty = check_synth(tc, env, it); qoz_gc_push_root(&it_ty); int64_t saved_len = (env->bindings.len); check_bind_for_loop(env, b1, b2, it_ty); (void)(check_synth(tc, env, b)); check_env_truncate(env, saved_len); _qoz_bv_46 = ty_ty_unit_();
    }
    _qoz_mv_27 = (_qoz_bv_46);  break; } case qoz_Expr_EReturn: { qoz_Expr* v = _qoz_ms_27->payload.EReturn.f1; qoz_Ty* _qoz_bv_47;
    {
        (void)(check_synth(tc, env, v)); _qoz_bv_47 = ty_ty_unit_();
    }
    _qoz_mv_27 = (_qoz_bv_47);  break; } case qoz_Expr_EDefer: { qoz_Expr* b = _qoz_ms_27->payload.EDefer.f1; qoz_Ty* _qoz_bv_48;
    {
        (void)(check_synth(tc, env, b)); _qoz_bv_48 = ty_ty_unit_();
    }
    _qoz_mv_27 = (_qoz_bv_48);  break; } case qoz_Expr_ESizeOf: { _qoz_mv_27 = (ty_ty_int_(64, true));  break; } case qoz_Expr_EArrayLit: { qoz_Vec__qoz_Expr elems = _qoz_ms_27->payload.EArrayLit.f1; qoz_Ty* _qoz_bv_49;
    {
        if ((elems.len) == 0) { return ty_ty_error_();} qoz_Ty* elem_ty = check_synth(tc, env, elems.data[0]); qoz_gc_push_root(&elem_ty); int64_t i = 1; while (i < (elems.len)) { (void)(check_synth(tc, env, elems.data[i])); i = i + 1; } qoz_Vec__qoz_Ty args = qoz_vec_make__qoz_Ty(); qoz_vec_push__qoz_Ty(&args, elem_ty); _qoz_bv_49 = ty_ty_record_(QOZ_STR_LIT("Vec"), args);
    }
    _qoz_mv_27 = (_qoz_bv_49);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_27;
}

qoz_Ty* check_synth_unary(qoz_TyContext* tc, qoz_Env* env, qoz_UnaryOp* op, qoz_Expr* rhs) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&op);
    qoz_gc_push_root(&rhs);
    qoz_Ty* t = check_synth(tc, env, rhs); qoz_gc_push_root(&t); qoz_UnaryOp* _qoz_ms_29 = op; qoz_Ty* _qoz_mv_29 = NULL; switch (_qoz_ms_29->tag) { case qoz_UnaryOp_UOpNeg: { _qoz_mv_29 = (t);  break; } case qoz_UnaryOp_UOpNot: { _qoz_mv_29 = (ty_ty_bool_());  break; } case qoz_UnaryOp_UOpDeref: { qoz_Ty* _qoz_ms_30 = t; qoz_Ty* _qoz_mv_30 = NULL; switch (_qoz_ms_30->tag) { case qoz_Ty_TyPtr: { qoz_Ty* inner = _qoz_ms_30->payload.TyPtr.f0; _qoz_mv_30 = (inner);  break; } default: { _qoz_mv_30 = (ty_ty_error_());  break; } } _qoz_mv_29 = (_qoz_mv_30);  break; } case qoz_UnaryOp_UOpAddr: { _qoz_mv_29 = (ty_ty_ptr_(t));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_29;
}

qoz_Ty* check_synth_field(qoz_TyContext* tc, qoz_Env* env, qoz_Span sp, qoz_Expr* base, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&base);
    qoz_Ty* bt = check_synth(tc, env, base); qoz_gc_push_root(&bt); qoz_gc_shadow_set_top(_qoz_shadow_guard); return check_field_type_of(tc, bt, name);
}

qoz_Ty* check_field_type_of(qoz_TyContext* tc, qoz_Ty* bt, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&bt);
    qoz_Ty* _qoz_ms_31 = bt; qoz_Ty* _qoz_mv_31 = NULL; switch (_qoz_ms_31->tag) { case qoz_Ty_TyPtr: { qoz_Ty* inner = _qoz_ms_31->payload.TyPtr.f0; _qoz_mv_31 = (check_field_type_of(tc, inner, name));  break; } case qoz_Ty_TyRecord: { qoz_string struct_name = _qoz_ms_31->payload.TyRecord.f0; qoz_Vec__qoz_Ty tyargs = _qoz_ms_31->payload.TyRecord.f1; _qoz_mv_31 = (check_lookup_field(tc, struct_name, tyargs, name));  break; } case qoz_Ty_TyTuple: { qoz_Vec__qoz_Ty elems = _qoz_ms_31->payload.TyTuple.f0; qoz_Ty* _qoz_bv_50;
    {
        if (((name).len >= 2) && (qoz_string_byte_at(name, 0) == 95)) { int64_t idx = qoz_string_parse_int(qoz_string_slice(name, 1, (name).len)); if ((idx >= 0) && (idx < (elems.len))) { return elems.data[idx];} } _qoz_bv_50 = ty_ty_error_();
    }
    _qoz_mv_31 = (_qoz_bv_50);  break; } default: { _qoz_mv_31 = (ty_ty_error_());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_31;
}

qoz_Ty* check_lookup_field(qoz_TyContext* tc, qoz_string struct_name, qoz_Vec__qoz_Ty tyargs, qoz_string field) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_Option__qoz_Decl* _qoz_ms_32 = qoz_map_get__qoz_string__qoz_Decl(&tc->structs, struct_name); qoz_Ty* _qoz_mv_32 = NULL; switch (_qoz_ms_32->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_32->payload.Some.f0; qoz_Decl* _qoz_ms_33 = d; qoz_Ty* _qoz_mv_33 = NULL; switch (_qoz_ms_33->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_33->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_33->payload.DStruct.f3; qoz_Ty* _qoz_bv_51;
    {
        { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; if (qoz_string_eq(f.name, field)) { if ((params.len) == (tyargs.len)) { return check_resolve_type_with_subst(tc, f.ty, params, tyargs);} return check_resolve_type(tc, f.ty);} } }_qoz_bv_51 = ty_ty_error_();
    }
    _qoz_mv_33 = (_qoz_bv_51);  break; } default: { _qoz_mv_33 = (ty_ty_error_());  break; } } _qoz_mv_32 = (_qoz_mv_33);  break; } case qoz_Option__qoz_Decl_None: { _qoz_mv_32 = (ty_ty_error_());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_32;
}

qoz_Ty* check_resolve_type_with_subst(qoz_TyContext* tc, qoz_TypeExpr* te, qoz_Vec__qoz_string params, qoz_Vec__qoz_Ty args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&te);
    qoz_Map__qoz_string__bool saved = tc->type_params; check_set_type_params(tc, params); qoz_Ty* raw = check_resolve_type(tc, te); qoz_gc_push_root(&raw); tc->type_params = saved; qoz_gc_shadow_set_top(_qoz_shadow_guard); return check_apply_subst(raw, params, args);
}

qoz_Ty* check_apply_subst(qoz_Ty* t, qoz_Vec__qoz_string params, qoz_Vec__qoz_Ty args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&t);
    qoz_Ty* _qoz_ms_34 = t; qoz_Ty* _qoz_mv_34 = NULL; switch (_qoz_ms_34->tag) { case qoz_Ty_TyVar: { qoz_string name = _qoz_ms_34->payload.TyVar.f1; qoz_Ty* _qoz_bv_52;
    {
        int64_t i = 0; { qoz_Vec__qoz_string __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string p = __col.data[__i]; (void)p; if (qoz_string_eq(p, name)) { if (i < (args.len)) { return args.data[i];} return t;} i = i + 1; } }_qoz_bv_52 = t;
    }
    _qoz_mv_34 = (_qoz_bv_52);  break; } case qoz_Ty_TyPtr: { qoz_Ty* inner = _qoz_ms_34->payload.TyPtr.f0; _qoz_mv_34 = (ty_ty_ptr_(check_apply_subst(inner, params, args)));  break; } case qoz_Ty_TyAdt: { qoz_string n = _qoz_ms_34->payload.TyAdt.f0; qoz_Vec__qoz_Ty a = _qoz_ms_34->payload.TyAdt.f1; qoz_Ty* _qoz_bv_53;
    {
        qoz_Vec__qoz_Ty na = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_Ty __col = a; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* x = __col.data[__i]; (void)x; qoz_vec_push__qoz_Ty(&na, check_apply_subst(x, params, args)); } }_qoz_bv_53 = ty_ty_adt_(n, na);
    }
    _qoz_mv_34 = (_qoz_bv_53);  break; } case qoz_Ty_TyRecord: { qoz_string n = _qoz_ms_34->payload.TyRecord.f0; qoz_Vec__qoz_Ty a = _qoz_ms_34->payload.TyRecord.f1; qoz_Ty* _qoz_bv_54;
    {
        qoz_Vec__qoz_Ty na = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_Ty __col = a; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* x = __col.data[__i]; (void)x; qoz_vec_push__qoz_Ty(&na, check_apply_subst(x, params, args)); } }_qoz_bv_54 = ty_ty_record_(n, na);
    }
    _qoz_mv_34 = (_qoz_bv_54);  break; } case qoz_Ty_TyFn: { qoz_Vec__qoz_Ty p = _qoz_ms_34->payload.TyFn.f0; qoz_Ty* r = _qoz_ms_34->payload.TyFn.f1; qoz_Ty* _qoz_bv_55;
    {
        qoz_Vec__qoz_Ty np = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_Ty __col = p; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* x = __col.data[__i]; (void)x; qoz_vec_push__qoz_Ty(&np, check_apply_subst(x, params, args)); } }_qoz_bv_55 = ty_ty_fn_(np, check_apply_subst(r, params, args));
    }
    _qoz_mv_34 = (_qoz_bv_55);  break; } default: { _qoz_mv_34 = (t);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_34;
}

qoz_Ty* check_synth_index(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* base, qoz_Expr* idx) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&base);
    qoz_gc_push_root(&idx);
    qoz_Ty* bt = check_synth(tc, env, base); qoz_gc_push_root(&bt); (void)(check_synth(tc, env, idx)); qoz_Ty* _qoz_ms_35 = bt; qoz_Ty* _qoz_mv_35 = NULL; switch (_qoz_ms_35->tag) { case qoz_Ty_TyPtr: { qoz_Ty* inner = _qoz_ms_35->payload.TyPtr.f0; _qoz_mv_35 = (inner);  break; } case qoz_Ty_TyRecord: { qoz_string name = _qoz_ms_35->payload.TyRecord.f0; qoz_Vec__qoz_Ty args = _qoz_ms_35->payload.TyRecord.f1; qoz_Ty* _qoz_bv_56;
    {
        if (qoz_string_eq(name, QOZ_STR_LIT("Vec")) && ((args.len) >= 1)) { return args.data[0];} _qoz_bv_56 = ty_ty_error_();
    }
    _qoz_mv_35 = (_qoz_bv_56);  break; } default: { _qoz_mv_35 = (ty_ty_error_());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_35;
}

bool check_is_strings_callee(qoz_Expr* callee, qoz_string method) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&callee);
    qoz_Expr* _qoz_ms_36 = callee; bool _qoz_mv_36 = false; switch (_qoz_ms_36->tag) { case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_36->payload.EField.f1; qoz_string m = _qoz_ms_36->payload.EField.f2; bool _qoz_bv_57;
    {
        if (!qoz_string_eq(m, method)) { return false;} qoz_Expr* _qoz_ms_37 = base; bool _qoz_mv_37 = false; switch (_qoz_ms_37->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_37->payload.EIdent.f1; _qoz_mv_37 = (qoz_string_eq(name, QOZ_STR_LIT("strings")));  break; } default: { _qoz_mv_37 = (false);  break; } } _qoz_bv_57 = _qoz_mv_37;
    }
    _qoz_mv_36 = (_qoz_bv_57);  break; } default: { _qoz_mv_36 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_36;
}

qoz_string check_sb_append_method_for(qoz_Ty* t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&t);
    qoz_Ty* _qoz_ms_38 = t; qoz_string _qoz_mv_38 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_38->tag) { case qoz_Ty_TyString: { _qoz_mv_38 = (QOZ_STR_LIT("sb_append"));  break; } case qoz_Ty_TyBool: { _qoz_mv_38 = (QOZ_STR_LIT("sb_append_bool"));  break; } case qoz_Ty_TyFloat: { _qoz_mv_38 = (QOZ_STR_LIT("sb_append_f64"));  break; } case qoz_Ty_TyInt: { _qoz_mv_38 = (QOZ_STR_LIT("sb_append_i64"));  break; } case qoz_Ty_TyChar: { _qoz_mv_38 = (QOZ_STR_LIT("sb_append_i64"));  break; } default: { _qoz_mv_38 = (QOZ_STR_LIT("sb_append"));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_38;
}

qoz_string check_resolve_callee_fn(qoz_TyContext* tc, qoz_Expr* callee) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&callee);
    qoz_Expr* _qoz_ms_39 = callee; qoz_string _qoz_mv_39 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_39->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_39->payload.EIdent.f1; _qoz_mv_39 = (name);  break; } case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_39->payload.EField.f1; qoz_string method = _qoz_ms_39->payload.EField.f2; qoz_Expr* _qoz_ms_40 = base; qoz_string _qoz_mv_40 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_40->tag) { case qoz_Expr_EIdent: { qoz_string pkg = _qoz_ms_40->payload.EIdent.f1; qoz_string _qoz_bv_58;
    {
        if (qoz_map_contains__qoz_string__bool(&tc->packages, pkg)) { return qoz_string_cat(qoz_string_cat(pkg, QOZ_STR_LIT("_")), method);} _qoz_bv_58 = QOZ_STR_LIT("");
    }
    _qoz_mv_40 = (_qoz_bv_58);  break; } default: { _qoz_mv_40 = (QOZ_STR_LIT(""));  break; } } _qoz_mv_39 = (_qoz_mv_40);  break; } default: { _qoz_mv_39 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_39;
}

qoz_Ty* check_synth_call_full(qoz_TyContext* tc, qoz_Env* env, qoz_Span sp, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&callee);
    qoz_string fn_name = check_resolve_callee_fn(tc, callee); if (qoz_string_eq(fn_name, QOZ_STR_LIT("len")) || qoz_string_eq(fn_name, QOZ_STR_LIT("size_of"))) { { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; (void)(check_synth(tc, env, a)); } }return ty_ty_int_(64, true);} if (qoz_string_eq(fn_name, QOZ_STR_LIT("hash"))) { { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; (void)(check_synth(tc, env, a)); } }return ty_ty_int_(64, false);} qoz_Vec__qoz_Ty arg_tys = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_Ty(&arg_tys, check_synth(tc, env, a)); } }if (qoz_string_eq(fn_name, QOZ_STR_LIT(""))) { return ty_ty_error_();} qoz_Option__qoz_Decl* _qoz_ms_41 = qoz_map_get__qoz_string__qoz_Decl(&tc->fns, fn_name); qoz_Ty* _qoz_mv_41 = NULL; switch (_qoz_ms_41->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_41->payload.Some.f0; _qoz_mv_41 = (check_synth_call_with_decl(tc, sp, d, type_args, arg_tys));  break; } case qoz_Option__qoz_Decl_None: { qoz_Option__qoz_Decl* _qoz_ms_42 = qoz_map_get__qoz_string__qoz_Decl(&tc->externs, fn_name); qoz_Ty* _qoz_mv_42 = NULL; switch (_qoz_ms_42->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_42->payload.Some.f0; _qoz_mv_42 = (check_synth_call_with_decl(tc, sp, d, type_args, arg_tys));  break; } case qoz_Option__qoz_Decl_None: { _qoz_mv_42 = (check_synth_variant_ctor(tc, fn_name));  break; } } _qoz_mv_41 = (_qoz_mv_42);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_41;
}

qoz_Ty* check_synth_variant_ctor(qoz_TyContext* tc, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_Option__qoz_string* _qoz_ms_43 = qoz_map_get__qoz_string__qoz_string(&tc->variant_of, name); qoz_Ty* _qoz_mv_43 = NULL; switch (_qoz_ms_43->tag) { case qoz_Option__qoz_string_Some: { qoz_string enum_name = _qoz_ms_43->payload.Some.f0; qoz_Option__qoz_Decl* _qoz_ms_44 = qoz_map_get__qoz_string__qoz_Decl(&tc->enums, enum_name); qoz_Ty* _qoz_mv_44 = NULL; switch (_qoz_ms_44->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_44->payload.Some.f0; qoz_Decl* _qoz_ms_45 = d; qoz_Ty* _qoz_mv_45 = NULL; switch (_qoz_ms_45->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string tparams = _qoz_ms_45->payload.DEnum.f2; qoz_Ty* _qoz_bv_59;
    {
        qoz_Vec__qoz_Ty args = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_string __col = tparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string p = __col.data[__i]; (void)p; qoz_vec_push__qoz_Ty(&args, ty_ty_var_(p)); } }_qoz_bv_59 = ty_ty_adt_(enum_name, args);
    }
    _qoz_mv_45 = (_qoz_bv_59);  break; } default: { _qoz_mv_45 = (ty_ty_error_());  break; } } _qoz_mv_44 = (_qoz_mv_45);  break; } case qoz_Option__qoz_Decl_None: { _qoz_mv_44 = (ty_ty_error_());  break; } } _qoz_mv_43 = (_qoz_mv_44);  break; } case qoz_Option__qoz_string_None: { _qoz_mv_43 = (ty_ty_error_());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_43;
}

qoz_Ty* check_synth_call_with_decl(qoz_TyContext* tc, qoz_Span sp, qoz_Decl* d, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Ty arg_tys) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_46 = d; qoz_Ty* _qoz_mv_46 = NULL; switch (_qoz_ms_46->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string tparams = _qoz_ms_46->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_46->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_46->payload.DFn.f4; qoz_Ty* _qoz_bv_60;
    {
        if ((arg_tys.len) != (params.len)) { qoz_string _qoz_bv_61;
    {
        qoz_Strbuf _qoz_sb_494_34 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_494_34); strings_sb_append(&_qoz_sb_494_34, QOZ_STR_LIT("function expects ")); strings_sb_append_i64(&_qoz_sb_494_34, (params.len)); strings_sb_append(&_qoz_sb_494_34, QOZ_STR_LIT(" argument(s), got ")); strings_sb_append_i64(&_qoz_sb_494_34, (arg_tys.len)); _qoz_bv_61 = strings_sb_finish(&_qoz_sb_494_34);
    }
    check_record_error(tc, sp, _qoz_bv_61); } if ((tparams.len) == 0) { check_set_type_params(tc, tparams); int64_t i = 0; while ((i < (params.len)) && (i < (arg_tys.len))) { qoz_Ty* p_ty = check_resolve_type(tc, params.data[i].ty); qoz_gc_push_root(&p_ty); if (!ty_ty_assignable(p_ty, arg_tys.data[i])) { qoz_string want = ty_ty_show(p_ty); qoz_string got = ty_ty_show(arg_tys.data[i]); qoz_string _qoz_bv_62;
    {
        qoz_Strbuf _qoz_sb_504_42 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_504_42); strings_sb_append(&_qoz_sb_504_42, QOZ_STR_LIT("expected ")); strings_sb_append(&_qoz_sb_504_42, want); strings_sb_append(&_qoz_sb_504_42, QOZ_STR_LIT(", got ")); strings_sb_append(&_qoz_sb_504_42, got); _qoz_bv_62 = strings_sb_finish(&_qoz_sb_504_42);
    }
    check_record_error(tc, sp, _qoz_bv_62); } i = i + 1; } qoz_Ty* r = check_resolve_type(tc, ret); qoz_gc_push_root(&r); check_clear_type_params(tc); return r;} qoz_Vec__qoz_Ty tyarg_tys = qoz_vec_make__qoz_Ty(); if ((type_args.len) == (tparams.len)) { { qoz_Vec__qoz_TypeExpr __col = type_args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* ta = __col.data[__i]; (void)ta; qoz_vec_push__qoz_Ty(&tyarg_tys, check_resolve_type(tc, ta)); } }}  else { qoz_Map__qoz_string__bool saved_a = tc->type_params; check_set_type_params(tc, tparams); qoz_Vec__qoz_Ty param_tys = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; qoz_vec_push__qoz_Ty(&param_tys, check_resolve_type(tc, pp.ty)); } }tc->type_params = saved_a; qoz_Map__qoz_string__qoz_Ty subst_env = qoz_map_make__qoz_string__qoz_Ty(); bool ok = true; int64_t i = 0; while ((i < (arg_tys.len)) && (i < (param_tys.len))) { if (!check_unify(param_tys.data[i], arg_tys.data[i], &subst_env)) { ok = false; } i = i + 1; } if (!ok) { check_record_error(tc, sp, QOZ_STR_LIT("could not infer generic type arguments at call site")); } { qoz_Vec__qoz_string __col = tparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string tp = __col.data[__i]; (void)tp; qoz_Option__qoz_Ty* _qoz_ms_47 = qoz_map_get__qoz_string__qoz_Ty(&subst_env, tp); switch (_qoz_ms_47->tag) { case qoz_Option__qoz_Ty_Some: { qoz_Ty* t = _qoz_ms_47->payload.Some.f0; qoz_vec_push__qoz_Ty(&tyarg_tys, t);  break; } case qoz_Option__qoz_Ty_None: { {
        qoz_string _qoz_bv_63;
    {
        qoz_Strbuf _qoz_sb_535_42 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_535_42); strings_sb_append(&_qoz_sb_535_42, QOZ_STR_LIT("type parameter `")); strings_sb_append(&_qoz_sb_535_42, tp); strings_sb_append(&_qoz_sb_535_42, QOZ_STR_LIT("` is not constrained by any argument; add an explicit annotation")); _qoz_bv_63 = strings_sb_finish(&_qoz_sb_535_42);
    }
    check_record_error(tc, sp, _qoz_bv_63); qoz_vec_push__qoz_Ty(&tyarg_tys, ty_ty_error_()); 
    }
    0;  break; } } 0; } }} qoz_Map__qoz_string__bool saved_b = tc->type_params; check_set_type_params(tc, tparams); qoz_Ty* raw_ret = check_resolve_type(tc, ret); qoz_gc_push_root(&raw_ret); tc->type_params = saved_b; _qoz_bv_60 = check_apply_subst(raw_ret, tparams, tyarg_tys);
    }
    _qoz_mv_46 = (_qoz_bv_60);  break; } case qoz_Decl_DExternal: { qoz_TypeExpr* ret = _qoz_ms_46->payload.DExternal.f4; _qoz_mv_46 = (check_resolve_type(tc, ret));  break; } default: { _qoz_mv_46 = (ty_ty_error_());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_46;
}

bool check_unify(qoz_Ty* pattern, qoz_Ty* concrete, qoz_Map__qoz_string__qoz_Ty* env) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&pattern);
    qoz_gc_push_root(&concrete);
    qoz_gc_push_root(&env);
    qoz_Ty* _qoz_ms_48 = pattern; bool _qoz_mv_48 = false; switch (_qoz_ms_48->tag) { case qoz_Ty_TyVar: { qoz_string name = _qoz_ms_48->payload.TyVar.f1; qoz_Option__qoz_Ty* _qoz_ms_49 = qoz_map_get__qoz_string__qoz_Ty(env, name); bool _qoz_mv_49 = false; switch (_qoz_ms_49->tag) { case qoz_Option__qoz_Ty_Some: { qoz_Ty* prior = _qoz_ms_49->payload.Some.f0; _qoz_mv_49 = (check_compatible_or_refine(env, name, prior, concrete));  break; } case qoz_Option__qoz_Ty_None: { bool _qoz_bv_64;
    {
        qoz_map_set__qoz_string__qoz_Ty(env, name, concrete); _qoz_bv_64 = true;
    }
    _qoz_mv_49 = (_qoz_bv_64);  break; } } _qoz_mv_48 = (_qoz_mv_49);  break; } case qoz_Ty_TyPtr: { qoz_Ty* pi = _qoz_ms_48->payload.TyPtr.f0; qoz_Ty* _qoz_ms_50 = concrete; bool _qoz_mv_50 = false; switch (_qoz_ms_50->tag) { case qoz_Ty_TyPtr: { qoz_Ty* ci = _qoz_ms_50->payload.TyPtr.f0; _qoz_mv_50 = (check_unify(pi, ci, env));  break; } case qoz_Ty_TyNil: { _qoz_mv_50 = (true);  break; } default: { _qoz_mv_50 = (false);  break; } } _qoz_mv_48 = (_qoz_mv_50);  break; } case qoz_Ty_TyAdt: { qoz_string pn = _qoz_ms_48->payload.TyAdt.f0; qoz_Vec__qoz_Ty pa = _qoz_ms_48->payload.TyAdt.f1; qoz_Ty* _qoz_ms_51 = concrete; bool _qoz_mv_51 = false; switch (_qoz_ms_51->tag) { case qoz_Ty_TyAdt: { qoz_string cn = _qoz_ms_51->payload.TyAdt.f0; qoz_Vec__qoz_Ty ca = _qoz_ms_51->payload.TyAdt.f1; _qoz_mv_51 = (qoz_string_eq(pn, cn) && check_unify_args(pa, ca, env));  break; } default: { _qoz_mv_51 = (false);  break; } } _qoz_mv_48 = (_qoz_mv_51);  break; } case qoz_Ty_TyRecord: { qoz_string pn = _qoz_ms_48->payload.TyRecord.f0; qoz_Vec__qoz_Ty pa = _qoz_ms_48->payload.TyRecord.f1; qoz_Ty* _qoz_ms_52 = concrete; bool _qoz_mv_52 = false; switch (_qoz_ms_52->tag) { case qoz_Ty_TyRecord: { qoz_string cn = _qoz_ms_52->payload.TyRecord.f0; qoz_Vec__qoz_Ty ca = _qoz_ms_52->payload.TyRecord.f1; _qoz_mv_52 = (qoz_string_eq(pn, cn) && check_unify_args(pa, ca, env));  break; } default: { _qoz_mv_52 = (false);  break; } } _qoz_mv_48 = (_qoz_mv_52);  break; } case qoz_Ty_TyInt: { qoz_IntInfo pi = _qoz_ms_48->payload.TyInt.f0; qoz_Ty* _qoz_ms_53 = concrete; bool _qoz_mv_53 = false; switch (_qoz_ms_53->tag) { case qoz_Ty_TyInt: { qoz_IntInfo ci = _qoz_ms_53->payload.TyInt.f0; _qoz_mv_53 = (ci.untyped || ((pi.width == ci.width) && (pi.is_signed == ci.is_signed)));  break; } default: { _qoz_mv_53 = (false);  break; } } _qoz_mv_48 = (_qoz_mv_53);  break; } case qoz_Ty_TyFloat: { qoz_FloatInfo pf = _qoz_ms_48->payload.TyFloat.f0; qoz_Ty* _qoz_ms_54 = concrete; bool _qoz_mv_54 = false; switch (_qoz_ms_54->tag) { case qoz_Ty_TyFloat: { qoz_FloatInfo cf = _qoz_ms_54->payload.TyFloat.f0; _qoz_mv_54 = (cf.untyped || (pf.width == cf.width));  break; } default: { _qoz_mv_54 = (false);  break; } } _qoz_mv_48 = (_qoz_mv_54);  break; } case qoz_Ty_TyFn: { qoz_Vec__qoz_Ty pp = _qoz_ms_48->payload.TyFn.f0; qoz_Ty* pr = _qoz_ms_48->payload.TyFn.f1; qoz_Ty* _qoz_ms_55 = concrete; bool _qoz_mv_55 = false; switch (_qoz_ms_55->tag) { case qoz_Ty_TyFn: { qoz_Vec__qoz_Ty cp = _qoz_ms_55->payload.TyFn.f0; qoz_Ty* cr = _qoz_ms_55->payload.TyFn.f1; _qoz_mv_55 = (check_unify_args(pp, cp, env) && check_unify(pr, cr, env));  break; } default: { _qoz_mv_55 = (false);  break; } } _qoz_mv_48 = (_qoz_mv_55);  break; } default: { _qoz_mv_48 = (ty_ty_eq(pattern, concrete));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_48;
}

bool check_compatible_or_refine(qoz_Map__qoz_string__qoz_Ty* env, qoz_string name, qoz_Ty* prior, qoz_Ty* concrete) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&prior);
    qoz_gc_push_root(&concrete);
    if (ty_ty_eq(prior, concrete)) { return true;} qoz_Ty* _qoz_ms_56 = prior; bool _qoz_mv_56 = false; switch (_qoz_ms_56->tag) { case qoz_Ty_TyInt: { qoz_IntInfo pi = _qoz_ms_56->payload.TyInt.f0; qoz_Ty* _qoz_ms_57 = concrete; bool _qoz_mv_57 = false; switch (_qoz_ms_57->tag) { case qoz_Ty_TyInt: { qoz_IntInfo ci = _qoz_ms_57->payload.TyInt.f0; bool _qoz_bv_65;
    {
        if (ci.untyped) { return true;} if (pi.untyped) { qoz_map_set__qoz_string__qoz_Ty(env, name, concrete); return true;} _qoz_bv_65 = false;
    }
    _qoz_mv_57 = (_qoz_bv_65);  break; } default: { _qoz_mv_57 = (false);  break; } } _qoz_mv_56 = (_qoz_mv_57);  break; } case qoz_Ty_TyFloat: { qoz_FloatInfo pf = _qoz_ms_56->payload.TyFloat.f0; qoz_Ty* _qoz_ms_58 = concrete; bool _qoz_mv_58 = false; switch (_qoz_ms_58->tag) { case qoz_Ty_TyFloat: { qoz_FloatInfo cf = _qoz_ms_58->payload.TyFloat.f0; bool _qoz_bv_66;
    {
        if (cf.untyped) { return true;} if (pf.untyped) { qoz_map_set__qoz_string__qoz_Ty(env, name, concrete); return true;} _qoz_bv_66 = false;
    }
    _qoz_mv_58 = (_qoz_bv_66);  break; } default: { _qoz_mv_58 = (false);  break; } } _qoz_mv_56 = (_qoz_mv_58);  break; } default: { _qoz_mv_56 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_56;
}

bool check_unify_args(qoz_Vec__qoz_Ty a, qoz_Vec__qoz_Ty b, qoz_Map__qoz_string__qoz_Ty* env) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&env);
    if ((a.len) != (b.len)) { return false;} int64_t i = 0; while (i < (a.len)) { if (!check_unify(a.data[i], b.data[i], env)) { return false;} i = i + 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return true;
}

bool check_is_ty_error(qoz_Ty* t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&t);
    qoz_Ty* _qoz_ms_59 = t; bool _qoz_mv_59 = false; switch (_qoz_ms_59->tag) { case qoz_Ty_TyError: { _qoz_mv_59 = (true);  break; } default: { _qoz_mv_59 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_59;
}

qoz_Ty* check_synth_ident(qoz_TyContext* tc, qoz_Env* env, qoz_Span span, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    if (check_env_has(env, name)) { return check_env_lookup(env, name);} if (qoz_map_contains__qoz_string__qoz_Decl(&tc->fns, name)) { return ty_ty_error_();} if (qoz_map_contains__qoz_string__qoz_Decl(&tc->externs, name)) { return ty_ty_error_();} qoz_Option__qoz_string* _qoz_ms_60 = qoz_map_get__qoz_string__qoz_string(&tc->variant_of, name); switch (_qoz_ms_60->tag) { case qoz_Option__qoz_string_Some: { qoz_string enum_name = _qoz_ms_60->payload.Some.f0; return check_synth_variant_ctor(tc, name); break; } case qoz_Option__qoz_string_None: { NULL;  break; } } 0; qoz_string _qoz_bv_67;
    {
        qoz_Strbuf _qoz_sb_668_28 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_668_28); strings_sb_append(&_qoz_sb_668_28, QOZ_STR_LIT("undefined name `")); strings_sb_append(&_qoz_sb_668_28, name); strings_sb_append(&_qoz_sb_668_28, QOZ_STR_LIT("`")); _qoz_bv_67 = strings_sb_finish(&_qoz_sb_668_28);
    }
    check_record_error(tc, span, _qoz_bv_67); qoz_gc_shadow_set_top(_qoz_shadow_guard); return ty_ty_error_();
}

qoz_Ty* check_synth_binary(qoz_TyContext* tc, qoz_Env* env, qoz_BinaryOp* op, qoz_Expr* l, qoz_Expr* r) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&op);
    qoz_gc_push_root(&l);
    qoz_gc_push_root(&r);
    qoz_Ty* lt = check_synth(tc, env, l); qoz_gc_push_root(&lt); (void)(check_synth(tc, env, r)); qoz_BinaryOp* _qoz_ms_61 = op; qoz_Ty* _qoz_mv_61 = NULL; switch (_qoz_ms_61->tag) { case qoz_BinaryOp_BOpEq: { _qoz_mv_61 = (ty_ty_bool_());  break; } case qoz_BinaryOp_BOpNe: { _qoz_mv_61 = (ty_ty_bool_());  break; } case qoz_BinaryOp_BOpLt: { _qoz_mv_61 = (ty_ty_bool_());  break; } case qoz_BinaryOp_BOpGt: { _qoz_mv_61 = (ty_ty_bool_());  break; } case qoz_BinaryOp_BOpLe: { _qoz_mv_61 = (ty_ty_bool_());  break; } case qoz_BinaryOp_BOpGe: { _qoz_mv_61 = (ty_ty_bool_());  break; } case qoz_BinaryOp_BOpAnd: { _qoz_mv_61 = (ty_ty_bool_());  break; } case qoz_BinaryOp_BOpOr: { _qoz_mv_61 = (ty_ty_bool_());  break; } case qoz_BinaryOp_BOpAdd: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpSub: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpMul: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpDiv: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpMod: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpBitAnd: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpBitOr: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpBitXor: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpShl: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpShr: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpRange: { _qoz_mv_61 = (lt);  break; } case qoz_BinaryOp_BOpRangeInclusive: { _qoz_mv_61 = (lt);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_61;
}

qoz_Ty* check_synth_block(qoz_TyContext* tc, qoz_Env* env, qoz_Vec__qoz_Stmt stmts, qoz_Expr* tail) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&tail);
    { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; qoz_Stmt* _qoz_ms_62 = s; switch (_qoz_ms_62->tag) { case qoz_Stmt_SLet: { qoz_string name = _qoz_ms_62->payload.SLet.f1; qoz_TypeExpr* te = _qoz_ms_62->payload.SLet.f2; qoz_Expr* val = _qoz_ms_62->payload.SLet.f3; {
        qoz_Ty* vt = check_synth(tc, env, val); qoz_gc_push_root(&vt); qoz_TypeExpr* _qoz_ms_63 = te; qoz_Ty* _qoz_mv_63 = NULL; switch (_qoz_ms_63->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_63 = (vt);  break; } default: { _qoz_mv_63 = (check_resolve_type(tc, te));  break; } } qoz_Ty* bound = _qoz_mv_63; qoz_gc_push_root(&bound); check_env_define(env, name, bound); 
    }
    0;  break; } case qoz_Stmt_SVar: { qoz_string name = _qoz_ms_62->payload.SVar.f1; qoz_TypeExpr* te = _qoz_ms_62->payload.SVar.f2; qoz_Expr* val = _qoz_ms_62->payload.SVar.f3; {
        qoz_Ty* vt = check_synth(tc, env, val); qoz_gc_push_root(&vt); qoz_TypeExpr* _qoz_ms_64 = te; qoz_Ty* _qoz_mv_64 = NULL; switch (_qoz_ms_64->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_64 = (vt);  break; } default: { _qoz_mv_64 = (check_resolve_type(tc, te));  break; } } qoz_Ty* bound = _qoz_mv_64; qoz_gc_push_root(&bound); check_env_define(env, name, bound); 
    }
    0;  break; } case qoz_Stmt_SExpr: { qoz_Expr* ex = _qoz_ms_62->payload.SExpr.f1; void* _qoz_bv_68;
    {
        (void)(check_synth(tc, env, ex)); _qoz_bv_68 = NULL;
    }
    _qoz_bv_68;  break; } } 0; } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return check_synth(tc, env, tail);
}

qoz_Ty* check_synth_if(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* c, qoz_Expr* t, qoz_Expr* f) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&c);
    qoz_gc_push_root(&t);
    qoz_gc_push_root(&f);
    (void)(check_synth(tc, env, c)); qoz_Ty* tt = check_synth(tc, env, t); qoz_gc_push_root(&tt); (void)(check_synth(tc, env, f)); qoz_gc_shadow_set_top(_qoz_shadow_guard); return tt;
}

qoz_Ty* check_synth_match(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* scrut, qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&scrut);
    qoz_Ty* scrut_ty = check_synth(tc, env, scrut); qoz_gc_push_root(&scrut_ty); qoz_Ty* result = ty_ty_unit_(); qoz_gc_push_root(&result); bool first = true; { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; int64_t saved_len = (env->bindings.len); check_bind_pattern(tc, env, arm.pat, scrut_ty); qoz_Ty* armt = check_synth(tc, env, arm.body); qoz_gc_push_root(&armt); check_env_truncate(env, saved_len); if (first) { result = armt; first = false; } } }check_check_match_exhaustiveness(tc, scrut, scrut_ty, arms); qoz_gc_shadow_set_top(_qoz_shadow_guard); return result;
}

void check_check_match_exhaustiveness(qoz_TyContext* tc, qoz_Expr* scrut, qoz_Ty* scrut_ty, qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&scrut);
    qoz_gc_push_root(&scrut_ty);
    qoz_string enum_name = check_enum_name_of_ty(scrut_ty); if (qoz_string_eq(enum_name, QOZ_STR_LIT(""))) { return;} qoz_Option__qoz_Decl* _qoz_ms_65 = qoz_map_get__qoz_string__qoz_Decl(&tc->enums, enum_name); switch (_qoz_ms_65->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_65->payload.Some.f0; qoz_Decl* _qoz_ms_66 = decl; switch (_qoz_ms_66->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_VariantDecl variants = _qoz_ms_66->payload.DEnum.f3; {
        qoz_CoverSet vs = ((qoz_CoverSet){ .bound = qoz_map_make__qoz_string__bool() }); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; qoz_map_set__qoz_string__bool(&vs.bound, v.name, true); } }if (check_has_catch_all_with_variants(arms, &vs)) { return;} qoz_CoverSet cs = ((qoz_CoverSet){ .bound = qoz_map_make__qoz_string__bool() }); check_collect_covered_variants(arms, &cs); qoz_Vec__qoz_string missing = qoz_vec_make__qoz_string(); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (!qoz_map_contains__qoz_string__bool(&cs.bound, v.name)) { qoz_vec_push__qoz_string(&missing, v.name); } } }if ((missing.len) > 0) { qoz_string msg = QOZ_STR_LIT("non-exhaustive match: missing"); { qoz_Vec__qoz_string __col = missing; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string m = __col.data[__i]; (void)m; msg = qoz_string_cat(msg, QOZ_STR_LIT(" ")); msg = qoz_string_cat(msg, m); } }check_record_error(tc, check_scrut_span(scrut), msg); } 
    }
    0;  break; } default: { NULL;  break; } } 0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; 
    return;
}

qoz_Span check_scrut_span(qoz_Expr* scrut) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&scrut);
    qoz_Expr* _qoz_ms_67 = scrut; qoz_Span _qoz_mv_67 = ((qoz_Span){0}); switch (_qoz_ms_67->tag) { case qoz_Expr_ENil: { qoz_Span sp = _qoz_ms_67->payload.ENil.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EInt: { qoz_Span sp = _qoz_ms_67->payload.EInt.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EFloat: { qoz_Span sp = _qoz_ms_67->payload.EFloat.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EString: { qoz_Span sp = _qoz_ms_67->payload.EString.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EChar: { qoz_Span sp = _qoz_ms_67->payload.EChar.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EBool: { qoz_Span sp = _qoz_ms_67->payload.EBool.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EIdent: { qoz_Span sp = _qoz_ms_67->payload.EIdent.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EPath: { qoz_Span sp = _qoz_ms_67->payload.EPath.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EReturn: { qoz_Span sp = _qoz_ms_67->payload.EReturn.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EDefer: { qoz_Span sp = _qoz_ms_67->payload.EDefer.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_ESizeOf: { qoz_Span sp = _qoz_ms_67->payload.ESizeOf.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_67->payload.EArrayLit.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_ETry: { qoz_Span sp = _qoz_ms_67->payload.ETry.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_ETuple: { qoz_Span sp = _qoz_ms_67->payload.ETuple.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_67->payload.EUnary.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_67->payload.EField.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EIndex: { qoz_Span sp = _qoz_ms_67->payload.EIndex.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_ECast: { qoz_Span sp = _qoz_ms_67->payload.ECast.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_ERecord: { qoz_Span sp = _qoz_ms_67->payload.ERecord.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EBlock: { qoz_Span sp = _qoz_ms_67->payload.EBlock.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EWhile: { qoz_Span sp = _qoz_ms_67->payload.EWhile.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_67->payload.EMatch.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EBinary: { qoz_Span sp = _qoz_ms_67->payload.EBinary.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EAssign: { qoz_Span sp = _qoz_ms_67->payload.EAssign.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_67->payload.ECall.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EIf: { qoz_Span sp = _qoz_ms_67->payload.EIf.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EClosure: { qoz_Span sp = _qoz_ms_67->payload.EClosure.f0; _qoz_mv_67 = (sp);  break; } case qoz_Expr_EFor: { qoz_Span sp = _qoz_ms_67->payload.EFor.f0; _qoz_mv_67 = (sp);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_67;
}

qoz_string check_enum_name_of_ty(qoz_Ty* t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&t);
    qoz_Ty* _qoz_ms_68 = t; qoz_string _qoz_mv_68 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_68->tag) { case qoz_Ty_TyAdt: { qoz_string name = _qoz_ms_68->payload.TyAdt.f0; _qoz_mv_68 = (name);  break; } default: { _qoz_mv_68 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_68;
}

bool check_has_catch_all_with_variants(qoz_Vec__qoz_MatchArm arms, qoz_CoverSet* variants) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&variants);
    { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; if (!arm.has_guard) { qoz_Pattern* _qoz_ms_69 = arm.pat; switch (_qoz_ms_69->tag) { case qoz_Pattern_PatWild: { return true; break; } case qoz_Pattern_PatBind: { qoz_string name = _qoz_ms_69->payload.PatBind.f1; if (!qoz_map_contains__qoz_string__bool(&variants->bound, name)) { return true;} 0;  break; } default: { NULL;  break; } } 0; } } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

void check_collect_covered_variants(qoz_Vec__qoz_MatchArm arms, qoz_CoverSet* cs) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&cs);
    { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; if (!arm.has_guard) { qoz_Pattern* _qoz_ms_70 = arm.pat; switch (_qoz_ms_70->tag) { case qoz_Pattern_PatVariant: { qoz_Vec__qoz_string path = _qoz_ms_70->payload.PatVariant.f1; if ((path.len) > 0) { qoz_string last = path.data[(path.len) - 1]; qoz_map_set__qoz_string__bool(&cs->bound, last, true); } 0;  break; } case qoz_Pattern_PatBind: { qoz_string name = _qoz_ms_70->payload.PatBind.f1; (qoz_map_set__qoz_string__bool(&cs->bound, name, true), 0);  break; } default: { NULL;  break; } } 0; } } }
    return;
}

void check_bind_for_loop(qoz_Env* env, qoz_string b1, qoz_string b2, qoz_Ty* it_ty) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&it_ty);
    if (qoz_string_eq(b2, QOZ_STR_LIT(""))) { qoz_Ty* _qoz_ms_71 = it_ty; switch (_qoz_ms_71->tag) { case qoz_Ty_TyRecord: { qoz_string name = _qoz_ms_71->payload.TyRecord.f0; qoz_Vec__qoz_Ty args = _qoz_ms_71->payload.TyRecord.f1; if (qoz_string_eq(name, QOZ_STR_LIT("Vec")) && ((args.len) >= 1)) { check_env_define(env, b1, args.data[0]); return;} 0;  break; } case qoz_Ty_TyPtr: { qoz_Ty* inner = _qoz_ms_71->payload.TyPtr.f0; {
        check_env_define(env, b1, inner); return;
    }
    0;  break; } default: { NULL;  break; } } 0; check_env_define(env, b1, ty_ty_int_(64, true)); }  else { qoz_Ty* _qoz_ms_72 = it_ty; switch (_qoz_ms_72->tag) { case qoz_Ty_TyRecord: { qoz_Vec__qoz_Ty args = _qoz_ms_72->payload.TyRecord.f1; if ((args.len) == 2) { check_env_define(env, b1, args.data[0]); check_env_define(env, b2, args.data[1]); return;} 0;  break; } default: { NULL;  break; } } 0; check_env_define(env, b1, ty_ty_int_(64, true)); check_env_define(env, b2, ty_ty_int_(64, true)); } 
    return;
}

void check_env_truncate(qoz_Env* env, int64_t n) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&env);
    env->bindings.len = n; 
    return;
}

void check_bind_pattern(qoz_TyContext* tc, qoz_Env* env, qoz_Pattern* pat, qoz_Ty* scrut) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&pat);
    qoz_gc_push_root(&scrut);
    qoz_Pattern* _qoz_ms_73 = pat; switch (_qoz_ms_73->tag) { case qoz_Pattern_PatWild: { NULL;  break; } case qoz_Pattern_PatLitInt: { NULL;  break; } case qoz_Pattern_PatLitString: { NULL;  break; } case qoz_Pattern_PatLitBool: { NULL;  break; } case qoz_Pattern_PatTuple: { NULL;  break; } case qoz_Pattern_PatBind: { qoz_string name = _qoz_ms_73->payload.PatBind.f1; check_env_define(env, name, scrut);  break; } case qoz_Pattern_PatVariant: { qoz_Vec__qoz_string path = _qoz_ms_73->payload.PatVariant.f1; qoz_Vec__qoz_Pattern sub_pats = _qoz_ms_73->payload.PatVariant.f2; check_bind_variant_pattern(tc, env, path, sub_pats, scrut);  break; } } 0; 
    return;
}

void check_bind_variant_pattern(qoz_TyContext* tc, qoz_Env* env, qoz_Vec__qoz_string path, qoz_Vec__qoz_Pattern sub_pats, qoz_Ty* scrut) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&scrut);
    if ((path.len) < 1) { return;} qoz_string variant = path.data[(path.len) - 1]; qoz_Option__qoz_string* _qoz_ms_74 = qoz_map_get__qoz_string__qoz_string(&tc->variant_of, variant); switch (_qoz_ms_74->tag) { case qoz_Option__qoz_string_Some: { qoz_string enum_name = _qoz_ms_74->payload.Some.f0; qoz_Option__qoz_Decl* _qoz_ms_75 = qoz_map_get__qoz_string__qoz_Decl(&tc->enums, enum_name); switch (_qoz_ms_75->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_75->payload.Some.f0; qoz_Decl* _qoz_ms_76 = d; switch (_qoz_ms_76->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string tparams = _qoz_ms_76->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_76->payload.DEnum.f3; {
        qoz_Vec__qoz_Ty tyargs = qoz_vec_make__qoz_Ty(); qoz_Ty* _qoz_ms_77 = scrut; switch (_qoz_ms_77->tag) { case qoz_Ty_TyAdt: { qoz_Vec__qoz_Ty args = _qoz_ms_77->payload.TyAdt.f1; tyargs = args;  break; } default: { NULL;  break; } } 0; { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (qoz_string_eq(v.name, variant)) { int64_t i = 0; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pos_te = __col.data[__i]; (void)pos_te; if (i < (sub_pats.len)) { qoz_Ty* pos_ty = check_resolve_type_with_subst(tc, pos_te, tparams, tyargs); qoz_gc_push_root(&pos_ty); check_bind_pattern(tc, env, sub_pats.data[i], pos_ty); } i = i + 1; } }} } }
    }
    0;  break; } default: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0; 
    return;
}

qoz_File check_infer_calls(qoz_TyContext* tc, qoz_File f) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_Vec__qoz_Decl new_decls = qoz_vec_make__qoz_Decl(); { qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_vec_push__qoz_Decl(&new_decls, check_infer_calls_decl(tc, d)); } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_File){ .path = f.path, .decls = new_decls });
}

qoz_Decl* check_infer_calls_decl(qoz_TyContext* tc, qoz_Decl* d) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_78 = d; qoz_Decl* _qoz_mv_78 = NULL; switch (_qoz_ms_78->tag) { case qoz_Decl_DFn: { qoz_Span sp = _qoz_ms_78->payload.DFn.f0; qoz_string name = _qoz_ms_78->payload.DFn.f1; qoz_Vec__qoz_string tparams = _qoz_ms_78->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_78->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_78->payload.DFn.f4; qoz_Expr* body = _qoz_ms_78->payload.DFn.f5; qoz_string attr = _qoz_ms_78->payload.DFn.f6; qoz_Decl* _qoz_bv_69;
    {
        check_set_type_params(tc, tparams); qoz_Env env = check_env_make(); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; check_env_define(&env, pp.name, check_resolve_type(tc, pp.ty)); } }qoz_Ty* ret_ty = check_resolve_type(tc, ret); qoz_gc_push_root(&ret_ty); qoz_Expr* new_body = check_infer_calls_expr(tc, &env, body, ret_ty); qoz_gc_push_root(&new_body); check_clear_type_params(tc); _qoz_bv_69 = qoz_make_Decl_DFn(sp, name, tparams, params, ret, new_body, attr);
    }
    _qoz_mv_78 = (_qoz_bv_69);  break; } case qoz_Decl_DConst: { qoz_Span sp = _qoz_ms_78->payload.DConst.f0; qoz_string name = _qoz_ms_78->payload.DConst.f1; qoz_TypeExpr* t = _qoz_ms_78->payload.DConst.f2; qoz_Expr* val = _qoz_ms_78->payload.DConst.f3; qoz_Decl* _qoz_bv_70;
    {
        qoz_Env env = check_env_make(); qoz_Ty* expected = check_resolve_type(tc, t); qoz_gc_push_root(&expected); qoz_Expr* new_val = check_infer_calls_expr(tc, &env, val, expected); qoz_gc_push_root(&new_val); _qoz_bv_70 = qoz_make_Decl_DConst(sp, name, t, new_val);
    }
    _qoz_mv_78 = (_qoz_bv_70);  break; } default: { _qoz_mv_78 = (d);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_78;
}

qoz_Expr* check_infer_calls_expr(qoz_TyContext* tc, qoz_Env* env, qoz_Expr* e, qoz_Ty* expected) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&expected);
    qoz_Expr* _qoz_ms_79 = e; qoz_Expr* _qoz_mv_79 = NULL; switch (_qoz_ms_79->tag) { case qoz_Expr_EInt: { _qoz_mv_79 = (e);  break; } case qoz_Expr_EFloat: { _qoz_mv_79 = (e);  break; } case qoz_Expr_EString: { _qoz_mv_79 = (e);  break; } case qoz_Expr_EChar: { _qoz_mv_79 = (e);  break; } case qoz_Expr_EBool: { _qoz_mv_79 = (e);  break; } case qoz_Expr_ENil: { _qoz_mv_79 = (e);  break; } case qoz_Expr_EIdent: { _qoz_mv_79 = (e);  break; } case qoz_Expr_EPath: { _qoz_mv_79 = (e);  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_79->payload.EUnary.f0; qoz_UnaryOp* op = _qoz_ms_79->payload.EUnary.f1; qoz_Expr* rhs = _qoz_ms_79->payload.EUnary.f2; _qoz_mv_79 = (qoz_make_Expr_EUnary(sp, op, check_infer_calls_expr(tc, env, rhs, ty_ty_error_())));  break; } case qoz_Expr_EBinary: { qoz_Span sp = _qoz_ms_79->payload.EBinary.f0; qoz_BinaryOp* op = _qoz_ms_79->payload.EBinary.f1; qoz_Expr* l = _qoz_ms_79->payload.EBinary.f2; qoz_Expr* r = _qoz_ms_79->payload.EBinary.f3; _qoz_mv_79 = (qoz_make_Expr_EBinary(sp, op, check_infer_calls_expr(tc, env, l, ty_ty_error_()), check_infer_calls_expr(tc, env, r, ty_ty_error_())));  break; } case qoz_Expr_EAssign: { qoz_Span sp = _qoz_ms_79->payload.EAssign.f0; qoz_AssignOp* op = _qoz_ms_79->payload.EAssign.f1; qoz_Expr* l = _qoz_ms_79->payload.EAssign.f2; qoz_Expr* r = _qoz_ms_79->payload.EAssign.f3; qoz_Expr* _qoz_bv_71;
    {
        qoz_Expr* new_l = check_infer_calls_expr(tc, env, l, ty_ty_error_()); qoz_gc_push_root(&new_l); qoz_Ty* lhs_ty = check_synth(tc, env, l); qoz_gc_push_root(&lhs_ty); _qoz_bv_71 = qoz_make_Expr_EAssign(sp, op, new_l, check_infer_calls_expr(tc, env, r, lhs_ty));
    }
    _qoz_mv_79 = (_qoz_bv_71);  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_79->payload.ECall.f0; qoz_Expr* callee = _qoz_ms_79->payload.ECall.f1; qoz_Vec__qoz_TypeExpr ta = _qoz_ms_79->payload.ECall.f2; qoz_Vec__qoz_Expr args = _qoz_ms_79->payload.ECall.f3; _qoz_mv_79 = (check_rewrite_call(tc, env, sp, callee, ta, args, expected));  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_79->payload.EField.f0; qoz_Expr* base = _qoz_ms_79->payload.EField.f1; qoz_string name = _qoz_ms_79->payload.EField.f2; _qoz_mv_79 = (qoz_make_Expr_EField(sp, check_infer_calls_expr(tc, env, base, ty_ty_error_()), name));  break; } case qoz_Expr_EIndex: { qoz_Span sp = _qoz_ms_79->payload.EIndex.f0; qoz_Expr* base = _qoz_ms_79->payload.EIndex.f1; qoz_Expr* idx = _qoz_ms_79->payload.EIndex.f2; _qoz_mv_79 = (qoz_make_Expr_EIndex(sp, check_infer_calls_expr(tc, env, base, ty_ty_error_()), check_infer_calls_expr(tc, env, idx, ty_ty_error_())));  break; } case qoz_Expr_ECast: { qoz_Span sp = _qoz_ms_79->payload.ECast.f0; qoz_Expr* value = _qoz_ms_79->payload.ECast.f1; qoz_TypeExpr* t = _qoz_ms_79->payload.ECast.f2; _qoz_mv_79 = (qoz_make_Expr_ECast(sp, check_infer_calls_expr(tc, env, value, ty_ty_error_()), t));  break; } case qoz_Expr_ETry: { qoz_Span sp = _qoz_ms_79->payload.ETry.f0; qoz_Expr* v = _qoz_ms_79->payload.ETry.f1; _qoz_mv_79 = (qoz_make_Expr_ETry(sp, check_infer_calls_expr(tc, env, v, expected)));  break; } case qoz_Expr_ETuple: { qoz_Span sp = _qoz_ms_79->payload.ETuple.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_79->payload.ETuple.f1; qoz_Expr* _qoz_bv_72;
    {
        qoz_Vec__qoz_Expr ne = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_Expr(&ne, check_infer_calls_expr(tc, env, el, ty_ty_error_())); } }_qoz_bv_72 = qoz_make_Expr_ETuple(sp, ne);
    }
    _qoz_mv_79 = (_qoz_bv_72);  break; } case qoz_Expr_ERecord: { qoz_Span sp = _qoz_ms_79->payload.ERecord.f0; qoz_TypeExpr* te = _qoz_ms_79->payload.ERecord.f1; qoz_Vec__qoz_RecordFieldLit fields = _qoz_ms_79->payload.ERecord.f2; qoz_Expr* _qoz_bv_73;
    {
        qoz_TypeExpr* effective_te = check_pick_record_type(te, expected); qoz_gc_push_root(&effective_te); qoz_Vec__qoz_RecordFieldLit nf = qoz_vec_make__qoz_RecordFieldLit(); { qoz_Vec__qoz_RecordFieldLit __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_RecordFieldLit f = __col.data[__i]; (void)f; qoz_Ty* field_hint = check_record_field_hint(tc, effective_te, f.name); qoz_gc_push_root(&field_hint); qoz_vec_push__qoz_RecordFieldLit(&nf, ((qoz_RecordFieldLit){ .name = f.name, .value = check_infer_calls_expr(tc, env, f.value, field_hint) })); } }_qoz_bv_73 = qoz_make_Expr_ERecord(sp, te, nf);
    }
    _qoz_mv_79 = (_qoz_bv_73);  break; } case qoz_Expr_EClosure: { qoz_Span sp = _qoz_ms_79->payload.EClosure.f0; qoz_Vec__qoz_ClosureParam cps = _qoz_ms_79->payload.EClosure.f1; qoz_TypeExpr* ret = _qoz_ms_79->payload.EClosure.f2; qoz_Expr* body = _qoz_ms_79->payload.EClosure.f3; _qoz_mv_79 = (qoz_make_Expr_EClosure(sp, cps, ret, check_infer_calls_expr(tc, env, body, ty_ty_error_())));  break; } case qoz_Expr_EBlock: { qoz_Span sp = _qoz_ms_79->payload.EBlock.f0; qoz_Vec__qoz_Stmt stmts = _qoz_ms_79->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_79->payload.EBlock.f2; qoz_Expr* _qoz_bv_74;
    {
        qoz_Vec__qoz_Stmt ns = qoz_vec_make__qoz_Stmt(); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; qoz_vec_push__qoz_Stmt(&ns, check_infer_calls_stmt(tc, env, s)); } }_qoz_bv_74 = qoz_make_Expr_EBlock(sp, ns, check_infer_calls_expr(tc, env, tail, expected));
    }
    _qoz_mv_79 = (_qoz_bv_74);  break; } case qoz_Expr_EIf: { qoz_Span sp = _qoz_ms_79->payload.EIf.f0; qoz_Expr* c = _qoz_ms_79->payload.EIf.f1; qoz_Expr* t = _qoz_ms_79->payload.EIf.f2; qoz_Expr* f = _qoz_ms_79->payload.EIf.f3; _qoz_mv_79 = (qoz_make_Expr_EIf(sp, check_infer_calls_expr(tc, env, c, ty_ty_error_()), check_infer_calls_expr(tc, env, t, expected), check_infer_calls_expr(tc, env, f, expected)));  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_79->payload.EMatch.f0; qoz_Expr* scrut = _qoz_ms_79->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_79->payload.EMatch.f2; qoz_Expr* _qoz_bv_75;
    {
        qoz_Expr* new_scrut = check_infer_calls_expr(tc, env, scrut, ty_ty_error_()); qoz_gc_push_root(&new_scrut); qoz_Ty* scrut_ty = check_synth(tc, env, scrut); qoz_gc_push_root(&scrut_ty); qoz_Vec__qoz_MatchArm na = qoz_vec_make__qoz_MatchArm(); { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm a = __col.data[__i]; (void)a; int64_t saved_len = (env->bindings.len); check_bind_pattern(tc, env, a.pat, scrut_ty); qoz_vec_push__qoz_MatchArm(&na, ((qoz_MatchArm){ .pat = a.pat, .body = check_infer_calls_expr(tc, env, a.body, expected), .has_guard = a.has_guard, .guard = a.guard })); check_env_truncate(env, saved_len); } }_qoz_bv_75 = qoz_make_Expr_EMatch(sp, new_scrut, na);
    }
    _qoz_mv_79 = (_qoz_bv_75);  break; } case qoz_Expr_EWhile: { qoz_Span sp = _qoz_ms_79->payload.EWhile.f0; qoz_Expr* c = _qoz_ms_79->payload.EWhile.f1; qoz_Expr* b = _qoz_ms_79->payload.EWhile.f2; _qoz_mv_79 = (qoz_make_Expr_EWhile(sp, check_infer_calls_expr(tc, env, c, ty_ty_error_()), check_infer_calls_expr(tc, env, b, ty_ty_error_())));  break; } case qoz_Expr_EFor: { qoz_Span sp = _qoz_ms_79->payload.EFor.f0; qoz_string b1 = _qoz_ms_79->payload.EFor.f1; qoz_string b2 = _qoz_ms_79->payload.EFor.f2; qoz_Expr* it = _qoz_ms_79->payload.EFor.f3; qoz_Expr* body = _qoz_ms_79->payload.EFor.f4; qoz_Expr* _qoz_bv_76;
    {
        qoz_Expr* new_it = check_infer_calls_expr(tc, env, it, ty_ty_error_()); qoz_gc_push_root(&new_it); qoz_Ty* it_ty = check_synth(tc, env, it); qoz_gc_push_root(&it_ty); int64_t saved_len = (env->bindings.len); check_bind_for_loop(env, b1, b2, it_ty); qoz_Expr* new_body = check_infer_calls_expr(tc, env, body, ty_ty_error_()); qoz_gc_push_root(&new_body); check_env_truncate(env, saved_len); _qoz_bv_76 = qoz_make_Expr_EFor(sp, b1, b2, new_it, new_body);
    }
    _qoz_mv_79 = (_qoz_bv_76);  break; } case qoz_Expr_EReturn: { qoz_Span sp = _qoz_ms_79->payload.EReturn.f0; qoz_Expr* v = _qoz_ms_79->payload.EReturn.f1; _qoz_mv_79 = (qoz_make_Expr_EReturn(sp, check_infer_calls_expr(tc, env, v, expected)));  break; } case qoz_Expr_EDefer: { qoz_Span sp = _qoz_ms_79->payload.EDefer.f0; qoz_Expr* v = _qoz_ms_79->payload.EDefer.f1; _qoz_mv_79 = (qoz_make_Expr_EDefer(sp, check_infer_calls_expr(tc, env, v, ty_ty_error_())));  break; } case qoz_Expr_ESizeOf: { _qoz_mv_79 = (e);  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_79->payload.EArrayLit.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_79->payload.EArrayLit.f1; qoz_Expr* _qoz_bv_77;
    {
        qoz_Vec__qoz_Expr ne = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_Expr(&ne, check_infer_calls_expr(tc, env, el, ty_ty_error_())); } }_qoz_bv_77 = qoz_make_Expr_EArrayLit(sp, ne);
    }
    _qoz_mv_79 = (_qoz_bv_77);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_79;
}

qoz_Stmt* check_infer_calls_stmt(qoz_TyContext* tc, qoz_Env* env, qoz_Stmt* s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&s);
    qoz_Stmt* _qoz_ms_80 = s; qoz_Stmt* _qoz_mv_80 = NULL; switch (_qoz_ms_80->tag) { case qoz_Stmt_SLet: { qoz_Span sp = _qoz_ms_80->payload.SLet.f0; qoz_string name = _qoz_ms_80->payload.SLet.f1; qoz_TypeExpr* te = _qoz_ms_80->payload.SLet.f2; qoz_Expr* val = _qoz_ms_80->payload.SLet.f3; qoz_Stmt* _qoz_bv_78;
    {
        qoz_Ty* annotated = check_resolve_type(tc, te); qoz_gc_push_root(&annotated); qoz_Expr* new_val = check_infer_calls_expr(tc, env, val, annotated); qoz_gc_push_root(&new_val); qoz_Ty* binding_ty = ((check_is_ty_unit_or_error(annotated)) ? check_synth(tc, env, new_val) : annotated); qoz_gc_push_root(&binding_ty); check_env_define(env, name, binding_ty); _qoz_bv_78 = qoz_make_Stmt_SLet(sp, name, te, new_val);
    }
    _qoz_mv_80 = (_qoz_bv_78);  break; } case qoz_Stmt_SVar: { qoz_Span sp = _qoz_ms_80->payload.SVar.f0; qoz_string name = _qoz_ms_80->payload.SVar.f1; qoz_TypeExpr* te = _qoz_ms_80->payload.SVar.f2; qoz_Expr* val = _qoz_ms_80->payload.SVar.f3; qoz_Stmt* _qoz_bv_79;
    {
        qoz_Ty* annotated = check_resolve_type(tc, te); qoz_gc_push_root(&annotated); qoz_Expr* new_val = check_infer_calls_expr(tc, env, val, annotated); qoz_gc_push_root(&new_val); qoz_Ty* binding_ty = ((check_is_ty_unit_or_error(annotated)) ? check_synth(tc, env, new_val) : annotated); qoz_gc_push_root(&binding_ty); check_env_define(env, name, binding_ty); _qoz_bv_79 = qoz_make_Stmt_SVar(sp, name, te, new_val);
    }
    _qoz_mv_80 = (_qoz_bv_79);  break; } case qoz_Stmt_SExpr: { qoz_Span sp = _qoz_ms_80->payload.SExpr.f0; qoz_Expr* x = _qoz_ms_80->payload.SExpr.f1; _qoz_mv_80 = (qoz_make_Stmt_SExpr(sp, check_infer_calls_expr(tc, env, x, ty_ty_error_())));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_80;
}

qoz_TypeExpr* check_pick_record_type(qoz_TypeExpr* te, qoz_Ty* expected) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&te);
    qoz_gc_push_root(&expected);
    qoz_TypeExpr* _qoz_ms_81 = te; qoz_TypeExpr* _qoz_mv_81 = NULL; switch (_qoz_ms_81->tag) { case qoz_TypeExpr_TENamed: { qoz_Span sp = _qoz_ms_81->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_81->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_81->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_80;
    {
        if ((args.len) > 0) { return te;} if ((path.len) != 1) { return te;} qoz_Ty* _qoz_ms_82 = expected; qoz_TypeExpr* _qoz_mv_82 = NULL; switch (_qoz_ms_82->tag) { case qoz_Ty_TyRecord: { qoz_string en = _qoz_ms_82->payload.TyRecord.f0; qoz_Vec__qoz_Ty eargs = _qoz_ms_82->payload.TyRecord.f1; qoz_TypeExpr* _qoz_bv_81;
    {
        if (qoz_string_eq(path.data[0], en) && ((eargs.len) > 0)) { qoz_Vec__qoz_TypeExpr ta = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_Ty __col = eargs; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_TypeExpr(&ta, ty_ty_to_type_expr_at(a, sp)); } }return qoz_make_TypeExpr_TENamed(sp, path, ta);} _qoz_bv_81 = te;
    }
    _qoz_mv_82 = (_qoz_bv_81);  break; } default: { _qoz_mv_82 = (te);  break; } } _qoz_bv_80 = _qoz_mv_82;
    }
    _qoz_mv_81 = (_qoz_bv_80);  break; } default: { _qoz_mv_81 = (te);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_81;
}

qoz_Ty* check_record_field_hint(qoz_TyContext* tc, qoz_TypeExpr* te, qoz_string field_name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_83 = te; qoz_Ty* _qoz_mv_83 = NULL; switch (_qoz_ms_83->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_83->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_83->payload.TENamed.f2; qoz_Ty* _qoz_bv_82;
    {
        if ((path.len) < 1) { return ty_ty_error_();} qoz_string name = path.data[(path.len) - 1]; qoz_Option__qoz_Decl* _qoz_ms_84 = qoz_map_get__qoz_string__qoz_Decl(&tc->structs, name); qoz_Ty* _qoz_mv_84 = NULL; switch (_qoz_ms_84->tag) { case qoz_Option__qoz_Decl_None: { _qoz_mv_84 = (ty_ty_error_());  break; } case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_84->payload.Some.f0; qoz_Decl* _qoz_ms_85 = d; qoz_Ty* _qoz_mv_85 = NULL; switch (_qoz_ms_85->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_85->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_85->payload.DStruct.f3; qoz_Ty* _qoz_bv_83;
    {
        qoz_Map__qoz_string__bool saved = tc->type_params; check_set_type_params(tc, params); qoz_Vec__qoz_Ty arg_tys = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_TypeExpr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_Ty(&arg_tys, check_resolve_type(tc, a)); } }qoz_Ty* result = ty_ty_error_(); qoz_gc_push_root(&result); { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; if (qoz_string_eq(f.name, field_name)) { qoz_Ty* raw = check_resolve_type(tc, f.ty); qoz_gc_push_root(&raw); result = ((((arg_tys.len) == (params.len)) && ((params.len) > 0)) ? check_apply_subst(raw, params, arg_tys) : raw); } } }tc->type_params = saved; _qoz_bv_83 = result;
    }
    _qoz_mv_85 = (_qoz_bv_83);  break; } default: { _qoz_mv_85 = (ty_ty_error_());  break; } } _qoz_mv_84 = (_qoz_mv_85);  break; } } _qoz_bv_82 = _qoz_mv_84;
    }
    _qoz_mv_83 = (_qoz_bv_82);  break; } default: { _qoz_mv_83 = (ty_ty_error_());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_83;
}

void check_collect_arg_hints(qoz_TyContext* tc, qoz_Expr* callee, int64_t n, qoz_Vec__qoz_Ty* out) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&callee);
    qoz_gc_push_root(&out);
    qoz_string name = check_resolve_callee_fn(tc, callee); if (!qoz_string_eq(name, QOZ_STR_LIT(""))) { qoz_Option__qoz_Decl* _qoz_ms_86 = qoz_map_get__qoz_string__qoz_Decl(&tc->fns, name); switch (_qoz_ms_86->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_86->payload.Some.f0; {
        check_fill_fn_param_hints(tc, d, out); return;
    }
    0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; qoz_Option__qoz_Decl* _qoz_ms_87 = qoz_map_get__qoz_string__qoz_Decl(&tc->externs, name); switch (_qoz_ms_87->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_87->payload.Some.f0; {
        check_fill_fn_param_hints(tc, d, out); return;
    }
    0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; } qoz_Expr* _qoz_ms_88 = callee; switch (_qoz_ms_88->tag) { case qoz_Expr_EIdent: { qoz_string vname = _qoz_ms_88->payload.EIdent.f1; qoz_Option__qoz_string* _qoz_ms_89 = qoz_map_get__qoz_string__qoz_string(&tc->variant_of, vname); switch (_qoz_ms_89->tag) { case qoz_Option__qoz_string_Some: { qoz_string enum_name = _qoz_ms_89->payload.Some.f0; qoz_Option__qoz_Decl* _qoz_ms_90 = qoz_map_get__qoz_string__qoz_Decl(&tc->enums, enum_name); switch (_qoz_ms_90->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_90->payload.Some.f0; qoz_Decl* _qoz_ms_91 = d; switch (_qoz_ms_91->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string tparams = _qoz_ms_91->payload.DEnum.f2; if ((tparams.len) == 0) { check_fill_variant_pos_hints(tc, d, vname, out); } 0;  break; } default: { NULL;  break; } } 0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0;  break; } case qoz_Option__qoz_string_None: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0; 
    return;
}

void check_fill_fn_param_hints(qoz_TyContext* tc, qoz_Decl* d, qoz_Vec__qoz_Ty* out) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&d);
    qoz_gc_push_root(&out);
    qoz_Decl* _qoz_ms_92 = d; switch (_qoz_ms_92->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string tparams = _qoz_ms_92->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_92->payload.DFn.f3; {
        qoz_Map__qoz_string__bool saved = tc->type_params; check_set_type_params(tc, tparams); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; qoz_vec_push__qoz_Ty(out, check_resolve_type(tc, pp.ty)); } }tc->type_params = saved; 
    }
    0;  break; } case qoz_Decl_DExternal: { qoz_Vec__qoz_FnParam params = _qoz_ms_92->payload.DExternal.f3; { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; qoz_vec_push__qoz_Ty(out, check_resolve_type(tc, pp.ty)); } }0;  break; } default: { NULL;  break; } } 0; 
    return;
}

void check_fill_variant_pos_hints(qoz_TyContext* tc, qoz_Decl* d, qoz_string variant_name, qoz_Vec__qoz_Ty* out) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&d);
    qoz_gc_push_root(&out);
    qoz_Decl* _qoz_ms_93 = d; switch (_qoz_ms_93->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string tparams = _qoz_ms_93->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_93->payload.DEnum.f3; {
        qoz_Map__qoz_string__bool saved = tc->type_params; check_set_type_params(tc, tparams); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (qoz_string_eq(v.name, variant_name)) { { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pt = __col.data[__i]; (void)pt; qoz_vec_push__qoz_Ty(out, check_resolve_type(tc, pt)); } }} } }tc->type_params = saved; 
    }
    0;  break; } default: { NULL;  break; } } 0; 
    return;
}

bool check_is_ty_unit_or_error(qoz_Ty* t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&t);
    qoz_Ty* _qoz_ms_94 = t; bool _qoz_mv_94 = false; switch (_qoz_ms_94->tag) { case qoz_Ty_TyUnit: { _qoz_mv_94 = (true);  break; } case qoz_Ty_TyError: { _qoz_mv_94 = (true);  break; } default: { _qoz_mv_94 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_94;
}

qoz_Expr* check_rewrite_call(qoz_TyContext* tc, qoz_Env* env, qoz_Span sp, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args, qoz_Ty* expected) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    qoz_gc_push_root(&env);
    qoz_gc_push_root(&callee);
    qoz_gc_push_root(&expected);
    if (check_is_strings_callee(callee, QOZ_STR_LIT("sb_append")) && ((args.len) == 2)) { qoz_Ty* val_ty = check_synth(tc, env, args.data[1]); qoz_gc_push_root(&val_ty); qoz_string method = check_sb_append_method_for(val_ty); if (!qoz_string_eq(method, QOZ_STR_LIT("sb_append"))) { qoz_Expr* new_callee2 = qoz_make_Expr_EField(sp, qoz_make_Expr_EIdent(sp, QOZ_STR_LIT("strings")), method); qoz_gc_push_root(&new_callee2); qoz_Vec__qoz_Expr new_args = qoz_vec_make__qoz_Expr(); qoz_vec_push__qoz_Expr(&new_args, check_infer_calls_expr(tc, env, args.data[0], ty_ty_error_())); qoz_vec_push__qoz_Expr(&new_args, check_infer_calls_expr(tc, env, args.data[1], val_ty)); return qoz_make_Expr_ECall(sp, new_callee2, type_args, new_args);} } qoz_Expr* new_callee = check_infer_calls_expr(tc, env, callee, ty_ty_error_()); qoz_gc_push_root(&new_callee); qoz_Vec__qoz_Expr new_args = qoz_vec_make__qoz_Expr(); qoz_Vec__qoz_Ty arg_hints = qoz_vec_make__qoz_Ty(); check_collect_arg_hints(tc, callee, (args.len), &arg_hints); int64_t ai = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; qoz_Ty* hint = ((ai < (arg_hints.len)) ? arg_hints.data[ai] : ty_ty_error_()); qoz_gc_push_root(&hint); qoz_vec_push__qoz_Expr(&new_args, check_infer_calls_expr(tc, env, a, hint)); ai = ai + 1; } }if ((type_args.len) > 0) { return qoz_make_Expr_ECall(sp, new_callee, type_args, new_args);} qoz_string fn_name = check_resolve_callee_fn(tc, callee); if (qoz_string_eq(fn_name, QOZ_STR_LIT(""))) { return qoz_make_Expr_ECall(sp, new_callee, type_args, new_args);} qoz_Option__qoz_Decl* _qoz_ms_95 = qoz_map_get__qoz_string__qoz_Decl(&tc->fns, fn_name); qoz_Expr* _qoz_mv_95 = NULL; switch (_qoz_ms_95->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_95->payload.Some.f0; qoz_Decl* _qoz_ms_96 = d; qoz_Expr* _qoz_mv_96 = NULL; switch (_qoz_ms_96->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string tparams = _qoz_ms_96->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_96->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_96->payload.DFn.f4; qoz_Expr* _qoz_bv_84;
    {
        if ((tparams.len) == 0) { return qoz_make_Expr_ECall(sp, new_callee, type_args, new_args);} qoz_Vec__qoz_Ty arg_tys = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_Ty(&arg_tys, check_synth(tc, env, a)); } }qoz_Map__qoz_string__bool saved_c = tc->type_params; check_set_type_params(tc, tparams); qoz_Vec__qoz_Ty param_tys = qoz_vec_make__qoz_Ty(); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; qoz_vec_push__qoz_Ty(&param_tys, check_resolve_type(tc, pp.ty)); } }qoz_Ty* ret_pattern = check_resolve_type(tc, ret); qoz_gc_push_root(&ret_pattern); tc->type_params = saved_c; qoz_Map__qoz_string__qoz_Ty subst_env = qoz_map_make__qoz_string__qoz_Ty(); int64_t i = 0; bool ok = true; while ((i < (arg_tys.len)) && (i < (param_tys.len))) { if (!check_unify(param_tys.data[i], arg_tys.data[i], &subst_env)) { ok = false; } i = i + 1; } if (!check_is_ty_error(expected)) { (void)(check_unify(ret_pattern, expected, &subst_env)); } if (!ok) { qoz_string _qoz_bv_85;
    {
        qoz_Strbuf _qoz_sb_1241_38 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1241_38); strings_sb_append(&_qoz_sb_1241_38, QOZ_STR_LIT("could not unify argument types with generic parameters of `")); strings_sb_append(&_qoz_sb_1241_38, fn_name); strings_sb_append(&_qoz_sb_1241_38, QOZ_STR_LIT("`")); _qoz_bv_85 = strings_sb_finish(&_qoz_sb_1241_38);
    }
    check_record_error(tc, sp, _qoz_bv_85); return qoz_make_Expr_ECall(sp, new_callee, type_args, new_args);} qoz_Vec__qoz_TypeExpr inferred = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_string __col = tparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string tp = __col.data[__i]; (void)tp; qoz_Option__qoz_Ty* _qoz_ms_97 = qoz_map_get__qoz_string__qoz_Ty(&subst_env, tp); switch (_qoz_ms_97->tag) { case qoz_Option__qoz_Ty_Some: { qoz_Ty* t = _qoz_ms_97->payload.Some.f0; qoz_vec_push__qoz_TypeExpr(&inferred, ty_ty_to_type_expr_at(t, sp));  break; } case qoz_Option__qoz_Ty_None: { {
        qoz_string _qoz_bv_86;
    {
        qoz_Strbuf _qoz_sb_1249_42 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1249_42); strings_sb_append(&_qoz_sb_1249_42, QOZ_STR_LIT("type parameter `")); strings_sb_append(&_qoz_sb_1249_42, tp); strings_sb_append(&_qoz_sb_1249_42, QOZ_STR_LIT("` of `")); strings_sb_append(&_qoz_sb_1249_42, fn_name); strings_sb_append(&_qoz_sb_1249_42, QOZ_STR_LIT("` is not constrained; add an explicit annotation")); _qoz_bv_86 = strings_sb_finish(&_qoz_sb_1249_42);
    }
    check_record_error(tc, sp, _qoz_bv_86); qoz_vec_push__qoz_TypeExpr(&inferred, ty_ty_to_type_expr_at(ty_ty_error_(), sp)); 
    }
    0;  break; } } 0; } }_qoz_bv_84 = qoz_make_Expr_ECall(sp, new_callee, inferred, new_args);
    }
    _qoz_mv_96 = (_qoz_bv_84);  break; } default: { _qoz_mv_96 = (qoz_make_Expr_ECall(sp, new_callee, type_args, new_args));  break; } } _qoz_mv_95 = (_qoz_mv_96);  break; } case qoz_Option__qoz_Decl_None: { _qoz_mv_95 = (qoz_make_Expr_ECall(sp, new_callee, type_args, new_args));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_95;
}

void check_check_fn_bodies(qoz_TyContext* tc, qoz_File f) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    { qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_98 = d; switch (_qoz_ms_98->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string tparams = _qoz_ms_98->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_98->payload.DFn.f3; qoz_Expr* body = _qoz_ms_98->payload.DFn.f5; {
        check_set_type_params(tc, tparams); qoz_Env env = check_env_make(); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; check_env_define(&env, pp.name, check_resolve_type(tc, pp.ty)); } }(void)(check_synth(tc, &env, body)); check_clear_type_params(tc); 
    }
    0;  break; } case qoz_Decl_DImport: { NULL;  break; } case qoz_Decl_DStruct: { NULL;  break; } case qoz_Decl_DEnum: { NULL;  break; } case qoz_Decl_DTypeAlias: { NULL;  break; } case qoz_Decl_DConst: { qoz_Expr* val = _qoz_ms_98->payload.DConst.f3; void* _qoz_bv_87;
    {
        qoz_Env env = check_env_make(); (void)(check_synth(tc, &env, val)); _qoz_bv_87 = NULL;
    }
    _qoz_bv_87;  break; } case qoz_Decl_DExternal: { NULL;  break; } case qoz_Decl_DLink: { NULL;  break; } } 0; } }
    return;
}

void check_set_type_params(qoz_TyContext* tc, qoz_Vec__qoz_string params) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    tc->type_params = qoz_map_make__qoz_string__bool(); { qoz_Vec__qoz_string __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string p = __col.data[__i]; (void)p; qoz_map_set__qoz_string__bool(&tc->type_params, p, true); } }
    return;
}

void check_clear_type_params(qoz_TyContext* tc) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    tc->type_params = qoz_map_make__qoz_string__bool(); 
    return;
}

void check_validate_signatures(qoz_TyContext* tc, qoz_File f) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&tc);
    { qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_99 = d; switch (_qoz_ms_99->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string tparams = _qoz_ms_99->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_99->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_99->payload.DFn.f4; {
        check_set_type_params(tc, tparams); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; (void)(check_resolve_type(tc, pp.ty)); } }(void)(check_resolve_type(tc, ret)); check_clear_type_params(tc); 
    }
    0;  break; } case qoz_Decl_DExternal: { qoz_Vec__qoz_FnParam params = _qoz_ms_99->payload.DExternal.f3; qoz_TypeExpr* ret = _qoz_ms_99->payload.DExternal.f4; void* _qoz_bv_88;
    {
        { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; (void)(check_resolve_type(tc, pp.ty)); } }(void)(check_resolve_type(tc, ret)); _qoz_bv_88 = NULL;
    }
    _qoz_bv_88;  break; } case qoz_Decl_DStruct: { qoz_Vec__qoz_string tparams = _qoz_ms_99->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_99->payload.DStruct.f3; {
        check_set_type_params(tc, tparams); { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField fld = __col.data[__i]; (void)fld; (void)(check_resolve_type(tc, fld.ty)); } }check_clear_type_params(tc); 
    }
    0;  break; } case qoz_Decl_DEnum: { qoz_Vec__qoz_string tparams = _qoz_ms_99->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_99->payload.DEnum.f3; {
        check_set_type_params(tc, tparams); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pp = __col.data[__i]; (void)pp; (void)(check_resolve_type(tc, pp)); } }{ qoz_Vec__qoz_StructField __col = v.named; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField nf = __col.data[__i]; (void)nf; (void)(check_resolve_type(tc, nf.ty)); } }} }check_clear_type_params(tc); 
    }
    0;  break; } case qoz_Decl_DTypeAlias: { qoz_Vec__qoz_string tparams = _qoz_ms_99->payload.DTypeAlias.f2; qoz_TypeExpr* target = _qoz_ms_99->payload.DTypeAlias.f3; {
        check_set_type_params(tc, tparams); (void)(check_resolve_type(tc, target)); check_clear_type_params(tc); 
    }
    0;  break; } case qoz_Decl_DConst: { qoz_TypeExpr* ty = _qoz_ms_99->payload.DConst.f2; void* _qoz_bv_89;
    {
        (void)(check_resolve_type(tc, ty)); _qoz_bv_89 = NULL;
    }
    _qoz_bv_89;  break; } case qoz_Decl_DImport: { NULL;  break; } case qoz_Decl_DLink: { NULL;  break; } } 0; } }
    return;
}

qoz_Emitter emit_make_emitter(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Emitter){ .out = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }), .expr_types = qoz_map_make__int64_t__qoz_TypeExpr(), .current_tparams = qoz_vec_make__qoz_string(), .current_targs = qoz_vec_make__qoz_TypeExpr(), .pos_fn_typedefs = -1, .pos_tuple_typedefs = -1, .pos_synth_fn_decls = -1, .variant_of = qoz_map_make__qoz_string__qoz_string(), .is_enum = qoz_map_make__qoz_string__bool(), .enum_decls = qoz_map_make__qoz_string__qoz_Decl(), .struct_decls = qoz_map_make__qoz_string__qoz_Decl(), .generic_decls = qoz_map_make__qoz_string__qoz_Decl(), .generic_fn_decls = qoz_map_make__qoz_string__qoz_Decl(), .packages = qoz_map_make__qoz_string__bool(), .record_insts = qoz_vec_make__qoz_Instantiation(), .record_seen = qoz_map_make__qoz_string__bool(), .enum_insts = qoz_vec_make__qoz_Instantiation(), .enum_seen = qoz_map_make__qoz_string__bool(), .fn_insts = qoz_vec_make__qoz_Instantiation(), .fn_seen = qoz_map_make__qoz_string__bool(), .locals = qoz_map_make__qoz_string__qoz_TypeExpr(), .externs = qoz_map_make__qoz_string__qoz_string(), .fn_returns = qoz_map_make__qoz_string__qoz_TypeExpr(), .fn_params = qoz_map_make__qoz_string__qoz_Vec__qoz_TypeExpr(), .match_counter = 0, .current_ret_te = qoz_make_TypeExpr_TEUnit(((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 })), .match_hint = qoz_make_TypeExpr_TEUnit(((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 })), .fn_typedefs = qoz_map_make__qoz_string__qoz_string(), .op_dispatch = qoz_map_make__qoz_string__qoz_string(), .byval_helpers_emitted = qoz_map_make__qoz_string__bool(), .pending_prologue = qoz_vec_make__qoz_string(), .current_return_restore = QOZ_STR_LIT(""), .tuple_typedefs = qoz_map_make__qoz_string__qoz_string(), .tuple_typedef_order = qoz_vec_make__qoz_string(), .fn_typedef_order = qoz_vec_make__qoz_string(), .closure_counter = 0, .synth_fn_defs = qoz_vec_make__qoz_string(), .synth_fn_decls = qoz_vec_make__qoz_string() });
}

void emit_register_generics(qoz_Emitter* e, qoz_File file) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    { qoz_Vec__qoz_Decl __col = file.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_100 = d; switch (_qoz_ms_100->tag) { case qoz_Decl_DStruct: { qoz_string name = _qoz_ms_100->payload.DStruct.f1; qoz_Vec__qoz_string params = _qoz_ms_100->payload.DStruct.f2; if ((params.len) > 0) { qoz_map_set__qoz_string__qoz_Decl(&e->generic_decls, name, d); } 0;  break; } case qoz_Decl_DEnum: { qoz_string name = _qoz_ms_100->payload.DEnum.f1; qoz_Vec__qoz_string params = _qoz_ms_100->payload.DEnum.f2; if ((params.len) > 0) { qoz_map_set__qoz_string__qoz_Decl(&e->generic_decls, name, d); } 0;  break; } case qoz_Decl_DFn: { qoz_string name = _qoz_ms_100->payload.DFn.f1; qoz_Vec__qoz_string params = _qoz_ms_100->payload.DFn.f2; if ((params.len) > 0) { qoz_map_set__qoz_string__qoz_Decl(&e->generic_fn_decls, name, d); } 0;  break; } default: { NULL;  break; } } 0; } }
    return;
}

void emit_register_fn_instantiation(qoz_Emitter* e, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&callee);
    if ((type_args.len) == 0) { return;} qoz_string name = emit_generic_callee_name(e, callee); if (qoz_string_eq(name, QOZ_STR_LIT(""))) { return;} if (!qoz_map_contains__qoz_string__qoz_Decl(&e->generic_fn_decls, name)) { return;} qoz_string mangled = emit_mangle_inst(e, name, type_args); if (qoz_map_contains__qoz_string__bool(&e->fn_seen, mangled)) { return;} qoz_map_set__qoz_string__bool(&e->fn_seen, mangled, true); qoz_vec_push__qoz_Instantiation(&e->fn_insts, ((qoz_Instantiation){ .name = name, .args = type_args, .mangled = mangled })); { qoz_Vec__qoz_TypeExpr __col = type_args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* ta = __col.data[__i]; (void)ta; emit_walk_typeexpr(e, ta); } }
    return;
}

qoz_string emit_generic_callee_name(qoz_Emitter* e, qoz_Expr* callee) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&callee);
    qoz_Expr* _qoz_ms_101 = callee; qoz_string _qoz_mv_101 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_101->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_101->payload.EIdent.f1; _qoz_mv_101 = (name);  break; } case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_101->payload.EField.f1; qoz_string method = _qoz_ms_101->payload.EField.f2; qoz_Expr* _qoz_ms_102 = base; qoz_string _qoz_mv_102 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_102->tag) { case qoz_Expr_EIdent: { qoz_string pkg = _qoz_ms_102->payload.EIdent.f1; qoz_string _qoz_bv_90;
    {
        if (qoz_map_contains__qoz_string__bool(&e->packages, pkg)) { return qoz_string_cat(qoz_string_cat(pkg, QOZ_STR_LIT("_")), method);} _qoz_bv_90 = QOZ_STR_LIT("");
    }
    _qoz_mv_102 = (_qoz_bv_90);  break; } default: { _qoz_mv_102 = (QOZ_STR_LIT(""));  break; } } _qoz_mv_101 = (_qoz_mv_102);  break; } default: { _qoz_mv_101 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_101;
}

void emit_collect_type_instantiations(qoz_Emitter* e, qoz_File file) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    { qoz_Vec__qoz_Decl __col = file.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; emit_walk_decl_for_types(e, d); } }bool stable = false; int64_t iters = 0; while (!stable) { if (iters > 32) { stable = true; } iters = iters + 1; int64_t before_r = (e->record_insts.len); int64_t before_e = (e->enum_insts.len); int64_t before_f = (e->fn_insts.len); int64_t i = 0; while (i < (e->record_insts.len)) { qoz_Instantiation inst = e->record_insts.data[i]; qoz_Option__qoz_Decl* _qoz_ms_103 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_decls, inst.name); switch (_qoz_ms_103->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_103->payload.Some.f0; emit_walk_struct_fields_with_subst(e, decl, inst.args);  break; } default: { NULL;  break; } } 0; i = i + 1; } int64_t j = 0; while (j < (e->enum_insts.len)) { qoz_Instantiation inst = e->enum_insts.data[j]; qoz_Option__qoz_Decl* _qoz_ms_104 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_decls, inst.name); switch (_qoz_ms_104->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_104->payload.Some.f0; emit_walk_enum_variants_with_subst(e, decl, inst.args);  break; } default: { NULL;  break; } } 0; j = j + 1; } int64_t k = 0; while (k < (e->fn_insts.len)) { qoz_Instantiation inst = e->fn_insts.data[k]; qoz_Option__qoz_Decl* _qoz_ms_105 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_fn_decls, inst.name); switch (_qoz_ms_105->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_105->payload.Some.f0; emit_walk_fn_body_with_subst(e, decl, inst.args);  break; } default: { NULL;  break; } } 0; k = k + 1; } if ((((e->record_insts.len) == before_r) && ((e->enum_insts.len) == before_e)) && ((e->fn_insts.len) == before_f)) { stable = true; } } 
    return;
}

void emit_walk_fn_body_with_subst(qoz_Emitter* e, qoz_Decl* d, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_106 = d; switch (_qoz_ms_106->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string params = _qoz_ms_106->payload.DFn.f2; qoz_Vec__qoz_FnParam fn_params = _qoz_ms_106->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_106->payload.DFn.f4; qoz_Expr* body = _qoz_ms_106->payload.DFn.f5; {
        { qoz_Vec__qoz_FnParam __col = fn_params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; emit_walk_typeexpr(e, emit_substitute_type(e, pp.ty, params, args)); } }emit_walk_typeexpr(e, emit_substitute_type(e, ret, params, args)); qoz_Expr* subst_body = emit_substitute_expr(e, body, params, args); qoz_gc_push_root(&subst_body); emit_walk_expr_for_types(e, subst_body); 
    }
    0;  break; } default: { NULL;  break; } } 0; 
    return;
}

void emit_walk_struct_fields_with_subst(qoz_Emitter* e, qoz_Decl* d, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_107 = d; switch (_qoz_ms_107->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_107->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_107->payload.DStruct.f3; { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; qoz_TypeExpr* substituted = emit_substitute_type(e, f.ty, params, args); qoz_gc_push_root(&substituted); emit_walk_typeexpr(e, substituted); } }0;  break; } default: { NULL;  break; } } 0; 
    return;
}

void emit_walk_enum_variants_with_subst(qoz_Emitter* e, qoz_Decl* d, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_108 = d; switch (_qoz_ms_108->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string params = _qoz_ms_108->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_108->payload.DEnum.f3; { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pt = __col.data[__i]; (void)pt; qoz_TypeExpr* substituted = emit_substitute_type(e, pt, params, args); qoz_gc_push_root(&substituted); emit_walk_typeexpr(e, substituted); } }{ qoz_Vec__qoz_StructField __col = v.named; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField nf = __col.data[__i]; (void)nf; qoz_TypeExpr* substituted = emit_substitute_type(e, nf.ty, params, args); qoz_gc_push_root(&substituted); emit_walk_typeexpr(e, substituted); } }} }0;  break; } default: { NULL;  break; } } 0; 
    return;
}

void emit_walk_decl_for_types(qoz_Emitter* e, qoz_Decl* d) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&d);
    qoz_Decl* _qoz_ms_109 = d; switch (_qoz_ms_109->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_109->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_109->payload.DStruct.f3; if ((params.len) == 0) { { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; emit_walk_typeexpr(e, f.ty); } }} 0;  break; } case qoz_Decl_DEnum: { qoz_Vec__qoz_string params = _qoz_ms_109->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_109->payload.DEnum.f3; if ((params.len) == 0) { { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pt = __col.data[__i]; (void)pt; emit_walk_typeexpr(e, pt); } }{ qoz_Vec__qoz_StructField __col = v.named; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField nf = __col.data[__i]; (void)nf; emit_walk_typeexpr(e, nf.ty); } }} }} 0;  break; } case qoz_Decl_DFn: { qoz_Vec__qoz_string params = _qoz_ms_109->payload.DFn.f2; qoz_Vec__qoz_FnParam fn_params = _qoz_ms_109->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_109->payload.DFn.f4; qoz_Expr* body = _qoz_ms_109->payload.DFn.f5; if ((params.len) == 0) { { qoz_Vec__qoz_FnParam __col = fn_params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam fp = __col.data[__i]; (void)fp; emit_walk_typeexpr(e, fp.ty); } }emit_walk_typeexpr(e, ret); e->locals = qoz_map_make__qoz_string__qoz_TypeExpr(); { qoz_Vec__qoz_FnParam __col = fn_params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam fp = __col.data[__i]; (void)fp; qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, fp.name, fp.ty); } }emit_walk_expr_for_types(e, body); } 0;  break; } case qoz_Decl_DExternal: { qoz_Vec__qoz_FnParam fn_params = _qoz_ms_109->payload.DExternal.f3; qoz_TypeExpr* ret = _qoz_ms_109->payload.DExternal.f4; {
        { qoz_Vec__qoz_FnParam __col = fn_params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam fp = __col.data[__i]; (void)fp; emit_walk_typeexpr(e, fp.ty); } }emit_walk_typeexpr(e, ret); 
    }
    0;  break; } case qoz_Decl_DConst: { qoz_TypeExpr* ty = _qoz_ms_109->payload.DConst.f2; qoz_Expr* value = _qoz_ms_109->payload.DConst.f3; {
        emit_walk_typeexpr(e, ty); emit_walk_expr_for_types(e, value); 
    }
    0;  break; } default: { NULL;  break; } } 0; 
    return;
}

void emit_walk_typeexpr(qoz_Emitter* e, qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_110 = te; switch (_qoz_ms_110->tag) { case qoz_TypeExpr_TEUnit: { NULL;  break; } case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_110->payload.TEPtr.f1; emit_walk_typeexpr(e, inner);  break; } case qoz_TypeExpr_TETuple: { qoz_Vec__qoz_TypeExpr elems = _qoz_ms_110->payload.TETuple.f1; { qoz_Vec__qoz_TypeExpr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* el = __col.data[__i]; (void)el; emit_walk_typeexpr(e, el); } }0;  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_110->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_110->payload.TENamed.f2; {
        if (((path.len) == 1) && ((args.len) > 0)) { qoz_string name = path.data[0]; if (qoz_map_contains__qoz_string__qoz_Decl(&e->generic_decls, name)) { { qoz_Vec__qoz_TypeExpr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* a = __col.data[__i]; (void)a; emit_walk_typeexpr(e, a); } }emit_register_instance(e, name, args); } } { qoz_Vec__qoz_TypeExpr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* a = __col.data[__i]; (void)a; emit_walk_typeexpr(e, a); } }
    }
    0;  break; } case qoz_TypeExpr_TEFn: { qoz_Vec__qoz_TypeExpr params = _qoz_ms_110->payload.TEFn.f1; qoz_TypeExpr* ret = _qoz_ms_110->payload.TEFn.f2; {
        { qoz_Vec__qoz_TypeExpr __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* p = __col.data[__i]; (void)p; emit_walk_typeexpr(e, p); } }emit_walk_typeexpr(e, ret); 
    }
    0;  break; } } 0; 
    return;
}

qoz_Option__qoz_TypeExpr* emit_literal_typeexpr(qoz_Expr* ex) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_111 = ex; qoz_Option__qoz_TypeExpr* _qoz_mv_111 = NULL; switch (_qoz_ms_111->tag) { case qoz_Expr_EInt: { qoz_Span sp = _qoz_ms_111->payload.EInt.f0; _qoz_mv_111 = (qoz_make_Option__qoz_TypeExpr_Some(emit_single_named_te(sp, QOZ_STR_LIT("i64"))));  break; } case qoz_Expr_EFloat: { qoz_Span sp = _qoz_ms_111->payload.EFloat.f0; _qoz_mv_111 = (qoz_make_Option__qoz_TypeExpr_Some(emit_single_named_te(sp, QOZ_STR_LIT("f64"))));  break; } case qoz_Expr_EString: { qoz_Span sp = _qoz_ms_111->payload.EString.f0; _qoz_mv_111 = (qoz_make_Option__qoz_TypeExpr_Some(emit_single_named_te(sp, QOZ_STR_LIT("string"))));  break; } case qoz_Expr_EBool: { qoz_Span sp = _qoz_ms_111->payload.EBool.f0; _qoz_mv_111 = (qoz_make_Option__qoz_TypeExpr_Some(emit_single_named_te(sp, QOZ_STR_LIT("bool"))));  break; } case qoz_Expr_EChar: { qoz_Span sp = _qoz_ms_111->payload.EChar.f0; _qoz_mv_111 = (qoz_make_Option__qoz_TypeExpr_Some(emit_single_named_te(sp, QOZ_STR_LIT("char"))));  break; } case qoz_Expr_ECast: { qoz_TypeExpr* t = _qoz_ms_111->payload.ECast.f2; _qoz_mv_111 = (qoz_make_Option__qoz_TypeExpr_Some(t));  break; } default: { _qoz_mv_111 = (qoz_make_Option__qoz_TypeExpr_None());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_111;
}

void emit_walk_let_value_for_array_hint(qoz_Emitter* e, qoz_TypeExpr* ty, qoz_Expr* val) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ty);
    qoz_gc_push_root(&val);
    if (emit_is_unit_typeexpr(ty)) { return;} qoz_TypeExpr* _qoz_ms_112 = ty; switch (_qoz_ms_112->tag) { case qoz_TypeExpr_TENamed: { qoz_Span sp = _qoz_ms_112->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_112->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_112->payload.TENamed.f2; {
        if ((path.len) < 1) { return;} if (!qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Vec"))) { return;} if ((args.len) < 1) { return;} qoz_Expr* _qoz_ms_113 = val; switch (_qoz_ms_113->tag) { case qoz_Expr_EArrayLit: { {
        qoz_Vec__qoz_TypeExpr vec_args = qoz_vec_make__qoz_TypeExpr(); qoz_vec_push__qoz_TypeExpr(&vec_args, args.data[0]); emit_walk_typeexpr(e, qoz_make_TypeExpr_TENamed(sp, path, vec_args)); emit_register_fn_instantiation(e, qoz_make_Expr_EField(sp, qoz_make_Expr_EIdent(sp, QOZ_STR_LIT("vec")), QOZ_STR_LIT("make")), vec_args); emit_register_fn_instantiation(e, qoz_make_Expr_EField(sp, qoz_make_Expr_EIdent(sp, QOZ_STR_LIT("vec")), QOZ_STR_LIT("push")), vec_args); 
    }
    0;  break; } default: { NULL;  break; } } 0; 
    }
    0;  break; } default: { NULL;  break; } } 0; 
    return;
}

void emit_walk_expr_for_types(qoz_Emitter* e, qoz_Expr* ex) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_114 = ex; switch (_qoz_ms_114->tag) { case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_114->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_114->payload.EBlock.f2; {
        { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; qoz_Stmt* _qoz_ms_115 = s; switch (_qoz_ms_115->tag) { case qoz_Stmt_SLet: { qoz_string name = _qoz_ms_115->payload.SLet.f1; qoz_TypeExpr* ty = _qoz_ms_115->payload.SLet.f2; qoz_Expr* val = _qoz_ms_115->payload.SLet.f3; {
        emit_walk_typeexpr(e, ty); emit_walk_let_value_for_array_hint(e, ty, val); emit_walk_expr_for_types(e, val); if (!emit_is_unit_typeexpr(ty)) { qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, name, ty); } 
    }
    0;  break; } case qoz_Stmt_SVar: { qoz_string name = _qoz_ms_115->payload.SVar.f1; qoz_TypeExpr* ty = _qoz_ms_115->payload.SVar.f2; qoz_Expr* val = _qoz_ms_115->payload.SVar.f3; {
        emit_walk_typeexpr(e, ty); emit_walk_let_value_for_array_hint(e, ty, val); emit_walk_expr_for_types(e, val); if (!emit_is_unit_typeexpr(ty)) { qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, name, ty); } 
    }
    0;  break; } case qoz_Stmt_SExpr: { qoz_Expr* ex2 = _qoz_ms_115->payload.SExpr.f1; emit_walk_expr_for_types(e, ex2);  break; } } 0; } }emit_walk_expr_for_types(e, tail); 
    }
    0;  break; } case qoz_Expr_ERecord: { qoz_TypeExpr* te = _qoz_ms_114->payload.ERecord.f1; qoz_Vec__qoz_RecordFieldLit fields = _qoz_ms_114->payload.ERecord.f2; {
        emit_walk_typeexpr(e, te); { qoz_Vec__qoz_RecordFieldLit __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_RecordFieldLit f = __col.data[__i]; (void)f; emit_walk_expr_for_types(e, f.value); } }
    }
    0;  break; } case qoz_Expr_ECast: { qoz_Expr* val = _qoz_ms_114->payload.ECast.f1; qoz_TypeExpr* te = _qoz_ms_114->payload.ECast.f2; {
        emit_walk_typeexpr(e, te); emit_walk_expr_for_types(e, val); 
    }
    0;  break; } case qoz_Expr_ESizeOf: { qoz_TypeExpr* te = _qoz_ms_114->payload.ESizeOf.f1; emit_walk_typeexpr(e, te);  break; } case qoz_Expr_EBinary: { qoz_Expr* l = _qoz_ms_114->payload.EBinary.f2; qoz_Expr* r = _qoz_ms_114->payload.EBinary.f3; {
        emit_walk_expr_for_types(e, l); emit_walk_expr_for_types(e, r); 
    }
    0;  break; } case qoz_Expr_EUnary: { qoz_Expr* rhs = _qoz_ms_114->payload.EUnary.f2; emit_walk_expr_for_types(e, rhs);  break; } case qoz_Expr_EAssign: { qoz_Expr* l = _qoz_ms_114->payload.EAssign.f2; qoz_Expr* r = _qoz_ms_114->payload.EAssign.f3; {
        emit_walk_expr_for_types(e, l); emit_walk_expr_for_types(e, r); emit_walk_assign_for_map_helpers(e, l); 
    }
    0;  break; } case qoz_Expr_ECall: { qoz_Expr* callee = _qoz_ms_114->payload.ECall.f1; qoz_Vec__qoz_TypeExpr type_args = _qoz_ms_114->payload.ECall.f2; qoz_Vec__qoz_Expr args = _qoz_ms_114->payload.ECall.f3; {
        emit_walk_expr_for_types(e, callee); { qoz_Vec__qoz_TypeExpr __col = type_args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* ta = __col.data[__i]; (void)ta; emit_walk_typeexpr(e, ta); } }{ qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; emit_walk_expr_for_types(e, a); } }emit_register_fn_instantiation(e, callee, type_args); 
    }
    0;  break; } case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_114->payload.EField.f1; emit_walk_expr_for_types(e, base);  break; } case qoz_Expr_EIndex: { qoz_Expr* base = _qoz_ms_114->payload.EIndex.f1; qoz_Expr* idx = _qoz_ms_114->payload.EIndex.f2; {
        emit_walk_expr_for_types(e, base); emit_walk_expr_for_types(e, idx); 
    }
    0;  break; } case qoz_Expr_ETry: { qoz_Expr* val = _qoz_ms_114->payload.ETry.f1; emit_walk_expr_for_types(e, val);  break; } case qoz_Expr_EIf: { qoz_Expr* c = _qoz_ms_114->payload.EIf.f1; qoz_Expr* t = _qoz_ms_114->payload.EIf.f2; qoz_Expr* f = _qoz_ms_114->payload.EIf.f3; {
        emit_walk_expr_for_types(e, c); emit_walk_expr_for_types(e, t); emit_walk_expr_for_types(e, f); 
    }
    0;  break; } case qoz_Expr_EMatch: { qoz_Expr* scrut = _qoz_ms_114->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_114->payload.EMatch.f2; {
        emit_walk_expr_for_types(e, scrut); { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; emit_walk_expr_for_types(e, arm.body); } }
    }
    0;  break; } case qoz_Expr_EWhile: { qoz_Expr* c = _qoz_ms_114->payload.EWhile.f1; qoz_Expr* b = _qoz_ms_114->payload.EWhile.f2; {
        emit_walk_expr_for_types(e, c); emit_walk_expr_for_types(e, b); 
    }
    0;  break; } case qoz_Expr_EFor: { qoz_Expr* it = _qoz_ms_114->payload.EFor.f3; qoz_Expr* b = _qoz_ms_114->payload.EFor.f4; {
        emit_walk_expr_for_types(e, it); emit_walk_expr_for_types(e, b); 
    }
    0;  break; } case qoz_Expr_EReturn: { qoz_Expr* v = _qoz_ms_114->payload.EReturn.f1; emit_walk_expr_for_types(e, v);  break; } case qoz_Expr_EDefer: { qoz_Expr* b = _qoz_ms_114->payload.EDefer.f1; emit_walk_expr_for_types(e, b);  break; } case qoz_Expr_ETuple: { qoz_Vec__qoz_Expr elems = _qoz_ms_114->payload.ETuple.f1; { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; emit_walk_expr_for_types(e, el); } }0;  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_114->payload.EArrayLit.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_114->payload.EArrayLit.f1; {
        { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; emit_walk_expr_for_types(e, el); } }if ((elems.len) > 0) { qoz_Option__qoz_TypeExpr* elem_te = emit_literal_typeexpr(elems.data[0]); qoz_gc_push_root(&elem_te); qoz_Option__qoz_TypeExpr* _qoz_ms_116 = elem_te; switch (_qoz_ms_116->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* et = _qoz_ms_116->payload.Some.f0; {
        qoz_Vec__qoz_TypeExpr args = qoz_vec_make__qoz_TypeExpr(); qoz_vec_push__qoz_TypeExpr(&args, et); qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("Vec")); emit_walk_typeexpr(e, qoz_make_TypeExpr_TENamed(sp, path, args)); emit_register_fn_instantiation(e, qoz_make_Expr_EField(sp, qoz_make_Expr_EIdent(sp, QOZ_STR_LIT("vec")), QOZ_STR_LIT("make")), args); emit_register_fn_instantiation(e, qoz_make_Expr_EField(sp, qoz_make_Expr_EIdent(sp, QOZ_STR_LIT("vec")), QOZ_STR_LIT("push")), args); 
    }
    0;  break; } case qoz_Option__qoz_TypeExpr_None: { NULL;  break; } } 0; } 
    }
    0;  break; } case qoz_Expr_EClosure: { qoz_Expr* body = _qoz_ms_114->payload.EClosure.f3; emit_walk_expr_for_types(e, body);  break; } default: { NULL;  break; } } 0; 
    return;
}

void emit_register_instance(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string mangled = emit_mangle_inst(e, name, args); if (qoz_map_contains__qoz_string__bool(&e->is_enum, name)) { if (qoz_map_contains__qoz_string__bool(&e->enum_seen, mangled)) { return;} qoz_map_set__qoz_string__bool(&e->enum_seen, mangled, true); qoz_vec_push__qoz_Instantiation(&e->enum_insts, ((qoz_Instantiation){ .name = name, .args = args, .mangled = mangled })); }  else { if (qoz_map_contains__qoz_string__bool(&e->record_seen, mangled)) { return;} qoz_map_set__qoz_string__bool(&e->record_seen, mangled, true); qoz_vec_push__qoz_Instantiation(&e->record_insts, ((qoz_Instantiation){ .name = name, .args = args, .mangled = mangled })); } 
    return;
}

qoz_string emit_mangle_inst(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string acc = name; { qoz_Vec__qoz_TypeExpr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* a = __col.data[__i]; (void)a; acc = qoz_string_cat(acc, QOZ_STR_LIT("__")); acc = qoz_string_cat(acc, emit_mangle_type(e, a)); } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return acc;
}

qoz_string emit_mangle_type(qoz_Emitter* e, qoz_TypeExpr* t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&t);
    qoz_TypeExpr* _qoz_ms_117 = t; qoz_string _qoz_mv_117 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_117->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_117 = (QOZ_STR_LIT("void"));  break; } case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_117->payload.TEPtr.f1; _qoz_mv_117 = (qoz_string_cat(emit_mangle_type(e, inner), QOZ_STR_LIT("_ptr")));  break; } case qoz_TypeExpr_TETuple: { qoz_Vec__qoz_TypeExpr elems = _qoz_ms_117->payload.TETuple.f1; qoz_string _qoz_bv_91;
    {
        qoz_string acc = QOZ_STR_LIT("tuple"); { qoz_Vec__qoz_TypeExpr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* el = __col.data[__i]; (void)el; acc = qoz_string_cat(acc, QOZ_STR_LIT("__")); acc = qoz_string_cat(acc, emit_mangle_type(e, el)); } }_qoz_bv_91 = acc;
    }
    _qoz_mv_117 = (_qoz_bv_91);  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_117->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_117->payload.TENamed.f2; _qoz_mv_117 = (emit_mangle_named(e, path, args));  break; } case qoz_TypeExpr_TEFn: { _qoz_mv_117 = (QOZ_STR_LIT("fn"));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_117;
}

qoz_string emit_mangle_named(qoz_Emitter* e, qoz_Vec__qoz_string path, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if ((path.len) != 1) { return QOZ_STR_LIT("unknown");} qoz_string n = path.data[0]; qoz_string prim = emit_primitive_c_name(n); if (!qoz_string_eq(prim, QOZ_STR_LIT(""))) { return prim;} if ((args.len) > 0) { return qoz_string_cat(QOZ_STR_LIT("qoz_"), emit_mangle_inst(e, n, args));} qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_cat(QOZ_STR_LIT("qoz_"), n);
}

qoz_string emit_primitive_c_name(qoz_string n) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if (qoz_string_eq(n, QOZ_STR_LIT("i8"))) { return QOZ_STR_LIT("int8_t");} if (qoz_string_eq(n, QOZ_STR_LIT("i16"))) { return QOZ_STR_LIT("int16_t");} if (qoz_string_eq(n, QOZ_STR_LIT("i32"))) { return QOZ_STR_LIT("int32_t");} if (qoz_string_eq(n, QOZ_STR_LIT("i64"))) { return QOZ_STR_LIT("int64_t");} if (qoz_string_eq(n, QOZ_STR_LIT("u8"))) { return QOZ_STR_LIT("uint8_t");} if (qoz_string_eq(n, QOZ_STR_LIT("u16"))) { return QOZ_STR_LIT("uint16_t");} if (qoz_string_eq(n, QOZ_STR_LIT("u32"))) { return QOZ_STR_LIT("uint32_t");} if (qoz_string_eq(n, QOZ_STR_LIT("u64"))) { return QOZ_STR_LIT("uint64_t");} if (qoz_string_eq(n, QOZ_STR_LIT("f32"))) { return QOZ_STR_LIT("float");} if (qoz_string_eq(n, QOZ_STR_LIT("f64"))) { return QOZ_STR_LIT("double");} if (qoz_string_eq(n, QOZ_STR_LIT("bool"))) { return QOZ_STR_LIT("bool");} if (qoz_string_eq(n, QOZ_STR_LIT("char"))) { return QOZ_STR_LIT("char");} if (qoz_string_eq(n, QOZ_STR_LIT("string"))) { return QOZ_STR_LIT("qoz_string");} qoz_gc_shadow_set_top(_qoz_shadow_guard); return QOZ_STR_LIT("");
}

qoz_TypeExpr* emit_substitute_type(qoz_Emitter* e, qoz_TypeExpr* t, qoz_Vec__qoz_string params, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&t);
    qoz_TypeExpr* _qoz_ms_118 = t; qoz_TypeExpr* _qoz_mv_118 = NULL; switch (_qoz_ms_118->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_118 = (t);  break; } case qoz_TypeExpr_TEPtr: { qoz_Span span = _qoz_ms_118->payload.TEPtr.f0; qoz_TypeExpr* inner = _qoz_ms_118->payload.TEPtr.f1; _qoz_mv_118 = (qoz_make_TypeExpr_TEPtr(span, emit_substitute_type(e, inner, params, args)));  break; } case qoz_TypeExpr_TETuple: { qoz_Span span = _qoz_ms_118->payload.TETuple.f0; qoz_Vec__qoz_TypeExpr elems = _qoz_ms_118->payload.TETuple.f1; qoz_TypeExpr* _qoz_bv_92;
    {
        qoz_Vec__qoz_TypeExpr new_elems = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_TypeExpr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_TypeExpr(&new_elems, emit_substitute_type(e, el, params, args)); } }_qoz_bv_92 = qoz_make_TypeExpr_TETuple(span, new_elems);
    }
    _qoz_mv_118 = (_qoz_bv_92);  break; } case qoz_TypeExpr_TENamed: { qoz_Span span = _qoz_ms_118->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_118->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr t_args = _qoz_ms_118->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_93;
    {
        if (((path.len) == 1) && ((t_args.len) == 0)) { qoz_string n = path.data[0]; int64_t i = 0; { qoz_Vec__qoz_string __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string p = __col.data[__i]; (void)p; if (qoz_string_eq(p, n)) { return args.data[i];} i = i + 1; } }} qoz_Vec__qoz_TypeExpr new_args = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_TypeExpr __col = t_args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_TypeExpr(&new_args, emit_substitute_type(e, a, params, args)); } }_qoz_bv_93 = qoz_make_TypeExpr_TENamed(span, path, new_args);
    }
    _qoz_mv_118 = (_qoz_bv_93);  break; } case qoz_TypeExpr_TEFn: { qoz_Span span = _qoz_ms_118->payload.TEFn.f0; qoz_Vec__qoz_TypeExpr fparams = _qoz_ms_118->payload.TEFn.f1; qoz_TypeExpr* ret = _qoz_ms_118->payload.TEFn.f2; qoz_TypeExpr* _qoz_bv_94;
    {
        qoz_Vec__qoz_TypeExpr new_fparams = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_TypeExpr __col = fparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* fp = __col.data[__i]; (void)fp; qoz_vec_push__qoz_TypeExpr(&new_fparams, emit_substitute_type(e, fp, params, args)); } }_qoz_bv_94 = qoz_make_TypeExpr_TEFn(span, new_fparams, emit_substitute_type(e, ret, params, args));
    }
    _qoz_mv_118 = (_qoz_bv_94);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_118;
}

qoz_Expr* emit_substitute_expr(qoz_Emitter* e, qoz_Expr* x, qoz_Vec__qoz_string params, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&x);
    qoz_Expr* _qoz_ms_119 = x; qoz_Expr* _qoz_mv_119 = NULL; switch (_qoz_ms_119->tag) { case qoz_Expr_EInt: { _qoz_mv_119 = (x);  break; } case qoz_Expr_EFloat: { _qoz_mv_119 = (x);  break; } case qoz_Expr_EString: { _qoz_mv_119 = (x);  break; } case qoz_Expr_EChar: { _qoz_mv_119 = (x);  break; } case qoz_Expr_EBool: { _qoz_mv_119 = (x);  break; } case qoz_Expr_ENil: { _qoz_mv_119 = (x);  break; } case qoz_Expr_EIdent: { _qoz_mv_119 = (x);  break; } case qoz_Expr_EPath: { _qoz_mv_119 = (x);  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_119->payload.EUnary.f0; qoz_UnaryOp* op = _qoz_ms_119->payload.EUnary.f1; qoz_Expr* rhs = _qoz_ms_119->payload.EUnary.f2; _qoz_mv_119 = (qoz_make_Expr_EUnary(sp, op, emit_substitute_expr(e, rhs, params, args)));  break; } case qoz_Expr_EBinary: { qoz_Span sp = _qoz_ms_119->payload.EBinary.f0; qoz_BinaryOp* op = _qoz_ms_119->payload.EBinary.f1; qoz_Expr* l = _qoz_ms_119->payload.EBinary.f2; qoz_Expr* r = _qoz_ms_119->payload.EBinary.f3; _qoz_mv_119 = (qoz_make_Expr_EBinary(sp, op, emit_substitute_expr(e, l, params, args), emit_substitute_expr(e, r, params, args)));  break; } case qoz_Expr_EAssign: { qoz_Span sp = _qoz_ms_119->payload.EAssign.f0; qoz_AssignOp* op = _qoz_ms_119->payload.EAssign.f1; qoz_Expr* l = _qoz_ms_119->payload.EAssign.f2; qoz_Expr* r = _qoz_ms_119->payload.EAssign.f3; _qoz_mv_119 = (qoz_make_Expr_EAssign(sp, op, emit_substitute_expr(e, l, params, args), emit_substitute_expr(e, r, params, args)));  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_119->payload.ECall.f0; qoz_Expr* callee = _qoz_ms_119->payload.ECall.f1; qoz_Vec__qoz_TypeExpr ta = _qoz_ms_119->payload.ECall.f2; qoz_Vec__qoz_Expr ax = _qoz_ms_119->payload.ECall.f3; qoz_Expr* _qoz_bv_95;
    {
        qoz_Vec__qoz_TypeExpr new_ta = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_TypeExpr __col = ta; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* t = __col.data[__i]; (void)t; qoz_vec_push__qoz_TypeExpr(&new_ta, emit_substitute_type(e, t, params, args)); } }qoz_Vec__qoz_Expr new_args = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Expr __col = ax; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_Expr(&new_args, emit_substitute_expr(e, a, params, args)); } }_qoz_bv_95 = qoz_make_Expr_ECall(sp, emit_substitute_expr(e, callee, params, args), new_ta, new_args);
    }
    _qoz_mv_119 = (_qoz_bv_95);  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_119->payload.EField.f0; qoz_Expr* base = _qoz_ms_119->payload.EField.f1; qoz_string name = _qoz_ms_119->payload.EField.f2; _qoz_mv_119 = (qoz_make_Expr_EField(sp, emit_substitute_expr(e, base, params, args), name));  break; } case qoz_Expr_EIndex: { qoz_Span sp = _qoz_ms_119->payload.EIndex.f0; qoz_Expr* base = _qoz_ms_119->payload.EIndex.f1; qoz_Expr* idx = _qoz_ms_119->payload.EIndex.f2; _qoz_mv_119 = (qoz_make_Expr_EIndex(sp, emit_substitute_expr(e, base, params, args), emit_substitute_expr(e, idx, params, args)));  break; } case qoz_Expr_ECast: { qoz_Span sp = _qoz_ms_119->payload.ECast.f0; qoz_Expr* value = _qoz_ms_119->payload.ECast.f1; qoz_TypeExpr* t = _qoz_ms_119->payload.ECast.f2; _qoz_mv_119 = (qoz_make_Expr_ECast(sp, emit_substitute_expr(e, value, params, args), emit_substitute_type(e, t, params, args)));  break; } case qoz_Expr_ETry: { qoz_Span sp = _qoz_ms_119->payload.ETry.f0; qoz_Expr* value = _qoz_ms_119->payload.ETry.f1; _qoz_mv_119 = (qoz_make_Expr_ETry(sp, emit_substitute_expr(e, value, params, args)));  break; } case qoz_Expr_ETuple: { qoz_Span sp = _qoz_ms_119->payload.ETuple.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_119->payload.ETuple.f1; qoz_Expr* _qoz_bv_96;
    {
        qoz_Vec__qoz_Expr new_e = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_Expr(&new_e, emit_substitute_expr(e, el, params, args)); } }_qoz_bv_96 = qoz_make_Expr_ETuple(sp, new_e);
    }
    _qoz_mv_119 = (_qoz_bv_96);  break; } case qoz_Expr_ERecord: { qoz_Span sp = _qoz_ms_119->payload.ERecord.f0; qoz_TypeExpr* te = _qoz_ms_119->payload.ERecord.f1; qoz_Vec__qoz_RecordFieldLit fields = _qoz_ms_119->payload.ERecord.f2; qoz_Expr* _qoz_bv_97;
    {
        qoz_Vec__qoz_RecordFieldLit new_fields = qoz_vec_make__qoz_RecordFieldLit(); { qoz_Vec__qoz_RecordFieldLit __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_RecordFieldLit f = __col.data[__i]; (void)f; qoz_vec_push__qoz_RecordFieldLit(&new_fields, ((qoz_RecordFieldLit){ .name = f.name, .value = emit_substitute_expr(e, f.value, params, args) })); } }_qoz_bv_97 = qoz_make_Expr_ERecord(sp, emit_substitute_type(e, te, params, args), new_fields);
    }
    _qoz_mv_119 = (_qoz_bv_97);  break; } case qoz_Expr_EClosure: { qoz_Span sp = _qoz_ms_119->payload.EClosure.f0; qoz_Vec__qoz_ClosureParam cps = _qoz_ms_119->payload.EClosure.f1; qoz_TypeExpr* ret = _qoz_ms_119->payload.EClosure.f2; qoz_Expr* body = _qoz_ms_119->payload.EClosure.f3; qoz_Expr* _qoz_bv_98;
    {
        qoz_Vec__qoz_ClosureParam new_cps = qoz_vec_make__qoz_ClosureParam(); { qoz_Vec__qoz_ClosureParam __col = cps; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_ClosureParam cp = __col.data[__i]; (void)cp; qoz_vec_push__qoz_ClosureParam(&new_cps, ((qoz_ClosureParam){ .name = cp.name, .ty = emit_substitute_type(e, cp.ty, params, args) })); } }_qoz_bv_98 = qoz_make_Expr_EClosure(sp, new_cps, emit_substitute_type(e, ret, params, args), emit_substitute_expr(e, body, params, args));
    }
    _qoz_mv_119 = (_qoz_bv_98);  break; } case qoz_Expr_EBlock: { qoz_Span sp = _qoz_ms_119->payload.EBlock.f0; qoz_Vec__qoz_Stmt stmts = _qoz_ms_119->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_119->payload.EBlock.f2; qoz_Expr* _qoz_bv_99;
    {
        qoz_Vec__qoz_Stmt new_stmts = qoz_vec_make__qoz_Stmt(); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; qoz_vec_push__qoz_Stmt(&new_stmts, emit_substitute_stmt(e, s, params, args)); } }_qoz_bv_99 = qoz_make_Expr_EBlock(sp, new_stmts, emit_substitute_expr(e, tail, params, args));
    }
    _qoz_mv_119 = (_qoz_bv_99);  break; } case qoz_Expr_EIf: { qoz_Span sp = _qoz_ms_119->payload.EIf.f0; qoz_Expr* c = _qoz_ms_119->payload.EIf.f1; qoz_Expr* t = _qoz_ms_119->payload.EIf.f2; qoz_Expr* f = _qoz_ms_119->payload.EIf.f3; _qoz_mv_119 = (qoz_make_Expr_EIf(sp, emit_substitute_expr(e, c, params, args), emit_substitute_expr(e, t, params, args), emit_substitute_expr(e, f, params, args)));  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_119->payload.EMatch.f0; qoz_Expr* scrut = _qoz_ms_119->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_119->payload.EMatch.f2; qoz_Expr* _qoz_bv_100;
    {
        qoz_Vec__qoz_MatchArm new_arms = qoz_vec_make__qoz_MatchArm(); { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm a = __col.data[__i]; (void)a; qoz_vec_push__qoz_MatchArm(&new_arms, ((qoz_MatchArm){ .pat = a.pat, .body = emit_substitute_expr(e, a.body, params, args), .has_guard = a.has_guard, .guard = ((a.has_guard) ? emit_substitute_expr(e, a.guard, params, args) : a.guard) })); } }_qoz_bv_100 = qoz_make_Expr_EMatch(sp, emit_substitute_expr(e, scrut, params, args), new_arms);
    }
    _qoz_mv_119 = (_qoz_bv_100);  break; } case qoz_Expr_EWhile: { qoz_Span sp = _qoz_ms_119->payload.EWhile.f0; qoz_Expr* c = _qoz_ms_119->payload.EWhile.f1; qoz_Expr* b = _qoz_ms_119->payload.EWhile.f2; _qoz_mv_119 = (qoz_make_Expr_EWhile(sp, emit_substitute_expr(e, c, params, args), emit_substitute_expr(e, b, params, args)));  break; } case qoz_Expr_EFor: { qoz_Span sp = _qoz_ms_119->payload.EFor.f0; qoz_string b1 = _qoz_ms_119->payload.EFor.f1; qoz_string b2 = _qoz_ms_119->payload.EFor.f2; qoz_Expr* it = _qoz_ms_119->payload.EFor.f3; qoz_Expr* body = _qoz_ms_119->payload.EFor.f4; _qoz_mv_119 = (qoz_make_Expr_EFor(sp, b1, b2, emit_substitute_expr(e, it, params, args), emit_substitute_expr(e, body, params, args)));  break; } case qoz_Expr_EReturn: { qoz_Span sp = _qoz_ms_119->payload.EReturn.f0; qoz_Expr* v = _qoz_ms_119->payload.EReturn.f1; _qoz_mv_119 = (qoz_make_Expr_EReturn(sp, emit_substitute_expr(e, v, params, args)));  break; } case qoz_Expr_EDefer: { qoz_Span sp = _qoz_ms_119->payload.EDefer.f0; qoz_Expr* v = _qoz_ms_119->payload.EDefer.f1; _qoz_mv_119 = (qoz_make_Expr_EDefer(sp, emit_substitute_expr(e, v, params, args)));  break; } case qoz_Expr_ESizeOf: { qoz_Span sp = _qoz_ms_119->payload.ESizeOf.f0; qoz_TypeExpr* t = _qoz_ms_119->payload.ESizeOf.f1; _qoz_mv_119 = (qoz_make_Expr_ESizeOf(sp, emit_substitute_type(e, t, params, args)));  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_119->payload.EArrayLit.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_119->payload.EArrayLit.f1; qoz_Expr* _qoz_bv_101;
    {
        qoz_Vec__qoz_Expr new_e = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_Expr(&new_e, emit_substitute_expr(e, el, params, args)); } }_qoz_bv_101 = qoz_make_Expr_EArrayLit(sp, new_e);
    }
    _qoz_mv_119 = (_qoz_bv_101);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_119;
}

qoz_Stmt* emit_substitute_stmt(qoz_Emitter* e, qoz_Stmt* s, qoz_Vec__qoz_string params, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&s);
    qoz_Stmt* _qoz_ms_120 = s; qoz_Stmt* _qoz_mv_120 = NULL; switch (_qoz_ms_120->tag) { case qoz_Stmt_SLet: { qoz_Span sp = _qoz_ms_120->payload.SLet.f0; qoz_string name = _qoz_ms_120->payload.SLet.f1; qoz_TypeExpr* te = _qoz_ms_120->payload.SLet.f2; qoz_Expr* val = _qoz_ms_120->payload.SLet.f3; _qoz_mv_120 = (qoz_make_Stmt_SLet(sp, name, emit_substitute_type(e, te, params, args), emit_substitute_expr(e, val, params, args)));  break; } case qoz_Stmt_SVar: { qoz_Span sp = _qoz_ms_120->payload.SVar.f0; qoz_string name = _qoz_ms_120->payload.SVar.f1; qoz_TypeExpr* te = _qoz_ms_120->payload.SVar.f2; qoz_Expr* val = _qoz_ms_120->payload.SVar.f3; _qoz_mv_120 = (qoz_make_Stmt_SVar(sp, name, emit_substitute_type(e, te, params, args), emit_substitute_expr(e, val, params, args)));  break; } case qoz_Stmt_SExpr: { qoz_Span sp = _qoz_ms_120->payload.SExpr.f0; qoz_Expr* x = _qoz_ms_120->payload.SExpr.f1; _qoz_mv_120 = (qoz_make_Stmt_SExpr(sp, emit_substitute_expr(e, x, params, args)));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_120;
}

qoz_string emit_operator_first_param_type_name(qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_121 = te; qoz_string _qoz_mv_121 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_121->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_121->payload.TEPtr.f1; qoz_TypeExpr* _qoz_ms_122 = inner; qoz_string _qoz_mv_122 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_122->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_122->payload.TENamed.f1; qoz_string _qoz_bv_102;
    {
        if ((path.len) > 0) { return path.data[(path.len) - 1];} _qoz_bv_102 = QOZ_STR_LIT("");
    }
    _qoz_mv_122 = (_qoz_bv_102);  break; } default: { _qoz_mv_122 = (QOZ_STR_LIT(""));  break; } } _qoz_mv_121 = (_qoz_mv_122);  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_121->payload.TENamed.f1; qoz_string _qoz_bv_103;
    {
        if ((path.len) > 0) { return path.data[(path.len) - 1];} _qoz_bv_103 = QOZ_STR_LIT("");
    }
    _qoz_mv_121 = (_qoz_bv_103);  break; } default: { _qoz_mv_121 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_121;
}

void emit_register_variants(qoz_Emitter* e, qoz_File file) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    { qoz_Vec__qoz_Decl __col = file.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_123 = d; switch (_qoz_ms_123->tag) { case qoz_Decl_DEnum: { qoz_string name = _qoz_ms_123->payload.DEnum.f1; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_123->payload.DEnum.f3; {
        qoz_map_set__qoz_string__bool(&e->is_enum, name, true); qoz_map_set__qoz_string__qoz_Decl(&e->enum_decls, name, d); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; qoz_map_set__qoz_string__qoz_string(&e->variant_of, v.name, name); } }
    }
    0;  break; } case qoz_Decl_DStruct: { qoz_string name = _qoz_ms_123->payload.DStruct.f1; (qoz_map_set__qoz_string__qoz_Decl(&e->struct_decls, name, d), 0);  break; } case qoz_Decl_DExternal: { qoz_string name = _qoz_ms_123->payload.DExternal.f1; qoz_string symbol = _qoz_ms_123->payload.DExternal.f2; qoz_Vec__qoz_FnParam fps = _qoz_ms_123->payload.DExternal.f3; qoz_TypeExpr* ret = _qoz_ms_123->payload.DExternal.f4; {
        qoz_map_set__qoz_string__qoz_string(&e->externs, name, symbol); qoz_map_set__qoz_string__qoz_TypeExpr(&e->fn_returns, name, ret); qoz_Vec__qoz_TypeExpr pts = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_FnParam __col = fps; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam fp = __col.data[__i]; (void)fp; qoz_vec_push__qoz_TypeExpr(&pts, fp.ty); } }qoz_map_set__qoz_string__qoz_Vec__qoz_TypeExpr(&e->fn_params, name, pts); 
    }
    0;  break; } case qoz_Decl_DFn: { qoz_string name = _qoz_ms_123->payload.DFn.f1; qoz_Vec__qoz_string tparams = _qoz_ms_123->payload.DFn.f2; qoz_Vec__qoz_FnParam fps = _qoz_ms_123->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_123->payload.DFn.f4; qoz_string attr = _qoz_ms_123->payload.DFn.f6; {
        if ((tparams.len) == 0) { qoz_map_set__qoz_string__qoz_TypeExpr(&e->fn_returns, name, ret); qoz_Vec__qoz_TypeExpr pts = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_FnParam __col = fps; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam fp = __col.data[__i]; (void)fp; qoz_vec_push__qoz_TypeExpr(&pts, fp.ty); } }qoz_map_set__qoz_string__qoz_Vec__qoz_TypeExpr(&e->fn_params, name, pts); } if (!qoz_string_eq(attr, QOZ_STR_LIT(""))) { if ((fps.len) > 0) { qoz_string tname = emit_operator_first_param_type_name(fps.data[0].ty); if (!qoz_string_eq(tname, QOZ_STR_LIT(""))) { qoz_string key = qoz_string_cat(qoz_string_cat(attr, QOZ_STR_LIT("::")), tname); qoz_map_set__qoz_string__qoz_string(&e->op_dispatch, key, name); } } } 
    }
    0;  break; } case qoz_Decl_DImport: { qoz_Vec__qoz_string ipath = _qoz_ms_123->payload.DImport.f1; qoz_string alias = _qoz_ms_123->payload.DImport.f2; {
        qoz_string pkg = alias; if (qoz_string_eq(pkg, QOZ_STR_LIT("")) && ((ipath.len) > 0)) { pkg = ipath.data[(ipath.len) - 1]; } if (!qoz_string_eq(pkg, QOZ_STR_LIT(""))) { qoz_map_set__qoz_string__bool(&e->packages, pkg, true); } 
    }
    0;  break; } default: { NULL;  break; } } 0; } }
    return;
}

qoz_string emit_strip_numeric_underscores(qoz_string s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_string clean = s; if (qoz_string_index_byte(s, 95) >= 0) { clean = QOZ_STR_LIT(""); int64_t i = 0; int64_t n = (s).len; while (i < n) { int64_t c = qoz_string_byte_at(s, i); if (c != 95) { clean = qoz_string_cat(clean, qoz_string_slice(s, i, i + 1)); } i = i + 1; } } if (((clean).len >= 2) && (qoz_string_byte_at(clean, 0) == 48)) { int64_t c1 = qoz_string_byte_at(clean, 1); if ((c1 == 98) || (c1 == 66)) { int64_t v = 0; int64_t j = 2; int64_t m = (clean).len; while (j < m) { int64_t b = qoz_string_byte_at(clean, j); if ((b == 48) || (b == 49)) { v = v * 2 + (b - 48); j = j + 1; }  else { return s;} } qoz_string _qoz_bv_104;
    {
        qoz_Strbuf _qoz_sb_723_20 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_723_20); strings_sb_append_i64(&_qoz_sb_723_20, v); _qoz_bv_104 = strings_sb_finish(&_qoz_sb_723_20);
    }
    return _qoz_bv_104;} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return clean;
}

void emit_push(qoz_Emitter* e, qoz_string s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    strings_sb_append(&e->out, s); 
    return;
}

qoz_string emit_render(qoz_Emitter* e) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    int64_t total = strings_sb_len(&e->out); if (total == 0) { return QOZ_STR_LIT("");} qoz_string typedef_block = QOZ_STR_LIT(""); { qoz_Vec__qoz_string __col = e->fn_typedef_order; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string name = __col.data[__i]; (void)name; qoz_Option__qoz_string* _qoz_ms_124 = qoz_map_get__qoz_string__qoz_string(&e->fn_typedefs, name); switch (_qoz_ms_124->tag) { case qoz_Option__qoz_string_Some: { qoz_string body = _qoz_ms_124->payload.Some.f0; (typedef_block = qoz_string_cat(typedef_block, body), 0);  break; } default: { NULL;  break; } } 0; } }if ((typedef_block).len > 0) { typedef_block = qoz_string_cat(typedef_block, QOZ_STR_LIT("\n")); } qoz_string synth_decl_block = QOZ_STR_LIT(""); { qoz_Vec__qoz_string __col = e->synth_fn_decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string d = __col.data[__i]; (void)d; synth_decl_block = qoz_string_cat(synth_decl_block, d); } }if ((synth_decl_block).len > 0) { synth_decl_block = qoz_string_cat(synth_decl_block, QOZ_STR_LIT("\n")); } qoz_string tuple_block = QOZ_STR_LIT(""); { qoz_Vec__qoz_string __col = e->tuple_typedef_order; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string tn = __col.data[__i]; (void)tn; qoz_Option__qoz_string* _qoz_ms_125 = qoz_map_get__qoz_string__qoz_string(&e->tuple_typedefs, tn); switch (_qoz_ms_125->tag) { case qoz_Option__qoz_string_Some: { qoz_string body = _qoz_ms_125->payload.Some.f0; (tuple_block = qoz_string_cat(tuple_block, body), 0);  break; } default: { NULL;  break; } } 0; } }if ((tuple_block).len > 0) { tuple_block = qoz_string_cat(tuple_block, QOZ_STR_LIT("\n")); } int64_t p1 = e->pos_fn_typedefs; int64_t p2 = e->pos_tuple_typedefs; int64_t p3 = e->pos_synth_fn_decls; qoz_string acc = QOZ_STR_LIT(""); acc = qoz_string_cat(acc, strings_sb_slice_copy(&e->out, 0, p1)); acc = qoz_string_cat(acc, typedef_block); acc = qoz_string_cat(acc, strings_sb_slice_copy(&e->out, p1, p2)); acc = qoz_string_cat(acc, tuple_block); acc = qoz_string_cat(acc, strings_sb_slice_copy(&e->out, p2, p3)); acc = qoz_string_cat(acc, synth_decl_block); acc = qoz_string_cat(acc, strings_sb_slice_copy(&e->out, p3, total)); qoz_gc_shadow_set_top(_qoz_shadow_guard); return acc;
}

qoz_string emit_register_fn_typedef(qoz_Emitter* e, qoz_Vec__qoz_TypeExpr params, qoz_TypeExpr* ret) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ret);
    qoz_string name = QOZ_STR_LIT("qoz_fn_"); name = qoz_string_cat(name, emit_mangle_type(e, ret)); { qoz_Vec__qoz_TypeExpr __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* p = __col.data[__i]; (void)p; name = qoz_string_cat(name, QOZ_STR_LIT("__")); name = qoz_string_cat(name, emit_mangle_type(e, p)); } }if (!qoz_map_contains__qoz_string__qoz_string(&e->fn_typedefs, name)) { qoz_string fn_field = emit_c_type_for(e, ret); fn_field = qoz_string_cat(fn_field, QOZ_STR_LIT(" (*fn)(void*")); { qoz_Vec__qoz_TypeExpr __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* p = __col.data[__i]; (void)p; fn_field = qoz_string_cat(fn_field, QOZ_STR_LIT(", ")); fn_field = qoz_string_cat(fn_field, emit_c_type_for(e, p)); } }fn_field = qoz_string_cat(fn_field, QOZ_STR_LIT(")")); qoz_string body = QOZ_STR_LIT("typedef struct { void *env; "); body = qoz_string_cat(body, fn_field); body = qoz_string_cat(body, QOZ_STR_LIT("; } ")); body = qoz_string_cat(body, name); body = qoz_string_cat(body, QOZ_STR_LIT(";\n")); qoz_map_set__qoz_string__qoz_string(&e->fn_typedefs, name, body); qoz_vec_push__qoz_string(&e->fn_typedef_order, name); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return name;
}

qoz_string emit_c_type_for(qoz_Emitter* e, qoz_TypeExpr* t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&t);
    qoz_TypeExpr* _qoz_ms_126 = t; qoz_string _qoz_mv_126 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_126->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_126 = (QOZ_STR_LIT("void"));  break; } case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_126->payload.TEPtr.f1; _qoz_mv_126 = (qoz_string_cat(emit_c_type_for(e, inner), QOZ_STR_LIT("*")));  break; } case qoz_TypeExpr_TETuple: { qoz_Vec__qoz_TypeExpr elems = _qoz_ms_126->payload.TETuple.f1; _qoz_mv_126 = (emit_register_tuple_typedef(e, elems));  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_126->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_126->payload.TENamed.f2; _qoz_mv_126 = (emit_c_type_for_named_with_args(e, path, args));  break; } case qoz_TypeExpr_TEFn: { qoz_Vec__qoz_TypeExpr params = _qoz_ms_126->payload.TEFn.f1; qoz_TypeExpr* ret = _qoz_ms_126->payload.TEFn.f2; _qoz_mv_126 = (emit_register_fn_typedef(e, params, ret));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_126;
}

qoz_string emit_register_tuple_typedef(qoz_Emitter* e, qoz_Vec__qoz_TypeExpr elems) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string name = QOZ_STR_LIT("qoz_tuple"); { qoz_Vec__qoz_TypeExpr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* el = __col.data[__i]; (void)el; name = qoz_string_cat(name, QOZ_STR_LIT("__")); name = qoz_string_cat(name, emit_mangle_type(e, el)); } }if (!qoz_map_contains__qoz_string__qoz_string(&e->tuple_typedefs, name)) { qoz_string body = QOZ_STR_LIT("typedef struct { "); int64_t i = 0; { qoz_Vec__qoz_TypeExpr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* el = __col.data[__i]; (void)el; body = qoz_string_cat(body, emit_c_type_for(e, el)); body = qoz_string_cat(body, QOZ_STR_LIT(" _")); body = qoz_string_cat(body, emit_int_to_string(i)); body = qoz_string_cat(body, QOZ_STR_LIT("; ")); i = i + 1; } }body = qoz_string_cat(body, QOZ_STR_LIT("} ")); body = qoz_string_cat(body, name); body = qoz_string_cat(body, QOZ_STR_LIT(";\n")); qoz_map_set__qoz_string__qoz_string(&e->tuple_typedefs, name, body); qoz_vec_push__qoz_string(&e->tuple_typedef_order, name); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return name;
}

qoz_string emit_c_type_for_named_with_args(qoz_Emitter* e, qoz_Vec__qoz_string path, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if ((path.len) != 1) { return QOZ_STR_LIT("int64_t");} qoz_string n = path.data[0]; qoz_string prim = emit_primitive_c_name(n); if (!qoz_string_eq(prim, QOZ_STR_LIT(""))) { return prim;} if (qoz_string_eq(n, QOZ_STR_LIT("cstring"))) { return QOZ_STR_LIT("const char *");} if (qoz_string_eq(n, QOZ_STR_LIT("void"))) { return QOZ_STR_LIT("void");} if (qoz_string_eq(n, QOZ_STR_LIT("unit"))) { return QOZ_STR_LIT("void");} qoz_string base = (((args.len) > 0) ? qoz_string_cat(QOZ_STR_LIT("qoz_"), emit_mangle_inst(e, n, args)) : qoz_string_cat(QOZ_STR_LIT("qoz_"), n)); if (qoz_map_contains__qoz_string__bool(&e->is_enum, n)) { return qoz_string_cat(base, QOZ_STR_LIT("*"));} qoz_gc_shadow_set_top(_qoz_shadow_guard); return base;
}

void emit_collect_closure_captures(qoz_Emitter* e, qoz_Expr* ex, qoz_CaptureScope* sc, qoz_Vec__qoz_string* out) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ex);
    qoz_gc_push_root(&sc);
    qoz_gc_push_root(&out);
    qoz_Expr* _qoz_ms_127 = ex; switch (_qoz_ms_127->tag) { case qoz_Expr_EInt: { NULL;  break; } case qoz_Expr_EFloat: { NULL;  break; } case qoz_Expr_EString: { NULL;  break; } case qoz_Expr_EChar: { NULL;  break; } case qoz_Expr_EBool: { NULL;  break; } case qoz_Expr_ENil: { NULL;  break; } case qoz_Expr_EPath: { NULL;  break; } case qoz_Expr_ESizeOf: { NULL;  break; } case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_127->payload.EIdent.f1; if (!qoz_map_contains__qoz_string__bool(&sc->bound, name)) { if (qoz_map_contains__qoz_string__qoz_TypeExpr(&e->locals, name)) { qoz_vec_push__qoz_string(out, name); } } 0;  break; } case qoz_Expr_EUnary: { qoz_Expr* sub = _qoz_ms_127->payload.EUnary.f2; emit_collect_closure_captures(e, sub, sc, out);  break; } case qoz_Expr_EBinary: { qoz_Expr* l = _qoz_ms_127->payload.EBinary.f2; qoz_Expr* r = _qoz_ms_127->payload.EBinary.f3; {
        emit_collect_closure_captures(e, l, sc, out); emit_collect_closure_captures(e, r, sc, out); 
    }
    0;  break; } case qoz_Expr_EAssign: { qoz_Expr* l = _qoz_ms_127->payload.EAssign.f2; qoz_Expr* r = _qoz_ms_127->payload.EAssign.f3; {
        emit_collect_closure_captures(e, l, sc, out); emit_collect_closure_captures(e, r, sc, out); 
    }
    0;  break; } case qoz_Expr_ECall: { qoz_Expr* callee = _qoz_ms_127->payload.ECall.f1; qoz_Vec__qoz_Expr args = _qoz_ms_127->payload.ECall.f3; {
        emit_collect_closure_captures(e, callee, sc, out); { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; emit_collect_closure_captures(e, a, sc, out); } }
    }
    0;  break; } case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_127->payload.EField.f1; emit_collect_closure_captures(e, base, sc, out);  break; } case qoz_Expr_EIndex: { qoz_Expr* base = _qoz_ms_127->payload.EIndex.f1; qoz_Expr* idx = _qoz_ms_127->payload.EIndex.f2; {
        emit_collect_closure_captures(e, base, sc, out); emit_collect_closure_captures(e, idx, sc, out); 
    }
    0;  break; } case qoz_Expr_ECast: { qoz_Expr* v = _qoz_ms_127->payload.ECast.f1; emit_collect_closure_captures(e, v, sc, out);  break; } case qoz_Expr_ETry: { qoz_Expr* v = _qoz_ms_127->payload.ETry.f1; emit_collect_closure_captures(e, v, sc, out);  break; } case qoz_Expr_EReturn: { qoz_Expr* v = _qoz_ms_127->payload.EReturn.f1; emit_collect_closure_captures(e, v, sc, out);  break; } case qoz_Expr_EDefer: { qoz_Expr* v = _qoz_ms_127->payload.EDefer.f1; emit_collect_closure_captures(e, v, sc, out);  break; } case qoz_Expr_ETuple: { qoz_Vec__qoz_Expr elems = _qoz_ms_127->payload.ETuple.f1; { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; emit_collect_closure_captures(e, el, sc, out); } }0;  break; } case qoz_Expr_ERecord: { qoz_Vec__qoz_RecordFieldLit fields = _qoz_ms_127->payload.ERecord.f2; { qoz_Vec__qoz_RecordFieldLit __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_RecordFieldLit fl = __col.data[__i]; (void)fl; emit_collect_closure_captures(e, fl.value, sc, out); } }0;  break; } case qoz_Expr_EArrayLit: { qoz_Vec__qoz_Expr elems = _qoz_ms_127->payload.EArrayLit.f1; { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; emit_collect_closure_captures(e, el, sc, out); } }0;  break; } case qoz_Expr_EClosure: { NULL;  break; } case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_127->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_127->payload.EBlock.f2; {
        { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* st = __col.data[__i]; (void)st; qoz_Stmt* _qoz_ms_128 = st; switch (_qoz_ms_128->tag) { case qoz_Stmt_SLet: { qoz_string name = _qoz_ms_128->payload.SLet.f1; qoz_Expr* value = _qoz_ms_128->payload.SLet.f3; {
        emit_collect_closure_captures(e, value, sc, out); qoz_map_set__qoz_string__bool(&sc->bound, name, true); 
    }
    0;  break; } case qoz_Stmt_SVar: { qoz_string name = _qoz_ms_128->payload.SVar.f1; qoz_Expr* value = _qoz_ms_128->payload.SVar.f3; {
        emit_collect_closure_captures(e, value, sc, out); qoz_map_set__qoz_string__bool(&sc->bound, name, true); 
    }
    0;  break; } case qoz_Stmt_SExpr: { qoz_Expr* expr = _qoz_ms_128->payload.SExpr.f1; emit_collect_closure_captures(e, expr, sc, out);  break; } } 0; } }emit_collect_closure_captures(e, tail, sc, out); 
    }
    0;  break; } case qoz_Expr_EIf: { qoz_Expr* c = _qoz_ms_127->payload.EIf.f1; qoz_Expr* t = _qoz_ms_127->payload.EIf.f2; qoz_Expr* f = _qoz_ms_127->payload.EIf.f3; {
        emit_collect_closure_captures(e, c, sc, out); emit_collect_closure_captures(e, t, sc, out); emit_collect_closure_captures(e, f, sc, out); 
    }
    0;  break; } case qoz_Expr_EMatch: { qoz_Expr* scrut = _qoz_ms_127->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_127->payload.EMatch.f2; {
        emit_collect_closure_captures(e, scrut, sc, out); { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; emit_collect_closure_captures(e, arm.body, sc, out); } }
    }
    0;  break; } case qoz_Expr_EWhile: { qoz_Expr* c = _qoz_ms_127->payload.EWhile.f1; qoz_Expr* b = _qoz_ms_127->payload.EWhile.f2; {
        emit_collect_closure_captures(e, c, sc, out); emit_collect_closure_captures(e, b, sc, out); 
    }
    0;  break; } case qoz_Expr_EFor: { qoz_string bn = _qoz_ms_127->payload.EFor.f1; qoz_string bn2 = _qoz_ms_127->payload.EFor.f2; qoz_Expr* it = _qoz_ms_127->payload.EFor.f3; qoz_Expr* b = _qoz_ms_127->payload.EFor.f4; {
        emit_collect_closure_captures(e, it, sc, out); qoz_map_set__qoz_string__bool(&sc->bound, bn, true); if (!qoz_string_eq(bn2, QOZ_STR_LIT(""))) { qoz_map_set__qoz_string__bool(&sc->bound, bn2, true); } emit_collect_closure_captures(e, b, sc, out); 
    }
    0;  break; } } 0; 
    return;
}

qoz_string emit_result_mangle_for(qoz_Emitter* e, qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_129 = te; qoz_string _qoz_mv_129 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_129->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_129->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_129->payload.TENamed.f2; qoz_string _qoz_bv_105;
    {
        if ((path.len) == 0) { return QOZ_STR_LIT("");} qoz_string last = path.data[(path.len) - 1]; if (!qoz_string_eq(last, QOZ_STR_LIT("Result"))) { return QOZ_STR_LIT("");} if ((args.len) != 2) { return QOZ_STR_LIT("");} _qoz_bv_105 = emit_mangle_inst(e, QOZ_STR_LIT("Result"), args);
    }
    _qoz_mv_129 = (_qoz_bv_105);  break; } default: { _qoz_mv_129 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_129;
}

void emit_emit_try(qoz_Emitter* e, qoz_Span sp, qoz_Expr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&value);
    qoz_TypeExpr* inner_te = emit_infer_value_te(e, value); qoz_gc_push_root(&inner_te); qoz_TypeExpr* outer_te = e->current_ret_te; qoz_gc_push_root(&outer_te); qoz_string inner_mangle = emit_result_mangle_for(e, inner_te); qoz_string outer_mangle = emit_result_mangle_for(e, outer_te); if (qoz_string_eq(inner_mangle, QOZ_STR_LIT(""))) { emit_emit_die(sp, QOZ_STR_LIT("try operator: operand is not Result<T, E>")); return;} if (qoz_string_eq(outer_mangle, QOZ_STR_LIT(""))) { emit_emit_die(sp, QOZ_STR_LIT("try operator: enclosing function does not return Result<T, E>")); return;} e->closure_counter = e->closure_counter + 1; qoz_string _qoz_bv_106;
    {
        qoz_Strbuf _qoz_sb_954_40 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_954_40); strings_sb_append_i64(&_qoz_sb_954_40, e->closure_counter); _qoz_bv_106 = strings_sb_finish(&_qoz_sb_954_40);
    }
    qoz_string tmp = qoz_string_cat(QOZ_STR_LIT("_qoz_try_"), _qoz_bv_106); int64_t start = strings_sb_len(&e->out); qoz_string _qoz_bv_107;
    {
        qoz_Strbuf _qoz_sb_960_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_960_13); strings_sb_append(&_qoz_sb_960_13, QOZ_STR_LIT("qoz_")); strings_sb_append(&_qoz_sb_960_13, inner_mangle); strings_sb_append(&_qoz_sb_960_13, QOZ_STR_LIT("* ")); strings_sb_append(&_qoz_sb_960_13, tmp); strings_sb_append(&_qoz_sb_960_13, QOZ_STR_LIT(" = ")); _qoz_bv_107 = strings_sb_finish(&_qoz_sb_960_13);
    }
    emit_push(e, _qoz_bv_107); emit_emit_expr(e, value); qoz_string crr = e->current_return_restore; qoz_string _qoz_bv_108;
    {
        qoz_Strbuf _qoz_sb_963_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_963_13); strings_sb_append(&_qoz_sb_963_13, QOZ_STR_LIT("; if (")); strings_sb_append(&_qoz_sb_963_13, tmp); strings_sb_append(&_qoz_sb_963_13, QOZ_STR_LIT("->tag == qoz_")); strings_sb_append(&_qoz_sb_963_13, inner_mangle); strings_sb_append(&_qoz_sb_963_13, QOZ_STR_LIT("_Err) { ")); strings_sb_append(&_qoz_sb_963_13, crr); strings_sb_append(&_qoz_sb_963_13, QOZ_STR_LIT("return qoz_make_")); strings_sb_append(&_qoz_sb_963_13, outer_mangle); strings_sb_append(&_qoz_sb_963_13, QOZ_STR_LIT("_Err(")); strings_sb_append(&_qoz_sb_963_13, tmp); strings_sb_append(&_qoz_sb_963_13, QOZ_STR_LIT("->payload.Err.f0); } ")); _qoz_bv_108 = strings_sb_finish(&_qoz_sb_963_13);
    }
    emit_push(e, _qoz_bv_108); emit_hoist_to_prologue(e, start); qoz_string _qoz_bv_109;
    {
        qoz_Strbuf _qoz_sb_966_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_966_13); strings_sb_append(&_qoz_sb_966_13, tmp); strings_sb_append(&_qoz_sb_966_13, QOZ_STR_LIT("->payload.Ok.f0")); _qoz_bv_109 = strings_sb_finish(&_qoz_sb_966_13);
    }
    emit_push(e, _qoz_bv_109); 
    return;
}

void emit_emit_tuple_lit(qoz_Emitter* e, qoz_Span sp, qoz_Vec__qoz_Expr elems) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_Vec__qoz_TypeExpr elem_tes = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_TypeExpr(&elem_tes, emit_infer_value_te(e, el)); } }qoz_string tname = emit_register_tuple_typedef(e, elem_tes); qoz_string _qoz_bv_110;
    {
        qoz_Strbuf _qoz_sb_973_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_973_13); strings_sb_append(&_qoz_sb_973_13, QOZ_STR_LIT("((")); strings_sb_append(&_qoz_sb_973_13, tname); strings_sb_append(&_qoz_sb_973_13, QOZ_STR_LIT("){ ")); _qoz_bv_110 = strings_sb_finish(&_qoz_sb_973_13);
    }
    emit_push(e, _qoz_bv_110); int64_t i = 0; { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } emit_emit_expr(e, el); i = i + 1; } }emit_push(e, QOZ_STR_LIT(" })")); 
    return;
}

void emit_emit_array_lit_with_hint(qoz_Emitter* e, qoz_Span sp, qoz_Vec__qoz_Expr elems, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&hint);
    qoz_TypeExpr* elem_hint = qoz_make_TypeExpr_TEUnit(sp); qoz_gc_push_root(&elem_hint); qoz_TypeExpr* _qoz_ms_130 = hint; switch (_qoz_ms_130->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_130->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_130->payload.TENamed.f2; if (((path.len) > 0) && ((args.len) >= 1)) { qoz_string last = path.data[(path.len) - 1]; if (qoz_string_eq(last, QOZ_STR_LIT("Vec"))) { elem_hint = args.data[0]; } } 0;  break; } default: { NULL;  break; } } 0; emit_emit_array_lit_using(e, sp, elems, elem_hint); 
    return;
}

void emit_emit_array_lit_using(qoz_Emitter* e, qoz_Span sp, qoz_Vec__qoz_Expr elems, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&hint);
    if (((elems.len) == 0) && emit_is_unit_typeexpr(hint)) { emit_emit_die(sp, QOZ_STR_LIT("empty array literal: element type cannot be inferred")); return;} qoz_TypeExpr* elem_te = hint; qoz_gc_push_root(&elem_te); if (emit_is_unit_typeexpr(elem_te)) { elem_te = emit_infer_value_te(e, elems.data[0]); } qoz_Vec__qoz_TypeExpr elem_args = qoz_vec_make__qoz_TypeExpr(); qoz_vec_push__qoz_TypeExpr(&elem_args, elem_te); qoz_Vec__qoz_string pkg_path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&pkg_path, QOZ_STR_LIT("vec")); qoz_Expr* vec_callee_make = qoz_make_Expr_EField(sp, qoz_make_Expr_EIdent(sp, QOZ_STR_LIT("vec")), QOZ_STR_LIT("make")); qoz_gc_push_root(&vec_callee_make); qoz_Expr* vec_callee_push = qoz_make_Expr_EField(sp, qoz_make_Expr_EIdent(sp, QOZ_STR_LIT("vec")), QOZ_STR_LIT("push")); qoz_gc_push_root(&vec_callee_push); emit_register_fn_instantiation(e, vec_callee_make, elem_args); emit_register_fn_instantiation(e, vec_callee_push, elem_args); qoz_Vec__qoz_string vec_path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&vec_path, QOZ_STR_LIT("Vec")); emit_walk_typeexpr(e, qoz_make_TypeExpr_TENamed(sp, vec_path, elem_args)); qoz_string make_mangled = emit_mangle_inst(e, QOZ_STR_LIT("vec_make"), elem_args); qoz_string push_mangled = emit_mangle_inst(e, QOZ_STR_LIT("vec_push"), elem_args); qoz_string vec_c = qoz_string_cat(QOZ_STR_LIT("qoz_"), emit_mangle_inst(e, QOZ_STR_LIT("Vec"), elem_args)); e->closure_counter = e->closure_counter + 1; qoz_string _qoz_bv_111;
    {
        qoz_Strbuf _qoz_sb_1023_40 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1023_40); strings_sb_append_i64(&_qoz_sb_1023_40, e->closure_counter); _qoz_bv_111 = strings_sb_finish(&_qoz_sb_1023_40);
    }
    qoz_string tmp = qoz_string_cat(QOZ_STR_LIT("_qoz_arr_"), _qoz_bv_111); int64_t start = strings_sb_len(&e->out); qoz_string _qoz_bv_112;
    {
        qoz_Strbuf _qoz_sb_1025_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1025_13); strings_sb_append(&_qoz_sb_1025_13, vec_c); strings_sb_append(&_qoz_sb_1025_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_1025_13, tmp); strings_sb_append(&_qoz_sb_1025_13, QOZ_STR_LIT(" = qoz_")); strings_sb_append(&_qoz_sb_1025_13, make_mangled); strings_sb_append(&_qoz_sb_1025_13, QOZ_STR_LIT("(); ")); _qoz_bv_112 = strings_sb_finish(&_qoz_sb_1025_13);
    }
    emit_push(e, _qoz_bv_112); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_string _qoz_bv_113;
    {
        qoz_Strbuf _qoz_sb_1027_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1027_17); strings_sb_append(&_qoz_sb_1027_17, QOZ_STR_LIT("qoz_")); strings_sb_append(&_qoz_sb_1027_17, push_mangled); strings_sb_append(&_qoz_sb_1027_17, QOZ_STR_LIT("(&")); strings_sb_append(&_qoz_sb_1027_17, tmp); strings_sb_append(&_qoz_sb_1027_17, QOZ_STR_LIT(", ")); _qoz_bv_113 = strings_sb_finish(&_qoz_sb_1027_17);
    }
    emit_push(e, _qoz_bv_113); emit_emit_expr(e, el); emit_push(e, QOZ_STR_LIT("); ")); } }emit_hoist_to_prologue(e, start); emit_push(e, tmp); 
    return;
}

void emit_emit_closure_lifted(qoz_Emitter* e, qoz_Span sp, qoz_Vec__qoz_ClosureParam cps, qoz_TypeExpr* ret, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ret);
    qoz_gc_push_root(&body);
    qoz_CaptureScope sc = ((qoz_CaptureScope){ .bound = qoz_map_make__qoz_string__bool() }); { qoz_Vec__qoz_ClosureParam __col = cps; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_ClosureParam cp = __col.data[__i]; (void)cp; qoz_map_set__qoz_string__bool(&sc.bound, cp.name, true); } }qoz_Vec__qoz_string captures = qoz_vec_make__qoz_string(); emit_collect_closure_captures(e, body, &sc, &captures); int64_t counter = e->closure_counter; e->closure_counter = e->closure_counter + 1; qoz_string _qoz_bv_114;
    {
        qoz_Strbuf _qoz_sb_1043_41 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1043_41); strings_sb_append_i64(&_qoz_sb_1043_41, counter); _qoz_bv_114 = strings_sb_finish(&_qoz_sb_1043_41);
    }
    qoz_string cname = qoz_string_cat(QOZ_STR_LIT("qoz_clo_"), _qoz_bv_114); qoz_string _qoz_bv_115;
    {
        qoz_Strbuf _qoz_sb_1044_50 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1044_50); strings_sb_append_i64(&_qoz_sb_1044_50, counter); _qoz_bv_115 = strings_sb_finish(&_qoz_sb_1044_50);
    }
    qoz_string env_struct = qoz_string_cat(QOZ_STR_LIT("qoz_clo_env_"), _qoz_bv_115); qoz_Vec__qoz_TypeExpr capture_tes = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_string __col = captures; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string c = __col.data[__i]; (void)c; qoz_Option__qoz_TypeExpr* _qoz_ms_131 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->locals, c); switch (_qoz_ms_131->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* te = _qoz_ms_131->payload.Some.f0; qoz_vec_push__qoz_TypeExpr(&capture_tes, te);  break; } case qoz_Option__qoz_TypeExpr_None: { qoz_vec_push__qoz_TypeExpr(&capture_tes, qoz_make_TypeExpr_TEUnit(sp));  break; } } 0; } }qoz_Vec__qoz_TypeExpr ptes = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_ClosureParam __col = cps; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_ClosureParam cp = __col.data[__i]; (void)cp; qoz_vec_push__qoz_TypeExpr(&ptes, cp.ty); } }qoz_string clo_type = emit_register_fn_typedef(e, ptes, ret); qoz_Vec__qoz_FnParam fparams = qoz_vec_make__qoz_FnParam(); { qoz_Vec__qoz_ClosureParam __col = cps; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_ClosureParam cp = __col.data[__i]; (void)cp; qoz_vec_push__qoz_FnParam(&fparams, ((qoz_FnParam){ .name = cp.name, .ty = cp.ty })); } }qoz_string sig = QOZ_STR_LIT("static "); sig = qoz_string_cat(sig, emit_c_type_for(e, ret)); sig = qoz_string_cat(sig, QOZ_STR_LIT(" ")); sig = qoz_string_cat(sig, cname); sig = qoz_string_cat(sig, QOZ_STR_LIT("(void *_qoz_env")); { qoz_Vec__qoz_FnParam __col = fparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam fp = __col.data[__i]; (void)fp; sig = qoz_string_cat(sig, QOZ_STR_LIT(", ")); sig = qoz_string_cat(sig, emit_c_type_for(e, fp.ty)); sig = qoz_string_cat(sig, QOZ_STR_LIT(" ")); sig = qoz_string_cat(sig, fp.name); } }sig = qoz_string_cat(sig, QOZ_STR_LIT(");\n")); qoz_vec_push__qoz_string(&e->synth_fn_decls, sig); if ((captures.len) > 0) { qoz_string env_decl = QOZ_STR_LIT("struct "); env_decl = qoz_string_cat(env_decl, env_struct); env_decl = qoz_string_cat(env_decl, QOZ_STR_LIT(" { ")); int64_t ci = 0; { qoz_Vec__qoz_string __col = captures; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string c = __col.data[__i]; (void)c; env_decl = qoz_string_cat(env_decl, emit_c_type_for(e, capture_tes.data[ci])); env_decl = qoz_string_cat(env_decl, QOZ_STR_LIT(" ")); env_decl = qoz_string_cat(env_decl, c); env_decl = qoz_string_cat(env_decl, QOZ_STR_LIT("; ")); ci = ci + 1; } }env_decl = qoz_string_cat(env_decl, QOZ_STR_LIT("};\n")); qoz_vec_push__qoz_string(&e->synth_fn_decls, env_decl); } int64_t saved_start = strings_sb_len(&e->out); qoz_Map__qoz_string__qoz_TypeExpr saved_locals = e->locals; qoz_TypeExpr* saved_ret = e->current_ret_te; qoz_gc_push_root(&saved_ret); qoz_TypeExpr* saved_hint = e->match_hint; qoz_gc_push_root(&saved_hint); qoz_string ret_c = emit_c_type_for(e, ret); qoz_string _qoz_bv_116;
    {
        qoz_Strbuf _qoz_sb_1103_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1103_13); strings_sb_append(&_qoz_sb_1103_13, QOZ_STR_LIT("static ")); strings_sb_append(&_qoz_sb_1103_13, ret_c); strings_sb_append(&_qoz_sb_1103_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_1103_13, cname); strings_sb_append(&_qoz_sb_1103_13, QOZ_STR_LIT("(void *_qoz_env")); _qoz_bv_116 = strings_sb_finish(&_qoz_sb_1103_13);
    }
    emit_push(e, _qoz_bv_116); { qoz_Vec__qoz_FnParam __col = fparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam fp = __col.data[__i]; (void)fp; qoz_string fp_c = emit_c_type_for(e, fp.ty); qoz_string fp_n = fp.name; qoz_string _qoz_bv_117;
    {
        qoz_Strbuf _qoz_sb_1107_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1107_17); strings_sb_append(&_qoz_sb_1107_17, QOZ_STR_LIT(", ")); strings_sb_append(&_qoz_sb_1107_17, fp_c); strings_sb_append(&_qoz_sb_1107_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_1107_17, fp_n); _qoz_bv_117 = strings_sb_finish(&_qoz_sb_1107_17);
    }
    emit_push(e, _qoz_bv_117); } }emit_push(e, QOZ_STR_LIT(") {\n    ")); e->locals = qoz_map_make__qoz_string__qoz_TypeExpr(); { qoz_Vec__qoz_FnParam __col = fparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam fp = __col.data[__i]; (void)fp; qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, fp.name, fp.ty); } }if ((captures.len) > 0) { qoz_string _qoz_bv_118;
    {
        qoz_Strbuf _qoz_sb_1113_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1113_17); strings_sb_append(&_qoz_sb_1113_17, QOZ_STR_LIT("struct ")); strings_sb_append(&_qoz_sb_1113_17, env_struct); strings_sb_append(&_qoz_sb_1113_17, QOZ_STR_LIT(" *_env = (struct ")); strings_sb_append(&_qoz_sb_1113_17, env_struct); strings_sb_append(&_qoz_sb_1113_17, QOZ_STR_LIT(" *)_qoz_env;\n    ")); _qoz_bv_118 = strings_sb_finish(&_qoz_sb_1113_17);
    }
    emit_push(e, _qoz_bv_118); int64_t ci2 = 0; { qoz_Vec__qoz_string __col = captures; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string c = __col.data[__i]; (void)c; qoz_string c_c = emit_c_type_for(e, capture_tes.data[ci2]); qoz_string _qoz_bv_119;
    {
        qoz_Strbuf _qoz_sb_1117_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1117_21); strings_sb_append(&_qoz_sb_1117_21, c_c); strings_sb_append(&_qoz_sb_1117_21, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_1117_21, c); strings_sb_append(&_qoz_sb_1117_21, QOZ_STR_LIT(" = _env->")); strings_sb_append(&_qoz_sb_1117_21, c); strings_sb_append(&_qoz_sb_1117_21, QOZ_STR_LIT(";\n    ")); _qoz_bv_119 = strings_sb_finish(&_qoz_sb_1117_21);
    }
    emit_push(e, _qoz_bv_119); qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, c, capture_tes.data[ci2]); ci2 = ci2 + 1; } }} e->current_ret_te = ret; if (emit_is_block(body)) { emit_emit_fn_body_block(e, body, ret); }  else { qoz_StmtScope saved = emit_open_statement_scope(e); if (emit_is_unit_typeexpr(ret)) { emit_emit_expr(e, body); emit_push(e, QOZ_STR_LIT(";\n    return;\n")); }  else { emit_push(e, QOZ_STR_LIT("return ")); emit_emit_value_with_hint(e, body, ret); emit_push(e, QOZ_STR_LIT(";\n")); } emit_close_statement_scope(e, saved); } emit_push(e, QOZ_STR_LIT("}\n\n")); qoz_string def = strings_sb_slice_copy(&e->out, saved_start, strings_sb_len(&e->out)); strings_sb_truncate(&e->out, saved_start); e->locals = saved_locals; e->current_ret_te = saved_ret; e->match_hint = saved_hint; qoz_vec_push__qoz_string(&e->synth_fn_defs, def); qoz_string _qoz_bv_120;
    {
        qoz_Strbuf _qoz_sb_1149_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1149_13); strings_sb_append(&_qoz_sb_1149_13, QOZ_STR_LIT("((")); strings_sb_append(&_qoz_sb_1149_13, clo_type); strings_sb_append(&_qoz_sb_1149_13, QOZ_STR_LIT("){ .env = ")); _qoz_bv_120 = strings_sb_finish(&_qoz_sb_1149_13);
    }
    emit_push(e, _qoz_bv_120); if ((captures.len) == 0) { emit_push(e, QOZ_STR_LIT("NULL")); }  else { qoz_string _qoz_bv_121;
    {
        qoz_Strbuf _qoz_sb_1155_50 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1155_50); strings_sb_append_i64(&_qoz_sb_1155_50, counter); _qoz_bv_121 = strings_sb_finish(&_qoz_sb_1155_50);
    }
    qoz_string env_local = qoz_string_cat(QOZ_STR_LIT("_qoz_env_"), _qoz_bv_121); int64_t start = strings_sb_len(&e->out); qoz_string _qoz_bv_122;
    {
        qoz_Strbuf _qoz_sb_1157_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1157_17); strings_sb_append(&_qoz_sb_1157_17, QOZ_STR_LIT("struct ")); strings_sb_append(&_qoz_sb_1157_17, env_struct); strings_sb_append(&_qoz_sb_1157_17, QOZ_STR_LIT(" *")); strings_sb_append(&_qoz_sb_1157_17, env_local); strings_sb_append(&_qoz_sb_1157_17, QOZ_STR_LIT(" = qoz_alloc(sizeof(struct ")); strings_sb_append(&_qoz_sb_1157_17, env_struct); strings_sb_append(&_qoz_sb_1157_17, QOZ_STR_LIT(")); ")); _qoz_bv_122 = strings_sb_finish(&_qoz_sb_1157_17);
    }
    emit_push(e, _qoz_bv_122); { qoz_Vec__qoz_string __col = captures; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string c = __col.data[__i]; (void)c; qoz_string _qoz_bv_123;
    {
        qoz_Strbuf _qoz_sb_1159_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1159_21); strings_sb_append(&_qoz_sb_1159_21, env_local); strings_sb_append(&_qoz_sb_1159_21, QOZ_STR_LIT("->")); strings_sb_append(&_qoz_sb_1159_21, c); strings_sb_append(&_qoz_sb_1159_21, QOZ_STR_LIT(" = ")); strings_sb_append(&_qoz_sb_1159_21, c); strings_sb_append(&_qoz_sb_1159_21, QOZ_STR_LIT("; ")); _qoz_bv_123 = strings_sb_finish(&_qoz_sb_1159_21);
    }
    emit_push(e, _qoz_bv_123); } }emit_hoist_to_prologue(e, start); qoz_string _qoz_bv_124;
    {
        qoz_Strbuf _qoz_sb_1162_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1162_17); strings_sb_append(&_qoz_sb_1162_17, QOZ_STR_LIT("(void *)")); strings_sb_append(&_qoz_sb_1162_17, env_local); _qoz_bv_124 = strings_sb_finish(&_qoz_sb_1162_17);
    }
    emit_push(e, _qoz_bv_124); } qoz_string _qoz_bv_125;
    {
        qoz_Strbuf _qoz_sb_1164_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1164_13); strings_sb_append(&_qoz_sb_1164_13, QOZ_STR_LIT(", .fn = ")); strings_sb_append(&_qoz_sb_1164_13, cname); strings_sb_append(&_qoz_sb_1164_13, QOZ_STR_LIT(" })")); _qoz_bv_125 = strings_sb_finish(&_qoz_sb_1164_13);
    }
    emit_push(e, _qoz_bv_125); 
    return;
}

void emit_emit_expr(qoz_Emitter* e, qoz_Expr* ex) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_132 = ex; switch (_qoz_ms_132->tag) { case qoz_Expr_EInt: { qoz_string text = _qoz_ms_132->payload.EInt.f1; emit_push(e, emit_strip_numeric_underscores(text));  break; } case qoz_Expr_EFloat: { qoz_string text = _qoz_ms_132->payload.EFloat.f1; emit_push(e, emit_strip_numeric_underscores(text));  break; } case qoz_Expr_EBool: { bool v = _qoz_ms_132->payload.EBool.f1; emit_push(e, ((v) ? QOZ_STR_LIT("true") : QOZ_STR_LIT("false")));  break; } case qoz_Expr_ENil: { emit_push(e, QOZ_STR_LIT("NULL"));  break; } case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_132->payload.EIdent.f1; qoz_Option__qoz_string* _qoz_ms_133 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); switch (_qoz_ms_133->tag) { case qoz_Option__qoz_string_Some: { qoz_string enum_name = _qoz_ms_133->payload.Some.f0; qoz_string _qoz_bv_126;
    {
        qoz_Strbuf _qoz_sb_1176_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1176_21); strings_sb_append(&_qoz_sb_1176_21, QOZ_STR_LIT("qoz_make_")); strings_sb_append(&_qoz_sb_1176_21, enum_name); strings_sb_append(&_qoz_sb_1176_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_1176_21, name); strings_sb_append(&_qoz_sb_1176_21, QOZ_STR_LIT("()")); _qoz_bv_126 = strings_sb_finish(&_qoz_sb_1176_21);
    }
    emit_push(e, _qoz_bv_126);  break; } case qoz_Option__qoz_string_None: { emit_push(e, name);  break; } } 0;  break; } case qoz_Expr_EString: { qoz_string text = _qoz_ms_132->payload.EString.f1; qoz_string _qoz_bv_127;
    {
        qoz_Strbuf _qoz_sb_1181_36 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1181_36); strings_sb_append(&_qoz_sb_1181_36, QOZ_STR_LIT("QOZ_STR_LIT(")); strings_sb_append(&_qoz_sb_1181_36, text); strings_sb_append(&_qoz_sb_1181_36, QOZ_STR_LIT(")")); _qoz_bv_127 = strings_sb_finish(&_qoz_sb_1181_36);
    }
    emit_push(e, _qoz_bv_127);  break; } case qoz_Expr_EBinary: { qoz_BinaryOp* op = _qoz_ms_132->payload.EBinary.f1; qoz_Expr* lhs = _qoz_ms_132->payload.EBinary.f2; qoz_Expr* rhs = _qoz_ms_132->payload.EBinary.f3; emit_emit_binary(e, op, lhs, rhs);  break; } case qoz_Expr_EUnary: { qoz_UnaryOp* op = _qoz_ms_132->payload.EUnary.f1; qoz_Expr* rhs = _qoz_ms_132->payload.EUnary.f2; {
        qoz_string op_text = emit_unary_op_text(op); if (!qoz_string_eq(op_text, QOZ_STR_LIT(""))) { qoz_TypeExpr* rhs_te = emit_infer_value_te(e, rhs); qoz_gc_push_root(&rhs_te); qoz_string tname = emit_operator_first_param_type_name(rhs_te); if (!qoz_string_eq(tname, QOZ_STR_LIT(""))) { qoz_string key = qoz_string_cat(qoz_string_cat(op_text, QOZ_STR_LIT("::")), tname); qoz_Option__qoz_string* _qoz_ms_134 = qoz_map_get__qoz_string__qoz_string(&e->op_dispatch, key); switch (_qoz_ms_134->tag) { case qoz_Option__qoz_string_Some: { qoz_string fn_name = _qoz_ms_134->payload.Some.f0; {
        qoz_string ct = emit_c_type_for(e, rhs_te); qoz_Option__qoz_TypeExpr* _qoz_ms_135 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->fn_returns, fn_name); qoz_TypeExpr* _qoz_mv_135 = NULL; switch (_qoz_ms_135->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* t = _qoz_ms_135->payload.Some.f0; _qoz_mv_135 = (t);  break; } case qoz_Option__qoz_TypeExpr_None: { _qoz_mv_135 = (qoz_make_TypeExpr_TEUnit(((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 })));  break; } } qoz_TypeExpr* ret_te = _qoz_mv_135; qoz_gc_push_root(&ret_te); qoz_string ret_ct = emit_c_type_for(e, ret_te); emit_ensure_unary_byval_dispatch_helper(e, fn_name, ct, ret_ct); qoz_string _qoz_bv_128;
    {
        qoz_Strbuf _qoz_sb_1202_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1202_29); strings_sb_append(&_qoz_sb_1202_29, QOZ_STR_LIT("_qoz_byval_")); strings_sb_append(&_qoz_sb_1202_29, fn_name); strings_sb_append(&_qoz_sb_1202_29, QOZ_STR_LIT("(")); _qoz_bv_128 = strings_sb_finish(&_qoz_sb_1202_29);
    }
    emit_push(e, _qoz_bv_128); emit_emit_expr(e, rhs); emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } default: { NULL;  break; } } 0; } } emit_push(e, emit_unary_c_op(op)); qoz_Expr* _qoz_ms_136 = rhs; switch (_qoz_ms_136->tag) { case qoz_Expr_EBinary: { {
        emit_push(e, QOZ_STR_LIT("(")); emit_emit_expr(e, rhs); emit_push(e, QOZ_STR_LIT(")")); 
    }
    0;  break; } default: { emit_emit_expr(e, rhs);  break; } } 0; 
    }
    0;  break; } case qoz_Expr_EReturn: { qoz_Expr* value = _qoz_ms_132->payload.EReturn.f1; if (emit_is_nil_expr(value)) { emit_push(e, QOZ_STR_LIT("return")); }  else { emit_push(e, QOZ_STR_LIT("return ")); emit_emit_value_with_hint(e, value, e->current_ret_te); } 0;  break; } case qoz_Expr_EBlock: { qoz_Span sp = _qoz_ms_132->payload.EBlock.f0; qoz_Vec__qoz_Stmt stmts = _qoz_ms_132->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_132->payload.EBlock.f2; {
        if ((stmts.len) == 0) { emit_emit_block_tail_as_value(e, tail); return;} qoz_Expr* _qoz_ms_137 = tail; switch (_qoz_ms_137->tag) { case qoz_Expr_EWhile: { {
        int64_t start_s = strings_sb_len(&e->out); emit_push(e, QOZ_STR_LIT("{\n        ")); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); emit_push(e, QOZ_STR_LIT("\n    }\n    ")); emit_hoist_to_prologue(e, start_s); emit_push(e, QOZ_STR_LIT("0")); return;
    }
    0;  break; } case qoz_Expr_EFor: { {
        int64_t start_s = strings_sb_len(&e->out); emit_push(e, QOZ_STR_LIT("{\n        ")); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); emit_push(e, QOZ_STR_LIT("\n    }\n    ")); emit_hoist_to_prologue(e, start_s); emit_push(e, QOZ_STR_LIT("0")); return;
    }
    0;  break; } case qoz_Expr_EAssign: { {
        int64_t start_s = strings_sb_len(&e->out); emit_push(e, QOZ_STR_LIT("{\n        ")); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); emit_push(e, QOZ_STR_LIT("\n    }\n    ")); emit_hoist_to_prologue(e, start_s); emit_push(e, QOZ_STR_LIT("0")); return;
    }
    0;  break; } case qoz_Expr_EDefer: { {
        int64_t start_s = strings_sb_len(&e->out); emit_push(e, QOZ_STR_LIT("{\n        ")); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); emit_push(e, QOZ_STR_LIT("\n    }\n    ")); emit_hoist_to_prologue(e, start_s); emit_push(e, QOZ_STR_LIT("0")); return;
    }
    0;  break; } case qoz_Expr_EReturn: { {
        int64_t start_s = strings_sb_len(&e->out); emit_push(e, QOZ_STR_LIT("{\n        ")); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); emit_push(e, QOZ_STR_LIT("\n    }\n    ")); emit_hoist_to_prologue(e, start_s); emit_push(e, QOZ_STR_LIT("0")); return;
    }
    0;  break; } default: { NULL;  break; } } 0; qoz_TypeExpr* tail_te = emit_infer_value_te(e, tail); qoz_gc_push_root(&tail_te); if (emit_is_unit_typeexpr(tail_te)) { int64_t start_u = strings_sb_len(&e->out); emit_push(e, QOZ_STR_LIT("{\n        ")); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }qoz_StmtScope saved_tail = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved_tail); emit_push(e, QOZ_STR_LIT("\n    }\n    ")); emit_hoist_to_prologue(e, start_u); emit_push(e, QOZ_STR_LIT("0")); return;} e->closure_counter = e->closure_counter + 1; int64_t counter = e->closure_counter; qoz_string _qoz_bv_129;
    {
        qoz_Strbuf _qoz_sb_1275_47 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1275_47); strings_sb_append_i64(&_qoz_sb_1275_47, counter); _qoz_bv_129 = strings_sb_finish(&_qoz_sb_1275_47);
    }
    qoz_string tmp_res = qoz_string_cat(QOZ_STR_LIT("_qoz_bv_"), _qoz_bv_129); qoz_string result_c = emit_c_type_for(e, tail_te); int64_t start = strings_sb_len(&e->out); qoz_string _qoz_bv_130;
    {
        qoz_Strbuf _qoz_sb_1278_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1278_17); strings_sb_append(&_qoz_sb_1278_17, result_c); strings_sb_append(&_qoz_sb_1278_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_1278_17, tmp_res); strings_sb_append(&_qoz_sb_1278_17, QOZ_STR_LIT(";\n    {\n        ")); _qoz_bv_130 = strings_sb_finish(&_qoz_sb_1278_17);
    }
    emit_push(e, _qoz_bv_130); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }qoz_StmtScope saved_inner = emit_open_statement_scope(e); qoz_string _qoz_bv_131;
    {
        qoz_Strbuf _qoz_sb_1281_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1281_17); strings_sb_append(&_qoz_sb_1281_17, tmp_res); strings_sb_append(&_qoz_sb_1281_17, QOZ_STR_LIT(" = ")); _qoz_bv_131 = strings_sb_finish(&_qoz_sb_1281_17);
    }
    emit_push(e, _qoz_bv_131); emit_emit_expr(e, tail); emit_push(e, QOZ_STR_LIT(";\n    ")); emit_close_statement_scope(e, saved_inner); emit_push(e, QOZ_STR_LIT("}\n    ")); emit_hoist_to_prologue(e, start); emit_push(e, tmp_res); 
    }
    0;  break; } case qoz_Expr_EChar: { qoz_string text = _qoz_ms_132->payload.EChar.f1; emit_push(e, text);  break; } case qoz_Expr_EPath: { emit_push(e, QOZ_STR_LIT("0"));  break; } case qoz_Expr_EAssign: { qoz_Expr* lhs = _qoz_ms_132->payload.EAssign.f2; qoz_Expr* rhs = _qoz_ms_132->payload.EAssign.f3; emit_emit_assign(e, lhs, rhs);  break; } case qoz_Expr_ECall: { qoz_Expr* callee = _qoz_ms_132->payload.ECall.f1; qoz_Vec__qoz_TypeExpr ta = _qoz_ms_132->payload.ECall.f2; qoz_Vec__qoz_Expr args = _qoz_ms_132->payload.ECall.f3; emit_emit_call(e, callee, ta, args);  break; } case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_132->payload.EField.f1; qoz_string name = _qoz_ms_132->payload.EField.f2; emit_emit_field(e, base, name);  break; } case qoz_Expr_EIndex: { qoz_Expr* base = _qoz_ms_132->payload.EIndex.f1; qoz_Expr* idx = _qoz_ms_132->payload.EIndex.f2; emit_emit_index(e, base, idx);  break; } case qoz_Expr_ECast: { qoz_Expr* value = _qoz_ms_132->payload.ECast.f1; qoz_TypeExpr* t = _qoz_ms_132->payload.ECast.f2; {
        qoz_string tc = emit_c_type_for(e, t); qoz_string _qoz_bv_132;
    {
        qoz_Strbuf _qoz_sb_1297_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1297_17); strings_sb_append(&_qoz_sb_1297_17, QOZ_STR_LIT("((")); strings_sb_append(&_qoz_sb_1297_17, tc); strings_sb_append(&_qoz_sb_1297_17, QOZ_STR_LIT(")")); _qoz_bv_132 = strings_sb_finish(&_qoz_sb_1297_17);
    }
    emit_push(e, _qoz_bv_132); emit_emit_expr(e, value); emit_push(e, QOZ_STR_LIT(")")); 
    }
    0;  break; } case qoz_Expr_ETry: { qoz_Span sp = _qoz_ms_132->payload.ETry.f0; qoz_Expr* value = _qoz_ms_132->payload.ETry.f1; emit_emit_try(e, sp, value);  break; } case qoz_Expr_ETuple: { qoz_Span sp = _qoz_ms_132->payload.ETuple.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_132->payload.ETuple.f1; emit_emit_tuple_lit(e, sp, elems);  break; } case qoz_Expr_ERecord: { qoz_TypeExpr* te = _qoz_ms_132->payload.ERecord.f1; qoz_Vec__qoz_RecordFieldLit fields = _qoz_ms_132->payload.ERecord.f2; emit_emit_record_lit_with_hint(e, te, fields, te);  break; } case qoz_Expr_EClosure: { qoz_Span sp = _qoz_ms_132->payload.EClosure.f0; qoz_Vec__qoz_ClosureParam cps = _qoz_ms_132->payload.EClosure.f1; qoz_TypeExpr* ret = _qoz_ms_132->payload.EClosure.f2; qoz_Expr* body = _qoz_ms_132->payload.EClosure.f3; emit_emit_closure_lifted(e, sp, cps, ret, body);  break; } case qoz_Expr_EIf: { qoz_Expr* c = _qoz_ms_132->payload.EIf.f1; qoz_Expr* t = _qoz_ms_132->payload.EIf.f2; qoz_Expr* f = _qoz_ms_132->payload.EIf.f3; {
        emit_push(e, QOZ_STR_LIT("(")); emit_emit_expr(e, c); emit_push(e, QOZ_STR_LIT(" ? ")); emit_emit_expr(e, t); emit_push(e, QOZ_STR_LIT(" : ")); emit_emit_expr(e, f); emit_push(e, QOZ_STR_LIT(")")); 
    }
    0;  break; } case qoz_Expr_EMatch: { qoz_Span span = _qoz_ms_132->payload.EMatch.f0; qoz_Expr* scrut = _qoz_ms_132->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_132->payload.EMatch.f2; {
        qoz_TypeExpr* saved = e->match_hint; qoz_gc_push_root(&saved); e->match_hint = qoz_make_TypeExpr_TEUnit(((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 })); emit_emit_match_as_expr(e, span, scrut, arms); e->match_hint = saved; 
    }
    0;  break; } case qoz_Expr_EWhile: { emit_push(e, QOZ_STR_LIT("0"));  break; } case qoz_Expr_EFor: { emit_push(e, QOZ_STR_LIT("0"));  break; } case qoz_Expr_EDefer: { emit_push(e, QOZ_STR_LIT("0"));  break; } case qoz_Expr_ESizeOf: { qoz_TypeExpr* te = _qoz_ms_132->payload.ESizeOf.f1; {
        qoz_string tc = emit_c_type_for(e, te); qoz_string _qoz_bv_133;
    {
        qoz_Strbuf _qoz_sb_1315_77 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1315_77); strings_sb_append(&_qoz_sb_1315_77, QOZ_STR_LIT("(int64_t)sizeof(")); strings_sb_append(&_qoz_sb_1315_77, tc); strings_sb_append(&_qoz_sb_1315_77, QOZ_STR_LIT(")")); _qoz_bv_133 = strings_sb_finish(&_qoz_sb_1315_77);
    }
    emit_push(e, _qoz_bv_133); 
    }
    0;  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_132->payload.EArrayLit.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_132->payload.EArrayLit.f1; emit_emit_array_lit_using(e, sp, elems, qoz_make_TypeExpr_TEUnit(sp));  break; } } 0; 
    return;
}

qoz_string emit_infer_value_ctype(qoz_Emitter* e, qoz_TypeExpr* ty, qoz_Expr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ty);
    qoz_gc_push_root(&value);
    qoz_TypeExpr* _qoz_ms_138 = ty; qoz_string _qoz_mv_138 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_138->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_138 = (emit_c_type_for(e, emit_binding_te(e, ty, value)));  break; } default: { _qoz_mv_138 = (emit_c_type_for(e, ty));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_138;
}

qoz_string emit_infer_expr_ctype(qoz_Emitter* e, qoz_Expr* ex) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_139 = ex; qoz_string _qoz_mv_139 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_139->tag) { case qoz_Expr_EAssign: { _qoz_mv_139 = (QOZ_STR_LIT("void"));  break; } case qoz_Expr_EReturn: { _qoz_mv_139 = (QOZ_STR_LIT("void"));  break; } case qoz_Expr_EWhile: { _qoz_mv_139 = (QOZ_STR_LIT("void"));  break; } case qoz_Expr_EFor: { _qoz_mv_139 = (QOZ_STR_LIT("void"));  break; } case qoz_Expr_EDefer: { _qoz_mv_139 = (QOZ_STR_LIT("void"));  break; } case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_139->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_139->payload.EBlock.f2; qoz_string _qoz_bv_134;
    {
        { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_ingest_stmt_locals(e, s); } }if (emit_is_nil_expr(tail)) { return QOZ_STR_LIT("void");} _qoz_bv_134 = emit_infer_expr_ctype(e, tail);
    }
    _qoz_mv_139 = (_qoz_bv_134);  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_139->payload.EMatch.f0; qoz_Expr* scrut = _qoz_ms_139->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_139->payload.EMatch.f2; qoz_string _qoz_bv_135;
    {
        if ((arms.len) == 0) { emit_emit_die(sp, QOZ_STR_LIT("match expression with no arms")); } qoz_string bare_enum = emit_bare_enum_for_pat(e, arms.data[0].pat); qoz_string enum_name = ((qoz_string_eq(bare_enum, QOZ_STR_LIT(""))) ? bare_enum : emit_enum_lookup_name(e, scrut, bare_enum)); emit_bind_arm_locals(e, enum_name, arms.data[0].pat); _qoz_bv_135 = emit_infer_expr_ctype(e, arms.data[0].body);
    }
    _qoz_mv_139 = (_qoz_bv_135);  break; } case qoz_Expr_EPath: { qoz_Span sp = _qoz_ms_139->payload.EPath.f0; _qoz_mv_139 = (emit_emit_die(sp, QOZ_STR_LIT("EPath in value position is not supported")));  break; } default: { _qoz_mv_139 = (emit_c_type_for(e, emit_infer_value_te(e, ex)));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_139;
}

void emit_ingest_stmt_locals(qoz_Emitter* e, qoz_Stmt* s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&s);
    qoz_Stmt* _qoz_ms_140 = s; switch (_qoz_ms_140->tag) { case qoz_Stmt_SLet: { qoz_string name = _qoz_ms_140->payload.SLet.f1; qoz_TypeExpr* te = _qoz_ms_140->payload.SLet.f2; qoz_Expr* val = _qoz_ms_140->payload.SLet.f3; qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, name, emit_binding_te(e, te, val));  break; } case qoz_Stmt_SVar: { qoz_string name = _qoz_ms_140->payload.SVar.f1; qoz_TypeExpr* te = _qoz_ms_140->payload.SVar.f2; qoz_Expr* val = _qoz_ms_140->payload.SVar.f3; qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, name, emit_binding_te(e, te, val));  break; } case qoz_Stmt_SExpr: { NULL;  break; } } 0; 
    return;
}

qoz_string emit_bare_enum_for_pat(qoz_Emitter* e, qoz_Pattern* pat) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&pat);
    qoz_Pattern* _qoz_ms_141 = pat; qoz_string _qoz_mv_141 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_141->tag) { case qoz_Pattern_PatVariant: { qoz_Vec__qoz_string path = _qoz_ms_141->payload.PatVariant.f1; qoz_string _qoz_bv_136;
    {
        if ((path.len) == 0) { return QOZ_STR_LIT("");} qoz_Option__qoz_string* _qoz_ms_142 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, path.data[(path.len) - 1]); qoz_string _qoz_mv_142 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_142->tag) { case qoz_Option__qoz_string_Some: { qoz_string en = _qoz_ms_142->payload.Some.f0; _qoz_mv_142 = (en);  break; } case qoz_Option__qoz_string_None: { _qoz_mv_142 = (QOZ_STR_LIT(""));  break; } } _qoz_bv_136 = _qoz_mv_142;
    }
    _qoz_mv_141 = (_qoz_bv_136);  break; } case qoz_Pattern_PatBind: { qoz_string name = _qoz_ms_141->payload.PatBind.f1; qoz_Option__qoz_string* _qoz_ms_143 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); qoz_string _qoz_mv_143 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_143->tag) { case qoz_Option__qoz_string_Some: { qoz_string en = _qoz_ms_143->payload.Some.f0; _qoz_mv_143 = (en);  break; } case qoz_Option__qoz_string_None: { _qoz_mv_143 = (QOZ_STR_LIT(""));  break; } } _qoz_mv_141 = (_qoz_mv_143);  break; } default: { _qoz_mv_141 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_141;
}

qoz_string emit_emit_die(qoz_Span sp, qoz_string msg) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_string _qoz_bv_137;
    {
        qoz_Strbuf _qoz_sb_1381_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1381_17); strings_sb_append(&_qoz_sb_1381_17, sp.file); strings_sb_append(&_qoz_sb_1381_17, QOZ_STR_LIT(":")); strings_sb_append_i64(&_qoz_sb_1381_17, sp.line); strings_sb_append(&_qoz_sb_1381_17, QOZ_STR_LIT(":")); strings_sb_append_i64(&_qoz_sb_1381_17, sp.col); strings_sb_append(&_qoz_sb_1381_17, QOZ_STR_LIT(": emit error: ")); strings_sb_append(&_qoz_sb_1381_17, msg); _qoz_bv_137 = strings_sb_finish(&_qoz_sb_1381_17);
    }
    fmt_println(_qoz_bv_137); qoz_os_exit(1); qoz_gc_shadow_set_top(_qoz_shadow_guard); return QOZ_STR_LIT("");
}

qoz_TypeExpr* emit_infer_base_typeexpr(qoz_Emitter* e, qoz_Expr* base) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&base);
    qoz_Expr* _qoz_ms_144 = base; qoz_TypeExpr* _qoz_mv_144 = NULL; switch (_qoz_ms_144->tag) { case qoz_Expr_EIdent: { qoz_Span sp = _qoz_ms_144->payload.EIdent.f0; qoz_string ident = _qoz_ms_144->payload.EIdent.f1; qoz_Option__qoz_TypeExpr* _qoz_ms_145 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->locals, ident); qoz_TypeExpr* _qoz_mv_145 = NULL; switch (_qoz_ms_145->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* te = _qoz_ms_145->payload.Some.f0; _qoz_mv_145 = (te);  break; } case qoz_Option__qoz_TypeExpr_None: { qoz_Option__qoz_string* _qoz_ms_146 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, ident); qoz_TypeExpr* _qoz_mv_146 = NULL; switch (_qoz_ms_146->tag) { case qoz_Option__qoz_string_Some: { qoz_string en = _qoz_ms_146->payload.Some.f0; qoz_TypeExpr* _qoz_bv_138;
    {
        qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, en); _qoz_bv_138 = qoz_make_TypeExpr_TENamed(sp, path, qoz_vec_make__qoz_TypeExpr());
    }
    _qoz_mv_146 = (_qoz_bv_138);  break; } case qoz_Option__qoz_string_None: { qoz_TypeExpr* _qoz_bv_139;
    {
        qoz_string _qoz_bv_140;
    {
        qoz_Strbuf _qoz_sb_1399_38 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1399_38); strings_sb_append(&_qoz_sb_1399_38, QOZ_STR_LIT("identifier `")); strings_sb_append(&_qoz_sb_1399_38, ident); strings_sb_append(&_qoz_sb_1399_38, QOZ_STR_LIT("` has no known TypeExpr (not a local, parameter, or variant)")); _qoz_bv_140 = strings_sb_finish(&_qoz_sb_1399_38);
    }
    (void)(emit_emit_die(sp, _qoz_bv_140)); _qoz_bv_139 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_146 = (_qoz_bv_139);  break; } } _qoz_mv_145 = (_qoz_mv_146);  break; } } _qoz_mv_144 = (_qoz_mv_145);  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_144->payload.EField.f0; qoz_Expr* b = _qoz_ms_144->payload.EField.f1; qoz_string n = _qoz_ms_144->payload.EField.f2; qoz_TypeExpr* _qoz_bv_141;
    {
        qoz_TypeExpr* bte = emit_infer_base_typeexpr(e, b); qoz_gc_push_root(&bte); _qoz_bv_141 = emit_field_typeexpr_for(e, bte, n, sp);
    }
    _qoz_mv_144 = (_qoz_bv_141);  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_144->payload.EUnary.f0; qoz_UnaryOp* op = _qoz_ms_144->payload.EUnary.f1; qoz_Expr* rhs = _qoz_ms_144->payload.EUnary.f2; qoz_UnaryOp* _qoz_ms_147 = op; qoz_TypeExpr* _qoz_mv_147 = NULL; switch (_qoz_ms_147->tag) { case qoz_UnaryOp_UOpDeref: { qoz_TypeExpr* _qoz_bv_142;
    {
        qoz_TypeExpr* inner = emit_infer_base_typeexpr(e, rhs); qoz_gc_push_root(&inner); qoz_TypeExpr* _qoz_ms_148 = inner; qoz_TypeExpr* _qoz_mv_148 = NULL; switch (_qoz_ms_148->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* t = _qoz_ms_148->payload.TEPtr.f1; _qoz_mv_148 = (t);  break; } default: { qoz_TypeExpr* _qoz_bv_143;
    {
        (void)(emit_emit_die(sp, QOZ_STR_LIT("dereference of non-pointer base in EField/EIndex"))); _qoz_bv_143 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_148 = (_qoz_bv_143);  break; } } _qoz_bv_142 = _qoz_mv_148;
    }
    _qoz_mv_147 = (_qoz_bv_142);  break; } case qoz_UnaryOp_UOpAddr: { _qoz_mv_147 = (qoz_make_TypeExpr_TEPtr(sp, emit_infer_base_typeexpr(e, rhs)));  break; } default: { qoz_TypeExpr* _qoz_bv_144;
    {
        (void)(emit_emit_die(sp, QOZ_STR_LIT("unary operator does not yield a base TypeExpr"))); _qoz_bv_144 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_147 = (_qoz_bv_144);  break; } } _qoz_mv_144 = (_qoz_mv_147);  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_144->payload.ECall.f0; qoz_Expr* c = _qoz_ms_144->payload.ECall.f1; qoz_Vec__qoz_TypeExpr ta = _qoz_ms_144->payload.ECall.f2; qoz_Vec__qoz_Expr va = _qoz_ms_144->payload.ECall.f3; _qoz_mv_144 = (emit_call_return_te(e, c, ta, va, sp));  break; } case qoz_Expr_ECast: { qoz_TypeExpr* t = _qoz_ms_144->payload.ECast.f2; _qoz_mv_144 = (t);  break; } case qoz_Expr_ERecord: { qoz_TypeExpr* t = _qoz_ms_144->payload.ERecord.f1; _qoz_mv_144 = (t);  break; } case qoz_Expr_EIndex: { qoz_Span sp = _qoz_ms_144->payload.EIndex.f0; qoz_Expr* b = _qoz_ms_144->payload.EIndex.f1; qoz_TypeExpr* _qoz_bv_145;
    {
        qoz_TypeExpr* bte = emit_infer_base_typeexpr(e, b); qoz_gc_push_root(&bte); qoz_TypeExpr* _qoz_ms_149 = bte; qoz_TypeExpr* _qoz_mv_149 = NULL; switch (_qoz_ms_149->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_149->payload.TEPtr.f1; _qoz_mv_149 = (inner);  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_149->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_149->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_146;
    {
        if ((((args.len) >= 1) && ((path.len) >= 1)) && qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Vec"))) { return args.data[0];} (void)(emit_emit_die(sp, QOZ_STR_LIT("indexing into non-Vec/non-pointer base"))); _qoz_bv_146 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_149 = (_qoz_bv_146);  break; } default: { qoz_TypeExpr* _qoz_bv_147;
    {
        (void)(emit_emit_die(sp, QOZ_STR_LIT("indexing into unrecognised base"))); _qoz_bv_147 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_149 = (_qoz_bv_147);  break; } } _qoz_bv_145 = _qoz_mv_149;
    }
    _qoz_mv_144 = (_qoz_bv_145);  break; } default: { qoz_TypeExpr* _qoz_bv_148;
    {
        (void)(emit_emit_die(emit_span_of_expr(base), QOZ_STR_LIT("base expression has no resolvable TypeExpr"))); _qoz_bv_148 = qoz_make_TypeExpr_TEUnit(emit_span_of_expr(base));
    }
    _qoz_mv_144 = (_qoz_bv_148);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_144;
}

qoz_Span emit_span_of_expr(qoz_Expr* e) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_Expr* _qoz_ms_150 = e; qoz_Span _qoz_mv_150 = ((qoz_Span){0}); switch (_qoz_ms_150->tag) { case qoz_Expr_ENil: { qoz_Span sp = _qoz_ms_150->payload.ENil.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EInt: { qoz_Span sp = _qoz_ms_150->payload.EInt.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EFloat: { qoz_Span sp = _qoz_ms_150->payload.EFloat.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EString: { qoz_Span sp = _qoz_ms_150->payload.EString.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EChar: { qoz_Span sp = _qoz_ms_150->payload.EChar.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EBool: { qoz_Span sp = _qoz_ms_150->payload.EBool.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EIdent: { qoz_Span sp = _qoz_ms_150->payload.EIdent.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EPath: { qoz_Span sp = _qoz_ms_150->payload.EPath.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EReturn: { qoz_Span sp = _qoz_ms_150->payload.EReturn.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EDefer: { qoz_Span sp = _qoz_ms_150->payload.EDefer.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_ESizeOf: { qoz_Span sp = _qoz_ms_150->payload.ESizeOf.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_150->payload.EArrayLit.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_ETry: { qoz_Span sp = _qoz_ms_150->payload.ETry.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_ETuple: { qoz_Span sp = _qoz_ms_150->payload.ETuple.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_150->payload.EUnary.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_150->payload.EField.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EIndex: { qoz_Span sp = _qoz_ms_150->payload.EIndex.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_ECast: { qoz_Span sp = _qoz_ms_150->payload.ECast.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_ERecord: { qoz_Span sp = _qoz_ms_150->payload.ERecord.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EBlock: { qoz_Span sp = _qoz_ms_150->payload.EBlock.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EWhile: { qoz_Span sp = _qoz_ms_150->payload.EWhile.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_150->payload.EMatch.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EBinary: { qoz_Span sp = _qoz_ms_150->payload.EBinary.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EAssign: { qoz_Span sp = _qoz_ms_150->payload.EAssign.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_150->payload.ECall.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EIf: { qoz_Span sp = _qoz_ms_150->payload.EIf.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EClosure: { qoz_Span sp = _qoz_ms_150->payload.EClosure.f0; _qoz_mv_150 = (sp);  break; } case qoz_Expr_EFor: { qoz_Span sp = _qoz_ms_150->payload.EFor.f0; _qoz_mv_150 = (sp);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_150;
}

qoz_TypeExpr* emit_field_typeexpr_for(qoz_Emitter* e, qoz_TypeExpr* base_te, qoz_string field, qoz_Span sp) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&base_te);
    qoz_TypeExpr* _qoz_ms_151 = base_te; qoz_TypeExpr* _qoz_mv_151 = NULL; switch (_qoz_ms_151->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_151->payload.TEPtr.f1; _qoz_mv_151 = (inner);  break; } default: { _qoz_mv_151 = (base_te);  break; } } qoz_TypeExpr* unwrapped = _qoz_mv_151; qoz_gc_push_root(&unwrapped); qoz_TypeExpr* _qoz_ms_152 = unwrapped; qoz_TypeExpr* _qoz_mv_152 = NULL; switch (_qoz_ms_152->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_152->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_152->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_149;
    {
        if ((path.len) < 1) { qoz_string _qoz_bv_150;
    {
        qoz_Strbuf _qoz_sb_1476_34 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1476_34); strings_sb_append(&_qoz_sb_1476_34, QOZ_STR_LIT("field access `.")); strings_sb_append(&_qoz_sb_1476_34, field); strings_sb_append(&_qoz_sb_1476_34, QOZ_STR_LIT("` on TypeExpr with empty path")); _qoz_bv_150 = strings_sb_finish(&_qoz_sb_1476_34);
    }
    (void)(emit_emit_die(sp, _qoz_bv_150)); return qoz_make_TypeExpr_TEUnit(sp);} qoz_string lookup = emit_type_lookup_key(e, path); qoz_Option__qoz_Decl* _qoz_ms_153 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_decls, lookup); qoz_Option__qoz_Decl* _qoz_mv_153 = NULL; switch (_qoz_ms_153->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_153->payload.Some.f0; _qoz_mv_153 = (qoz_make_Option__qoz_Decl_Some(d));  break; } case qoz_Option__qoz_Decl_None: { _qoz_mv_153 = (qoz_map_get__qoz_string__qoz_Decl(&e->struct_decls, lookup));  break; } } qoz_Option__qoz_Decl* decl_opt = _qoz_mv_153; qoz_gc_push_root(&decl_opt); qoz_Option__qoz_Decl* _qoz_ms_154 = decl_opt; qoz_TypeExpr* _qoz_mv_154 = NULL; switch (_qoz_ms_154->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_154->payload.Some.f0; qoz_Decl* _qoz_ms_155 = decl; qoz_TypeExpr* _qoz_mv_155 = NULL; switch (_qoz_ms_155->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_155->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_155->payload.DStruct.f3; qoz_TypeExpr* _qoz_bv_151;
    {
        { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; if (qoz_string_eq(f.name, field)) { if ((args.len) == (params.len)) { return emit_substitute_type(e, f.ty, params, args);} return f.ty;} } }qoz_string _qoz_bv_152;
    {
        qoz_Strbuf _qoz_sb_1496_38 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1496_38); strings_sb_append(&_qoz_sb_1496_38, QOZ_STR_LIT("struct `")); strings_sb_append(&_qoz_sb_1496_38, lookup); strings_sb_append(&_qoz_sb_1496_38, QOZ_STR_LIT("` has no field `")); strings_sb_append(&_qoz_sb_1496_38, field); strings_sb_append(&_qoz_sb_1496_38, QOZ_STR_LIT("`")); _qoz_bv_152 = strings_sb_finish(&_qoz_sb_1496_38);
    }
    (void)(emit_emit_die(sp, _qoz_bv_152)); _qoz_bv_151 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_155 = (_qoz_bv_151);  break; } default: { qoz_TypeExpr* _qoz_bv_153;
    {
        qoz_string _qoz_bv_154;
    {
        qoz_Strbuf _qoz_sb_1500_38 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1500_38); strings_sb_append(&_qoz_sb_1500_38, QOZ_STR_LIT("decl `")); strings_sb_append(&_qoz_sb_1500_38, lookup); strings_sb_append(&_qoz_sb_1500_38, QOZ_STR_LIT("` is not a struct")); _qoz_bv_154 = strings_sb_finish(&_qoz_sb_1500_38);
    }
    (void)(emit_emit_die(sp, _qoz_bv_154)); _qoz_bv_153 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_155 = (_qoz_bv_153);  break; } } _qoz_mv_154 = (_qoz_mv_155);  break; } case qoz_Option__qoz_Decl_None: { qoz_TypeExpr* _qoz_bv_155;
    {
        qoz_string _qoz_bv_156;
    {
        qoz_Strbuf _qoz_sb_1506_34 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1506_34); strings_sb_append(&_qoz_sb_1506_34, QOZ_STR_LIT("unknown struct `")); strings_sb_append(&_qoz_sb_1506_34, lookup); strings_sb_append(&_qoz_sb_1506_34, QOZ_STR_LIT("` for field `.")); strings_sb_append(&_qoz_sb_1506_34, field); strings_sb_append(&_qoz_sb_1506_34, QOZ_STR_LIT("`")); _qoz_bv_156 = strings_sb_finish(&_qoz_sb_1506_34);
    }
    (void)(emit_emit_die(sp, _qoz_bv_156)); _qoz_bv_155 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_154 = (_qoz_bv_155);  break; } } _qoz_bv_149 = _qoz_mv_154;
    }
    _qoz_mv_152 = (_qoz_bv_149);  break; } case qoz_TypeExpr_TETuple: { qoz_Vec__qoz_TypeExpr elems = _qoz_ms_152->payload.TETuple.f1; qoz_TypeExpr* _qoz_bv_157;
    {
        int64_t idx = emit_tuple_field_index(field); if ((idx >= 0) && (idx < (elems.len))) { return elems.data[idx];} qoz_string _qoz_bv_158;
    {
        qoz_Strbuf _qoz_sb_1514_30 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1514_30); strings_sb_append(&_qoz_sb_1514_30, QOZ_STR_LIT("tuple has no field `.")); strings_sb_append(&_qoz_sb_1514_30, field); strings_sb_append(&_qoz_sb_1514_30, QOZ_STR_LIT("`")); _qoz_bv_158 = strings_sb_finish(&_qoz_sb_1514_30);
    }
    (void)(emit_emit_die(sp, _qoz_bv_158)); _qoz_bv_157 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_152 = (_qoz_bv_157);  break; } default: { qoz_TypeExpr* _qoz_bv_159;
    {
        qoz_string _qoz_bv_160;
    {
        qoz_Strbuf _qoz_sb_1518_30 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1518_30); strings_sb_append(&_qoz_sb_1518_30, QOZ_STR_LIT("field access `.")); strings_sb_append(&_qoz_sb_1518_30, field); strings_sb_append(&_qoz_sb_1518_30, QOZ_STR_LIT("` on a non-named base")); _qoz_bv_160 = strings_sb_finish(&_qoz_sb_1518_30);
    }
    (void)(emit_emit_die(sp, _qoz_bv_160)); _qoz_bv_159 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_152 = (_qoz_bv_159);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_152;
}

int64_t emit_tuple_field_index(qoz_string field) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if ((field).len < 2) { return -1;} if (qoz_string_byte_at(field, 0) != 95) { return -1;} int64_t n = 0; int64_t i = 1; while (i < (field).len) { int64_t b = qoz_string_byte_at(field, i); if ((b < 48) || (b > 57)) { return -1;} n = n * 10 + (b - 48); i = i + 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return n;
}

qoz_Span emit_span_of_te(qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_156 = te; qoz_Span _qoz_mv_156 = ((qoz_Span){0}); switch (_qoz_ms_156->tag) { case qoz_TypeExpr_TEUnit: { qoz_Span sp = _qoz_ms_156->payload.TEUnit.f0; _qoz_mv_156 = (sp);  break; } case qoz_TypeExpr_TEPtr: { qoz_Span sp = _qoz_ms_156->payload.TEPtr.f0; _qoz_mv_156 = (sp);  break; } case qoz_TypeExpr_TETuple: { qoz_Span sp = _qoz_ms_156->payload.TETuple.f0; _qoz_mv_156 = (sp);  break; } case qoz_TypeExpr_TENamed: { qoz_Span sp = _qoz_ms_156->payload.TENamed.f0; _qoz_mv_156 = (sp);  break; } case qoz_TypeExpr_TEFn: { qoz_Span sp = _qoz_ms_156->payload.TEFn.f0; _qoz_mv_156 = (sp);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_156;
}

qoz_TypeExpr* emit_binding_te(qoz_Emitter* e, qoz_TypeExpr* declared, qoz_Expr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&declared);
    qoz_gc_push_root(&value);
    qoz_TypeExpr* _qoz_ms_157 = declared; qoz_TypeExpr* _qoz_mv_157 = NULL; switch (_qoz_ms_157->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_157 = (emit_infer_value_te(e, value));  break; } default: { _qoz_mv_157 = (declared);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_157;
}

int64_t emit_expr_id(qoz_Expr* e) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((int64_t)((void*)e));
}

qoz_TypeExpr* emit_infer_value_te_cached(qoz_Emitter* e, qoz_Expr* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&v);
    qoz_Option__qoz_TypeExpr* _qoz_ms_158 = qoz_map_get__int64_t__qoz_TypeExpr(&e->expr_types, emit_expr_id(v)); qoz_TypeExpr* _qoz_mv_158 = NULL; switch (_qoz_ms_158->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* te = _qoz_ms_158->payload.Some.f0; _qoz_mv_158 = ((((e->current_tparams.len) > 0) ? emit_substitute_type(e, te, e->current_tparams, e->current_targs) : te));  break; } case qoz_Option__qoz_TypeExpr_None: { _qoz_mv_158 = (emit_infer_value_te(e, v));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_158;
}

qoz_TypeExpr* emit_infer_value_te(qoz_Emitter* e, qoz_Expr* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&v);
    qoz_Expr* _qoz_ms_159 = v; qoz_TypeExpr* _qoz_mv_159 = NULL; switch (_qoz_ms_159->tag) { case qoz_Expr_ECast: { qoz_TypeExpr* t = _qoz_ms_159->payload.ECast.f2; _qoz_mv_159 = (t);  break; } case qoz_Expr_ERecord: { qoz_TypeExpr* t = _qoz_ms_159->payload.ERecord.f1; _qoz_mv_159 = (t);  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_159->payload.EField.f0; qoz_Expr* base = _qoz_ms_159->payload.EField.f1; qoz_string name = _qoz_ms_159->payload.EField.f2; _qoz_mv_159 = (emit_field_typeexpr_for(e, emit_infer_base_typeexpr(e, base), name, sp));  break; } case qoz_Expr_EIndex: { qoz_Span sp = _qoz_ms_159->payload.EIndex.f0; qoz_Expr* base = _qoz_ms_159->payload.EIndex.f1; qoz_TypeExpr* _qoz_bv_161;
    {
        qoz_TypeExpr* bte = emit_infer_base_typeexpr(e, base); qoz_gc_push_root(&bte); qoz_TypeExpr* _qoz_ms_160 = bte; qoz_TypeExpr* _qoz_mv_160 = NULL; switch (_qoz_ms_160->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_160->payload.TEPtr.f1; _qoz_mv_160 = (inner);  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_160->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_160->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_162;
    {
        if ((((args.len) >= 1) && ((path.len) >= 1)) && qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Vec"))) { return args.data[0];} (void)(emit_emit_die(sp, QOZ_STR_LIT("let-binding value indexed into non-Vec base; cannot determine binding type"))); _qoz_bv_162 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_160 = (_qoz_bv_162);  break; } default: { qoz_TypeExpr* _qoz_bv_163;
    {
        (void)(emit_emit_die(sp, QOZ_STR_LIT("let-binding value indexed into unrecognised base"))); _qoz_bv_163 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_160 = (_qoz_bv_163);  break; } } _qoz_bv_161 = _qoz_mv_160;
    }
    _qoz_mv_159 = (_qoz_bv_161);  break; } case qoz_Expr_EIdent: { qoz_Span sp = _qoz_ms_159->payload.EIdent.f0; qoz_string name = _qoz_ms_159->payload.EIdent.f1; qoz_Option__qoz_TypeExpr* _qoz_ms_161 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->locals, name); qoz_TypeExpr* _qoz_mv_161 = NULL; switch (_qoz_ms_161->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* t = _qoz_ms_161->payload.Some.f0; _qoz_mv_161 = (t);  break; } case qoz_Option__qoz_TypeExpr_None: { qoz_TypeExpr* _qoz_bv_164;
    {
        qoz_string _qoz_bv_165;
    {
        qoz_Strbuf _qoz_sb_1602_34 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1602_34); strings_sb_append(&_qoz_sb_1602_34, QOZ_STR_LIT("let-binding value references unknown name `")); strings_sb_append(&_qoz_sb_1602_34, name); strings_sb_append(&_qoz_sb_1602_34, QOZ_STR_LIT("`")); _qoz_bv_165 = strings_sb_finish(&_qoz_sb_1602_34);
    }
    (void)(emit_emit_die(sp, _qoz_bv_165)); _qoz_bv_164 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_161 = (_qoz_bv_164);  break; } } _qoz_mv_159 = (_qoz_mv_161);  break; } case qoz_Expr_ECall: { qoz_Span sp = _qoz_ms_159->payload.ECall.f0; qoz_Expr* callee = _qoz_ms_159->payload.ECall.f1; qoz_Vec__qoz_TypeExpr type_args = _qoz_ms_159->payload.ECall.f2; qoz_Vec__qoz_Expr vargs = _qoz_ms_159->payload.ECall.f3; _qoz_mv_159 = (emit_call_return_te(e, callee, type_args, vargs, sp));  break; } case qoz_Expr_EInt: { qoz_Span sp = _qoz_ms_159->payload.EInt.f0; _qoz_mv_159 = (emit_single_named_te(sp, QOZ_STR_LIT("i64")));  break; } case qoz_Expr_EFloat: { qoz_Span sp = _qoz_ms_159->payload.EFloat.f0; _qoz_mv_159 = (emit_single_named_te(sp, QOZ_STR_LIT("f64")));  break; } case qoz_Expr_EString: { qoz_Span sp = _qoz_ms_159->payload.EString.f0; _qoz_mv_159 = (emit_single_named_te(sp, QOZ_STR_LIT("string")));  break; } case qoz_Expr_EBool: { qoz_Span sp = _qoz_ms_159->payload.EBool.f0; _qoz_mv_159 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_Expr_EChar: { qoz_Span sp = _qoz_ms_159->payload.EChar.f0; _qoz_mv_159 = (emit_single_named_te(sp, QOZ_STR_LIT("char")));  break; } case qoz_Expr_ENil: { qoz_Span sp = _qoz_ms_159->payload.ENil.f0; _qoz_mv_159 = (qoz_make_TypeExpr_TEPtr(sp, emit_single_named_te(sp, QOZ_STR_LIT("void"))));  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_159->payload.EUnary.f0; qoz_UnaryOp* op = _qoz_ms_159->payload.EUnary.f1; qoz_Expr* rhs = _qoz_ms_159->payload.EUnary.f2; qoz_UnaryOp* _qoz_ms_162 = op; qoz_TypeExpr* _qoz_mv_162 = NULL; switch (_qoz_ms_162->tag) { case qoz_UnaryOp_UOpAddr: { _qoz_mv_162 = (qoz_make_TypeExpr_TEPtr(sp, emit_infer_value_te(e, rhs)));  break; } case qoz_UnaryOp_UOpDeref: { qoz_TypeExpr* _qoz_bv_166;
    {
        qoz_TypeExpr* inner = emit_infer_value_te(e, rhs); qoz_gc_push_root(&inner); qoz_TypeExpr* _qoz_ms_163 = inner; qoz_TypeExpr* _qoz_mv_163 = NULL; switch (_qoz_ms_163->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* t = _qoz_ms_163->payload.TEPtr.f1; _qoz_mv_163 = (t);  break; } default: { qoz_TypeExpr* _qoz_bv_167;
    {
        (void)(emit_emit_die(sp, QOZ_STR_LIT("let-binding dereferences a non-pointer"))); _qoz_bv_167 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_163 = (_qoz_bv_167);  break; } } _qoz_bv_166 = _qoz_mv_163;
    }
    _qoz_mv_162 = (_qoz_bv_166);  break; } case qoz_UnaryOp_UOpNot: { _qoz_mv_162 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_UnaryOp_UOpNeg: { _qoz_mv_162 = (emit_infer_value_te(e, rhs));  break; } } _qoz_mv_159 = (_qoz_mv_162);  break; } case qoz_Expr_EBinary: { qoz_Span sp = _qoz_ms_159->payload.EBinary.f0; qoz_BinaryOp* op = _qoz_ms_159->payload.EBinary.f1; qoz_Expr* l = _qoz_ms_159->payload.EBinary.f2; qoz_BinaryOp* _qoz_ms_164 = op; qoz_TypeExpr* _qoz_mv_164 = NULL; switch (_qoz_ms_164->tag) { case qoz_BinaryOp_BOpEq: { _qoz_mv_164 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_BinaryOp_BOpNe: { _qoz_mv_164 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_BinaryOp_BOpLt: { _qoz_mv_164 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_BinaryOp_BOpGt: { _qoz_mv_164 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_BinaryOp_BOpLe: { _qoz_mv_164 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_BinaryOp_BOpGe: { _qoz_mv_164 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_BinaryOp_BOpAnd: { _qoz_mv_164 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_BinaryOp_BOpOr: { _qoz_mv_164 = (emit_single_named_te(sp, QOZ_STR_LIT("bool")));  break; } default: { _qoz_mv_164 = (emit_infer_value_te(e, l));  break; } } _qoz_mv_159 = (_qoz_mv_164);  break; } case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_159->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_159->payload.EBlock.f2; qoz_TypeExpr* _qoz_bv_168;
    {
        { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_ingest_stmt_locals(e, s); } }_qoz_bv_168 = emit_infer_value_te(e, tail);
    }
    _qoz_mv_159 = (_qoz_bv_168);  break; } case qoz_Expr_EIf: { qoz_Expr* t = _qoz_ms_159->payload.EIf.f2; _qoz_mv_159 = (emit_infer_value_te(e, t));  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_159->payload.EMatch.f0; qoz_Expr* scrut = _qoz_ms_159->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_159->payload.EMatch.f2; qoz_TypeExpr* _qoz_bv_169;
    {
        if ((arms.len) == 0) { (void)(emit_emit_die(sp, QOZ_STR_LIT("match expression with no arms"))); return qoz_make_TypeExpr_TEUnit(sp);} qoz_string bare_enum = emit_find_enum_from_arms(e, arms); qoz_string enum_name = ((qoz_string_eq(bare_enum, QOZ_STR_LIT(""))) ? bare_enum : emit_enum_lookup_name(e, scrut, bare_enum)); emit_bind_arm_locals(e, enum_name, arms.data[0].pat); _qoz_bv_169 = emit_infer_value_te(e, arms.data[0].body);
    }
    _qoz_mv_159 = (_qoz_bv_169);  break; } case qoz_Expr_ESizeOf: { qoz_Span sp = _qoz_ms_159->payload.ESizeOf.f0; _qoz_mv_159 = (emit_single_named_te(sp, QOZ_STR_LIT("i64")));  break; } case qoz_Expr_ETuple: { qoz_Span sp = _qoz_ms_159->payload.ETuple.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_159->payload.ETuple.f1; qoz_TypeExpr* _qoz_bv_170;
    {
        qoz_Vec__qoz_TypeExpr ets = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_Expr __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_TypeExpr(&ets, emit_infer_value_te(e, el)); } }_qoz_bv_170 = qoz_make_TypeExpr_TETuple(sp, ets);
    }
    _qoz_mv_159 = (_qoz_bv_170);  break; } case qoz_Expr_EClosure: { qoz_Span sp = _qoz_ms_159->payload.EClosure.f0; qoz_Vec__qoz_ClosureParam cps = _qoz_ms_159->payload.EClosure.f1; qoz_TypeExpr* ret = _qoz_ms_159->payload.EClosure.f2; qoz_TypeExpr* _qoz_bv_171;
    {
        qoz_Vec__qoz_TypeExpr pte = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_ClosureParam __col = cps; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_ClosureParam cp = __col.data[__i]; (void)cp; qoz_vec_push__qoz_TypeExpr(&pte, cp.ty); } }_qoz_bv_171 = qoz_make_TypeExpr_TEFn(sp, pte, ret);
    }
    _qoz_mv_159 = (_qoz_bv_171);  break; } case qoz_Expr_ETry: { qoz_Span sp = _qoz_ms_159->payload.ETry.f0; qoz_Expr* value = _qoz_ms_159->payload.ETry.f1; qoz_TypeExpr* _qoz_bv_172;
    {
        qoz_TypeExpr* inner = emit_infer_value_te(e, value); qoz_gc_push_root(&inner); qoz_TypeExpr* _qoz_ms_165 = inner; qoz_TypeExpr* _qoz_mv_165 = NULL; switch (_qoz_ms_165->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_165->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_165->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_173;
    {
        if ((args.len) == 2) { if ((path.len) > 0) { qoz_string last = path.data[(path.len) - 1]; if (qoz_string_eq(last, QOZ_STR_LIT("Result"))) { return args.data[0];} } } _qoz_bv_173 = inner;
    }
    _qoz_mv_165 = (_qoz_bv_173);  break; } default: { _qoz_mv_165 = (inner);  break; } } _qoz_bv_172 = _qoz_mv_165;
    }
    _qoz_mv_159 = (_qoz_bv_172);  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_159->payload.EArrayLit.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_159->payload.EArrayLit.f1; qoz_TypeExpr* _qoz_bv_174;
    {
        if ((elems.len) == 0) { (void)(emit_emit_die(sp, QOZ_STR_LIT("empty array literal: element type cannot be inferred"))); return qoz_make_TypeExpr_TEUnit(sp);} qoz_TypeExpr* elem_te = emit_infer_value_te(e, elems.data[0]); qoz_gc_push_root(&elem_te); qoz_Vec__qoz_TypeExpr args = qoz_vec_make__qoz_TypeExpr(); qoz_vec_push__qoz_TypeExpr(&args, elem_te); qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("Vec")); _qoz_bv_174 = qoz_make_TypeExpr_TENamed(sp, path, args);
    }
    _qoz_mv_159 = (_qoz_bv_174);  break; } case qoz_Expr_EAssign: { qoz_Span sp = _qoz_ms_159->payload.EAssign.f0; _qoz_mv_159 = (qoz_make_TypeExpr_TEUnit(sp));  break; } case qoz_Expr_EWhile: { qoz_Span sp = _qoz_ms_159->payload.EWhile.f0; _qoz_mv_159 = (qoz_make_TypeExpr_TEUnit(sp));  break; } case qoz_Expr_EFor: { qoz_Span sp = _qoz_ms_159->payload.EFor.f0; _qoz_mv_159 = (qoz_make_TypeExpr_TEUnit(sp));  break; } case qoz_Expr_EReturn: { qoz_Span sp = _qoz_ms_159->payload.EReturn.f0; _qoz_mv_159 = (qoz_make_TypeExpr_TEUnit(sp));  break; } case qoz_Expr_EDefer: { qoz_Span sp = _qoz_ms_159->payload.EDefer.f0; _qoz_mv_159 = (qoz_make_TypeExpr_TEUnit(sp));  break; } case qoz_Expr_EPath: { qoz_Span sp = _qoz_ms_159->payload.EPath.f0; qoz_TypeExpr* _qoz_bv_175;
    {
        (void)(emit_emit_die(sp, QOZ_STR_LIT("EPath in value position is not supported (expected pkg.fn or a record name)"))); _qoz_bv_175 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_159 = (_qoz_bv_175);  break; } default: { qoz_TypeExpr* _qoz_bv_176;
    {
        (void)(emit_emit_die(emit_span_of_expr(v), QOZ_STR_LIT("let-binding value shape not handled by inference"))); _qoz_bv_176 = qoz_make_TypeExpr_TEUnit(emit_span_of_expr(v));
    }
    _qoz_mv_159 = (_qoz_bv_176);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_159;
}

qoz_TypeExpr* emit_single_named_te(qoz_Span sp, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, name); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_TypeExpr_TENamed(sp, path, qoz_vec_make__qoz_TypeExpr());
}

qoz_Vec__qoz_TypeExpr emit_infer_variant_type_args(qoz_Emitter* e, qoz_string enum_name, qoz_string variant, qoz_Vec__qoz_Expr args, qoz_Span sp) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_Vec__qoz_TypeExpr out = qoz_vec_make__qoz_TypeExpr(); qoz_Option__qoz_Decl* _qoz_ms_166 = qoz_map_get__qoz_string__qoz_Decl(&e->enum_decls, enum_name); qoz_Vec__qoz_TypeExpr _qoz_mv_166 = ((qoz_Vec__qoz_TypeExpr){0}); switch (_qoz_ms_166->tag) { case qoz_Option__qoz_Decl_None: { _qoz_mv_166 = (out);  break; } case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_166->payload.Some.f0; qoz_Decl* _qoz_ms_167 = d; qoz_Vec__qoz_TypeExpr _qoz_mv_167 = ((qoz_Vec__qoz_TypeExpr){0}); switch (_qoz_ms_167->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string tparams = _qoz_ms_167->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_167->payload.DEnum.f3; qoz_Vec__qoz_TypeExpr _qoz_bv_177;
    {
        if ((tparams.len) == 0) { return out;} qoz_Map__qoz_string__qoz_TypeExpr bindings = qoz_map_make__qoz_string__qoz_TypeExpr(); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (qoz_string_eq(v.name, variant)) { int64_t i = 0; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pos_te = __col.data[__i]; (void)pos_te; if (i < (args.len)) { qoz_TypeExpr* arg_te = emit_infer_value_te(e, args.data[i]); qoz_gc_push_root(&arg_te); emit_unify_te(pos_te, arg_te, &bindings); } i = i + 1; } }} } }{ qoz_Vec__qoz_string __col = tparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string tp = __col.data[__i]; (void)tp; qoz_Option__qoz_TypeExpr* _qoz_ms_168 = qoz_map_get__qoz_string__qoz_TypeExpr(&bindings, tp); switch (_qoz_ms_168->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* t = _qoz_ms_168->payload.Some.f0; qoz_vec_push__qoz_TypeExpr(&out, t);  break; } case qoz_Option__qoz_TypeExpr_None: { qoz_vec_push__qoz_TypeExpr(&out, emit_single_named_te(sp, QOZ_STR_LIT("i64")));  break; } } 0; } }_qoz_bv_177 = out;
    }
    _qoz_mv_167 = (_qoz_bv_177);  break; } default: { _qoz_mv_167 = (out);  break; } } _qoz_mv_166 = (_qoz_mv_167);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_166;
}

void emit_unify_te(qoz_TypeExpr* pattern, qoz_TypeExpr* concrete, qoz_Map__qoz_string__qoz_TypeExpr* env) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&pattern);
    qoz_gc_push_root(&concrete);
    qoz_gc_push_root(&env);
    qoz_TypeExpr* _qoz_ms_169 = pattern; switch (_qoz_ms_169->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_169->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr p_args = _qoz_ms_169->payload.TENamed.f2; {
        if (((path.len) == 1) && ((p_args.len) == 0)) { qoz_string n = path.data[0]; if (emit_is_type_var_name(n)) { if (!qoz_map_contains__qoz_string__qoz_TypeExpr(env, n)) { qoz_map_set__qoz_string__qoz_TypeExpr(env, n, concrete); } return;} } qoz_TypeExpr* _qoz_ms_170 = concrete; switch (_qoz_ms_170->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string c_path = _qoz_ms_170->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr c_args = _qoz_ms_170->payload.TENamed.f2; if ((p_args.len) == (c_args.len)) { int64_t i = 0; while (i < (p_args.len)) { emit_unify_te(p_args.data[i], c_args.data[i], env); i = i + 1; } } 0;  break; } default: { NULL;  break; } } 0; 
    }
    0;  break; } case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* p_inner = _qoz_ms_169->payload.TEPtr.f1; qoz_TypeExpr* _qoz_ms_171 = concrete; switch (_qoz_ms_171->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* c_inner = _qoz_ms_171->payload.TEPtr.f1; emit_unify_te(p_inner, c_inner, env);  break; } default: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0; 
    return;
}

bool emit_is_type_var_name(qoz_string n) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if ((n).len == 0) { return false;} if ((n).len > 2) { return false;} int64_t c = qoz_string_byte_at(n, 0); qoz_gc_shadow_set_top(_qoz_shadow_guard); return (c >= 65) && (c <= 90);
}

qoz_TypeExpr* emit_call_return_te(qoz_Emitter* e, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args, qoz_Span sp) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&callee);
    qoz_string name = emit_generic_callee_name(e, callee); if (qoz_string_eq(name, QOZ_STR_LIT(""))) { (void)(emit_emit_die(sp, QOZ_STR_LIT("could not derive return type: callee has no recognisable name"))); return qoz_make_TypeExpr_TEUnit(sp);} if (qoz_string_eq(name, QOZ_STR_LIT("len"))) { return emit_single_named_te(sp, QOZ_STR_LIT("i64"));} if (qoz_string_eq(name, QOZ_STR_LIT("size_of"))) { return emit_single_named_te(sp, QOZ_STR_LIT("i64"));} if (qoz_string_eq(name, QOZ_STR_LIT("hash"))) { return emit_single_named_te(sp, QOZ_STR_LIT("u64"));} qoz_Option__qoz_string* _qoz_ms_172 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); switch (_qoz_ms_172->tag) { case qoz_Option__qoz_string_Some: { qoz_string en = _qoz_ms_172->payload.Some.f0; {
        qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, en); qoz_Vec__qoz_TypeExpr final_args = (((type_args.len) > 0) ? type_args : emit_infer_variant_type_args(e, en, name, args, sp)); return qoz_make_TypeExpr_TENamed(sp, path, final_args);
    }
    0;  break; } case qoz_Option__qoz_string_None: { NULL;  break; } } 0; qoz_Option__qoz_Decl* _qoz_ms_173 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_fn_decls, name); qoz_TypeExpr* _qoz_mv_173 = NULL; switch (_qoz_ms_173->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_173->payload.Some.f0; qoz_Decl* _qoz_ms_174 = d; qoz_TypeExpr* _qoz_mv_174 = NULL; switch (_qoz_ms_174->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string tparams = _qoz_ms_174->payload.DFn.f2; qoz_TypeExpr* ret = _qoz_ms_174->payload.DFn.f4; qoz_TypeExpr* _qoz_bv_178;
    {
        if (((type_args.len) == (tparams.len)) && ((tparams.len) > 0)) { return emit_substitute_type(e, ret, tparams, type_args);} _qoz_bv_178 = ret;
    }
    _qoz_mv_174 = (_qoz_bv_178);  break; } default: { qoz_TypeExpr* _qoz_bv_179;
    {
        qoz_string _qoz_bv_180;
    {
        qoz_Strbuf _qoz_sb_1837_34 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1837_34); strings_sb_append(&_qoz_sb_1837_34, QOZ_STR_LIT("`")); strings_sb_append(&_qoz_sb_1837_34, name); strings_sb_append(&_qoz_sb_1837_34, QOZ_STR_LIT("` is in generic_fn_decls but not a DFn")); _qoz_bv_180 = strings_sb_finish(&_qoz_sb_1837_34);
    }
    (void)(emit_emit_die(sp, _qoz_bv_180)); _qoz_bv_179 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_174 = (_qoz_bv_179);  break; } } _qoz_mv_173 = (_qoz_mv_174);  break; } case qoz_Option__qoz_Decl_None: { qoz_Option__qoz_TypeExpr* _qoz_ms_175 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->fn_returns, name); qoz_TypeExpr* _qoz_mv_175 = NULL; switch (_qoz_ms_175->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* t = _qoz_ms_175->payload.Some.f0; _qoz_mv_175 = (t);  break; } case qoz_Option__qoz_TypeExpr_None: { qoz_TypeExpr* _qoz_bv_181;
    {
        qoz_string _qoz_bv_182;
    {
        qoz_Strbuf _qoz_sb_1846_34 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1846_34); strings_sb_append(&_qoz_sb_1846_34, QOZ_STR_LIT("call to unknown function `")); strings_sb_append(&_qoz_sb_1846_34, name); strings_sb_append(&_qoz_sb_1846_34, QOZ_STR_LIT("` (no DFn, DExternal, or builtin matched)")); _qoz_bv_182 = strings_sb_finish(&_qoz_sb_1846_34);
    }
    (void)(emit_emit_die(sp, _qoz_bv_182)); _qoz_bv_181 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_175 = (_qoz_bv_181);  break; } } _qoz_mv_173 = (_qoz_mv_175);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_173;
}

qoz_StmtScope emit_open_statement_scope(qoz_Emitter* e) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_StmtScope saved = ((qoz_StmtScope){ .start = strings_sb_len(&e->out), .prologue = e->pending_prologue }); e->pending_prologue = qoz_vec_make__qoz_string(); qoz_gc_shadow_set_top(_qoz_shadow_guard); return saved;
}

void emit_close_statement_scope(qoz_Emitter* e, qoz_StmtScope saved) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    int64_t cur = strings_sb_len(&e->out); qoz_string captured = strings_sb_slice_copy(&e->out, saved.start, cur); strings_sb_truncate(&e->out, saved.start); qoz_Vec__qoz_string prologues = e->pending_prologue; e->pending_prologue = saved.prologue; { qoz_Vec__qoz_string __col = prologues; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string p = __col.data[__i]; (void)p; emit_push(e, p); } }emit_push(e, captured); 
    return;
}

void emit_hoist_to_prologue(qoz_Emitter* e, int64_t start) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string captured = strings_sb_slice_copy(&e->out, start, strings_sb_len(&e->out)); strings_sb_truncate(&e->out, start); qoz_vec_push__qoz_string(&e->pending_prologue, captured); 
    return;
}

void emit_emit_stmt(qoz_Emitter* e, qoz_Stmt* s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&s);
    qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_inner(e, s); emit_close_statement_scope(e, saved); 
    return;
}

void emit_emit_stmt_inner(qoz_Emitter* e, qoz_Stmt* s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&s);
    qoz_Stmt* _qoz_ms_176 = s; switch (_qoz_ms_176->tag) { case qoz_Stmt_SLet: { qoz_string name = _qoz_ms_176->payload.SLet.f1; qoz_TypeExpr* ty = _qoz_ms_176->payload.SLet.f2; qoz_Expr* value = _qoz_ms_176->payload.SLet.f3; {
        qoz_TypeExpr* bind_te = emit_binding_te(e, ty, value); qoz_gc_push_root(&bind_te); if (qoz_string_eq(name, QOZ_STR_LIT("_"))) { emit_push(e, QOZ_STR_LIT("(void)(")); emit_emit_value_with_hint(e, value, bind_te); emit_push(e, QOZ_STR_LIT("); ")); return;} qoz_string ct = emit_infer_value_ctype(e, ty, value); qoz_string _qoz_bv_183;
    {
        qoz_Strbuf _qoz_sb_1915_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1915_17); strings_sb_append(&_qoz_sb_1915_17, ct); strings_sb_append(&_qoz_sb_1915_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_1915_17, name); strings_sb_append(&_qoz_sb_1915_17, QOZ_STR_LIT(" = ")); _qoz_bv_183 = strings_sb_finish(&_qoz_sb_1915_17);
    }
    emit_push(e, _qoz_bv_183); emit_emit_value_with_hint(e, value, bind_te); emit_push(e, QOZ_STR_LIT("; ")); if (emit_c_type_is_pointer(ct)) { qoz_string _qoz_bv_184;
    {
        qoz_Strbuf _qoz_sb_1919_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1919_21); strings_sb_append(&_qoz_sb_1919_21, QOZ_STR_LIT("qoz_gc_push_root(&")); strings_sb_append(&_qoz_sb_1919_21, name); strings_sb_append(&_qoz_sb_1919_21, QOZ_STR_LIT("); ")); _qoz_bv_184 = strings_sb_finish(&_qoz_sb_1919_21);
    }
    emit_push(e, _qoz_bv_184); } qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, name, bind_te); 
    }
    0;  break; } case qoz_Stmt_SVar: { qoz_string name = _qoz_ms_176->payload.SVar.f1; qoz_TypeExpr* ty = _qoz_ms_176->payload.SVar.f2; qoz_Expr* value = _qoz_ms_176->payload.SVar.f3; {
        qoz_TypeExpr* bind_te = emit_binding_te(e, ty, value); qoz_gc_push_root(&bind_te); if (qoz_string_eq(name, QOZ_STR_LIT("_"))) { emit_push(e, QOZ_STR_LIT("(void)(")); emit_emit_value_with_hint(e, value, bind_te); emit_push(e, QOZ_STR_LIT("); ")); return;} qoz_string ct = emit_infer_value_ctype(e, ty, value); qoz_string _qoz_bv_185;
    {
        qoz_Strbuf _qoz_sb_1932_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1932_17); strings_sb_append(&_qoz_sb_1932_17, ct); strings_sb_append(&_qoz_sb_1932_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_1932_17, name); strings_sb_append(&_qoz_sb_1932_17, QOZ_STR_LIT(" = ")); _qoz_bv_185 = strings_sb_finish(&_qoz_sb_1932_17);
    }
    emit_push(e, _qoz_bv_185); emit_emit_value_with_hint(e, value, bind_te); emit_push(e, QOZ_STR_LIT("; ")); if (emit_c_type_is_pointer(ct)) { qoz_string _qoz_bv_186;
    {
        qoz_Strbuf _qoz_sb_1936_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_1936_21); strings_sb_append(&_qoz_sb_1936_21, QOZ_STR_LIT("qoz_gc_push_root(&")); strings_sb_append(&_qoz_sb_1936_21, name); strings_sb_append(&_qoz_sb_1936_21, QOZ_STR_LIT("); ")); _qoz_bv_186 = strings_sb_finish(&_qoz_sb_1936_21);
    }
    emit_push(e, _qoz_bv_186); } qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, name, bind_te); 
    }
    0;  break; } case qoz_Stmt_SExpr: { qoz_Expr* expr = _qoz_ms_176->payload.SExpr.f1; qoz_Expr* _qoz_ms_177 = expr; switch (_qoz_ms_177->tag) { case qoz_Expr_EDefer: { NULL;  break; } default: { emit_emit_stmt_expr(e, expr);  break; } } 0;  break; } } 0; 
    return;
}

qoz_Vec__qoz_Expr emit_collect_defers_in_stmts(qoz_Vec__qoz_Stmt stmts) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_Expr defers = qoz_vec_make__qoz_Expr(); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; qoz_Stmt* _qoz_ms_178 = s; switch (_qoz_ms_178->tag) { case qoz_Stmt_SExpr: { qoz_Expr* expr = _qoz_ms_178->payload.SExpr.f1; qoz_Expr* _qoz_ms_179 = expr; switch (_qoz_ms_179->tag) { case qoz_Expr_EDefer: { qoz_Expr* body = _qoz_ms_179->payload.EDefer.f1; qoz_vec_push__qoz_Expr(&defers, body);  break; } default: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0; } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return defers;
}

void emit_emit_defers_reverse(qoz_Emitter* e, qoz_Vec__qoz_Expr defers) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    int64_t i = (defers.len) - 1; while (i >= 0) { emit_emit_stmt_expr(e, defers.data[i]); i = i - 1; } 
    return;
}

void emit_emit_cond(qoz_Emitter* e, qoz_Expr* cond) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&cond);
    emit_push(e, QOZ_STR_LIT("(")); qoz_Expr* _qoz_ms_180 = cond; switch (_qoz_ms_180->tag) { case qoz_Expr_EBinary: { qoz_BinaryOp* op = _qoz_ms_180->payload.EBinary.f1; qoz_Expr* lhs = _qoz_ms_180->payload.EBinary.f2; qoz_Expr* rhs = _qoz_ms_180->payload.EBinary.f3; emit_emit_binary(e, op, lhs, rhs);  break; } default: { emit_emit_expr(e, cond);  break; } } 0; emit_push(e, QOZ_STR_LIT(")")); 
    return;
}

void emit_emit_stmt_expr(qoz_Emitter* e, qoz_Expr* expr) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&expr);
    qoz_Expr* _qoz_ms_181 = expr; switch (_qoz_ms_181->tag) { case qoz_Expr_EIf: { qoz_Expr* cond = _qoz_ms_181->payload.EIf.f1; qoz_Expr* then_b = _qoz_ms_181->payload.EIf.f2; qoz_Expr* else_b = _qoz_ms_181->payload.EIf.f3; {
        emit_push(e, QOZ_STR_LIT("if ")); emit_emit_cond(e, cond); emit_push(e, QOZ_STR_LIT(" ")); emit_emit_branch_as_statement(e, then_b); if (!emit_is_nil_expr(else_b)) { emit_push(e, QOZ_STR_LIT(" else ")); emit_emit_branch_as_statement(e, else_b); } 
    }
    0;  break; } case qoz_Expr_EWhile: { qoz_Expr* cond = _qoz_ms_181->payload.EWhile.f1; qoz_Expr* body = _qoz_ms_181->payload.EWhile.f2; {
        emit_push(e, QOZ_STR_LIT("while (")); emit_emit_expr(e, cond); emit_push(e, QOZ_STR_LIT(") ")); emit_emit_branch_as_statement(e, body); 
    }
    0;  break; } case qoz_Expr_EFor: { qoz_string binding = _qoz_ms_181->payload.EFor.f1; qoz_string binding2 = _qoz_ms_181->payload.EFor.f2; qoz_Expr* iter = _qoz_ms_181->payload.EFor.f3; qoz_Expr* body = _qoz_ms_181->payload.EFor.f4; emit_emit_for_loop_one(e, binding, binding2, iter, body);  break; } case qoz_Expr_EReturn: { qoz_Expr* value = _qoz_ms_181->payload.EReturn.f1; if (emit_is_nil_expr(value)) { emit_push(e, QOZ_STR_LIT("return;")); }  else { emit_push(e, QOZ_STR_LIT("return ")); emit_emit_value_with_hint(e, value, e->current_ret_te); emit_push(e, QOZ_STR_LIT(";")); } 0;  break; } default: { {
        emit_emit_expr(e, expr); emit_push(e, QOZ_STR_LIT("; ")); 
    }
    0;  break; } } 0; 
    return;
}

void emit_emit_branch_as_statement(qoz_Emitter* e, qoz_Expr* br) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&br);
    qoz_Expr* _qoz_ms_182 = br; switch (_qoz_ms_182->tag) { case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_182->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_182->payload.EBlock.f2; {
        emit_push(e, QOZ_STR_LIT("{ ")); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }if (!emit_is_nil_expr(tail)) { qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); } emit_push(e, QOZ_STR_LIT("} ")); 
    }
    0;  break; } default: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, br); emit_close_statement_scope(e, saved); 
    }
    0;  break; } } 0; 
    return;
}

bool emit_path_is(qoz_Expr* callee, qoz_string a, qoz_string b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&callee);
    qoz_Expr* _qoz_ms_183 = callee; bool _qoz_mv_183 = false; switch (_qoz_ms_183->tag) { case qoz_Expr_EPath: { qoz_Vec__qoz_string segs = _qoz_ms_183->payload.EPath.f1; _qoz_mv_183 = ((((segs.len) == 2) && qoz_string_eq(segs.data[0], a)) && qoz_string_eq(segs.data[1], b));  break; } case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_183->payload.EField.f1; qoz_string name = _qoz_ms_183->payload.EField.f2; _qoz_mv_183 = (qoz_string_eq(name, b) && emit_ident_is(base, a));  break; } default: { _qoz_mv_183 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_183;
}

bool emit_ident_is(qoz_Expr* ex, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_184 = ex; bool _qoz_mv_184 = false; switch (_qoz_ms_184->tag) { case qoz_Expr_EIdent: { qoz_string n = _qoz_ms_184->payload.EIdent.f1; _qoz_mv_184 = (qoz_string_eq(n, name));  break; } default: { _qoz_mv_184 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_184;
}

qoz_string emit_qualified_call_name(qoz_Emitter* e, qoz_Expr* callee) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&callee);
    qoz_Expr* _qoz_ms_185 = callee; qoz_string _qoz_mv_185 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_185->tag) { case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_185->payload.EField.f1; qoz_string method = _qoz_ms_185->payload.EField.f2; qoz_Expr* _qoz_ms_186 = base; qoz_string _qoz_mv_186 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_186->tag) { case qoz_Expr_EIdent: { qoz_string pkg = _qoz_ms_186->payload.EIdent.f1; qoz_string _qoz_bv_187;
    {
        if (qoz_map_contains__qoz_string__bool(&e->packages, pkg)) { return qoz_string_cat(qoz_string_cat(pkg, QOZ_STR_LIT("_")), method);} _qoz_bv_187 = QOZ_STR_LIT("");
    }
    _qoz_mv_186 = (_qoz_bv_187);  break; } default: { _qoz_mv_186 = (QOZ_STR_LIT(""));  break; } } _qoz_mv_185 = (_qoz_mv_186);  break; } default: { _qoz_mv_185 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_185;
}

qoz_string emit_match_result_ctype_with_hint(qoz_Emitter* e, qoz_string enum_name, qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if (!emit_is_unit_typeexpr(e->match_hint)) { return emit_c_type_for(e, e->match_hint);} qoz_gc_shadow_set_top(_qoz_shadow_guard); return emit_match_result_ctype(e, enum_name, arms);
}

qoz_string emit_match_result_ctype(qoz_Emitter* e, qoz_string enum_name, qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    bool all_void = true; { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; emit_bind_arm_locals(e, enum_name, arm.pat); qoz_string ct = emit_arm_body_ctype(e, enum_name, arm.body); if (!qoz_string_eq(ct, QOZ_STR_LIT("void"))) { all_void = false; } if (!qoz_string_eq(ct, QOZ_STR_LIT("int64_t")) && !qoz_string_eq(ct, QOZ_STR_LIT("void"))) { return ct;} } }if (all_void) { return QOZ_STR_LIT("void");} qoz_gc_shadow_set_top(_qoz_shadow_guard); return QOZ_STR_LIT("int64_t");
}

qoz_string emit_arm_body_ctype(qoz_Emitter* e, qoz_string enum_name, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&body);
    qoz_Expr* _qoz_ms_187 = body; qoz_string _qoz_mv_187 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_187->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_187->payload.EIdent.f1; qoz_Option__qoz_string* _qoz_ms_188 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); qoz_string _qoz_mv_188 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_188->tag) { case qoz_Option__qoz_string_Some: { qoz_string en = _qoz_ms_188->payload.Some.f0; qoz_string _qoz_bv_188;
    {
        if (emit_same_enum_base(e, en, enum_name)) { return qoz_string_cat(qoz_string_cat(QOZ_STR_LIT("qoz_"), enum_name), QOZ_STR_LIT("*"));} _qoz_bv_188 = qoz_string_cat(qoz_string_cat(QOZ_STR_LIT("qoz_"), en), QOZ_STR_LIT("*"));
    }
    _qoz_mv_188 = (_qoz_bv_188);  break; } case qoz_Option__qoz_string_None: { _qoz_mv_188 = (emit_infer_expr_ctype(e, body));  break; } } _qoz_mv_187 = (_qoz_mv_188);  break; } case qoz_Expr_ECall: { qoz_Expr* callee = _qoz_ms_187->payload.ECall.f1; qoz_Expr* _qoz_ms_189 = callee; qoz_string _qoz_mv_189 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_189->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_189->payload.EIdent.f1; qoz_Option__qoz_string* _qoz_ms_190 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); qoz_string _qoz_mv_190 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_190->tag) { case qoz_Option__qoz_string_Some: { qoz_string en = _qoz_ms_190->payload.Some.f0; qoz_string _qoz_bv_189;
    {
        if (emit_same_enum_base(e, en, enum_name)) { return qoz_string_cat(qoz_string_cat(QOZ_STR_LIT("qoz_"), enum_name), QOZ_STR_LIT("*"));} _qoz_bv_189 = qoz_string_cat(qoz_string_cat(QOZ_STR_LIT("qoz_"), en), QOZ_STR_LIT("*"));
    }
    _qoz_mv_190 = (_qoz_bv_189);  break; } case qoz_Option__qoz_string_None: { _qoz_mv_190 = (emit_infer_expr_ctype(e, body));  break; } } _qoz_mv_189 = (_qoz_mv_190);  break; } default: { _qoz_mv_189 = (emit_infer_expr_ctype(e, body));  break; } } _qoz_mv_187 = (_qoz_mv_189);  break; } default: { _qoz_mv_187 = (emit_infer_expr_ctype(e, body));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_187;
}

bool emit_same_enum_base(qoz_Emitter* e, qoz_string bare, qoz_string maybe_mangled) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if (qoz_string_eq(bare, maybe_mangled)) { return true;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_eq(emit_strip_mangled(maybe_mangled), bare);
}

void emit_bind_arm_locals(qoz_Emitter* e, qoz_string enum_name, qoz_Pattern* pat) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&pat);
    qoz_Pattern* _qoz_ms_191 = pat; switch (_qoz_ms_191->tag) { case qoz_Pattern_PatVariant: { qoz_Vec__qoz_string path = _qoz_ms_191->payload.PatVariant.f1; qoz_Vec__qoz_Pattern sub_pats = _qoz_ms_191->payload.PatVariant.f2; {
        if ((path.len) < 1) { return;} qoz_string vname = path.data[(path.len) - 1]; int64_t i = 0; { qoz_Vec__qoz_Pattern __col = sub_pats; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Pattern* sp = __col.data[__i]; (void)sp; qoz_Pattern* _qoz_ms_192 = sp; switch (_qoz_ms_192->tag) { case qoz_Pattern_PatBind: { qoz_string bname = _qoz_ms_192->payload.PatBind.f1; (qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, bname, emit_variant_payload_typeexpr(e, enum_name, vname, i)), 0);  break; } default: { NULL;  break; } } 0; i = i + 1; } }
    }
    0;  break; } case qoz_Pattern_PatBind: { qoz_string bname = _qoz_ms_191->payload.PatBind.f1; qoz_Option__qoz_string* _qoz_ms_193 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, bname); switch (_qoz_ms_193->tag) { case qoz_Option__qoz_string_Some: { NULL;  break; } case qoz_Option__qoz_string_None: { {
        qoz_Span unit_sp = ((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 }); qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, bname, qoz_make_TypeExpr_TEUnit(unit_sp)); 
    }
    0;  break; } } 0;  break; } default: { NULL;  break; } } 0; 
    return;
}

qoz_string emit_default_value_for(qoz_string ctype) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if (qoz_string_eq(ctype, QOZ_STR_LIT("qoz_string"))) { return QOZ_STR_LIT("((qoz_string){ NULL, 0 })");} if (qoz_string_eq(ctype, QOZ_STR_LIT("bool"))) { return QOZ_STR_LIT("false");} if (qoz_string_has_suffix(ctype, QOZ_STR_LIT("*"))) { return QOZ_STR_LIT("NULL");} if (emit_is_int_ctype(ctype)) { return QOZ_STR_LIT("0");} if (qoz_string_eq(ctype, QOZ_STR_LIT("double")) || qoz_string_eq(ctype, QOZ_STR_LIT("float"))) { return QOZ_STR_LIT("0");} if (qoz_string_eq(ctype, QOZ_STR_LIT("char"))) { return QOZ_STR_LIT("0");} qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_cat(qoz_string_cat(QOZ_STR_LIT("(("), ctype), QOZ_STR_LIT("){0})"));
}

bool emit_is_int_ctype(qoz_string ctype) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if (qoz_string_eq(ctype, QOZ_STR_LIT("int8_t"))) { return true;} if (qoz_string_eq(ctype, QOZ_STR_LIT("int16_t"))) { return true;} if (qoz_string_eq(ctype, QOZ_STR_LIT("int32_t"))) { return true;} if (qoz_string_eq(ctype, QOZ_STR_LIT("int64_t"))) { return true;} if (qoz_string_eq(ctype, QOZ_STR_LIT("uint8_t"))) { return true;} if (qoz_string_eq(ctype, QOZ_STR_LIT("uint16_t"))) { return true;} if (qoz_string_eq(ctype, QOZ_STR_LIT("uint32_t"))) { return true;} if (qoz_string_eq(ctype, QOZ_STR_LIT("uint64_t"))) { return true;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

void emit_emit_field(qoz_Emitter* e, qoz_Expr* base, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&base);
    qoz_string op = emit_field_access_op(e, base); emit_emit_expr(e, base); qoz_string _qoz_bv_190;
    {
        qoz_Strbuf _qoz_sb_2190_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2190_13); strings_sb_append(&_qoz_sb_2190_13, op); strings_sb_append(&_qoz_sb_2190_13, name); _qoz_bv_190 = strings_sb_finish(&_qoz_sb_2190_13);
    }
    emit_push(e, _qoz_bv_190); 
    return;
}

qoz_string emit_field_access_op(qoz_Emitter* e, qoz_Expr* base) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&base);
    qoz_Expr* _qoz_ms_194 = base; qoz_string _qoz_mv_194 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_194->tag) { case qoz_Expr_EIdent: { qoz_string ident = _qoz_ms_194->payload.EIdent.f1; qoz_Option__qoz_TypeExpr* _qoz_ms_195 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->locals, ident); qoz_string _qoz_mv_195 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_195->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* te = _qoz_ms_195->payload.Some.f0; qoz_TypeExpr* _qoz_ms_196 = te; qoz_string _qoz_mv_196 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_196->tag) { case qoz_TypeExpr_TEPtr: { _qoz_mv_196 = (QOZ_STR_LIT("->"));  break; } default: { _qoz_mv_196 = (QOZ_STR_LIT("."));  break; } } _qoz_mv_195 = (_qoz_mv_196);  break; } case qoz_Option__qoz_TypeExpr_None: { _qoz_mv_195 = (QOZ_STR_LIT("."));  break; } } _qoz_mv_194 = (_qoz_mv_195);  break; } case qoz_Expr_EUnary: { qoz_UnaryOp* op = _qoz_ms_194->payload.EUnary.f1; qoz_UnaryOp* _qoz_ms_197 = op; qoz_string _qoz_mv_197 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_197->tag) { case qoz_UnaryOp_UOpAddr: { _qoz_mv_197 = (QOZ_STR_LIT("->"));  break; } default: { _qoz_mv_197 = (QOZ_STR_LIT("."));  break; } } _qoz_mv_194 = (_qoz_mv_197);  break; } default: { _qoz_mv_194 = (QOZ_STR_LIT("."));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_194;
}

void emit_walk_assign_for_map_helpers(qoz_Emitter* e, qoz_Expr* lhs) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&lhs);
    qoz_Expr* _qoz_ms_198 = lhs; switch (_qoz_ms_198->tag) { case qoz_Expr_EIndex: { qoz_Expr* base = _qoz_ms_198->payload.EIndex.f1; {
        qoz_TypeExpr* bte = emit_base_type_for_walk(e, base); qoz_gc_push_root(&bte); qoz_TypeExpr* _qoz_ms_199 = bte; switch (_qoz_ms_199->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_199->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_199->payload.TENamed.f2; if ((((path.len) >= 1) && qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Map"))) && ((args.len) == 2)) { emit_register_map_helper(e, QOZ_STR_LIT("map_set"), args); } 0;  break; } default: { NULL;  break; } } 0; 
    }
    0;  break; } default: { NULL;  break; } } 0; 
    return;
}

qoz_TypeExpr* emit_base_type_for_walk(qoz_Emitter* e, qoz_Expr* ex) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_200 = ex; qoz_TypeExpr* _qoz_mv_200 = NULL; switch (_qoz_ms_200->tag) { case qoz_Expr_EIdent: { qoz_Span sp = _qoz_ms_200->payload.EIdent.f0; qoz_string name = _qoz_ms_200->payload.EIdent.f1; qoz_Option__qoz_TypeExpr* _qoz_ms_201 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->locals, name); qoz_TypeExpr* _qoz_mv_201 = NULL; switch (_qoz_ms_201->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* te = _qoz_ms_201->payload.Some.f0; _qoz_mv_201 = (te);  break; } case qoz_Option__qoz_TypeExpr_None: { _qoz_mv_201 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } _qoz_mv_200 = (_qoz_mv_201);  break; } case qoz_Expr_EField: { qoz_Span sp = _qoz_ms_200->payload.EField.f0; qoz_Expr* b = _qoz_ms_200->payload.EField.f1; qoz_string name = _qoz_ms_200->payload.EField.f2; qoz_TypeExpr* _qoz_bv_191;
    {
        qoz_TypeExpr* bte = emit_base_type_for_walk(e, b); qoz_gc_push_root(&bte); _qoz_bv_191 = emit_field_typeexpr_for_no_die(e, bte, name, sp);
    }
    _qoz_mv_200 = (_qoz_bv_191);  break; } case qoz_Expr_EUnary: { qoz_Span sp = _qoz_ms_200->payload.EUnary.f0; qoz_UnaryOp* op = _qoz_ms_200->payload.EUnary.f1; qoz_Expr* rhs = _qoz_ms_200->payload.EUnary.f2; qoz_UnaryOp* _qoz_ms_202 = op; qoz_TypeExpr* _qoz_mv_202 = NULL; switch (_qoz_ms_202->tag) { case qoz_UnaryOp_UOpDeref: { qoz_TypeExpr* _qoz_bv_192;
    {
        qoz_TypeExpr* inner = emit_base_type_for_walk(e, rhs); qoz_gc_push_root(&inner); qoz_TypeExpr* _qoz_ms_203 = inner; qoz_TypeExpr* _qoz_mv_203 = NULL; switch (_qoz_ms_203->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* t = _qoz_ms_203->payload.TEPtr.f1; _qoz_mv_203 = (t);  break; } default: { _qoz_mv_203 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } _qoz_bv_192 = _qoz_mv_203;
    }
    _qoz_mv_202 = (_qoz_bv_192);  break; } case qoz_UnaryOp_UOpAddr: { _qoz_mv_202 = (qoz_make_TypeExpr_TEPtr(sp, emit_base_type_for_walk(e, rhs)));  break; } default: { _qoz_mv_202 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } _qoz_mv_200 = (_qoz_mv_202);  break; } default: { _qoz_mv_200 = (qoz_make_TypeExpr_TEUnit(emit_span_of_expr(ex)));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_200;
}

qoz_TypeExpr* emit_field_typeexpr_for_no_die(qoz_Emitter* e, qoz_TypeExpr* base_te, qoz_string field, qoz_Span sp) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&base_te);
    qoz_TypeExpr* _qoz_ms_204 = base_te; qoz_TypeExpr* _qoz_mv_204 = NULL; switch (_qoz_ms_204->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_204->payload.TEPtr.f1; _qoz_mv_204 = (inner);  break; } default: { _qoz_mv_204 = (base_te);  break; } } qoz_TypeExpr* unwrapped = _qoz_mv_204; qoz_gc_push_root(&unwrapped); qoz_TypeExpr* _qoz_ms_205 = unwrapped; qoz_TypeExpr* _qoz_mv_205 = NULL; switch (_qoz_ms_205->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_205->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_205->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_193;
    {
        if ((path.len) < 1) { return qoz_make_TypeExpr_TEUnit(sp);} qoz_string lookup = emit_type_lookup_key(e, path); qoz_Option__qoz_Decl* _qoz_ms_206 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_decls, lookup); qoz_Option__qoz_Decl* _qoz_mv_206 = NULL; switch (_qoz_ms_206->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_206->payload.Some.f0; _qoz_mv_206 = (qoz_make_Option__qoz_Decl_Some(d));  break; } case qoz_Option__qoz_Decl_None: { _qoz_mv_206 = (qoz_map_get__qoz_string__qoz_Decl(&e->struct_decls, lookup));  break; } } qoz_Option__qoz_Decl* decl_opt = _qoz_mv_206; qoz_gc_push_root(&decl_opt); qoz_Option__qoz_Decl* _qoz_ms_207 = decl_opt; qoz_TypeExpr* _qoz_mv_207 = NULL; switch (_qoz_ms_207->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_207->payload.Some.f0; qoz_Decl* _qoz_ms_208 = decl; qoz_TypeExpr* _qoz_mv_208 = NULL; switch (_qoz_ms_208->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_208->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_208->payload.DStruct.f3; qoz_TypeExpr* _qoz_bv_194;
    {
        { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; if (qoz_string_eq(f.name, field)) { if ((args.len) == (params.len)) { return emit_substitute_type(e, f.ty, params, args);} return f.ty;} } }_qoz_bv_194 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_208 = (_qoz_bv_194);  break; } default: { _qoz_mv_208 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } _qoz_mv_207 = (_qoz_mv_208);  break; } case qoz_Option__qoz_Decl_None: { _qoz_mv_207 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } _qoz_bv_193 = _qoz_mv_207;
    }
    _qoz_mv_205 = (_qoz_bv_193);  break; } default: { _qoz_mv_205 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_205;
}

void emit_register_map_helper(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_TypeExpr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if (!qoz_map_contains__qoz_string__qoz_Decl(&e->generic_fn_decls, name)) { return;} qoz_string mangled = emit_mangle_inst(e, name, args); if (qoz_map_contains__qoz_string__bool(&e->fn_seen, mangled)) { return;} qoz_map_set__qoz_string__bool(&e->fn_seen, mangled, true); qoz_vec_push__qoz_Instantiation(&e->fn_insts, ((qoz_Instantiation){ .name = name, .args = args, .mangled = mangled })); qoz_Option__qoz_Decl* _qoz_ms_209 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_fn_decls, name); switch (_qoz_ms_209->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_209->payload.Some.f0; emit_walk_fn_body_with_subst(e, d, args);  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; 
    return;
}

void emit_emit_assign(qoz_Emitter* e, qoz_Expr* lhs, qoz_Expr* rhs) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&lhs);
    qoz_gc_push_root(&rhs);
    qoz_Expr* _qoz_ms_210 = lhs; switch (_qoz_ms_210->tag) { case qoz_Expr_EIndex: { qoz_Expr* base = _qoz_ms_210->payload.EIndex.f1; qoz_Expr* idx = _qoz_ms_210->payload.EIndex.f2; {
        qoz_TypeExpr* te = emit_infer_base_typeexpr(e, base); qoz_gc_push_root(&te); qoz_TypeExpr* _qoz_ms_211 = te; switch (_qoz_ms_211->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_211->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_211->payload.TENamed.f2; if ((((path.len) >= 1) && qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Map"))) && ((args.len) == 2)) { emit_register_map_helper(e, QOZ_STR_LIT("map_set"), args); qoz_string k_m = emit_mangle_type(e, args.data[0]); qoz_string v_m = emit_mangle_type(e, args.data[1]); qoz_string _qoz_bv_195;
    {
        qoz_Strbuf _qoz_sb_2331_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2331_25); strings_sb_append(&_qoz_sb_2331_25, QOZ_STR_LIT("qoz_map_set__")); strings_sb_append(&_qoz_sb_2331_25, k_m); strings_sb_append(&_qoz_sb_2331_25, QOZ_STR_LIT("__")); strings_sb_append(&_qoz_sb_2331_25, v_m); strings_sb_append(&_qoz_sb_2331_25, QOZ_STR_LIT("(&")); _qoz_bv_195 = strings_sb_finish(&_qoz_sb_2331_25);
    }
    emit_push(e, _qoz_bv_195); emit_emit_expr(e, base); emit_push(e, QOZ_STR_LIT(", ")); emit_emit_expr(e, idx); emit_push(e, QOZ_STR_LIT(", ")); emit_emit_expr(e, rhs); emit_push(e, QOZ_STR_LIT(")")); return;} 0;  break; } default: { NULL;  break; } } 0; 
    }
    0;  break; } default: { NULL;  break; } } 0; emit_emit_expr(e, lhs); emit_push(e, QOZ_STR_LIT(" = ")); qoz_TypeExpr* hint = emit_lvalue_hint(e, lhs); qoz_gc_push_root(&hint); emit_emit_value_with_hint(e, rhs, hint); 
    return;
}

qoz_TypeExpr* emit_lvalue_hint(qoz_Emitter* e, qoz_Expr* lhs) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&lhs);
    qoz_Span sp = ((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 }); qoz_Expr* _qoz_ms_212 = lhs; qoz_TypeExpr* _qoz_mv_212 = NULL; switch (_qoz_ms_212->tag) { case qoz_Expr_EField: { qoz_Expr* base = _qoz_ms_212->payload.EField.f1; qoz_string name = _qoz_ms_212->payload.EField.f2; qoz_TypeExpr* _qoz_bv_196;
    {
        qoz_TypeExpr* bte = emit_infer_base_typeexpr(e, base); qoz_gc_push_root(&bte); _qoz_bv_196 = emit_field_typeexpr_for_no_die(e, bte, name, sp);
    }
    _qoz_mv_212 = (_qoz_bv_196);  break; } case qoz_Expr_EIndex: { qoz_Expr* base = _qoz_ms_212->payload.EIndex.f1; qoz_TypeExpr* _qoz_bv_197;
    {
        qoz_TypeExpr* bte = emit_infer_base_typeexpr(e, base); qoz_gc_push_root(&bte); qoz_TypeExpr* _qoz_ms_213 = bte; qoz_TypeExpr* _qoz_mv_213 = NULL; switch (_qoz_ms_213->tag) { case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_213->payload.TEPtr.f1; _qoz_mv_213 = (inner);  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_213->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_213->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_198;
    {
        if ((((args.len) >= 1) && ((path.len) >= 1)) && qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Vec"))) { return args.data[0];} _qoz_bv_198 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_213 = (_qoz_bv_198);  break; } default: { _qoz_mv_213 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } _qoz_bv_197 = _qoz_mv_213;
    }
    _qoz_mv_212 = (_qoz_bv_197);  break; } case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_212->payload.EIdent.f1; qoz_Option__qoz_TypeExpr* _qoz_ms_214 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->locals, name); qoz_TypeExpr* _qoz_mv_214 = NULL; switch (_qoz_ms_214->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* te = _qoz_ms_214->payload.Some.f0; _qoz_mv_214 = (te);  break; } case qoz_Option__qoz_TypeExpr_None: { _qoz_mv_214 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } _qoz_mv_212 = (_qoz_mv_214);  break; } default: { _qoz_mv_212 = (qoz_make_TypeExpr_TEUnit(sp));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_212;
}

void emit_emit_index(qoz_Emitter* e, qoz_Expr* base, qoz_Expr* idx) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&base);
    qoz_gc_push_root(&idx);
    qoz_TypeExpr* te = emit_infer_base_typeexpr(e, base); qoz_gc_push_root(&te); qoz_TypeExpr* _qoz_ms_215 = te; switch (_qoz_ms_215->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_215->payload.TENamed.f1; if ((path.len) >= 1) { qoz_string n = path.data[(path.len) - 1]; if (qoz_string_eq(n, QOZ_STR_LIT("Vec"))) { qoz_string op = emit_field_access_op(e, base); emit_emit_expr(e, base); qoz_string _qoz_bv_199;
    {
        qoz_Strbuf _qoz_sb_2391_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2391_25); strings_sb_append(&_qoz_sb_2391_25, op); strings_sb_append(&_qoz_sb_2391_25, QOZ_STR_LIT("data[")); _qoz_bv_199 = strings_sb_finish(&_qoz_sb_2391_25);
    }
    emit_push(e, _qoz_bv_199); emit_emit_expr(e, idx); emit_push(e, QOZ_STR_LIT("]")); return;} } 0;  break; } default: { NULL;  break; } } 0; emit_emit_expr(e, base); emit_push(e, QOZ_STR_LIT("[")); emit_emit_expr(e, idx); emit_push(e, QOZ_STR_LIT("]")); 
    return;
}

void emit_emit_hash_builtin(qoz_Emitter* e, qoz_Expr* arg) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&arg);
    qoz_string ct = emit_infer_expr_ctype(e, arg); if (qoz_string_eq(ct, QOZ_STR_LIT("qoz_string"))) { emit_push(e, QOZ_STR_LIT("qoz_string_hash(")); emit_emit_expr(e, arg); emit_push(e, QOZ_STR_LIT(")")); return;} qoz_string record_name = emit_record_struct_name_of_ctype(e, ct); if (!qoz_string_eq(record_name, QOZ_STR_LIT(""))) { qoz_string _qoz_bv_200;
    {
        qoz_Strbuf _qoz_sb_2415_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2415_17); strings_sb_append(&_qoz_sb_2415_17, QOZ_STR_LIT("qoz_hash_")); strings_sb_append(&_qoz_sb_2415_17, record_name); strings_sb_append(&_qoz_sb_2415_17, QOZ_STR_LIT("(")); _qoz_bv_200 = strings_sb_finish(&_qoz_sb_2415_17);
    }
    emit_push(e, _qoz_bv_200); emit_emit_expr(e, arg); emit_push(e, QOZ_STR_LIT(")")); return;} emit_push(e, QOZ_STR_LIT("(uint64_t)(")); emit_emit_expr(e, arg); emit_push(e, QOZ_STR_LIT(")")); 
    return;
}

void emit_emit_len_builtin(qoz_Emitter* e, qoz_Expr* arg) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&arg);
    qoz_string ct = emit_infer_expr_ctype(e, arg); if (qoz_string_eq(ct, QOZ_STR_LIT("qoz_string"))) { emit_push(e, QOZ_STR_LIT("(")); emit_emit_expr(e, arg); emit_push(e, QOZ_STR_LIT(").len")); return;} emit_push(e, QOZ_STR_LIT("(")); qoz_string op = emit_field_access_op(e, arg); if (qoz_string_eq(op, QOZ_STR_LIT("->"))) { emit_emit_expr(e, arg); emit_push(e, QOZ_STR_LIT("->len")); }  else { emit_emit_expr(e, arg); emit_push(e, QOZ_STR_LIT(".len")); } emit_push(e, QOZ_STR_LIT(")")); 
    return;
}

void emit_emit_call(qoz_Emitter* e, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&callee);
    qoz_string qualified = emit_qualified_call_name(e, callee); if (!qoz_string_eq(qualified, QOZ_STR_LIT(""))) { if (((type_args.len) > 0) && qoz_map_contains__qoz_string__qoz_Decl(&e->generic_fn_decls, qualified)) { qoz_string mi = emit_mangle_inst(e, qualified, type_args); qoz_string _qoz_bv_201;
    {
        qoz_Strbuf _qoz_sb_2450_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2450_21); strings_sb_append(&_qoz_sb_2450_21, QOZ_STR_LIT("qoz_")); strings_sb_append(&_qoz_sb_2450_21, mi); strings_sb_append(&_qoz_sb_2450_21, QOZ_STR_LIT("(")); _qoz_bv_201 = strings_sb_finish(&_qoz_sb_2450_21);
    }
    emit_push(e, _qoz_bv_201); int64_t i = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } emit_emit_expr(e, a); i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); return;} qoz_Option__qoz_string* _qoz_ms_216 = qoz_map_get__qoz_string__qoz_string(&e->externs, qualified); qoz_string _qoz_mv_216 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_216->tag) { case qoz_Option__qoz_string_Some: { qoz_string sym = _qoz_ms_216->payload.Some.f0; _qoz_mv_216 = (sym);  break; } case qoz_Option__qoz_string_None: { _qoz_mv_216 = (qualified);  break; } } qoz_string dispatch_name = _qoz_mv_216; qoz_string _qoz_bv_202;
    {
        qoz_Strbuf _qoz_sb_2464_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2464_17); strings_sb_append(&_qoz_sb_2464_17, dispatch_name); strings_sb_append(&_qoz_sb_2464_17, QOZ_STR_LIT("(")); _qoz_bv_202 = strings_sb_finish(&_qoz_sb_2464_17);
    }
    emit_push(e, _qoz_bv_202); qoz_Vec__qoz_TypeExpr hints = qoz_vec_make__qoz_TypeExpr(); qoz_Option__qoz_Vec__qoz_TypeExpr* _qoz_ms_217 = qoz_map_get__qoz_string__qoz_Vec__qoz_TypeExpr(&e->fn_params, qualified); switch (_qoz_ms_217->tag) { case qoz_Option__qoz_Vec__qoz_TypeExpr_Some: { qoz_Vec__qoz_TypeExpr pts = _qoz_ms_217->payload.Some.f0; hints = pts;  break; } case qoz_Option__qoz_Vec__qoz_TypeExpr_None: { NULL;  break; } } 0; int64_t i = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } if (i < (hints.len)) { emit_emit_value_with_hint(e, a, hints.data[i]); }  else { emit_emit_expr(e, a); } i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); return;} qoz_Expr* _qoz_ms_218 = callee; switch (_qoz_ms_218->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_218->payload.EIdent.f1; {
        if (qoz_string_eq(name, QOZ_STR_LIT("size_of")) && ((args.len) == 1)) { emit_push(e, QOZ_STR_LIT("(int64_t)sizeof(int64_t)")); return;} if (qoz_string_eq(name, QOZ_STR_LIT("len")) && ((args.len) == 1)) { emit_emit_len_builtin(e, args.data[0]); return;} if (qoz_string_eq(name, QOZ_STR_LIT("hash")) && ((args.len) == 1)) { emit_emit_hash_builtin(e, args.data[0]); return;} 
    }
    0;  break; } default: { NULL;  break; } } 0; qoz_Expr* _qoz_ms_219 = callee; switch (_qoz_ms_219->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_219->payload.EIdent.f1; qoz_Option__qoz_string* _qoz_ms_220 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); switch (_qoz_ms_220->tag) { case qoz_Option__qoz_string_Some: { qoz_string enum_name = _qoz_ms_220->payload.Some.f0; {
        qoz_string _qoz_bv_203;
    {
        qoz_Strbuf _qoz_sb_2505_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2505_21); strings_sb_append(&_qoz_sb_2505_21, QOZ_STR_LIT("qoz_make_")); strings_sb_append(&_qoz_sb_2505_21, enum_name); strings_sb_append(&_qoz_sb_2505_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_2505_21, name); strings_sb_append(&_qoz_sb_2505_21, QOZ_STR_LIT("(")); _qoz_bv_203 = strings_sb_finish(&_qoz_sb_2505_21);
    }
    emit_push(e, _qoz_bv_203); int64_t i = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } emit_emit_expr(e, a); i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } case qoz_Option__qoz_string_None: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0; if ((type_args.len) > 0) { qoz_Expr* _qoz_ms_221 = callee; switch (_qoz_ms_221->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_221->payload.EIdent.f1; if (qoz_map_contains__qoz_string__qoz_Decl(&e->generic_fn_decls, name)) { qoz_string mi = emit_mangle_inst(e, name, type_args); qoz_string _qoz_bv_204;
    {
        qoz_Strbuf _qoz_sb_2526_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2526_25); strings_sb_append(&_qoz_sb_2526_25, QOZ_STR_LIT("qoz_")); strings_sb_append(&_qoz_sb_2526_25, mi); strings_sb_append(&_qoz_sb_2526_25, QOZ_STR_LIT("(")); _qoz_bv_204 = strings_sb_finish(&_qoz_sb_2526_25);
    }
    emit_push(e, _qoz_bv_204); int64_t i = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } emit_emit_expr(e, a); i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); return;} 0;  break; } default: { NULL;  break; } } 0; } qoz_Expr* _qoz_ms_222 = callee; switch (_qoz_ms_222->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_222->payload.EIdent.f1; qoz_Option__qoz_string* _qoz_ms_223 = qoz_map_get__qoz_string__qoz_string(&e->externs, name); switch (_qoz_ms_223->tag) { case qoz_Option__qoz_string_Some: { qoz_string sym = _qoz_ms_223->payload.Some.f0; {
        qoz_string _qoz_bv_205;
    {
        qoz_Strbuf _qoz_sb_2548_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2548_21); strings_sb_append(&_qoz_sb_2548_21, sym); strings_sb_append(&_qoz_sb_2548_21, QOZ_STR_LIT("(")); _qoz_bv_205 = strings_sb_finish(&_qoz_sb_2548_21);
    }
    emit_push(e, _qoz_bv_205); qoz_Vec__qoz_TypeExpr hints = qoz_vec_make__qoz_TypeExpr(); qoz_Option__qoz_Vec__qoz_TypeExpr* _qoz_ms_224 = qoz_map_get__qoz_string__qoz_Vec__qoz_TypeExpr(&e->fn_params, name); switch (_qoz_ms_224->tag) { case qoz_Option__qoz_Vec__qoz_TypeExpr_Some: { qoz_Vec__qoz_TypeExpr pts = _qoz_ms_224->payload.Some.f0; hints = pts;  break; } case qoz_Option__qoz_Vec__qoz_TypeExpr_None: { NULL;  break; } } 0; int64_t i = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } if (i < (hints.len)) { emit_emit_value_with_hint(e, a, hints.data[i]); }  else { emit_emit_expr(e, a); } i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } case qoz_Option__qoz_string_None: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0; qoz_TypeExpr* callee_te = emit_callee_value_typeexpr(e, callee); qoz_gc_push_root(&callee_te); qoz_TypeExpr* _qoz_ms_225 = callee_te; switch (_qoz_ms_225->tag) { case qoz_TypeExpr_TEFn: { qoz_Vec__qoz_TypeExpr fn_params_te = _qoz_ms_225->payload.TEFn.f1; {
        emit_push(e, QOZ_STR_LIT("(")); emit_emit_expr(e, callee); emit_push(e, QOZ_STR_LIT(").fn((")); emit_emit_expr(e, callee); emit_push(e, QOZ_STR_LIT(").env")); int64_t i = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; emit_push(e, QOZ_STR_LIT(", ")); if (i < (fn_params_te.len)) { emit_emit_value_with_hint(e, a, fn_params_te.data[i]); }  else { emit_emit_expr(e, a); } i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } default: { NULL;  break; } } 0; emit_emit_expr(e, callee); emit_push(e, QOZ_STR_LIT("(")); qoz_Vec__qoz_TypeExpr fallback_hints = qoz_vec_make__qoz_TypeExpr(); qoz_Expr* _qoz_ms_226 = callee; switch (_qoz_ms_226->tag) { case qoz_Expr_EIdent: { qoz_string cname = _qoz_ms_226->payload.EIdent.f1; qoz_Option__qoz_Vec__qoz_TypeExpr* _qoz_ms_227 = qoz_map_get__qoz_string__qoz_Vec__qoz_TypeExpr(&e->fn_params, cname); switch (_qoz_ms_227->tag) { case qoz_Option__qoz_Vec__qoz_TypeExpr_Some: { qoz_Vec__qoz_TypeExpr pts = _qoz_ms_227->payload.Some.f0; fallback_hints = pts;  break; } case qoz_Option__qoz_Vec__qoz_TypeExpr_None: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0; int64_t i = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } if (i < (fallback_hints.len)) { emit_emit_value_with_hint(e, a, fallback_hints.data[i]); }  else { emit_emit_expr(e, a); } i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); 
    return;
}

qoz_TypeExpr* emit_callee_value_typeexpr(qoz_Emitter* e, qoz_Expr* callee) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&callee);
    qoz_Expr* _qoz_ms_228 = callee; qoz_TypeExpr* _qoz_mv_228 = NULL; switch (_qoz_ms_228->tag) { case qoz_Expr_EIdent: { qoz_Span sp = _qoz_ms_228->payload.EIdent.f0; qoz_string name = _qoz_ms_228->payload.EIdent.f1; qoz_TypeExpr* _qoz_bv_206;
    {
        qoz_Option__qoz_TypeExpr* _qoz_ms_229 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->locals, name); switch (_qoz_ms_229->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* te = _qoz_ms_229->payload.Some.f0; return te;  break; } default: { NULL;  break; } } 0; _qoz_bv_206 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_228 = (_qoz_bv_206);  break; } default: { _qoz_mv_228 = (qoz_make_TypeExpr_TEUnit(emit_span_of_expr(callee)));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_228;
}

void emit_ensure_unary_byval_dispatch_helper(qoz_Emitter* e, qoz_string fn_name, qoz_string operand_ct, qoz_string ret_ct) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if (qoz_map_contains__qoz_string__bool(&e->byval_helpers_emitted, fn_name)) { return;} qoz_map_set__qoz_string__bool(&e->byval_helpers_emitted, fn_name, true); qoz_string src = QOZ_STR_LIT("static "); src = qoz_string_cat(src, ret_ct); src = qoz_string_cat(src, QOZ_STR_LIT(" _qoz_byval_")); src = qoz_string_cat(src, fn_name); src = qoz_string_cat(src, QOZ_STR_LIT("(")); src = qoz_string_cat(src, operand_ct); src = qoz_string_cat(src, QOZ_STR_LIT(" a) { return ")); src = qoz_string_cat(src, fn_name); src = qoz_string_cat(src, QOZ_STR_LIT("(&a); }\n")); qoz_string proto = QOZ_STR_LIT("static "); proto = qoz_string_cat(proto, ret_ct); proto = qoz_string_cat(proto, QOZ_STR_LIT(" _qoz_byval_")); proto = qoz_string_cat(proto, fn_name); proto = qoz_string_cat(proto, QOZ_STR_LIT("(")); proto = qoz_string_cat(proto, operand_ct); proto = qoz_string_cat(proto, QOZ_STR_LIT(");\n")); qoz_vec_push__qoz_string(&e->synth_fn_decls, proto); qoz_vec_push__qoz_string(&e->synth_fn_defs, src); 
    return;
}

qoz_string emit_unary_op_text(qoz_UnaryOp* op) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&op);
    qoz_UnaryOp* _qoz_ms_230 = op; qoz_string _qoz_mv_230 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_230->tag) { case qoz_UnaryOp_UOpNeg: { _qoz_mv_230 = (QOZ_STR_LIT("unary-"));  break; } case qoz_UnaryOp_UOpNot: { _qoz_mv_230 = (QOZ_STR_LIT("unary!"));  break; } case qoz_UnaryOp_UOpDeref: { _qoz_mv_230 = (QOZ_STR_LIT(""));  break; } case qoz_UnaryOp_UOpAddr: { _qoz_mv_230 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_230;
}

void emit_ensure_byval_dispatch_helper(qoz_Emitter* e, qoz_string fn_name, qoz_string operand_ct, qoz_string ret_ct) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if (qoz_map_contains__qoz_string__bool(&e->byval_helpers_emitted, fn_name)) { return;} qoz_map_set__qoz_string__bool(&e->byval_helpers_emitted, fn_name, true); qoz_string src = QOZ_STR_LIT("static "); src = qoz_string_cat(src, ret_ct); src = qoz_string_cat(src, QOZ_STR_LIT(" _qoz_byval_")); src = qoz_string_cat(src, fn_name); src = qoz_string_cat(src, QOZ_STR_LIT("(")); src = qoz_string_cat(src, operand_ct); src = qoz_string_cat(src, QOZ_STR_LIT(" a, ")); src = qoz_string_cat(src, operand_ct); src = qoz_string_cat(src, QOZ_STR_LIT(" b) { return ")); src = qoz_string_cat(src, fn_name); qoz_string proto = QOZ_STR_LIT("static "); proto = qoz_string_cat(proto, ret_ct); proto = qoz_string_cat(proto, QOZ_STR_LIT(" _qoz_byval_")); proto = qoz_string_cat(proto, fn_name); proto = qoz_string_cat(proto, QOZ_STR_LIT("(")); proto = qoz_string_cat(proto, operand_ct); proto = qoz_string_cat(proto, QOZ_STR_LIT(", ")); proto = qoz_string_cat(proto, operand_ct); proto = qoz_string_cat(proto, QOZ_STR_LIT(");\n")); qoz_vec_push__qoz_string(&e->synth_fn_decls, proto); src = qoz_string_cat(src, QOZ_STR_LIT("(&a, &b); }\n")); qoz_vec_push__qoz_string(&e->synth_fn_defs, src); 
    return;
}

qoz_string emit_binary_op_text(qoz_BinaryOp* op) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&op);
    qoz_BinaryOp* _qoz_ms_231 = op; qoz_string _qoz_mv_231 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_231->tag) { case qoz_BinaryOp_BOpEq: { _qoz_mv_231 = (QOZ_STR_LIT("=="));  break; } case qoz_BinaryOp_BOpNe: { _qoz_mv_231 = (QOZ_STR_LIT("!="));  break; } case qoz_BinaryOp_BOpLt: { _qoz_mv_231 = (QOZ_STR_LIT("<"));  break; } case qoz_BinaryOp_BOpGt: { _qoz_mv_231 = (QOZ_STR_LIT(">"));  break; } case qoz_BinaryOp_BOpLe: { _qoz_mv_231 = (QOZ_STR_LIT("<="));  break; } case qoz_BinaryOp_BOpGe: { _qoz_mv_231 = (QOZ_STR_LIT(">="));  break; } case qoz_BinaryOp_BOpAdd: { _qoz_mv_231 = (QOZ_STR_LIT("+"));  break; } case qoz_BinaryOp_BOpSub: { _qoz_mv_231 = (QOZ_STR_LIT("-"));  break; } case qoz_BinaryOp_BOpMul: { _qoz_mv_231 = (QOZ_STR_LIT("*"));  break; } case qoz_BinaryOp_BOpDiv: { _qoz_mv_231 = (QOZ_STR_LIT("/"));  break; } case qoz_BinaryOp_BOpMod: { _qoz_mv_231 = (QOZ_STR_LIT("%"));  break; } default: { _qoz_mv_231 = (QOZ_STR_LIT(""));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_231;
}

qoz_string emit_record_struct_name_of_ctype(qoz_Emitter* e, qoz_string ct) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if ((ct).len < 5) { return QOZ_STR_LIT("");} qoz_string prefix = qoz_string_slice(ct, 0, 4); if (!qoz_string_eq(prefix, QOZ_STR_LIT("qoz_"))) { return QOZ_STR_LIT("");} int64_t last = qoz_string_byte_at(ct, (ct).len - 1); if (last == 42) { return QOZ_STR_LIT("");} qoz_string name = qoz_string_slice(ct, 4, (ct).len); if (qoz_map_contains__qoz_string__bool(&e->is_enum, name)) { return QOZ_STR_LIT("");} qoz_gc_shadow_set_top(_qoz_shadow_guard); return name;
}

void emit_emit_binary(qoz_Emitter* e, qoz_BinaryOp* op, qoz_Expr* lhs, qoz_Expr* rhs) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&op);
    qoz_gc_push_root(&lhs);
    qoz_gc_push_root(&rhs);
    qoz_string lct = emit_infer_expr_ctype(e, lhs); if (qoz_string_eq(lct, QOZ_STR_LIT("qoz_string"))) { qoz_BinaryOp* _qoz_ms_232 = op; switch (_qoz_ms_232->tag) { case qoz_BinaryOp_BOpEq: { {
        emit_push(e, QOZ_STR_LIT("qoz_string_eq(")); emit_emit_expr(e, lhs); emit_push(e, QOZ_STR_LIT(", ")); emit_emit_expr(e, rhs); emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } case qoz_BinaryOp_BOpNe: { {
        emit_push(e, QOZ_STR_LIT("!qoz_string_eq(")); emit_emit_expr(e, lhs); emit_push(e, QOZ_STR_LIT(", ")); emit_emit_expr(e, rhs); emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } default: { NULL;  break; } } 0; } qoz_string op_text = emit_binary_op_text(op); if (!qoz_string_eq(op_text, QOZ_STR_LIT(""))) { qoz_TypeExpr* lhs_te = emit_infer_value_te(e, lhs); qoz_gc_push_root(&lhs_te); qoz_string tname = emit_operator_first_param_type_name(lhs_te); if (!qoz_string_eq(tname, QOZ_STR_LIT(""))) { qoz_string key = qoz_string_cat(qoz_string_cat(op_text, QOZ_STR_LIT("::")), tname); qoz_Option__qoz_string* _qoz_ms_233 = qoz_map_get__qoz_string__qoz_string(&e->op_dispatch, key); switch (_qoz_ms_233->tag) { case qoz_Option__qoz_string_Some: { qoz_string fn_name = _qoz_ms_233->payload.Some.f0; {
        qoz_string ct = emit_c_type_for(e, lhs_te); qoz_Option__qoz_TypeExpr* _qoz_ms_234 = qoz_map_get__qoz_string__qoz_TypeExpr(&e->fn_returns, fn_name); qoz_TypeExpr* _qoz_mv_234 = NULL; switch (_qoz_ms_234->tag) { case qoz_Option__qoz_TypeExpr_Some: { qoz_TypeExpr* t = _qoz_ms_234->payload.Some.f0; _qoz_mv_234 = (t);  break; } case qoz_Option__qoz_TypeExpr_None: { _qoz_mv_234 = (qoz_make_TypeExpr_TEUnit(((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 })));  break; } } qoz_TypeExpr* ret_te = _qoz_mv_234; qoz_gc_push_root(&ret_te); qoz_string ret_ct = emit_c_type_for(e, ret_te); emit_ensure_byval_dispatch_helper(e, fn_name, ct, ret_ct); qoz_string _qoz_bv_207;
    {
        qoz_Strbuf _qoz_sb_2776_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2776_25); strings_sb_append(&_qoz_sb_2776_25, QOZ_STR_LIT("_qoz_byval_")); strings_sb_append(&_qoz_sb_2776_25, fn_name); strings_sb_append(&_qoz_sb_2776_25, QOZ_STR_LIT("(")); _qoz_bv_207 = strings_sb_finish(&_qoz_sb_2776_25);
    }
    emit_push(e, _qoz_bv_207); emit_emit_expr(e, lhs); emit_push(e, QOZ_STR_LIT(", ")); emit_emit_expr(e, rhs); emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } default: { NULL;  break; } } 0; } } qoz_string record_name = emit_record_struct_name_of_ctype(e, lct); if (!qoz_string_eq(record_name, QOZ_STR_LIT(""))) { qoz_BinaryOp* _qoz_ms_235 = op; switch (_qoz_ms_235->tag) { case qoz_BinaryOp_BOpEq: { {
        qoz_string _qoz_bv_208;
    {
        qoz_Strbuf _qoz_sb_2789_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2789_21); strings_sb_append(&_qoz_sb_2789_21, QOZ_STR_LIT("qoz_eq_")); strings_sb_append(&_qoz_sb_2789_21, record_name); strings_sb_append(&_qoz_sb_2789_21, QOZ_STR_LIT("(")); _qoz_bv_208 = strings_sb_finish(&_qoz_sb_2789_21);
    }
    emit_push(e, _qoz_bv_208); emit_emit_expr(e, lhs); emit_push(e, QOZ_STR_LIT(", ")); emit_emit_expr(e, rhs); emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } case qoz_BinaryOp_BOpNe: { {
        qoz_string _qoz_bv_209;
    {
        qoz_Strbuf _qoz_sb_2797_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2797_21); strings_sb_append(&_qoz_sb_2797_21, QOZ_STR_LIT("!qoz_eq_")); strings_sb_append(&_qoz_sb_2797_21, record_name); strings_sb_append(&_qoz_sb_2797_21, QOZ_STR_LIT("(")); _qoz_bv_209 = strings_sb_finish(&_qoz_sb_2797_21);
    }
    emit_push(e, _qoz_bv_209); emit_emit_expr(e, lhs); emit_push(e, QOZ_STR_LIT(", ")); emit_emit_expr(e, rhs); emit_push(e, QOZ_STR_LIT(")")); return;
    }
    0;  break; } default: { NULL;  break; } } 0; } int64_t p = emit_binary_prec(op); bool needs_paren_child = emit_is_logical_op(op); emit_emit_binary_child(e, lhs, p, needs_paren_child); qoz_string bop = emit_binary_c_op(op); qoz_string _qoz_bv_210;
    {
        qoz_Strbuf _qoz_sb_2811_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2811_13); strings_sb_append(&_qoz_sb_2811_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_2811_13, bop); strings_sb_append(&_qoz_sb_2811_13, QOZ_STR_LIT(" ")); _qoz_bv_210 = strings_sb_finish(&_qoz_sb_2811_13);
    }
    emit_push(e, _qoz_bv_210); emit_emit_binary_child(e, rhs, p + 1, needs_paren_child); 
    return;
}

bool emit_is_logical_op(qoz_BinaryOp* op) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&op);
    qoz_BinaryOp* _qoz_ms_236 = op; bool _qoz_mv_236 = false; switch (_qoz_ms_236->tag) { case qoz_BinaryOp_BOpAnd: { _qoz_mv_236 = (true);  break; } case qoz_BinaryOp_BOpOr: { _qoz_mv_236 = (true);  break; } default: { _qoz_mv_236 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_236;
}

void emit_emit_binary_child(qoz_Emitter* e, qoz_Expr* ex, int64_t min_prec, bool force_paren_if_binary) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_237 = ex; switch (_qoz_ms_237->tag) { case qoz_Expr_EBinary: { if (force_paren_if_binary) { emit_push(e, QOZ_STR_LIT("(")); emit_emit_expr(e, ex); emit_push(e, QOZ_STR_LIT(")")); }  else { emit_emit_with_prec(e, ex, min_prec); } 0;  break; } default: { emit_emit_with_prec(e, ex, min_prec);  break; } } 0; 
    return;
}

int64_t emit_binary_prec(qoz_BinaryOp* op) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&op);
    qoz_BinaryOp* _qoz_ms_238 = op; int64_t _qoz_mv_238 = 0; switch (_qoz_ms_238->tag) { case qoz_BinaryOp_BOpMul: { _qoz_mv_238 = (12);  break; } case qoz_BinaryOp_BOpDiv: { _qoz_mv_238 = (12);  break; } case qoz_BinaryOp_BOpMod: { _qoz_mv_238 = (12);  break; } case qoz_BinaryOp_BOpAdd: { _qoz_mv_238 = (11);  break; } case qoz_BinaryOp_BOpSub: { _qoz_mv_238 = (11);  break; } case qoz_BinaryOp_BOpShl: { _qoz_mv_238 = (10);  break; } case qoz_BinaryOp_BOpShr: { _qoz_mv_238 = (10);  break; } case qoz_BinaryOp_BOpLt: { _qoz_mv_238 = (8);  break; } case qoz_BinaryOp_BOpGt: { _qoz_mv_238 = (8);  break; } case qoz_BinaryOp_BOpLe: { _qoz_mv_238 = (8);  break; } case qoz_BinaryOp_BOpGe: { _qoz_mv_238 = (8);  break; } case qoz_BinaryOp_BOpRange: { _qoz_mv_238 = (8);  break; } case qoz_BinaryOp_BOpRangeInclusive: { _qoz_mv_238 = (8);  break; } case qoz_BinaryOp_BOpEq: { _qoz_mv_238 = (7);  break; } case qoz_BinaryOp_BOpNe: { _qoz_mv_238 = (7);  break; } case qoz_BinaryOp_BOpBitAnd: { _qoz_mv_238 = (6);  break; } case qoz_BinaryOp_BOpBitXor: { _qoz_mv_238 = (5);  break; } case qoz_BinaryOp_BOpBitOr: { _qoz_mv_238 = (4);  break; } case qoz_BinaryOp_BOpAnd: { _qoz_mv_238 = (3);  break; } case qoz_BinaryOp_BOpOr: { _qoz_mv_238 = (2);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_238;
}

void emit_emit_with_prec(qoz_Emitter* e, qoz_Expr* ex, int64_t min_prec) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_239 = ex; switch (_qoz_ms_239->tag) { case qoz_Expr_EBinary: { qoz_BinaryOp* op = _qoz_ms_239->payload.EBinary.f1; {
        int64_t p = emit_binary_prec(op); if (p < min_prec) { emit_push(e, QOZ_STR_LIT("(")); emit_emit_expr(e, ex); emit_push(e, QOZ_STR_LIT(")")); }  else { emit_emit_expr(e, ex); } 
    }
    0;  break; } default: { emit_emit_expr(e, ex);  break; } } 0; 
    return;
}

qoz_string emit_binary_c_op(qoz_BinaryOp* op) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&op);
    qoz_BinaryOp* _qoz_ms_240 = op; qoz_string _qoz_mv_240 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_240->tag) { case qoz_BinaryOp_BOpAdd: { _qoz_mv_240 = (QOZ_STR_LIT("+"));  break; } case qoz_BinaryOp_BOpSub: { _qoz_mv_240 = (QOZ_STR_LIT("-"));  break; } case qoz_BinaryOp_BOpMul: { _qoz_mv_240 = (QOZ_STR_LIT("*"));  break; } case qoz_BinaryOp_BOpDiv: { _qoz_mv_240 = (QOZ_STR_LIT("/"));  break; } case qoz_BinaryOp_BOpMod: { _qoz_mv_240 = (QOZ_STR_LIT("%"));  break; } case qoz_BinaryOp_BOpEq: { _qoz_mv_240 = (QOZ_STR_LIT("=="));  break; } case qoz_BinaryOp_BOpNe: { _qoz_mv_240 = (QOZ_STR_LIT("!="));  break; } case qoz_BinaryOp_BOpLt: { _qoz_mv_240 = (QOZ_STR_LIT("<"));  break; } case qoz_BinaryOp_BOpGt: { _qoz_mv_240 = (QOZ_STR_LIT(">"));  break; } case qoz_BinaryOp_BOpLe: { _qoz_mv_240 = (QOZ_STR_LIT("<="));  break; } case qoz_BinaryOp_BOpGe: { _qoz_mv_240 = (QOZ_STR_LIT(">="));  break; } case qoz_BinaryOp_BOpAnd: { _qoz_mv_240 = (QOZ_STR_LIT("&&"));  break; } case qoz_BinaryOp_BOpOr: { _qoz_mv_240 = (QOZ_STR_LIT("||"));  break; } case qoz_BinaryOp_BOpBitAnd: { _qoz_mv_240 = (QOZ_STR_LIT("&"));  break; } case qoz_BinaryOp_BOpBitOr: { _qoz_mv_240 = (QOZ_STR_LIT("|"));  break; } case qoz_BinaryOp_BOpBitXor: { _qoz_mv_240 = (QOZ_STR_LIT("^"));  break; } case qoz_BinaryOp_BOpShl: { _qoz_mv_240 = (QOZ_STR_LIT("<<"));  break; } case qoz_BinaryOp_BOpShr: { _qoz_mv_240 = (QOZ_STR_LIT(">>"));  break; } case qoz_BinaryOp_BOpRange: { _qoz_mv_240 = (QOZ_STR_LIT("<"));  break; } case qoz_BinaryOp_BOpRangeInclusive: { _qoz_mv_240 = (QOZ_STR_LIT("<="));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_240;
}

qoz_string emit_unary_c_op(qoz_UnaryOp* op) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&op);
    qoz_UnaryOp* _qoz_ms_241 = op; qoz_string _qoz_mv_241 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_241->tag) { case qoz_UnaryOp_UOpNeg: { _qoz_mv_241 = (QOZ_STR_LIT("-"));  break; } case qoz_UnaryOp_UOpNot: { _qoz_mv_241 = (QOZ_STR_LIT("!"));  break; } case qoz_UnaryOp_UOpDeref: { _qoz_mv_241 = (QOZ_STR_LIT("*"));  break; } case qoz_UnaryOp_UOpAddr: { _qoz_mv_241 = (QOZ_STR_LIT("&"));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_241;
}

bool emit_is_block(qoz_Expr* ex) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_242 = ex; bool _qoz_mv_242 = false; switch (_qoz_ms_242->tag) { case qoz_Expr_EBlock: { _qoz_mv_242 = (true);  break; } default: { _qoz_mv_242 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_242;
}

bool emit_is_nil_expr(qoz_Expr* ex) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&ex);
    qoz_Expr* _qoz_ms_243 = ex; bool _qoz_mv_243 = false; switch (_qoz_ms_243->tag) { case qoz_Expr_ENil: { _qoz_mv_243 = (true);  break; } default: { _qoz_mv_243 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_243;
}

void emit_emit_fn_body_block(qoz_Emitter* e, qoz_Expr* body, qoz_TypeExpr* ret_hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&body);
    qoz_gc_push_root(&ret_hint);
    qoz_Expr* _qoz_ms_244 = body; switch (_qoz_ms_244->tag) { case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_244->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_244->payload.EBlock.f2; {
        qoz_Vec__qoz_Expr defers = emit_collect_defers_in_stmts(stmts); qoz_Expr* effective_tail = tail; qoz_gc_push_root(&effective_tail); qoz_Expr* _qoz_ms_245 = tail; switch (_qoz_ms_245->tag) { case qoz_Expr_EDefer: { qoz_Expr* dbody = _qoz_ms_245->payload.EDefer.f1; {
        qoz_vec_push__qoz_Expr(&defers, dbody); effective_tail = qoz_make_Expr_ENil(emit_span_of_expr(tail)); 
    }
    0;  break; } default: { NULL;  break; } } 0; { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }if (emit_is_nil_expr(effective_tail)) { emit_emit_defers_reverse(e, defers); emit_push(e, QOZ_STR_LIT("return;\n")); return;} if (emit_is_unit_typeexpr(ret_hint)) { qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, effective_tail); emit_close_statement_scope(e, saved); emit_emit_defers_reverse(e, defers); emit_push(e, QOZ_STR_LIT("\n    return;\n")); return;} qoz_Expr* _qoz_ms_246 = effective_tail; switch (_qoz_ms_246->tag) { case qoz_Expr_EReturn: { {
        emit_emit_expr(e, effective_tail); emit_push(e, QOZ_STR_LIT(";\n")); return;
    }
    0;  break; } default: { NULL;  break; } } 0; if ((defers.len) == 0) { qoz_StmtScope saved = emit_open_statement_scope(e); emit_push(e, e->current_return_restore); emit_push(e, QOZ_STR_LIT("return ")); emit_emit_value_with_hint(e, effective_tail, ret_hint); emit_push(e, QOZ_STR_LIT(";\n")); emit_close_statement_scope(e, saved); }  else { e->closure_counter = e->closure_counter + 1; qoz_string _qoz_bv_211;
    {
        qoz_Strbuf _qoz_sb_2973_48 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2973_48); strings_sb_append_i64(&_qoz_sb_2973_48, e->closure_counter); _qoz_bv_211 = strings_sb_finish(&_qoz_sb_2973_48);
    }
    qoz_string tmp = qoz_string_cat(QOZ_STR_LIT("_qoz_ret_"), _qoz_bv_211); qoz_string rt = emit_c_type_for(e, ret_hint); qoz_string _qoz_bv_212;
    {
        qoz_Strbuf _qoz_sb_2975_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2975_21); strings_sb_append(&_qoz_sb_2975_21, rt); strings_sb_append(&_qoz_sb_2975_21, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_2975_21, tmp); strings_sb_append(&_qoz_sb_2975_21, QOZ_STR_LIT(" = ")); _qoz_bv_212 = strings_sb_finish(&_qoz_sb_2975_21);
    }
    emit_push(e, _qoz_bv_212); emit_emit_value_with_hint(e, effective_tail, ret_hint); emit_push(e, QOZ_STR_LIT(";\n    ")); emit_emit_defers_reverse(e, defers); qoz_string _qoz_bv_213;
    {
        qoz_Strbuf _qoz_sb_2979_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_2979_21); strings_sb_append(&_qoz_sb_2979_21, QOZ_STR_LIT("\n    return ")); strings_sb_append(&_qoz_sb_2979_21, tmp); strings_sb_append(&_qoz_sb_2979_21, QOZ_STR_LIT(";\n")); _qoz_bv_213 = strings_sb_finish(&_qoz_sb_2979_21);
    }
    emit_push(e, _qoz_bv_213); } 
    }
    0;  break; } default: { NULL;  break; } } 0; 
    return;
}

bool emit_is_unit_typeexpr(qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_247 = te; bool _qoz_mv_247 = false; switch (_qoz_ms_247->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_247 = (true);  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_247->payload.TENamed.f1; bool _qoz_bv_214;
    {
        if ((path.len) != 1) { return false;} qoz_string n = path.data[0]; _qoz_bv_214 = qoz_string_eq(n, QOZ_STR_LIT("unit")) || qoz_string_eq(n, QOZ_STR_LIT("void"));
    }
    _qoz_mv_247 = (_qoz_bv_214);  break; } default: { _qoz_mv_247 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_247;
}

void emit_emit_fn(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_FnParam params, qoz_TypeExpr* ret, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ret);
    qoz_gc_push_root(&body);
    qoz_string ret_c = emit_c_type_for(e, ret); qoz_string _qoz_bv_215;
    {
        qoz_Strbuf _qoz_sb_3000_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3000_13); strings_sb_append(&_qoz_sb_3000_13, ret_c); strings_sb_append(&_qoz_sb_3000_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_3000_13, name); _qoz_bv_215 = strings_sb_finish(&_qoz_sb_3000_13);
    }
    emit_push(e, _qoz_bv_215); if ((params.len) == 0) { emit_push(e, QOZ_STR_LIT("(void)")); }  else { emit_push(e, QOZ_STR_LIT("(")); int64_t i = 0; { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam p = __col.data[__i]; (void)p; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } qoz_string pc = emit_c_type_for(e, p.ty); qoz_string pn = p.name; qoz_string _qoz_bv_216;
    {
        qoz_Strbuf _qoz_sb_3010_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3010_21); strings_sb_append(&_qoz_sb_3010_21, pc); strings_sb_append(&_qoz_sb_3010_21, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_3010_21, pn); _qoz_bv_216 = strings_sb_finish(&_qoz_sb_3010_21);
    }
    emit_push(e, _qoz_bv_216); i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); } emit_push(e, QOZ_STR_LIT(" {\n    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();\n    ")); qoz_string saved_ret = e->current_return_restore; e->current_return_restore = QOZ_STR_LIT("qoz_gc_shadow_set_top(_qoz_shadow_guard); "); e->locals = qoz_map_make__qoz_string__qoz_TypeExpr(); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam p = __col.data[__i]; (void)p; qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, p.name, p.ty); if (emit_c_type_is_pointer(emit_c_type_for(e, p.ty))) { qoz_string pn = p.name; qoz_string _qoz_bv_217;
    {
        qoz_Strbuf _qoz_sb_3023_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3023_21); strings_sb_append(&_qoz_sb_3023_21, QOZ_STR_LIT("qoz_gc_push_root(&")); strings_sb_append(&_qoz_sb_3023_21, pn); strings_sb_append(&_qoz_sb_3023_21, QOZ_STR_LIT(");\n    ")); _qoz_bv_217 = strings_sb_finish(&_qoz_sb_3023_21);
    }
    emit_push(e, _qoz_bv_217); } } }e->current_ret_te = ret; bool is_b = emit_is_block(body); if (is_b) { emit_emit_fn_body_block(e, body, ret); }  else { qoz_StmtScope saved = emit_open_statement_scope(e); qoz_string crr = e->current_return_restore; if (emit_is_unit_typeexpr(ret)) { emit_emit_expr(e, body); qoz_string _qoz_bv_218;
    {
        qoz_Strbuf _qoz_sb_3035_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3035_21); strings_sb_append(&_qoz_sb_3035_21, QOZ_STR_LIT(";\n    ")); strings_sb_append(&_qoz_sb_3035_21, crr); strings_sb_append(&_qoz_sb_3035_21, QOZ_STR_LIT("return;\n")); _qoz_bv_218 = strings_sb_finish(&_qoz_sb_3035_21);
    }
    emit_push(e, _qoz_bv_218); }  else { qoz_string _qoz_bv_219;
    {
        qoz_Strbuf _qoz_sb_3037_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3037_21); strings_sb_append(&_qoz_sb_3037_21, crr); strings_sb_append(&_qoz_sb_3037_21, QOZ_STR_LIT("return ")); _qoz_bv_219 = strings_sb_finish(&_qoz_sb_3037_21);
    }
    emit_push(e, _qoz_bv_219); emit_emit_value_with_hint(e, body, ret); emit_push(e, QOZ_STR_LIT(";\n")); } emit_close_statement_scope(e, saved); } e->current_return_restore = saved_ret; e->locals = qoz_map_make__qoz_string__qoz_TypeExpr(); emit_push(e, QOZ_STR_LIT("}\n\n")); 
    return;
}

bool emit_c_type_is_pointer(qoz_string ct) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if ((ct).len == 0) { return false;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_byte_at(ct, (ct).len - 1) == 42;
}

void emit_emit_records_topological(qoz_Emitter* e, qoz_File f) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_Vec__qoz_string names = qoz_vec_make__qoz_string(); qoz_Vec__qoz_Vec__qoz_StructField field_lists = qoz_vec_make__qoz_Vec__qoz_StructField(); { qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_248 = d; switch (_qoz_ms_248->tag) { case qoz_Decl_DStruct: { qoz_string name = _qoz_ms_248->payload.DStruct.f1; qoz_Vec__qoz_string params = _qoz_ms_248->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_248->payload.DStruct.f3; if ((params.len) == 0) { qoz_vec_push__qoz_string(&names, name); qoz_vec_push__qoz_Vec__qoz_StructField(&field_lists, fields); } 0;  break; } default: { NULL;  break; } } 0; } }{ qoz_Vec__qoz_Instantiation __col = e->record_insts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Instantiation inst = __col.data[__i]; (void)inst; qoz_Option__qoz_Decl* _qoz_ms_249 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_decls, inst.name); switch (_qoz_ms_249->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_249->payload.Some.f0; qoz_Decl* _qoz_ms_250 = decl; switch (_qoz_ms_250->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_250->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_250->payload.DStruct.f3; {
        qoz_Vec__qoz_StructField subst_fields = qoz_vec_make__qoz_StructField(); { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField fld = __col.data[__i]; (void)fld; qoz_vec_push__qoz_StructField(&subst_fields, ((qoz_StructField){ .name = fld.name, .ty = emit_substitute_type(e, fld.ty, params, inst.args) })); } }qoz_vec_push__qoz_string(&names, inst.mangled); qoz_vec_push__qoz_Vec__qoz_StructField(&field_lists, subst_fields); 
    }
    0;  break; } default: { NULL;  break; } } 0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; } }qoz_Map__qoz_string__bool visited = qoz_map_make__qoz_string__bool(); int64_t i = 0; while (i < (names.len)) { emit_emit_record_visit(e, names.data[i], names, field_lists, &visited); i = i + 1; } 
    return;
}

void emit_emit_record_visit(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_string names, qoz_Vec__qoz_Vec__qoz_StructField field_lists, qoz_Map__qoz_string__bool* visited) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&visited);
    if (qoz_map_contains__qoz_string__bool(visited, name)) { return;} qoz_map_set__qoz_string__bool(visited, name, true); int64_t i = 0; while (i < (names.len)) { if (qoz_string_eq(names.data[i], name)) { qoz_Vec__qoz_StructField fields = field_lists.data[i]; { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField fld = __col.data[__i]; (void)fld; qoz_Option__qoz_string* _qoz_ms_251 = emit_value_field_dep(e, fld.ty); switch (_qoz_ms_251->tag) { case qoz_Option__qoz_string_Some: { qoz_string dep = _qoz_ms_251->payload.Some.f0; emit_emit_record_visit(e, dep, names, field_lists, visited);  break; } case qoz_Option__qoz_string_None: { NULL;  break; } } 0; } }emit_emit_struct(e, name, fields); return;} i = i + 1; } 
    return;
}

qoz_Option__qoz_string* emit_value_field_dep(qoz_Emitter* e, qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_252 = te; qoz_Option__qoz_string* _qoz_mv_252 = NULL; switch (_qoz_ms_252->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_252->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_252->payload.TENamed.f2; qoz_Option__qoz_string* _qoz_bv_220;
    {
        if ((path.len) < 1) { return qoz_make_Option__qoz_string_None();} qoz_string n = path.data[(path.len) - 1]; if (!qoz_string_eq(emit_primitive_c_name(n), QOZ_STR_LIT(""))) { return qoz_make_Option__qoz_string_None();} if (qoz_map_contains__qoz_string__bool(&e->is_enum, n)) { return qoz_make_Option__qoz_string_None();} if ((args.len) > 0) { return qoz_make_Option__qoz_string_Some(emit_mangle_inst(e, n, args));} _qoz_bv_220 = qoz_make_Option__qoz_string_Some(n);
    }
    _qoz_mv_252 = (_qoz_bv_220);  break; } default: { _qoz_mv_252 = (qoz_make_Option__qoz_string_None());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_252;
}

bool emit_is_main(qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_string_eq(name, QOZ_STR_LIT("main"));
}

qoz_string emit_strip_runtime_includes(qoz_string src) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_string out = src; out = strings_replace_all(out, QOZ_STR_LIT("#include \"gc.h\"\n"), QOZ_STR_LIT("")); out = strings_replace_all(out, QOZ_STR_LIT("#include \"qoz_runtime.h\"\n"), QOZ_STR_LIT("")); qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

qoz_string emit_emit_program(qoz_File f, qoz_Map__int64_t__qoz_TypeExpr expr_types) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Emitter e = emit_make_emitter(); e.expr_types = expr_types; strings_sb_init(&e.out); emit_register_variants(&e, f); emit_register_generics(&e, f); emit_collect_type_instantiations(&e, f); emit_push(&e, QOZ_STR_LIT("/* embedded runtime: gc.h */\n")); emit_push(&e, emit_strip_runtime_includes(QOZ_STR_LIT("/* qoz_gc: precise mark/sweep collector with type descriptors.\n *\n * Each managed allocation is paired with a `qoz_type_desc *` that the\n * compiler emits when it lays out the type. The descriptor lists\n * pointer-field byte offsets inside the object so the collector can find\n * every outgoing reference without scanning conservatively.\n *\n * Roots come from a shadow stack the compiler maintains: at every\n * function entry pointer-typed locals are registered, at exit they are\n * popped via the __cleanup__ attribute.\n *\n * Auto-collection triggers from qoz_gc_alloc once heap byte usage\n * crosses a growth threshold. Shutdown frees everything regardless.\n */\n#ifndef QOZ_GC_H\n#define QOZ_GC_H\n\n#include <stdint.h>\n#include <stdbool.h>\n#include <stddef.h>\n\ntypedef enum {\n    QOZ_DESC_LEAF        = 0,  /* no outgoing pointers */\n    QOZ_DESC_OFFSETS     = 1,  /* offsets[] lists pointer-field byte offsets */\n    QOZ_DESC_ADT         = 2,  /* tag-dispatched: each variant has its own offset list */\n    QOZ_DESC_CONSERVATIVE = 3, /* fall back to scanning every word (avoid when possible) */\n} qoz_desc_kind;\n\ntypedef struct qoz_variant_desc {\n    int32_t        tag;        /* matches the ADT's runtime discriminant */\n    int32_t        nptrs;\n    const int32_t *offsets;    /* byte offsets within the variant's payload area */\n} qoz_variant_desc;\n\ntypedef struct qoz_type_desc {\n    qoz_desc_kind             kind;\n    int32_t                   size;     /* total object size in bytes */\n    int32_t                   nptrs;    /* meaningful when kind == OFFSETS */\n    const int32_t            *offsets;  /* meaningful when kind == OFFSETS */\n    int32_t                   tag_off;  /* offset of the tag field; meaningful when kind == ADT */\n    int32_t                   payload_off; /* offset of the payload union; meaningful when kind == ADT */\n    int32_t                   nvariants;\n    const qoz_variant_desc   *variants;\n    const char               *name;     /* debug only */\n} qoz_type_desc;\n\n/* Allocate `size` bytes; record `desc` against the allocation so future\n * mark walks can find outgoing pointers. `desc` may be NULL, in which\n * case the allocation is treated as opaque (conservative scan fallback).\n */\nvoid *qoz_gc_alloc(int64_t size, const qoz_type_desc *desc);\n\n/* Look up the descriptor associated with an allocation. Returns NULL\n * when the pointer is not a known managed allocation or no descriptor\n * was registered. */\nconst qoz_type_desc *qoz_gc_desc_of(const void *ptr);\n\n/* Shadow-stack root operations. The compiler emits push at every fn\n * entry for each pointer-typed local and pop at exit. */\nvoid qoz_gc_push_root(void *root);\nvoid qoz_gc_pop_roots(int64_t n);\n\n/* Snapshot / restore the shadow-stack top. emit_fn declares a scoped\n * variable holding the entry snapshot and registers `qoz_gc_restore_shadow`\n * as a __cleanup__ attribute so every exit path (including early\n * returns) restores automatically. */\nint64_t qoz_gc_shadow_top(void);\nvoid    qoz_gc_shadow_set_top(int64_t top);\nvoid    qoz_gc_restore_shadow(int64_t *base);\n\n/* Walk all shadow-stack roots and invoke the callback for each non-NULL\n * pointer found at `*root_slot`. */\nvoid qoz_gc_walk_shadow_roots(void (*cb)(void *arg, void *child),\n                              void *arg);\n\n/* Run a precise mark phase walking the shadow stack. Each reachable\n * allocation gets its mark bit set; everything else is left unmarked.\n * Returns the number of allocations marked. */\nint64_t qoz_gc_mark_phase(void);\n\n/* Free every unmarked allocation. Returns the number of allocations\n * freed. Call after qoz_gc_mark_phase. */\nint64_t qoz_gc_sweep_phase(void);\n\n/* Mark + sweep in one call. Returns the number of allocations freed. */\nint64_t qoz_gc_run(void);\n\n/* Record the bottom of the C stack (called once from qoz_init with the\n * address of a local in main). The conservative supplement scans from\n * the current top up to this anchor at every collection. */\nvoid qoz_gc_set_stack_bottom(void *anchor);\n\n/* Free all remaining allocations (process-shutdown hook). */\nvoid qoz_gc_shutdown(void);\n\n/* Allocation byte-size as recorded at qoz_gc_alloc time. Returns 0\n * for pointers not registered with the GC. */\nint64_t qoz_gc_alloc_size(const void *ptr);\n\n/* Explicitly free a single allocation (used by qoz_realloc to retire\n * the old buffer). Safe on already-freed or unknown pointers. */\nvoid qoz_gc_free(void *ptr);\n\n/* Diagnostic: total number of managed allocations currently registered\n * with the precise GC (used by the self-test). */\nint64_t qoz_gc_total_allocations(void);\n\n/* Diagnostic: reset all mark bits to 0. Called by the test harness\n * before counting reachability. */\nvoid qoz_gc_clear_marks(void);\n\n/* True when the allocation is currently marked. */\nbool qoz_gc_is_marked(const void *ptr);\n\n/* Walk the outgoing pointers of `ptr` using its registered descriptor,\n * invoking `cb(arg, child)` for each non-NULL pointer field. Returns\n * 1 when the scan happened (caller should skip conservative fallback),\n * 0 when no descriptor was registered. */\nint qoz_gc_scan_object_callback(void *ptr,\n                                void (*cb)(void *arg, void *child),\n                                void *arg);\n\n#endif\n"))); emit_push(&e, QOZ_STR_LIT("\n/* embedded runtime: qoz_runtime.h */\n")); emit_push(&e, emit_strip_runtime_includes(QOZ_STR_LIT("#ifndef QOZ_RUNTIME_H\n#define QOZ_RUNTIME_H\n\n#include <stdint.h>\n#include <stdbool.h>\n#include <stddef.h>\n#include <math.h>\n#include \"gc.h\"\n\ntypedef struct {\n    const char *data;\n    int64_t len;\n    /* Allocation-start pointer that keeps the data buffer reachable\n     * through tgc's exact-match scan. Equal to data for fresh allocations,\n     * inherited from the source for slices, NULL for static literals. */\n    const char *root;\n} qoz_string;\n\nvoid qoz_init(int *stack_anchor);\nvoid qoz_shutdown(void);\n\n/* Allocation */\nvoid *qoz_alloc(int64_t size);\nvoid *qoz_calloc(int64_t size);\nvoid *qoz_realloc(void *ptr, int64_t size);\n\n/* String helpers */\nbool     qoz_string_eq(qoz_string a, qoz_string b);\nuint64_t qoz_string_hash(qoz_string s);\nbool     qoz_string_has_prefix(qoz_string s, qoz_string p);\nbool     qoz_string_has_suffix(qoz_string s, qoz_string suf);\nint64_t  qoz_string_index_byte(qoz_string s, int64_t byte);\nint64_t  qoz_string_byte_at(qoz_string s, int64_t i);\nqoz_string qoz_string_cat(qoz_string a, qoz_string b);\nqoz_string qoz_string_slice(qoz_string s, int64_t from, int64_t to);\nint64_t  qoz_string_parse_int(qoz_string s);\n\n/* OS bridge */\nvoid       qoz_set_argv(int argc, char **argv);\nint64_t    qoz_os_argc(void);\nqoz_string qoz_os_arg(int64_t i);\nvoid       qoz_os_exit(int64_t code);\nqoz_string qoz_os_getenv(qoz_string name);\n\n/* Unrecoverable abort. Prints a one-line diagnostic to stderr (the\n * message, the source location if the call site provided one, and a\n * fixed prefix) and calls abort(). Intended for programmer bugs:\n * invariants that \"cannot\" fail, exhausted match arms reaching the\n * default, integer overflow on counter limits, etc. Expected errors\n * flow through Result<T, E> with the `try` operator; panic is not\n * recoverable. */\n_Noreturn void qoz_panic(qoz_string msg);\n\n/* File system */\nqoz_string qoz_fs_read_file(qoz_string path);\nbool       qoz_fs_write_file(qoz_string path, qoz_string content);\nbool       qoz_fs_file_exists(qoz_string path);\n/* List every regular file in `dir` whose name ends in \".qoz\", sorted\n * by filename. Returns a single string with the filenames separated by\n * newlines (no trailing newline). Returns an empty string if the\n * directory cannot be opened. */\nqoz_string qoz_fs_list_qoz_files(qoz_string dir);\n\n/* Opaque-pointer access to a string's internal data pointer. Used by\n * Qoz code that needs to memcpy string bytes into a raw buffer (e.g.\n * std/strings/Strbuf::sb_append). */\nvoid      *qoz_string_data(qoz_string s);\n\n/* Construct a string that aliases the bytes at `buf` for `n` bytes.\n * The `root` field of the returned qoz_string is `buf`, so the GC\n * keeps the allocation reachable as long as the string is reachable.\n * `buf` must point at a GC-managed allocation. */\nqoz_string qoz_string_alias(void *buf, int64_t n);\n\n/* Copy `n` bytes from `src` to `dst`. Wrapper over libc memcpy so Qoz\n * can call it portably; on darwin memcpy is a fortify-source macro and\n * cannot be extern-declared by Qoz directly. */\nvoid       qoz_bytes_copy(void *dst, void *src, int64_t n);\n\n/* Print primitives. The generated code emits a sequence of these per\n * `fmt.println(args...)` call: one print per argument, separators in between,\n * and a newline at the end.\n */\nvoid qoz_print_str(qoz_string s);\nvoid qoz_print_cstr(const char *s);\nvoid qoz_print_i64(int64_t v);\nvoid qoz_print_i32(int32_t v);\nvoid qoz_print_f64(double v);\nvoid qoz_print_bool(bool v);\nvoid qoz_print_sep(void);\nvoid qoz_print_nl(void);\n/* Print a string followed by a newline and flush stdout. Backs the\n * `fmt.println` stdlib function. */\nvoid qoz_print_line(qoz_string s);\n\n/* Format builder */\ntypedef struct { char *buf; int64_t len; int64_t cap; } qoz_strbuf;\nvoid       qoz_strbuf_init(void *b);\nvoid       qoz_strbuf_append_str(void *b, qoz_string s);\nvoid       qoz_strbuf_append_cstr(void *b, const char *s);\nvoid       qoz_strbuf_append_i64(void *b, int64_t v);\nvoid       qoz_strbuf_append_f64(void *b, double v);\nvoid       qoz_strbuf_append_bool(void *b, bool v);\nqoz_string qoz_strbuf_finish(void *b);\n\n#define QOZ_STR_LIT(s) ((qoz_string){ (s), (int64_t)(sizeof(s) - 1) })\n\n/* Fork+exec a child, capture its stdout and stderr into Qoz strings,\n * and report the child's exit status. `argv` points to an array of `n`\n * qoz_string values; the first is the program to launch, the rest are\n * its arguments. The output pointers must point to valid storage; on\n * return they hold the captured streams. Exit code is the process's\n * raw exit value, or -1 if the spawn itself failed. */\nvoid qoz_process_exec(qoz_string *argv, int64_t n,\n                      int64_t *out_exit,\n                      qoz_string *out_stdout,\n                      qoz_string *out_stderr);\n\n#endif\n"))); emit_push(&e, QOZ_STR_LIT("\n/* embedded runtime: gc.c */\n")); emit_push(&e, emit_strip_runtime_includes(QOZ_STR_LIT("/* qoz_gc: precise mark/sweep with type descriptors.\n *\n * Owns the heap directly. Each call to qoz_gc_alloc returns malloc'd\n * memory and registers the allocation in a side table that records the\n * pointer, its TypeDesc, and a mark bit. Roots come from a shadow stack\n * the compiler emits at every function entry.\n *\n * Auto-collection triggers from qoz_gc_alloc once heap byte usage\n * crosses a growth threshold. Shutdown frees everything regardless.\n */\n\n#include \"gc.h\"\n\n#include <stdint.h>\n#include <stdlib.h>\n#include <stdbool.h>\n#include <setjmp.h>\n#include <pthread.h>\n\n/* state: 0 = empty (probe stops); 1 = live; 2 = tombstone (probe continues) */\ntypedef struct {\n    void                       *ptr;\n    const qoz_type_desc        *desc;\n    int64_t                     size;\n    uint8_t                     mark;\n    uint8_t                     state;\n} qoz_gc_slot;\n\ntypedef struct {\n    qoz_gc_slot *slots;\n    int64_t      nslots;\n    int64_t      nlive;\n    int64_t      ntomb;\n} qoz_gc_table;\n\nstatic qoz_gc_table g_table  = { NULL, 0, 0, 0 };\n\n/* Heap-usage tracking. qoz_gc_run triggers automatically from\n * qoz_gc_alloc once g_bytes_live crosses g_bytes_threshold. After each\n * sweep, the threshold is reset to max(initial, 2 * live_after_sweep)\n * so steady-state working sets pay one collection per doubling. */\n#define QOZ_GC_INITIAL_THRESHOLD (1 << 20)   /* 1 MiB */\nstatic int64_t g_bytes_live      = 0;\nstatic int64_t g_bytes_threshold = QOZ_GC_INITIAL_THRESHOLD;\n\n/* Conservative C-stack scan supplements the shadow stack. Return values\n * from function calls live in registers between the callee return and\n * the caller's qoz_gc_push_root, where the precise scan would miss\n * them. At collection time, setjmp spills callee-saved registers into\n * a jmp_buf on the stack, and the scan walks from the current stack\n * pointer up to the top of the thread's stack region (queried via\n * pthread). The user-supplied anchor in qoz_init is recorded too but\n * the pthread bound is preferred because it covers frames above main\n * (dyld, _start, libc init) that may transit managed pointers during\n * argv setup. */\nstatic void *g_stack_bottom = NULL;\nstatic void *g_stack_top_bound = NULL;\n\n#define ROOT_STACK_INIT 4096\nstatic void   **g_roots     = NULL;\nstatic int64_t  g_roots_top = 0;\nstatic int64_t  g_roots_cap = 0;\n\nstatic uint64_t hash_ptr(const void *p) {\n    uint64_t x = (uint64_t)(uintptr_t)p;\n    x ^= x >> 30; x *= 0xbf58476d1ce4e5b9ULL;\n    x ^= x >> 27; x *= 0x94d049bb133111ebULL;\n    x ^= x >> 31;\n    return x;\n}\n\nstatic void table_init_if_needed(void) {\n    if (g_table.slots) return;\n    g_table.nslots = 1024;\n    g_table.slots  = (qoz_gc_slot *)calloc((size_t)g_table.nslots, sizeof(qoz_gc_slot));\n    g_table.nlive  = 0;\n}\n\nstatic qoz_gc_slot *find_slot(const void *ptr, int insert) {\n    table_init_if_needed();\n    int64_t mask = g_table.nslots - 1;\n    int64_t i = (int64_t)(hash_ptr(ptr) & (uint64_t)mask);\n    qoz_gc_slot *first_tomb = NULL;\n    for (int64_t step = 0; step < g_table.nslots; step++) {\n        qoz_gc_slot *s = &g_table.slots[(i + step) & mask];\n        if (s->state == 0) {\n            if (insert) return first_tomb ? first_tomb : s;\n            return NULL;\n        }\n        if (s->state == 2) {\n            if (insert && !first_tomb) first_tomb = s;\n            continue;\n        }\n        if (s->ptr == ptr) return s;\n    }\n    return insert ? first_tomb : NULL;\n}\n\nstatic void table_grow(void) {\n    int64_t      old_n     = g_table.nslots;\n    qoz_gc_slot *old_slots = g_table.slots;\n    g_table.nslots = old_n * 2;\n    g_table.slots  = (qoz_gc_slot *)calloc((size_t)g_table.nslots, sizeof(qoz_gc_slot));\n    g_table.nlive  = 0;\n    g_table.ntomb  = 0;\n    for (int64_t i = 0; i < old_n; i++) {\n        if (old_slots[i].state == 1) {\n            qoz_gc_slot *s = find_slot(old_slots[i].ptr, 1);\n            if (s) {\n                *s = old_slots[i];\n                g_table.nlive++;\n            }\n        }\n    }\n    free(old_slots);\n}\n\nvoid *qoz_gc_alloc(int64_t size, const qoz_type_desc *desc) {\n    if (g_bytes_live >= g_bytes_threshold) {\n        qoz_gc_run();\n    }\n    void *p = calloc(1, (size_t)size);\n    if (!p) return NULL;\n    table_init_if_needed();\n    if ((g_table.nlive + g_table.ntomb) * 2 >= g_table.nslots) table_grow();\n    qoz_gc_slot *s = find_slot(p, 1);\n    if (s) {\n        uint8_t prev = s->state;\n        s->ptr   = p;\n        s->desc  = desc;\n        s->size  = size;\n        s->mark  = 0;\n        s->state = 1;\n        if (prev == 2) g_table.ntomb--;\n        g_table.nlive++;\n        g_bytes_live += size;\n    }\n    return p;\n}\n\nconst qoz_type_desc *qoz_gc_desc_of(const void *ptr) {\n    if (!ptr) return NULL;\n    qoz_gc_slot *s = find_slot(ptr, 0);\n    return (s && s->state == 1) ? s->desc : NULL;\n}\n\nvoid qoz_gc_push_root(void *root) {\n    if (g_roots_top == g_roots_cap) {\n        int64_t new_cap = g_roots_cap ? g_roots_cap * 2 : ROOT_STACK_INIT;\n        g_roots = (void **)realloc(g_roots, (size_t)new_cap * sizeof(void *));\n        g_roots_cap = new_cap;\n    }\n    g_roots[g_roots_top++] = root;\n}\n\nvoid qoz_gc_pop_roots(int64_t n) {\n    g_roots_top -= n;\n    if (g_roots_top < 0) g_roots_top = 0;\n}\n\nint64_t qoz_gc_shadow_top(void) { return g_roots_top; }\n\nvoid qoz_gc_shadow_set_top(int64_t top) {\n    if (top < 0) top = 0;\n    if (top > g_roots_top) return;\n    g_roots_top = top;\n}\n\nvoid qoz_gc_restore_shadow(int64_t *base) {\n    qoz_gc_shadow_set_top(*base);\n}\n\nvoid qoz_gc_walk_shadow_roots(void (*cb)(void *arg, void *child), void *arg) {\n    for (int64_t i = 0; i < g_roots_top; i++) {\n        void *slot = g_roots[i];\n        if (!slot) continue;\n        void *p = *((void **)slot);\n        if (p) cb(arg, p);\n    }\n}\n\nvoid qoz_gc_clear_marks(void) {\n    table_init_if_needed();\n    for (int64_t i = 0; i < g_table.nslots; i++) g_table.slots[i].mark = 0;\n}\n\nbool qoz_gc_is_marked(const void *ptr) {\n    qoz_gc_slot *s = find_slot(ptr, 0);\n    return (s && s->state == 1) ? (s->mark != 0) : false;\n}\n\nint64_t qoz_gc_total_allocations(void) { return g_table.nlive; }\n\nint qoz_gc_scan_object_callback(void *ptr,\n                                void (*cb)(void *arg, void *child),\n                                void *arg) {\n    qoz_gc_slot *s = find_slot(ptr, 0);\n    if (!s || s->state != 1 || !s->desc) return 0;\n    const qoz_type_desc *desc = s->desc;\n    switch (desc->kind) {\n    case QOZ_DESC_LEAF:\n        return 1;\n    case QOZ_DESC_OFFSETS:\n        for (int32_t i = 0; i < desc->nptrs; i++) {\n            void *child = *((void **)((char *)ptr + desc->offsets[i]));\n            if (child) cb(arg, child);\n        }\n        return 1;\n    case QOZ_DESC_ADT: {\n        int32_t tag = *((int32_t *)((char *)ptr + desc->tag_off));\n        for (int32_t v = 0; v < desc->nvariants; v++) {\n            if (desc->variants[v].tag == tag) {\n                void *payload = (char *)ptr + desc->payload_off;\n                for (int32_t i = 0; i < desc->variants[v].nptrs; i++) {\n                    void *child = *((void **)((char *)payload + desc->variants[v].offsets[i]));\n                    if (child) cb(arg, child);\n                }\n                break;\n            }\n        }\n        return 1;\n    }\n    case QOZ_DESC_CONSERVATIVE:\n    default:\n        return 0;\n    }\n}\n\ntypedef struct {\n    void  **items;\n    int64_t top;\n    int64_t cap;\n} qoz_gc_mark_stack;\n\nstatic void mark_stack_push(qoz_gc_mark_stack *ms, void *p) {\n    if (ms->top == ms->cap) {\n        int64_t new_cap = ms->cap ? ms->cap * 2 : 1024;\n        ms->items = (void **)realloc(ms->items, (size_t)new_cap * sizeof(void *));\n        ms->cap = new_cap;\n    }\n    ms->items[ms->top++] = p;\n}\n\nstatic void mark_callback(void *arg, void *child) {\n    mark_stack_push((qoz_gc_mark_stack *)arg, child);\n}\n\nstatic void scan_conservative_range(qoz_gc_mark_stack *ms, void *lo, void *hi) {\n    if (!lo || !hi) return;\n    if (lo > hi) { void *t = lo; lo = hi; hi = t; }\n    /* Align lo upward to pointer size. */\n    uintptr_t alo = ((uintptr_t)lo + sizeof(void *) - 1) & ~(uintptr_t)(sizeof(void *) - 1);\n    void **p = (void **)alo;\n    void **e = (void **)hi;\n    for (; p < e; p++) {\n        void *cand = *p;\n        if (!cand) continue;\n        qoz_gc_slot *s = find_slot(cand, 0);\n        if (s && s->state == 1) mark_stack_push(ms, cand);\n    }\n}\n\nint64_t qoz_gc_mark_phase(void) {\n    qoz_gc_clear_marks();\n    qoz_gc_mark_stack ms = { NULL, 0, 0 };\n    qoz_gc_walk_shadow_roots(mark_callback, &ms);\n    /* Supplement with a conservative scan of the C stack so register-\n     * resident return values and call temporaries are not missed. */\n    {\n        jmp_buf jb;\n        (void)setjmp(jb);   /* spills callee-saved registers to jb */\n        void *stack_top = (void *)&jb;\n        void *upper = g_stack_top_bound ? g_stack_top_bound : g_stack_bottom;\n        if (upper) scan_conservative_range(&ms, stack_top, upper);\n    }\n\n    int64_t marked = 0;\n    while (ms.top > 0) {\n        void *obj = ms.items[--ms.top];\n        qoz_gc_slot *s = find_slot(obj, 0);\n        if (!s || s->state != 1 || s->mark) continue;\n        s->mark = 1;\n        marked++;\n        if (s->desc && s->desc->kind == QOZ_DESC_LEAF) continue;\n        if (!qoz_gc_scan_object_callback(obj, mark_callback, &ms)) {\n            /* No descriptor: conservative scan of the allocation. */\n            int64_t nwords = s->size / (int64_t)sizeof(void *);\n            void **words = (void **)obj;\n            for (int64_t k = 0; k < nwords; k++) {\n                if (words[k]) mark_stack_push(&ms, words[k]);\n            }\n        }\n    }\n    free(ms.items);\n    return marked;\n}\n\nint64_t qoz_gc_sweep_phase(void) {\n    int64_t freed = 0;\n    table_init_if_needed();\n    for (int64_t i = 0; i < g_table.nslots; i++) {\n        qoz_gc_slot *s = &g_table.slots[i];\n        if (s->state == 1 && !s->mark) {\n            g_bytes_live -= s->size;\n            free(s->ptr);\n            s->ptr   = NULL;\n            s->desc  = NULL;\n            s->size  = 0;\n            s->state = 2;\n            g_table.nlive--;\n            g_table.ntomb++;\n            freed++;\n        }\n    }\n    return freed;\n}\n\nint64_t qoz_gc_run(void) {\n    qoz_gc_mark_phase();\n    int64_t freed = qoz_gc_sweep_phase();\n    int64_t doubled = g_bytes_live * 2;\n    g_bytes_threshold = doubled > QOZ_GC_INITIAL_THRESHOLD\n                        ? doubled : QOZ_GC_INITIAL_THRESHOLD;\n    return freed;\n}\n\nvoid qoz_gc_set_stack_bottom(void *anchor) {\n    g_stack_bottom = anchor;\n    pthread_t self = pthread_self();\n    void *addr = pthread_get_stackaddr_np(self);\n    size_t sz   = pthread_get_stacksize_np(self);\n    /* pthread_get_stackaddr_np returns the address one past the high end\n     * of the stack on darwin; subtract one word to land inside. */\n    (void)sz;\n    if (addr) g_stack_top_bound = (char *)addr - sizeof(void *);\n}\n\nint64_t qoz_gc_alloc_size(const void *ptr) {\n    if (!ptr) return 0;\n    qoz_gc_slot *s = find_slot(ptr, 0);\n    return (s && s->state == 1) ? s->size : 0;\n}\n\nvoid qoz_gc_free(void *ptr) {\n    if (!ptr) return;\n    qoz_gc_slot *s = find_slot(ptr, 0);\n    if (!s || s->state != 1) return;\n    g_bytes_live -= s->size;\n    free(s->ptr);\n    s->ptr   = NULL;\n    s->desc  = NULL;\n    s->size  = 0;\n    s->mark  = 0;\n    s->state = 2;\n    g_table.nlive--;\n    g_table.ntomb++;\n}\n\n/* Free everything (called at process shutdown). */\nvoid qoz_gc_shutdown(void) {\n    if (!g_table.slots) return;\n    for (int64_t i = 0; i < g_table.nslots; i++) {\n        if (g_table.slots[i].state == 1) {\n            free(g_table.slots[i].ptr);\n            g_table.slots[i].ptr   = NULL;\n            g_table.slots[i].state = 0;\n        }\n    }\n    free(g_table.slots);\n    g_table.slots  = NULL;\n    g_table.nslots = 0;\n    g_table.nlive  = 0;\n    g_table.ntomb  = 0;\n    g_bytes_live   = 0;\n    free(g_roots);\n    g_roots     = NULL;\n    g_roots_top = 0;\n    g_roots_cap = 0;\n}\n"))); emit_push(&e, QOZ_STR_LIT("\n/* embedded runtime: qoz_runtime.c */\n")); emit_push(&e, emit_strip_runtime_includes(QOZ_STR_LIT("#include \"qoz_runtime.h\"\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <inttypes.h>\n#include <dirent.h>\n#include <sys/types.h>\n\nstatic int qoz_argc_val = 0;\nstatic char **qoz_argv_val = NULL;\n\nvoid qoz_set_argv(int argc, char **argv) {\n    qoz_argc_val = argc;\n    qoz_argv_val = argv;\n}\n\nint64_t qoz_os_argc(void) { return (int64_t)qoz_argc_val; }\n\nqoz_string qoz_os_arg(int64_t i) {\n    if (i < 0 || i >= (int64_t)qoz_argc_val) return (qoz_string){ NULL, 0 };\n    const char *s = qoz_argv_val[i];\n    return (qoz_string){ s, (int64_t)strlen(s) };\n}\n\nvoid qoz_os_exit(int64_t code) { exit((int)code); }\n\nvoid qoz_panic(qoz_string msg) {\n    fputs(\"qoz: panic: \", stderr);\n    if (msg.len > 0) fwrite(msg.data, 1, (size_t)msg.len, stderr);\n    fputc('\\n', stderr);\n    fflush(stderr);\n    abort();\n}\n\nqoz_string qoz_os_getenv(qoz_string name) {\n    char buf[1024];\n    if (name.len < 0 || (size_t)name.len >= sizeof(buf)) return (qoz_string){ NULL, 0 };\n    memcpy(buf, name.data, (size_t)name.len);\n    buf[name.len] = 0;\n    const char *v = getenv(buf);\n    if (!v) return (qoz_string){ NULL, 0 };\n    return (qoz_string){ v, (int64_t)strlen(v) };\n}\n\nstatic int qoz_copy_path_nul(qoz_string path, char *buf, size_t buflen) {\n    if (path.len < 0 || (size_t)path.len >= buflen) return 0;\n    memcpy(buf, path.data, (size_t)path.len);\n    buf[path.len] = 0;\n    return 1;\n}\n\nqoz_string qoz_fs_read_file(qoz_string path) {\n    char buf[4096];\n    if (!qoz_copy_path_nul(path, buf, sizeof(buf))) return (qoz_string){ NULL, -1 };\n    FILE *f = fopen(buf, \"rb\");\n    if (!f) return (qoz_string){ NULL, -1 };\n    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return (qoz_string){ NULL, -1 }; }\n    long n = ftell(f);\n    if (n < 0) { fclose(f); return (qoz_string){ NULL, -1 }; }\n    if (fseek(f, 0, SEEK_SET) != 0) { fclose(f); return (qoz_string){ NULL, -1 }; }\n    char *data = (char *)qoz_alloc((int64_t)n);\n    size_t got = fread(data, 1, (size_t)n, f);\n    fclose(f);\n    return (qoz_string){ data, (int64_t)got, data };\n}\n\nbool qoz_fs_file_exists(qoz_string path) {\n    char buf[4096];\n    if (!qoz_copy_path_nul(path, buf, sizeof(buf))) return false;\n    FILE *f = fopen(buf, \"rb\");\n    if (!f) return false;\n    fclose(f);\n    return true;\n}\n\nvoid *qoz_string_data(qoz_string s) {\n    return (void *)s.data;\n}\n\nqoz_string qoz_string_alias(void *buf, int64_t n) {\n    return (qoz_string){ (const char *)buf, n, (const char *)buf };\n}\n\nvoid qoz_bytes_copy(void *dst, void *src, int64_t n) {\n    if (n > 0) memcpy(dst, src, (size_t)n);\n}\n\nbool qoz_fs_write_file(qoz_string path, qoz_string content) {\n    char buf[4096];\n    if (!qoz_copy_path_nul(path, buf, sizeof(buf))) return false;\n    FILE *f = fopen(buf, \"wb\");\n    if (!f) return false;\n    size_t wrote = 0;\n    if (content.len > 0) wrote = fwrite(content.data, 1, (size_t)content.len, f);\n    fclose(f);\n    return wrote == (size_t)content.len;\n}\n\nstatic int qoz_str_lex_less(const void *a, const void *b) {\n    return strcmp(*(const char *const *)a, *(const char *const *)b);\n}\n\nqoz_string qoz_fs_list_qoz_files(qoz_string dir) {\n    char path[4096];\n    if (!qoz_copy_path_nul(dir, path, sizeof(path))) return (qoz_string){ NULL, 0 };\n    DIR *d = opendir(path);\n    if (!d) return (qoz_string){ NULL, 0 };\n\n    const char **names = NULL;\n    int64_t count = 0;\n    int64_t cap = 0;\n\n    struct dirent *ent;\n    while ((ent = readdir(d)) != NULL) {\n        const char *name = ent->d_name;\n        size_t nlen = strlen(name);\n        if (nlen < 4) continue;\n        if (memcmp(name + nlen - 4, \".qoz\", 4) != 0) continue;\n        if (count == cap) {\n            cap = cap == 0 ? 8 : cap * 2;\n            names = (const char **)realloc((void *)names, (size_t)cap * sizeof(*names));\n        }\n        char *dup = (char *)malloc(nlen + 1);\n        memcpy(dup, name, nlen + 1);\n        names[count++] = dup;\n    }\n    closedir(d);\n\n    if (count == 0) {\n        free((void *)names);\n        return (qoz_string){ NULL, 0 };\n    }\n    qsort(names, (size_t)count, sizeof(*names), qoz_str_lex_less);\n\n    int64_t total = 0;\n    for (int64_t i = 0; i < count; i++) total += (int64_t)strlen(names[i]);\n    total += count - 1;\n\n    char *out = (char *)qoz_alloc(total);\n    int64_t off = 0;\n    for (int64_t i = 0; i < count; i++) {\n        if (i > 0) { out[off++] = '\\n'; }\n        size_t nlen = strlen(names[i]);\n        memcpy(out + off, names[i], nlen);\n        off += (int64_t)nlen;\n        free((void *)names[i]);\n    }\n    free((void *)names);\n    return (qoz_string){ out, total, out };\n}\n\nbool qoz_string_has_prefix(qoz_string s, qoz_string p) {\n    if (p.len > s.len) return false;\n    return memcmp(s.data, p.data, (size_t)p.len) == 0;\n}\n\nbool qoz_string_has_suffix(qoz_string s, qoz_string suf) {\n    if (suf.len > s.len) return false;\n    return memcmp(s.data + s.len - suf.len, suf.data, (size_t)suf.len) == 0;\n}\n\nqoz_string qoz_string_cat(qoz_string a, qoz_string b) {\n    int64_t n = (a.len < 0 ? 0 : a.len) + (b.len < 0 ? 0 : b.len);\n    char *buf = (char *)qoz_alloc(n);\n    if (a.len > 0) memcpy(buf,         a.data, (size_t)a.len);\n    if (b.len > 0) memcpy(buf + a.len, b.data, (size_t)b.len);\n    return (qoz_string){ buf, n, buf };\n}\n\nint64_t qoz_string_byte_at(qoz_string s, int64_t i) {\n    if (i < 0 || i >= s.len) return -1;\n    return (int64_t)(unsigned char)s.data[i];\n}\n\nint64_t qoz_string_index_byte(qoz_string s, int64_t byte) {\n    for (int64_t i = 0; i < s.len; i++) {\n        if ((unsigned char)s.data[i] == (unsigned char)byte) return i;\n    }\n    return -1;\n}\n\nqoz_string qoz_string_slice(qoz_string s, int64_t from, int64_t to) {\n    if (from < 0) from = 0;\n    if (to > s.len) to = s.len;\n    if (from > to) from = to;\n    return (qoz_string){ s.data + from, to - from, s.root };\n}\n\nvoid qoz_strbuf_init(void *bv) {\n    qoz_strbuf *b = (qoz_strbuf *)bv;\n    b->buf = NULL;\n    b->len = 0;\n    b->cap = 0;\n}\n\nstatic void qoz_strbuf_grow(qoz_strbuf *b, int64_t needed) {\n    int64_t new_cap = b->cap == 0 ? 64 : b->cap;\n    while (new_cap < b->len + needed) new_cap *= 2;\n    b->buf = (char *)qoz_realloc(b->buf, new_cap);\n    b->cap = new_cap;\n}\n\nvoid qoz_strbuf_append_str(void *bv, qoz_string s) {\n    qoz_strbuf *b = (qoz_strbuf *)bv;\n    if (s.len <= 0) return;\n    if (b->len + s.len > b->cap) qoz_strbuf_grow(b, s.len);\n    memcpy(b->buf + b->len, s.data, (size_t)s.len);\n    b->len += s.len;\n}\n\nvoid qoz_strbuf_append_cstr(void *bv, const char *s) {\n    qoz_string ss = { s, (int64_t)strlen(s) };\n    qoz_strbuf_append_str(bv, ss);\n}\n\nvoid qoz_strbuf_append_i64(void *bv, int64_t v) {\n    qoz_strbuf *b = (qoz_strbuf *)bv;\n    char tmp[32];\n    int n = snprintf(tmp, sizeof(tmp), \"%\" PRId64, v);\n    if (n > 0) {\n        if (b->len + n > b->cap) qoz_strbuf_grow(b, n);\n        memcpy(b->buf + b->len, tmp, (size_t)n);\n        b->len += n;\n    }\n}\n\nvoid qoz_strbuf_append_f64(void *bv, double v) {\n    qoz_strbuf *b = (qoz_strbuf *)bv;\n    char tmp[64];\n    int n = snprintf(tmp, sizeof(tmp), \"%g\", v);\n    if (n > 0) {\n        if (b->len + n > b->cap) qoz_strbuf_grow(b, n);\n        memcpy(b->buf + b->len, tmp, (size_t)n);\n        b->len += n;\n    }\n}\n\nvoid qoz_strbuf_append_bool(void *bv, bool v) {\n    qoz_string s = v ? (qoz_string){ \"true\", 4 } : (qoz_string){ \"false\", 5 };\n    qoz_strbuf_append_str(bv, s);\n}\n\nqoz_string qoz_strbuf_finish(void *bv) {\n    qoz_strbuf *b = (qoz_strbuf *)bv;\n    return (qoz_string){ b->buf, b->len, b->buf };\n}\n\nint64_t qoz_string_parse_int(qoz_string s) {\n    char buf[64];\n    if (s.len < 0 || (size_t)s.len >= sizeof(buf)) return 0;\n    memcpy(buf, s.data, (size_t)s.len);\n    buf[s.len] = 0;\n    return (int64_t)strtoll(buf, NULL, 10);\n}\n\nvoid qoz_init(int *stack_anchor) {\n    /* gc.c owns the heap and auto-collects from qoz_gc_alloc once the\n     * live-byte threshold is crossed. The shadow stack registers every\n     * pointer-typed parameter and local; a conservative C-stack scan\n     * supplements that so register-resident return values are reached\n     * during the mark phase. */\n    qoz_gc_set_stack_bottom(stack_anchor);\n}\n\nvoid qoz_shutdown(void) {\n    qoz_gc_shutdown();\n}\n\nvoid *qoz_alloc(int64_t size) {\n    /* Used by string / Vec / Map data buffers and other arrays that do\n     * not carry a per-element type descriptor. Tracked by gc.c so the\n     * shutdown sweep frees them. */\n    return qoz_gc_alloc(size, NULL);\n}\n\nvoid *qoz_calloc(int64_t size) {\n    /* qoz_gc_alloc already zero-fills. */\n    return qoz_gc_alloc(size, NULL);\n}\n\nvoid *qoz_realloc(void *ptr, int64_t size) {\n    if (ptr == NULL) return qoz_gc_alloc(size, NULL);\n    /* For now treat realloc as alloc-and-copy. The GC tracks the new\n     * allocation; the old one will be freed by the next sweep (or at\n     * shutdown). Vec/Map are the common callers and they always retain\n     * a reference to the new pointer, so this is safe. */\n    int64_t old_size = qoz_gc_alloc_size(ptr);\n    void *p = qoz_gc_alloc(size, NULL);\n    if (p != NULL && old_size > 0) {\n        int64_t copy = old_size < size ? old_size : size;\n        memcpy(p, ptr, (size_t)copy);\n    }\n    qoz_gc_free(ptr);\n    return p;\n}\n\nbool qoz_string_eq(qoz_string a, qoz_string b) {\n    if (a.len != b.len) return false;\n    if (a.len == 0) return true;\n    return memcmp(a.data, b.data, (size_t)a.len) == 0;\n}\n\nuint64_t qoz_string_hash(qoz_string s) {\n    /* FNV-1a 64-bit. */\n    uint64_t h = 14695981039346656037ULL;\n    for (int64_t i = 0; i < s.len; i++) {\n        h ^= (uint8_t)s.data[i];\n        h *= 1099511628211ULL;\n    }\n    return h;\n}\n\nvoid qoz_print_str(qoz_string s) {\n    fwrite(s.data, 1, (size_t)s.len, stdout);\n}\n\nvoid qoz_print_cstr(const char *s) {\n    fputs(s, stdout);\n}\n\nvoid qoz_print_i64(int64_t v) {\n    printf(\"%\" PRId64, v);\n}\n\nvoid qoz_print_i32(int32_t v) {\n    printf(\"%\" PRId32, v);\n}\n\nvoid qoz_print_f64(double v) {\n    printf(\"%g\", v);\n}\n\nvoid qoz_print_bool(bool v) {\n    fputs(v ? \"true\" : \"false\", stdout);\n}\n\nvoid qoz_print_sep(void) {\n    fputc(' ', stdout);\n}\n\nvoid qoz_print_nl(void) {\n    fputc('\\n', stdout);\n    fflush(stdout);\n}\n\nvoid qoz_print_line(qoz_string s) {\n    if (s.len > 0) fwrite(s.data, 1, (size_t)s.len, stdout);\n    fputc('\\n', stdout);\n    fflush(stdout);\n}\n\n#include <unistd.h>\n#include <sys/wait.h>\n#include <errno.h>\n\n/* Drain a file descriptor into a newly-allocated qoz_string. The pipe\n * is read in chunks; the buffer doubles when full. Returns a string\n * whose `len` is the byte count and whose `data` and `root` point to a\n * GC allocation, so the returned value stays reachable while in scope. */\nstatic qoz_string qoz_drain_fd(int fd) {\n    int64_t cap = 4096;\n    char *buf = (char *)qoz_alloc(cap);\n    int64_t n = 0;\n    for (;;) {\n        if (n + 1024 > cap) {\n            int64_t new_cap = cap * 2;\n            char *nb = (char *)qoz_alloc(new_cap);\n            memcpy(nb, buf, (size_t)n);\n            buf = nb;\n            cap = new_cap;\n        }\n        ssize_t got = read(fd, buf + n, (size_t)(cap - n));\n        if (got <= 0) break;\n        n += (int64_t)got;\n    }\n    close(fd);\n    return (qoz_string){ buf, n, buf };\n}\n\nvoid qoz_process_exec(qoz_string *argv, int64_t n,\n                      int64_t *out_exit,\n                      qoz_string *out_stdout,\n                      qoz_string *out_stderr) {\n    *out_exit = -1;\n    *out_stdout = (qoz_string){ NULL, 0 };\n    *out_stderr = (qoz_string){ NULL, 0 };\n\n    if (n <= 0 || argv == NULL) return;\n\n    /* Build a NUL-terminated char** for execvp from the input qoz_string\n     * array. Strings inside argv may not be NUL-terminated (they can\n     * alias slices), so each one is copied into its own buffer. */\n    char **cargv = (char **)qoz_alloc((int64_t)((n + 1) * (int64_t)sizeof(char *)));\n    for (int64_t i = 0; i < n; i++) {\n        int64_t len = argv[i].len;\n        char *s = (char *)qoz_alloc(len + 1);\n        if (len > 0) memcpy(s, argv[i].data, (size_t)len);\n        s[len] = '\\0';\n        cargv[i] = s;\n    }\n    cargv[n] = NULL;\n\n    int out_pipe[2] = { -1, -1 };\n    int err_pipe[2] = { -1, -1 };\n    if (pipe(out_pipe) != 0) return;\n    if (pipe(err_pipe) != 0) { close(out_pipe[0]); close(out_pipe[1]); return; }\n\n    pid_t pid = fork();\n    if (pid < 0) {\n        close(out_pipe[0]); close(out_pipe[1]);\n        close(err_pipe[0]); close(err_pipe[1]);\n        return;\n    }\n    if (pid == 0) {\n        /* Child. */\n        dup2(out_pipe[1], 1);\n        dup2(err_pipe[1], 2);\n        close(out_pipe[0]); close(out_pipe[1]);\n        close(err_pipe[0]); close(err_pipe[1]);\n        execvp(cargv[0], cargv);\n        /* If exec fails the child exits 127, matching POSIX convention\n         * for \"command not found\". */\n        _exit(127);\n    }\n\n    /* Parent. Close the write ends; read until EOF so the child can\n     * make progress when its pipes are full. */\n    close(out_pipe[1]);\n    close(err_pipe[1]);\n    *out_stdout = qoz_drain_fd(out_pipe[0]);\n    *out_stderr = qoz_drain_fd(err_pipe[0]);\n\n    int status = 0;\n    while (waitpid(pid, &status, 0) < 0) {\n        if (errno != EINTR) { *out_exit = -1; return; }\n    }\n    if (WIFEXITED(status)) {\n        *out_exit = (int64_t)WEXITSTATUS(status);\n    } else {\n        *out_exit = -1;\n    }\n}\n"))); emit_push(&e, QOZ_STR_LIT("\n/* user code */\n")); e.pos_fn_typedefs = strings_sb_len(&e.out); e.pos_tuple_typedefs = strings_sb_len(&e.out); { qoz_Vec__qoz_Instantiation __col = e.record_insts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Instantiation inst = __col.data[__i]; (void)inst; emit_push(&e, QOZ_STR_LIT("typedef struct qoz_")); emit_push(&e, inst.mangled); emit_push(&e, QOZ_STR_LIT(" qoz_")); emit_push(&e, inst.mangled); emit_push(&e, QOZ_STR_LIT(";\n")); } }{ qoz_Vec__qoz_Instantiation __col = e.enum_insts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Instantiation inst = __col.data[__i]; (void)inst; emit_push(&e, QOZ_STR_LIT("typedef struct qoz_")); emit_push(&e, inst.mangled); emit_push(&e, QOZ_STR_LIT(" qoz_")); emit_push(&e, inst.mangled); emit_push(&e, QOZ_STR_LIT(";\n")); qoz_map_set__qoz_string__bool(&e.is_enum, inst.mangled, true); } }emit_push(&e, QOZ_STR_LIT("\n")); { qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_253 = d; switch (_qoz_ms_253->tag) { case qoz_Decl_DStruct: { qoz_string name = _qoz_ms_253->payload.DStruct.f1; qoz_Vec__qoz_string params = _qoz_ms_253->payload.DStruct.f2; if ((params.len) == 0) { emit_push(&e, QOZ_STR_LIT("typedef struct qoz_")); emit_push(&e, name); emit_push(&e, QOZ_STR_LIT(" qoz_")); emit_push(&e, name); emit_push(&e, QOZ_STR_LIT(";\n")); } 0;  break; } case qoz_Decl_DEnum: { qoz_string name = _qoz_ms_253->payload.DEnum.f1; qoz_Vec__qoz_string params = _qoz_ms_253->payload.DEnum.f2; if ((params.len) == 0) { emit_push(&e, QOZ_STR_LIT("typedef struct qoz_")); emit_push(&e, name); emit_push(&e, QOZ_STR_LIT(" qoz_")); emit_push(&e, name); emit_push(&e, QOZ_STR_LIT(";\n")); } 0;  break; } default: { NULL;  break; } } 0; } }emit_push(&e, QOZ_STR_LIT("\n")); e.pos_synth_fn_decls = strings_sb_len(&e.out); emit_emit_records_topological(&e, f); { qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_254 = d; switch (_qoz_ms_254->tag) { case qoz_Decl_DEnum: { qoz_string name = _qoz_ms_254->payload.DEnum.f1; qoz_Vec__qoz_string params = _qoz_ms_254->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_254->payload.DEnum.f3; if ((params.len) == 0) { emit_emit_enum(&e, name, variants); } 0;  break; } default: { NULL;  break; } } 0; } }{ qoz_Vec__qoz_Instantiation __col = e.enum_insts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Instantiation inst = __col.data[__i]; (void)inst; qoz_Option__qoz_Decl* _qoz_ms_255 = qoz_map_get__qoz_string__qoz_Decl(&e.generic_decls, inst.name); switch (_qoz_ms_255->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_255->payload.Some.f0; qoz_Decl* _qoz_ms_256 = decl; switch (_qoz_ms_256->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string params = _qoz_ms_256->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_256->payload.DEnum.f3; {
        qoz_Vec__qoz_VariantDecl subst_variants = qoz_vec_make__qoz_VariantDecl(); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; qoz_Vec__qoz_TypeExpr new_pos = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pt = __col.data[__i]; (void)pt; qoz_vec_push__qoz_TypeExpr(&new_pos, emit_substitute_type(&e, pt, params, inst.args)); } }qoz_Vec__qoz_StructField new_named = qoz_vec_make__qoz_StructField(); { qoz_Vec__qoz_StructField __col = v.named; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField nf = __col.data[__i]; (void)nf; qoz_vec_push__qoz_StructField(&new_named, ((qoz_StructField){ .name = nf.name, .ty = emit_substitute_type(&e, nf.ty, params, inst.args) })); } }qoz_vec_push__qoz_VariantDecl(&subst_variants, ((qoz_VariantDecl){ .span = v.span, .name = v.name, .kind = v.kind, .pos = new_pos, .named = new_named })); } }emit_emit_enum(&e, inst.mangled, subst_variants); { qoz_Vec__qoz_VariantDecl __col = subst_variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; emit_emit_variant_ctor(&e, inst.mangled, v); } }
    }
    0;  break; } default: { NULL;  break; } } 0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; } }{ qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_257 = d; switch (_qoz_ms_257->tag) { case qoz_Decl_DEnum: { qoz_string name = _qoz_ms_257->payload.DEnum.f1; qoz_Vec__qoz_string params = _qoz_ms_257->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_257->payload.DEnum.f3; if ((params.len) == 0) { { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; emit_emit_variant_ctor(&e, name, v); } }} 0;  break; } default: { NULL;  break; } } 0; } }{ qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_258 = d; switch (_qoz_ms_258->tag) { case qoz_Decl_DFn: { qoz_string name = _qoz_ms_258->payload.DFn.f1; qoz_Vec__qoz_string tparams = _qoz_ms_258->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_258->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_258->payload.DFn.f4; if (!emit_is_main(name) && ((tparams.len) == 0)) { emit_emit_fn_proto(&e, name, params, ret); } 0;  break; } case qoz_Decl_DExternal: { qoz_string symbol = _qoz_ms_258->payload.DExternal.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_258->payload.DExternal.f3; qoz_TypeExpr* ret = _qoz_ms_258->payload.DExternal.f4; emit_emit_extern_proto(&e, symbol, params, ret);  break; } default: { NULL;  break; } } 0; } }{ qoz_Vec__qoz_Instantiation __col = e.fn_insts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Instantiation inst = __col.data[__i]; (void)inst; qoz_Option__qoz_Decl* _qoz_ms_259 = qoz_map_get__qoz_string__qoz_Decl(&e.generic_fn_decls, inst.name); switch (_qoz_ms_259->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_259->payload.Some.f0; qoz_Decl* _qoz_ms_260 = decl; switch (_qoz_ms_260->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string tparams = _qoz_ms_260->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_260->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_260->payload.DFn.f4; {
        qoz_Vec__qoz_FnParam subst_params = qoz_vec_make__qoz_FnParam(); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; qoz_vec_push__qoz_FnParam(&subst_params, ((qoz_FnParam){ .name = pp.name, .ty = emit_substitute_type(&e, pp.ty, tparams, inst.args) })); } }qoz_TypeExpr* subst_ret = emit_substitute_type(&e, ret, tparams, inst.args); qoz_gc_push_root(&subst_ret); emit_emit_fn_proto(&e, qoz_string_cat(QOZ_STR_LIT("qoz_"), inst.mangled), subst_params, subst_ret); 
    }
    0;  break; } default: { NULL;  break; } } 0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; } }emit_push(&e, QOZ_STR_LIT("\n")); { qoz_Vec__qoz_Decl __col = f.decls; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Decl* d = __col.data[__i]; (void)d; qoz_Decl* _qoz_ms_261 = d; switch (_qoz_ms_261->tag) { case qoz_Decl_DFn: { qoz_string name = _qoz_ms_261->payload.DFn.f1; qoz_Vec__qoz_string tparams = _qoz_ms_261->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_261->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_261->payload.DFn.f4; qoz_Expr* body = _qoz_ms_261->payload.DFn.f5; if (emit_is_main(name)) { emit_emit_main(&e, ret, body); }  else if ((tparams.len) == 0) { emit_emit_fn(&e, name, params, ret, body); } 0;  break; } default: { NULL;  break; } } 0; } }{ qoz_Vec__qoz_Instantiation __col = e.fn_insts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Instantiation inst = __col.data[__i]; (void)inst; qoz_Option__qoz_Decl* _qoz_ms_262 = qoz_map_get__qoz_string__qoz_Decl(&e.generic_fn_decls, inst.name); switch (_qoz_ms_262->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_262->payload.Some.f0; qoz_Decl* _qoz_ms_263 = decl; switch (_qoz_ms_263->tag) { case qoz_Decl_DFn: { qoz_Vec__qoz_string tparams = _qoz_ms_263->payload.DFn.f2; qoz_Vec__qoz_FnParam params = _qoz_ms_263->payload.DFn.f3; qoz_TypeExpr* ret = _qoz_ms_263->payload.DFn.f4; qoz_Expr* body = _qoz_ms_263->payload.DFn.f5; {
        qoz_Vec__qoz_FnParam subst_params = qoz_vec_make__qoz_FnParam(); { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam pp = __col.data[__i]; (void)pp; qoz_vec_push__qoz_FnParam(&subst_params, ((qoz_FnParam){ .name = pp.name, .ty = emit_substitute_type(&e, pp.ty, tparams, inst.args) })); } }qoz_TypeExpr* subst_ret = emit_substitute_type(&e, ret, tparams, inst.args); qoz_gc_push_root(&subst_ret); qoz_Expr* subst_body = emit_substitute_expr(&e, body, tparams, inst.args); qoz_gc_push_root(&subst_body); qoz_Vec__qoz_string saved_tparams = e.current_tparams; qoz_Vec__qoz_TypeExpr saved_targs = e.current_targs; e.current_tparams = tparams; e.current_targs = inst.args; emit_emit_fn(&e, qoz_string_cat(QOZ_STR_LIT("qoz_"), inst.mangled), subst_params, subst_ret, subst_body); e.current_tparams = saved_tparams; e.current_targs = saved_targs; 
    }
    0;  break; } default: { NULL;  break; } } 0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; } }{ qoz_Vec__qoz_string __col = e.synth_fn_defs; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string s = __col.data[__i]; (void)s; emit_push(&e, s); } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return emit_render(&e);
}

void emit_emit_enum(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_VariantDecl variants) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    emit_push(e, QOZ_STR_LIT("typedef enum {\n")); int64_t i = 0; { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; qoz_string vn = v.name; qoz_string _qoz_bv_221;
    {
        qoz_Strbuf _qoz_sb_3358_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3358_17); strings_sb_append(&_qoz_sb_3358_17, QOZ_STR_LIT("    qoz_")); strings_sb_append(&_qoz_sb_3358_17, name); strings_sb_append(&_qoz_sb_3358_17, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3358_17, vn); strings_sb_append(&_qoz_sb_3358_17, QOZ_STR_LIT(",\n")); _qoz_bv_221 = strings_sb_finish(&_qoz_sb_3358_17);
    }
    emit_push(e, _qoz_bv_221); i = i + 1; } }qoz_string _qoz_bv_222;
    {
        qoz_Strbuf _qoz_sb_3361_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3361_13); strings_sb_append(&_qoz_sb_3361_13, QOZ_STR_LIT("} qoz_")); strings_sb_append(&_qoz_sb_3361_13, name); strings_sb_append(&_qoz_sb_3361_13, QOZ_STR_LIT("_tag;\n\n")); _qoz_bv_222 = strings_sb_finish(&_qoz_sb_3361_13);
    }
    emit_push(e, _qoz_bv_222); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (emit_variant_has_positional(v)) { qoz_string vn = v.name; emit_push(e, QOZ_STR_LIT("typedef struct {\n")); int64_t k = 0; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pt = __col.data[__i]; (void)pt; qoz_string pc = emit_c_type_for(e, pt); qoz_string ks = emit_int_to_string(k); qoz_string _qoz_bv_223;
    {
        qoz_Strbuf _qoz_sb_3370_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3370_25); strings_sb_append(&_qoz_sb_3370_25, QOZ_STR_LIT("    ")); strings_sb_append(&_qoz_sb_3370_25, pc); strings_sb_append(&_qoz_sb_3370_25, QOZ_STR_LIT(" f")); strings_sb_append(&_qoz_sb_3370_25, ks); strings_sb_append(&_qoz_sb_3370_25, QOZ_STR_LIT(";\n")); _qoz_bv_223 = strings_sb_finish(&_qoz_sb_3370_25);
    }
    emit_push(e, _qoz_bv_223); k = k + 1; } }qoz_string _qoz_bv_224;
    {
        qoz_Strbuf _qoz_sb_3373_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3373_21); strings_sb_append(&_qoz_sb_3373_21, QOZ_STR_LIT("} qoz_")); strings_sb_append(&_qoz_sb_3373_21, name); strings_sb_append(&_qoz_sb_3373_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3373_21, vn); strings_sb_append(&_qoz_sb_3373_21, QOZ_STR_LIT("_payload;\n\n")); _qoz_bv_224 = strings_sb_finish(&_qoz_sb_3373_21);
    }
    emit_push(e, _qoz_bv_224); } } }qoz_string _qoz_bv_225;
    {
        qoz_Strbuf _qoz_sb_3376_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3376_13); strings_sb_append(&_qoz_sb_3376_13, QOZ_STR_LIT("struct qoz_")); strings_sb_append(&_qoz_sb_3376_13, name); strings_sb_append(&_qoz_sb_3376_13, QOZ_STR_LIT(" {\n    qoz_")); strings_sb_append(&_qoz_sb_3376_13, name); strings_sb_append(&_qoz_sb_3376_13, QOZ_STR_LIT("_tag tag;\n")); _qoz_bv_225 = strings_sb_finish(&_qoz_sb_3376_13);
    }
    emit_push(e, _qoz_bv_225); bool any_payload = false; { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (emit_variant_has_positional(v)) { any_payload = true; } } }if (any_payload) { emit_push(e, QOZ_STR_LIT("    union {\n")); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (emit_variant_has_positional(v)) { qoz_string vn = v.name; qoz_string _qoz_bv_226;
    {
        qoz_Strbuf _qoz_sb_3386_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3386_25); strings_sb_append(&_qoz_sb_3386_25, QOZ_STR_LIT("        qoz_")); strings_sb_append(&_qoz_sb_3386_25, name); strings_sb_append(&_qoz_sb_3386_25, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3386_25, vn); strings_sb_append(&_qoz_sb_3386_25, QOZ_STR_LIT("_payload ")); strings_sb_append(&_qoz_sb_3386_25, vn); strings_sb_append(&_qoz_sb_3386_25, QOZ_STR_LIT(";\n")); _qoz_bv_226 = strings_sb_finish(&_qoz_sb_3386_25);
    }
    emit_push(e, _qoz_bv_226); } } }emit_push(e, QOZ_STR_LIT("    } payload;\n")); } emit_push(e, QOZ_STR_LIT("};\n\n")); emit_emit_adt_desc(e, name, variants); 
    return;
}

bool emit_variant_has_positional(qoz_VariantDecl v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return (v.pos.len) > 0;
}

qoz_string emit_int_to_string(int64_t n) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Strbuf _qoz_sb_3397_37 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3397_37); strings_sb_append_i64(&_qoz_sb_3397_37, n); qoz_gc_shadow_set_top(_qoz_shadow_guard); return strings_sb_finish(&_qoz_sb_3397_37);
}

void emit_emit_variant_ctor(qoz_Emitter* e, qoz_string enum_name, qoz_VariantDecl v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string vn = v.name; qoz_string _qoz_bv_227;
    {
        qoz_Strbuf _qoz_sb_3401_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3401_13); strings_sb_append(&_qoz_sb_3401_13, QOZ_STR_LIT("static qoz_")); strings_sb_append(&_qoz_sb_3401_13, enum_name); strings_sb_append(&_qoz_sb_3401_13, QOZ_STR_LIT(" *qoz_make_")); strings_sb_append(&_qoz_sb_3401_13, enum_name); strings_sb_append(&_qoz_sb_3401_13, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3401_13, vn); _qoz_bv_227 = strings_sb_finish(&_qoz_sb_3401_13);
    }
    emit_push(e, _qoz_bv_227); if (!emit_variant_has_positional(v)) { emit_push(e, QOZ_STR_LIT("(void) {\n")); }  else { emit_push(e, QOZ_STR_LIT("(")); int64_t k = 0; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pt = __col.data[__i]; (void)pt; if (k > 0) { emit_push(e, QOZ_STR_LIT(", ")); } qoz_string pc = emit_c_type_for(e, pt); qoz_string ks = emit_int_to_string(k); qoz_string _qoz_bv_228;
    {
        qoz_Strbuf _qoz_sb_3411_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3411_21); strings_sb_append(&_qoz_sb_3411_21, pc); strings_sb_append(&_qoz_sb_3411_21, QOZ_STR_LIT(" f")); strings_sb_append(&_qoz_sb_3411_21, ks); _qoz_bv_228 = strings_sb_finish(&_qoz_sb_3411_21);
    }
    emit_push(e, _qoz_bv_228); k = k + 1; } }emit_push(e, QOZ_STR_LIT(") {\n")); } qoz_string _qoz_bv_229;
    {
        qoz_Strbuf _qoz_sb_3416_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3416_13); strings_sb_append(&_qoz_sb_3416_13, QOZ_STR_LIT("    qoz_")); strings_sb_append(&_qoz_sb_3416_13, enum_name); strings_sb_append(&_qoz_sb_3416_13, QOZ_STR_LIT(" *p = qoz_gc_alloc(sizeof(qoz_")); strings_sb_append(&_qoz_sb_3416_13, enum_name); strings_sb_append(&_qoz_sb_3416_13, QOZ_STR_LIT("), &qoz_")); strings_sb_append(&_qoz_sb_3416_13, enum_name); strings_sb_append(&_qoz_sb_3416_13, QOZ_STR_LIT("_desc);\n")); _qoz_bv_229 = strings_sb_finish(&_qoz_sb_3416_13);
    }
    emit_push(e, _qoz_bv_229); qoz_string _qoz_bv_230;
    {
        qoz_Strbuf _qoz_sb_3417_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3417_13); strings_sb_append(&_qoz_sb_3417_13, QOZ_STR_LIT("    p->tag = qoz_")); strings_sb_append(&_qoz_sb_3417_13, enum_name); strings_sb_append(&_qoz_sb_3417_13, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3417_13, vn); strings_sb_append(&_qoz_sb_3417_13, QOZ_STR_LIT(";\n")); _qoz_bv_230 = strings_sb_finish(&_qoz_sb_3417_13);
    }
    emit_push(e, _qoz_bv_230); if (emit_variant_has_positional(v)) { int64_t j = 0; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* _ = __col.data[__i]; (void)_; qoz_string js = emit_int_to_string(j); qoz_string _qoz_bv_231;
    {
        qoz_Strbuf _qoz_sb_3422_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3422_21); strings_sb_append(&_qoz_sb_3422_21, QOZ_STR_LIT("    p->payload.")); strings_sb_append(&_qoz_sb_3422_21, vn); strings_sb_append(&_qoz_sb_3422_21, QOZ_STR_LIT(".f")); strings_sb_append(&_qoz_sb_3422_21, js); strings_sb_append(&_qoz_sb_3422_21, QOZ_STR_LIT(" = f")); strings_sb_append(&_qoz_sb_3422_21, js); strings_sb_append(&_qoz_sb_3422_21, QOZ_STR_LIT(";\n")); _qoz_bv_231 = strings_sb_finish(&_qoz_sb_3422_21);
    }
    emit_push(e, _qoz_bv_231); j = j + 1; } }} emit_push(e, QOZ_STR_LIT("    return p;\n}\n\n")); 
    return;
}

void emit_collect_field_ptr_offsets(qoz_Emitter* e, qoz_string parent, qoz_string field, qoz_TypeExpr* te, qoz_Vec__qoz_string* out) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_gc_push_root(&out);
    qoz_string base = qoz_string_cat(QOZ_STR_LIT("offsetof("), qoz_string_cat(parent, qoz_string_cat(QOZ_STR_LIT(", "), qoz_string_cat(field, QOZ_STR_LIT(")"))))); qoz_TypeExpr* _qoz_ms_264 = te; switch (_qoz_ms_264->tag) { case qoz_TypeExpr_TEPtr: { qoz_vec_push__qoz_string(out, qoz_string_cat(QOZ_STR_LIT("(int32_t)"), base));  break; } case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_264->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_264->payload.TENamed.f2; if ((path.len) >= 1) { qoz_string n = path.data[(path.len) - 1]; if (qoz_map_contains__qoz_string__bool(&e->is_enum, n)) { qoz_vec_push__qoz_string(out, qoz_string_cat(QOZ_STR_LIT("(int32_t)"), base)); return;} if (qoz_string_eq(n, QOZ_STR_LIT("string"))) { qoz_vec_push__qoz_string(out, qoz_string_cat(QOZ_STR_LIT("(int32_t)("), qoz_string_cat(base, QOZ_STR_LIT(" + offsetof(qoz_string, data))")))); qoz_vec_push__qoz_string(out, qoz_string_cat(QOZ_STR_LIT("(int32_t)("), qoz_string_cat(base, QOZ_STR_LIT(" + offsetof(qoz_string, root))")))); return;} if (qoz_string_eq(n, QOZ_STR_LIT("Vec")) || qoz_string_eq(n, QOZ_STR_LIT("Map"))) { qoz_vec_push__qoz_string(out, qoz_string_cat(QOZ_STR_LIT("(int32_t)"), base)); return;} if ((args.len) == 0) { qoz_Option__qoz_Decl* _qoz_ms_265 = qoz_map_get__qoz_string__qoz_Decl(&e->struct_decls, n); switch (_qoz_ms_265->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_265->payload.Some.f0; qoz_Decl* _qoz_ms_266 = d; switch (_qoz_ms_266->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_266->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_266->payload.DStruct.f3; if ((params.len) == 0) { qoz_string inner_parent = qoz_string_cat(QOZ_STR_LIT("struct qoz_"), n); { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField sf = __col.data[__i]; (void)sf; qoz_string composed_parent = inner_parent; qoz_string composed_field = sf.name; qoz_Vec__qoz_string inner_offsets = qoz_vec_make__qoz_string(); emit_collect_field_ptr_offsets(e, composed_parent, composed_field, sf.ty, &inner_offsets); { qoz_Vec__qoz_string __col = inner_offsets; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string inner = __col.data[__i]; (void)inner; qoz_string stripped = qoz_string_slice(inner, 9, (inner).len); qoz_vec_push__qoz_string(out, qoz_string_cat(QOZ_STR_LIT("(int32_t)("), qoz_string_cat(base, qoz_string_cat(QOZ_STR_LIT(" + "), qoz_string_cat(stripped, QOZ_STR_LIT(")")))))); } }} }return;} 0;  break; } default: { NULL;  break; } } 0;  break; } case qoz_Option__qoz_Decl_None: { NULL;  break; } } 0; } } 0;  break; } default: { NULL;  break; } } 0; 
    return;
}

void emit_emit_record_desc(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_StructField fields) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string parent = qoz_string_cat(QOZ_STR_LIT("struct qoz_"), name); qoz_Vec__qoz_string offsets = qoz_vec_make__qoz_string(); { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; emit_collect_field_ptr_offsets(e, parent, f.name, f.ty, &offsets); } }int64_t nptrs = (offsets.len); if (nptrs > 0) { qoz_string _qoz_bv_232;
    {
        qoz_Strbuf _qoz_sb_3511_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3511_17); strings_sb_append(&_qoz_sb_3511_17, QOZ_STR_LIT("static const int32_t qoz_")); strings_sb_append(&_qoz_sb_3511_17, name); strings_sb_append(&_qoz_sb_3511_17, QOZ_STR_LIT("_offsets[] = { ")); _qoz_bv_232 = strings_sb_finish(&_qoz_sb_3511_17);
    }
    emit_push(e, _qoz_bv_232); int64_t i = 0; { qoz_Vec__qoz_string __col = offsets; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string expr = __col.data[__i]; (void)expr; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } emit_push(e, expr); i = i + 1; } }emit_push(e, QOZ_STR_LIT(" };\n")); } if (nptrs == 0) { qoz_string _qoz_bv_233;
    {
        qoz_Strbuf _qoz_sb_3521_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3521_17); strings_sb_append(&_qoz_sb_3521_17, QOZ_STR_LIT("static const qoz_type_desc qoz_")); strings_sb_append(&_qoz_sb_3521_17, name); strings_sb_append(&_qoz_sb_3521_17, QOZ_STR_LIT("_desc = { QOZ_DESC_LEAF, (int32_t)sizeof(struct qoz_")); strings_sb_append(&_qoz_sb_3521_17, name); strings_sb_append(&_qoz_sb_3521_17, QOZ_STR_LIT("), 0, NULL, 0, 0, 0, NULL, \"")); strings_sb_append(&_qoz_sb_3521_17, name); strings_sb_append(&_qoz_sb_3521_17, QOZ_STR_LIT("\" };\n\n")); _qoz_bv_233 = strings_sb_finish(&_qoz_sb_3521_17);
    }
    emit_push(e, _qoz_bv_233); }  else { qoz_string np = emit_int_to_string(nptrs); qoz_string _qoz_bv_234;
    {
        qoz_Strbuf _qoz_sb_3524_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3524_17); strings_sb_append(&_qoz_sb_3524_17, QOZ_STR_LIT("static const qoz_type_desc qoz_")); strings_sb_append(&_qoz_sb_3524_17, name); strings_sb_append(&_qoz_sb_3524_17, QOZ_STR_LIT("_desc = { QOZ_DESC_OFFSETS, (int32_t)sizeof(struct qoz_")); strings_sb_append(&_qoz_sb_3524_17, name); strings_sb_append(&_qoz_sb_3524_17, QOZ_STR_LIT("), ")); strings_sb_append(&_qoz_sb_3524_17, np); strings_sb_append(&_qoz_sb_3524_17, QOZ_STR_LIT(", qoz_")); strings_sb_append(&_qoz_sb_3524_17, name); strings_sb_append(&_qoz_sb_3524_17, QOZ_STR_LIT("_offsets, 0, 0, 0, NULL, \"")); strings_sb_append(&_qoz_sb_3524_17, name); strings_sb_append(&_qoz_sb_3524_17, QOZ_STR_LIT("\" };\n\n")); _qoz_bv_234 = strings_sb_finish(&_qoz_sb_3524_17);
    }
    emit_push(e, _qoz_bv_234); } 
    return;
}

qoz_Vec__qoz_string emit_collect_variant_ptr_offsets(qoz_Emitter* e, qoz_string enum_name, qoz_VariantDecl v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_Vec__qoz_string out = qoz_vec_make__qoz_string(); qoz_string parent = qoz_string_cat(QOZ_STR_LIT("qoz_"), qoz_string_cat(enum_name, qoz_string_cat(QOZ_STR_LIT("_"), qoz_string_cat(v.name, QOZ_STR_LIT("_payload"))))); if (emit_variant_has_positional(v)) { int64_t i = 0; { qoz_Vec__qoz_TypeExpr __col = v.pos; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* pt = __col.data[__i]; (void)pt; qoz_string field = qoz_string_cat(QOZ_STR_LIT("f"), emit_int_to_string(i)); emit_collect_field_ptr_offsets(e, parent, field, pt, &out); i = i + 1; } }return out;} { qoz_Vec__qoz_StructField __col = v.named; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField sf = __col.data[__i]; (void)sf; emit_collect_field_ptr_offsets(e, parent, sf.name, sf.ty, &out); } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

void emit_emit_adt_desc(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_VariantDecl variants) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; qoz_Vec__qoz_string voffs = emit_collect_variant_ptr_offsets(e, name, v); if ((voffs.len) > 0) { qoz_string vn = v.name; qoz_string _qoz_bv_235;
    {
        qoz_Strbuf _qoz_sb_3561_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3561_21); strings_sb_append(&_qoz_sb_3561_21, QOZ_STR_LIT("static const int32_t qoz_")); strings_sb_append(&_qoz_sb_3561_21, name); strings_sb_append(&_qoz_sb_3561_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3561_21, vn); strings_sb_append(&_qoz_sb_3561_21, QOZ_STR_LIT("_offsets[] = { ")); _qoz_bv_235 = strings_sb_finish(&_qoz_sb_3561_21);
    }
    emit_push(e, _qoz_bv_235); int64_t j = 0; { qoz_Vec__qoz_string __col = voffs; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_string expr = __col.data[__i]; (void)expr; if (j > 0) { emit_push(e, QOZ_STR_LIT(", ")); } emit_push(e, expr); j = j + 1; } }emit_push(e, QOZ_STR_LIT(" };\n")); } } }qoz_string _qoz_bv_236;
    {
        qoz_Strbuf _qoz_sb_3571_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3571_13); strings_sb_append(&_qoz_sb_3571_13, QOZ_STR_LIT("static const qoz_variant_desc qoz_")); strings_sb_append(&_qoz_sb_3571_13, name); strings_sb_append(&_qoz_sb_3571_13, QOZ_STR_LIT("_variants[] = {\n")); _qoz_bv_236 = strings_sb_finish(&_qoz_sb_3571_13);
    }
    emit_push(e, _qoz_bv_236); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; qoz_Vec__qoz_string voffs = emit_collect_variant_ptr_offsets(e, name, v); int64_t nptrs = (voffs.len); qoz_string vn = v.name; qoz_string np = emit_int_to_string(nptrs); if (nptrs > 0) { qoz_string _qoz_bv_237;
    {
        qoz_Strbuf _qoz_sb_3578_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3578_21); strings_sb_append(&_qoz_sb_3578_21, QOZ_STR_LIT("    { qoz_")); strings_sb_append(&_qoz_sb_3578_21, name); strings_sb_append(&_qoz_sb_3578_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3578_21, vn); strings_sb_append(&_qoz_sb_3578_21, QOZ_STR_LIT(", ")); strings_sb_append(&_qoz_sb_3578_21, np); strings_sb_append(&_qoz_sb_3578_21, QOZ_STR_LIT(", qoz_")); strings_sb_append(&_qoz_sb_3578_21, name); strings_sb_append(&_qoz_sb_3578_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3578_21, vn); strings_sb_append(&_qoz_sb_3578_21, QOZ_STR_LIT("_offsets },\n")); _qoz_bv_237 = strings_sb_finish(&_qoz_sb_3578_21);
    }
    emit_push(e, _qoz_bv_237); }  else { qoz_string _qoz_bv_238;
    {
        qoz_Strbuf _qoz_sb_3580_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3580_21); strings_sb_append(&_qoz_sb_3580_21, QOZ_STR_LIT("    { qoz_")); strings_sb_append(&_qoz_sb_3580_21, name); strings_sb_append(&_qoz_sb_3580_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3580_21, vn); strings_sb_append(&_qoz_sb_3580_21, QOZ_STR_LIT(", ")); strings_sb_append(&_qoz_sb_3580_21, np); strings_sb_append(&_qoz_sb_3580_21, QOZ_STR_LIT(", NULL },\n")); _qoz_bv_238 = strings_sb_finish(&_qoz_sb_3580_21);
    }
    emit_push(e, _qoz_bv_238); } } }emit_push(e, QOZ_STR_LIT("};\n")); bool any_payload2 = false; { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (emit_variant_has_positional(v)) { any_payload2 = true; } } }qoz_string payload_off_expr = ((any_payload2) ? qoz_string_cat(QOZ_STR_LIT("(int32_t)offsetof(struct qoz_"), qoz_string_cat(name, QOZ_STR_LIT(", payload)"))) : QOZ_STR_LIT("0")); qoz_string nvar = emit_int_to_string((variants.len)); qoz_string _qoz_bv_239;
    {
        qoz_Strbuf _qoz_sb_3594_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3594_13); strings_sb_append(&_qoz_sb_3594_13, QOZ_STR_LIT("static const qoz_type_desc qoz_")); strings_sb_append(&_qoz_sb_3594_13, name); strings_sb_append(&_qoz_sb_3594_13, QOZ_STR_LIT("_desc = { QOZ_DESC_ADT, (int32_t)sizeof(struct qoz_")); strings_sb_append(&_qoz_sb_3594_13, name); strings_sb_append(&_qoz_sb_3594_13, QOZ_STR_LIT("), 0, NULL, (int32_t)offsetof(struct qoz_")); strings_sb_append(&_qoz_sb_3594_13, name); strings_sb_append(&_qoz_sb_3594_13, QOZ_STR_LIT(", tag), ")); strings_sb_append(&_qoz_sb_3594_13, payload_off_expr); strings_sb_append(&_qoz_sb_3594_13, QOZ_STR_LIT(", ")); strings_sb_append(&_qoz_sb_3594_13, nvar); strings_sb_append(&_qoz_sb_3594_13, QOZ_STR_LIT(", qoz_")); strings_sb_append(&_qoz_sb_3594_13, name); strings_sb_append(&_qoz_sb_3594_13, QOZ_STR_LIT("_variants, \"")); strings_sb_append(&_qoz_sb_3594_13, name); strings_sb_append(&_qoz_sb_3594_13, QOZ_STR_LIT("\" };\n\n")); _qoz_bv_239 = strings_sb_finish(&_qoz_sb_3594_13);
    }
    emit_push(e, _qoz_bv_239); 
    return;
}

void emit_emit_struct(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_StructField fields) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string _qoz_bv_240;
    {
        qoz_Strbuf _qoz_sb_3598_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3598_13); strings_sb_append(&_qoz_sb_3598_13, QOZ_STR_LIT("struct qoz_")); strings_sb_append(&_qoz_sb_3598_13, name); strings_sb_append(&_qoz_sb_3598_13, QOZ_STR_LIT(" {\n")); _qoz_bv_240 = strings_sb_finish(&_qoz_sb_3598_13);
    }
    emit_push(e, _qoz_bv_240); { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; qoz_string ct = emit_c_type_for(e, f.ty); qoz_string fname = f.name; qoz_string _qoz_bv_241;
    {
        qoz_Strbuf _qoz_sb_3602_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3602_17); strings_sb_append(&_qoz_sb_3602_17, QOZ_STR_LIT("    ")); strings_sb_append(&_qoz_sb_3602_17, ct); strings_sb_append(&_qoz_sb_3602_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_3602_17, fname); strings_sb_append(&_qoz_sb_3602_17, QOZ_STR_LIT(";\n")); _qoz_bv_241 = strings_sb_finish(&_qoz_sb_3602_17);
    }
    emit_push(e, _qoz_bv_241); } }emit_push(e, QOZ_STR_LIT("};\n\n")); emit_emit_record_eq(e, name, fields); emit_emit_record_desc(e, name, fields); 
    return;
}

void emit_emit_record_eq(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_StructField fields) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string _qoz_bv_242;
    {
        qoz_Strbuf _qoz_sb_3610_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3610_13); strings_sb_append(&_qoz_sb_3610_13, QOZ_STR_LIT("static bool qoz_eq_")); strings_sb_append(&_qoz_sb_3610_13, name); strings_sb_append(&_qoz_sb_3610_13, QOZ_STR_LIT("(qoz_")); strings_sb_append(&_qoz_sb_3610_13, name); strings_sb_append(&_qoz_sb_3610_13, QOZ_STR_LIT(" a, qoz_")); strings_sb_append(&_qoz_sb_3610_13, name); strings_sb_append(&_qoz_sb_3610_13, QOZ_STR_LIT(" b) {\n    return ")); _qoz_bv_242 = strings_sb_finish(&_qoz_sb_3610_13);
    }
    emit_push(e, _qoz_bv_242); if ((fields.len) == 0) { emit_push(e, QOZ_STR_LIT("true")); }  else { int64_t i = 0; { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; if (i > 0) { emit_push(e, QOZ_STR_LIT(" && ")); } emit_emit_field_eq_expr(e, f.ty, f.name); i = i + 1; } }} emit_push(e, QOZ_STR_LIT(";\n}\n\n")); emit_emit_record_hash(e, name, fields); 
    return;
}

void emit_emit_record_hash(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_StructField fields) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string _qoz_bv_243;
    {
        qoz_Strbuf _qoz_sb_3626_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3626_13); strings_sb_append(&_qoz_sb_3626_13, QOZ_STR_LIT("static uint64_t qoz_hash_")); strings_sb_append(&_qoz_sb_3626_13, name); strings_sb_append(&_qoz_sb_3626_13, QOZ_STR_LIT("(qoz_")); strings_sb_append(&_qoz_sb_3626_13, name); strings_sb_append(&_qoz_sb_3626_13, QOZ_STR_LIT(" v) {\n    uint64_t h = 0;\n")); _qoz_bv_243 = strings_sb_finish(&_qoz_sb_3626_13);
    }
    emit_push(e, _qoz_bv_243); { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField f = __col.data[__i]; (void)f; emit_push(e, QOZ_STR_LIT("    h = h * 31 + (uint64_t)(")); emit_emit_field_hash_expr(e, f.ty, f.name); emit_push(e, QOZ_STR_LIT(");\n")); } }emit_push(e, QOZ_STR_LIT("    return h;\n}\n\n")); 
    return;
}

void emit_emit_field_hash_expr(qoz_Emitter* e, qoz_TypeExpr* te, qoz_string fname) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_string ct = emit_c_type_for(e, te); if (qoz_string_eq(ct, QOZ_STR_LIT("qoz_string"))) { qoz_string _qoz_bv_244;
    {
        qoz_Strbuf _qoz_sb_3638_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3638_17); strings_sb_append(&_qoz_sb_3638_17, QOZ_STR_LIT("qoz_string_hash(v.")); strings_sb_append(&_qoz_sb_3638_17, fname); strings_sb_append(&_qoz_sb_3638_17, QOZ_STR_LIT(")")); _qoz_bv_244 = strings_sb_finish(&_qoz_sb_3638_17);
    }
    emit_push(e, _qoz_bv_244); return;} qoz_TypeExpr* _qoz_ms_267 = te; switch (_qoz_ms_267->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_267->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_267->payload.TENamed.f2; if ((path.len) >= 1) { qoz_string n = path.data[(path.len) - 1]; if (qoz_string_eq(emit_primitive_c_name(n), QOZ_STR_LIT(""))) { if (!qoz_map_contains__qoz_string__bool(&e->is_enum, n)) { qoz_string mangled = (((args.len) > 0) ? emit_mangle_inst(e, n, args) : n); qoz_string _qoz_bv_245;
    {
        qoz_Strbuf _qoz_sb_3648_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3648_29); strings_sb_append(&_qoz_sb_3648_29, QOZ_STR_LIT("qoz_hash_")); strings_sb_append(&_qoz_sb_3648_29, mangled); strings_sb_append(&_qoz_sb_3648_29, QOZ_STR_LIT("(v.")); strings_sb_append(&_qoz_sb_3648_29, fname); strings_sb_append(&_qoz_sb_3648_29, QOZ_STR_LIT(")")); _qoz_bv_245 = strings_sb_finish(&_qoz_sb_3648_29);
    }
    emit_push(e, _qoz_bv_245); return;} } } 0;  break; } default: { NULL;  break; } } 0; qoz_string _qoz_bv_246;
    {
        qoz_Strbuf _qoz_sb_3656_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3656_13); strings_sb_append(&_qoz_sb_3656_13, QOZ_STR_LIT("v.")); strings_sb_append(&_qoz_sb_3656_13, fname); _qoz_bv_246 = strings_sb_finish(&_qoz_sb_3656_13);
    }
    emit_push(e, _qoz_bv_246); 
    return;
}

void emit_emit_field_eq_expr(qoz_Emitter* e, qoz_TypeExpr* te, qoz_string fname) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_string ct = emit_c_type_for(e, te); if (qoz_string_eq(ct, QOZ_STR_LIT("qoz_string"))) { qoz_string _qoz_bv_247;
    {
        qoz_Strbuf _qoz_sb_3662_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3662_17); strings_sb_append(&_qoz_sb_3662_17, QOZ_STR_LIT("qoz_string_eq(a.")); strings_sb_append(&_qoz_sb_3662_17, fname); strings_sb_append(&_qoz_sb_3662_17, QOZ_STR_LIT(", b.")); strings_sb_append(&_qoz_sb_3662_17, fname); strings_sb_append(&_qoz_sb_3662_17, QOZ_STR_LIT(")")); _qoz_bv_247 = strings_sb_finish(&_qoz_sb_3662_17);
    }
    emit_push(e, _qoz_bv_247); return;} qoz_TypeExpr* _qoz_ms_268 = te; switch (_qoz_ms_268->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_268->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_268->payload.TENamed.f2; if ((path.len) >= 1) { qoz_string n = path.data[(path.len) - 1]; if (qoz_string_eq(emit_primitive_c_name(n), QOZ_STR_LIT(""))) { if (!qoz_map_contains__qoz_string__bool(&e->is_enum, n)) { qoz_string mangled = (((args.len) > 0) ? emit_mangle_inst(e, n, args) : n); qoz_string _qoz_bv_248;
    {
        qoz_Strbuf _qoz_sb_3672_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3672_29); strings_sb_append(&_qoz_sb_3672_29, QOZ_STR_LIT("qoz_eq_")); strings_sb_append(&_qoz_sb_3672_29, mangled); strings_sb_append(&_qoz_sb_3672_29, QOZ_STR_LIT("(a.")); strings_sb_append(&_qoz_sb_3672_29, fname); strings_sb_append(&_qoz_sb_3672_29, QOZ_STR_LIT(", b.")); strings_sb_append(&_qoz_sb_3672_29, fname); strings_sb_append(&_qoz_sb_3672_29, QOZ_STR_LIT(")")); _qoz_bv_248 = strings_sb_finish(&_qoz_sb_3672_29);
    }
    emit_push(e, _qoz_bv_248); return;} } } 0;  break; } default: { NULL;  break; } } 0; qoz_string _qoz_bv_249;
    {
        qoz_Strbuf _qoz_sb_3680_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3680_13); strings_sb_append(&_qoz_sb_3680_13, QOZ_STR_LIT("a.")); strings_sb_append(&_qoz_sb_3680_13, fname); strings_sb_append(&_qoz_sb_3680_13, QOZ_STR_LIT(" == b.")); strings_sb_append(&_qoz_sb_3680_13, fname); _qoz_bv_249 = strings_sb_finish(&_qoz_sb_3680_13);
    }
    emit_push(e, _qoz_bv_249); 
    return;
}

void emit_emit_fn_proto(qoz_Emitter* e, qoz_string name, qoz_Vec__qoz_FnParam params, qoz_TypeExpr* ret) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ret);
    qoz_string rc = emit_c_type_for(e, ret); qoz_string _qoz_bv_250;
    {
        qoz_Strbuf _qoz_sb_3685_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3685_13); strings_sb_append(&_qoz_sb_3685_13, rc); strings_sb_append(&_qoz_sb_3685_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_3685_13, name); _qoz_bv_250 = strings_sb_finish(&_qoz_sb_3685_13);
    }
    emit_push(e, _qoz_bv_250); if ((params.len) == 0) { emit_push(e, QOZ_STR_LIT("(void);\n")); return;} emit_push(e, QOZ_STR_LIT("(")); int64_t i = 0; { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam p = __col.data[__i]; (void)p; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } qoz_string pc = emit_c_type_for(e, p.ty); qoz_string pn = p.name; qoz_string _qoz_bv_251;
    {
        qoz_Strbuf _qoz_sb_3696_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3696_17); strings_sb_append(&_qoz_sb_3696_17, pc); strings_sb_append(&_qoz_sb_3696_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_3696_17, pn); _qoz_bv_251 = strings_sb_finish(&_qoz_sb_3696_17);
    }
    emit_push(e, _qoz_bv_251); i = i + 1; } }emit_push(e, QOZ_STR_LIT(");\n")); 
    return;
}

void emit_emit_extern_proto(qoz_Emitter* e, qoz_string symbol, qoz_Vec__qoz_FnParam params, qoz_TypeExpr* ret) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ret);
    qoz_string rc = emit_c_type_for(e, ret); qoz_string _qoz_bv_252;
    {
        qoz_Strbuf _qoz_sb_3704_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3704_13); strings_sb_append(&_qoz_sb_3704_13, QOZ_STR_LIT("extern ")); strings_sb_append(&_qoz_sb_3704_13, rc); strings_sb_append(&_qoz_sb_3704_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_3704_13, symbol); _qoz_bv_252 = strings_sb_finish(&_qoz_sb_3704_13);
    }
    emit_push(e, _qoz_bv_252); if ((params.len) == 0) { emit_push(e, QOZ_STR_LIT("(void);\n")); return;} emit_push(e, QOZ_STR_LIT("(")); int64_t i = 0; { qoz_Vec__qoz_FnParam __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_FnParam p = __col.data[__i]; (void)p; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } emit_push(e, emit_c_type_for(e, p.ty)); i = i + 1; } }emit_push(e, QOZ_STR_LIT(");\n")); 
    return;
}

bool emit_is_range_op(qoz_BinaryOp* op) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&op);
    qoz_BinaryOp* _qoz_ms_269 = op; bool _qoz_mv_269 = false; switch (_qoz_ms_269->tag) { case qoz_BinaryOp_BOpRange: { _qoz_mv_269 = (true);  break; } case qoz_BinaryOp_BOpRangeInclusive: { _qoz_mv_269 = (true);  break; } default: { _qoz_mv_269 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_269;
}

qoz_string emit_range_cmp_op(qoz_BinaryOp* op) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&op);
    qoz_BinaryOp* _qoz_ms_270 = op; qoz_string _qoz_mv_270 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_270->tag) { case qoz_BinaryOp_BOpRangeInclusive: { _qoz_mv_270 = (QOZ_STR_LIT("<="));  break; } default: { _qoz_mv_270 = (QOZ_STR_LIT("<"));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_270;
}

void emit_emit_block_tail_as_value(qoz_Emitter* e, qoz_Expr* tail) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&tail);
    qoz_Expr* _qoz_ms_271 = tail; switch (_qoz_ms_271->tag) { case qoz_Expr_EAssign: { qoz_Expr* lhs = _qoz_ms_271->payload.EAssign.f2; qoz_Expr* rhs = _qoz_ms_271->payload.EAssign.f3; {
        emit_push(e, QOZ_STR_LIT("(")); emit_emit_assign(e, lhs, rhs); emit_push(e, QOZ_STR_LIT(", 0)")); 
    }
    0;  break; } case qoz_Expr_EWhile: { {
        int64_t start = strings_sb_len(&e->out); emit_emit_stmt_expr(e, tail); emit_hoist_to_prologue(e, start); emit_push(e, QOZ_STR_LIT("0")); 
    }
    0;  break; } case qoz_Expr_EFor: { {
        int64_t start = strings_sb_len(&e->out); emit_emit_stmt_expr(e, tail); emit_hoist_to_prologue(e, start); emit_push(e, QOZ_STR_LIT("0")); 
    }
    0;  break; } case qoz_Expr_EDefer: { {
        int64_t start = strings_sb_len(&e->out); emit_emit_stmt_expr(e, tail); emit_hoist_to_prologue(e, start); emit_push(e, QOZ_STR_LIT("0")); 
    }
    0;  break; } case qoz_Expr_EIf: { qoz_Expr* f = _qoz_ms_271->payload.EIf.f3; {
        bool unit_if = emit_is_nil_expr(f) || emit_is_unit_typeexpr(emit_infer_value_te(e, tail)); if (unit_if) { int64_t start = strings_sb_len(&e->out); emit_emit_stmt_expr(e, tail); emit_hoist_to_prologue(e, start); emit_push(e, QOZ_STR_LIT("0")); }  else { emit_emit_expr(e, tail); } 
    }
    0;  break; } default: { emit_emit_expr(e, tail);  break; } } 0; 
    return;
}

bool emit_hint_is_cstring(qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&hint);
    qoz_TypeExpr* _qoz_ms_272 = hint; bool _qoz_mv_272 = false; switch (_qoz_ms_272->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_272->payload.TENamed.f1; _qoz_mv_272 = (((path.len) == 1) && qoz_string_eq(path.data[0], QOZ_STR_LIT("cstring")));  break; } default: { _qoz_mv_272 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_272;
}

void emit_emit_value_with_hint(qoz_Emitter* e, qoz_Expr* value, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&value);
    qoz_gc_push_root(&hint);
    qoz_Expr* _qoz_ms_273 = value; switch (_qoz_ms_273->tag) { case qoz_Expr_EString: { qoz_string raw = _qoz_ms_273->payload.EString.f1; {
        if (emit_hint_is_cstring(hint)) { emit_push(e, raw); }  else { qoz_string _qoz_bv_253;
    {
        qoz_Strbuf _qoz_sb_3786_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3786_21); strings_sb_append(&_qoz_sb_3786_21, QOZ_STR_LIT("QOZ_STR_LIT(")); strings_sb_append(&_qoz_sb_3786_21, raw); strings_sb_append(&_qoz_sb_3786_21, QOZ_STR_LIT(")")); _qoz_bv_253 = strings_sb_finish(&_qoz_sb_3786_21);
    }
    emit_push(e, _qoz_bv_253); } return;
    }
    0;  break; } default: { NULL;  break; } } 0; qoz_Expr* _qoz_ms_274 = value; switch (_qoz_ms_274->tag) { case qoz_Expr_ERecord: { qoz_TypeExpr* te = _qoz_ms_274->payload.ERecord.f1; qoz_Vec__qoz_RecordFieldLit fields = _qoz_ms_274->payload.ERecord.f2; emit_emit_record_lit_with_hint(e, te, fields, hint);  break; } case qoz_Expr_ECall: { qoz_Expr* callee = _qoz_ms_274->payload.ECall.f1; qoz_Vec__qoz_TypeExpr ta = _qoz_ms_274->payload.ECall.f2; qoz_Vec__qoz_Expr args = _qoz_ms_274->payload.ECall.f3; emit_emit_call_with_hint(e, callee, ta, args, hint);  break; } case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_274->payload.EIdent.f1; emit_emit_ident_with_hint(e, name, hint);  break; } case qoz_Expr_EArrayLit: { qoz_Span sp = _qoz_ms_274->payload.EArrayLit.f0; qoz_Vec__qoz_Expr elems = _qoz_ms_274->payload.EArrayLit.f1; emit_emit_array_lit_with_hint(e, sp, elems, hint);  break; } case qoz_Expr_EBlock: { qoz_Span sp = _qoz_ms_274->payload.EBlock.f0; qoz_Vec__qoz_Stmt stmts = _qoz_ms_274->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_274->payload.EBlock.f2; {
        if ((stmts.len) == 0) { qoz_Expr* _qoz_ms_275 = tail; switch (_qoz_ms_275->tag) { case qoz_Expr_EWhile: { {
        emit_push(e, QOZ_STR_LIT("((")); emit_emit_stmt_expr(e, tail); emit_push(e, QOZ_STR_LIT("), 0)")); 
    }
    0;  break; } case qoz_Expr_EFor: { {
        emit_push(e, QOZ_STR_LIT("((")); emit_emit_stmt_expr(e, tail); emit_push(e, QOZ_STR_LIT("), 0)")); 
    }
    0;  break; } case qoz_Expr_EAssign: { {
        emit_push(e, QOZ_STR_LIT("((")); emit_emit_stmt_expr(e, tail); emit_push(e, QOZ_STR_LIT("), 0)")); 
    }
    0;  break; } default: { emit_emit_value_with_hint(e, tail, hint);  break; } } 0; return;} e->closure_counter = e->closure_counter + 1; int64_t counter = e->closure_counter; qoz_string _qoz_bv_254;
    {
        qoz_Strbuf _qoz_sb_3818_47 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3818_47); strings_sb_append_i64(&_qoz_sb_3818_47, counter); _qoz_bv_254 = strings_sb_finish(&_qoz_sb_3818_47);
    }
    qoz_string tmp_res = qoz_string_cat(QOZ_STR_LIT("_qoz_bv_"), _qoz_bv_254); qoz_string result_c = emit_c_type_for(e, hint); int64_t start = strings_sb_len(&e->out); qoz_string _qoz_bv_255;
    {
        qoz_Strbuf _qoz_sb_3821_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3821_17); strings_sb_append(&_qoz_sb_3821_17, result_c); strings_sb_append(&_qoz_sb_3821_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_3821_17, tmp_res); strings_sb_append(&_qoz_sb_3821_17, QOZ_STR_LIT(";\n    {\n        ")); _qoz_bv_255 = strings_sb_finish(&_qoz_sb_3821_17);
    }
    emit_push(e, _qoz_bv_255); { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }qoz_Expr* _qoz_ms_276 = tail; switch (_qoz_ms_276->tag) { case qoz_Expr_EWhile: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); qoz_string _qoz_bv_256;
    {
        qoz_Strbuf _qoz_sb_3828_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3828_21); strings_sb_append(&_qoz_sb_3828_21, QOZ_STR_LIT("\n        ")); strings_sb_append(&_qoz_sb_3828_21, tmp_res); strings_sb_append(&_qoz_sb_3828_21, QOZ_STR_LIT(" = 0;\n    ")); _qoz_bv_256 = strings_sb_finish(&_qoz_sb_3828_21);
    }
    emit_push(e, _qoz_bv_256); 
    }
    0;  break; } case qoz_Expr_EFor: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); qoz_string _qoz_bv_257;
    {
        qoz_Strbuf _qoz_sb_3828_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3828_21); strings_sb_append(&_qoz_sb_3828_21, QOZ_STR_LIT("\n        ")); strings_sb_append(&_qoz_sb_3828_21, tmp_res); strings_sb_append(&_qoz_sb_3828_21, QOZ_STR_LIT(" = 0;\n    ")); _qoz_bv_257 = strings_sb_finish(&_qoz_sb_3828_21);
    }
    emit_push(e, _qoz_bv_257); 
    }
    0;  break; } case qoz_Expr_EAssign: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); qoz_string _qoz_bv_258;
    {
        qoz_Strbuf _qoz_sb_3828_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3828_21); strings_sb_append(&_qoz_sb_3828_21, QOZ_STR_LIT("\n        ")); strings_sb_append(&_qoz_sb_3828_21, tmp_res); strings_sb_append(&_qoz_sb_3828_21, QOZ_STR_LIT(" = 0;\n    ")); _qoz_bv_258 = strings_sb_finish(&_qoz_sb_3828_21);
    }
    emit_push(e, _qoz_bv_258); 
    }
    0;  break; } default: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); qoz_string _qoz_bv_259;
    {
        qoz_Strbuf _qoz_sb_3832_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3832_21); strings_sb_append(&_qoz_sb_3832_21, tmp_res); strings_sb_append(&_qoz_sb_3832_21, QOZ_STR_LIT(" = ")); _qoz_bv_259 = strings_sb_finish(&_qoz_sb_3832_21);
    }
    emit_push(e, _qoz_bv_259); emit_emit_value_with_hint(e, tail, hint); emit_push(e, QOZ_STR_LIT(";\n    ")); emit_close_statement_scope(e, saved); 
    }
    0;  break; } } 0; emit_push(e, QOZ_STR_LIT("}\n    ")); emit_hoist_to_prologue(e, start); emit_push(e, tmp_res); 
    }
    0;  break; } case qoz_Expr_EIf: { qoz_Span sp = _qoz_ms_274->payload.EIf.f0; qoz_Expr* c = _qoz_ms_274->payload.EIf.f1; qoz_Expr* t = _qoz_ms_274->payload.EIf.f2; qoz_Expr* f = _qoz_ms_274->payload.EIf.f3; {
        emit_push(e, QOZ_STR_LIT("(")); emit_emit_cond(e, c); emit_push(e, QOZ_STR_LIT(" ? ")); emit_emit_value_with_hint(e, t, hint); emit_push(e, QOZ_STR_LIT(" : ")); emit_emit_value_with_hint(e, f, hint); emit_push(e, QOZ_STR_LIT(")")); 
    }
    0;  break; } case qoz_Expr_EMatch: { qoz_Span sp = _qoz_ms_274->payload.EMatch.f0; qoz_Expr* scrut = _qoz_ms_274->payload.EMatch.f1; qoz_Vec__qoz_MatchArm arms = _qoz_ms_274->payload.EMatch.f2; emit_emit_match_as_expr_with_hint(e, sp, scrut, arms, hint);  break; } default: { emit_emit_expr(e, value);  break; } } 0; 
    return;
}

void emit_emit_match_as_expr_with_hint(qoz_Emitter* e, qoz_Span sp, qoz_Expr* scrut, qoz_Vec__qoz_MatchArm arms, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&scrut);
    qoz_gc_push_root(&hint);
    qoz_TypeExpr* saved_hint = e->match_hint; qoz_gc_push_root(&saved_hint); e->match_hint = hint; emit_emit_match_as_expr(e, sp, scrut, arms); e->match_hint = saved_hint; 
    return;
}

void emit_emit_ident_with_hint(qoz_Emitter* e, qoz_string name, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&hint);
    qoz_Option__qoz_string* _qoz_ms_277 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); switch (_qoz_ms_277->tag) { case qoz_Option__qoz_string_Some: { qoz_string enum_name = _qoz_ms_277->payload.Some.f0; {
        qoz_Vec__qoz_TypeExpr hint_args = emit_hint_args_for_enum(hint, enum_name); if ((hint_args.len) > 0) { qoz_string mangled = emit_mangle_inst(e, enum_name, hint_args); qoz_string _qoz_bv_260;
    {
        qoz_Strbuf _qoz_sb_3869_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3869_21); strings_sb_append(&_qoz_sb_3869_21, QOZ_STR_LIT("qoz_make_")); strings_sb_append(&_qoz_sb_3869_21, mangled); strings_sb_append(&_qoz_sb_3869_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3869_21, name); strings_sb_append(&_qoz_sb_3869_21, QOZ_STR_LIT("()")); _qoz_bv_260 = strings_sb_finish(&_qoz_sb_3869_21);
    }
    emit_push(e, _qoz_bv_260); return;} qoz_string _qoz_bv_261;
    {
        qoz_Strbuf _qoz_sb_3872_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3872_17); strings_sb_append(&_qoz_sb_3872_17, QOZ_STR_LIT("qoz_make_")); strings_sb_append(&_qoz_sb_3872_17, enum_name); strings_sb_append(&_qoz_sb_3872_17, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3872_17, name); strings_sb_append(&_qoz_sb_3872_17, QOZ_STR_LIT("()")); _qoz_bv_261 = strings_sb_finish(&_qoz_sb_3872_17);
    }
    emit_push(e, _qoz_bv_261); 
    }
    0;  break; } case qoz_Option__qoz_string_None: { {
        qoz_TypeExpr* _qoz_ms_278 = hint; switch (_qoz_ms_278->tag) { case qoz_TypeExpr_TEFn: { qoz_Vec__qoz_TypeExpr fn_ps = _qoz_ms_278->payload.TEFn.f1; qoz_TypeExpr* fn_ret = _qoz_ms_278->payload.TEFn.f2; if (qoz_map_contains__qoz_string__qoz_Vec__qoz_TypeExpr(&e->fn_params, name)) { if (!qoz_map_contains__qoz_string__qoz_TypeExpr(&e->locals, name)) { qoz_string thunk = emit_register_fn_thunk(e, name, fn_ps, fn_ret); qoz_string clo_t = emit_c_type_for(e, hint); qoz_string _qoz_bv_262;
    {
        qoz_Strbuf _qoz_sb_3883_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3883_29); strings_sb_append(&_qoz_sb_3883_29, QOZ_STR_LIT("((")); strings_sb_append(&_qoz_sb_3883_29, clo_t); strings_sb_append(&_qoz_sb_3883_29, QOZ_STR_LIT("){ .env = NULL, .fn = ")); strings_sb_append(&_qoz_sb_3883_29, thunk); strings_sb_append(&_qoz_sb_3883_29, QOZ_STR_LIT(" })")); _qoz_bv_262 = strings_sb_finish(&_qoz_sb_3883_29);
    }
    emit_push(e, _qoz_bv_262); return;} } 0;  break; } default: { NULL;  break; } } 0; emit_push(e, name); 
    }
    0;  break; } } 0; 
    return;
}

qoz_string emit_register_fn_thunk(qoz_Emitter* e, qoz_string fn_name, qoz_Vec__qoz_TypeExpr params, qoz_TypeExpr* ret) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ret);
    qoz_string key = fn_name; key = qoz_string_cat(key, QOZ_STR_LIT("__thunk")); if (qoz_map_contains__qoz_string__qoz_string(&e->fn_typedefs, key)) { return key;} qoz_map_set__qoz_string__qoz_string(&e->fn_typedefs, key, QOZ_STR_LIT("")); qoz_string sig = QOZ_STR_LIT("static "); sig = qoz_string_cat(sig, emit_c_type_for(e, ret)); sig = qoz_string_cat(sig, QOZ_STR_LIT(" ")); sig = qoz_string_cat(sig, key); sig = qoz_string_cat(sig, QOZ_STR_LIT("(void *_qoz_env")); int64_t pi = 0; { qoz_Vec__qoz_TypeExpr __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* p = __col.data[__i]; (void)p; sig = qoz_string_cat(sig, QOZ_STR_LIT(", ")); sig = qoz_string_cat(sig, emit_c_type_for(e, p)); sig = qoz_string_cat(sig, QOZ_STR_LIT(" _p")); sig = qoz_string_cat(sig, emit_int_to_string(pi)); pi = pi + 1; } }qoz_string sig_decl = qoz_string_cat(sig, QOZ_STR_LIT(");\n")); qoz_vec_push__qoz_string(&e->synth_fn_decls, sig_decl); qoz_string def = sig; def = qoz_string_cat(def, QOZ_STR_LIT(") {\n    (void)_qoz_env;\n    ")); if (emit_is_unit_typeexpr(ret)) { def = qoz_string_cat(def, fn_name); def = qoz_string_cat(def, QOZ_STR_LIT("(")); int64_t pj = 0; { qoz_Vec__qoz_TypeExpr __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* _p = __col.data[__i]; (void)_p; if (pj > 0) { def = qoz_string_cat(def, QOZ_STR_LIT(", ")); } def = qoz_string_cat(def, QOZ_STR_LIT("_p")); def = qoz_string_cat(def, emit_int_to_string(pj)); pj = pj + 1; } }def = qoz_string_cat(def, QOZ_STR_LIT(");\n    return;\n")); }  else { def = qoz_string_cat(def, QOZ_STR_LIT("return ")); def = qoz_string_cat(def, fn_name); def = qoz_string_cat(def, QOZ_STR_LIT("(")); int64_t pj = 0; { qoz_Vec__qoz_TypeExpr __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_TypeExpr* _p = __col.data[__i]; (void)_p; if (pj > 0) { def = qoz_string_cat(def, QOZ_STR_LIT(", ")); } def = qoz_string_cat(def, QOZ_STR_LIT("_p")); def = qoz_string_cat(def, emit_int_to_string(pj)); pj = pj + 1; } }def = qoz_string_cat(def, QOZ_STR_LIT(");\n")); } def = qoz_string_cat(def, QOZ_STR_LIT("}\n\n")); qoz_vec_push__qoz_string(&e->synth_fn_defs, def); qoz_gc_shadow_set_top(_qoz_shadow_guard); return key;
}

void emit_emit_call_with_hint(qoz_Emitter* e, qoz_Expr* callee, qoz_Vec__qoz_TypeExpr type_args, qoz_Vec__qoz_Expr args, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&callee);
    qoz_gc_push_root(&hint);
    if ((type_args.len) > 0) { emit_emit_call(e, callee, type_args, args); return;} qoz_Expr* _qoz_ms_279 = callee; switch (_qoz_ms_279->tag) { case qoz_Expr_EIdent: { qoz_string name = _qoz_ms_279->payload.EIdent.f1; qoz_Option__qoz_string* _qoz_ms_280 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); switch (_qoz_ms_280->tag) { case qoz_Option__qoz_string_Some: { qoz_string enum_name = _qoz_ms_280->payload.Some.f0; {
        qoz_Vec__qoz_TypeExpr hint_args = emit_hint_args_for_enum(hint, enum_name); if ((hint_args.len) > 0) { qoz_string mangled = emit_mangle_inst(e, enum_name, hint_args); qoz_string _qoz_bv_263;
    {
        qoz_Strbuf _qoz_sb_3968_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_3968_25); strings_sb_append(&_qoz_sb_3968_25, QOZ_STR_LIT("qoz_make_")); strings_sb_append(&_qoz_sb_3968_25, mangled); strings_sb_append(&_qoz_sb_3968_25, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_3968_25, name); strings_sb_append(&_qoz_sb_3968_25, QOZ_STR_LIT("(")); _qoz_bv_263 = strings_sb_finish(&_qoz_sb_3968_25);
    }
    emit_push(e, _qoz_bv_263); int64_t i = 0; { qoz_Vec__qoz_Expr __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Expr* a = __col.data[__i]; (void)a; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } emit_emit_expr(e, a); i = i + 1; } }emit_push(e, QOZ_STR_LIT(")")); return;} 
    }
    0;  break; } case qoz_Option__qoz_string_None: { NULL;  break; } } 0;  break; } default: { NULL;  break; } } 0; emit_emit_call(e, callee, type_args, args); 
    return;
}

qoz_Vec__qoz_TypeExpr emit_hint_args_for_enum(qoz_TypeExpr* hint, qoz_string enum_name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&hint);
    qoz_TypeExpr* _qoz_ms_281 = hint; qoz_Vec__qoz_TypeExpr _qoz_mv_281 = ((qoz_Vec__qoz_TypeExpr){0}); switch (_qoz_ms_281->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_281->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_281->payload.TENamed.f2; qoz_Vec__qoz_TypeExpr _qoz_bv_264;
    {
        if ((path.len) >= 1) { qoz_string last = path.data[(path.len) - 1]; if (qoz_string_eq(last, enum_name)) { return args;} } _qoz_bv_264 = qoz_vec_make__qoz_TypeExpr();
    }
    _qoz_mv_281 = (_qoz_bv_264);  break; } default: { _qoz_mv_281 = (qoz_vec_make__qoz_TypeExpr());  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_281;
}

void emit_emit_record_lit_with_hint(qoz_Emitter* e, qoz_TypeExpr* te, qoz_Vec__qoz_RecordFieldLit fields, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_gc_push_root(&hint);
    qoz_TypeExpr* effective = emit_pick_type_with_args(te, hint); qoz_gc_push_root(&effective); qoz_string ec = emit_c_type_for(e, effective); qoz_string _qoz_bv_265;
    {
        qoz_Strbuf _qoz_sb_4003_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4003_13); strings_sb_append(&_qoz_sb_4003_13, QOZ_STR_LIT("((")); strings_sb_append(&_qoz_sb_4003_13, ec); strings_sb_append(&_qoz_sb_4003_13, QOZ_STR_LIT("){ ")); _qoz_bv_265 = strings_sb_finish(&_qoz_sb_4003_13);
    }
    emit_push(e, _qoz_bv_265); int64_t i = 0; { qoz_Vec__qoz_RecordFieldLit __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_RecordFieldLit f = __col.data[__i]; (void)f; if (i > 0) { emit_push(e, QOZ_STR_LIT(", ")); } qoz_string fn = f.name; qoz_string _qoz_bv_266;
    {
        qoz_Strbuf _qoz_sb_4008_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4008_17); strings_sb_append(&_qoz_sb_4008_17, QOZ_STR_LIT(".")); strings_sb_append(&_qoz_sb_4008_17, fn); strings_sb_append(&_qoz_sb_4008_17, QOZ_STR_LIT(" = ")); _qoz_bv_266 = strings_sb_finish(&_qoz_sb_4008_17);
    }
    emit_push(e, _qoz_bv_266); qoz_TypeExpr* field_hint = emit_field_type_hint(e, effective, f.name); qoz_gc_push_root(&field_hint); emit_emit_value_with_hint(e, f.value, field_hint); i = i + 1; } }emit_push(e, QOZ_STR_LIT(" })")); 
    return;
}

qoz_string emit_type_lookup_key(qoz_Emitter* e, qoz_Vec__qoz_string path) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    if ((path.len) == 1) { return path.data[0];} if ((path.len) == 2) { qoz_string qualified = qoz_string_cat(qoz_string_cat(path.data[0], QOZ_STR_LIT("_")), path.data[1]); if (qoz_map_contains__qoz_string__qoz_Decl(&e->generic_decls, qualified)) { return qualified;} return path.data[1];} qoz_gc_shadow_set_top(_qoz_shadow_guard); return path.data[(path.len) - 1];
}

qoz_TypeExpr* emit_field_type_hint(qoz_Emitter* e, qoz_TypeExpr* te, qoz_string field_name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_282 = te; qoz_TypeExpr* _qoz_mv_282 = NULL; switch (_qoz_ms_282->tag) { case qoz_TypeExpr_TENamed: { qoz_Span sp = _qoz_ms_282->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_282->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_282->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_267;
    {
        if ((path.len) < 1) { return te;} qoz_string name = emit_type_lookup_key(e, path); if (!qoz_map_contains__qoz_string__qoz_Decl(&e->generic_decls, name)) { return te;} qoz_Option__qoz_Decl* _qoz_ms_283 = qoz_map_get__qoz_string__qoz_Decl(&e->generic_decls, name); qoz_TypeExpr* _qoz_mv_283 = NULL; switch (_qoz_ms_283->tag) { case qoz_Option__qoz_Decl_Some: { qoz_Decl* decl = _qoz_ms_283->payload.Some.f0; qoz_Decl* _qoz_ms_284 = decl; qoz_TypeExpr* _qoz_mv_284 = NULL; switch (_qoz_ms_284->tag) { case qoz_Decl_DStruct: { qoz_Vec__qoz_string params = _qoz_ms_284->payload.DStruct.f2; qoz_Vec__qoz_StructField fields = _qoz_ms_284->payload.DStruct.f3; qoz_TypeExpr* _qoz_bv_268;
    {
        { qoz_Vec__qoz_StructField __col = fields; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_StructField sf = __col.data[__i]; (void)sf; if (qoz_string_eq(sf.name, field_name)) { if ((args.len) == (params.len)) { return emit_substitute_type(e, sf.ty, params, args);} return sf.ty;} } }_qoz_bv_268 = te;
    }
    _qoz_mv_284 = (_qoz_bv_268);  break; } default: { _qoz_mv_284 = (te);  break; } } _qoz_mv_283 = (_qoz_mv_284);  break; } case qoz_Option__qoz_Decl_None: { _qoz_mv_283 = (te);  break; } } _qoz_bv_267 = _qoz_mv_283;
    }
    _qoz_mv_282 = (_qoz_bv_267);  break; } default: { _qoz_mv_282 = (te);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_282;
}

qoz_TypeExpr* emit_pick_type_with_args(qoz_TypeExpr* te, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&te);
    qoz_gc_push_root(&hint);
    qoz_TypeExpr* _qoz_ms_285 = te; qoz_TypeExpr* _qoz_mv_285 = NULL; switch (_qoz_ms_285->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string te_path = _qoz_ms_285->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr te_args = _qoz_ms_285->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_269;
    {
        if ((te_args.len) > 0) { return te;} qoz_TypeExpr* _qoz_ms_286 = hint; qoz_TypeExpr* _qoz_mv_286 = NULL; switch (_qoz_ms_286->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string h_path = _qoz_ms_286->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr h_args = _qoz_ms_286->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_270;
    {
        if ((h_args.len) == 0) { return te;} if ((((h_path.len) == 1) && ((te_path.len) == 1)) && qoz_string_eq(h_path.data[0], te_path.data[0])) { return hint;} _qoz_bv_270 = te;
    }
    _qoz_mv_286 = (_qoz_bv_270);  break; } default: { _qoz_mv_286 = (te);  break; } } _qoz_bv_269 = _qoz_mv_286;
    }
    _qoz_mv_285 = (_qoz_bv_269);  break; } default: { _qoz_mv_285 = (te);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_285;
}

void emit_emit_for_loop_one(qoz_Emitter* e, qoz_string binding, qoz_string binding2, qoz_Expr* iter, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&iter);
    qoz_gc_push_root(&body);
    qoz_Expr* _qoz_ms_287 = iter; switch (_qoz_ms_287->tag) { case qoz_Expr_EBinary: { qoz_BinaryOp* op = _qoz_ms_287->payload.EBinary.f1; qoz_Expr* lo = _qoz_ms_287->payload.EBinary.f2; qoz_Expr* hi = _qoz_ms_287->payload.EBinary.f3; if (emit_is_range_op(op)) { qoz_string _qoz_bv_271;
    {
        qoz_Strbuf _qoz_sb_4084_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4084_21); strings_sb_append(&_qoz_sb_4084_21, QOZ_STR_LIT("for (int64_t ")); strings_sb_append(&_qoz_sb_4084_21, binding); strings_sb_append(&_qoz_sb_4084_21, QOZ_STR_LIT(" = ")); _qoz_bv_271 = strings_sb_finish(&_qoz_sb_4084_21);
    }
    emit_push(e, _qoz_bv_271); emit_emit_expr(e, lo); qoz_string cmp = emit_range_cmp_op(op); qoz_string _qoz_bv_272;
    {
        qoz_Strbuf _qoz_sb_4087_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4087_21); strings_sb_append(&_qoz_sb_4087_21, QOZ_STR_LIT("; ")); strings_sb_append(&_qoz_sb_4087_21, binding); strings_sb_append(&_qoz_sb_4087_21, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4087_21, cmp); strings_sb_append(&_qoz_sb_4087_21, QOZ_STR_LIT(" ")); _qoz_bv_272 = strings_sb_finish(&_qoz_sb_4087_21);
    }
    emit_push(e, _qoz_bv_272); emit_emit_expr(e, hi); qoz_string _qoz_bv_273;
    {
        qoz_Strbuf _qoz_sb_4089_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4089_21); strings_sb_append(&_qoz_sb_4089_21, QOZ_STR_LIT("; ")); strings_sb_append(&_qoz_sb_4089_21, binding); strings_sb_append(&_qoz_sb_4089_21, QOZ_STR_LIT("++) ")); _qoz_bv_273 = strings_sb_finish(&_qoz_sb_4089_21);
    }
    emit_push(e, _qoz_bv_273); emit_emit_branch_as_statement(e, body); return;} 0;  break; } default: { NULL;  break; } } 0; if (qoz_string_eq(binding2, QOZ_STR_LIT(""))) { emit_emit_vec_for(e, binding, iter, body); }  else { emit_emit_map_for(e, binding, binding2, iter, body); } 
    return;
}

void emit_emit_vec_for(qoz_Emitter* e, qoz_string binding, qoz_Expr* iter, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&iter);
    qoz_gc_push_root(&body);
    qoz_TypeExpr* elem_te = emit_vec_element_typeexpr(e, iter); qoz_gc_push_root(&elem_te); qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, binding, elem_te); qoz_string col_ct = emit_c_type_for(e, emit_infer_base_typeexpr(e, iter)); qoz_string elem_ct = emit_c_type_for(e, elem_te); qoz_string _qoz_bv_274;
    {
        qoz_Strbuf _qoz_sb_4109_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4109_13); strings_sb_append(&_qoz_sb_4109_13, QOZ_STR_LIT("{ ")); strings_sb_append(&_qoz_sb_4109_13, col_ct); strings_sb_append(&_qoz_sb_4109_13, QOZ_STR_LIT(" __col = ")); _qoz_bv_274 = strings_sb_finish(&_qoz_sb_4109_13);
    }
    emit_push(e, _qoz_bv_274); emit_emit_expr(e, iter); qoz_string _qoz_bv_275;
    {
        qoz_Strbuf _qoz_sb_4111_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4111_13); strings_sb_append(&_qoz_sb_4111_13, QOZ_STR_LIT("; for (int64_t __i = 0; __i < __col.len; __i++) { ")); strings_sb_append(&_qoz_sb_4111_13, elem_ct); strings_sb_append(&_qoz_sb_4111_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4111_13, binding); strings_sb_append(&_qoz_sb_4111_13, QOZ_STR_LIT(" = __col.data[__i]; (void)")); strings_sb_append(&_qoz_sb_4111_13, binding); strings_sb_append(&_qoz_sb_4111_13, QOZ_STR_LIT("; ")); _qoz_bv_275 = strings_sb_finish(&_qoz_sb_4111_13);
    }
    emit_push(e, _qoz_bv_275); emit_emit_branch_body_inline(e, body); emit_push(e, QOZ_STR_LIT("} }")); 
    return;
}

qoz_TypeExpr* emit_vec_element_typeexpr(qoz_Emitter* e, qoz_Expr* iter) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&iter);
    qoz_TypeExpr* te = emit_infer_base_typeexpr(e, iter); qoz_gc_push_root(&te); qoz_TypeExpr* _qoz_ms_288 = te; qoz_TypeExpr* _qoz_mv_288 = NULL; switch (_qoz_ms_288->tag) { case qoz_TypeExpr_TENamed: { qoz_Span sp = _qoz_ms_288->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_288->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_288->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_276;
    {
        if ((((args.len) >= 1) && ((path.len) >= 1)) && qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Vec"))) { return args.data[0];} _qoz_bv_276 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_288 = (_qoz_bv_276);  break; } case qoz_TypeExpr_TEPtr: { qoz_TypeExpr* inner = _qoz_ms_288->payload.TEPtr.f1; _qoz_mv_288 = (inner);  break; } default: { _qoz_mv_288 = (qoz_make_TypeExpr_TEUnit(emit_span_of_expr(iter)));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_288;
}

void emit_emit_map_for(qoz_Emitter* e, qoz_string binding, qoz_string binding2, qoz_Expr* iter, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&iter);
    qoz_gc_push_root(&body);
    qoz_TypeExpr* iter_te = emit_infer_base_typeexpr(e, iter); qoz_gc_push_root(&iter_te); qoz_string col_ct = emit_c_type_for(e, iter_te); qoz_TypeExpr* key_te = emit_map_key_typeexpr(e, iter_te); qoz_gc_push_root(&key_te); qoz_TypeExpr* val_te = emit_map_val_typeexpr(e, iter_te); qoz_gc_push_root(&val_te); qoz_string key_ct = emit_c_type_for(e, key_te); qoz_string val_ct = emit_c_type_for(e, val_te); qoz_string _qoz_bv_277;
    {
        qoz_Strbuf _qoz_sb_4135_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4135_13); strings_sb_append(&_qoz_sb_4135_13, QOZ_STR_LIT("{ ")); strings_sb_append(&_qoz_sb_4135_13, col_ct); strings_sb_append(&_qoz_sb_4135_13, QOZ_STR_LIT(" __col = ")); _qoz_bv_277 = strings_sb_finish(&_qoz_sb_4135_13);
    }
    emit_push(e, _qoz_bv_277); emit_emit_expr(e, iter); qoz_string _qoz_bv_278;
    {
        qoz_Strbuf _qoz_sb_4137_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4137_13); strings_sb_append(&_qoz_sb_4137_13, QOZ_STR_LIT("; for (int64_t __i = 0; __i < __col.cap; __i++) { if (!__col.slots[__i].occupied || __col.slots[__i].deleted) continue; ")); strings_sb_append(&_qoz_sb_4137_13, key_ct); strings_sb_append(&_qoz_sb_4137_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4137_13, binding); strings_sb_append(&_qoz_sb_4137_13, QOZ_STR_LIT(" = __col.slots[__i].key; ")); strings_sb_append(&_qoz_sb_4137_13, val_ct); strings_sb_append(&_qoz_sb_4137_13, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4137_13, binding2); strings_sb_append(&_qoz_sb_4137_13, QOZ_STR_LIT(" = __col.slots[__i].value; (void)")); strings_sb_append(&_qoz_sb_4137_13, binding); strings_sb_append(&_qoz_sb_4137_13, QOZ_STR_LIT("; (void)")); strings_sb_append(&_qoz_sb_4137_13, binding2); strings_sb_append(&_qoz_sb_4137_13, QOZ_STR_LIT("; ")); _qoz_bv_278 = strings_sb_finish(&_qoz_sb_4137_13);
    }
    emit_push(e, _qoz_bv_278); emit_emit_branch_body_inline(e, body); emit_push(e, QOZ_STR_LIT("} }")); 
    return;
}

qoz_TypeExpr* emit_map_key_typeexpr(qoz_Emitter* e, qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_289 = te; qoz_TypeExpr* _qoz_mv_289 = NULL; switch (_qoz_ms_289->tag) { case qoz_TypeExpr_TENamed: { qoz_Span sp = _qoz_ms_289->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_289->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_289->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_279;
    {
        if ((((args.len) >= 2) && ((path.len) >= 1)) && qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Map"))) { return args.data[0];} _qoz_bv_279 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_289 = (_qoz_bv_279);  break; } default: { _qoz_mv_289 = (qoz_make_TypeExpr_TEUnit(((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 })));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_289;
}

qoz_TypeExpr* emit_map_val_typeexpr(qoz_Emitter* e, qoz_TypeExpr* te) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_290 = te; qoz_TypeExpr* _qoz_mv_290 = NULL; switch (_qoz_ms_290->tag) { case qoz_TypeExpr_TENamed: { qoz_Span sp = _qoz_ms_290->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_290->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_290->payload.TENamed.f2; qoz_TypeExpr* _qoz_bv_280;
    {
        if ((((args.len) >= 2) && ((path.len) >= 1)) && qoz_string_eq(path.data[(path.len) - 1], QOZ_STR_LIT("Map"))) { return args.data[1];} _qoz_bv_280 = qoz_make_TypeExpr_TEUnit(sp);
    }
    _qoz_mv_290 = (_qoz_bv_280);  break; } default: { _qoz_mv_290 = (qoz_make_TypeExpr_TEUnit(((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 })));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_290;
}

void emit_emit_branch_body_inline(qoz_Emitter* e, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&body);
    qoz_Expr* _qoz_ms_291 = body; switch (_qoz_ms_291->tag) { case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_291->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_291->payload.EBlock.f2; {
        { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }if (!emit_is_nil_expr(tail)) { qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_close_statement_scope(e, saved); } 
    }
    0;  break; } default: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, body); emit_close_statement_scope(e, saved); 
    }
    0;  break; } } 0; 
    return;
}

qoz_string emit_find_enum_from_arms(qoz_Emitter* e, qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; qoz_string en = emit_bare_enum_for_pat(e, arm.pat); if (!qoz_string_eq(en, QOZ_STR_LIT(""))) { return en;} } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return QOZ_STR_LIT("");
}

bool emit_any_arm_has_guard(qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm a = __col.data[__i]; (void)a; if (a.has_guard) { return true;} } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

bool emit_any_arm_is_literal(qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm a = __col.data[__i]; (void)a; qoz_Pattern* _qoz_ms_292 = a.pat; switch (_qoz_ms_292->tag) { case qoz_Pattern_PatLitInt: { return true; break; } case qoz_Pattern_PatLitString: { return true; break; } case qoz_Pattern_PatLitBool: { return true; break; } default: { NULL;  break; } } 0; } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

void emit_emit_match_as_expr(qoz_Emitter* e, qoz_Span span, qoz_Expr* scrut, qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&scrut);
    if (emit_any_arm_has_guard(arms)) { emit_emit_match_as_if_chain(e, span, scrut, arms); return;} if (emit_any_arm_is_literal(arms)) { emit_emit_match_as_if_chain(e, span, scrut, arms); return;} qoz_string bare_enum = emit_find_enum_from_arms(e, arms); if (qoz_string_eq(bare_enum, QOZ_STR_LIT(""))) { emit_emit_match_as_if_chain(e, span, scrut, arms); return;} qoz_string enum_name = emit_enum_lookup_name(e, scrut, bare_enum); e->match_counter = e->match_counter + 1; qoz_string _qoz_bv_281;
    {
        qoz_Strbuf _qoz_sb_4223_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4223_21); strings_sb_append(&_qoz_sb_4223_21, QOZ_STR_LIT("_qoz_ms_")); strings_sb_append_i64(&_qoz_sb_4223_21, e->match_counter); _qoz_bv_281 = strings_sb_finish(&_qoz_sb_4223_21);
    }
    qoz_string scrut_tmp = _qoz_bv_281; qoz_string _qoz_bv_282;
    {
        qoz_Strbuf _qoz_sb_4224_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4224_21); strings_sb_append(&_qoz_sb_4224_21, QOZ_STR_LIT("_qoz_mv_")); strings_sb_append_i64(&_qoz_sb_4224_21, e->match_counter); _qoz_bv_282 = strings_sb_finish(&_qoz_sb_4224_21);
    }
    qoz_string res_tmp = _qoz_bv_282; qoz_string res_ctype = emit_match_result_ctype_with_hint(e, enum_name, arms); bool is_void = qoz_string_eq(res_ctype, QOZ_STR_LIT("void")); int64_t start = strings_sb_len(&e->out); qoz_string _qoz_bv_283;
    {
        qoz_Strbuf _qoz_sb_4233_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4233_13); strings_sb_append(&_qoz_sb_4233_13, QOZ_STR_LIT("qoz_")); strings_sb_append(&_qoz_sb_4233_13, enum_name); strings_sb_append(&_qoz_sb_4233_13, QOZ_STR_LIT("* ")); strings_sb_append(&_qoz_sb_4233_13, scrut_tmp); strings_sb_append(&_qoz_sb_4233_13, QOZ_STR_LIT(" = ")); _qoz_bv_283 = strings_sb_finish(&_qoz_sb_4233_13);
    }
    emit_push(e, _qoz_bv_283); emit_emit_expr(e, scrut); emit_push(e, QOZ_STR_LIT("; ")); if (!is_void) { qoz_string dv = emit_default_value_for(res_ctype); qoz_string _qoz_bv_284;
    {
        qoz_Strbuf _qoz_sb_4238_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4238_17); strings_sb_append(&_qoz_sb_4238_17, res_ctype); strings_sb_append(&_qoz_sb_4238_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4238_17, res_tmp); strings_sb_append(&_qoz_sb_4238_17, QOZ_STR_LIT(" = ")); strings_sb_append(&_qoz_sb_4238_17, dv); strings_sb_append(&_qoz_sb_4238_17, QOZ_STR_LIT("; ")); _qoz_bv_284 = strings_sb_finish(&_qoz_sb_4238_17);
    }
    emit_push(e, _qoz_bv_284); } qoz_string _qoz_bv_285;
    {
        qoz_Strbuf _qoz_sb_4240_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4240_13); strings_sb_append(&_qoz_sb_4240_13, QOZ_STR_LIT("switch (")); strings_sb_append(&_qoz_sb_4240_13, scrut_tmp); strings_sb_append(&_qoz_sb_4240_13, QOZ_STR_LIT("->tag) { ")); _qoz_bv_285 = strings_sb_finish(&_qoz_sb_4240_13);
    }
    emit_push(e, _qoz_bv_285); { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; emit_emit_match_arm_with_kind(e, enum_name, scrut_tmp, res_tmp, arm, is_void); } }emit_push(e, QOZ_STR_LIT("} ")); emit_hoist_to_prologue(e, start); if (is_void) { emit_push(e, QOZ_STR_LIT("0")); }  else { emit_push(e, res_tmp); } 
    return;
}

void emit_emit_match_as_if_chain(qoz_Emitter* e, qoz_Span span, qoz_Expr* scrut, qoz_Vec__qoz_MatchArm arms) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&scrut);
    qoz_string bare_enum = emit_find_enum_from_arms(e, arms); bool is_enum_match = !qoz_string_eq(bare_enum, QOZ_STR_LIT("")); qoz_string enum_name = QOZ_STR_LIT(""); if (is_enum_match) { enum_name = emit_enum_lookup_name(e, scrut, bare_enum); } qoz_string scrut_ctype = emit_infer_expr_ctype(e, scrut); qoz_TypeExpr* scrut_te = emit_infer_value_te(e, scrut); qoz_gc_push_root(&scrut_te); e->match_counter = e->match_counter + 1; qoz_string _qoz_bv_286;
    {
        qoz_Strbuf _qoz_sb_4261_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4261_21); strings_sb_append(&_qoz_sb_4261_21, QOZ_STR_LIT("_qoz_ms_")); strings_sb_append_i64(&_qoz_sb_4261_21, e->match_counter); _qoz_bv_286 = strings_sb_finish(&_qoz_sb_4261_21);
    }
    qoz_string scrut_tmp = _qoz_bv_286; qoz_string _qoz_bv_287;
    {
        qoz_Strbuf _qoz_sb_4262_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4262_21); strings_sb_append(&_qoz_sb_4262_21, QOZ_STR_LIT("_qoz_mv_")); strings_sb_append_i64(&_qoz_sb_4262_21, e->match_counter); _qoz_bv_287 = strings_sb_finish(&_qoz_sb_4262_21);
    }
    qoz_string res_tmp = _qoz_bv_287; qoz_string _qoz_bv_288;
    {
        qoz_Strbuf _qoz_sb_4263_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4263_21); strings_sb_append(&_qoz_sb_4263_21, QOZ_STR_LIT("_qoz_mm_")); strings_sb_append_i64(&_qoz_sb_4263_21, e->match_counter); _qoz_bv_288 = strings_sb_finish(&_qoz_sb_4263_21);
    }
    qoz_string mflag = _qoz_bv_288; qoz_string res_ctype = emit_match_result_ctype_with_hint(e, enum_name, arms); bool is_void = qoz_string_eq(res_ctype, QOZ_STR_LIT("void")); int64_t start = strings_sb_len(&e->out); if (is_enum_match) { qoz_string _qoz_bv_289;
    {
        qoz_Strbuf _qoz_sb_4268_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4268_17); strings_sb_append(&_qoz_sb_4268_17, QOZ_STR_LIT("qoz_")); strings_sb_append(&_qoz_sb_4268_17, enum_name); strings_sb_append(&_qoz_sb_4268_17, QOZ_STR_LIT("* ")); strings_sb_append(&_qoz_sb_4268_17, scrut_tmp); strings_sb_append(&_qoz_sb_4268_17, QOZ_STR_LIT(" = ")); _qoz_bv_289 = strings_sb_finish(&_qoz_sb_4268_17);
    }
    emit_push(e, _qoz_bv_289); }  else { qoz_string _qoz_bv_290;
    {
        qoz_Strbuf _qoz_sb_4270_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4270_17); strings_sb_append(&_qoz_sb_4270_17, scrut_ctype); strings_sb_append(&_qoz_sb_4270_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4270_17, scrut_tmp); strings_sb_append(&_qoz_sb_4270_17, QOZ_STR_LIT(" = ")); _qoz_bv_290 = strings_sb_finish(&_qoz_sb_4270_17);
    }
    emit_push(e, _qoz_bv_290); } emit_emit_expr(e, scrut); emit_push(e, QOZ_STR_LIT("; ")); if (!is_void) { qoz_string dv = emit_default_value_for(res_ctype); qoz_string _qoz_bv_291;
    {
        qoz_Strbuf _qoz_sb_4276_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4276_17); strings_sb_append(&_qoz_sb_4276_17, res_ctype); strings_sb_append(&_qoz_sb_4276_17, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4276_17, res_tmp); strings_sb_append(&_qoz_sb_4276_17, QOZ_STR_LIT(" = ")); strings_sb_append(&_qoz_sb_4276_17, dv); strings_sb_append(&_qoz_sb_4276_17, QOZ_STR_LIT("; ")); _qoz_bv_291 = strings_sb_finish(&_qoz_sb_4276_17);
    }
    emit_push(e, _qoz_bv_291); } qoz_string _qoz_bv_292;
    {
        qoz_Strbuf _qoz_sb_4278_13 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4278_13); strings_sb_append(&_qoz_sb_4278_13, QOZ_STR_LIT("bool ")); strings_sb_append(&_qoz_sb_4278_13, mflag); strings_sb_append(&_qoz_sb_4278_13, QOZ_STR_LIT(" = false; ")); _qoz_bv_292 = strings_sb_finish(&_qoz_sb_4278_13);
    }
    emit_push(e, _qoz_bv_292); { qoz_Vec__qoz_MatchArm __col = arms; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_MatchArm arm = __col.data[__i]; (void)arm; emit_emit_arm_in_chain_with_te(e, enum_name, scrut_tmp, scrut_te, res_tmp, mflag, arm, is_void); } }emit_hoist_to_prologue(e, start); if (is_void) { emit_push(e, QOZ_STR_LIT("0")); }  else { emit_push(e, res_tmp); } 
    return;
}

void emit_emit_arm_in_chain_with_te(qoz_Emitter* e, qoz_string enum_name, qoz_string scrut_tmp, qoz_TypeExpr* scrut_te, qoz_string res_tmp, qoz_string mflag, qoz_MatchArm arm, bool is_void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&scrut_te);
    qoz_TypeExpr* body_hint = emit_match_body_hint(e, enum_name, arm.body); qoz_gc_push_root(&body_hint); bool is_enum_match = !qoz_string_eq(enum_name, QOZ_STR_LIT("")); emit_push(e, QOZ_STR_LIT("if (!")); emit_push(e, mflag); qoz_Pattern* _qoz_ms_293 = arm.pat; switch (_qoz_ms_293->tag) { case qoz_Pattern_PatWild: { emit_push(e, QOZ_STR_LIT(") { "));  break; } case qoz_Pattern_PatBind: { qoz_string name = _qoz_ms_293->payload.PatBind.f1; if (is_enum_match) { qoz_Option__qoz_string* _qoz_ms_294 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); switch (_qoz_ms_294->tag) { case qoz_Option__qoz_string_Some: { qoz_string _qoz_bv_293;
    {
        qoz_Strbuf _qoz_sb_4300_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4300_25); strings_sb_append(&_qoz_sb_4300_25, QOZ_STR_LIT(" && ")); strings_sb_append(&_qoz_sb_4300_25, scrut_tmp); strings_sb_append(&_qoz_sb_4300_25, QOZ_STR_LIT("->tag == qoz_")); strings_sb_append(&_qoz_sb_4300_25, enum_name); strings_sb_append(&_qoz_sb_4300_25, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_4300_25, name); strings_sb_append(&_qoz_sb_4300_25, QOZ_STR_LIT(") { ")); _qoz_bv_293 = strings_sb_finish(&_qoz_sb_4300_25);
    }
    emit_push(e, _qoz_bv_293);  break; } default: { qoz_string _qoz_bv_294;
    {
        qoz_Strbuf _qoz_sb_4302_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4302_25); strings_sb_append(&_qoz_sb_4302_25, QOZ_STR_LIT(") { int64_t ")); strings_sb_append(&_qoz_sb_4302_25, name); strings_sb_append(&_qoz_sb_4302_25, QOZ_STR_LIT(" = 0; (void)")); strings_sb_append(&_qoz_sb_4302_25, name); strings_sb_append(&_qoz_sb_4302_25, QOZ_STR_LIT("; ")); _qoz_bv_294 = strings_sb_finish(&_qoz_sb_4302_25);
    }
    emit_push(e, _qoz_bv_294);  break; } } 0; }  else { qoz_string sc = emit_c_type_for(e, scrut_te); qoz_string _qoz_bv_295;
    {
        qoz_Strbuf _qoz_sb_4306_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4306_21); strings_sb_append(&_qoz_sb_4306_21, QOZ_STR_LIT(") { ")); strings_sb_append(&_qoz_sb_4306_21, sc); strings_sb_append(&_qoz_sb_4306_21, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4306_21, name); strings_sb_append(&_qoz_sb_4306_21, QOZ_STR_LIT(" = ")); strings_sb_append(&_qoz_sb_4306_21, scrut_tmp); strings_sb_append(&_qoz_sb_4306_21, QOZ_STR_LIT("; (void)")); strings_sb_append(&_qoz_sb_4306_21, name); strings_sb_append(&_qoz_sb_4306_21, QOZ_STR_LIT("; ")); _qoz_bv_295 = strings_sb_finish(&_qoz_sb_4306_21);
    }
    emit_push(e, _qoz_bv_295); qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, name, scrut_te); } 0;  break; } case qoz_Pattern_PatLitInt: { qoz_string text = _qoz_ms_293->payload.PatLitInt.f1; {
        qoz_string tn = emit_strip_numeric_underscores(text); qoz_string _qoz_bv_296;
    {
        qoz_Strbuf _qoz_sb_4312_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4312_17); strings_sb_append(&_qoz_sb_4312_17, QOZ_STR_LIT(" && ")); strings_sb_append(&_qoz_sb_4312_17, scrut_tmp); strings_sb_append(&_qoz_sb_4312_17, QOZ_STR_LIT(" == ")); strings_sb_append(&_qoz_sb_4312_17, tn); strings_sb_append(&_qoz_sb_4312_17, QOZ_STR_LIT(") { ")); _qoz_bv_296 = strings_sb_finish(&_qoz_sb_4312_17);
    }
    emit_push(e, _qoz_bv_296); 
    }
    0;  break; } case qoz_Pattern_PatLitBool: { bool b = _qoz_ms_293->payload.PatLitBool.f1; if (b) { qoz_string _qoz_bv_297;
    {
        qoz_Strbuf _qoz_sb_4315_24 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4315_24); strings_sb_append(&_qoz_sb_4315_24, QOZ_STR_LIT(" && ")); strings_sb_append(&_qoz_sb_4315_24, scrut_tmp); strings_sb_append(&_qoz_sb_4315_24, QOZ_STR_LIT(" == true) { ")); _qoz_bv_297 = strings_sb_finish(&_qoz_sb_4315_24);
    }
    emit_push(e, _qoz_bv_297); }  else { qoz_string _qoz_bv_298;
    {
        qoz_Strbuf _qoz_sb_4316_24 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4316_24); strings_sb_append(&_qoz_sb_4316_24, QOZ_STR_LIT(" && ")); strings_sb_append(&_qoz_sb_4316_24, scrut_tmp); strings_sb_append(&_qoz_sb_4316_24, QOZ_STR_LIT(" == false) { ")); _qoz_bv_298 = strings_sb_finish(&_qoz_sb_4316_24);
    }
    emit_push(e, _qoz_bv_298); } 0;  break; } case qoz_Pattern_PatLitString: { qoz_string text = _qoz_ms_293->payload.PatLitString.f1; qoz_string _qoz_bv_299;
    {
        qoz_Strbuf _qoz_sb_4319_17 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4319_17); strings_sb_append(&_qoz_sb_4319_17, QOZ_STR_LIT(" && qoz_string_eq(")); strings_sb_append(&_qoz_sb_4319_17, scrut_tmp); strings_sb_append(&_qoz_sb_4319_17, QOZ_STR_LIT(", QOZ_STR_LIT(")); strings_sb_append(&_qoz_sb_4319_17, text); strings_sb_append(&_qoz_sb_4319_17, QOZ_STR_LIT("))) { ")); _qoz_bv_299 = strings_sb_finish(&_qoz_sb_4319_17);
    }
    emit_push(e, _qoz_bv_299);  break; } case qoz_Pattern_PatVariant: { qoz_Vec__qoz_string path = _qoz_ms_293->payload.PatVariant.f1; qoz_Vec__qoz_Pattern sub_pats = _qoz_ms_293->payload.PatVariant.f2; if ((path.len) > 0) { qoz_string vname = path.data[(path.len) - 1]; qoz_string _qoz_bv_300;
    {
        qoz_Strbuf _qoz_sb_4324_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4324_21); strings_sb_append(&_qoz_sb_4324_21, QOZ_STR_LIT(" && ")); strings_sb_append(&_qoz_sb_4324_21, scrut_tmp); strings_sb_append(&_qoz_sb_4324_21, QOZ_STR_LIT("->tag == qoz_")); strings_sb_append(&_qoz_sb_4324_21, enum_name); strings_sb_append(&_qoz_sb_4324_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_4324_21, vname); strings_sb_append(&_qoz_sb_4324_21, QOZ_STR_LIT(") { ")); _qoz_bv_300 = strings_sb_finish(&_qoz_sb_4324_21);
    }
    emit_push(e, _qoz_bv_300); int64_t i = 0; { qoz_Vec__qoz_Pattern __col = sub_pats; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Pattern* sp = __col.data[__i]; (void)sp; qoz_Pattern* _qoz_ms_295 = sp; switch (_qoz_ms_295->tag) { case qoz_Pattern_PatBind: { qoz_string bname = _qoz_ms_295->payload.PatBind.f1; {
        qoz_string pc = emit_variant_payload_ctype(e, enum_name, vname, i); qoz_string is = emit_int_to_string(i); qoz_string _qoz_bv_301;
    {
        qoz_Strbuf _qoz_sb_4331_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4331_29); strings_sb_append(&_qoz_sb_4331_29, pc); strings_sb_append(&_qoz_sb_4331_29, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4331_29, bname); strings_sb_append(&_qoz_sb_4331_29, QOZ_STR_LIT(" = ")); strings_sb_append(&_qoz_sb_4331_29, scrut_tmp); strings_sb_append(&_qoz_sb_4331_29, QOZ_STR_LIT("->payload.")); strings_sb_append(&_qoz_sb_4331_29, vname); strings_sb_append(&_qoz_sb_4331_29, QOZ_STR_LIT(".f")); strings_sb_append(&_qoz_sb_4331_29, is); strings_sb_append(&_qoz_sb_4331_29, QOZ_STR_LIT("; ")); _qoz_bv_301 = strings_sb_finish(&_qoz_sb_4331_29);
    }
    emit_push(e, _qoz_bv_301); qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, bname, emit_variant_payload_typeexpr(e, enum_name, vname, i)); 
    }
    0;  break; } default: { NULL;  break; } } 0; i = i + 1; } }}  else { emit_push(e, QOZ_STR_LIT(") { ")); } 0;  break; } default: { emit_push(e, QOZ_STR_LIT(") { "));  break; } } 0; if (arm.has_guard) { emit_push(e, QOZ_STR_LIT("if (")); emit_emit_expr(e, arm.guard); emit_push(e, QOZ_STR_LIT(") { ")); } if (is_void) { emit_emit_stmt_expr(e, arm.body); }  else { emit_push(e, res_tmp); emit_push(e, QOZ_STR_LIT(" = (")); emit_emit_value_with_hint(e, arm.body, body_hint); emit_push(e, QOZ_STR_LIT("); ")); } emit_push(e, mflag); emit_push(e, QOZ_STR_LIT(" = true; ")); if (arm.has_guard) { emit_push(e, QOZ_STR_LIT("} ")); } emit_push(e, QOZ_STR_LIT("} ")); 
    return;
}

void emit_emit_arm_body_kind(qoz_Emitter* e, bool is_void, qoz_string res_tmp, qoz_Expr* body, qoz_TypeExpr* body_hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&body);
    qoz_gc_push_root(&body_hint);
    qoz_StmtScope saved = emit_open_statement_scope(e); if (is_void) { emit_emit_stmt_expr(e, body); }  else { emit_emit_arm_body_with_hint(e, res_tmp, body, body_hint); } emit_close_statement_scope(e, saved); 
    return;
}

void emit_emit_match_arm_with_kind(qoz_Emitter* e, qoz_string enum_name, qoz_string scrut_tmp, qoz_string res_tmp, qoz_MatchArm arm, bool is_void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_TypeExpr* body_hint = emit_match_body_hint(e, enum_name, arm.body); qoz_gc_push_root(&body_hint); qoz_Pattern* _qoz_ms_296 = arm.pat; switch (_qoz_ms_296->tag) { case qoz_Pattern_PatWild: { {
        emit_push(e, QOZ_STR_LIT("default: { ")); emit_emit_arm_body_kind(e, is_void, res_tmp, arm.body, body_hint); emit_push(e, QOZ_STR_LIT(" break; } ")); 
    }
    0;  break; } case qoz_Pattern_PatBind: { qoz_string name = _qoz_ms_296->payload.PatBind.f1; qoz_Option__qoz_string* _qoz_ms_297 = qoz_map_get__qoz_string__qoz_string(&e->variant_of, name); switch (_qoz_ms_297->tag) { case qoz_Option__qoz_string_Some: { {
        qoz_string _qoz_bv_302;
    {
        qoz_Strbuf _qoz_sb_4394_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4394_21); strings_sb_append(&_qoz_sb_4394_21, QOZ_STR_LIT("case qoz_")); strings_sb_append(&_qoz_sb_4394_21, enum_name); strings_sb_append(&_qoz_sb_4394_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_4394_21, name); strings_sb_append(&_qoz_sb_4394_21, QOZ_STR_LIT(": { ")); _qoz_bv_302 = strings_sb_finish(&_qoz_sb_4394_21);
    }
    emit_push(e, _qoz_bv_302); if (arm.has_guard) { emit_push(e, QOZ_STR_LIT("if (")); emit_emit_expr(e, arm.guard); emit_push(e, QOZ_STR_LIT(") { ")); emit_emit_arm_body_kind(e, is_void, res_tmp, arm.body, body_hint); emit_push(e, QOZ_STR_LIT("} ")); }  else { emit_emit_arm_body_kind(e, is_void, res_tmp, arm.body, body_hint); } emit_push(e, QOZ_STR_LIT(" break; } ")); 
    }
    0;  break; } case qoz_Option__qoz_string_None: { {
        if (is_void) { emit_push(e, QOZ_STR_LIT("default: { ")); }  else { qoz_string _qoz_bv_303;
    {
        qoz_Strbuf _qoz_sb_4410_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4410_25); strings_sb_append(&_qoz_sb_4410_25, QOZ_STR_LIT("default: { int64_t ")); strings_sb_append(&_qoz_sb_4410_25, name); strings_sb_append(&_qoz_sb_4410_25, QOZ_STR_LIT(" = 0; (void)")); strings_sb_append(&_qoz_sb_4410_25, name); strings_sb_append(&_qoz_sb_4410_25, QOZ_STR_LIT("; ")); _qoz_bv_303 = strings_sb_finish(&_qoz_sb_4410_25);
    }
    emit_push(e, _qoz_bv_303); } if (arm.has_guard) { emit_push(e, QOZ_STR_LIT("if (")); emit_emit_expr(e, arm.guard); emit_push(e, QOZ_STR_LIT(") { ")); emit_emit_arm_body_kind(e, is_void, res_tmp, arm.body, body_hint); emit_push(e, QOZ_STR_LIT("} ")); }  else { emit_emit_arm_body_kind(e, is_void, res_tmp, arm.body, body_hint); } emit_push(e, QOZ_STR_LIT(" break; } ")); 
    }
    0;  break; } } 0;  break; } case qoz_Pattern_PatVariant: { qoz_Vec__qoz_string path = _qoz_ms_296->payload.PatVariant.f1; qoz_Vec__qoz_Pattern sub_pats = _qoz_ms_296->payload.PatVariant.f2; if ((path.len) > 0) { qoz_string vname = path.data[(path.len) - 1]; qoz_string _qoz_bv_304;
    {
        qoz_Strbuf _qoz_sb_4428_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4428_21); strings_sb_append(&_qoz_sb_4428_21, QOZ_STR_LIT("case qoz_")); strings_sb_append(&_qoz_sb_4428_21, enum_name); strings_sb_append(&_qoz_sb_4428_21, QOZ_STR_LIT("_")); strings_sb_append(&_qoz_sb_4428_21, vname); strings_sb_append(&_qoz_sb_4428_21, QOZ_STR_LIT(": { ")); _qoz_bv_304 = strings_sb_finish(&_qoz_sb_4428_21);
    }
    emit_push(e, _qoz_bv_304); int64_t i = 0; { qoz_Vec__qoz_Pattern __col = sub_pats; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Pattern* sp = __col.data[__i]; (void)sp; qoz_Pattern* _qoz_ms_298 = sp; switch (_qoz_ms_298->tag) { case qoz_Pattern_PatBind: { qoz_string bname = _qoz_ms_298->payload.PatBind.f1; {
        qoz_string pc = emit_variant_payload_ctype(e, enum_name, vname, i); qoz_string is = emit_int_to_string(i); qoz_string _qoz_bv_305;
    {
        qoz_Strbuf _qoz_sb_4435_29 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4435_29); strings_sb_append(&_qoz_sb_4435_29, pc); strings_sb_append(&_qoz_sb_4435_29, QOZ_STR_LIT(" ")); strings_sb_append(&_qoz_sb_4435_29, bname); strings_sb_append(&_qoz_sb_4435_29, QOZ_STR_LIT(" = ")); strings_sb_append(&_qoz_sb_4435_29, scrut_tmp); strings_sb_append(&_qoz_sb_4435_29, QOZ_STR_LIT("->payload.")); strings_sb_append(&_qoz_sb_4435_29, vname); strings_sb_append(&_qoz_sb_4435_29, QOZ_STR_LIT(".f")); strings_sb_append(&_qoz_sb_4435_29, is); strings_sb_append(&_qoz_sb_4435_29, QOZ_STR_LIT("; ")); _qoz_bv_305 = strings_sb_finish(&_qoz_sb_4435_29);
    }
    emit_push(e, _qoz_bv_305); qoz_map_set__qoz_string__qoz_TypeExpr(&e->locals, bname, emit_variant_payload_typeexpr(e, enum_name, vname, i)); 
    }
    0;  break; } default: { NULL;  break; } } 0; i = i + 1; } }if (arm.has_guard) { emit_push(e, QOZ_STR_LIT("if (")); emit_emit_expr(e, arm.guard); emit_push(e, QOZ_STR_LIT(") { ")); emit_emit_arm_body_kind(e, is_void, res_tmp, arm.body, body_hint); emit_push(e, QOZ_STR_LIT("} ")); }  else { emit_emit_arm_body_kind(e, is_void, res_tmp, arm.body, body_hint); } emit_push(e, QOZ_STR_LIT(" break; } ")); } 0;  break; } default: { NULL;  break; } } 0; 
    return;
}

qoz_string emit_enum_lookup_name(qoz_Emitter* e, qoz_Expr* scrut, qoz_string bare_enum) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&scrut);
    qoz_TypeExpr* te = emit_infer_base_typeexpr(e, scrut); qoz_gc_push_root(&te); qoz_gc_shadow_set_top(_qoz_shadow_guard); return emit_enum_name_from_type(e, te, bare_enum);
}

qoz_string emit_enum_name_from_type(qoz_Emitter* e, qoz_TypeExpr* te, qoz_string bare_enum) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&te);
    qoz_TypeExpr* _qoz_ms_299 = te; qoz_string _qoz_mv_299 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_299->tag) { case qoz_TypeExpr_TENamed: { qoz_Vec__qoz_string path = _qoz_ms_299->payload.TENamed.f1; qoz_Vec__qoz_TypeExpr args = _qoz_ms_299->payload.TENamed.f2; qoz_string _qoz_bv_306;
    {
        if (((args.len) > 0) && ((path.len) >= 1)) { qoz_string last = path.data[(path.len) - 1]; if (qoz_string_eq(last, bare_enum)) { return emit_mangle_inst(e, bare_enum, args);} } _qoz_bv_306 = bare_enum;
    }
    _qoz_mv_299 = (_qoz_bv_306);  break; } default: { _qoz_mv_299 = (bare_enum);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_299;
}

qoz_Vec__qoz_TypeExpr emit_variant_inst_args(qoz_Emitter* e, qoz_string enum_name, qoz_string bare) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    { qoz_Vec__qoz_Instantiation __col = e->enum_insts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Instantiation inst = __col.data[__i]; (void)inst; if (qoz_string_eq(inst.mangled, enum_name)) { return inst.args;} } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return emit_mangled_args(e, enum_name, bare);
}

qoz_TypeExpr* emit_variant_payload_typeexpr(qoz_Emitter* e, qoz_string enum_name, qoz_string variant, int64_t pos) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string bare = emit_strip_mangled(enum_name); qoz_Span unit_sp = ((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 }); qoz_Option__qoz_Decl* _qoz_ms_300 = qoz_map_get__qoz_string__qoz_Decl(&e->enum_decls, bare); qoz_TypeExpr* _qoz_mv_300 = NULL; switch (_qoz_ms_300->tag) { case qoz_Option__qoz_Decl_None: { _qoz_mv_300 = (qoz_make_TypeExpr_TEUnit(unit_sp));  break; } case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_300->payload.Some.f0; qoz_Decl* _qoz_ms_301 = d; qoz_TypeExpr* _qoz_mv_301 = NULL; switch (_qoz_ms_301->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string tparams = _qoz_ms_301->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_301->payload.DEnum.f3; qoz_TypeExpr* _qoz_bv_307;
    {
        qoz_Vec__qoz_TypeExpr inst_args = emit_variant_inst_args(e, enum_name, bare); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (qoz_string_eq(v.name, variant)) { if ((pos >= 0) && (pos < (v.pos.len))) { if (((tparams.len) > 0) && ((inst_args.len) == (tparams.len))) { return emit_substitute_type(e, v.pos.data[pos], tparams, inst_args);} return v.pos.data[pos];} } } }_qoz_bv_307 = qoz_make_TypeExpr_TEUnit(unit_sp);
    }
    _qoz_mv_301 = (_qoz_bv_307);  break; } default: { _qoz_mv_301 = (qoz_make_TypeExpr_TEUnit(unit_sp));  break; } } _qoz_mv_300 = (_qoz_mv_301);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_300;
}

qoz_string emit_variant_payload_ctype(qoz_Emitter* e, qoz_string enum_name, qoz_string variant, int64_t pos) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_string bare = emit_strip_mangled(enum_name); qoz_Option__qoz_Decl* _qoz_ms_302 = qoz_map_get__qoz_string__qoz_Decl(&e->enum_decls, bare); qoz_string _qoz_mv_302 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_302->tag) { case qoz_Option__qoz_Decl_None: { _qoz_mv_302 = (QOZ_STR_LIT("int64_t"));  break; } case qoz_Option__qoz_Decl_Some: { qoz_Decl* d = _qoz_ms_302->payload.Some.f0; qoz_Decl* _qoz_ms_303 = d; qoz_string _qoz_mv_303 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_303->tag) { case qoz_Decl_DEnum: { qoz_Vec__qoz_string tparams = _qoz_ms_303->payload.DEnum.f2; qoz_Vec__qoz_VariantDecl variants = _qoz_ms_303->payload.DEnum.f3; qoz_string _qoz_bv_308;
    {
        qoz_Vec__qoz_TypeExpr inst_args = emit_variant_inst_args(e, enum_name, bare); { qoz_Vec__qoz_VariantDecl __col = variants; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_VariantDecl v = __col.data[__i]; (void)v; if (qoz_string_eq(v.name, variant)) { if ((pos >= 0) && (pos < (v.pos.len))) { qoz_TypeExpr* subst = ((((tparams.len) > 0) && ((inst_args.len) == (tparams.len))) ? emit_substitute_type(e, v.pos.data[pos], tparams, inst_args) : v.pos.data[pos]); qoz_gc_push_root(&subst); return emit_c_type_for(e, subst);} } } }_qoz_bv_308 = QOZ_STR_LIT("int64_t");
    }
    _qoz_mv_303 = (_qoz_bv_308);  break; } default: { _qoz_mv_303 = (QOZ_STR_LIT("int64_t"));  break; } } _qoz_mv_302 = (_qoz_mv_303);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_302;
}

qoz_string emit_strip_mangled(qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    int64_t idx = qoz_string_index_byte(name, 95); if (idx < 0) { return name;} int64_t n = (name).len; int64_t i = 0; while (i + 1 < n) { if ((qoz_string_byte_at(name, i) == 95) && (qoz_string_byte_at(name, i + 1) == 95)) { return qoz_string_slice(name, 0, i);} i = i + 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return name;
}

qoz_Vec__qoz_TypeExpr emit_mangled_args(qoz_Emitter* e, qoz_string mangled, qoz_string bare) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_Vec__qoz_TypeExpr out = qoz_vec_make__qoz_TypeExpr(); int64_t bn = (bare).len; int64_t mn = (mangled).len; if (mn <= bn + 2) { return out;} qoz_Span sp = ((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 }); qoz_string tail = qoz_string_slice(mangled, bn + 2, mn); int64_t n = (tail).len; int64_t start = 0; int64_t i = 0; while (i + 1 < n) { if ((qoz_string_byte_at(tail, i) == 95) && (qoz_string_byte_at(tail, i + 1) == 95)) { qoz_vec_push__qoz_TypeExpr(&out, emit_type_expr_from_mangled_part(qoz_string_slice(tail, start, i), sp)); start = i + 2; i = i + 2; }  else { i = i + 1; } } if (start < n) { qoz_vec_push__qoz_TypeExpr(&out, emit_type_expr_from_mangled_part(qoz_string_slice(tail, start, n), sp)); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return out;
}

qoz_TypeExpr* emit_type_expr_from_mangled_part(qoz_string part, qoz_Span sp) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_string unmapped = emit_unmangle_primitive(part); qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, unmapped); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_TypeExpr_TENamed(sp, path, qoz_vec_make__qoz_TypeExpr());
}

qoz_string emit_unmangle_primitive(qoz_string s) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if (qoz_string_eq(s, QOZ_STR_LIT("int8_t"))) { return QOZ_STR_LIT("i8");} if (qoz_string_eq(s, QOZ_STR_LIT("int16_t"))) { return QOZ_STR_LIT("i16");} if (qoz_string_eq(s, QOZ_STR_LIT("int32_t"))) { return QOZ_STR_LIT("i32");} if (qoz_string_eq(s, QOZ_STR_LIT("int64_t"))) { return QOZ_STR_LIT("i64");} if (qoz_string_eq(s, QOZ_STR_LIT("uint8_t"))) { return QOZ_STR_LIT("u8");} if (qoz_string_eq(s, QOZ_STR_LIT("uint16_t"))) { return QOZ_STR_LIT("u16");} if (qoz_string_eq(s, QOZ_STR_LIT("uint32_t"))) { return QOZ_STR_LIT("u32");} if (qoz_string_eq(s, QOZ_STR_LIT("uint64_t"))) { return QOZ_STR_LIT("u64");} if (qoz_string_eq(s, QOZ_STR_LIT("float"))) { return QOZ_STR_LIT("f32");} if (qoz_string_eq(s, QOZ_STR_LIT("double"))) { return QOZ_STR_LIT("f64");} if (qoz_string_eq(s, QOZ_STR_LIT("qoz_string"))) { return QOZ_STR_LIT("string");} if (qoz_string_eq(s, QOZ_STR_LIT("bool"))) { return QOZ_STR_LIT("bool");} if (qoz_string_eq(s, QOZ_STR_LIT("char"))) { return QOZ_STR_LIT("char");} if (qoz_string_has_prefix(s, QOZ_STR_LIT("qoz_"))) { return qoz_string_slice(s, 4, (s).len);} qoz_gc_shadow_set_top(_qoz_shadow_guard); return s;
}

void emit_emit_arm_body_with_hint(qoz_Emitter* e, qoz_string res_tmp, qoz_Expr* body, qoz_TypeExpr* hint) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&body);
    qoz_gc_push_root(&hint);
    qoz_Expr* _qoz_ms_304 = body; switch (_qoz_ms_304->tag) { case qoz_Expr_EReturn: { qoz_Expr* v = _qoz_ms_304->payload.EReturn.f1; if (emit_is_nil_expr(v)) { emit_push(e, QOZ_STR_LIT("return; ")); }  else { emit_push(e, QOZ_STR_LIT("return ")); emit_emit_value_with_hint(e, v, e->current_ret_te); emit_push(e, QOZ_STR_LIT("; ")); } 0;  break; } default: { {
        emit_push(e, res_tmp); emit_push(e, QOZ_STR_LIT(" = (")); emit_emit_value_with_hint(e, body, hint); emit_push(e, QOZ_STR_LIT("); ")); 
    }
    0;  break; } } 0; 
    return;
}

qoz_TypeExpr* emit_match_body_hint(qoz_Emitter* e, qoz_string enum_name, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&body);
    if (!emit_is_unit_typeexpr(e->match_hint)) { return e->match_hint;} qoz_Span sp = ((qoz_Span){ .file = QOZ_STR_LIT(""), .line = 0, .col = 0 }); qoz_string bare = emit_strip_mangled(enum_name); qoz_Vec__qoz_TypeExpr inst_args = emit_mangled_args(e, enum_name, bare); qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, bare); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_TypeExpr_TENamed(sp, path, inst_args);
}

void emit_emit_main(qoz_Emitter* e, qoz_TypeExpr* ret, qoz_Expr* body) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&ret);
    qoz_gc_push_root(&body);
    qoz_MainRetKind* kind = emit_main_return_kind(ret); qoz_gc_push_root(&kind); emit_push(e, QOZ_STR_LIT("int main(int argc, char **argv) {\n    qoz_set_argv(argc, argv);\n    int qoz_stack_anchor;\n    qoz_init(&qoz_stack_anchor);\n    ")); qoz_Expr* _qoz_ms_305 = body; switch (_qoz_ms_305->tag) { case qoz_Expr_EBlock: { qoz_Vec__qoz_Stmt stmts = _qoz_ms_305->payload.EBlock.f1; qoz_Expr* tail = _qoz_ms_305->payload.EBlock.f2; {
        { qoz_Vec__qoz_Stmt __col = stmts; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Stmt* s = __col.data[__i]; (void)s; emit_emit_stmt(e, s); } }if (emit_is_nil_expr(tail)) { emit_push(e, QOZ_STR_LIT("\n    qoz_shutdown();\n    return 0;\n}\n")); return;} emit_emit_main_tail(e, kind, tail); 
    }
    0;  break; } default: { emit_emit_main_tail(e, kind, body);  break; } } 0; 
    return;
}

qoz_MainRetKind* emit_main_return_kind(qoz_TypeExpr* ret) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&ret);
    qoz_TypeExpr* _qoz_ms_306 = ret; qoz_MainRetKind* _qoz_mv_306 = NULL; switch (_qoz_ms_306->tag) { case qoz_TypeExpr_TEUnit: { _qoz_mv_306 = (qoz_make_MainRetKind_MainRetUnit());  break; } case qoz_TypeExpr_TENamed: { qoz_Span sp = _qoz_ms_306->payload.TENamed.f0; qoz_Vec__qoz_string path = _qoz_ms_306->payload.TENamed.f1; qoz_MainRetKind* _qoz_bv_309;
    {
        if ((path.len) != 1) { qoz_string _qoz_bv_310;
    {
        qoz_Strbuf _qoz_sb_4678_25 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4678_25); strings_sb_append(&_qoz_sb_4678_25, sp.file); strings_sb_append(&_qoz_sb_4678_25, QOZ_STR_LIT(":")); strings_sb_append_i64(&_qoz_sb_4678_25, sp.line); strings_sb_append(&_qoz_sb_4678_25, QOZ_STR_LIT(":")); strings_sb_append_i64(&_qoz_sb_4678_25, sp.col); strings_sb_append(&_qoz_sb_4678_25, QOZ_STR_LIT(": main return type must be a primitive integer or unit")); _qoz_bv_310 = strings_sb_finish(&_qoz_sb_4678_25);
    }
    fmt_println(_qoz_bv_310); return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("int64_t"));} qoz_string n = path.data[0]; if (qoz_string_eq(n, QOZ_STR_LIT("i8"))) { return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("int8_t"));} if (qoz_string_eq(n, QOZ_STR_LIT("i16"))) { return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("int16_t"));} if (qoz_string_eq(n, QOZ_STR_LIT("i32"))) { return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("int32_t"));} if (qoz_string_eq(n, QOZ_STR_LIT("i64"))) { return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("int64_t"));} if (qoz_string_eq(n, QOZ_STR_LIT("u8"))) { return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("uint8_t"));} if (qoz_string_eq(n, QOZ_STR_LIT("u16"))) { return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("uint16_t"));} if (qoz_string_eq(n, QOZ_STR_LIT("u32"))) { return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("uint32_t"));} if (qoz_string_eq(n, QOZ_STR_LIT("u64"))) { return qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("uint64_t"));} if (qoz_string_eq(n, QOZ_STR_LIT("unit"))) { return qoz_make_MainRetKind_MainRetUnit();} if (qoz_string_eq(n, QOZ_STR_LIT("void"))) { return qoz_make_MainRetKind_MainRetUnit();} qoz_string _qoz_bv_311;
    {
        qoz_Strbuf _qoz_sb_4692_21 = ((qoz_Strbuf){ .buf = ((uint8_t*)NULL), .len = 0, .cap = 0 }); strings_sb_init(&_qoz_sb_4692_21); strings_sb_append(&_qoz_sb_4692_21, sp.file); strings_sb_append(&_qoz_sb_4692_21, QOZ_STR_LIT(":")); strings_sb_append_i64(&_qoz_sb_4692_21, sp.line); strings_sb_append(&_qoz_sb_4692_21, QOZ_STR_LIT(":")); strings_sb_append_i64(&_qoz_sb_4692_21, sp.col); strings_sb_append(&_qoz_sb_4692_21, QOZ_STR_LIT(": main may return only integer types or unit, got `")); strings_sb_append(&_qoz_sb_4692_21, n); strings_sb_append(&_qoz_sb_4692_21, QOZ_STR_LIT("`")); _qoz_bv_311 = strings_sb_finish(&_qoz_sb_4692_21);
    }
    fmt_println(_qoz_bv_311); _qoz_bv_309 = qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("int64_t"));
    }
    _qoz_mv_306 = (_qoz_bv_309);  break; } default: { qoz_MainRetKind* _qoz_bv_312;
    {
        fmt_println(QOZ_STR_LIT("main return type must be a primitive integer or unit")); _qoz_bv_312 = qoz_make_MainRetKind_MainRetInt(QOZ_STR_LIT("int64_t"));
    }
    _qoz_mv_306 = (_qoz_bv_312);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_306;
}

void emit_emit_main_tail(qoz_Emitter* e, qoz_MainRetKind* kind, qoz_Expr* tail) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&e);
    qoz_gc_push_root(&kind);
    qoz_gc_push_root(&tail);
    qoz_MainRetKind* _qoz_ms_307 = kind; switch (_qoz_ms_307->tag) { case qoz_MainRetKind_MainRetUnit: { {
        qoz_Expr* _qoz_ms_308 = tail; bool _qoz_mv_308 = false; switch (_qoz_ms_308->tag) { case qoz_Expr_EIf: { qoz_Expr* f = _qoz_ms_308->payload.EIf.f3; _qoz_mv_308 = (emit_is_nil_expr(f) || emit_is_unit_typeexpr(emit_infer_value_te(e, tail)));  break; } default: { _qoz_mv_308 = (false);  break; } } bool is_unit_if = _qoz_mv_308; qoz_Expr* _qoz_ms_309 = tail; switch (_qoz_ms_309->tag) { case qoz_Expr_EFor: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_push(e, QOZ_STR_LIT("\n    qoz_shutdown();\n    return 0;\n}\n")); emit_close_statement_scope(e, saved); return;
    }
    0;  break; } case qoz_Expr_EWhile: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_push(e, QOZ_STR_LIT("\n    qoz_shutdown();\n    return 0;\n}\n")); emit_close_statement_scope(e, saved); return;
    }
    0;  break; } case qoz_Expr_EAssign: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_push(e, QOZ_STR_LIT("\n    qoz_shutdown();\n    return 0;\n}\n")); emit_close_statement_scope(e, saved); return;
    }
    0;  break; } case qoz_Expr_EDefer: { {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_push(e, QOZ_STR_LIT("\n    qoz_shutdown();\n    return 0;\n}\n")); emit_close_statement_scope(e, saved); return;
    }
    0;  break; } default: { NULL;  break; } } 0; if (is_unit_if) { qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_stmt_expr(e, tail); emit_push(e, QOZ_STR_LIT("\n    qoz_shutdown();\n    return 0;\n}\n")); emit_close_statement_scope(e, saved); return;} qoz_StmtScope saved = emit_open_statement_scope(e); emit_emit_expr(e, tail); emit_push(e, QOZ_STR_LIT(";\n    qoz_shutdown();\n    return 0;\n}\n")); emit_close_statement_scope(e, saved); 
    }
    0;  break; } case qoz_MainRetKind_MainRetInt: { qoz_string ctype = _qoz_ms_307->payload.MainRetInt.f0; {
        qoz_StmtScope saved = emit_open_statement_scope(e); emit_push(e, ctype); emit_push(e, QOZ_STR_LIT(" qoz_main_result = ")); emit_emit_expr(e, tail); emit_push(e, QOZ_STR_LIT(";\n    qoz_shutdown();\n    return (int)qoz_main_result;\n}\n")); emit_close_statement_scope(e, saved); 
    }
    0;  break; } } 0; 
    return;
}

qoz_Ty* ty_ty_int_(int64_t width, bool is_signed) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyInt(((qoz_IntInfo){ .width = width, .is_signed = is_signed, .untyped = false }));
}

qoz_Ty* ty_ty_int_untyped(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyInt(((qoz_IntInfo){ .width = 0, .is_signed = true, .untyped = true }));
}

qoz_Ty* ty_ty_float_(int64_t width) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyFloat(((qoz_FloatInfo){ .width = width, .untyped = false }));
}

qoz_Ty* ty_ty_float_untyped(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyFloat(((qoz_FloatInfo){ .width = 0, .untyped = true }));
}

qoz_Ty* ty_ty_bool_(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyBool();
}

qoz_Ty* ty_ty_char_(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyChar();
}

qoz_Ty* ty_ty_string_(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyString();
}

qoz_Ty* ty_ty_cstring_(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyCstring();
}

qoz_Ty* ty_ty_unit_(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyUnit();
}

qoz_Ty* ty_ty_nil_(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyNil();
}

qoz_Ty* ty_ty_error_(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyError();
}

qoz_Ty* ty_ty_ptr_(qoz_Ty* inner) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&inner);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyPtr(inner);
}

qoz_Ty* ty_ty_adt_(qoz_string name, qoz_Vec__qoz_Ty args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyAdt(name, args);
}

qoz_Ty* ty_ty_record_(qoz_string name, qoz_Vec__qoz_Ty args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyRecord(name, args);
}

qoz_Ty* ty_ty_fn_(qoz_Vec__qoz_Ty params, qoz_Ty* ret) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&ret);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyFn(params, ret);
}

qoz_Ty* ty_ty_tuple_(qoz_Vec__qoz_Ty elems) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyTuple(elems);
}

qoz_Ty* ty_ty_var_(qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Ty_TyVar(0, name);
}

bool ty_ty_eq(qoz_Ty* a, qoz_Ty* b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&a);
    qoz_gc_push_root(&b);
    qoz_Ty* _qoz_ms_310 = a; bool _qoz_mv_310 = false; switch (_qoz_ms_310->tag) { case qoz_Ty_TyInt: { qoz_IntInfo ai = _qoz_ms_310->payload.TyInt.f0; qoz_Ty* _qoz_ms_311 = b; bool _qoz_mv_311 = false; switch (_qoz_ms_311->tag) { case qoz_Ty_TyInt: { qoz_IntInfo bi = _qoz_ms_311->payload.TyInt.f0; _qoz_mv_311 = (((ai.width == bi.width) && (ai.is_signed == bi.is_signed)) && (ai.untyped == bi.untyped));  break; } default: { _qoz_mv_311 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_311);  break; } case qoz_Ty_TyFloat: { qoz_FloatInfo af = _qoz_ms_310->payload.TyFloat.f0; qoz_Ty* _qoz_ms_312 = b; bool _qoz_mv_312 = false; switch (_qoz_ms_312->tag) { case qoz_Ty_TyFloat: { qoz_FloatInfo bf = _qoz_ms_312->payload.TyFloat.f0; _qoz_mv_312 = ((af.width == bf.width) && (af.untyped == bf.untyped));  break; } default: { _qoz_mv_312 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_312);  break; } case qoz_Ty_TyBool: { qoz_Ty* _qoz_ms_313 = b; bool _qoz_mv_313 = false; switch (_qoz_ms_313->tag) { case qoz_Ty_TyBool: { _qoz_mv_313 = (true);  break; } default: { _qoz_mv_313 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_313);  break; } case qoz_Ty_TyChar: { qoz_Ty* _qoz_ms_314 = b; bool _qoz_mv_314 = false; switch (_qoz_ms_314->tag) { case qoz_Ty_TyChar: { _qoz_mv_314 = (true);  break; } default: { _qoz_mv_314 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_314);  break; } case qoz_Ty_TyString: { qoz_Ty* _qoz_ms_315 = b; bool _qoz_mv_315 = false; switch (_qoz_ms_315->tag) { case qoz_Ty_TyString: { _qoz_mv_315 = (true);  break; } default: { _qoz_mv_315 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_315);  break; } case qoz_Ty_TyCstring: { qoz_Ty* _qoz_ms_316 = b; bool _qoz_mv_316 = false; switch (_qoz_ms_316->tag) { case qoz_Ty_TyCstring: { _qoz_mv_316 = (true);  break; } default: { _qoz_mv_316 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_316);  break; } case qoz_Ty_TyUnit: { qoz_Ty* _qoz_ms_317 = b; bool _qoz_mv_317 = false; switch (_qoz_ms_317->tag) { case qoz_Ty_TyUnit: { _qoz_mv_317 = (true);  break; } default: { _qoz_mv_317 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_317);  break; } case qoz_Ty_TyNil: { qoz_Ty* _qoz_ms_318 = b; bool _qoz_mv_318 = false; switch (_qoz_ms_318->tag) { case qoz_Ty_TyNil: { _qoz_mv_318 = (true);  break; } default: { _qoz_mv_318 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_318);  break; } case qoz_Ty_TyError: { qoz_Ty* _qoz_ms_319 = b; bool _qoz_mv_319 = false; switch (_qoz_ms_319->tag) { case qoz_Ty_TyError: { _qoz_mv_319 = (true);  break; } default: { _qoz_mv_319 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_319);  break; } case qoz_Ty_TyPtr: { qoz_Ty* ai = _qoz_ms_310->payload.TyPtr.f0; qoz_Ty* _qoz_ms_320 = b; bool _qoz_mv_320 = false; switch (_qoz_ms_320->tag) { case qoz_Ty_TyPtr: { qoz_Ty* bi = _qoz_ms_320->payload.TyPtr.f0; _qoz_mv_320 = (ty_ty_eq(ai, bi));  break; } default: { _qoz_mv_320 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_320);  break; } case qoz_Ty_TyAdt: { qoz_string an = _qoz_ms_310->payload.TyAdt.f0; qoz_Vec__qoz_Ty aa = _qoz_ms_310->payload.TyAdt.f1; qoz_Ty* _qoz_ms_321 = b; bool _qoz_mv_321 = false; switch (_qoz_ms_321->tag) { case qoz_Ty_TyAdt: { qoz_string bn = _qoz_ms_321->payload.TyAdt.f0; qoz_Vec__qoz_Ty ba = _qoz_ms_321->payload.TyAdt.f1; _qoz_mv_321 = (qoz_string_eq(an, bn) && ty_ty_args_eq(aa, ba));  break; } default: { _qoz_mv_321 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_321);  break; } case qoz_Ty_TyRecord: { qoz_string an = _qoz_ms_310->payload.TyRecord.f0; qoz_Vec__qoz_Ty aa = _qoz_ms_310->payload.TyRecord.f1; qoz_Ty* _qoz_ms_322 = b; bool _qoz_mv_322 = false; switch (_qoz_ms_322->tag) { case qoz_Ty_TyRecord: { qoz_string bn = _qoz_ms_322->payload.TyRecord.f0; qoz_Vec__qoz_Ty ba = _qoz_ms_322->payload.TyRecord.f1; _qoz_mv_322 = (qoz_string_eq(an, bn) && ty_ty_args_eq(aa, ba));  break; } default: { _qoz_mv_322 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_322);  break; } case qoz_Ty_TyFn: { qoz_Vec__qoz_Ty ap = _qoz_ms_310->payload.TyFn.f0; qoz_Ty* ar = _qoz_ms_310->payload.TyFn.f1; qoz_Ty* _qoz_ms_323 = b; bool _qoz_mv_323 = false; switch (_qoz_ms_323->tag) { case qoz_Ty_TyFn: { qoz_Vec__qoz_Ty bp = _qoz_ms_323->payload.TyFn.f0; qoz_Ty* br = _qoz_ms_323->payload.TyFn.f1; _qoz_mv_323 = (ty_ty_args_eq(ap, bp) && ty_ty_eq(ar, br));  break; } default: { _qoz_mv_323 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_323);  break; } case qoz_Ty_TyTuple: { qoz_Vec__qoz_Ty ae = _qoz_ms_310->payload.TyTuple.f0; qoz_Ty* _qoz_ms_324 = b; bool _qoz_mv_324 = false; switch (_qoz_ms_324->tag) { case qoz_Ty_TyTuple: { qoz_Vec__qoz_Ty be = _qoz_ms_324->payload.TyTuple.f0; _qoz_mv_324 = (ty_ty_args_eq(ae, be));  break; } default: { _qoz_mv_324 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_324);  break; } case qoz_Ty_TyVar: { qoz_string an = _qoz_ms_310->payload.TyVar.f1; qoz_Ty* _qoz_ms_325 = b; bool _qoz_mv_325 = false; switch (_qoz_ms_325->tag) { case qoz_Ty_TyVar: { qoz_string bn = _qoz_ms_325->payload.TyVar.f1; _qoz_mv_325 = (qoz_string_eq(an, bn));  break; } default: { _qoz_mv_325 = (false);  break; } } _qoz_mv_310 = (_qoz_mv_325);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_310;
}

qoz_TypeExpr* ty_ty_to_type_expr_at(qoz_Ty* t, qoz_Span sp) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&t);
    qoz_Ty* _qoz_ms_326 = t; qoz_TypeExpr* _qoz_mv_326 = NULL; switch (_qoz_ms_326->tag) { case qoz_Ty_TyInt: { qoz_IntInfo info = _qoz_ms_326->payload.TyInt.f0; _qoz_mv_326 = (qoz_make_TypeExpr_TENamed(sp, ty_ints_to_path(info.width, info.is_signed), qoz_vec_make__qoz_TypeExpr()));  break; } case qoz_Ty_TyFloat: { qoz_FloatInfo info = _qoz_ms_326->payload.TyFloat.f0; qoz_TypeExpr* _qoz_bv_313;
    {
        qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); if (info.width == 32) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("f32")); }  else { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("f64")); } _qoz_bv_313 = qoz_make_TypeExpr_TENamed(sp, path, qoz_vec_make__qoz_TypeExpr());
    }
    _qoz_mv_326 = (_qoz_bv_313);  break; } case qoz_Ty_TyBool: { _qoz_mv_326 = (ty_single_path_te(sp, QOZ_STR_LIT("bool")));  break; } case qoz_Ty_TyChar: { _qoz_mv_326 = (ty_single_path_te(sp, QOZ_STR_LIT("char")));  break; } case qoz_Ty_TyString: { _qoz_mv_326 = (ty_single_path_te(sp, QOZ_STR_LIT("string")));  break; } case qoz_Ty_TyCstring: { _qoz_mv_326 = (ty_single_path_te(sp, QOZ_STR_LIT("cstring")));  break; } case qoz_Ty_TyUnit: { _qoz_mv_326 = (qoz_make_TypeExpr_TEUnit(sp));  break; } case qoz_Ty_TyNil: { _qoz_mv_326 = (ty_single_path_te(sp, QOZ_STR_LIT("nil")));  break; } case qoz_Ty_TyError: { _qoz_mv_326 = (ty_single_path_te(sp, QOZ_STR_LIT("?")));  break; } case qoz_Ty_TyPtr: { qoz_Ty* inner = _qoz_ms_326->payload.TyPtr.f0; _qoz_mv_326 = (qoz_make_TypeExpr_TEPtr(sp, ty_ty_to_type_expr_at(inner, sp)));  break; } case qoz_Ty_TyAdt: { qoz_string name = _qoz_ms_326->payload.TyAdt.f0; qoz_Vec__qoz_Ty args = _qoz_ms_326->payload.TyAdt.f1; _qoz_mv_326 = (ty_named_with_args(sp, name, args));  break; } case qoz_Ty_TyRecord: { qoz_string name = _qoz_ms_326->payload.TyRecord.f0; qoz_Vec__qoz_Ty args = _qoz_ms_326->payload.TyRecord.f1; _qoz_mv_326 = (ty_named_with_args(sp, name, args));  break; } case qoz_Ty_TyFn: { qoz_Vec__qoz_Ty fparams = _qoz_ms_326->payload.TyFn.f0; qoz_Ty* ret = _qoz_ms_326->payload.TyFn.f1; qoz_TypeExpr* _qoz_bv_314;
    {
        qoz_Vec__qoz_TypeExpr pte = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_Ty __col = fparams; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* p = __col.data[__i]; (void)p; qoz_vec_push__qoz_TypeExpr(&pte, ty_ty_to_type_expr_at(p, sp)); } }_qoz_bv_314 = qoz_make_TypeExpr_TEFn(sp, pte, ty_ty_to_type_expr_at(ret, sp));
    }
    _qoz_mv_326 = (_qoz_bv_314);  break; } case qoz_Ty_TyTuple: { qoz_Vec__qoz_Ty elems = _qoz_ms_326->payload.TyTuple.f0; qoz_TypeExpr* _qoz_bv_315;
    {
        qoz_Vec__qoz_TypeExpr ete = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_Ty __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* el = __col.data[__i]; (void)el; qoz_vec_push__qoz_TypeExpr(&ete, ty_ty_to_type_expr_at(el, sp)); } }_qoz_bv_315 = qoz_make_TypeExpr_TETuple(sp, ete);
    }
    _qoz_mv_326 = (_qoz_bv_315);  break; } case qoz_Ty_TyVar: { qoz_string name = _qoz_ms_326->payload.TyVar.f1; _qoz_mv_326 = (ty_single_path_te(sp, name));  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_326;
}

qoz_Vec__qoz_string ty_ints_to_path(int64_t width, bool is_signed) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); if (is_signed) { if (width == 8) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("i8")); } if (width == 16) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("i16")); } if (width == 32) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("i32")); } if (width == 64) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("i64")); } }  else { if (width == 8) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("u8")); } if (width == 16) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("u16")); } if (width == 32) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("u32")); } if (width == 64) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("u64")); } } if ((path.len) == 0) { qoz_vec_push__qoz_string(&path, QOZ_STR_LIT("i64")); } qoz_gc_shadow_set_top(_qoz_shadow_guard); return path;
}

qoz_TypeExpr* ty_single_path_te(qoz_Span sp, qoz_string name) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, name); qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_TypeExpr_TENamed(sp, path, qoz_vec_make__qoz_TypeExpr());
}

qoz_TypeExpr* ty_named_with_args(qoz_Span sp, qoz_string name, qoz_Vec__qoz_Ty args) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_Vec__qoz_string path = qoz_vec_make__qoz_string(); qoz_vec_push__qoz_string(&path, name); qoz_Vec__qoz_TypeExpr te_args = qoz_vec_make__qoz_TypeExpr(); { qoz_Vec__qoz_Ty __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* a = __col.data[__i]; (void)a; qoz_vec_push__qoz_TypeExpr(&te_args, ty_ty_to_type_expr_at(a, sp)); } }qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_TypeExpr_TENamed(sp, path, te_args);
}

bool ty_ty_args_eq(qoz_Vec__qoz_Ty a, qoz_Vec__qoz_Ty b) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if ((a.len) != (b.len)) { return false;} int64_t i = 0; while (i < (a.len)) { if (!ty_ty_eq(a.data[i], b.data[i])) { return false;} i = i + 1; } qoz_gc_shadow_set_top(_qoz_shadow_guard); return true;
}

qoz_string ty_ty_show(qoz_Ty* t) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&t);
    qoz_Ty* _qoz_ms_327 = t; qoz_string _qoz_mv_327 = ((qoz_string){ NULL, 0 }); switch (_qoz_ms_327->tag) { case qoz_Ty_TyInt: { qoz_IntInfo i = _qoz_ms_327->payload.TyInt.f0; qoz_string _qoz_bv_316;
    {
        qoz_string prefix = ((i.is_signed) ? QOZ_STR_LIT("i") : QOZ_STR_LIT("u")); qoz_string w = ((i.width == 8) ? QOZ_STR_LIT("8") : ((i.width == 16) ? QOZ_STR_LIT("16") : ((i.width == 32) ? QOZ_STR_LIT("32") : QOZ_STR_LIT("64")))); _qoz_bv_316 = qoz_string_cat(prefix, w);
    }
    _qoz_mv_327 = (_qoz_bv_316);  break; } case qoz_Ty_TyFloat: { qoz_FloatInfo f = _qoz_ms_327->payload.TyFloat.f0; _qoz_mv_327 = (((f.width == 32) ? QOZ_STR_LIT("f32") : QOZ_STR_LIT("f64")));  break; } case qoz_Ty_TyBool: { _qoz_mv_327 = (QOZ_STR_LIT("bool"));  break; } case qoz_Ty_TyChar: { _qoz_mv_327 = (QOZ_STR_LIT("char"));  break; } case qoz_Ty_TyString: { _qoz_mv_327 = (QOZ_STR_LIT("string"));  break; } case qoz_Ty_TyCstring: { _qoz_mv_327 = (QOZ_STR_LIT("cstring"));  break; } case qoz_Ty_TyUnit: { _qoz_mv_327 = (QOZ_STR_LIT("unit"));  break; } case qoz_Ty_TyNil: { _qoz_mv_327 = (QOZ_STR_LIT("nil"));  break; } case qoz_Ty_TyError: { _qoz_mv_327 = (QOZ_STR_LIT("<error>"));  break; } case qoz_Ty_TyVar: { qoz_string n = _qoz_ms_327->payload.TyVar.f1; _qoz_mv_327 = (n);  break; } case qoz_Ty_TyPtr: { qoz_Ty* inner = _qoz_ms_327->payload.TyPtr.f0; _qoz_mv_327 = (qoz_string_cat(QOZ_STR_LIT("*"), ty_ty_show(inner)));  break; } case qoz_Ty_TyAdt: { qoz_string n = _qoz_ms_327->payload.TyAdt.f0; qoz_Vec__qoz_Ty args = _qoz_ms_327->payload.TyAdt.f1; qoz_string _qoz_bv_317;
    {
        if ((args.len) == 0) { return n;} qoz_string out = qoz_string_cat(n, QOZ_STR_LIT("<")); int64_t i = 0; { qoz_Vec__qoz_Ty __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* a = __col.data[__i]; (void)a; if (i > 0) { out = qoz_string_cat(out, QOZ_STR_LIT(", ")); } out = qoz_string_cat(out, ty_ty_show(a)); i = i + 1; } }_qoz_bv_317 = qoz_string_cat(out, QOZ_STR_LIT(">"));
    }
    _qoz_mv_327 = (_qoz_bv_317);  break; } case qoz_Ty_TyRecord: { qoz_string n = _qoz_ms_327->payload.TyRecord.f0; qoz_Vec__qoz_Ty args = _qoz_ms_327->payload.TyRecord.f1; qoz_string _qoz_bv_318;
    {
        if ((args.len) == 0) { return n;} qoz_string out = qoz_string_cat(n, QOZ_STR_LIT("<")); int64_t i = 0; { qoz_Vec__qoz_Ty __col = args; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* a = __col.data[__i]; (void)a; if (i > 0) { out = qoz_string_cat(out, QOZ_STR_LIT(", ")); } out = qoz_string_cat(out, ty_ty_show(a)); i = i + 1; } }_qoz_bv_318 = qoz_string_cat(out, QOZ_STR_LIT(">"));
    }
    _qoz_mv_327 = (_qoz_bv_318);  break; } case qoz_Ty_TyFn: { qoz_Vec__qoz_Ty params = _qoz_ms_327->payload.TyFn.f0; qoz_Ty* ret = _qoz_ms_327->payload.TyFn.f1; qoz_string _qoz_bv_319;
    {
        qoz_string out = QOZ_STR_LIT("fn("); int64_t i = 0; { qoz_Vec__qoz_Ty __col = params; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* p = __col.data[__i]; (void)p; if (i > 0) { out = qoz_string_cat(out, QOZ_STR_LIT(", ")); } out = qoz_string_cat(out, ty_ty_show(p)); i = i + 1; } }out = qoz_string_cat(out, QOZ_STR_LIT(") -> ")); _qoz_bv_319 = qoz_string_cat(out, ty_ty_show(ret));
    }
    _qoz_mv_327 = (_qoz_bv_319);  break; } case qoz_Ty_TyTuple: { qoz_Vec__qoz_Ty elems = _qoz_ms_327->payload.TyTuple.f0; qoz_string _qoz_bv_320;
    {
        qoz_string out = QOZ_STR_LIT("("); int64_t i = 0; { qoz_Vec__qoz_Ty __col = elems; for (int64_t __i = 0; __i < __col.len; __i++) { qoz_Ty* e = __col.data[__i]; (void)e; if (i > 0) { out = qoz_string_cat(out, QOZ_STR_LIT(", ")); } out = qoz_string_cat(out, ty_ty_show(e)); i = i + 1; } }_qoz_bv_320 = qoz_string_cat(out, QOZ_STR_LIT(")"));
    }
    _qoz_mv_327 = (_qoz_bv_320);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_327;
}

bool ty_arg_is_nil_accepted_by(qoz_Ty* param) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&param);
    qoz_Ty* _qoz_ms_328 = param; bool _qoz_mv_328 = false; switch (_qoz_ms_328->tag) { case qoz_Ty_TyPtr: { _qoz_mv_328 = (true);  break; } default: { _qoz_mv_328 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_328;
}

bool ty_untyped_int_fits(qoz_Ty* param) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&param);
    qoz_Ty* _qoz_ms_329 = param; bool _qoz_mv_329 = false; switch (_qoz_ms_329->tag) { case qoz_Ty_TyInt: { _qoz_mv_329 = (true);  break; } default: { _qoz_mv_329 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_329;
}

bool ty_untyped_float_fits(qoz_Ty* param) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&param);
    qoz_Ty* _qoz_ms_330 = param; bool _qoz_mv_330 = false; switch (_qoz_ms_330->tag) { case qoz_Ty_TyFloat: { _qoz_mv_330 = (true);  break; } default: { _qoz_mv_330 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_330;
}

bool ty_int_widens_to(qoz_IntInfo param_int, qoz_IntInfo arg_int) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    if (param_int.is_signed != arg_int.is_signed) { return false;} qoz_gc_shadow_set_top(_qoz_shadow_guard); return arg_int.width <= param_int.width;
}

bool ty_arg_passes_to_param(qoz_Ty* param, qoz_Ty* arg) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&param);
    qoz_gc_push_root(&arg);
    qoz_Ty* _qoz_ms_331 = arg; bool _qoz_mv_331 = false; switch (_qoz_ms_331->tag) { case qoz_Ty_TyError: { _qoz_mv_331 = (true);  break; } case qoz_Ty_TyNil: { _qoz_mv_331 = (ty_arg_is_nil_accepted_by(param));  break; } case qoz_Ty_TyInt: { qoz_IntInfo ai = _qoz_ms_331->payload.TyInt.f0; bool _qoz_bv_321;
    {
        if (ai.untyped && ty_untyped_int_fits(param)) { return true;} qoz_Ty* _qoz_ms_332 = param; bool _qoz_mv_332 = false; switch (_qoz_ms_332->tag) { case qoz_Ty_TyInt: { qoz_IntInfo pi = _qoz_ms_332->payload.TyInt.f0; _qoz_mv_332 = (ty_int_widens_to(pi, ai));  break; } default: { _qoz_mv_332 = (false);  break; } } _qoz_bv_321 = _qoz_mv_332;
    }
    _qoz_mv_331 = (_qoz_bv_321);  break; } case qoz_Ty_TyFloat: { qoz_FloatInfo af = _qoz_ms_331->payload.TyFloat.f0; bool _qoz_bv_322;
    {
        if (af.untyped && ty_untyped_float_fits(param)) { return true;} qoz_Ty* _qoz_ms_333 = param; bool _qoz_mv_333 = false; switch (_qoz_ms_333->tag) { case qoz_Ty_TyFloat: { qoz_FloatInfo pf = _qoz_ms_333->payload.TyFloat.f0; _qoz_mv_333 = (pf.width >= af.width);  break; } default: { _qoz_mv_333 = (false);  break; } } _qoz_bv_322 = _qoz_mv_333;
    }
    _qoz_mv_331 = (_qoz_bv_322);  break; } default: { _qoz_mv_331 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_331;
}

bool ty_same_constructor_assignable(qoz_Ty* param, qoz_Ty* arg) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&param);
    qoz_gc_push_root(&arg);
    qoz_Ty* _qoz_ms_334 = param; bool _qoz_mv_334 = false; switch (_qoz_ms_334->tag) { case qoz_Ty_TyAdt: { qoz_string pn = _qoz_ms_334->payload.TyAdt.f0; qoz_Vec__qoz_Ty pa = _qoz_ms_334->payload.TyAdt.f1; qoz_Ty* _qoz_ms_335 = arg; bool _qoz_mv_335 = false; switch (_qoz_ms_335->tag) { case qoz_Ty_TyAdt: { qoz_string an = _qoz_ms_335->payload.TyAdt.f0; qoz_Vec__qoz_Ty aa = _qoz_ms_335->payload.TyAdt.f1; bool _qoz_bv_323;
    {
        if (!qoz_string_eq(pn, an)) { return false;} if ((pa.len) != (aa.len)) { return false;} int64_t i = 0; while (i < (pa.len)) { if (!ty_ty_assignable(pa.data[i], aa.data[i])) { return false;} i = i + 1; } _qoz_bv_323 = true;
    }
    _qoz_mv_335 = (_qoz_bv_323);  break; } default: { _qoz_mv_335 = (false);  break; } } _qoz_mv_334 = (_qoz_mv_335);  break; } case qoz_Ty_TyRecord: { qoz_string pn = _qoz_ms_334->payload.TyRecord.f0; qoz_Vec__qoz_Ty pa = _qoz_ms_334->payload.TyRecord.f1; qoz_Ty* _qoz_ms_336 = arg; bool _qoz_mv_336 = false; switch (_qoz_ms_336->tag) { case qoz_Ty_TyRecord: { qoz_string an = _qoz_ms_336->payload.TyRecord.f0; qoz_Vec__qoz_Ty aa = _qoz_ms_336->payload.TyRecord.f1; bool _qoz_bv_324;
    {
        if (!qoz_string_eq(pn, an)) { return false;} if ((pa.len) != (aa.len)) { return false;} int64_t i = 0; while (i < (pa.len)) { if (!ty_ty_assignable(pa.data[i], aa.data[i])) { return false;} i = i + 1; } _qoz_bv_324 = true;
    }
    _qoz_mv_336 = (_qoz_bv_324);  break; } default: { _qoz_mv_336 = (false);  break; } } _qoz_mv_334 = (_qoz_mv_336);  break; } case qoz_Ty_TyPtr: { qoz_Ty* pi = _qoz_ms_334->payload.TyPtr.f0; qoz_Ty* _qoz_ms_337 = arg; bool _qoz_mv_337 = false; switch (_qoz_ms_337->tag) { case qoz_Ty_TyPtr: { qoz_Ty* ai = _qoz_ms_337->payload.TyPtr.f0; _qoz_mv_337 = (ty_ty_assignable(pi, ai));  break; } default: { _qoz_mv_337 = (false);  break; } } _qoz_mv_334 = (_qoz_mv_337);  break; } default: { _qoz_mv_334 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_334;
}

bool ty_ty_assignable(qoz_Ty* param, qoz_Ty* arg) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&param);
    qoz_gc_push_root(&arg);
    if (ty_ty_eq(param, arg)) { return true;} if (ty_arg_passes_to_param(param, arg)) { return true;} qoz_Ty* _qoz_ms_338 = arg; switch (_qoz_ms_338->tag) { case qoz_Ty_TyVar: { return true; break; } default: { NULL;  break; } } 0; qoz_Ty* _qoz_ms_339 = param; switch (_qoz_ms_339->tag) { case qoz_Ty_TyVar: { return true; break; } default: { NULL;  break; } } 0; if (ty_same_constructor_assignable(param, arg)) { return true;} qoz_Ty* _qoz_ms_340 = param; bool _qoz_mv_340 = false; switch (_qoz_ms_340->tag) { case qoz_Ty_TyError: { _qoz_mv_340 = (true);  break; } case qoz_Ty_TyVar: { _qoz_mv_340 = (true);  break; } default: { _qoz_mv_340 = (false);  break; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return _qoz_mv_340;
}

qoz_Vec__qoz_string qoz_vec_make__qoz_string(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_string){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_string(qoz_Vec__qoz_string* v, qoz_string x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_string(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

bool qoz_map_contains__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return false;} int64_t idx = qoz_map_probe__qoz_string__bool(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return true;} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

qoz_Vec__qoz_Expr qoz_vec_make__qoz_Expr(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Expr){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Expr(qoz_Vec__qoz_Expr* v, qoz_Expr* x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    qoz_gc_push_root(&x);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Expr(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_RecordFieldLit qoz_vec_make__qoz_RecordFieldLit(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_RecordFieldLit){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_RecordFieldLit(qoz_Vec__qoz_RecordFieldLit* v, qoz_RecordFieldLit x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_RecordFieldLit(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_Stmt qoz_vec_make__qoz_Stmt(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Stmt){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Stmt(qoz_Vec__qoz_Stmt* v, qoz_Stmt* x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    qoz_gc_push_root(&x);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Stmt(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_MatchArm qoz_vec_make__qoz_MatchArm(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_MatchArm){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_MatchArm(qoz_Vec__qoz_MatchArm* v, qoz_MatchArm x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_MatchArm(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_Pending qoz_vec_make__qoz_Pending(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Pending){ .data = NULL, .len = 0, .cap = 0 });
}

qoz_Map__qoz_string__bool qoz_map_make__qoz_string__bool(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Map__qoz_string__bool){ .slots = NULL, .len = 0, .cap = 0 });
}

qoz_Vec__qoz_Decl qoz_vec_make__qoz_Decl(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Decl){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Pending(qoz_Vec__qoz_Pending* v, qoz_Pending x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Pending(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

void qoz_map_set__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key, bool value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_map_insert__qoz_string__bool(m, key, value);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

void qoz_map_insert__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key, bool value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { qoz_map_grow__qoz_string__bool(m); } if (m->len * 2 >= m->cap) { qoz_map_grow__qoz_string__bool(m); } qoz_map_insert_raw__qoz_string__bool(m, key, value); 
    return;
}

void qoz_vec_push__qoz_Decl(qoz_Vec__qoz_Decl* v, qoz_Decl* x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    qoz_gc_push_root(&x);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Decl(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Option__bool* qoz_map_get__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return qoz_make_Option__bool_None();} int64_t idx = qoz_map_probe__qoz_string__bool(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return qoz_make_Option__bool_Some(m->slots[idx].value);} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Option__bool_None();
}

qoz_Vec__qoz_Token qoz_vec_make__qoz_Token(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Token){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Token(qoz_Vec__qoz_Token* v, qoz_Token x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Token(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_TypeExpr qoz_vec_make__qoz_TypeExpr(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_TypeExpr){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_TypeExpr(qoz_Vec__qoz_TypeExpr* v, qoz_TypeExpr* x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    qoz_gc_push_root(&x);
    if (v->len == v->cap) { qoz_vec_grow__qoz_TypeExpr(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_ClosureParam qoz_vec_make__qoz_ClosureParam(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_ClosureParam){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_ClosureParam(qoz_Vec__qoz_ClosureParam* v, qoz_ClosureParam x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_ClosureParam(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_Pattern qoz_vec_make__qoz_Pattern(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Pattern){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Pattern(qoz_Vec__qoz_Pattern* v, qoz_Pattern* x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    qoz_gc_push_root(&x);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Pattern(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_StructField qoz_vec_make__qoz_StructField(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_StructField){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_StructField(qoz_Vec__qoz_StructField* v, qoz_StructField x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_StructField(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_VariantDecl qoz_vec_make__qoz_VariantDecl(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_VariantDecl){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_VariantDecl(qoz_Vec__qoz_VariantDecl* v, qoz_VariantDecl x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_VariantDecl(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Vec__qoz_FnParam qoz_vec_make__qoz_FnParam(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_FnParam){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_FnParam(qoz_Vec__qoz_FnParam* v, qoz_FnParam x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_FnParam(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

qoz_Map__qoz_string__qoz_Decl qoz_map_make__qoz_string__qoz_Decl(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Map__qoz_string__qoz_Decl){ .slots = NULL, .len = 0, .cap = 0 });
}

qoz_Map__qoz_string__qoz_string qoz_map_make__qoz_string__qoz_string(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Map__qoz_string__qoz_string){ .slots = NULL, .len = 0, .cap = 0 });
}

qoz_Vec__qoz_TypeError qoz_vec_make__qoz_TypeError(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_TypeError){ .data = NULL, .len = 0, .cap = 0 });
}

qoz_Map__int64_t__qoz_TypeExpr qoz_map_make__int64_t__qoz_TypeExpr(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Map__int64_t__qoz_TypeExpr){ .slots = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_TypeError(qoz_Vec__qoz_TypeError* v, qoz_TypeError x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_TypeError(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

void qoz_map_set__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key, qoz_Decl* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    qoz_map_insert__qoz_string__qoz_Decl(m, key, value);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

void qoz_map_insert__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key, qoz_Decl* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    if (m->cap == 0) { qoz_map_grow__qoz_string__qoz_Decl(m); } if (m->len * 2 >= m->cap) { qoz_map_grow__qoz_string__qoz_Decl(m); } qoz_map_insert_raw__qoz_string__qoz_Decl(m, key, value); 
    return;
}

void qoz_map_set__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key, qoz_string value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_map_insert__qoz_string__qoz_string(m, key, value);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

void qoz_map_insert__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key, qoz_string value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { qoz_map_grow__qoz_string__qoz_string(m); } if (m->len * 2 >= m->cap) { qoz_map_grow__qoz_string__qoz_string(m); } qoz_map_insert_raw__qoz_string__qoz_string(m, key, value); 
    return;
}

qoz_Vec__qoz_Ty qoz_vec_make__qoz_Ty(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Ty){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Ty(qoz_Vec__qoz_Ty* v, qoz_Ty* x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    qoz_gc_push_root(&x);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Ty(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

bool qoz_map_contains__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return false;} int64_t idx = qoz_map_probe__qoz_string__qoz_Decl(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return true;} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

qoz_Vec__qoz_Binding qoz_vec_make__qoz_Binding(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Binding){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Binding(qoz_Vec__qoz_Binding* v, qoz_Binding x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Binding(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

void qoz_map_set__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key, qoz_TypeExpr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    qoz_map_insert__int64_t__qoz_TypeExpr(m, key, value);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

void qoz_map_insert__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key, qoz_TypeExpr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    if (m->cap == 0) { qoz_map_grow__int64_t__qoz_TypeExpr(m); } if (m->len * 2 >= m->cap) { qoz_map_grow__int64_t__qoz_TypeExpr(m); } qoz_map_insert_raw__int64_t__qoz_TypeExpr(m, key, value); 
    return;
}

qoz_Option__qoz_Decl* qoz_map_get__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return qoz_make_Option__qoz_Decl_None();} int64_t idx = qoz_map_probe__qoz_string__qoz_Decl(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return qoz_make_Option__qoz_Decl_Some(m->slots[idx].value);} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Option__qoz_Decl_None();
}

qoz_Option__qoz_string* qoz_map_get__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return qoz_make_Option__qoz_string_None();} int64_t idx = qoz_map_probe__qoz_string__qoz_string(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return qoz_make_Option__qoz_string_Some(m->slots[idx].value);} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Option__qoz_string_None();
}

qoz_Map__qoz_string__qoz_Ty qoz_map_make__qoz_string__qoz_Ty(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Map__qoz_string__qoz_Ty){ .slots = NULL, .len = 0, .cap = 0 });
}

qoz_Option__qoz_Ty* qoz_map_get__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return qoz_make_Option__qoz_Ty_None();} int64_t idx = qoz_map_probe__qoz_string__qoz_Ty(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return qoz_make_Option__qoz_Ty_Some(m->slots[idx].value);} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Option__qoz_Ty_None();
}

void qoz_map_set__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key, qoz_Ty* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    qoz_map_insert__qoz_string__qoz_Ty(m, key, value);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

qoz_Vec__qoz_Instantiation qoz_vec_make__qoz_Instantiation(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Instantiation){ .data = NULL, .len = 0, .cap = 0 });
}

qoz_Map__qoz_string__qoz_TypeExpr qoz_map_make__qoz_string__qoz_TypeExpr(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Map__qoz_string__qoz_TypeExpr){ .slots = NULL, .len = 0, .cap = 0 });
}

qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr qoz_map_make__qoz_string__qoz_Vec__qoz_TypeExpr(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr){ .slots = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Instantiation(qoz_Vec__qoz_Instantiation* v, qoz_Instantiation x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Instantiation(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

void qoz_map_set__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key, qoz_TypeExpr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    qoz_map_insert__qoz_string__qoz_TypeExpr(m, key, value);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

void qoz_map_insert__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key, qoz_TypeExpr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    if (m->cap == 0) { qoz_map_grow__qoz_string__qoz_TypeExpr(m); } if (m->len * 2 >= m->cap) { qoz_map_grow__qoz_string__qoz_TypeExpr(m); } qoz_map_insert_raw__qoz_string__qoz_TypeExpr(m, key, value); 
    return;
}

void qoz_map_set__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key, qoz_Vec__qoz_TypeExpr value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_map_insert__qoz_string__qoz_Vec__qoz_TypeExpr(m, key, value);
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return;
}

void qoz_map_insert__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key, qoz_Vec__qoz_TypeExpr value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { qoz_map_grow__qoz_string__qoz_Vec__qoz_TypeExpr(m); } if (m->len * 2 >= m->cap) { qoz_map_grow__qoz_string__qoz_Vec__qoz_TypeExpr(m); } qoz_map_insert_raw__qoz_string__qoz_Vec__qoz_TypeExpr(m, key, value); 
    return;
}

bool qoz_map_contains__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return false;} int64_t idx = qoz_map_probe__qoz_string__qoz_string(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return true;} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

bool qoz_map_contains__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return false;} int64_t idx = qoz_map_probe__qoz_string__qoz_TypeExpr(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return true;} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

qoz_Option__qoz_TypeExpr* qoz_map_get__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return qoz_make_Option__qoz_TypeExpr_None();} int64_t idx = qoz_map_probe__qoz_string__qoz_TypeExpr(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return qoz_make_Option__qoz_TypeExpr_Some(m->slots[idx].value);} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Option__qoz_TypeExpr_None();
}

qoz_Option__qoz_TypeExpr* qoz_map_get__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return qoz_make_Option__qoz_TypeExpr_None();} int64_t idx = qoz_map_probe__int64_t__qoz_TypeExpr(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return qoz_make_Option__qoz_TypeExpr_Some(m->slots[idx].value);} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Option__qoz_TypeExpr_None();
}

qoz_Option__qoz_Vec__qoz_TypeExpr* qoz_map_get__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return qoz_make_Option__qoz_Vec__qoz_TypeExpr_None();} int64_t idx = qoz_map_probe__qoz_string__qoz_Vec__qoz_TypeExpr(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return qoz_make_Option__qoz_Vec__qoz_TypeExpr_Some(m->slots[idx].value);} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return qoz_make_Option__qoz_Vec__qoz_TypeExpr_None();
}

qoz_Vec__qoz_Vec__qoz_StructField qoz_vec_make__qoz_Vec__qoz_StructField(void) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_shadow_set_top(_qoz_shadow_guard); return ((qoz_Vec__qoz_Vec__qoz_StructField){ .data = NULL, .len = 0, .cap = 0 });
}

void qoz_vec_push__qoz_Vec__qoz_StructField(qoz_Vec__qoz_Vec__qoz_StructField* v, qoz_Vec__qoz_StructField x) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    if (v->len == v->cap) { qoz_vec_grow__qoz_Vec__qoz_StructField(v); } v->data[v->len] = x; v->len = v->len + 1; 
    return;
}

bool qoz_map_contains__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    if (m->cap == 0) { return false;} int64_t idx = qoz_map_probe__qoz_string__qoz_Vec__qoz_TypeExpr(m, key); if (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { return true;} } qoz_gc_shadow_set_top(_qoz_shadow_guard); return false;
}

void qoz_vec_grow__qoz_string(qoz_Vec__qoz_string* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_string* new_data = ((qoz_string*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_string))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

int64_t qoz_map_probe__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    uint64_t h = qoz_string_hash(key); int64_t idx = ((int64_t)h % ((uint64_t)m->cap)); while (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { if (qoz_string_eq(m->slots[idx].key, key)) { return idx;} } idx = idx + 1; if (idx == m->cap) { idx = 0; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return idx;
}

void qoz_vec_grow__qoz_Expr(qoz_Vec__qoz_Expr* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Expr** new_data = ((qoz_Expr**)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Expr*))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_RecordFieldLit(qoz_Vec__qoz_RecordFieldLit* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_RecordFieldLit* new_data = ((qoz_RecordFieldLit*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_RecordFieldLit))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_Stmt(qoz_Vec__qoz_Stmt* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Stmt** new_data = ((qoz_Stmt**)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Stmt*))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_MatchArm(qoz_Vec__qoz_MatchArm* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_MatchArm* new_data = ((qoz_MatchArm*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_MatchArm))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_Pending(qoz_Vec__qoz_Pending* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Pending* new_data = ((qoz_Pending*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Pending))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_map_grow__qoz_string__bool(qoz_Map__qoz_string__bool* m) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_Slot__qoz_string__bool* old_slots = m->slots; qoz_gc_push_root(&old_slots); int64_t old_cap = m->cap; int64_t new_cap = ((m->cap == 0) ? 8 : m->cap * 2); qoz_Slot__qoz_string__bool* new_slots = ((qoz_Slot__qoz_string__bool*)qoz_calloc(new_cap * (int64_t)sizeof(qoz_Slot__qoz_string__bool))); qoz_gc_push_root(&new_slots); m->slots = new_slots; m->cap = new_cap; m->len = 0; int64_t j = 0; while (j < old_cap) { qoz_Slot__qoz_string__bool slot = old_slots[j]; if (slot.occupied) { if (!slot.deleted) { qoz_map_insert_raw__qoz_string__bool(m, slot.key, slot.value); } } j = j + 1; } 
    return;
}

void qoz_map_insert_raw__qoz_string__bool(qoz_Map__qoz_string__bool* m, qoz_string key, bool value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    int64_t idx = qoz_map_probe__qoz_string__bool(m, key); bool was_occupied = m->slots[idx].occupied; m->slots[idx] = ((qoz_Slot__qoz_string__bool){ .key = key, .value = value, .occupied = true, .deleted = false }); if (!was_occupied) { m->len = m->len + 1; } 
    return;
}

void qoz_vec_grow__qoz_Decl(qoz_Vec__qoz_Decl* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Decl** new_data = ((qoz_Decl**)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Decl*))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_Token(qoz_Vec__qoz_Token* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Token* new_data = ((qoz_Token*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Token))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_TypeExpr(qoz_Vec__qoz_TypeExpr* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_TypeExpr** new_data = ((qoz_TypeExpr**)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_TypeExpr*))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_ClosureParam(qoz_Vec__qoz_ClosureParam* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_ClosureParam* new_data = ((qoz_ClosureParam*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_ClosureParam))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_Pattern(qoz_Vec__qoz_Pattern* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Pattern** new_data = ((qoz_Pattern**)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Pattern*))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_StructField(qoz_Vec__qoz_StructField* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_StructField* new_data = ((qoz_StructField*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_StructField))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_VariantDecl(qoz_Vec__qoz_VariantDecl* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_VariantDecl* new_data = ((qoz_VariantDecl*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_VariantDecl))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_FnParam(qoz_Vec__qoz_FnParam* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_FnParam* new_data = ((qoz_FnParam*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_FnParam))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_vec_grow__qoz_TypeError(qoz_Vec__qoz_TypeError* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_TypeError* new_data = ((qoz_TypeError*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_TypeError))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_map_grow__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_Slot__qoz_string__qoz_Decl* old_slots = m->slots; qoz_gc_push_root(&old_slots); int64_t old_cap = m->cap; int64_t new_cap = ((m->cap == 0) ? 8 : m->cap * 2); qoz_Slot__qoz_string__qoz_Decl* new_slots = ((qoz_Slot__qoz_string__qoz_Decl*)qoz_calloc(new_cap * (int64_t)sizeof(qoz_Slot__qoz_string__qoz_Decl))); qoz_gc_push_root(&new_slots); m->slots = new_slots; m->cap = new_cap; m->len = 0; int64_t j = 0; while (j < old_cap) { qoz_Slot__qoz_string__qoz_Decl slot = old_slots[j]; if (slot.occupied) { if (!slot.deleted) { qoz_map_insert_raw__qoz_string__qoz_Decl(m, slot.key, slot.value); } } j = j + 1; } 
    return;
}

void qoz_map_insert_raw__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key, qoz_Decl* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    int64_t idx = qoz_map_probe__qoz_string__qoz_Decl(m, key); bool was_occupied = m->slots[idx].occupied; m->slots[idx] = ((qoz_Slot__qoz_string__qoz_Decl){ .key = key, .value = value, .occupied = true, .deleted = false }); if (!was_occupied) { m->len = m->len + 1; } 
    return;
}

void qoz_map_grow__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_Slot__qoz_string__qoz_string* old_slots = m->slots; qoz_gc_push_root(&old_slots); int64_t old_cap = m->cap; int64_t new_cap = ((m->cap == 0) ? 8 : m->cap * 2); qoz_Slot__qoz_string__qoz_string* new_slots = ((qoz_Slot__qoz_string__qoz_string*)qoz_calloc(new_cap * (int64_t)sizeof(qoz_Slot__qoz_string__qoz_string))); qoz_gc_push_root(&new_slots); m->slots = new_slots; m->cap = new_cap; m->len = 0; int64_t j = 0; while (j < old_cap) { qoz_Slot__qoz_string__qoz_string slot = old_slots[j]; if (slot.occupied) { if (!slot.deleted) { qoz_map_insert_raw__qoz_string__qoz_string(m, slot.key, slot.value); } } j = j + 1; } 
    return;
}

void qoz_map_insert_raw__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key, qoz_string value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    int64_t idx = qoz_map_probe__qoz_string__qoz_string(m, key); bool was_occupied = m->slots[idx].occupied; m->slots[idx] = ((qoz_Slot__qoz_string__qoz_string){ .key = key, .value = value, .occupied = true, .deleted = false }); if (!was_occupied) { m->len = m->len + 1; } 
    return;
}

void qoz_vec_grow__qoz_Ty(qoz_Vec__qoz_Ty* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Ty** new_data = ((qoz_Ty**)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Ty*))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

int64_t qoz_map_probe__qoz_string__qoz_Decl(qoz_Map__qoz_string__qoz_Decl* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    uint64_t h = qoz_string_hash(key); int64_t idx = ((int64_t)h % ((uint64_t)m->cap)); while (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { if (qoz_string_eq(m->slots[idx].key, key)) { return idx;} } idx = idx + 1; if (idx == m->cap) { idx = 0; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return idx;
}

void qoz_vec_grow__qoz_Binding(qoz_Vec__qoz_Binding* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Binding* new_data = ((qoz_Binding*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Binding))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_map_grow__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_Slot__int64_t__qoz_TypeExpr* old_slots = m->slots; qoz_gc_push_root(&old_slots); int64_t old_cap = m->cap; int64_t new_cap = ((m->cap == 0) ? 8 : m->cap * 2); qoz_Slot__int64_t__qoz_TypeExpr* new_slots = ((qoz_Slot__int64_t__qoz_TypeExpr*)qoz_calloc(new_cap * (int64_t)sizeof(qoz_Slot__int64_t__qoz_TypeExpr))); qoz_gc_push_root(&new_slots); m->slots = new_slots; m->cap = new_cap; m->len = 0; int64_t j = 0; while (j < old_cap) { qoz_Slot__int64_t__qoz_TypeExpr slot = old_slots[j]; if (slot.occupied) { if (!slot.deleted) { qoz_map_insert_raw__int64_t__qoz_TypeExpr(m, slot.key, slot.value); } } j = j + 1; } 
    return;
}

void qoz_map_insert_raw__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key, qoz_TypeExpr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    int64_t idx = qoz_map_probe__int64_t__qoz_TypeExpr(m, key); bool was_occupied = m->slots[idx].occupied; m->slots[idx] = ((qoz_Slot__int64_t__qoz_TypeExpr){ .key = key, .value = value, .occupied = true, .deleted = false }); if (!was_occupied) { m->len = m->len + 1; } 
    return;
}

int64_t qoz_map_probe__qoz_string__qoz_string(qoz_Map__qoz_string__qoz_string* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    uint64_t h = qoz_string_hash(key); int64_t idx = ((int64_t)h % ((uint64_t)m->cap)); while (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { if (qoz_string_eq(m->slots[idx].key, key)) { return idx;} } idx = idx + 1; if (idx == m->cap) { idx = 0; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return idx;
}

int64_t qoz_map_probe__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    uint64_t h = qoz_string_hash(key); int64_t idx = ((int64_t)h % ((uint64_t)m->cap)); while (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { if (qoz_string_eq(m->slots[idx].key, key)) { return idx;} } idx = idx + 1; if (idx == m->cap) { idx = 0; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return idx;
}

void qoz_map_insert__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key, qoz_Ty* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    if (m->cap == 0) { qoz_map_grow__qoz_string__qoz_Ty(m); } if (m->len * 2 >= m->cap) { qoz_map_grow__qoz_string__qoz_Ty(m); } qoz_map_insert_raw__qoz_string__qoz_Ty(m, key, value); 
    return;
}

void qoz_vec_grow__qoz_Instantiation(qoz_Vec__qoz_Instantiation* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Instantiation* new_data = ((qoz_Instantiation*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Instantiation))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_map_grow__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_Slot__qoz_string__qoz_TypeExpr* old_slots = m->slots; qoz_gc_push_root(&old_slots); int64_t old_cap = m->cap; int64_t new_cap = ((m->cap == 0) ? 8 : m->cap * 2); qoz_Slot__qoz_string__qoz_TypeExpr* new_slots = ((qoz_Slot__qoz_string__qoz_TypeExpr*)qoz_calloc(new_cap * (int64_t)sizeof(qoz_Slot__qoz_string__qoz_TypeExpr))); qoz_gc_push_root(&new_slots); m->slots = new_slots; m->cap = new_cap; m->len = 0; int64_t j = 0; while (j < old_cap) { qoz_Slot__qoz_string__qoz_TypeExpr slot = old_slots[j]; if (slot.occupied) { if (!slot.deleted) { qoz_map_insert_raw__qoz_string__qoz_TypeExpr(m, slot.key, slot.value); } } j = j + 1; } 
    return;
}

void qoz_map_insert_raw__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key, qoz_TypeExpr* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    int64_t idx = qoz_map_probe__qoz_string__qoz_TypeExpr(m, key); bool was_occupied = m->slots[idx].occupied; m->slots[idx] = ((qoz_Slot__qoz_string__qoz_TypeExpr){ .key = key, .value = value, .occupied = true, .deleted = false }); if (!was_occupied) { m->len = m->len + 1; } 
    return;
}

void qoz_map_grow__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr* old_slots = m->slots; qoz_gc_push_root(&old_slots); int64_t old_cap = m->cap; int64_t new_cap = ((m->cap == 0) ? 8 : m->cap * 2); qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr* new_slots = ((qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr*)qoz_calloc(new_cap * (int64_t)sizeof(qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr))); qoz_gc_push_root(&new_slots); m->slots = new_slots; m->cap = new_cap; m->len = 0; int64_t j = 0; while (j < old_cap) { qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr slot = old_slots[j]; if (slot.occupied) { if (!slot.deleted) { qoz_map_insert_raw__qoz_string__qoz_Vec__qoz_TypeExpr(m, slot.key, slot.value); } } j = j + 1; } 
    return;
}

void qoz_map_insert_raw__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key, qoz_Vec__qoz_TypeExpr value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    int64_t idx = qoz_map_probe__qoz_string__qoz_Vec__qoz_TypeExpr(m, key); bool was_occupied = m->slots[idx].occupied; m->slots[idx] = ((qoz_Slot__qoz_string__qoz_Vec__qoz_TypeExpr){ .key = key, .value = value, .occupied = true, .deleted = false }); if (!was_occupied) { m->len = m->len + 1; } 
    return;
}

int64_t qoz_map_probe__qoz_string__qoz_TypeExpr(qoz_Map__qoz_string__qoz_TypeExpr* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    uint64_t h = qoz_string_hash(key); int64_t idx = ((int64_t)h % ((uint64_t)m->cap)); while (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { if (qoz_string_eq(m->slots[idx].key, key)) { return idx;} } idx = idx + 1; if (idx == m->cap) { idx = 0; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return idx;
}

int64_t qoz_map_probe__int64_t__qoz_TypeExpr(qoz_Map__int64_t__qoz_TypeExpr* m, int64_t key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    uint64_t h = (uint64_t)(key); int64_t idx = ((int64_t)h % ((uint64_t)m->cap)); while (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { if (m->slots[idx].key == key) { return idx;} } idx = idx + 1; if (idx == m->cap) { idx = 0; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return idx;
}

int64_t qoz_map_probe__qoz_string__qoz_Vec__qoz_TypeExpr(qoz_Map__qoz_string__qoz_Vec__qoz_TypeExpr* m, qoz_string key) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    uint64_t h = qoz_string_hash(key); int64_t idx = ((int64_t)h % ((uint64_t)m->cap)); while (m->slots[idx].occupied) { if (!m->slots[idx].deleted) { if (qoz_string_eq(m->slots[idx].key, key)) { return idx;} } idx = idx + 1; if (idx == m->cap) { idx = 0; } } qoz_gc_shadow_set_top(_qoz_shadow_guard); return idx;
}

void qoz_vec_grow__qoz_Vec__qoz_StructField(qoz_Vec__qoz_Vec__qoz_StructField* v) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&v);
    int64_t new_cap = ((v->cap == 0) ? 8 : v->cap * 2); qoz_Vec__qoz_StructField* new_data = ((qoz_Vec__qoz_StructField*)qoz_realloc(((void*)v->data), new_cap * (int64_t)sizeof(qoz_Vec__qoz_StructField))); qoz_gc_push_root(&new_data); v->data = new_data; v->cap = new_cap; 
    return;
}

void qoz_map_grow__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_Slot__qoz_string__qoz_Ty* old_slots = m->slots; qoz_gc_push_root(&old_slots); int64_t old_cap = m->cap; int64_t new_cap = ((m->cap == 0) ? 8 : m->cap * 2); qoz_Slot__qoz_string__qoz_Ty* new_slots = ((qoz_Slot__qoz_string__qoz_Ty*)qoz_calloc(new_cap * (int64_t)sizeof(qoz_Slot__qoz_string__qoz_Ty))); qoz_gc_push_root(&new_slots); m->slots = new_slots; m->cap = new_cap; m->len = 0; int64_t j = 0; while (j < old_cap) { qoz_Slot__qoz_string__qoz_Ty slot = old_slots[j]; if (slot.occupied) { if (!slot.deleted) { qoz_map_insert_raw__qoz_string__qoz_Ty(m, slot.key, slot.value); } } j = j + 1; } 
    return;
}

void qoz_map_insert_raw__qoz_string__qoz_Ty(qoz_Map__qoz_string__qoz_Ty* m, qoz_string key, qoz_Ty* value) {
    int64_t _qoz_shadow_guard = qoz_gc_shadow_top();
    qoz_gc_push_root(&m);
    qoz_gc_push_root(&value);
    int64_t idx = qoz_map_probe__qoz_string__qoz_Ty(m, key); bool was_occupied = m->slots[idx].occupied; m->slots[idx] = ((qoz_Slot__qoz_string__qoz_Ty){ .key = key, .value = value, .occupied = true, .deleted = false }); if (!was_occupied) { m->len = m->len + 1; } 
    return;
}

