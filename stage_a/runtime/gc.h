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
 * Auto-collection is paused at startup. Programs that need true GC
 * behaviour call qoz_gc_resume() and qoz_gc_run() explicitly. Shutdown
 * frees everything regardless.
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

void qoz_gc_pause(void);
void qoz_gc_resume(void);
bool qoz_gc_is_paused(void);

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
