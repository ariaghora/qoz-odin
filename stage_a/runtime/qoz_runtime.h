#ifndef QOZ_RUNTIME_H
#define QOZ_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <math.h>
#include "gc.h"

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
