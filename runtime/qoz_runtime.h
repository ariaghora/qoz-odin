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
     * through tgc's exact-match scan. Equal to data for fresh
     * allocations, inherited from the source for slices, NULL for
     * static literals. */
    const char *root;
} qoz_string;

void qoz_init(int *stack_anchor);
void qoz_shutdown(void);

/* Allocation. Every Qoz heap object goes through these wrappers around
 * the tgc collector. */
void *qoz_alloc(int64_t size);
void *qoz_calloc(int64_t size);
void *qoz_realloc(void *ptr, int64_t size);

/* OS bridge. argv access is set up once at program start; getenv
 * reads the live environment; exit terminates the process. */
void       qoz_set_argv(int argc, char **argv);
int64_t    qoz_os_argc(void);
qoz_string qoz_os_arg(int64_t i);
void       qoz_os_exit(int64_t code);
qoz_string qoz_os_getenv(qoz_string name);

/* Unrecoverable abort. Prints a one-line diagnostic to stderr (the
 * message and a fixed prefix) and calls abort(). Use for programmer
 * bugs such as broken invariants, exhausted match arms reaching a
 * default, or counter limits that should never be hit. Recoverable
 * errors flow through Result<T, E> with the `try` operator. */
_Noreturn void qoz_panic(qoz_string msg);

/* Wall-clock time. `qoz_time_unix` returns whole seconds since the
 * Unix epoch. `qoz_time_unix_micros` returns microseconds since the
 * epoch. Both go through POSIX `gettimeofday`. */
int64_t qoz_time_unix(void);
int64_t qoz_time_unix_micros(void);

/* Qoz-level call-frame tracker. The compiler emits push at function
 * entry and pop before each return; qoz_panic walks the frame stack
 * to print a Qoz-level backtrace on abort. */
void qoz_frame_push(const char *name);
void qoz_frame_pop(void);

/* Byte-level string equality and FNV-1a hash. The compiler's
 * auto-derived record eq/hash emits direct calls to these helpers so
 * a file does not need to import std/strings to compare records that
 * contain string fields. User code reaches the same behaviour through
 * the @operator dispatch on `string`, implemented in std/strings. */
bool     qoz_string_eq(qoz_string a, qoz_string b);
uint64_t qoz_string_hash(qoz_string s);

/* Byte access for `s[i]` expressions. The compiler emits a direct
 * call to this helper so `s[i]` works without importing strings;
 * returns the byte as i64 (zero-extended). Out-of-range indices
 * return 0, mirroring the user-level byte_at helper. */
int64_t  qoz_string_byte_at(qoz_string s, int64_t i);

/* File system. read_file returns a qoz_string with len == -1 on
 * failure. write_file returns true on success. list_qoz_files returns
 * the regular `.qoz` filenames in `dir`, sorted, joined by newline,
 * with no trailing newline. Empty result means the directory cannot
 * be opened. */
qoz_string qoz_fs_read_file(qoz_string path);
bool       qoz_fs_write_file(qoz_string path, qoz_string content);
bool       qoz_fs_file_exists(qoz_string path);
qoz_string qoz_fs_list_qoz_files(qoz_string dir);

/* String byte-buffer access for std/strings. `qoz_string_data`
 * returns the raw data pointer of a string. `qoz_string_alias`
 * constructs a string that aliases `n` bytes at `buf`; the string's
 * root field is set so the GC keeps the allocation reachable.
 * `qoz_bytes_copy` is a portable memcpy wrapper, used here because on
 * darwin memcpy expands through a fortify-source macro that Qoz
 * cannot reference directly through its FFI. */
void      *qoz_string_data(qoz_string s);
qoz_string qoz_string_alias(void *buf, int64_t n);
void       qoz_bytes_copy(void *dst, void *src, int64_t n);

/* Stream output primitives that the compiler emits for `fmt.println`
 * and friends. Each takes a single value and writes its rendered
 * bytes to stdout. */
void qoz_print_str(qoz_string s);
void qoz_print_cstr(const char *s);
void qoz_print_i64(int64_t v);
void qoz_print_i32(int32_t v);
void qoz_print_f64(double v);
void qoz_print_bool(bool v);
void qoz_print_sep(void);
void qoz_print_nl(void);
/* Print a string followed by a newline and flush stdout. Backs the
 * single-argument std/fmt::println. */
void qoz_print_line(qoz_string s);

/* Growable byte buffer used by string interpolation. The struct
 * layout is shared with std/strings::Strbuf so the same value can
 * round-trip between Qoz code and the libm-backed f64 appender below. */
typedef struct { char *buf; int64_t len; int64_t cap; } qoz_strbuf;
/* Format a double into the buffer using snprintf("%g", v). Lives in
 * C because Qoz does not yet have a floating-point printer. */
void       qoz_strbuf_append_f64(void *b, double v);

#define QOZ_STR_LIT(s) ((qoz_string){ (s), (int64_t)(sizeof(s) - 1) })

/* Fork-and-exec a child process, capture its stdout and stderr into
 * Qoz strings, and report the child's exit status. `argv` points to
 * an array of `n` qoz_string values, the first of which is the
 * program to launch and the rest its arguments. The output pointers
 * must point to valid storage. On return they hold the captured
 * streams. The exit code is the process's raw exit value, or -1 if
 * the spawn itself failed. */
void qoz_process_exec(qoz_string *argv, int64_t n,
                      int64_t *out_exit,
                      qoz_string *out_stdout,
                      qoz_string *out_stderr);

#endif
