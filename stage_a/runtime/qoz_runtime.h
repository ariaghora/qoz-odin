#ifndef QOZ_RUNTIME_H
#define QOZ_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
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

/* File system */
qoz_string qoz_fs_read_file(qoz_string path);
bool       qoz_fs_write_file(qoz_string path, qoz_string content);
bool       qoz_fs_file_exists(qoz_string path);

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

/* Format builder */
typedef struct { char *buf; int64_t len; int64_t cap; } qoz_strbuf;
void       qoz_strbuf_init(qoz_strbuf *b);
void       qoz_strbuf_append_str(qoz_strbuf *b, qoz_string s);
void       qoz_strbuf_append_cstr(qoz_strbuf *b, const char *s);
void       qoz_strbuf_append_i64(qoz_strbuf *b, int64_t v);
void       qoz_strbuf_append_f64(qoz_strbuf *b, double v);
void       qoz_strbuf_append_bool(qoz_strbuf *b, bool v);
qoz_string qoz_strbuf_finish(qoz_strbuf *b);

#define QOZ_STR_LIT(s) ((qoz_string){ (s), (int64_t)(sizeof(s) - 1) })

#endif
