#include "qoz_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

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

void qoz_strbuf_init(qoz_strbuf *b) {
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

void qoz_strbuf_append_str(qoz_strbuf *b, qoz_string s) {
    if (s.len <= 0) return;
    if (b->len + s.len > b->cap) qoz_strbuf_grow(b, s.len);
    memcpy(b->buf + b->len, s.data, (size_t)s.len);
    b->len += s.len;
}

void qoz_strbuf_append_cstr(qoz_strbuf *b, const char *s) {
    qoz_string ss = { s, (int64_t)strlen(s) };
    qoz_strbuf_append_str(b, ss);
}

void qoz_strbuf_append_i64(qoz_strbuf *b, int64_t v) {
    char tmp[32];
    int n = snprintf(tmp, sizeof(tmp), "%" PRId64, v);
    if (n > 0) {
        if (b->len + n > b->cap) qoz_strbuf_grow(b, n);
        memcpy(b->buf + b->len, tmp, (size_t)n);
        b->len += n;
    }
}

void qoz_strbuf_append_f64(qoz_strbuf *b, double v) {
    char tmp[64];
    int n = snprintf(tmp, sizeof(tmp), "%g", v);
    if (n > 0) {
        if (b->len + n > b->cap) qoz_strbuf_grow(b, n);
        memcpy(b->buf + b->len, tmp, (size_t)n);
        b->len += n;
    }
}

void qoz_strbuf_append_bool(qoz_strbuf *b, bool v) {
    qoz_string s = v ? (qoz_string){ "true", 4 } : (qoz_string){ "false", 5 };
    qoz_strbuf_append_str(b, s);
}

qoz_string qoz_strbuf_finish(qoz_strbuf *b) {
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
