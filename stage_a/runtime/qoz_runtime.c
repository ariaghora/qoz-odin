#include "qoz_runtime.h"
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
