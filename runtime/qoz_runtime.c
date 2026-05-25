#include "qoz_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/time.h>

static int qoz_argc_val = 0;
static char **qoz_argv_val = NULL;

void qoz_set_argv(int argc, char **argv) {
    qoz_argc_val = argc;
    qoz_argv_val = argv;
}

int64_t qoz_time_unix(void) {
    struct timeval tv;
    if (gettimeofday(&tv, NULL) != 0) return 0;
    return (int64_t)tv.tv_sec;
}

int64_t qoz_time_unix_micros(void) {
    struct timeval tv;
    if (gettimeofday(&tv, NULL) != 0) return 0;
    return (int64_t)tv.tv_sec * 1000000 + (int64_t)tv.tv_usec;
}

int64_t qoz_os_argc(void) { return (int64_t)qoz_argc_val; }

qoz_string qoz_os_arg(int64_t i) {
    if (i < 0 || i >= (int64_t)qoz_argc_val) return (qoz_string){ NULL, 0 };
    const char *s = qoz_argv_val[i];
    return (qoz_string){ s, (int64_t)strlen(s) };
}

void qoz_os_exit(int64_t code) { exit((int)code); }

/* Qoz call-frame tracker. Each entered Qoz function pushes a static
 * string describing its name; the runtime keeps the names in a
 * fixed-size array. qoz_panic walks the array to produce a Qoz-level
 * backtrace. The implementation is portable C11 with no platform
 * extensions. */
#define QOZ_FRAME_CAP 1024
static const char *qoz_frame_stack[QOZ_FRAME_CAP];
static int64_t qoz_frame_top = 0;

void qoz_frame_push(const char *name) {
    if (qoz_frame_top < QOZ_FRAME_CAP) {
        qoz_frame_stack[qoz_frame_top] = name;
    }
    qoz_frame_top++;
}

void qoz_frame_pop(void) {
    if (qoz_frame_top > 0) qoz_frame_top--;
}

void qoz_panic(qoz_string msg) {
    fputs("qoz: panic: ", stderr);
    if (msg.len > 0) fwrite(msg.data, 1, (size_t)msg.len, stderr);
    fputc('\n', stderr);
    if (qoz_frame_top > 0) {
        fputs("backtrace (most recent call first):\n", stderr);
        int64_t shown = qoz_frame_top;
        if (shown > QOZ_FRAME_CAP) shown = QOZ_FRAME_CAP;
        for (int64_t i = shown - 1; i >= 0; i--) {
            const char *n = qoz_frame_stack[i];
            if (n == NULL) n = "<unknown>";
            fprintf(stderr, "  at %s\n", n);
        }
        if (qoz_frame_top > QOZ_FRAME_CAP) {
            fprintf(stderr, "  ... %" PRId64 " more frames truncated\n",
                    qoz_frame_top - QOZ_FRAME_CAP);
        }
    }
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
    if (n == 0) { fclose(f); return (qoz_string){ NULL, 0, NULL }; }
    char *data = (char *)qoz_alloc((int64_t)n);
    if (data == NULL) { fclose(f); return (qoz_string){ NULL, -1 }; }
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

/* Format a double into the buffer using snprintf("%g", v). The
 * buffer layout matches std/strings::Strbuf so a Qoz-side strbuf can
 * be passed in directly. */
void qoz_strbuf_append_f64(void *bv, double v) {
    qoz_strbuf *b = (qoz_strbuf *)bv;
    char tmp[64];
    int n = snprintf(tmp, sizeof(tmp), "%g", v);
    if (n > 0) {
        int64_t new_cap = b->cap == 0 ? 64 : b->cap;
        while (new_cap < b->len + n) new_cap *= 2;
        if (new_cap > b->cap) {
            b->buf = (char *)qoz_realloc(b->buf, new_cap);
            b->cap = new_cap;
        }
        memcpy(b->buf + b->len, tmp, (size_t)n);
        b->len += n;
    }
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
     * shutdown sweep frees them. A negative size is a programmer bug
     * (typically an arithmetic underflow); fail loudly rather than
     * silently producing a huge size_t and returning NULL. */
    if (size < 0) {
        qoz_panic((qoz_string){"qoz_alloc: negative size", 24, NULL});
    }
    return qoz_gc_alloc(size, NULL);
}

void *qoz_calloc(int64_t size) {
    /* qoz_gc_alloc already zero-fills. */
    if (size < 0) {
        qoz_panic((qoz_string){"qoz_calloc: negative size", 25, NULL});
    }
    return qoz_gc_alloc(size, NULL);
}

void *qoz_realloc(void *ptr, int64_t size) {
    if (size < 0) {
        qoz_panic((qoz_string){"qoz_realloc: negative size", 26, NULL});
    }
    if (ptr == NULL) return qoz_gc_alloc(size, NULL);
    /* Alloc-and-copy. The GC tracks the new allocation and the old
     * block is freed only after the copy completes. Vec / Map and
     * other callers retain a reference to the new pointer. */
    int64_t old_size = qoz_gc_alloc_size(ptr);
    void *p = qoz_gc_alloc(size, NULL);
    if (p == NULL) {
        /* New allocation failed. Leave the old block alive so the
         * caller can keep using it (or fail safely) rather than
         * destroying their data. */
        return NULL;
    }
    if (old_size > 0) {
        int64_t copy = old_size < size ? old_size : size;
        memcpy(p, ptr, (size_t)copy);
    }
    qoz_gc_free(ptr);
    return p;
}

/* Byte-equality and FNV-1a hash on qoz_string. The compiler's
 * auto-derived record eq/hash calls these directly so a record type
 * containing string fields works without an explicit `import
 * std/strings`. User-level `==` and `hash` on `string` go through the
 * std/strings @operator dispatch, which calls into here as well. */
int64_t qoz_string_byte_at(qoz_string s, int64_t i) {
    if (i < 0 || i >= s.len) {
        qoz_panic((qoz_string){"string index out of bounds", 26, NULL});
    }
    return (int64_t)(unsigned char)s.data[i];
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
    if (s.len > 0) {
        fwrite(s.data, 1, (size_t)s.len, stdout);
    }
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
#include <poll.h>

/* Append `got` bytes from `chunk` into a growing buffer. Doubles `cap`
 * when needed. Allocations go through qoz_alloc so the result stays
 * reachable through the GC for the caller's qoz_string. */
static char *qoz_buf_append(char *buf, int64_t *cap, int64_t *n, const char *chunk, int64_t got) {
    if (*n + got > *cap) {
        int64_t new_cap = *cap;
        if (new_cap < 4096) new_cap = 4096;
        while (new_cap < *n + got) new_cap *= 2;
        char *nb = (char *)qoz_alloc(new_cap);
        if (nb == NULL) return buf;
        if (*n > 0) memcpy(nb, buf, (size_t)*n);
        buf = nb;
        *cap = new_cap;
    }
    memcpy(buf + *n, chunk, (size_t)got);
    *n += got;
    return buf;
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

    /* Parent. Close the write ends and drain both pipes concurrently
     * through poll(); a sequential drain deadlocks when the child
     * writes more than one pipe-buffer to whichever stream is read
     * second. */
    close(out_pipe[1]);
    close(err_pipe[1]);

    char *o_buf = NULL; int64_t o_cap = 0; int64_t o_n = 0;
    char *e_buf = NULL; int64_t e_cap = 0; int64_t e_n = 0;
    struct pollfd fds[2];
    fds[0].fd = out_pipe[0]; fds[0].events = POLLIN;
    fds[1].fd = err_pipe[0]; fds[1].events = POLLIN;
    int open_count = 2;
    char chunk[4096];
    while (open_count > 0) {
        int pr = poll(fds, 2, -1);
        if (pr < 0) {
            if (errno == EINTR) continue;
            break;
        }
        for (int i = 0; i < 2; i++) {
            if (fds[i].fd < 0) continue;
            if (fds[i].revents & (POLLIN | POLLHUP | POLLERR)) {
                ssize_t got = read(fds[i].fd, chunk, sizeof(chunk));
                if (got > 0) {
                    if (i == 0) o_buf = qoz_buf_append(o_buf, &o_cap, &o_n, chunk, (int64_t)got);
                    else        e_buf = qoz_buf_append(e_buf, &e_cap, &e_n, chunk, (int64_t)got);
                } else if (got == 0 || (got < 0 && errno != EINTR && errno != EAGAIN)) {
                    close(fds[i].fd);
                    fds[i].fd = -1;
                    open_count--;
                }
            }
        }
    }
    if (fds[0].fd >= 0) close(fds[0].fd);
    if (fds[1].fd >= 0) close(fds[1].fd);

    *out_stdout = (qoz_string){ o_buf, o_n, o_buf };
    *out_stderr = (qoz_string){ e_buf, e_n, e_buf };

    int status = 0;
    while (waitpid(pid, &status, 0) < 0) {
        if (errno != EINTR) { *out_exit = -1; return; }
    }
    if (WIFEXITED(status)) {
        *out_exit = (int64_t)WEXITSTATUS(status);
    } else if (WIFSIGNALED(status)) {
        /* Child killed by signal: report 128 + signal, matching the
         * shell convention. Distinguishes from exec failure (-1). */
        *out_exit = (int64_t)(128 + WTERMSIG(status));
    } else {
        *out_exit = -1;
    }
}
