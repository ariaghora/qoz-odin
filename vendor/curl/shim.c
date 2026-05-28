// Typed wrappers around `curl_easy_setopt` so the Qoz binding can
// pass each option with a sized argument. The macro hides a
// variadic call in C; the wrappers fix the third argument's type
// per option family.
//
// Also: a tiny growable byte buffer plus a curl write callback
// that appends into it. The Qoz binding owns the buffer through
// opaque pointers and copies the bytes out into a Qoz string
// after the transfer completes.
//
// All symbols are appended into libcurl.a at vendor time.

#include <stdlib.h>
#include <string.h>
#include "include/curl/curl.h"

int qoz_curl_setopt_str(CURL *h, CURLoption opt, const char *s) {
    return curl_easy_setopt(h, opt, s);
}

int qoz_curl_setopt_long(CURL *h, CURLoption opt, long v) {
    return curl_easy_setopt(h, opt, v);
}

int qoz_curl_setopt_ptr(CURL *h, CURLoption opt, void *p) {
    return curl_easy_setopt(h, opt, p);
}

int qoz_curl_setopt_off(CURL *h, CURLoption opt, curl_off_t v) {
    return curl_easy_setopt(h, opt, v);
}

int qoz_curl_getinfo_long(CURL *h, CURLINFO info, long *out) {
    return curl_easy_getinfo(h, info, out);
}

int qoz_curl_getinfo_str(CURL *h, CURLINFO info, char **out) {
    return curl_easy_getinfo(h, info, out);
}

typedef struct {
    char *data;
    size_t len;
    size_t cap;
} qoz_curl_buf;

qoz_curl_buf *qoz_curl_buf_new(void) {
    qoz_curl_buf *b = (qoz_curl_buf *)calloc(1, sizeof(qoz_curl_buf));
    return b;
}

size_t qoz_curl_buf_write_cb(char *ptr, size_t size, size_t nmemb, void *userdata) {
    size_t n = size * nmemb;
    qoz_curl_buf *b = (qoz_curl_buf *)userdata;
    if (b->len + n + 1 > b->cap) {
        size_t cap = b->cap == 0 ? 4096 : b->cap * 2;
        while (cap < b->len + n + 1) cap *= 2;
        char *nd = (char *)realloc(b->data, cap);
        if (!nd) return 0;
        b->data = nd;
        b->cap = cap;
    }
    memcpy(b->data + b->len, ptr, n);
    b->len += n;
    b->data[b->len] = '\0';
    return n;
}

const char *qoz_curl_buf_data(qoz_curl_buf *b) { return b->data ? b->data : ""; }
long        qoz_curl_buf_len(qoz_curl_buf *b)  { return (long)b->len; }
void        qoz_curl_buf_free(qoz_curl_buf *b) {
    if (!b) return;
    free(b->data);
    free(b);
}

// `curl_easy_setopt` for the write callback. The callback symbol
// is the shim's own `qoz_curl_buf_write_cb`. Wrapper isolates the
// function-pointer cast from the Qoz binding.
int qoz_curl_setopt_writecb_default(CURL *h) {
    return curl_easy_setopt(h, CURLOPT_WRITEFUNCTION, qoz_curl_buf_write_cb);
}
