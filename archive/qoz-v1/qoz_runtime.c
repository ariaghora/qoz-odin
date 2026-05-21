#ifndef QOZ_RUNTIME_H
#define QOZ_RUNTIME_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
  const char *data;
  size_t len;
} Qoz_String;

#endif