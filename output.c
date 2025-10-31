#include <stdio.h>
#include <stdint.h>

void* malloc(int32_t size);
typedef struct Vec2D Vec2D;
typedef struct Foo Foo;
typedef struct Bar Bar;
typedef struct Node Node;
int32_t main(void);

typedef struct Vec2D {
    int32_t x;
    int32_t y;
} Vec2D;

typedef struct Foo {
    Foo position;
} Foo;

typedef struct Bar {
    Bar position;
} Bar;

typedef struct Node {
    Vec2D value;
} Node;

int32_t main(void) {
    Vec2D pos = {.x = 10, .y = 20};
    Node root = {.value = pos};
    printf("%d\n", root.value.x);
    return 0;
}

