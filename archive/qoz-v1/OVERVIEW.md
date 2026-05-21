# Qoz Language Overview

Qoz is a systems programming language that compiles to C. It provides explicit control over memory allocation.

## Design Philosophy

**Explicit Memory Management**  
All heap allocations require an explicit allocator. There are no hidden allocations or implicit constructors. The `new()` and `del()` functions make memory management visible at the call site.

**Minimal Runtime**  
Qoz compiles to portable C code with a minimal runtime. The C output serves as a compilation target and intermediate representation.

**Systems Programming Features**  
The language provides raw pointers, C interop, and explicit performance control.

## Type System

### Primitive Types
- Integers: `i8`, `u8`, `i32`, `i64`
- Floating point: `f32`, `f64`
- Other: `bool`, `void`

### Composite Types
- **Fixed arrays**: `arr<T, N>` - stack-allocated, fixed size
- **Dynamic arrays**: `vec<T>` - heap-allocated, growable
- **Pointers**: `*T` - nullable pointers to any type
- **Structs**: Named fields, stack or heap allocated

### String Types
- `string` - Qoz native string (pointer + length)
- `cstring` - C-compatible null-terminated string
- String literals are untyped and coerce based on context

## Memory Management

### Allocators
Qoz uses explicit allocators for all heap operations. The standard library provides `mem.Allocator`:

```qoz
import "std:mem"

main := func() {
    alloc := mem.default_allocator()
    
    p := new(Person, alloc)
    p.name = "Alice"
    del(p, alloc)
}
```

### Dynamic Arrays
The `vec<T>` type is a growable array that embeds its allocator:

```qoz
v := new(vec<i32>, alloc)
defer del(v)  // Uses embedded allocator

append(v, 10)
append(v, 20)

for item in v {
    println("item:", item)
}
```

The `del()` function for vectors does not require an explicit allocator parameter, as the allocator is stored within the vec structure.

### Design Notes
- **No heap container literals**: Heap-allocated containers like `vec<T>` do not support literal syntax (e.g., `vec{1, 2, 3}`) as this would hide allocation. Stack-allocated arrays support literals: `arr<i32, 3>{1, 2, 3}`
- **Embedded allocators in containers**: `vec<T>` embeds its allocator so `append()` can operate without passing allocators explicitly
- **No allocators in value types**: `string` does not embed an allocator, keeping the size at 16 bytes

## Control Flow

### Loops
```qoz
// C-style for loop
for i := 0; i < 10; i += 1 {
    println(i)
}

// While loop
while condition {
    // body
}

// For-in loop (arrays and vecs)
for item in collection {
    println(item)
}
```

### Defer
Defer statements execute when the current scope exits:

```qoz
f := open_file("data.txt")
defer close_file(f)

// File automatically closed at scope exit
```

## C Interoperability

External C functions are declared with the `external` keyword:

```qoz
malloc := func(size: i64): *void external("malloc")
free   := func(ptr: *void): void external("free")
```

For C library bindings, use `cstring` for string parameters:

```qoz
printf := func(fmt: cstring): void external("printf")

main := func() {
    printf("Hello, World\n")  // String literal coerces to cstring
}
```

## Package System

Qoz supports a simple package system with qualified imports:

```qoz
import "std:mem"
import "vendor:raylib"

main := func() {
    alloc := mem.default_allocator()
    raylib.init_window(800, 600, "Demo")
}
```

## Current Status

**Implemented:**
- Core language features (functions, structs, pointers, arrays)
- Dynamic arrays (`vec<T>`)
- Explicit memory management (`new`, `del`, allocators)
- Control flow (for, while, for-in, defer)
- C interop (external functions, cstring)
- Basic standard library (mem module)

**In Development:**
- Hash maps (`map<K, V>`)
- Additional standard library modules
- Compiler optimizations

## Example: Dynamic Array

```qoz
import "std:mem"

main := func() {
    alloc := mem.default_allocator()
    
    // Create a vector
    v := new(vec<i32>, alloc)
    defer del(v)
    
    // Append some elements
    append(v, 10)
    append(v, 20)
    append(v, 30)
    
    println("Vector length:", len(v))
    
    // Access by index
    println("v[0] =", v[0])
    println("v[1] =", v[1])
    println("v[2] =", v[2])
    
    // Iterate
    for item in v {
        println("  item:", item)
    }
}
```

## Implementation

The Qoz compiler is written in Odin and generates C code. The compilation pipeline:

1. **Tokenization**: Source code to tokens
2. **Parsing**: Tokens to abstract syntax tree (AST)
3. **Semantic Analysis**: Type checking, scope resolution, validation
4. **Code Generation**: AST to C code
5. **C Compilation**: Generated C compiled with system C compiler

The generated C code serves as a portable compilation target, leveraging existing C toolchains for optimization and platform support.

