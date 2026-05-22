# Qoz Language Specification

Qoz is a statically typed, garbage-collected systems language that compiles to C. It is pragmatic ML in feel: algebraic data types, pattern matching, type inference, first-class functions and closures, with mutation allowed where useful.

## Design Principles

- Pragmatic ML semantics. ADTs, pattern matching, and closures are central. Strict-FP dogma is rejected.
- Strict static typing with bidirectional inference. Annotations are required only where inference would be ambiguous.
- Clean, regular syntax. Less noise than OCaml. One way to spell each construct.
- Garbage collection via tgc (Daniel Holden's conservative mark-and-sweep). The user never sees an allocator.
- Compiles to portable C. Clang is the backend assembler.

---

## Lexical

- Identifiers: `[A-Za-z_][A-Za-z0-9_]*`. Type names conventionally start uppercase.
- Integer literals: decimal `42`, hex `0xff`, binary `0b1010`, octal `0o755`. Untyped by default, coerce to context. Underscores `_` may appear between digits and are ignored: `1_000_000`, `0xff_ff`.
- Float literals: `1.0`, `3.14`, `1e9`, `2.5e-3`. Underscores are ignored as in integer literals.
- String literals: `"hello\n"`. UTF-8 bytes.
- Character literals: `'a'`, `'\n'`. A single byte.
- Boolean literals: `true`, `false`.
- Comments: `// line` and `/* block */`. Block comments nest.
- Statements are terminated by newline. A trailing comma in any list is permitted. A long expression may be split across lines either by ending the previous line with the operator (`x = a +\n    b`) or by starting the next line with the operator (`x = a\n    + b`); both are accepted.

---

## Bindings

```
let x = 10              // immutable, type inferred
let y: i64 = 20         // immutable, type annotated
var counter = 0         // mutable
counter = counter + 1
```

`let` binds an immutable name. `var` binds a mutable name. There is no third form.

A `let` whose left-hand side is a refutable pattern (a variant constructor, a record pattern, or any pattern that may fail to match) requires an `else` clause and is described in the [Let-Else](#let-else) section below.

---

## Primitive Types

| Type | Width | Notes |
|------|-------|-------|
| `i8`, `i16`, `i32`, `i64` | signed integers | |
| `u8`, `u16`, `u32`, `u64` | unsigned integers | |
| `f32`, `f64` | floats | IEEE 754 |
| `bool` | 1 byte | `true` or `false` |
| `char` | 1 byte | single byte; multibyte UTF-8 handling is library code |
| `string` | pointer + length | immutable UTF-8 bytes |
| `cstring` | `const char *` | NUL-terminated C string for FFI; string literals coerce to this when context expects it |
| `unit` | zero-size | the type of expressions with no value |

---

## Type Declarations

A single keyword `type` introduces every named type. The right-hand side picks the shape.

### Records

```
type Point = { x: f64, y: f64 }

let p = Point { x: 1.0, y: 2.0 }
let q = Point { x: 0.0, ..p }       // partial update: q.y == p.y
let dx = p.x

var r = Point { x: 0.0, y: 0.0 }
r.x = 5.0
```

A record's field is mutable when the binding holding the record is `var`. There is no per-field mutability annotation. The `..base` form copies the remaining fields from another record of the same type.

### Algebraic Data Types

```
type Option<T> =
    | None
    | Some(T)

type Shape =
    | Circle(f64)
    | Rectangle { width: f64, height: f64 }
    | Origin
```

Variants may carry zero payloads, positional payloads, or named-field payloads. Construction uses `Type.Variant`:

```
let a = Some(42)
let b: Option<i32> = None
let s = Shape.Rectangle { width: 3.0, height: 4.0 }
let c = Shape.Circle(1.5)
```

Variants are accessed only through pattern matching.

### Type Aliases

```
type Vec3 = (f64, f64, f64)
type Name = string
```

A `type` whose right-hand side is a single type expression (with no `|` and no `{`) is an alias. Aliases are transparent: `Vec3` and `(f64, f64, f64)` are interchangeable.

---

## Tuples

```
let pair: (i32, string) = (42, "hello")
let (n, s) = pair
let divmod = (a, b) -> (a / b, a % b)
```

Tuples are anonymous, positional, and used for multiple returns and ad-hoc grouping. They are not records.

---

## Operators

Built-in operators on primitive types:

| Category | Operators | Operand types |
|----------|-----------|---------------|
| Arithmetic | `+`, `-`, `*`, `/`, `%` | integers and floats |
| Comparison | `==`, `!=`, `<`, `>`, `<=`, `>=` | integers, floats, `bool`, `char`, `string`, `cstring` |
| Logical | `&&`, `\|\|`, `!` | `bool` |
| Bitwise | `&`, `\|`, `^`, `<<`, `>>` | integer types only |
| Unary | `-`, `!`, `*` (deref), `&` (address-of) | per type |
| Cast | `expr as T` | numeric, pointer, and primitive conversions |

Bitwise `|` and match-arm `|` share the token; inside a `match` arm body, a bare `|` is treated as the next arm separator. Parenthesise to use bitwise `|` inside a match arm: `match c { | x -> (x | 0xff) }`.

The right-shift token `>>` is recognised by the parser as two `>` tokens when they are column-adjacent. This avoids ambiguity with nested generic type arguments such as `Vec<Vec<i32>>`.

Records and ADTs gain operators through `@operator` registrations and structural derivation; see [Operator Overloading](#operator-overloading).

---

## Pattern Matching

```
let describe(s: Shape): string =
    match s {
    | Shape.Circle(r)                          -> fmt.format("circle r={}", r)
    | Shape.Rectangle { width: w, height: h }  -> fmt.format("rect {}x{}", w, h)
    | Shape.Origin                             -> "origin"
    }

let result =
    match opt {
    | Some(v) if v > 0  -> v * 2
    | Some(_)           -> 0
    | None              -> -1
    }
```

Match is exhaustive. The compiler rejects matches that do not cover all variants. Patterns include:

- Wildcards: `_`
- Bindings: identifier (binds the matched value)
- Literals: `0`, `"hello"`, `true`
- Variant patterns: `Some(p)`, `Shape.Rectangle { width: w, height: h }`
- Tuple patterns: `(a, b)`
- Guard clauses: `pattern if expr -> body`
- Multi-pattern arms: `| A | B | C -> body`. The same body runs when any listed pattern matches. Every alternative must bind the same names to compatible types so the body sees a consistent environment. Useful for collapsing variants that share an action: `| EInt(sp, _) | EFloat(sp, _) | EString(sp, _) -> sp`.

Patterns nest. Each arm starts with `|` and uses `->`. Arms are enclosed in braces.

A `match` on an ADT scrutinee must cover every variant. The compiler rejects a non-exhaustive match and lists the missing variants. Add the missing arms, or include a wildcard arm (`| _ -> ...`) as a catch-all.

---

## Let-Else

```
let unwrap_or_zero(opt: Option): i64 = {
    let Option.Some(v) = opt else {
        return 0
    }
    v
}
```

`let pat = expr else { ... }` is a statement that binds the names from `pat` when the value of `expr` matches `pat`, and runs the else block otherwise. The bound names enter the surrounding scope. The form is for the common case of "extract one shape from a value, or bail out."

Rules:

- `pat` must be a refutable pattern (a variant constructor, a record pattern, or any pattern that can fail to match). A simple identifier is not a refutable pattern; use `let name = expr` for that.
- The else block must diverge. It must end with `return`, with another diverging expression, or with a call to a function known to never return. The compiler rejects an else block that can fall through.
- On the falling-through path, the bindings introduced by `pat` are unconditionally valid. The diverging-else rule is what makes this sound.
- Multiple `let-else` statements compose without nesting. Each runs in the surrounding block, each contributes its own bindings, and each gets to bail out independently.

```
let parse_pair(s: string): i32 = {
    let Result.Ok(parts) = split(s, ",") else { return -1 }
    let Result.Ok(a)     = parse_int(parts[0]) else { return -2 }
    let Result.Ok(b)     = parse_int(parts[1]) else { return -3 }
    a + b
}
```

The form composes cleanly with the `?` operator (see [Error Handling](#error-handling)). Use `?` when the error type matches between the called function and the enclosing function. Use `let-else` when the error needs to be transformed or when the failure path runs custom recovery code.

---

## Functions

```
let add(x: i32, y: i32): i32 = x + y

let greet(name: string) =                       // no return type means unit
    println("hello", name)

let identity<T>(x: T): T = x

let main() = {
    let n = add(2, 3)
    println(n)
}
```

A function is a `let` binding whose right-hand side is `(params): T = expr` (or `(params) = expr` for unit). The body can be any expression. Multi-statement bodies use a block `{ ... }`. The last expression in a block is the block's value. `return expr` exits early.

Function definitions are top-level. Local helpers use closures.

---

## Closures

```
let inc = (x) -> x + 1
let n = inc(5)                                  // 6

let doubled = vec.map((x) -> x * 2, v)
let positives = vec.filter((x: i32) -> x > 0, v)
let sum = vec.fold((acc, x) -> acc + x, 0, v)
```

Closures use a parenthesised parameter list followed by `->` and a body expression. Parameters may carry type annotations: `(x: i32, y: i32): i32 -> x + y`. The parser distinguishes a closure from a parenthesised expression by looking ahead one token past the matching `)`: a trailing `->` (or `:` for the return type) marks a closure.

Closures capture surrounding bindings by reference when they are mutable, by value when they are immutable. The captured environment lives on the heap and is managed by the garbage collector.

A function type spelled `(T1, T2, ..., Tn) -> R` is the type of any callable value of matching arity and signature. A closure literal and a top-level function with the same signature are interchangeable as values of this type.

A closure value is represented as a pair `(env, fn)`. The environment is a heap-allocated record containing the captured bindings, allocated by the garbage collector. A plain top-level function used in a closure-typed slot is wrapped by an adapter thunk whose environment is `NULL`. Closures without captures use the same representation with a `NULL` environment.

---

## Control Flow

```
let label =
    if x > 0      { "positive" }
    else if x < 0 { "negative" }
    else          { "zero" }

while counter < 10 {
    counter = counter + 1
}

for c in collection {
    println(c)
}

for i in 0..<10 {                               // 0, 1, ..., 9
    print(i)
}

for i in 0..=10 {                               // 0, 1, ..., 10
    print(i)
}
```

`if` is an expression. Both arms must have the same type. `while` and `for` are statements; their value is `unit`.

### If-Let

```
if let Some(v) = map.get(&m, key) {
    use(v)
} else {
    handle_missing()
}
```

`if let pattern = expr { ... } else { ... }` desugars to a two-arm `match` whose first arm is `pattern -> then_block` and whose second arm is `_ -> else_block`. The else clause is optional; when omitted, the value of the form when no match occurs is `unit`. The form composes nested `Option` and `Result` access without forcing the user to write a full `match` for a single shape of interest.

### Ranges

Ranges have two explicit forms:

- `a..<b` is the half-open range from `a` (inclusive) to `b` (exclusive).
- `a..=b` is the closed range from `a` to `b`, both inclusive.

There is no implicit-bound `..` range. The form must be explicit. The `..` token, on its own, is reserved for the record spread operator (`..base`).

---

## Error Handling

```
type Result<T, E> =
    | Ok(T)
    | Err(E)

let parse_pair(s: string): Result<(i32, i32), ParseError> = {
    let parts = strings.split(s, ",")
    let a = parse_int(parts[0])?
    let b = parse_int(parts[1])?
    Ok((a, b))
}
```

`?` on a `Result<T, E>` expression unwraps `Ok(v)` to `v` and early-returns `Err(e)` from the enclosing function. The enclosing function's return type must be `Result<_, E>`.

---

## Defer

```
let read_two_files(): Result<string, FsError> = {
    let f = fs.open("a.txt")?
    defer fs.close(f)
    let g = fs.open("b.txt")?
    defer fs.close(g)
    let combined = fs.read_all(f) + fs.read_all(g)
    Ok(combined)
}
```

`defer expr` runs `expr` when the enclosing block exits, including through `return`, error propagation, or normal fall-through. Defers run in reverse declaration order. `defer` is block-scoped, not function-scoped.

---

## Packages

A package is a directory under the source tree. The directory name is the package name. The package's source lives in a file named after the directory (`std/vec/vec.qoz`, `std/map/map.qoz`, and so on).

```
import std/fmt
import std/vec
import std/map

let main() = {
    var v: Vec<i32> = vec.make()
    vec.push(&v, 1)
    fmt.println("len:", vec.len(&v))
}
```

`import path` brings the package's last path segment into scope as a namespace. `std/vec` becomes the name `vec`. `import path as alias` overrides the binding. Calls and references that cross packages are qualified with the package name: `vec.make`, `map.insert`, `strings.eq_raw`. The dot is name resolution; it is not method dispatch.

Inside a package, references to sibling declarations are unqualified. A function in `std/vec/vec.qoz` calls `grow(v)`, not `vec.grow(v)`. From outside the package, the same function is called as `vec.grow(&v)`. Resolution is fall-through: bare name first, then package-prefixed.

Types live in a flat namespace: `Vec<T>` is accessible by its bare name from any file that imports the package. Functions live under the package prefix and require it from outside.

Name collisions across packages are resolved by the qualifier. A program can `import std/vec` and `import std/map` without `vec.len` and `map.len` clashing, even though both functions are named `len` inside their packages.

---

## Memory Model

All heap memory is owned by the garbage collector. There is no `free`, no allocator parameter, no manual deallocation.

- Algebraic data types are reference types. Every value of an ADT is a heap reference under the hood, allocated by its variant constructor. A variant may carry other ADTs of the same or different type without ceremony: `type Tree<T> = | Leaf | Node(Tree<T>, T, Tree<T>)` works. The user writes `Expr.Add(a, b)`, never `new Expr.Add(...)`.
- Records are value types. `Point { x: 1.0, y: 2.0 }` is a stack value; assignment copies. Use `new Point { ... }` to obtain a heap pointer to a record.
- Container growth (`Vec.push`, `Map.insert`) calls into the collector.
- Closures heap-allocate their captured environment.
- The compiler is permitted to stack-allocate ADT values whose lifetime is provably bounded by their scope, but this is an optimisation; the user model is reference semantics.

Pointers `*T` exist for FFI and for explicit indirection over non-ADT types. User code rarely needs them. `*T` is nullable; `nil` is the null pointer. Dereferencing nil is undefined behaviour, matching C.

Field access through a pointer uses `.`, identical to the value-receiver form. `p.x` works whether `p` is `Point` or `*Point`; the compiler picks the right C operator at lowering time. There is no `->` in Qoz source. Explicit `*p` is available for the rare case where the value, not a field of it, is wanted.

Equality and hashing of records are structural by default; the compiler synthesises field-wise `==` and `hash` so `Point { x: 1.0, y: 2.0 } == Point { x: 1.0, y: 2.0 }` is true. ADTs are not auto-derived: `Some(1) == Some(1)` is rejected at compile time unless the user registers `@operator("==")` and `@operator("hash")` for the ADT. The rationale is in [Operator Overloading](#operator-overloading).

---

## Generics

Type parameters are written with angle brackets:

```
let first<T>(v: Vec<T>): Option<T> =
    if Vec.len(v) > 0 { Some(v[0]) } else { None }

type Pair<A, B> = { fst: A, snd: B }

type Tree<T> =
    | Leaf
    | Node { value: T, left: Tree<T>, right: Tree<T> }
```

Generics are monomorphized at compile time. Each instantiation produces a distinct C function or struct. Type arguments are inferred at call sites; explicit type arguments are written `first<i32>(v)` when needed.

Bounds on type parameters are not part of the syntax. A generic function such as `map_probe<K, V>` may require `hash` and `==` on `K`, but the requirement is not declared in the signature. The compiler discovers it at monomorphization: when an instantiation triggers a call to an operator that has no implementation for the substituted type, the error is reported at the failing call. Declaration-site bounds may be added in a future revision; the current behaviour is to detect at use.

---

## Type Inference

Inference is bidirectional. The compiler propagates expected types into expression positions and infers types of unannotated bindings from the right-hand side. Annotations are needed for:

- Function parameters and return types.
- Top-level constant bindings whose value is not a literal.
- Cases where a value has multiple plausible types and no surrounding context picks one.

There is no let-polymorphism. A generic function must be declared with explicit type parameters; the compiler does not generalize over inferred types of `let` bindings.

---

## Casts

```
let i: i64 = 5
let f = i as f64
let n = (3.7 as i32)                            // 3, truncates
```

`expr as T` is an explicit cast. Allowed conversions: numeric widening and narrowing, pointer to pointer (FFI), integer to pointer and back (FFI), `char` to `u8` and back. Other conversions are compile errors.

---

## Built-in Functions

Three functions are in scope without import. They are reserved names: a top-level `let` declaration whose name collides with one of these is rejected by the compiler.

- `size_of(T): i64`. Returns the size of `T` in bytes. The argument is a type expression, not a value. Resolved at codegen against the type's C layout.
- `hash(x): u64`. Returns a hash of `x`. Dispatches to `@operator("hash")` for `x`'s type when one is registered, otherwise to the auto-derived hash for records, otherwise to a built-in bit-cast lowering for numeric, boolean, character, pointer, and `cstring` operands. Reports a compile error when none of these paths applies (for example, for ADTs with no registered `@operator("hash")`).
- `len(x): i64`. Returns the length of `x`. Dispatches to `@operator("len")` for `x`'s type. `std/vec` and `std/map` register `len` for their containers; `len` on a `string` is inlined to the underlying length field. Reports a compile error for types that have no `len` registered.

`println` is also a reserved name and behaves as a variadic print to stdout. It is provided by `std/fmt` and accessible without an import.

A reserved name may shadow nothing. A function inside a package may share the bare name with a builtin: `let len<T>(v: *Vec<T>): i64 = v.len` is fine inside `std/vec` because after package qualification the function name is `vec.len`. The builtin `len(v)` then dispatches to it through the operator table.

---

## C FFI

```
external let puts(s: cstring): i32
external let malloc(size: i64): *void
external let free(ptr: *void)

@link_name("pthread_create")
external let create_thread(thread: *void, attr: *void, start: *void, arg: *void): i32

#link("c")
#link("m")
#link_framework("CoreFoundation")
```

`external let name(...): T` declares a function whose Qoz name and C linker symbol are both `name`. The declaration introduces a Qoz identifier and binds it to a foreign symbol. No body is permitted.

To bind a Qoz name that differs from the C symbol, annotate the declaration with `@link_name("c_symbol")`. The Qoz identifier is what you call from Qoz source; the string passed to `@link_name` is the symbol the linker resolves. The same mechanism handles C symbols that are not valid Qoz identifiers (mangled names, platform-specific suffixes).

Link directives at the top of a file surface as linker flags to clang. `#link("name")` adds `-lname` for a regular library; `#link_framework("Name")` adds `-framework Name` on macOS.

### Compile-time embedding

`#load_string("relative/path")` in expression position evaluates at parse time to a string literal whose contents are the bytes of the file at that path. The path is resolved relative to the working directory of the compiler invocation. A missing file is a hard parse error, never a silent fallback.

```qoz
let runtime_h: string = #load_string("stage_a/runtime/qoz_runtime.h")
```

The form follows the same `#`-prefix convention as `#link`. Compile-time directives are visually distinct from runtime function calls.

### Attributes

`@name(arg)` syntax precedes a declaration and attaches metadata to it. Supported attributes:

- `@link_name("symbol")` on `external` declarations overrides the C symbol. Default is the Qoz identifier.
- `@operator("op")` on `let` declarations registers the function as the implementation of operator `op` for its first parameter's type. See [Operator Overloading](#operator-overloading).

Unknown attributes and attributes on declarations that do not accept them are parse errors. The grammar is reserved for future per-declaration directives (calling convention, linkage, deprecation, inlining hints).

### Passing strings to C

The `cstring` primitive type maps directly to `const char *`. String literals coerce to `cstring` when the context demands one, so the common case is transparent:

```
external let puts(s: cstring): i32
external let printf(fmt: cstring): i32

let main() = {
    puts("hello")
    printf("formatted\n")
}
```

For runtime strings (the `string` type, which carries a pointer and a length), use `string.cstr(s)` from `std/strings` to obtain a NUL-terminated copy suitable for C. C strings coming back from C functions can be wrapped with `string.from_cstr(p)`.

---

## Operator Overloading

A function may be registered as the implementation of an operator for its first parameter's type by using the `@operator(...)` attribute.

```
type Pair = { x: i64, y: i64 }

@operator("+")
let pair_add(a: *Pair, b: *Pair): Pair = Pair { x: a.x + b.x, y: a.y + b.y }

@operator("==")
let pair_eq(a: *Pair, b: *Pair): bool = a.x == b.x && a.y == b.y

@operator("unary-")
let pair_neg(a: *Pair): Pair = Pair { x: -a.x, y: -a.y }

@operator("hash")
let pair_hash(p: *Pair): u64 = (p.x as u64) + (p.y as u64)
```

Indexing and indexed assignment follow the same pattern:

```
@operator("[]")
let vec_index<T>(v: *Vec<T>, i: i64): T = v.data[i]

@operator("[]=")
let map_set<K, V>(m: *Map<K, V>, key: K, value: V) = map_insert(m, key, value)
```

After registration, the user writes `a + b`, `a == b`, `-a`, `hash(a)`, `v[i]`, or `m[k] = v` and the compiler lowers each to the appropriate call.

### Recognized Operators

| Operator | Arity | Signature shape |
|----------|-------|-----------------|
| `[]` | 2 | `(*Container, key): Element` |
| `[]=` | 3 | `(*Container, key, value): unit` |
| `+`, `-`, `*`, `/`, `%` | 2 | `(*Container, *Container): Container` (return type is up to the function) |
| `==`, `!=` | 2 | `(*Container, *Container): bool` |
| `<`, `>`, `<=`, `>=` | 2 | `(*Container, *Container): bool` |
| `unary-`, `unary!` | 1 | `(*Container): Container` (return type is up to the function) |
| `hash` | 1 | `(*Container): u64` |
| `len` | 1 | `(*Container): i64` |

The first parameter is always a pointer to a named type (record, ADT, or built-in primitive such as `string`). The compiler takes the address at the call site.

### Registration Rules

- Exactly one registration per `(operator, container type)` pair. A second registration for the same pair is rejected at compile time, with a message naming the prior function.
- Registrations are program-wide. There is no scoping: a registration in any file affects every use site in the program.
- A registration for a generic container (such as `Vec<T>`) is parameterised over the container's type parameters. The compiler monomorphises the registration alongside the container.
- Built-in lowerings apply when no override is registered: C `+`, `==`, and the others for numeric, boolean, and character types; bit-cast for `hash` on those types; pointer comparison for raw `*T`.

### Auto-Derived `hash` and `==` for Records

A record does not need any `@operator` registration to be used as a `Map` key. The compiler synthesises `hash` and `==` structurally:

- `hash` combines each field's hash with an FNV-1a mixer.
- `==` is short-circuit field-wise comparison.

The synthesised functions are emitted alongside the record. A user-defined `@operator("hash")` or `@operator("==")` for the same record suppresses the synthesised version. The override always wins.

The recursion descends through field types. A record whose fields include another record triggers derivation for the inner record too. Primitive fields (numeric, boolean, character, pointer, `cstring`) are bit-cast for hashing and compared with C `==`. `string` fields use the runtime helpers in `std/strings`.

ADTs are not auto-derived. Using an ADT as a `Map` key requires registering `@operator("hash")` and `@operator("==")` for it. The deliberate restriction trades a small ergonomic loss for two safety properties:

- A structural hash on an ADT recurses through the entire data structure, which is wrong for tree-shaped types (ASTs, IRs) where identity-based comparison is usually intended. A silent default in the wrong direction is a debugging hazard.
- Deeply nested or recursive ADT values blow the stack at hash time. There is no iterative form for the synthesised function.

The error at the use site names the missing operator and provides a copy-pasteable template:

```
no `hash` for `Option<i64>`. ADTs are not auto-derived.
Register one with `@operator("hash") let your_hash(p: *Option): u64 = ...`
```

---

## Standard Library

The standard library lives at `std/` and is written in Qoz on top of thin C bindings.

| Package | Provides |
|---------|----------|
| `std/mem` | `mem.alloc`, `mem.calloc`, `mem.realloc` bound to the tgc-backed runtime |
| `std/strings` | `strings.eq_raw`, `strings.hash_raw`, plus `@operator("==")` and `@operator("hash")` registrations for `string` |
| `std/fmt` | `println` (reserved builtin, no import needed); `format` and `Builder` planned |
| `std/vec` | `Vec<T>`, `vec.make`, `vec.push`, `vec.grow`, `vec.get`, `vec.len`, plus `@operator("[]")` and `@operator("len")` for `Vec<T>` |
| `std/map` | `Map<K, V>`, `map.make`, `map.insert`, `map.get`, `map.contains`, `map.len`, plus `@operator("[]=")` and `@operator("len")` for `Map<K, V>` |
| `std/libc` | planned: thin C bindings (open, read, write, fork, execvp, memcpy, getenv) |
| `std/os` | planned: `args()`, `exit()`, `getenv()`, `process_exec()` |
| `std/fs` | planned: `read_file`, `write_file`, `remove`, `read_dir` |
| `std/path` | planned: `abs`, `dir`, `base`, `join`, `clean` |
| `std/cmath` | planned: bindings to libm |

`Option<T>` and `Result<T, E>` are built-in types in the prelude. Their constructors `Some`, `None`, `Ok`, `Err` are in scope without import. `Vec<T>` lives in `std/vec`, `Map<K, V>` lives in `std/map`. Both require an explicit `import`. Types are imported into the bare namespace; functions are imported under the package prefix.

---

## Compilation Pipeline

1. Lex source files to tokens.
2. Parse tokens to AST per file.
3. Build dependency graph across files and packages; topologically sort.
4. Type-check and infer types. Resolve generics through monomorphization.
5. Lower: desugar pattern matches to decision trees, lower closures to environment-plus-function pairs, lower ADTs to tag-plus-payload layouts.
6. Emit C from the lowered IR.
7. Invoke clang on the emitted C with link directives.

The runtime is statically linked. Every Qoz binary contains tgc.

### CLI

```
qoz build <package>             // compile to executable
qoz run <package>               // compile, run, remove executable
qoz build <package> --emit-c    // stop after C emission
qoz build <package> -O0..<4     // optimization level (default -O3)
qoz build <package> -o name     // output name
```

---

## Examples

### Hello World

```
import std/fmt

let main() =
    fmt.println("Hello, world.")
```

### Expression Evaluator

```
import std/fmt

type Expr =
    | Num(i64)
    | Add(Expr, Expr)
    | Mul(Expr, Expr)

let eval(e: Expr): i64 =
    match e {
    | Expr.Num(n)    -> n
    | Expr.Add(a, b) -> eval(a) + eval(b)
    | Expr.Mul(a, b) -> eval(a) * eval(b)
    }

let main() = {
    let e = Expr.Add(
        Expr.Num(3),
        Expr.Mul(Expr.Num(4), Expr.Num(5)),
    )
    fmt.println("result:", eval(e))                 // 23
}
```

### Vec Walk

```
import std/fmt
import std/vec

let main() = {
    let v: Vec<i32> = [10, 20, 30]
    var sum: i32 = 0
    for x in v {
        sum = sum + x
    }
    fmt.println("sum:", sum)                        // 60
}
```

Array literal syntax `[a, b, c]` constructs a `Vec<T>` whose element type is inferred from the literal's elements or from the binding annotation. The literal lowers to `vec.make<T>()` followed by `vec.push<T>(&tmp, e)` for each element. `for x in v` iterates the resulting `Vec`. `vec.map`, `vec.filter`, and `vec.fold` are planned but not yet supported.
