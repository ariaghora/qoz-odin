# Qoz Language TODO

## Completed ✓
- [x] Lexer/tokenizer with line/column tracking
- [x] Parser with proper AST and spans
- [x] Semantic analysis with type checking
- [x] Symbol tables and scopes
- [x] Function types as first-class citizens
- [x] Basic codegen (transpile to C)
- [x] Arithmetic operators (+, -, *, /)
- [x] Unary operators (+, -)
- [x] Function definitions and calls
- [x] Return statements with type validation
- [x] Print statement (basic)

## High Priority (Core Language Features)
- [ ] **Control Flow Bundle**
  - [ ] Bool primitive type
  - [ ] Comparison operators (<, >, <=, >=, ==, !=)
  - [ ] if/else statements
  - [ ] while loops
  - [ ] Update semantic analysis for control flow
  - [ ] Update codegen for control flow

- [ ] **Logical Operators**
  - [ ] && (and)
  - [ ] || (or)
  - [ ] ! (not)

## Medium Priority (Essential Data Types)
- [ ] **String Type**
  - [ ] String literals in tokenizer
  - [ ] String type in type system
  - [ ] String concatenation (+)
  - [ ] Update print for strings

- [ ] **Arrays (Fixed-Size)**
  - [ ] Array type syntax: arr<T, N>
  - [ ] Array literals
  - [ ] Array indexing
  - [ ] Array operations

- [ ] **For Loops**
  - [ ] Range-based iteration (for i in 0..<10)
  - [ ] Array iteration

## Lower Priority (Advanced Features)
- [ ] **Structs**
  - [ ] Struct definition syntax
  - [ ] Struct literals
  - [ ] Field access
  - [ ] Methods?

- [ ] **Pointers/References**
  - [ ] Pointer type syntax
  - [ ] Address-of operator (&)
  - [ ] Dereference operator (*)

- [ ] **Dynamic Arrays (Slices)**
  - [ ] vec<T> syntax
  - [ ] Dynamic allocation
  - [ ] Slice operations

- [ ] **Import System**
  - [ ] import statement parsing
  - [ ] Multi-file parsing
  - [ ] Dependency resolution
  - [ ] Namespace/module system
  - [ ] Single C file output with prefixing

- [ ] **Standard Library**
  - [ ] String utilities
  - [ ] Math functions
  - [ ] File I/O
  - [ ] Memory allocators

## Future Enhancements
- [ ] Lambda lifting for nested non-capturing functions
- [ ] Type suffixes for literals (1i64, 1.0f64)
- [ ] Variable assignment (=) separate from declaration (:=)
- [ ] Better error messages with context
- [ ] Comprehensive test suite
- [ ] Documentation generator
- [ ] REPL
- [ ] Formatter
- [ ] LSP server

## Known Issues / Technical Debt
- [ ] Print statement needs proper formatting based on type (currently basic)
- [ ] Function parameter types in signatures (bug: uses return type)
- [ ] All code paths return validation (basic version, needs improvement for control flow)
- [ ] Type comparison for Function_Type (currently returns false)

## Architecture Notes
- Using 3 separate arenas: lexer, parser, semantic
- AST nodes annotated with inferred types during semantic analysis
- Single-pass codegen reads annotated AST
- Transpiles to C11 with int32_t, etc.
- Strict typing (no implicit conversions)
- No variable shadowing
- Forward declarations for all functions (supports mutual recursion)

---
*Last updated: [Current Session]*