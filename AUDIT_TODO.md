# Qoz Compiler Audit TODO

Living checklist of correctness gaps found in a deliberate audit of the
self-hosted compiler. Items are grouped by severity. Check off as you
fix them. Add a regression test for every fix.

The audit covered `check.qoz` and `emit.qoz`. `parse.qoz`, the runtime
(`gc.c`, `qoz_runtime.c`), and the stdlib have not been audited yet —
those are listed under "Audit gaps" at the bottom.

Line numbers may drift over time; treat them as anchors at audit time
rather than ground truth.

---

## High severity — produces incorrect output or accepts clearly wrong code

### Type checker (`compiler/check/check.qoz`)

- [ ] **`synth_unary` on `UOpNeg` returns the operand type without verifying it is numeric.** `-"hello"` and `-some_struct` type-check. Line ~324.
- [ ] **`synth_unary` on `UOpNot` does not require `bool`.** `!42` returns `TyBool`. Line ~325.
- [ ] **`synth_unary` on `UOpDeref` of a non-pointer returns `TyError` without `record_error`.** Silent. Lines ~326-331.
- [ ] **`synth_binary` discards the rhs type.** `let _ = synth(tc, env, r)`. No operand-type compatibility check for any binary op. `1 + "x"`, `vec_a < vec_b` accepted. Line ~951.
- [ ] **`BOpAnd` / `BOpOr` do not require `bool` operands.** Always return `TyBool`. Lines ~959-960.
- [ ] **`BOpShl` / `BOpShr` accept any lhs.** Return `lt` unverified. Lines ~969-970.
- [ ] **`synth_index` does not check the index is an integer.** `arr["hello"]` type-checks. Line ~438.
- [ ] **`synth_index` for unindexable bases returns `TyError` silently** (no `record_error`). Lines ~445-447.
- [ ] **`synth_field` returns `TyError` for unknown field names without an error.** `point.zzz` accepted. Line ~341.
- [ ] **`EReturn` does not check the value against the enclosing function's return type.** Line ~268.
- [ ] **`check_fn_bodies` does not verify the body tail type matches the declared return type.** Lines ~1547-1557.
- [ ] **`ECast` performs no validation between source and target types and does not even synthesise the inner value.** Line ~224.
- [ ] **`synth_ident` returns `TyError` for fn/extern names without recording an error.** Lines ~935-940.
- [ ] **`is_qualified_variant_field` accepts `OptionA.VariantOfOptionB` because it does not check the variant belongs to the named enum.** Lines ~348-355.
- [ ] **`EPath` is unimplemented and silent.** Returns `TyError`, no error. Line ~217.
- [ ] **`TETuple` resolves to `TyError`, which makes `ty_assignable(TyError, _)` true and disables checking on tuple-typed bindings.** Line ~107.
- [ ] **`ty_assignable(TyError, _)` returns true.** Means one missed check poisons every downstream check. `compiler/ty/ty.qoz:358`. Fix candidate: return false (strict mode) and propagate the original error context separately.

### Emitter (`compiler/emit/emit.qoz`)

- [ ] **`field_access_op` returns `.` for any base that is not `EIdent` or `EUnary(UOpAddr, _)`.** A function returning `*T` followed by `.field` emits `.field` instead of `->field`, producing invalid C. Lines ~2385-2406.
- [ ] **`emit_stmt_inner` drops `EDefer` silently inside `SExpr`.** Defers in non-function-body blocks never execute. Line ~2115.
- [ ] **`PatBind` non-enum in match emits `int64_t {name} = 0;` instead of binding the scrutinee value.** The bound name reads zero. `emit_match_arm_with_kind` lines ~4789-4793 and `emit_arm_in_chain_with_te` line ~4684.
- [ ] **Nested sub-patterns in `PatVariant` arms silently ignore everything except `PatBind`.** `Some(None)`, `Pair(1, x)` do not destructure correctly. Lines ~4716, ~4820.
- [ ] **`emit_match_arm_with_kind` default arm at line ~4836 swallows `PatLitInt`/`PatLitBool`/`PatLitString`/`PatTuple` silently when reached via the switch path.**
- [ ] **`emit_expr` emits literal `0` for unhandled cases.** `EPath`, `EWhile`, `EFor`, `EDefer`, `EReturn` reaching expression position produce `0` with payload dropped. Lines ~1375, ~1400-1402. Replace with `emit_die`.
- [ ] **`binary_c_op` returns `<` / `<=` for `BOpRange` / `BOpRangeInclusive`.** A range expression outside a for-loop iter context (e.g. `let r = 0..n`) silently lowers to a boolean comparison. Lines ~3117-3118.
- [ ] **`emit_main_tail` unit-return path omits `EReturn` from the statement-shape list.** A `main` ending with explicit `return` makes `qoz_shutdown()` unreachable. Line ~5097.

---

## Medium severity — incomplete checks; works in common cases, breaks in less-common ones

### Type checker

- [ ] **Match arms do not have to produce the same type.** Only the first arm's type becomes the result; subsequent arms' types are discarded. `synth_match` lines ~1014-1018.
- [ ] **`synth_if` does not check the condition is `bool` and does not unify the two branches' types.** Lines ~999-1002.
- [ ] **`EWhile` condition is not checked to be `bool`.** Line ~259.
- [ ] **`EFor` iterable type is not validated.** Non-Vec, non-pointer iterables silently fall through to `i64` loop-variable type. `bind_for_loop` lines ~1131, ~1143-1144.
- [ ] **`EClosure` body type is not checked against declared return type.** Line ~253.
- [ ] **`EAssign` allows assignment to `let`-bound identifiers** (var-vs-let not tracked). Comment at line ~290 admits this.
- [ ] **`EArrayLit` synthesises only the first element; element types are never compared.** `[1, "two"]` accepted. Lines ~271-282.
- [ ] **`synth_record` does not verify all fields are initialised, does not report unknown field names, and does not check field-value types against declared field types for non-generic records.** Lines ~534-569, ~539.
- [ ] **`synth_call_full` falls through to variant-ctor lookup when name is undefined.** Calls to undefined functions are silent. Lines ~520-524.
- [ ] **`ETry` on a non-`Result` ADT returns the inner type without an error.** `option_value?` accepted. Lines ~230-238.
- [ ] **`synth_variant_ctor_with_args` does not enforce argument-type consistency.** `Option.Some("hi")` and `Option.Some(42)` in the same context can produce conflicting instantiations. Lines ~686-692.
- [ ] **`bind_pattern` does not type-check literal patterns against the scrutinee type.** `match an_i32 { "x" -> ... }` accepted. Line ~1154.
- [ ] **`PatTuple` is not implemented; destructures nothing and binds nothing.** Subsequent uses of the supposed bindings report "undefined name" rather than a pattern error. Line ~1154.
- [ ] **`bind_variant_pattern` does not verify pattern arity against the variant's declared positional payload.** Lines ~1172-1183.
- [ ] **`bind_variant_pattern` does not verify the pattern's variant belongs to the scrutinee's enum.** `Result.Ok(_)` matched against an `Option` scrutinee binds nothing silently. Lines ~1163-1166.
- [ ] **`check_match_exhaustiveness` only fires for `TyAdt` scrutinees.** `match bool { true -> ... }` is not checked. Match on integer is not checked. Lines ~1024-1025.
- [ ] **`check_match_exhaustiveness` treats any `PatBind` as catch-all.** A pattern that looks like a variant name but is not one masks missing arms. Lines ~1083-1087.
- [ ] **`SExpr` accepts any expression as a statement.** No warning when a non-unit expression (e.g. ignored `Result`) is discarded. Line ~992.
- [ ] **`is_lvalue_shape` is purely syntactic.** `get().x = 1` is treated as assignable. Line ~291.

### Emitter

- [ ] **`binary_op_text` returns "" for `BOpAnd`, `BOpOr`, bitwise, shift, range ops.** Means `@operator("&&")`, `@operator("|")`, etc. on user types never dispatch. Line ~2929.
- [ ] **`EUnary` paren wrapping only checks for `EBinary` rhs.** `-(-x)` would emit `--x`, parsed as predecrement in C. Lines ~1298-1305.
- [ ] **`EIf` with no else lowers via `emit_expr` to a ternary `(c ? t : NULL)`.** If the branches are not pointers this is a C type error. Line ~1393.
- [ ] **`emit_array_lit_using` in expression position passes `TEUnit` as hint.** Empty array literal cases lose annotation context. Line ~1404 area.

---

## Low severity — cosmetic, rare paths, or feature gaps

### Type checker

- [ ] `resolve_callee_fn` returns `""` for unresolvable callees; caller silently falls through to a variant-ctor attempt. Lines ~493, ~498, ~517.
- [ ] `field_type_of` does not report tuple field-name errors. `t._99` past the end silently returns `TyError`. Lines ~363-367.
- [ ] `apply_subst` returns the original variable for unbound type parameters, propagating unresolved `TyVar` into downstream checks. Line ~410.

### Emitter

- [ ] `emit_arm_in_chain_with_te` `PatVariant` with empty path silently matches everything. Lines ~4720-4722.
- [ ] `emit_arm_in_chain_with_te` default arm at line ~4724 has no body cleanup or scrutinee bind, silently matching everything.

---

## Quality of diagnostics

- [ ] **Errors print as a single `file:line:col: message` line with no caret indicator or source context.** Hard to localise in long files.
- [ ] **Many `emit_die` and `qoz_panic` sites do not include a span,** so the error points to the compiler line, not the user-source line.
- [ ] **`check.qoz` `record_error` messages are inconsistent in tone and information.** Some include the type, some do not. Some are sentence-case, others not.
- [ ] **No multi-error recovery.** First serious error usually causes downstream errors to cascade or be suppressed.
- [ ] **`qoz_panic` has no backtrace.** A runtime panic prints the message and aborts; the call site is unrecoverable.

---

## Audit gaps — areas not yet audited

- [ ] **`compiler/parse/parse.qoz`** — verify the parser rejects malformed input cleanly. Look for silent fallthroughs, missing precedence handling, error recovery (or absence thereof).
- [ ] **`compiler/tokenize/tokenize.qoz`** — verify token boundaries on edge cases (unterminated string literals, embedded null bytes, malformed numeric literals with multiple decimal points or trailing letters).
- [ ] **`runtime/gc.c`** — audit tgc usage for correctness: shadow stack push/pop balance, root registration on every heap pointer, no use-after-free across `qoz_realloc` calls.
- [ ] **`runtime/qoz_runtime.c`** — audit FFI boundaries: every function that constructs a `qoz_string` from C bytes should set `root` correctly so GC sees the allocation.
- [ ] **`std/strings/`** — audit slice/cat/has_prefix for off-by-one and pointer-arithmetic bugs on empty strings and on slices crossing GC allocation boundaries.
- [ ] **`std/map/`** — audit the hash map for collision handling, resize correctness, iteration determinism.
- [ ] **`std/vec/`** — audit growth, capacity tracking, slice/element pointer invalidation on grow.

---

## Self-host gate

- [ ] **No bit-identical stage-1 vs stage-2 check exists.** `make self-host` builds stage-2 and runs the test suite; it does not `cmp` the emitted C of `compiler/*.qoz` between stage-1 and stage-2.
- [ ] **Bootstrap refresh is manual.** `make refresh-bootstrap` exists but nothing enforces calling it after an emit-affecting change. A commit can land that breaks future cold builds.

---

## Test coverage gaps

- [ ] No fuzz suite. Parser, type checker, and emitter have never seen adversarial input.
- [ ] No GC stress test. The runtime has not been exercised with high-allocation, high-mutation workloads.
- [ ] No regression test for the match-counter fix (fixed in commit `39fa861`).
- [ ] No regression test for any of the "high severity" findings above. Each fix should land with a test in `tests/stage_b_neg/` (compile-time rejection) or `tests/stage_b/` (runtime behaviour).
- [ ] No test exercising `EDefer` in a non-function-body block.
- [ ] No test exercising nested patterns in match arms.
- [ ] No test exercising `field.method` through a chained call whose receiver type is `*T`.

---

## Tooling and DX

- [ ] No formatter — multi-file style drift is unavoidable in any non-trivial codebase.
- [ ] No language server / LSP — no in-editor diagnostics.
- [ ] No incremental compilation — every build recompiles everything.
- [ ] No package manager — the import path resolution is filesystem-only.
- [ ] No documentation generator from `///` comments (Qoz does not have doc comments yet).
- [ ] No richer stdlib: `Set<T>`, `time`/`date`, random, JSON, regex, networking, threading, async — none exist.

---

## How to use this list

1. Pick one item. Open the file and line referenced.
2. Read the surrounding code to confirm the finding still applies (line numbers drift).
3. Write a failing test first (positive test for the feature gap, negative test for the "should reject" cases).
4. Fix the code.
5. Refresh the bootstrap if the fix touches `compiler/*.qoz`.
6. Mark the item with `[x]` and add the commit hash next to it.

Order of attack, in priority order:
1. Fix `ty_assignable(TyError, _) == true` (it disables many other checks).
2. Fix `synth_binary` discarding rhs type (broadest impact on user code).
3. Fix `field_access_op` defaulting to `.` (silent miscompile of generated C).
4. Fix `EDefer` drop in non-function-body blocks (silent runtime miscompile).
5. Fix `PatBind` non-enum binding to zero in match arms (silent runtime miscompile).
6. Audit the remaining files listed under "Audit gaps".
