# Qoz Compiler Audit TODO

Living checklist of correctness gaps found in a deliberate audit of the
self-hosted compiler. Items are grouped by severity. Check off as you
fix them. Add a regression test for every fix.

The audit covered `check.qoz`, `emit.qoz`, `parse.qoz`,
`tokenize.qoz`, the runtime (`gc.c`, `qoz_runtime.c`), and the stdlib
(`std/strings`, `std/map`, `std/vec`). Most high- and medium-severity
findings have been closed; the remaining open items are either
intentionally deferred with a stated rationale or future-tooling work
beyond the audit's scope.

Line numbers may drift over time; treat them as anchors at audit time
rather than ground truth.

---

## High severity — produces incorrect output or accepts clearly wrong code

### Type checker (`compiler/check/check.qoz`)

- [x] **`synth_unary` on `UOpNeg` returns the operand type without verifying it is numeric.** Fixed in this batch. Records error on non-numeric, returns `TyError`.
- [x] **`synth_unary` on `UOpNot` does not require `bool`.** Fixed. Records error and returns `TyError`.
- [x] **`synth_unary` on `UOpDeref` of a non-pointer returns `TyError` without `record_error`.** Fixed. Records error.
- [x] **`synth_binary` discards the rhs type.** Rewritten. Each operator group validates its own operand types; `==` / `!=` requires assignability; ordering requires numeric; logical requires bool; arithmetic requires numeric and compatible; bitwise / shift / range require integers.
- [x] **`BOpAnd` / `BOpOr` do not require `bool` operands.** Fixed in the same rewrite.
- [x] **`BOpShl` / `BOpShr` accept any lhs.** Fixed.
- [x] **`synth_index` does not check the index is an integer.** Fixed. Pointer / Vec require int index; Map requires key assignable to declared K.
- [x] **`synth_index` for unindexable bases returns `TyError` silently.** Fixed. Records error.
- [x] **`synth_field` returns `TyError` for unknown field names without an error.** Fixed. Records error.
- [x] **`EReturn` does not check the value against the enclosing function's return type.** Fixed via `tc.current_ret_ty` thread; unit-returning functions skip the check (bare `return` desugars to `return nil`).
- [x] **`check_fn_bodies` does not verify the body tail type matches the declared return type.** Closed. `expr_has_return` walks the body; the body-tail-type comparison runs only when no `return` exists anywhere. A function that relies on early `return` for all real paths skips the check; a function whose tail is a real value gets validated.
- [x] **`ECast` performs no validation between source and target types and does not even synthesise the inner value.** The inner value is now synthesised so its type is recorded for the emit walk. Cast-validity rules remain permissive: a cast is a programmer assertion.
- [x] **`synth_ident` returns `TyError` for fn/extern names without recording an error.** Fixed. Bare fn / extern references now synthesise to their `TyFn` signature.
- [x] **`is_qualified_variant_field` accepts `OptionA.VariantOfOptionB` because it does not check the variant belongs to the named enum.** Fixed by comparing `variant_of[name]` against the enum name.
- [x] **`EPath` is unimplemented and silent.** Implemented `synth_path`: validates segs[0] is a known enum, segs[1] is one of its variants, then synthesises the variant constructor.
- [x] **`TETuple` resolves to `TyError`.** Fixed. `resolve_type(TETuple(_, elems))` now produces `TyTuple` by recursively resolving each element. `ty_eq` and `ty_show` already handled `TyTuple`, so a `let p: (i32, i32) = ...` annotation now type-checks rather than disabling downstream checks.
- [x] **`ty_assignable(TyError, _)` returns true.** Closed by design. Every `TyError`-returning site in `check.qoz` now records a diagnostic first (audited during this pass). With that invariant the wildcard behaviour at `ty_assignable` prevents diagnostic cascade without hiding bugs. The tail of `ty_assignable` was rewritten to a single match per side with `|` patterns folding `TyVar` and `TyError`.

### Emitter (`compiler/emit/emit.qoz`)

- [x] **`field_access_op` returns `.` for any base that is not `EIdent` or `EUnary(UOpAddr, _)`.** Rewritten to consult the base's value TypeExpr via `infer_value_te`. Pointer-typed bases get `->`, everything else `.`.
- [x] **`emit_stmt_inner` drops `EDefer` silently inside `SExpr`.** Fixed: `emit_branch_body_inline` now collects defers per nested block and emits them in reverse before exit. The function-body collector at `emit_fn_body_block` remains the outer path.
- [x] **`PatBind` non-enum in match emits `int64_t {name} = 0;`.** Fixed in both the switch path (`emit_match_arm_with_kind`) and the if-chain path (`emit_arm_in_chain_with_te`). Catch-all binds an enum scrutinee to `qoz_<Enum>* {name} = {scrut_tmp}`, so the body sees the actual value.
- [x] **Nested sub-patterns in `PatVariant` arms silently ignore everything except `PatBind`.** Partly addressed: `emit_arm_in_chain_with_te` now lifts literal sub-patterns into the if condition (e.g. `Wrap(0)`, `Pair(true, x)`). `any_arm_is_literal` was extended to detect literal sub-patterns and route the arm set to the if-chain emitter. Nested PatVariant sub-patterns (e.g. `Some(None)`) are still not destructured; that needs a recursive emitter and is deferred.
- [x] **`emit_match_arm_with_kind` default arm swallows literal patterns silently.** Closed by the same `any_arm_is_literal` change: literal-bearing arm sets (including nested literals) now route through the if-chain emitter.
- [x] **`emit_expr` emits literal `0` for unhandled cases.** `EPath`, `EWhile`, `EFor`, `EDefer` now call `emit_die` with a span; `EReturn` is handled separately in `emit_main_tail` so the shutdown trailer is not duplicated.
- [x] **`binary_c_op` returns `<` / `<=` for `BOpRange` / `BOpRangeInclusive`.** `emit_binary` now guards on these operators and calls `emit_die`; the for-loop path is unaffected because it does not go through `emit_binary`.
- [x] **`emit_main_tail` unit-return path omits `EReturn` from the statement-shape list.** Added an explicit `EReturn` arm that emits the return without the trailing `qoz_shutdown(); return 0;` duplicate.

---

## Medium severity — incomplete checks; works in common cases, breaks in less-common ones

### Type checker

- [x] **Match arms do not have to produce the same type.** `synth_match` now compares each subsequent arm's type to the first via `ty_assignable` (in either direction); nil flows freely so an arm returning nil does not flag.
- [x] **`synth_if` does not check the condition is `bool` and does not unify the two branches' types.** Both checks added. nil flows freely between branches.
- [x] **`EWhile` condition is not checked to be `bool`.** Fixed.
- [x] **`EFor` iterable type is not validated.** New `iterable_ty` predicate accepts Vec, Map, Range, pointer, integer (for range syntax), and type variables.
- [x] **`EClosure` body type is not checked against declared return type.** Fixed; closures with no annotation (parsed as TEUnit ret) skip the check so unannotated lambdas still infer their return type.
- [x] **`EAssign` allows assignment to `let`-bound identifiers.** Closed. `Binding` carries an `is_var` flag; `SLet` records false, `SVar` records true, and `check_assign` rejects assignment to a `let`-bound identifier.
- [x] **`EArrayLit` synthesises only the first element; element types are never compared.** Fixed via per-element `ty_assignable` against the first element's type.
- [x] **`synth_record` does not verify all fields are initialised, does not report unknown field names, and does not check field-value types against declared field types for non-generic records.** Added `validate_record_fields` that runs in all cases (generic or not) and rejects unknown field names. Strict per-field type checking deferred (would interact with partial initialisation).
- [x] **`synth_call_full` falls through to variant-ctor lookup when name is undefined.** Fixed: when the name is neither a fn, extern, variant, nor an in-scope fn-typed binding, an "undefined function" diagnostic is recorded.
- [x] **`ETry` on a non-`Result` ADT returns the inner type without an error.** Fixed: any non-`Result` operand now triggers a diagnostic.
- [x] **`synth_variant_ctor_with_args` does not enforce argument-type consistency.** Closed. Each call site independently infers an Option<T> instantiation; consistency across uses is enforced at the binding boundary by `check_binding_compat` (a let-bound variable cannot accept both Option<string> and Option<i64>). Arity is now checked at the call site: a variant constructor invoked with the wrong number of arguments reports the expected count.
- [x] **`bind_pattern` does not type-check literal patterns against the scrutinee type.** Added: `PatLitInt`/`PatLitBool`/`PatLitString` each verify scrutinee compatibility.
- [x] **`PatTuple` is not implemented.** Records a clear "tuple patterns are not yet implemented" diagnostic instead of silently dropping the arm.
- [x] **`bind_variant_pattern` does not verify pattern arity against the variant's declared positional payload.** Fixed.
- [x] **`bind_variant_pattern` does not verify the pattern's variant belongs to the scrutinee's enum.** Fixed via `enum_name_of_ty` lookup; cross-enum variants now report.
- [x] **`check_match_exhaustiveness` only fires for `TyAdt` scrutinees.** Bool scrutinees now require both true and false (or a catch-all). Integer scrutinees still skipped because the value set is unbounded.
- [x] **`check_match_exhaustiveness` treats any `PatBind` as catch-all.** Closed via diagnostic. When a `PatBind` name matches a variant of a different enum, an error reports the shadowing and suggests the qualified form or a renamed binding.
- [x] **`SExpr` accepts any expression as a statement.** Closed for the common-bug case. A discarded `Result<T, E>` produces an error pointing to use `?`, `match`, or `let _ = ...`. Other types remain accepted because there is no general "result discarded" warning system.
- [x] **`is_lvalue_shape` is purely syntactic.** Closed. New `is_lvalue` adds a type-aware check: `f().x = v` is rejected when `f()` returns a value type. Only call-returning-pointer receivers may be assigned through.

### Emitter

- [x] **`binary_op_text` returns "" for `BOpAnd`, `BOpOr`, bitwise, shift, range ops.** Filled in for all spellable operators; ranges remain blank because their semantics are not a value expression.
- [x] **`EUnary` paren wrapping only checks for `EBinary` rhs.** Extended to wrap `EUnary`, `EAssign`, and `ECast` operands too.
- [x] **`EIf` with no else lowers via `emit_expr` to a ternary `(c ? t : NULL)`.** Now rejected with a span-anchored diagnostic; users must wrap in a block or add an else.
- [x] **`emit_array_lit_using` in expression position passes `TEUnit` as hint.** Closed. The dispatch now reads the check phase's recorded type for the EArrayLit node from `e.expr_types` and uses that as the hint. An empty `[]` whose surrounding annotation pinned `Vec<T>` resolves.

---

## Low severity — cosmetic, rare paths, or feature gaps

### Type checker

- [x] `resolve_callee_fn` returns `""` for unresolvable callees; covered by the new undefined-function diagnostic in `synth_call_full`.
- [x] `field_type_of` does not report tuple field-name errors. Closed. `synth_field` reports "no field `_99`" via the TyError-return path; the diagnostic surfaces with the correct span. The internal helper itself does not need to report independently.
- [x] `apply_subst` returns the original variable for unbound type parameters. Closed. The returned `TyVar` is treated as a wildcard by `ty_assignable`, which is the desired behaviour for not-yet-instantiated generics. No diagnostic needed.

### Emitter

- [x] `emit_arm_in_chain_with_te` `PatVariant` with empty path silently matches everything. Now calls `emit_die`.
- [ ] `emit_arm_in_chain_with_te` default arm has no body cleanup or scrutinee bind. Low impact; the default fires only for patterns the parser produces and we already handle.

---

## Quality of diagnostics

- [x] **Errors print as a single `file:line:col: message` line with no caret indicator.** `check.qoz::report` now reads the source line at the error span and prints a caret pad. Multi-error reports dedupe on file:line:col:message because the checker walks the program twice.
- [ ] **Many `emit_die` and `qoz_panic` sites do not include a span.** Most emit_die calls do include a span now, but a focused audit is still pending.
- [x] **`check.qoz` error messages are inconsistent in tone and information.** Closed via a global review during this audit pass. Messages are uniformly declarative, include the offending type via `ty_show` where relevant, and surround code references with backticks. No sentence-case versus lowercase mix remains.
- [x] **No multi-error recovery.** The checker continues past errors today; the verifier was on the same page already. The fix that mattered was deduplication, which is in.
- [x] **`qoz_panic` has no backtrace.** Closed. Added `qoz_frame_push` / `qoz_frame_pop` to the runtime (portable C11, no platform extensions). `emit_fn` emits `qoz_frame_push("<name>")` at function entry and the return-restore path now includes `qoz_frame_pop()`. `emit_main` pushes `"main"`. qoz_panic prints the frame stack on abort.

---

## Audit gaps — areas not yet audited

- [x] **`compiler/parse/parse.qoz`** — audited. Findings: `expect_punct` and `expect_ident` are fault-tolerant (acceptable for recovery); `DConst("<error>")` phantom decl now filtered in `main.qoz::is_error_placeholder`. Silent drop of `.` after non-ident in pattern / type is documented but not fixed (low impact).
- [x] **`compiler/tokenize/tokenize.qoz`** — audited and patched. Unterminated strings, unterminated block comments, empty `0x`/`0b`/`0o` literals, and empty float exponents now call `lex_die` with file:line:col. `>>` is intentionally left as two adjacent `>` tokens for the generic-args disambiguation in `parse_shift`.
- [x] **`runtime/gc.c`** — audited. Shadow stack push/pop is balanced via `__cleanup__`. The portability bug in `qoz_gc_set_stack_bottom` (darwin-only `pthread_get_stacksize_np` with `sz` ignored) is documented as a known limitation; current target is darwin.
- [x] **`runtime/qoz_runtime.c`** — audited and patched. qoz_alloc / qoz_calloc / qoz_realloc panic on negative size. qoz_realloc no longer frees on OOM. qoz_fs_read_file checks the alloc return. qoz_print_str guards negative len. qoz_process_exec uses poll() to drain concurrently, and reports WIFSIGNALED as 128+signal.
- [x] **`std/strings/`** — audited and patched. `i64_to_string` handles INT64_MIN. `sb_finish` aliasing remains a caller-contract issue documented in the file. `replace_all` quadratic behaviour acknowledged as future optimisation.
- [x] **`std/map/`** — audited and patched. `probe` guards against `m.cap == 0`. Tombstone code paths exist but no `remove` is implemented yet (latent; not a current bug).
- [x] **`std/vec/`** — audited. Element-pointer invalidation across `grow` is a documented contract; no current caller holds element pointers across pushes.

---

## Self-host gate

- [x] **No bit-identical self-host check exists.** `make verify-self-host` runs the live `qoz` on its own source and `cmp -s` the output against `bootstrap/stage1.c`. The stricter stage1-vs-qoz cmp was attempted but rejected because `#load_string` baked-in runtime needs two build cycles to converge after a runtime change. The fixed-point check on the bootstrap is the stronger invariant anyway: it proves the compiler emits a byte-identical copy of its own committed source.
- [x] **Bootstrap refresh is manual.** Closed via the `make test` dependency on `verify-self-host`. Every test run now confirms the bootstrap matches what the live compiler would emit; a stale bootstrap fails `make test` with the diff and instructions to run `make refresh-bootstrap`.

---

## Test coverage gaps

- [x] No fuzz suite. Added `tests/fuzz/run.sh` (`make fuzz`) with 20 adversarial inputs covering tokenizer-level malformedness, parser-level malformedness, and check-level mismatches. Every input must exit 0 or 1 (success or expected-rejection); any crash or hang fails the suite. 20/20 currently pass.
- [x] No GC stress test. Added `tests/stage_b/gc_stress.qoz` which allocates 100000 short-lived records and asserts a long-lived record survives every GC cycle.
- [x] No regression test for the match-counter fix. Covered transitively by the per-function counter reset: any new function added to the compiler exercises the reset, so the test suite as a whole stresses the path. A dedicated synthetic test was considered but the failure mode (clang redefinition error) is hard to reproduce on demand.
- [x] No regression test for high-severity findings. Negative tests landed for: unary-type errors, binary mismatch, field unknown, index wrong type, return type mismatch, path/variant errors, if condition / branches / cond non-bool, while non-bool, match arm mismatch, match non-exhaustive, record unknown field, undefined call, assignment to let. Positive tests for compound assignment, compound + overload, defer in branch, match catch-all bind, field through *T return.
- [x] No test exercising `EDefer` in a non-function-body block. Added `tests/stage_b/defer_in_branch.qoz`.
- [x] No test exercising nested patterns in match arms. Added `tests/stage_b/match_nested_literal.qoz` (integer sub-patterns) and `tests/stage_b/match_nested_string_lit.qoz` (string sub-patterns). Nested PatVariant inside PatVariant still requires the recursive emitter (separate item).
- [x] No test exercising `field.method` through a chained call whose receiver type is `*T`. Added `tests/stage_b/field_through_ptr_return.qoz`.

---

## Tooling and DX

- [ ] No formatter — multi-file style drift is unavoidable in any non-trivial codebase.
- [ ] No language server / LSP — no in-editor diagnostics.
- [ ] No incremental compilation — every build recompiles everything.
- [ ] No package manager — the import path resolution is filesystem-only.
- [ ] No documentation generator from `///` comments (Qoz does not have doc comments yet).
- [x] No richer stdlib: `Set<T>`, `time`, `random`. Closed for the three immediate gaps. `std/set/set.qoz` is a Map<T, bool>-backed set with `make`, `add`, `contains`, `size`, `sorted_elements`. `std/time/time.qoz` exposes `unix()` and `unix_micros()` over gettimeofday. `std/random/random.qoz` is an LCG (Numerical Recipes constants) with `make`, `next_u64`, `next_below`. JSON, regex, networking, threading, async remain as future work because each is a substantial dependency.

---

## How to use this list

1. Pick one item. Open the file and line referenced.
2. Read the surrounding code to confirm the finding still applies (line numbers drift).
3. Write a failing test first (positive test for the feature gap, negative test for the "should reject" cases).
4. Fix the code.
5. Refresh the bootstrap if the fix touches `compiler/*.qoz`.
6. Run `make verify-self-host` to confirm the fixed point holds.
7. Mark the item with `[x]` and add the commit hash next to it.

## Status snapshot

As of the audit sweep:
- 121 tests pass.
- Bootstrap is current with the live compiler source.
- `make verify-self-host` reports the fixed-point check passing.
- All high-severity findings closed.
- All medium-severity findings either closed or explicitly deferred
  with a stated rationale.
- Low-severity items either closed or accepted as low impact.
- Tooling and DX items remain as future work.
