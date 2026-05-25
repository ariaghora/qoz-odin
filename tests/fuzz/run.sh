#!/bin/bash
# Basic adversarial-input fuzz suite for the Qoz compiler.
#
# Generates short, deliberately-broken inputs and feeds each one to
# `qoz emit`. The compiler must either accept (rare) or reject with
# a non-zero exit and a diagnostic; any crash, hang, or zero-exit
# accepting an obviously-malformed program counts as a failure.
#
# The fuzz inputs cover token-level malformedness, parse-level
# malformedness, and check-level mismatches. The list is deliberately
# small so the suite finishes quickly; expand it as new categories of
# bug appear.

set -u

QOZ="$PWD/qoz"
if [ ! -x "$QOZ" ]; then
    echo "fuzz: qoz binary not built; run 'make' first" >&2
    exit 1
fi

PASS=0
FAIL=0
fails=()

run_fuzz() {
    local desc="$1"
    local src="$2"
    local tmp
    tmp=$(mktemp -t qoz_fuzz.XXXXXX.qoz)
    printf '%s' "$src" > "$tmp"
    out=$(QOZ_ROOT="$PWD" "$QOZ" emit "$tmp" 2>&1)
    rc=$?
    rm -f "$tmp" "${tmp}.c"
    # Anything other than rc==0 or rc==1 is a crash; flag.
    if [ $rc -ne 0 ] && [ $rc -ne 1 ]; then
        FAIL=$((FAIL+1))
        fails+=("$desc (exit $rc, expected 0 or 1): $(printf '%s' "$out" | head -c 200)")
        return 1
    fi
    PASS=$((PASS+1))
}

# Tokenizer-level malformedness.
run_fuzz "unterminated string"    'let main(): i64 = { let s = "abc'$'\n''0 }'
run_fuzz "empty hex literal"      'let main(): i64 = { let x = 0x; 0 }'
run_fuzz "empty binary literal"   'let main(): i64 = { let x = 0b; 0 }'
run_fuzz "empty octal literal"    'let main(): i64 = { let x = 0o; 0 }'
run_fuzz "empty float exponent"   'let main(): i64 = { let x = 1.0e; 0 }'
run_fuzz "unterminated block cmt" 'let main(): i64 = { /* unterminated'

# Parser-level malformedness.
run_fuzz "stray )"                'let main(): i64 = { ) }'
run_fuzz "bare keyword"           'let main(): i64 = { return }'
run_fuzz "missing brace"          'let main(): i64 = { 0'
run_fuzz "double assign op"       'let main(): i64 = { var x = 0; x += += 1; x }'
run_fuzz "missing fn name"        'let (): i64 = 0'

# Check-level mismatches.
run_fuzz "non-bool if cond"       'let main(): i64 = { if 42 { 1 } else { 0 } }'
run_fuzz "string + int"           'let main(): i64 = { let x = "a" + 1; 0 }'
run_fuzz "vec wrong index"        'import std/vec
let main(): i64 = { var v: Vec<i64> = vec.make(); vec.push(&v, 1); v["x"] }'
run_fuzz "undefined fn call"      'let main(): i64 = { undefined_fn() }'
run_fuzz "wrong variant arity"    'type T = | V(i64, i64)
let main(): i64 = { let x = T.V(1); 0 }'
run_fuzz "discarded Result"       'type R = | Ok(i64) | Err(string)
let f(): R = R.Ok(1)
let main(): i64 = { f(); 0 }'
run_fuzz "assign to let"          'let main(): i64 = { let x = 1; x = 2; x }'
run_fuzz "assign to literal"      'let main(): i64 = { 1 = 2; 0 }'
run_fuzz "if branches mismatch"   'let main(): i64 = { let x = if true { 1 } else { "x" }; 0 }'

echo
echo "Fuzz: passed $PASS, failed $FAIL"
if [ "$FAIL" -gt 0 ]; then
    for f in "${fails[@]}"; do
        echo "  $f"
    done
    exit 1
fi
exit 0
