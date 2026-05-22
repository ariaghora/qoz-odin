#!/bin/bash
# Regression runner. Expects to be invoked from the repo root.
#
# tests/tokenizer/* and tests/parser/*: must compile and run with exit 0.
# tests/typecheck/*: must FAIL compilation. Output is not checked beyond that.

set -u

# Resource caps inherited by every child process. A runaway compiler
# invocation can otherwise grow without bound: see the parser hang on
# unhandled function types. These caps trade a hard fail for a hung laptop.
ulimit -d 1048576 2>/dev/null || true   # data segment: 1 GB
ulimit -t 30 2>/dev/null || true        # CPU seconds: 30 per child

QOZ="./stage_a/stage_a.exe"
if [ ! -x "$QOZ" ]; then
    echo "$QOZ not found. Build with: cd stage_a && odin build . -out:stage_a.exe -debug"
    exit 2
fi

PASS=0
FAIL=0
fails=()

run_pos() {
    local t="$1"
    out=$("$QOZ" run "$t" 2>&1)
    rc=$?
    if [ $rc -ne 0 ] || echo "$out" | grep -q "error"; then
        FAIL=$((FAIL+1))
        fails+=("$t (expected pass)")
        return 1
    fi
    PASS=$((PASS+1))
}

run_neg() {
    local t="$1"
    out=$("$QOZ" run "$t" 2>&1)
    rc=$?
    if [ $rc -eq 0 ]; then
        FAIL=$((FAIL+1))
        fails+=("$t (expected fail, exit 0)")
        return 1
    fi
    PASS=$((PASS+1))
}

for t in tests/tokenizer/*.qoz tests/parser/*.qoz; do
    [ -f "$t" ] || continue
    run_pos "$t"
done

for t in tests/typecheck/*.qoz; do
    [ -f "$t" ] || continue
    run_neg "$t"
done

# Stage B tests: build with Stage A if needed, then compile each source
# through Stage B, link with clang under -Wall -Werror, and check the
# binary's exit code against the value declared in the file's `// expect: N`
# header line.
STAGE_B="$PWD/main"
"$QOZ" build compiler/main.qoz >/dev/null 2>&1
if [ ! -x "$STAGE_B" ]; then
    echo "Stage B build failed; skipping Stage B tests"
else
    RUNTIME_OBJ=/tmp/qoz_runtime_b.o
    GC_OBJ=/tmp/gc_b.o
    clang -c -I stage_a/runtime stage_a/runtime/qoz_runtime.c -o "$RUNTIME_OBJ" 2>/dev/null
    clang -c -I stage_a/runtime stage_a/runtime/gc.c -o "$GC_OBJ" 2>/dev/null

    run_stage_b_pos() {
        local t="$1"
        local expect
        expect=$(head -1 "$t" | awk '/^\/\/ expect: [0-9]+/{print $3; exit}')
        if [ -z "$expect" ]; then
            FAIL=$((FAIL+1))
            fails+=("$t (missing // expect: header)")
            return 1
        fi
        out=$(QOZ_ROOT="$PWD" "$STAGE_B" "$t" 2>&1)
        rc=$?
        if [ $rc -ne 0 ]; then
            FAIL=$((FAIL+1))
            fails+=("$t (Stage B failed: $out)")
            return 1
        fi
        bin=$(mktemp -t qozb.XXXXXX)
        if ! clang -I stage_a/runtime -Wall -Werror "${t}.c" "$RUNTIME_OBJ" "$GC_OBJ" -o "$bin" >/tmp/qozb_clang.log 2>&1; then
            FAIL=$((FAIL+1))
            fails+=("$t (clang failed; see /tmp/qozb_clang.log)")
            rm -f "$bin" "${t}.c"
            return 1
        fi
        "$bin"
        rc=$?
        rm -f "$bin" "${t}.c"
        if [ "$rc" != "$expect" ]; then
            FAIL=$((FAIL+1))
            fails+=("$t (exit $rc, expected $expect)")
            return 1
        fi
        PASS=$((PASS+1))
    }

    run_stage_b_neg() {
        local t="$1"
        out=$(QOZ_ROOT="$PWD" "$STAGE_B" "$t" 2>&1)
        rc=$?
        rm -f "${t}.c"
        if [ $rc -eq 0 ]; then
            FAIL=$((FAIL+1))
            fails+=("$t (expected rejection, exit 0)")
            return 1
        fi
        PASS=$((PASS+1))
    }

    for t in tests/stage_b/*.qoz; do
        [ -f "$t" ] || continue
        run_stage_b_pos "$t"
    done

    for t in tests/stage_b_neg/*.qoz; do
        [ -f "$t" ] || continue
        run_stage_b_neg "$t"
    done

    for t in tests/stage_b_gaps/*.qoz; do
        [ -f "$t" ] || continue
        run_stage_b_pos "$t"
    done
fi

rm -f *.qoz.c 2>/dev/null

if [ ${#fails[@]} -gt 0 ]; then
    echo
    echo "FAILED:"
    for f in "${fails[@]}"; do echo "  $f"; done
fi

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $FAIL
