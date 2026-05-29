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

# The active compiler is `./main`. When it is missing the runner
# builds it from bootstrap/stage1.c via clang. No further fallback;
# if bootstrap/stage1.c is broken the suite refuses to run.
QOZ_BIN="$PWD/main"
if [ ! -x "$QOZ_BIN" ] && [ -f bootstrap/stage1.c ]; then
    clang -std=c11 -pedantic -Wall \
          -Wno-unused-function -Wno-unused-variable -Wno-unused-but-set-variable \
          -Wno-unused-const-variable -Wno-parentheses-equality -Wno-unused-value \
          -Wno-overlength-strings \
          bootstrap/stage1.c -o "$QOZ_BIN" >/dev/null 2>&1
fi
if [ ! -x "$QOZ_BIN" ]; then
    echo "qoz binary (./main) not found and bootstrap/stage1.c could not produce one."
    exit 2
fi
QOZ="$QOZ_BIN"

# Host platform tag used by the `// platforms-only:` header marker.
# Maps `uname -s` to one of: linux, darwin, windows. Anything else
# resolves to the empty string, which matches no marker.
PLATFORM=""
case "$(uname -s 2>/dev/null)" in
    Linux*)               PLATFORM=linux ;;
    Darwin*)              PLATFORM=darwin ;;
    MINGW*|MSYS*|CYGWIN*) PLATFORM=windows ;;
esac

PASS=0
FAIL=0
fails=()

# Return 0 when the test should run on this platform. A test with no
# `// platforms-only:` header runs everywhere. A header like
# `// platforms-only: linux,darwin` skips on any platform not listed.
should_run_on_platform() {
    local t="$1"
    local marker
    marker=$(awk 'NR<=3 && /\/\/ platforms-only:/{sub(/.*platforms-only:[ \t]*/,""); print; exit}' "$t")
    if [ -z "$marker" ]; then return 0; fi
    case ",$marker," in
        *",$PLATFORM,"*) return 0 ;;
        *) return 1 ;;
    esac
}

# Positive: Stage B compiles, links, and runs. The binary must exit 0.
# Some tests (e.g. let_else) exercise features Stage B does not yet
# support. They carry the marker `// stage-b-skip` on the first line
# and the runner skips them so the suite still represents what works.
run_pos() {
    local t="$1"
    if head -1 "$t" | grep -q 'stage-b-skip'; then
        return 0
    fi
    if ! should_run_on_platform "$t"; then
        return 0
    fi
    out=$(QOZ_ROOT="$PWD" "$QOZ" run "$t" 2>&1)
    rc=$?
    rm -f "${t}.c" "${t%.qoz}"
    if [ $rc -ne 0 ]; then
        FAIL=$((FAIL+1))
        fails+=("$t (expected pass; exit $rc; $out)")
        return 1
    fi
    PASS=$((PASS+1))
}

run_neg() {
    local t="$1"
    out=$(QOZ_ROOT="$PWD" "$QOZ" emit "$t" 2>&1)
    rc=$?
    rm -f "${t}.c"
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

# Stage B integration tests: compile each source through Stage B's
# emit, link with clang under -Wall -Werror, and check the binary's
# exit code against the value declared in the file's `// expect: N`
# header line.
if true; then
    # Stage B's emitted .c is self-contained (runtime baked in via
    # #load_string in emit.qoz), so no extra object files or -I needed.

    run_stage_b_pos() {
        local t="$1"
        local expect
        expect=$(head -1 "$t" | awk '/^\/\/ expect: [0-9]+/{print $3; exit}')
        if [ -z "$expect" ]; then
            FAIL=$((FAIL+1))
            fails+=("$t (missing // expect: header)")
            return 1
        fi
        out=$(QOZ_ROOT="$PWD" "$QOZ_BIN" emit "$t" 2>&1)
        rc=$?
        if [ $rc -ne 0 ]; then
            FAIL=$((FAIL+1))
            fails+=("$t (Stage B failed: $out)")
            return 1
        fi
        bin=$(mktemp -t qozb.XXXXXX)
        # Build the test binary. MSVC uses a different flag dialect
        # entirely; dispatch when CC names cl. Everything else goes
        # through the gcc/clang path.
        case "$(basename "${CC:-clang}")" in
            cl|cl.exe)
                if ! cl /nologo /std:c11 /O2 /W3 /WX /MD \
                        /wd4100 /wd4101 /wd4102 /wd4189 /wd4505 /wd4127 \
                        /wd4244 /wd4267 /wd4090 /wd4146 /wd4477 /wd4133 \
                        "${t}.c" "/Fe:${bin}.exe" "/Fo:.\\" \
                        >/tmp/qozb_clang.log 2>&1; then
                    FAIL=$((FAIL+1))
                    fails+=("$t (cl failed; see /tmp/qozb_clang.log)")
                    rm -f "$bin" "${bin}.exe" "${t}.c"
                    return 1
                fi
                mv "${bin}.exe" "$bin"
                rm -f *.obj 2>/dev/null
                ;;
            *)
                if ! clang -std=c11 -pedantic -Wall -Werror -Wno-unused-function -Wno-unused-variable -Wno-unused-but-set-variable -Wno-unused-const-variable -Wno-parentheses-equality -Wno-unused-value -Wno-overlength-strings "${t}.c" -o "$bin" >/tmp/qozb_clang.log 2>&1; then
                    FAIL=$((FAIL+1))
                    fails+=("$t (clang failed; see /tmp/qozb_clang.log)")
                    rm -f "$bin" "${t}.c"
                    return 1
                fi
                ;;
        esac
        # Tests whose expected exit code is in the signal range
        # (>= 128) deliberately die from a signal (SIGABRT for the
        # panic test, etc.). Bash prints its own "Abort trap: 6" line
        # to stderr in those cases, which makes the suite output
        # look like there is a problem. Suppress stderr for those
        # runs so the runner's own diagnostics stay readable. Tests
        # with normal exit codes keep stderr live so genuine failures
        # surface.
        if [ "$expect" -ge 128 ]; then
            # Run in the background and wait, so bash does not print
            # its automatic "Abort trap: 6" line for direct foreground
            # signal deaths. The waited-on PID's exit status is what
            # `wait` reports.
            "$bin" 2>/dev/null &
            wait $! 2>/dev/null
        else
            "$bin"
        fi
        rc=$?
        rm -f "$bin" "${t}.c"
        # On Windows a process that calls abort() exits with status 3
        # (the MSVC convention); on POSIX it exits with 128+SIGABRT
        # (134). Accept either when the test asks for the POSIX form.
        if [ "$rc" != "$expect" ]; then
            if [ "$PLATFORM" = "windows" ] && [ "$expect" = "134" ] && [ "$rc" = "3" ]; then
                PASS=$((PASS+1))
                return 0
            fi
            FAIL=$((FAIL+1))
            fails+=("$t (exit $rc, expected $expect)")
            return 1
        fi
        PASS=$((PASS+1))
    }

    run_stage_b_neg() {
        local t="$1"
        out=$(QOZ_ROOT="$PWD" "$QOZ_BIN" emit "$t" 2>&1)
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
