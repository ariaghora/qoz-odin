# Qoz build.
#
# `make` does the full self-host pipeline:
#   1. clang bootstrap/stage1.c into a stage-0 binary.
#   2. stage-0 emits compiler/main.qoz.c.
#   3. clang that into a stage-1 binary.
#   4. stage-1 emits compiler/main.qoz.c again (this is the self-host
#      step: the compiler compiling its own source).
#   5. clang that into ./qoz, the final self-hosted compiler.
#
# Targets:
#   make                  (default) the pipeline above.
#   make test             build qoz and run the regression suite.
#   make refresh-bootstrap     regenerate bootstrap/stage1.c from the
#                              live source. Use this when the live
#                              compiler/main.qoz drifts from what
#                              bootstrap/stage1.c was last built against.
#   make clean            remove built artifacts.
#
# Parallel builds with `make -jN` are safe: every recipe has explicit
# dependencies and writes to a distinct output file.

# Disable built-in suffix rules and implicit recipes. Otherwise make
# tries to derive `.qoz` files from `.qoz.c` via default `%.x: %.x.y`
# patterns and prints "Circular dependency dropped" warnings.
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

# Platform: .exe suffix on Windows.
EXE :=
ifeq ($(OS),Windows_NT)
    EXE := .exe
endif

# Default to gcc. Make pre-seeds CC=cc as a built-in default, so
# an `?=` here would never fire. Honour an explicit override from
# the command line or environment by checking origin.
ifeq ($(origin CC), default)
    CC := gcc
endif
CFLAGS  := -std=c11 -pedantic -O3 -Wall -Werror
WARN    := -Wno-unused-function -Wno-unused-variable \
           -Wno-unused-but-set-variable -Wno-unused-const-variable \
           -Wno-unused-value -Wno-overlength-strings

# Probe the compiler family. `cc` and `gcc` on macOS are both
# clang aliases, so a name check alone is unreliable for those.
# MSVC `cl.exe` rejects `--version` and only prints its banner
# from a bare invocation, so identify MSVC by the binary name.
# Everything else: parse the --version banner.
ifneq ($(filter cl cl.exe,$(notdir $(CC))),)
    COMPILER_FAMILY := msvc
else
    CC_VERSION := $(shell $(CC) --version 2>/dev/null | head -n 1)
    ifneq ($(findstring clang,$(CC_VERSION)),)
        COMPILER_FAMILY := clang
    else ifneq ($(findstring GCC,$(CC_VERSION))$(findstring gcc,$(CC_VERSION))$(findstring Free Software Foundation,$(CC_VERSION)),)
        COMPILER_FAMILY := gcc
    else
        COMPILER_FAMILY := other
    endif
endif

ifeq ($(COMPILER_FAMILY),clang)
    # clang warns on `if (x == y)` in some configurations; the
    # generated C uses that pattern. GCC does not warn.
    WARN += -Wno-parentheses-equality
endif
ifeq ($(COMPILER_FAMILY),gcc)
    # GCC 12+ flags `qoz_gc_push_root(&local)` in the GC shadow
    # stack as a dangling pointer. The pattern is correct: the GC
    # pops the entry before the local goes out of scope. Silence
    # the false positive. Unknown to GCC < 12, which silently
    # accepts the `-Wno-` form.
    WARN += -Wno-dangling-pointer
    # GCC's flow analysis cannot see through the generated match
    # dispatch and reports `may be used uninitialized` for locals
    # the compiler proves are always written before read. Clang
    # has stricter scoping rules and does not warn here.
    WARN += -Wno-maybe-uninitialized
endif
ifeq ($(COMPILER_FAMILY),msvc)
    # cl.exe has its own flag dialect. Replace CFLAGS and WARN
    # wholesale; the output-naming pattern changes from `-o $@`
    # to `/Fe:$@`. Object files go to CWD via `/Fo:.\`.
    CFLAGS := /nologo /std:c11 /O2 /W3 /WX /MD
    WARN   := /wd4100 /wd4101 /wd4102 /wd4189 /wd4505 /wd4127 \
              /wd4244 /wd4267 /wd4090 /wd4146 /wd4477 /wd4133 /wd4090
    OUTOPT = /Fe:$@ /Fo:.\\
else
    OUTOPT = -o $@
endif

QOZ       := qoz$(EXE)
STAGE0    := stage0$(EXE)
STAGE1    := stage1$(EXE)
BOOTSTRAP := bootstrap/stage1.c

COMPILER_SRCS := $(shell find compiler -name '*.qoz' 2>/dev/null)
STDLIB_SRCS   := $(shell find std       -name '*.qoz' 2>/dev/null)
RUNTIME_SRCS  := $(wildcard runtime/*.c runtime/*.h)

.PHONY: all test refresh-bootstrap clean help

all: $(QOZ)

# Stage 0: bootstrap C compiled to a working compiler. Only step that
# needs no pre-existing Qoz binary.
$(STAGE0): $(BOOTSTRAP) $(RUNTIME_SRCS)
	$(CC) $(CFLAGS) $(WARN) $(BOOTSTRAP) $(OUTOPT)

# Stage 1: stage0 emits a fresh stage-1 source, clang turns it into a
# binary. This is what a working compiler from the bootstrap can do.
stage1-emit.qoz.c: $(STAGE0) $(COMPILER_SRCS) $(STDLIB_SRCS) $(RUNTIME_SRCS)
	QOZ_ROOT=$(CURDIR) ./$(STAGE0) emit compiler/main.qoz
	mv compiler/main.qoz.c stage1-emit.qoz.c

$(STAGE1): stage1-emit.qoz.c
	$(CC) $(CFLAGS) $(WARN) stage1-emit.qoz.c $(OUTOPT)

# Self-host: stage1 emits its own source. The result is the final
# qoz binary. If stage1 cannot reproduce its own input, the build
# fails here loudly.
qoz-emit.qoz.c: $(STAGE1)
	QOZ_ROOT=$(CURDIR) ./$(STAGE1) emit compiler/main.qoz
	mv compiler/main.qoz.c qoz-emit.qoz.c

$(QOZ): qoz-emit.qoz.c
	$(CC) $(CFLAGS) $(WARN) qoz-emit.qoz.c $(OUTOPT)
	@rm -f $(STAGE0) $(STAGE1) stage1-emit.qoz.c qoz-emit.qoz.c
	@rm -f stage1.obj stage1-emit.obj qoz-emit.obj

# Fixed-point self-host check. Run qoz on its own source, compare the
# output to the C source that qoz itself was compiled from. If they
# match, qoz emits a byte-identical copy of its own input, which is
# the strongest self-host invariant.
.PHONY: verify-self-host
verify-self-host: $(QOZ)
	@QOZ_ROOT=$(CURDIR) ./$(QOZ) emit compiler/main.qoz
	@if ! cmp -s compiler/main.qoz.c bootstrap/stage1.c; then \
	    echo "self-host fixed-point check failed: qoz emits a different bootstrap than the checked-in one"; \
	    echo "run 'make refresh-bootstrap' to update bootstrap/stage1.c, then 'make' and 'make verify-self-host' again."; \
	    diff compiler/main.qoz.c bootstrap/stage1.c | head -50; \
	    rm -f compiler/main.qoz.c; \
	    exit 1; \
	fi
	@rm -f compiler/main.qoz.c
	@echo "self-host fixed-point check passed"

test: $(QOZ) verify-self-host
	@cp $(QOZ) main
	bash tests/run.sh
	@rm -f main

.PHONY: fuzz
fuzz: $(QOZ)
	bash tests/fuzz/run.sh

# Refresh bootstrap/stage1.c from the live source. Requires an
# existing qoz binary to do the emit.
refresh-bootstrap: $(QOZ)
	@cp $(QOZ) main
	QOZ_ROOT=$(CURDIR) ./main emit compiler/main.qoz
	cp compiler/main.qoz.c $(BOOTSTRAP)
	@rm -f main

clean:
	rm -f $(QOZ) $(STAGE0) $(STAGE1) main main.bak stage2$(EXE)
	rm -f stage1-emit.qoz.c qoz-emit.qoz.c
	rm -f compiler/main.qoz.c
	find tests -name '*.qoz.c' -delete 2>/dev/null || true
	find tests -name '*.qoz' | while read f; do rm -f "${f%.qoz}"; done

help:
	@echo "Qoz build targets:"
	@echo "  make                     stage0 -> stage1 -> qoz (self-host)"
	@echo "  make test                run the regression suite"
	@echo "  make refresh-bootstrap   regenerate bootstrap/stage1.c"
	@echo "  make clean               remove built artifacts"
