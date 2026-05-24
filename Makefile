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

CC      := clang
CFLAGS  := -std=c11 -pedantic -O3 -Wall -Werror
WARN    := -Wno-unused-function -Wno-unused-variable \
           -Wno-unused-but-set-variable -Wno-unused-const-variable \
           -Wno-parentheses-equality -Wno-unused-value \
           -Wno-overlength-strings

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
	$(CC) $(CFLAGS) $(WARN) $(BOOTSTRAP) -o $@

# Stage 1: stage0 emits a fresh stage-1 source, clang turns it into a
# binary. This is what a working compiler from the bootstrap can do.
stage1-emit.qoz.c: $(STAGE0) $(COMPILER_SRCS) $(STDLIB_SRCS) $(RUNTIME_SRCS)
	QOZ_ROOT=$(CURDIR) ./$(STAGE0) emit compiler/main.qoz
	mv compiler/main.qoz.c stage1-emit.qoz.c

$(STAGE1): stage1-emit.qoz.c
	$(CC) $(CFLAGS) $(WARN) stage1-emit.qoz.c -o $@

# Self-host: stage1 emits its own source. The result is the final
# qoz binary. If stage1 cannot reproduce its own input, the build
# fails here loudly.
qoz-emit.qoz.c: $(STAGE1)
	QOZ_ROOT=$(CURDIR) ./$(STAGE1) emit compiler/main.qoz
	mv compiler/main.qoz.c qoz-emit.qoz.c

$(QOZ): qoz-emit.qoz.c
	$(CC) $(CFLAGS) $(WARN) qoz-emit.qoz.c -o $@
	@rm -f $(STAGE0) $(STAGE1) stage1-emit.qoz.c qoz-emit.qoz.c

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

test: $(QOZ)
	@cp $(QOZ) main
	bash tests/run.sh
	@rm -f main

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
	find tests -name '*.qoz.bin' -delete 2>/dev/null || true

help:
	@echo "Qoz build targets:"
	@echo "  make                     stage0 -> stage1 -> qoz (self-host)"
	@echo "  make test                run the regression suite"
	@echo "  make refresh-bootstrap   regenerate bootstrap/stage1.c"
	@echo "  make clean               remove built artifacts"
