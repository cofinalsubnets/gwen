# Benchmark harness. Times equivalent numeric / list workloads written in many
# languages and prints a side-by-side comparison. Each language self-times the
# inner loop (auto-scaling reps past a 200ms floor) so interpreter startup is
# excluded; see bench.l / lib/bench.* for the shared protocol.
#
# Results are cached per language under out/bench/<lang>.txt. The user-facing
# targets DEPEND on those files and just pretty-print them -- a bench is only
# (re)run when its result file is missing or older than the bench sources (or,
# for love, the love binary). So `make bench` reformats instantly once the files
# exist; touch a source or `make clean` to force a re-run.
#
#   make / make bench   show the default column set (love go rust julia
#                          luajit pypy node), running only what is stale
#   make all            show every language present on this machine
#   make <lang>         show one language (love is shown alongside for contrast),
#                          e.g. make chez / make pypy / make rust
#   make BENCHES=fib …  restrict the workload set (then clean to refresh files)
#   make TIMEOUT=60 …   per-bench wall-clock cutoff in seconds (default 30)
#   make SAMPLES=5 …    timed windows per bench; the median ms/it is reported
#                          (default 5; SAMPLES=1 = the old single-shot, fast but noisy)
#   make raw            cat the raw "<name> <lang> <reps> <ms> <chk>" lines
#   make clean          remove out/bench/
#
# A language whose interpreter/compiler is not on PATH is skipped automatically, as
# is any bench a language has no implementation file for (e.g. luajit/rust lack bell
# -- no bignums); the cell drops out of the table. Pairs that exceed TIMEOUT are
# listed in SKIP (below) and dropped up front so the build never pays the timeout
# wall for them. The per-language run logic lives in run.sh.
R := ..
include $(R)/common.mk

GL   := $(R)/out/host/love
HARN := bench.l
BDIR := $(R)/out/bench
TIMEOUT ?= 30
SAMPLES ?= 5

# workloads, in display order; override with `make BENCHES="fib primes"`.
BENCHES := fib tak sum deforest primes bell strcat strscan hash sort tree bintrees mandelbrot closure cdcl

# <lang>:<bench> pairs that exceed TIMEOUT -- dropped up front so the build never
# pays the timeout wall for them (re-test by removing one and `make clean`).
SKIP :=

# every language, in report-column order. love first, then native (go rust java),
# JIT (julia luajit pypy node), BEAM (elixir), a scheme (chez), and a CL (sbcl).
# (lean + apl dropped from the report: lean's per-sample AOT compile made it slow;
# apl, once the closeable folds close, only sorted to the end. luajit stays -- it's
# the inspiration for the auto-JIT.)
ALL_LANGS := love \
             go rust java \
             julia luajit pypy node \
             elixir chez sbcl

# the columns `make bench` shows by default.
DEFAULT_LANGS := love go rust julia luajit pypy node

# per-language: source extension, harness file (a result depends on it), the
# interpreter binary (for the PATH presence check), and any extra prereq.
EXT_ai := l
EXT_chez := ss
EXT_sbcl := lisp
EXT_elixir := exs
EXT_pypy := py
EXT_node := js
EXT_luajit := lua
EXT_julia := jl
EXT_go := go
EXT_java := java
EXT_rust := rs
EXT_lean := lean
EXT_apl := apl

HARN_ai := $(HARN)
HARN_chez := lib/bench.ss
HARN_sbcl := lib/bench.lisp
HARN_elixir := lib/bench.exs
HARN_pypy := lib/bench.py
HARN_node := lib/bench.js
HARN_luajit := lib/bench.lua
HARN_julia := lib/bench.jl
HARN_go := lib/bench.go
HARN_java := lib/Bench.java
HARN_rust := lib/bench.rs
HARN_lean := lib/Bench.lean
HARN_apl := lib/bench.apl

BIN_chez := chez
BIN_sbcl := sbcl
BIN_elixir := elixir
BIN_pypy := pypy3
BIN_node := node
BIN_luajit := luajit
BIN_julia := julia
BIN_go := go
BIN_java := javac
BIN_rust := rustc
BIN_lean := lean
BIN_apl := dyalogscript

EXTRA_ai := $(GL)

# love is always available (we build it); others only if their binary is on PATH.
present = $(if $(filter love,$(1)),$(1),$(if $(shell command -v $(BIN_$(1)) 2>/dev/null),$(1)))
PRESENT := $(foreach L,$(ALL_LANGS),$(call present,$(L)))
SHOW    := $(filter $(PRESENT),$(DEFAULT_LANGS))

txt = $(addprefix $(BDIR)/,$(addsuffix .txt,$(1)))

# a failed recipe takes its half-written target with it -- else a 0-byte artifact
# carries a fresh mtime and the next make calls it up to date. this makefile stands
# alone (the root does not include it), so it says so itself. see ../Makefile.
.DELETE_ON_ERROR:

.PHONY: all bench run raw html clean satrace setup ccbench ccsize ccdead $(ALL_LANGS)
.DEFAULT_GOAL := bench

# Build/refresh the host binary via the root Makefile.
$(GL):
	@$(MAKE) -s -C $(R) host

$(BDIR):
	@mkdir -p $(BDIR)

# Per-language result file + single-language report target. The result file is
# regenerated (benches re-run) only when its sources change; the phony <lang>
# target just formats it, alongside love for contrast (love shows only itself).
define LANG_rule
$$(BDIR)/$(1).txt: $(HARN_$(1)) $$(wildcard benches/*.$(EXT_$(1))) run.sh $(EXTRA_$(1)) | $$(BDIR)
	@./run.sh $(1) "$$(BENCHES)" $$(TIMEOUT) "$$(SKIP)" $$(SAMPLES) > $$@

$(1): $$(call txt,$(1)) $(if $(filter love,$(1)),,$$(call txt,love))
	@awk -v langs='$(if $(filter love,$(1)),love,love $(1))' -f report.awk $$^
endef
$(foreach L,$(ALL_LANGS),$(eval $(call LANG_rule,$(L))))

# Default view: the chosen column set (minus any whose interpreter is absent).
bench: $(call txt,$(SHOW))
	@awk -v langs='$(SHOW)' -f report.awk $^

# Full view: every language present on this machine.
all run: $(call txt,$(PRESENT))
	@awk -v langs='$(PRESENT)' -f report.awk $^

raw: $(call txt,$(PRESENT))
	@cat $^

# The SAT-solver shootout (the page's SECOND table): love's CDCL vs the reference C
# solvers on PHP(5..8). Cached like the language results -- re-run only when the
# runner or the solver changes. `make satrace` to refresh it on its own.
SATRACE := $(BDIR)/satrace.txt
$(SATRACE): satrace.sh $(R)/crew/sat/sat.l $(GL) | $(BDIR)
	@./satrace.sh $(TIMEOUT) > $@
satrace: $(SATRACE)
	@cat $(SATRACE)

# The cold-start row: median source->trivial-result wall-clock per language (compiled langs pay
# their compile; interpreters pay startup). Cached; `make setup` to refresh it on its own.
SETUP := $(BDIR)/setup.txt
$(SETUP): setup.sh $(GL) | $(BDIR)
	@./setup.sh > $@
setup: $(SETUP)
	@cat $(SETUP)

# The compiler shootout (the page's FOURTH table): build the love host binary with
# mooncc / gcc / clang, then run the full corpus ($t) and the two cipher rows
# (bench/ccrypto.l) through each. Rows build+test+chacha+poly1305,
# columns the compilers. ai_cflags + the corpus come from the Makefile (one source of
# truth); the mooncc lane needs out/host/mooncc(+.image), so depend on `host`. Cached;
# `make ccbench` to refresh it on its own. (mooncc is x86-64 only -- dnf elsewhere.)
CCBENCH := $(BDIR)/ccbench.txt
$(CCBENCH): ccbench.sh ccrypto.l $(GL) $(R)/core/love.c $(wildcard $(R)/host/*.c) | $(BDIR)
	@$(MAKE) -s -C $(R) host out/host/mooncc out/host/mooncc.image
	@LOVE_CFLAGS='$(ai_cflags)' CORPUS='$(t)' ./ccbench.sh $(TIMEOUT) $(SAMPLES) > $@
ccbench: $(CCBENCH)
	@cat $(CCBENCH)

# the SIZE half of the same differential -- .text per lane, split into love's own C and
# the libc under it, over the binaries ccbench left behind. Not cached: it only reads.
ccsize: $(CCBENCH)
	@./ccsize.sh

# how much of that libc the binary can ever REACH -- the reachability half, which is
# what tells a fat libc from an indivisible one. CCDEAD_V=1 lists the biggest dead.
ccdead: $(CCBENCH)
	@./ccdead.py

# Self-contained results page: bench/bench.html, the full roster as columns
# (a present-but-empty language shows a dotted column), with a transpose button
# and a viewport-chosen initial orientation, plus the SAT-solver + compiler tables.
# Regenerate after a fresh `make all` (and `make satrace`/`make setup`/`make ccbench` for fresh times).
html: $(call txt,$(PRESENT)) $(SATRACE) $(SETUP) $(CCBENCH) mkhtml.sh | $(BDIR)
	@cat $(call txt,$(PRESENT)) $(SETUP) | ./mkhtml.sh "$(ALL_LANGS)" "$(SATRACE)" "$(CCBENCH)" > bench.html
	@echo "wrote bench/bench.html"

clean:
	@rm -rf $(BDIR)
