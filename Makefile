# Trashtalk Makefile
# Build system for Trashtalk DSL
#
# Architecture:
#   .trash source -> jq-compiler -> compiled bash functions
#
# Usage:
#   make          - Compile all classes to bash
#   make test     - Run all tests
#   make clean    - Remove all build artifacts

# Use Homebrew bash on macOS for associative array support
SHELL := $(shell command -v /opt/homebrew/bin/bash 2>/dev/null || echo /bin/bash)

# Directories
TRASH_DIR := trash
COMPILED_DIR := $(TRASH_DIR)/.compiled
LIB_DIR := lib
TESTS_DIR := tests

# AST cache follows TRASHTALK_DIR, which may be overridden independently of
# the repo's .compiled output, so `clean` must remove it explicitly.
TRASHTALK_DIR ?= $(CURDIR)
AST_CACHE_DIR := $(TRASHTALK_DIR)/trash/.compiled/.astcache

# Tools
JQ_COMPILER := $(LIB_DIR)/jq-compiler/driver.bash

# Platform detection (for parallel jobs)
UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
    NPROCS := $(shell sysctl -n hw.ncpu 2>/dev/null || echo 4)
else
    NPROCS := $(shell nproc 2>/dev/null || echo 4)
endif

# Source files
SOURCES := $(wildcard $(TRASH_DIR)/*.trash)
TRAIT_SOURCES := $(wildcard $(TRASH_DIR)/traits/*.trash)
USER_SOURCES := $(wildcard $(TRASH_DIR)/user/*.trash)
NAMESPACE_SOURCES := $(filter-out $(wildcard $(TRASH_DIR)/traits/*.trash) $(wildcard $(TRASH_DIR)/user/*.trash), \
                      $(wildcard $(TRASH_DIR)/*/*.trash))
ALL_SOURCES := $(SOURCES) $(TRAIT_SOURCES) $(USER_SOURCES) $(NAMESPACE_SOURCES)

.PHONY: all bash test test-serial test-compiler verify test-verbose clean help info single watch doctor bench

# =============================================================================
# Main Targets
# =============================================================================

# Default: compile to bash
all: bash

# Benchmarks exercise installed/generated classes; build before measuring.
bench: bash
	@bash bin/trash-bench

# Diagnose environment/setup issues (bash version, deps, sqlite3, compiled classes)
doctor: bash
	@source $(LIB_DIR)/trash.bash 2>/dev/null && @ Trash doctor

# =============================================================================
# Bash Compilation
# =============================================================================

bash: $(COMPILED_DIR) $(COMPILED_DIR)/traits
	@echo "Compiling to bash ($(NPROCS) parallel jobs)..."
	@printf '%s\n' $(ALL_SOURCES) | xargs -P$(NPROCS) -I{} \
		$(LIB_DIR)/compile-bash.sh {} $(COMPILED_DIR) $(JQ_COMPILER) $(TRASH_DIR)
	@echo "✓ Compilation complete"

# =============================================================================
# Single Class Compilation
# =============================================================================

single:
ifndef CLASS
	@echo "Usage: make single CLASS=ClassName"
	@echo "       make single CLASS=Namespace/ClassName"
	@exit 1
endif
	@classarg="$(CLASS)"; \
	classarg=$$(echo "$$classarg" | sed 's/__/\//g'); \
	srcfile=""; \
	for dir in "$(TRASH_DIR)" "$(TRASH_DIR)/traits" "$(TRASH_DIR)/user"; do \
		if [[ -f "$$dir/$$classarg.trash" ]]; then \
			srcfile="$$dir/$$classarg.trash"; \
			break; \
		fi; \
	done; \
	if [[ -z "$$srcfile" ]]; then \
		srcfile="$(TRASH_DIR)/$$classarg.trash"; \
	fi; \
	if [[ ! -f "$$srcfile" ]]; then \
		echo "Error: $$srcfile not found"; \
		exit 1; \
	fi; \
	$(LIB_DIR)/compile-bash.sh "$$srcfile" $(COMPILED_DIR) $(JQ_COMPILER) $(TRASH_DIR)

# =============================================================================
# Testing
# =============================================================================

test: bash
	@echo ""
	@bash $(LIB_DIR)/run-tests.sh $(TESTS_DIR)

test-serial: bash
	@echo ""
	@bash $(LIB_DIR)/run-tests.sh $(TESTS_DIR) --serial

test-compiler: bash
	@bash $(LIB_DIR)/run-tests.sh $(LIB_DIR)/jq-compiler/tests

verify: test test-compiler

test-verbose: bash
	@echo "Running tests (verbose)..."
	@for test in $(TESTS_DIR)/test_*.bash; do \
		if [[ -f "$$test" ]]; then \
			echo ""; \
			echo "=== $$(basename $$test) ==="; \
			TRASH_TEST_TRACE=1 bash $(LIB_DIR)/test-isolated.bash "$$test"; \
		fi; \
	done

# =============================================================================
# Watch Mode
# =============================================================================

watch:
	@echo "Watching for changes..."
	@echo "Press Ctrl+C to stop"
	@if command -v fswatch >/dev/null 2>&1; then \
		fswatch -o $(TRASH_DIR)/*.trash $(TRASH_DIR)/traits/*.trash $(TRASH_DIR)/user/*.trash $(TRASH_DIR)/*/*.trash 2>/dev/null | \
		while read; do \
			echo ""; \
			echo "[$(shell date '+%H:%M:%S')] Change detected, rebuilding..."; \
			$(MAKE) bash; \
		done; \
	else \
		echo "Error: fswatch not found. Install with: brew install fswatch"; \
		exit 1; \
	fi

# =============================================================================
# Cleanup
# =============================================================================

clean:
	@echo "Cleaning all build artifacts..."
	@rm -rf $(COMPILED_DIR)
	@rm -rf $(AST_CACHE_DIR)
	@echo "✓ Clean complete"

# =============================================================================
# Directory Creation
# =============================================================================

$(COMPILED_DIR):
	@mkdir -p $(COMPILED_DIR)

$(COMPILED_DIR)/traits:
	@mkdir -p $(COMPILED_DIR)/traits

# =============================================================================
# Help & Info
# =============================================================================

help:
	@echo "Trashtalk Build System"
	@echo "======================"
	@echo ""
	@echo "Build Targets:"
	@echo "  make              Compile all .trash files to bash"
	@echo "  make bash         Same as above"
	@echo ""
	@echo "Single Class:"
	@echo "  make single CLASS=Counter"
	@echo "  make single CLASS=Tools/Jq"
	@echo ""
	@echo "Testing:"
	@echo "  make verify       Build and run both isolated test suites"
	@echo "  make test         Run isolated runtime tests in parallel"
	@echo "  make test-compiler Run isolated compiler tests in parallel"
	@echo "  make test-verbose Run tests with bash -x"
	@echo ""
	@echo "Other:"
	@echo "  make watch        Watch and rebuild on changes"
	@echo "  make clean        Remove all build artifacts"
	@echo "  make info         Show project information"

info:
	@echo "Trashtalk Project Info"
	@echo "======================"
	@echo ""
	@echo "Source Files:"
	@echo "  Classes:    $(words $(SOURCES))"
	@echo "  Traits:     $(words $(TRAIT_SOURCES))"
	@echo "  User:       $(words $(USER_SOURCES))"
	@echo "  Namespaced: $(words $(NAMESPACE_SOURCES))"
	@echo ""
	@echo "Directories:"
	@echo "  Source:     $(TRASH_DIR)/"
	@echo "  Compiled:   $(COMPILED_DIR)/"
	@echo ""
	@echo "Compiler:"
	@echo "  jq-compiler: $(JQ_COMPILER)"
