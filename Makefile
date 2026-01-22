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

# Tools
JQ_COMPILER := $(LIB_DIR)/jq-compiler/driver.bash

# Platform detection (for parallel jobs)
UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
    NPROCS := $(shell sysctl -n hw.ncpu)
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

.PHONY: all bash test test-verbose clean help info single watch

# =============================================================================
# Main Targets
# =============================================================================

# Default: compile to bash
all: bash

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
	outname=$$(echo "$$classarg" | sed 's/\//__/g'); \
	outfile="$(COMPILED_DIR)/$$outname"; \
	echo "Compiling $$srcfile..."; \
	if $(JQ_COMPILER) compile "$$srcfile" > "$$outfile"; then \
		echo "  ✓ $$outfile"; \
	else \
		echo "  ✗ Compilation failed"; \
		exit 1; \
	fi

# =============================================================================
# Testing
# =============================================================================

test: bash
	@echo ""
	@echo "Running tests..."
	@failed=0; passed=0; \
	for test in $(TESTS_DIR)/test_*.bash; do \
		if [[ -f "$$test" ]]; then \
			echo ""; \
			echo "=== $$(basename $$test) ==="; \
			if bash "$$test"; then \
				((passed++)); \
			else \
				((failed++)); \
			fi; \
		fi; \
	done; \
	echo ""; \
	echo "================================"; \
	echo "Passed: $$passed, Failed: $$failed"; \
	[[ $$failed -eq 0 ]]

test-verbose: bash
	@echo "Running tests (verbose)..."
	@for test in $(TESTS_DIR)/test_*.bash; do \
		if [[ -f "$$test" ]]; then \
			echo ""; \
			echo "=== $$(basename $$test) ==="; \
			bash -x "$$test"; \
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
	@echo "  make test         Run all tests"
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
