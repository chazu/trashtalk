# Trashtalk Makefile
# Simplified build system for Trashtalk DSL
#
# Architecture:
#   - Bash compilation: jq-compiler parses .trash → Procyon generates bash
#   - Native plugins: jq-compiler parses .trash → Procyon generates Go → .dylib
#   - Daemon: tt loads plugins, handles native dispatch
#
# Usage:
#   make          - Full build (bash + plugins + daemon)
#   make bash     - Compile to bash only
#   make plugins  - Build native .dylib plugins only
#   make daemon   - Build tt daemon only
#   make legacy   - Use legacy jq-compiler (no Procyon)
#   make clean    - Remove all build artifacts

# Use Homebrew bash on macOS for associative array support
SHELL := $(shell command -v /opt/homebrew/bin/bash 2>/dev/null || echo /bin/bash)

# Directories
TRASH_DIR := trash
COMPILED_DIR := $(TRASH_DIR)/.compiled
BUILD_DIR := $(COMPILED_DIR)/.build
LIB_DIR := lib
TESTS_DIR := tests

# Tools
PROCYON := $(LIB_DIR)/procyon/procyon
JQ_COMPILER := $(LIB_DIR)/jq-compiler/driver.bash
PROCYON_SRC := $(HOME)/dev/go/procyon

# Platform detection
UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
    DYLIB_EXT := dylib
    NPROCS := $(shell sysctl -n hw.ncpu)
else
    DYLIB_EXT := so
    NPROCS := $(shell nproc 2>/dev/null || echo 4)
endif

# Source files
SOURCES := $(wildcard $(TRASH_DIR)/*.trash)
TRAIT_SOURCES := $(wildcard $(TRASH_DIR)/traits/*.trash)
NAMESPACE_SOURCES := $(filter-out $(wildcard $(TRASH_DIR)/traits/*.trash) $(wildcard $(TRASH_DIR)/user/*.trash), \
                      $(wildcard $(TRASH_DIR)/*/*.trash))
ALL_SOURCES := $(SOURCES) $(TRAIT_SOURCES) $(NAMESPACE_SOURCES)

# Exclude test classes from plugin builds
PLUGIN_SOURCES := $(filter-out $(wildcard $(TRASH_DIR)/Test*.trash),$(SOURCES)) $(NAMESPACE_SOURCES)

.PHONY: all build bash plugins daemon legacy test test-verbose clean clean-plugins help info single watch

# =============================================================================
# Main Targets
# =============================================================================

# Default: full build
all: build

# Full build: bash compilation + native plugins + daemon
build: bash plugins daemon
	@echo ""
	@echo "✓ Build complete"
	@echo "  Bash:    $(COMPILED_DIR)/*"
	@echo "  Plugins: $(COMPILED_DIR)/*.$(DYLIB_EXT)"
	@echo "  Daemon:  $(LIB_DIR)/tt"

# =============================================================================
# Bash Compilation (Procyon --mode=bash)
# =============================================================================

bash: $(COMPILED_DIR) $(COMPILED_DIR)/traits
	@echo "Compiling to bash ($(NPROCS) parallel jobs)..."
	@printf '%s\n' $(ALL_SOURCES) | xargs -P$(NPROCS) -I{} \
		$(LIB_DIR)/compile-bash.sh {} $(COMPILED_DIR) $(JQ_COMPILER) $(PROCYON) $(TRASH_DIR)
	@echo "✓ Bash compilation complete"

# =============================================================================
# Native Plugin Compilation (Procyon --mode=plugin)
# =============================================================================

plugins: $(BUILD_DIR)
	@echo "Building native plugins..."
	@# Initialize Go module for plugin builds
	@cd $(BUILD_DIR) && \
		(go mod init trashtalk-plugins 2>/dev/null || true) && \
		go get github.com/mattn/go-sqlite3 github.com/google/uuid \
		       google.golang.org/grpc google.golang.org/protobuf \
		       github.com/golang/protobuf \
		       github.com/jhump/protoreflect github.com/bufbuild/protocompile
	@# Build plugins in parallel
	@printf '%s\n' $(PLUGIN_SOURCES) | xargs -P$(NPROCS) -I{} \
		$(LIB_DIR)/compile-plugin.sh {} $(BUILD_DIR) $(COMPILED_DIR) $(JQ_COMPILER) $(PROCYON) $(DYLIB_EXT) $(TRASH_DIR)
	@echo "✓ Plugin compilation complete"

# =============================================================================
# Daemon Build
# =============================================================================

daemon:
	@echo ""
	@echo "Building tt daemon..."
	@if [[ -d "$(PROCYON_SRC)" ]]; then \
		if cd "$(PROCYON_SRC)" && go build -o "$(CURDIR)/$(LIB_DIR)/tt" ./cmd/tt; then \
			echo "  ✓ tt"; \
		else \
			echo "  ✗ tt (build failed)"; \
			exit 1; \
		fi; \
	else \
		echo "  ✗ Procyon source not found at $(PROCYON_SRC)"; \
		exit 1; \
	fi

# =============================================================================
# Legacy Build (jq-compiler only, no Procyon)
# =============================================================================

legacy: $(COMPILED_DIR) $(COMPILED_DIR)/traits
	@echo "Compiling with legacy jq-compiler..."
	@for src in $(ALL_SOURCES); do \
		relpath=$${src#$(TRASH_DIR)/}; \
		if [[ "$$relpath" == traits/* ]]; then \
			outname="traits/$${relpath#traits/}"; \
		else \
			outname="$$(echo "$$relpath" | sed 's/\//__/g')"; \
		fi; \
		outname="$${outname%.trash}"; \
		outfile="$(COMPILED_DIR)/$$outname"; \
		mkdir -p "$$(dirname "$$outfile")"; \
		echo "Compiling $$src..."; \
		if $(JQ_COMPILER) compile "$$src" > "$$outfile"; then \
			echo "  → $$outfile"; \
		else \
			echo "  ✗ Failed to compile $$src"; \
			exit 1; \
		fi; \
	done
	@echo "✓ Legacy compilation complete"

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
	srcpath="$$(cd "$$(dirname "$$srcfile")" && pwd)/$$(basename "$$srcfile")"; \
	echo "Compiling $$srcfile..."; \
	echo ""; \
	echo "=== Bash ==="; \
	if $(JQ_COMPILER) parse "$$srcfile" | $(PROCYON) --mode=bash --source-file="$$srcpath" > "$$outfile"; then \
		echo "  ✓ $$outfile"; \
	else \
		echo "  ✗ Bash compilation failed"; \
		exit 1; \
	fi; \
	echo ""; \
	echo "=== Plugin ==="; \
	mkdir -p $(BUILD_DIR); \
	cd $(BUILD_DIR) && go mod init single-build 2>/dev/null || true; \
	cd $(BUILD_DIR) && go get github.com/mattn/go-sqlite3 github.com/google/uuid github.com/golang/protobuf 2>/dev/null || true; \
	gofile="$(BUILD_DIR)/$$outname.go"; \
	if $(JQ_COMPILER) parse "$$srcfile" | $(PROCYON) --mode=shared > "$$gofile" 2>&1; then \
		if [[ -s "$$gofile" ]]; then \
			if (cd $(BUILD_DIR) && CGO_ENABLED=1 go build -buildmode=c-shared -o "$(COMPILED_DIR)/$$outname.$(DYLIB_EXT)" "$$outname.go"); then \
				echo "  ✓ $(COMPILED_DIR)/$$outname.$(DYLIB_EXT)"; \
			else \
				echo "  ✗ Plugin build failed"; \
			fi; \
		else \
			echo "  - Skipped (no native support)"; \
		fi; \
	else \
		echo "  ✗ Plugin codegen failed"; \
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
		fswatch -o $(TRASH_DIR)/*.trash $(TRASH_DIR)/traits/*.trash $(TRASH_DIR)/*/*.trash 2>/dev/null | \
		while read; do \
			echo ""; \
			echo "[$(shell date '+%H:%M:%S')] Change detected, rebuilding..."; \
			$(MAKE) build; \
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
	@rm -f $(LIB_DIR)/tt
	@echo "✓ Clean complete"

clean-plugins:
	@echo "Cleaning native plugins only..."
	@rm -f $(COMPILED_DIR)/*.$(DYLIB_EXT)
	@rm -f $(COMPILED_DIR)/*.h
	@rm -rf $(BUILD_DIR)
	@echo "✓ Plugins cleaned"

# =============================================================================
# Directory Creation
# =============================================================================

$(COMPILED_DIR):
	@mkdir -p $(COMPILED_DIR)

$(COMPILED_DIR)/traits:
	@mkdir -p $(COMPILED_DIR)/traits

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

# =============================================================================
# Help & Info
# =============================================================================

help:
	@echo "Trashtalk Build System"
	@echo "======================"
	@echo ""
	@echo "Build Targets:"
	@echo "  make              Full build (bash + plugins + daemon)"
	@echo "  make bash         Compile .trash to bash only"
	@echo "  make plugins      Build native .dylib plugins"
	@echo "  make daemon       Build tt daemon"
	@echo "  make legacy       Use legacy jq-compiler (no Procyon)"
	@echo ""
	@echo "Single Class:"
	@echo "  make single CLASS=Counter"
	@echo "  make single CLASS=Yutani/Widget"
	@echo ""
	@echo "Testing:"
	@echo "  make test         Run all tests"
	@echo "  make test-verbose Run tests with bash -x"
	@echo ""
	@echo "Other:"
	@echo "  make watch        Watch and rebuild on changes"
	@echo "  make clean        Remove all build artifacts"
	@echo "  make clean-plugins Remove only .dylib files"
	@echo "  make info         Show project information"

info:
	@echo "Trashtalk Project Info"
	@echo "======================"
	@echo ""
	@echo "Source Files:"
	@echo "  Classes:    $(words $(SOURCES))"
	@echo "  Traits:     $(words $(TRAIT_SOURCES))"
	@echo "  Namespaced: $(words $(NAMESPACE_SOURCES))"
	@echo ""
	@echo "Directories:"
	@echo "  Source:     $(TRASH_DIR)/"
	@echo "  Compiled:   $(COMPILED_DIR)/"
	@echo "  Plugins:    $(COMPILED_DIR)/*.$(DYLIB_EXT)"
	@echo ""
	@echo "Tools:"
	@echo "  Procyon:    $(PROCYON)"
	@echo "  jq-compiler: $(JQ_COMPILER)"
	@echo "  Daemon:     $(LIB_DIR)/tt"
