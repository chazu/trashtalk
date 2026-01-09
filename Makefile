# Trashtalk Makefile
# Build, test, and development workflow

# Use Homebrew bash on macOS for associative array support
SHELL := $(shell command -v /opt/homebrew/bin/bash 2>/dev/null || echo /bin/bash)
TRASH_DIR := trash
COMPILED_DIR := $(TRASH_DIR)/.compiled
TRAITS_DIR := $(TRASH_DIR)/traits
COMPILED_TRAITS_DIR := $(COMPILED_DIR)/traits
USER_DIR := $(TRASH_DIR)/user
LIB_DIR := lib
TESTS_DIR := tests

# Compiler selection: procyon (default) or jq
# Override with: make TRASHTALK_COMPILER=jq
TRASHTALK_COMPILER ?= procyon
PROCYON := $(LIB_DIR)/procyon/procyon
JQ_COMPILER := $(LIB_DIR)/jq-compiler/driver.bash

# Parallel build settings - detect CPU count
NPROCS := $(shell sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4)

# Find all .trash source files
SOURCES := $(wildcard $(TRASH_DIR)/*.trash)
TRAIT_SOURCES := $(wildcard $(TRASH_DIR)/traits/*.trash)
USER_SOURCES := $(wildcard $(USER_DIR)/*.trash)
# Namespace subdirectories (e.g., trash/Yutani/*.trash)
NAMESPACE_SOURCES := $(wildcard $(TRASH_DIR)/*/*.trash)
# Filter out traits and user from namespace sources
NAMESPACE_SOURCES := $(filter-out $(TRAIT_SOURCES) $(USER_SOURCES),$(NAMESPACE_SOURCES))

# Compiled outputs (strip .trash extension)
COMPILED := $(patsubst $(TRASH_DIR)/%.trash,$(COMPILED_DIR)/%,$(SOURCES))
COMPILED_TRAITS := $(patsubst $(TRASH_DIR)/traits/%.trash,$(COMPILED_TRAITS_DIR)/%,$(TRAIT_SOURCES))
COMPILED_USER := $(patsubst $(USER_DIR)/%.trash,$(COMPILED_DIR)/%,$(USER_SOURCES))
# Namespace classes: trash/Yutani/Widget.trash -> .compiled/Yutani__Widget
COMPILED_NAMESPACES := $(foreach src,$(NAMESPACE_SOURCES),$(COMPILED_DIR)/$(subst /,__,$(patsubst $(TRASH_DIR)/%.trash,%,$(src))))

.PHONY: all compile compile-traits fast test test-verbose test-dsl watch clean help info reload native native-single clean-native

# Default target - parallel build
all: fast

# Fast parallel compile (default)
fast: $(COMPILED_DIR) $(COMPILED_TRAITS_DIR)
	@$(MAKE) -j$(NPROCS) --no-print-directory compile-all
	@echo "✓ All classes compiled ($(NPROCS) parallel jobs)"

# Internal target for parallel compilation
compile-all: $(COMPILED) $(COMPILED_TRAITS) $(COMPILED_USER) $(COMPILED_NAMESPACES)

# Sequential compile (for debugging)
compile: $(COMPILED_DIR) $(COMPILED_TRAITS_DIR) $(COMPILED) $(COMPILED_TRAITS) $(COMPILED_USER) $(COMPILED_NAMESPACES)
	@echo "✓ All classes compiled (sequential)"

# Compile traits
compile-traits: $(COMPILED_TRAITS_DIR) $(COMPILED_TRAITS)
	@echo "✓ All traits compiled"

# Create compiled directories
$(COMPILED_DIR):
	@mkdir -p $(COMPILED_DIR)

$(COMPILED_TRAITS_DIR):
	@mkdir -p $(COMPILED_TRAITS_DIR)

# Compile helper: tries Procyon first, falls back to jq-compiler on error
# Usage: $(call try_compile,source.trash,output)
# Falls back if: Procyon fails, output is empty, or output has bash syntax errors
define try_compile
if [[ "$(TRASHTALK_COMPILER)" == "jq" ]]; then \
	$(JQ_COMPILER) compile "$(1)" 2>/dev/null > "$(2)"; \
else \
	if ! $(JQ_COMPILER) parse "$(1)" 2>/dev/null | $(PROCYON) --mode=bash 2>/dev/null > "$(2)" \
		|| [[ ! -s "$(2)" ]] \
		|| ! bash -n "$(2)" 2>/dev/null; then \
		$(JQ_COMPILER) compile "$(1)" 2>/dev/null > "$(2)"; \
	fi; \
fi
endef

# Pattern rule for compiling classes
$(COMPILED_DIR)/%: $(TRASH_DIR)/%.trash $(JQ_COMPILER)
	@echo "Compiling $<..."
	@$(call try_compile,$<,$@)
	@echo "  → $@"

# Pattern rule for compiling traits
$(COMPILED_TRAITS_DIR)/%: $(TRASH_DIR)/traits/%.trash $(JQ_COMPILER)
	@echo "Compiling trait $<..."
	@$(call try_compile,$<,$@)
	@echo "  → $@"

# Pattern rule for compiling user classes (output to .compiled, not .compiled/user)
# Note: This rule must come before the main class rule to take precedence
$(COMPILED_DIR)/%: $(USER_DIR)/%.trash $(JQ_COMPILER)
	@echo "Compiling user class $<..."
	@$(call try_compile,$<,$@)
	@echo "  → $@"

# Compile namespace classes (e.g., trash/Yutani/Widget.trash -> .compiled/Yutani__Widget)
.PHONY: compile-namespaces
compile-namespaces:
	@for src in $(NAMESPACE_SOURCES); do \
		relpath=$${src#$(TRASH_DIR)/}; \
		outname=$${relpath%.trash}; \
		outname=$$(echo "$$outname" | sed 's/\//__/g'); \
		outfile="$(COMPILED_DIR)/$$outname"; \
		echo "Compiling namespace class $$src..."; \
		if [[ "$(TRASHTALK_COMPILER)" == "jq" ]]; then \
			$(JQ_COMPILER) compile "$$src" 2>/dev/null > "$$outfile"; \
		else \
			if ! $(JQ_COMPILER) parse "$$src" 2>/dev/null | $(PROCYON) --mode=bash 2>/dev/null > "$$outfile" \
				|| [[ ! -s "$$outfile" ]] \
				|| ! bash -n "$$outfile" 2>/dev/null; then \
				$(JQ_COMPILER) compile "$$src" 2>/dev/null > "$$outfile"; \
			fi; \
		fi; \
		echo "  → $$outfile"; \
	done

# Individual namespace targets (generated dynamically)
# This enables parallel compilation of namespace classes
$(COMPILED_NAMESPACES): $(COMPILED_DIR)
	@src="$(TRASH_DIR)/$$(echo '$(@F)' | sed 's/__/\//g').trash"; \
	echo "Compiling namespace class $$src..."; \
	if [[ "$(TRASHTALK_COMPILER)" == "jq" ]]; then \
		$(JQ_COMPILER) compile "$$src" 2>/dev/null > "$@"; \
	else \
		if ! $(JQ_COMPILER) parse "$$src" 2>/dev/null | $(PROCYON) --mode=bash 2>/dev/null > "$@" \
			|| [[ ! -s "$@" ]] \
			|| ! bash -n "$@" 2>/dev/null; then \
			$(JQ_COMPILER) compile "$$src" 2>/dev/null > "$@"; \
		fi; \
	fi; \
	echo "  → $@"

# Run all tests
test:
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

# Run tests with verbose output
test-verbose:
	@echo "Running tests (verbose)..."
	@for test in $(TESTS_DIR)/test_*.bash; do \
		if [[ -f "$$test" ]]; then \
			echo ""; \
			echo "=== $$(basename $$test) ==="; \
			bash -x "$$test"; \
		fi; \
	done

# Run DSL tests (Trashtalk test classes)
test-dsl: compile
	@echo "Running DSL tests..."
	@failed=0; passed=0; \
	source $(LIB_DIR)/trash.bash 2>/dev/null; \
	for testclass in $(TRASH_DIR)/Test*.trash; do \
		if [[ -f "$$testclass" ]]; then \
			classname=$$(basename "$$testclass" .trash); \
			if [[ "$$classname" != "TestCase" ]]; then \
				echo ""; \
				echo "=== $$classname ==="; \
				if @ "$$classname" runAll 2>&1 | grep -q "failed"; then \
					((failed++)); \
				else \
					((passed++)); \
				fi; \
			fi; \
		fi; \
	done; \
	echo ""; \
	echo "================================"; \
	echo "Test classes: Passed: $$passed, Failed: $$failed"; \
	[[ $$failed -eq 0 ]]

# Watch for changes and recompile (requires fswatch on macOS or inotifywait on Linux)
watch:
	@echo "Watching for changes in $(TRASH_DIR)/*.trash, traits/, and user/..."
	@echo "Press Ctrl+C to stop"
	@if command -v fswatch >/dev/null 2>&1; then \
		fswatch -o $(TRASH_DIR)/*.trash $(TRASH_DIR)/traits/*.trash $(USER_DIR)/*.trash 2>/dev/null | \
		while read; do \
			echo ""; \
			echo "[$(shell date '+%H:%M:%S')] Change detected, recompiling..."; \
			$(MAKE) compile --no-print-directory; \
		done; \
	elif command -v inotifywait >/dev/null 2>&1; then \
		while inotifywait -q -e modify $(TRASH_DIR)/*.trash $(TRASH_DIR)/traits/*.trash $(USER_DIR)/*.trash 2>/dev/null; do \
			echo ""; \
			echo "[$(shell date '+%H:%M:%S')] Change detected, recompiling..."; \
			$(MAKE) compile --no-print-directory; \
		done; \
	else \
		echo "Error: Neither fswatch (macOS) nor inotifywait (Linux) found."; \
		echo "Install with: brew install fswatch (macOS) or apt install inotify-tools (Linux)"; \
		exit 1; \
	fi

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@rm -rf $(COMPILED_DIR)
	@echo "✓ Clean complete"

# Show project info
info:
	@echo "Trashtalk Project Info"
	@echo "======================"
	@echo "Source files:    $(words $(SOURCES)) classes, $(words $(TRAIT_SOURCES)) traits, $(words $(USER_SOURCES)) user classes"
	@echo "Compiled dir:    $(COMPILED_DIR)"
	@echo ""
	@echo "Classes:"
	@for src in $(SOURCES); do echo "  - $$(basename $$src .trash)"; done
	@echo ""
	@echo "Traits:"
	@for src in $(TRAIT_SOURCES); do echo "  - $$(basename $$src .trash)"; done
	@echo ""
	@echo "User Classes:"
	@if [ -n "$(USER_SOURCES)" ]; then \
		for src in $(USER_SOURCES); do echo "  - $$(basename $$src .trash)"; done; \
	else \
		echo "  (none)"; \
	fi

# Reload the shell environment (for interactive use)
reload:
	@echo "To reload Trashtalk in your current shell, run:"
	@echo "  source $(LIB_DIR)/trash.bash"

# Compile a single class (usage: make single CLASS=Counter)
# Handles namespaced classes (package: Foo → outputs Foo__ClassName)
# Searches in trash/, trash/user/, and trash/traits/
single:
ifndef CLASS
	@echo "Usage: make single CLASS=ClassName"
	@exit 1
endif
	@echo "Compiling $(CLASS)..."
	@srcfile=""; \
	if [[ -f "$(TRASH_DIR)/$(CLASS).trash" ]]; then \
		srcfile="$(TRASH_DIR)/$(CLASS).trash"; \
	elif [[ -f "$(USER_DIR)/$(CLASS).trash" ]]; then \
		srcfile="$(USER_DIR)/$(CLASS).trash"; \
	elif [[ -f "$(TRAITS_DIR)/$(CLASS).trash" ]]; then \
		srcfile="$(TRAITS_DIR)/$(CLASS).trash"; \
	else \
		echo "Error: $(CLASS).trash not found in trash/, trash/user/, or trash/traits/"; \
		exit 1; \
	fi; \
	pkg=$$(grep -E '^package:' "$$srcfile" 2>/dev/null | awk '{print $$2}'); \
	if [[ -n "$$pkg" ]]; then \
		outfile="$(COMPILED_DIR)/$${pkg}__$(CLASS)"; \
	elif [[ "$$srcfile" == *"/traits/"* ]]; then \
		outfile="$(COMPILED_TRAITS_DIR)/$(CLASS)"; \
	else \
		outfile="$(COMPILED_DIR)/$(CLASS)"; \
	fi; \
	if [[ "$(TRASHTALK_COMPILER)" == "jq" ]]; then \
		$(JQ_COMPILER) compile "$$srcfile" 2>/dev/null > "$$outfile"; \
	else \
		if ! $(JQ_COMPILER) parse "$$srcfile" 2>/dev/null | $(PROCYON) --mode=bash 2>/dev/null > "$$outfile" \
			|| [[ ! -s "$$outfile" ]] \
			|| ! bash -n "$$outfile" 2>/dev/null; then \
			$(JQ_COMPILER) compile "$$srcfile" 2>/dev/null > "$$outfile"; \
		fi; \
	fi; \
	echo "✓ $(CLASS) compiled → $$outfile"

# =====================
# NATIVE BUILD TARGETS
# =====================

# Procyon compiler location (pre-built binary for plugin compilation)
PROCYON := $(LIB_DIR)/procyon/procyon
# Procyon source directory (for building daemon)
PROCYON_SRC := $(HOME)/dev/go/procyon

# Platform-specific shared library extension
DYLIB_EXT := $(shell if [ "$$(uname)" = "Darwin" ]; then echo "dylib"; else echo "so"; fi)

# Find all classes that can be compiled natively
# (Exclude Test* classes)
PLUGIN_CANDIDATES := $(filter-out $(wildcard $(TRASH_DIR)/Test*.trash),$(SOURCES))
ALL_NATIVE_CANDIDATES := $(PLUGIN_CANDIDATES) $(NAMESPACE_SOURCES)

# Full build: bash compilation + daemon + dylib plugins
# This is the recommended way to build everything with native acceleration
native: fast $(COMPILED_DIR)/.build
	@echo ""
	@echo "Building trashtalk-daemon..."
	@if [ -d "$(PROCYON_SRC)" ]; then \
		cd "$(PROCYON_SRC)" && go build -o $(LIB_DIR)/trashtalk-daemon ./cmd/trashtalk-daemon 2>&1 && \
		echo "  ✓ trashtalk-daemon"; \
	else \
		echo "  ✗ daemon build failed (procyon source not found at $(PROCYON_SRC))"; \
	fi
	@echo ""
	@echo "Building native plugins..."
	@cd $(COMPILED_DIR)/.build && \
		go mod init trashtalk-native 2>/dev/null || true && \
		go get github.com/mattn/go-sqlite3 github.com/google/uuid google.golang.org/grpc google.golang.org/protobuf github.com/jhump/protoreflect 2>/dev/null || true
	@for src in $(ALL_NATIVE_CANDIDATES); do \
		relpath=$${src#$(TRASH_DIR)/}; \
		outname=$${relpath%.trash}; \
		outname=$$(echo "$$outname" | sed 's/\//__/g'); \
		echo "  Building $$outname.$(DYLIB_EXT)..."; \
		$(LIB_DIR)/jq-compiler/driver.bash parse "$$src" 2>/dev/null | \
			$(PROCYON) --mode=plugin 2>/dev/null > "$(COMPILED_DIR)/.build/$$outname.go" 2>/dev/null; \
		if [[ -s "$(COMPILED_DIR)/.build/$$outname.go" ]]; then \
			(cd $(COMPILED_DIR)/.build && \
				CGO_ENABLED=1 go build -buildmode=c-shared -o ../$$outname.$(DYLIB_EXT) $$outname.go 2>/dev/null) && \
			echo "    ✓ $$outname.$(DYLIB_EXT)" || echo "    ✗ $$outname (build failed)"; \
		else \
			echo "    - $$outname (skipped, not supported by procyon)"; \
		fi; \
	done
	@echo ""
	@echo "✓ Native build complete"
	@echo "  Bash:    $(COMPILED_DIR)/*"
	@echo "  Daemon:  $(LIB_DIR)/trashtalk-daemon"
	@echo "  Plugins: $(COMPILED_DIR)/*.$(DYLIB_EXT)"

# Build a single class natively (bash + dylib)
# Usage: make native-single CLASS=Counter
#        make native-single CLASS=Yutani/Widget (namespaced)
native-single: $(COMPILED_DIR)/.build
ifndef CLASS
	@echo "Usage: make native-single CLASS=ClassName"
	@echo "       make native-single CLASS=Namespace/ClassName"
	@exit 1
endif
	@classarg="$(CLASS)"; \
	classarg=$$(echo "$$classarg" | sed 's/__/\//g'); \
	srcfile="$(TRASH_DIR)/$$classarg.trash"; \
	outname=$$(echo "$$classarg" | sed 's/\//__/g'); \
	if [[ ! -f "$$srcfile" ]]; then \
		echo "Error: $$srcfile not found"; \
		exit 1; \
	fi; \
	echo "Compiling $$outname (bash)..."; \
	$(LIB_DIR)/jq-compiler/driver.bash compile "$$srcfile" > "$(COMPILED_DIR)/$$outname"; \
	echo "  → $(COMPILED_DIR)/$$outname"; \
	echo "Building $$outname.$(DYLIB_EXT)..."; \
	cd $(COMPILED_DIR)/.build && \
		go mod init $$outname 2>/dev/null || true && \
		go get github.com/mattn/go-sqlite3 github.com/google/uuid google.golang.org/grpc google.golang.org/protobuf github.com/jhump/protoreflect 2>/dev/null || true; \
	$(LIB_DIR)/jq-compiler/driver.bash parse "$$srcfile" 2>/dev/null | \
		$(PROCYON) --mode=plugin 2>/dev/null > "$(COMPILED_DIR)/.build/$$outname.go"; \
	if [[ -s "$(COMPILED_DIR)/.build/$$outname.go" ]]; then \
		cd $(COMPILED_DIR)/.build && \
			CGO_ENABLED=1 go build -buildmode=c-shared -o ../$$outname.$(DYLIB_EXT) $$outname.go && \
		echo "  → $(COMPILED_DIR)/$$outname.$(DYLIB_EXT)" || echo "  ✗ Build failed"; \
	else \
		echo "  - Skipped (not supported by procyon)"; \
	fi

$(COMPILED_DIR)/.build:
	@mkdir -p $(COMPILED_DIR)/.build

# Clean native artifacts only (keeps bash-compiled files)
clean-native:
	@echo "Cleaning native artifacts..."
	@rm -f $(LIB_DIR)/trashtalk-daemon
	@rm -f $(COMPILED_DIR)/*.$(DYLIB_EXT)
	@rm -f $(COMPILED_DIR)/*.h
	@rm -rf $(COMPILED_DIR)/.build
	@echo "✓ Native artifacts cleaned"

# Help
help:
	@echo "Trashtalk Makefile"
	@echo "=================="
	@echo ""
	@echo "Bash Compilation:"
	@echo "  make              - Parallel compile all .trash files (default)"
	@echo "  make compile      - Sequential compile (for debugging)"
	@echo "  make single CLASS=Name - Compile a single class"
	@echo ""
	@echo "Compiler Selection (default: procyon):"
	@echo "  make TRASHTALK_COMPILER=jq    - Use jq-compiler (legacy)"
	@echo "  make TRASHTALK_COMPILER=procyon - Use Procyon (default)"
	@echo ""
	@echo "Native Build (bash + daemon + dylib plugins):"
	@echo "  make native       - Full build: bash + daemon + all dylib plugins"
	@echo "  make native-single CLASS=Name - Build single class (bash + dylib)"
	@echo "  make clean-native - Remove only native artifacts (.dylib/.so)"
	@echo ""
	@echo "Testing:"
	@echo "  make test         - Run all bash tests"
	@echo "  make test-dsl     - Run DSL test classes (Test*.trash)"
	@echo "  make test-verbose - Run tests with verbose output"
	@echo ""
	@echo "Other:"
	@echo "  make watch        - Watch for changes and auto-recompile"
	@echo "  make clean        - Remove all compiled files"
	@echo "  make info         - Show project information"
