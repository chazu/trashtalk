# Trashtalk Makefile
# Build, test, and development workflow

# Use Homebrew bash on macOS for associative array support
SHELL := $(shell command -v /opt/homebrew/bin/bash 2>/dev/null || echo /bin/bash)
TRASH_DIR := trash
COMPILED_DIR := $(TRASH_DIR)/.compiled
TRAITS_DIR := $(TRASH_DIR)/traits
COMPILED_TRAITS_DIR := $(COMPILED_DIR)/traits
LIB_DIR := lib
TESTS_DIR := tests

# Find all .trash source files
SOURCES := $(wildcard $(TRASH_DIR)/*.trash)
TRAIT_SOURCES := $(wildcard $(TRASH_DIR)/traits/*.trash)

# Compiled outputs (strip .trash extension)
COMPILED := $(patsubst $(TRASH_DIR)/%.trash,$(COMPILED_DIR)/%,$(SOURCES))
COMPILED_TRAITS := $(patsubst $(TRASH_DIR)/traits/%.trash,$(COMPILED_TRAITS_DIR)/%,$(TRAIT_SOURCES))

.PHONY: all compile compile-traits test test-verbose test-dsl watch clean help info reload

# Default target
all: compile

# Compile all .trash files
compile: $(COMPILED_DIR) $(COMPILED_TRAITS_DIR) $(COMPILED) $(COMPILED_TRAITS)
	@echo "✓ All classes compiled"

# Compile traits
compile-traits: $(COMPILED_TRAITS_DIR) $(COMPILED_TRAITS)
	@echo "✓ All traits compiled"

# Create compiled directories
$(COMPILED_DIR):
	@mkdir -p $(COMPILED_DIR)

$(COMPILED_TRAITS_DIR):
	@mkdir -p $(COMPILED_TRAITS_DIR)

# Pattern rule for compiling classes (using jq-based compiler)
$(COMPILED_DIR)/%: $(TRASH_DIR)/%.trash $(LIB_DIR)/jq-compiler/driver.bash
	@echo "Compiling $<..."
	@$(LIB_DIR)/jq-compiler/driver.bash compile "$<" 2>/dev/null > "$@"
	@echo "  → $@"

# Pattern rule for compiling traits (using jq-based compiler)
$(COMPILED_TRAITS_DIR)/%: $(TRASH_DIR)/traits/%.trash $(LIB_DIR)/jq-compiler/driver.bash
	@echo "Compiling trait $<..."
	@$(LIB_DIR)/jq-compiler/driver.bash compile "$<" 2>/dev/null > "$@"
	@echo "  → $@"

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
	@echo "Watching for changes in $(TRASH_DIR)/*.trash..."
	@echo "Press Ctrl+C to stop"
	@if command -v fswatch >/dev/null 2>&1; then \
		fswatch -o $(TRASH_DIR)/*.trash $(TRASH_DIR)/traits/*.trash 2>/dev/null | \
		while read; do \
			echo ""; \
			echo "[$(shell date '+%H:%M:%S')] Change detected, recompiling..."; \
			$(MAKE) compile --no-print-directory; \
		done; \
	elif command -v inotifywait >/dev/null 2>&1; then \
		while inotifywait -q -e modify $(TRASH_DIR)/*.trash $(TRASH_DIR)/traits/*.trash 2>/dev/null; do \
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
	@echo "Source files:    $(words $(SOURCES)) classes, $(words $(TRAIT_SOURCES)) traits"
	@echo "Compiled dir:    $(COMPILED_DIR)"
	@echo ""
	@echo "Classes:"
	@for src in $(SOURCES); do echo "  - $$(basename $$src .trash)"; done
	@echo ""
	@echo "Traits:"
	@for src in $(TRAIT_SOURCES); do echo "  - $$(basename $$src .trash)"; done

# Reload the shell environment (for interactive use)
reload:
	@echo "To reload Trashtalk in your current shell, run:"
	@echo "  source $(LIB_DIR)/trash.bash"

# Compile a single class (usage: make single CLASS=Counter)
single:
ifndef CLASS
	@echo "Usage: make single CLASS=ClassName"
	@exit 1
endif
	@echo "Compiling $(CLASS)..."
	@$(LIB_DIR)/jq-compiler/driver.bash compile "$(TRASH_DIR)/$(CLASS).trash" 2>/dev/null > "$(COMPILED_DIR)/$(CLASS)"
	@echo "✓ $(CLASS) compiled → $(COMPILED_DIR)/$(CLASS)"

# Help
help:
	@echo "Trashtalk Makefile"
	@echo "=================="
	@echo ""
	@echo "Targets:"
	@echo "  make              - Compile all .trash files (default)"
	@echo "  make compile      - Compile all .trash files"
	@echo "  make single CLASS=Name - Compile a single class"
	@echo "  make test         - Run all bash tests"
	@echo "  make test-dsl     - Run DSL test classes (Test*.trash)"
	@echo "  make test-verbose - Run bash tests with verbose output"
	@echo "  make watch        - Watch for changes and auto-recompile"
	@echo "  make clean        - Remove compiled files"
	@echo "  make info         - Show project information"
	@echo "  make help         - Show this help"
