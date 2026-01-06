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

.PHONY: all compile compile-traits fast test test-verbose test-dsl watch clean help info reload

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

# Pattern rule for compiling user classes (output to .compiled, not .compiled/user)
# Note: This rule must come before the main class rule to take precedence
$(COMPILED_DIR)/%: $(USER_DIR)/%.trash $(LIB_DIR)/jq-compiler/driver.bash
	@echo "Compiling user class $<..."
	@$(LIB_DIR)/jq-compiler/driver.bash compile "$<" 2>/dev/null > "$@"
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
		$(LIB_DIR)/jq-compiler/driver.bash compile "$$src" 2>/dev/null > "$$outfile"; \
		echo "  → $$outfile"; \
	done

# Individual namespace targets (generated dynamically)
# This enables parallel compilation of namespace classes
$(COMPILED_NAMESPACES): $(COMPILED_DIR)
	@src="$(TRASH_DIR)/$$(echo '$(@F)' | sed 's/__/\//g').trash"; \
	echo "Compiling namespace class $$src..."; \
	$(LIB_DIR)/jq-compiler/driver.bash compile "$$src" 2>/dev/null > "$@"; \
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
	$(LIB_DIR)/jq-compiler/driver.bash compile "$$srcfile" 2>/dev/null > "$$outfile"; \
	echo "✓ $(CLASS) compiled → $$outfile"

# =====================
# NATIVE PLUGIN TARGETS
# =====================

# Procyon compiler location
PROCYON := ~/dev/go/procyon/procyon
PROCYON_SRC := ~/dev/go/procyon

# Platform-specific shared library extension
DYLIB_EXT := $(shell if [ "$$(uname)" = "Darwin" ]; then echo "dylib"; else echo "so"; fi)

# Find all classes that can be compiled to plugins
# (Exclude Test* classes and internal system classes)
PLUGIN_CANDIDATES := $(filter-out $(wildcard $(TRASH_DIR)/Test*.trash),$(SOURCES))
PLUGINS := $(patsubst $(TRASH_DIR)/%.trash,$(COMPILED_DIR)/%.$(DYLIB_EXT),$(PLUGIN_CANDIDATES))

.PHONY: plugins plugin daemon native clean-native binary binaries

# Build a single class as a standalone native binary (usage: make binary CLASS=Counter)
binary:
ifndef CLASS
	@echo "Usage: make binary CLASS=ClassName"
	@exit 1
endif
	@echo "Building native binary for $(CLASS)..."
	@srcfile="$(TRASH_DIR)/$(CLASS).trash"; \
	if [[ ! -f "$$srcfile" ]]; then \
		echo "Error: $$srcfile not found"; \
		exit 1; \
	fi; \
	mkdir -p $(COMPILED_DIR)/.build; \
	$(LIB_DIR)/jq-compiler/driver.bash parse "$$srcfile" 2>/dev/null | \
		$(PROCYON) --mode=binary 2>/dev/null > "$(COMPILED_DIR)/.build/$(CLASS).go"; \
	cp "$$srcfile" "$(COMPILED_DIR)/.build/$(CLASS).trash"; \
	cd $(COMPILED_DIR)/.build && \
		go mod init $(CLASS) 2>/dev/null || true; \
		go get github.com/mattn/go-sqlite3 2>/dev/null || true; \
		CGO_ENABLED=1 go build -o ../$(CLASS).native $(CLASS).go; \
	echo "✓ $(CLASS) native binary built → $(COMPILED_DIR)/$(CLASS).native"

# Build native binaries for all eligible classes
binaries: $(COMPILED_DIR)/.build
	@echo "Building native binaries (this requires CGO for sqlite3)..."
	@cd $(COMPILED_DIR)/.build && \
		go mod init trashtalk-binaries 2>/dev/null || true && \
		go get github.com/mattn/go-sqlite3 2>/dev/null || true
	@for src in $(PLUGIN_CANDIDATES); do \
		class=$$(basename "$$src" .trash); \
		echo "Building native binary for $$class..."; \
		$(LIB_DIR)/jq-compiler/driver.bash parse "$$src" 2>/dev/null | \
			$(PROCYON) --mode=binary 2>/dev/null > "$(COMPILED_DIR)/.build/$$class.go"; \
		cp "$$src" "$(COMPILED_DIR)/.build/$$class.trash"; \
		(cd $(COMPILED_DIR)/.build && \
			CGO_ENABLED=1 go build -o ../$$class.native $$class.go 2>/dev/null); \
		if [[ -f "$(COMPILED_DIR)/$$class.native" ]]; then \
			echo "  ✓ $$class.native"; \
		else \
			echo "  ✗ $$class failed to build"; \
		fi; \
	done
	@echo "Native binary build complete"

# Build a single class as a plugin (usage: make plugin CLASS=Counter)
plugin:
ifndef CLASS
	@echo "Usage: make plugin CLASS=ClassName"
	@exit 1
endif
	@echo "Building plugin for $(CLASS)..."
	@srcfile="$(TRASH_DIR)/$(CLASS).trash"; \
	if [[ ! -f "$$srcfile" ]]; then \
		echo "Error: $$srcfile not found"; \
		exit 1; \
	fi; \
	mkdir -p $(COMPILED_DIR)/.build; \
	$(LIB_DIR)/jq-compiler/driver.bash parse "$$srcfile" 2>/dev/null | \
		$(PROCYON) --mode=plugin 2>/dev/null > "$(COMPILED_DIR)/.build/$(CLASS).go"; \
	cd $(COMPILED_DIR)/.build && \
		go mod init $(CLASS) 2>/dev/null || true; \
		go get github.com/mattn/go-sqlite3 2>/dev/null || true; \
		CGO_ENABLED=1 go build -buildmode=c-shared -o ../$(CLASS).$(DYLIB_EXT) $(CLASS).go; \
	echo "✓ $(CLASS) plugin built → $(COMPILED_DIR)/$(CLASS).$(DYLIB_EXT)"

# Build plugins for all eligible classes
plugins: $(COMPILED_DIR)/.build
	@echo "Building plugins (this requires CGO)..."
	@cd $(COMPILED_DIR)/.build && \
		go mod init trashtalk-plugins 2>/dev/null || true && \
		go get github.com/mattn/go-sqlite3 2>/dev/null || true
	@for src in $(PLUGIN_CANDIDATES); do \
		class=$$(basename "$$src" .trash); \
		echo "Building plugin for $$class..."; \
		$(LIB_DIR)/jq-compiler/driver.bash parse "$$src" 2>/dev/null | \
			$(PROCYON) --mode=plugin 2>/dev/null > "$(COMPILED_DIR)/.build/$$class.go"; \
		cd $(COMPILED_DIR)/.build && \
			CGO_ENABLED=1 go build -buildmode=c-shared -o ../$$class.$(DYLIB_EXT) $$class.go 2>/dev/null; \
		if [[ -f "$(COMPILED_DIR)/$$class.$(DYLIB_EXT)" ]]; then \
			echo "  ✓ $$class.$(DYLIB_EXT)"; \
		else \
			echo "  ✗ $$class failed to build"; \
		fi; \
	done
	@echo "Plugin build complete"

$(COMPILED_DIR)/.build:
	@mkdir -p $(COMPILED_DIR)/.build

# Build the trashtalk-daemon
daemon:
	@echo "Building trashtalk-daemon..."
	@cd $(PROCYON_SRC) && go build ./cmd/trashtalk-daemon
	@mkdir -p bin
	@cp $(PROCYON_SRC)/trashtalk-daemon bin/
	@echo "✓ trashtalk-daemon installed → bin/trashtalk-daemon"

# Build procyon compiler (from Go source)
procyon:
	@echo "Building procyon compiler..."
	@cd $(PROCYON_SRC) && go build ./cmd/procyon
	@echo "✓ procyon built → $(PROCYON_SRC)/procyon"

# Full native build: standalone binaries for all classes
# (This is what integrates with lib/trash.bash's native dispatch)
native: binaries
	@echo ""
	@echo "Native build complete!"
	@echo "  Binaries: $(COMPILED_DIR)/*.native"

# Full native build with daemon and plugins (for daemon-based dispatch)
native-daemon: daemon plugins
	@echo ""
	@echo "Native daemon build complete!"
	@echo "  Daemon: bin/trashtalk-daemon"
	@echo "  Plugins: $(COMPILED_DIR)/*.$(DYLIB_EXT)"

# Clean native artifacts
clean-native:
	@echo "Cleaning native artifacts..."
	@rm -f $(COMPILED_DIR)/*.native
	@rm -f $(COMPILED_DIR)/*.$(DYLIB_EXT)
	@rm -f $(COMPILED_DIR)/*.h
	@rm -rf $(COMPILED_DIR)/.build
	@rm -f bin/trashtalk-daemon
	@echo "✓ Native artifacts cleaned"

# Help
help:
	@echo "Trashtalk Makefile"
	@echo "=================="
	@echo ""
	@echo "Targets:"
	@echo "  make              - Parallel compile all .trash files (default, $(NPROCS) jobs)"
	@echo "  make fast         - Same as above (parallel compile)"
	@echo "  make compile      - Sequential compile (for debugging)"
	@echo "  make single CLASS=Name - Compile a single class"
	@echo "  make test         - Run all bash tests"
	@echo "  make test-dsl     - Run DSL test classes (Test*.trash)"
	@echo "  make test-verbose - Run bash tests with verbose output"
	@echo "  make watch        - Watch for changes and auto-recompile"
	@echo "  make clean        - Remove compiled files"
	@echo "  make info         - Show project information"
	@echo "  make help         - Show this help"
	@echo ""
	@echo "Native Targets:"
	@echo "  make binary CLASS=Name - Build a single class as native binary"
	@echo "  make binaries     - Build all classes as native binaries"
	@echo "  make native       - Same as binaries (integrates with @ dispatch)"
	@echo "  make procyon      - Build the procyon compiler"
	@echo "  make clean-native - Remove native artifacts"
	@echo ""
	@echo "Daemon/Plugin Targets (optional, for persistent daemon):"
	@echo "  make plugin CLASS=Name - Build a single class as c-shared plugin"
	@echo "  make plugins      - Build all classes as plugins"
	@echo "  make daemon       - Build the trashtalk-daemon"
	@echo "  make native-daemon - Build daemon + all plugins"
