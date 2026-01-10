# ==============================================================================
# Trashtalk Intermediate Representation (IR) Layer
# ==============================================================================
#
# A proof-of-concept IR layer for the jq compiler. This validates the IR design
# before a full Go port and provides:
#
#   1. Name resolution with scope tracking
#   2. Backend marking for expressions (bash vs portable)
#   3. Method and class IR construction
#
# Usage:
#   # Name resolution
#   echo '{"ivars": {"count": {"type": "any"}}, "locals": {}, "params": {}}' | \
#     jq -f ir.jq --arg name "count" 'resolve_name($name)'
#
#   # Backend marking
#   echo '{"type": "subshell", "value": "$(ls)"}' | jq -f ir.jq 'mark_backend'
#
#   # Method IR
#   echo '<method-ast>' | jq -f ir.jq --argjson ivars '{"x": {"type": "any"}}' \
#     'build_method_ir($ivars)'
#
#   # Class IR
#   echo '<class-ast>' | jq -f ir.jq 'build_class_ir'
#
# ==============================================================================

# ==============================================================================
# Name Resolution
# ==============================================================================
#
# Resolves a name to its kind (ivar, local, param, or unknown) given a scope.
# Scope structure:
#   {
#     "ivars": {"name": {"type": "any"}, ...},
#     "locals": {"name": {"type": "any"}, ...},
#     "params": {"name": {"type": "any"}, ...}
#   }
#
# Returns:
#   {"kind": "ivar"|"local"|"param"|"unknown", "name": $name, "type": "any"}

def resolve_name($name):
  # Check each scope in order: locals > params > ivars > unknown
  # (locals shadow params, params shadow ivars)
  if .locals[$name] != null then
    {kind: "local", name: $name, type: (.locals[$name].type // "any")}
  elif .params[$name] != null then
    {kind: "param", name: $name, type: (.params[$name].type // "any")}
  elif .ivars[$name] != null then
    {kind: "ivar", name: $name, type: (.ivars[$name].type // "any")}
  else
    {kind: "unknown", name: $name, type: "any"}
  end;

# ==============================================================================
# Backend Marking
# ==============================================================================
#
# Marks expressions with their required backend:
#   - "bash": Must execute in bash (subshells, bash variables, etc.)
#   - "any": Can execute in any backend (pure expressions, message sends)
#
# Input: Expression AST node
# Output: Same node with added "backend" field (and optionally "reason")

def mark_backend:
  if .type == "subshell" then
    . + {backend: "bash", reason: "subshell expression"}
  elif .type == "arithmetic" then
    . + {backend: "bash", reason: "arithmetic expression"}
  elif .type == "variable" then
    if (.value // "" | startswith("$")) then
      . + {backend: "bash", reason: "bash variable"}
    else
      . + {backend: "any"}
    end
  elif .type == "path" then
    . + {backend: "bash", reason: "file path"}
  elif .type == "command" then
    . + {backend: "bash", reason: "shell command"}
  elif .type == "block" then
    # Blocks may contain bash-specific code
    . + {backend: "bash", reason: "block expression"}
  elif .type == "block_literal" then
    . + {backend: "bash", reason: "block literal with params"}
  elif .type == "message_send" then
    # Message sends to qualified names are portable
    if .receiver.type == "qualified_name" then
      . + {backend: "any"}
    elif .receiver.type == "self" then
      . + {backend: "any"}
    elif .receiver.type == "identifier" then
      # Could be a class or variable - assume portable
      . + {backend: "any"}
    else
      # Default to bash for unknown receivers
      . + {backend: "bash", reason: "unknown receiver type"}
    end
  elif .type == "cascade" then
    . + {backend: "any"}
  elif .type == "binary" then
    # Binary operations like + - * / are portable
    . + {backend: "any"}
  elif .type == "assignment" then
    . + {backend: "any"}
  elif .type == "return" then
    . + {backend: "any"}
  elif .type == "identifier" then
    . + {backend: "any"}
  elif .type == "qualified_name" then
    . + {backend: "any"}
  elif .type == "self" then
    . + {backend: "any"}
  elif .type == "number" then
    . + {backend: "any"}
  elif .type == "string" then
    . + {backend: "any"}
  elif .type == "dstring" then
    # Double-quoted strings may contain interpolation
    if (.value // "" | contains("$")) then
      . + {backend: "bash", reason: "string interpolation"}
    else
      . + {backend: "any"}
    end
  elif .type == "triplestring" then
    . + {backend: "any"}
  elif .type == "symbol" then
    . + {backend: "any"}
  elif .type == "locals" then
    . + {backend: "any"}
  elif .type == "control_flow" then
    # Control flow is portable at the IR level
    . + {backend: "any"}
  elif .type == "json_primitive" then
    . + {backend: "any"}
  else
    # Unknown types default to "any"
    . + {backend: "any"}
  end;

# Recursively mark backends for all nested expressions
def mark_backend_deep:
  mark_backend |
  # Recursively process known child fields
  if .left != null then .left |= mark_backend_deep else . end |
  if .right != null then .right |= mark_backend_deep else . end |
  if .receiver != null then .receiver |= mark_backend_deep else . end |
  if .args != null then .args |= map(mark_backend_deep) else . end |
  if .value != null and (.value | type) == "object" then .value |= mark_backend_deep else . end |
  if .condition != null then .condition |= mark_backend_deep else . end |
  if .then_block != null then .then_block |= mark_backend_deep else . end |
  if .else_block != null then .else_block |= mark_backend_deep else . end;

# ==============================================================================
# Method IR Construction
# ==============================================================================
#
# Builds an IR representation of a method from parser output.
#
# Input: Method object from parser
#   {
#     "type": "method",
#     "kind": "instance"|"class",
#     "raw": true|false,
#     "selector": "methodName",
#     "args": ["arg1", "arg2"],
#     "body": {"type": "block", "tokens": [...]},
#     "pragmas": ["direct", ...]
#   }
#
# Parameters:
#   $class_ivars: Object of instance variable names -> {type: "any"}
#
# Output: IR method object
#   {
#     "selector": "methodName",
#     "kind": "instance"|"class",
#     "raw": true|false,
#     "backend": "bash"|"any",
#     "fallback_reason": "reason"|null,
#     "scope": {...},
#     "pragmas": [...]
#   }

def build_method_ir($class_ivars):
  . as $method |

  # Build scope from class ivars and method args
  ($method.args // [] | map({key: ., value: {type: "any"}}) | from_entries) as $params |

  {
    ivars: $class_ivars,
    locals: {},  # Would be populated by analyzing method body for local declarations
    params: $params
  } as $scope |

  # Determine backend requirement
  if $method.raw == true then
    {
      selector: $method.selector,
      kind: $method.kind,
      raw: true,
      backend: "bash",
      fallback_reason: "raw method",
      scope: $scope,
      pragmas: ($method.pragmas // []),
      args: ($method.args // []),
      keywords: ($method.keywords // [])
    }
  elif ($method.pragmas // [] | any(. == "direct")) then
    {
      selector: $method.selector,
      kind: $method.kind,
      raw: false,
      backend: "bash",
      fallback_reason: "pragma: direct",
      scope: $scope,
      pragmas: ($method.pragmas // []),
      args: ($method.args // []),
      keywords: ($method.keywords // [])
    }
  else
    {
      selector: $method.selector,
      kind: $method.kind,
      raw: false,
      backend: "any",
      fallback_reason: null,
      scope: $scope,
      pragmas: ($method.pragmas // []),
      args: ($method.args // []),
      keywords: ($method.keywords // [])
    }
  end;

# ==============================================================================
# Class IR Construction
# ==============================================================================
#
# Builds an IR representation of a class from parser output.
#
# Input: Parsed class object
#   {
#     "type": "class",
#     "name": "Counter",
#     "parent": "Object",
#     "package": "MyApp"|null,
#     "isTrait": false,
#     "instanceVars": [{"name": "value", "default": {...}}, ...],
#     "traits": ["Debuggable", ...],
#     "methods": [...]
#   }
#
# Output: IR program object
#   {
#     "name": "Counter",
#     "qualified_name": "MyApp::Counter"|"Counter",
#     "parent": "Object",
#     "is_trait": false,
#     "instance_vars": {...},
#     "methods": [<ir-method>, ...]
#   }

def build_class_ir:
  . as $class |

  # Build instance vars lookup from array
  (($class.instanceVars // []) | map({key: .name, value: {type: "any", default: .default}}) | from_entries) as $ivars |

  # Build qualified name
  (if $class.package != null then "\($class.package)::\($class.name)" else $class.name end) as $qualified_name |

  # Transform methods to IR
  (($class.methods // []) | map(build_method_ir($ivars))) as $ir_methods |

  {
    name: $class.name,
    qualified_name: $qualified_name,
    parent: $class.parent,
    parent_package: $class.parentPackage,
    package: $class.package,
    imports: ($class.imports // []),
    is_trait: ($class.isTrait // false),
    instance_vars: $ivars,
    class_instance_vars: (($class.classInstanceVars // []) | map({key: .name, value: {type: "any", default: .default}}) | from_entries),
    traits: ($class.traits // []),
    requires: ($class.requires // []),
    method_requirements: ($class.methodRequirements // []),
    aliases: ($class.aliases // []),
    advice: ($class.advice // []),
    methods: $ir_methods
  };

# ==============================================================================
# Analysis Helpers
# ==============================================================================

# Check if a method requires bash backend
def method_requires_bash:
  .backend == "bash";

# Get all methods that require bash fallback
def methods_requiring_bash:
  .methods | map(select(method_requires_bash));

# Get all portable methods
def portable_methods:
  .methods | map(select(.backend == "any"));

# Summary of backend requirements for a class
def backend_summary:
  {
    total_methods: (.methods | length),
    bash_required: (methods_requiring_bash | length),
    portable: (portable_methods | length),
    fallback_reasons: (methods_requiring_bash | map({selector: .selector, reason: .fallback_reason}))
  };

# ==============================================================================
# Scope Utilities
# ==============================================================================

# Create an empty scope
def empty_scope:
  {ivars: {}, locals: {}, params: {}};

# Add a local variable to scope
def add_local($name):
  .locals[$name] = {type: "any"};

# Add multiple locals to scope
def add_locals($names):
  reduce $names[] as $name (.; add_local($name));

# Add a parameter to scope
def add_param($name):
  .params[$name] = {type: "any"};

# Check if a name is defined in scope
def is_defined($name):
  .locals[$name] != null or .params[$name] != null or .ivars[$name] != null;

# ==============================================================================
# Expression Analysis (for body scanning)
# ==============================================================================

# Check if any expression in a tree requires bash
def any_requires_bash:
  if type != "object" then false
  elif .backend == "bash" then true
  else
    # Check all object values recursively
    to_entries | map(.value | any_requires_bash) | any
  end;

# Collect all bash fallback reasons from an expression tree
def collect_fallback_reasons:
  if type != "object" then []
  elif .backend == "bash" and .reason != null then [.reason]
  else
    to_entries | map(.value | collect_fallback_reasons) | flatten
  end | unique;

# ==============================================================================
# Test Entry Point
# ==============================================================================
#
# To run standalone tests:
#
#   # Test resolve_name
#   echo '{"ivars": {"count": {"type": "any"}}, "locals": {"x": {"type": "any"}}, "params": {"n": {"type": "any"}}}' | \
#     jq -f ir.jq 'resolve_name("count")'
#   # Output: {"kind": "ivar", "name": "count", "type": "any"}
#
#   echo '{"ivars": {"count": {"type": "any"}}, "locals": {"x": {"type": "any"}}, "params": {"n": {"type": "any"}}}' | \
#     jq -f ir.jq 'resolve_name("x")'
#   # Output: {"kind": "local", "name": "x", "type": "any"}
#
#   echo '{"ivars": {"count": {"type": "any"}}, "locals": {"x": {"type": "any"}}, "params": {"n": {"type": "any"}}}' | \
#     jq -f ir.jq 'resolve_name("unknown_var")'
#   # Output: {"kind": "unknown", "name": "unknown_var", "type": "any"}
#
#   # Test mark_backend
#   echo '{"type": "subshell", "value": "$(ls)"}' | jq -f ir.jq 'mark_backend'
#   # Output: {"type": "subshell", "value": "$(ls)", "backend": "bash", "reason": "subshell expression"}
#
#   echo '{"type": "variable", "value": "$HOME"}' | jq -f ir.jq 'mark_backend'
#   # Output: {"type": "variable", "value": "$HOME", "backend": "bash", "reason": "bash variable"}
#
#   echo '{"type": "message_send", "receiver": {"type": "qualified_name", "package": "Core", "name": "Object"}, "selector": "new"}' | \
#     jq -f ir.jq 'mark_backend'
#   # Output: {..., "backend": "any"}
#
#   echo '{"type": "number", "value": "42"}' | jq -f ir.jq 'mark_backend'
#   # Output: {"type": "number", "value": "42", "backend": "any"}
#
#   # Test build_method_ir
#   echo '{"type": "method", "kind": "instance", "raw": false, "selector": "increment", "args": [], "body": {"type": "block", "tokens": []}}' | \
#     jq -f ir.jq 'build_method_ir({"value": {"type": "any"}})'
#   # Output: {... "backend": "any", "fallback_reason": null, ...}
#
#   echo '{"type": "method", "kind": "instance", "raw": true, "selector": "doRaw", "args": ["x"], "body": {"type": "block", "tokens": []}}' | \
#     jq -f ir.jq 'build_method_ir({})'
#   # Output: {... "backend": "bash", "fallback_reason": "raw method", ...}
#
#   # Test build_class_ir (full pipeline)
#   cat <<'EOF' | jq -f ir.jq 'build_class_ir'
#   {
#     "type": "class",
#     "name": "Counter",
#     "parent": "Object",
#     "package": "MyApp",
#     "isTrait": false,
#     "instanceVars": [{"name": "value", "default": {"type": "number", "value": "0"}}],
#     "traits": [],
#     "methods": [
#       {"type": "method", "kind": "instance", "raw": false, "selector": "increment", "args": [], "body": {"type": "block", "tokens": []}},
#       {"type": "method", "kind": "instance", "raw": true, "selector": "rawOp", "args": [], "body": {"type": "block", "tokens": []}}
#     ]
#   }
#   EOF
#   # Output: Full IR with methods marked with backends
#
# ==============================================================================

# Note: This file is a pure library with only function definitions.
# To use as a library: jq 'include "ir"; ...' -L lib/jq-compiler
# For standalone testing, use the test script: tests/test_ir.bash
