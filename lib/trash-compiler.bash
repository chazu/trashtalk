#!/bin/bash

# Trashtalk DSL Compiler
# Compiles .trash files to namespaced bash functions
#
# DSL Syntax:
#   ClassName subclass: ParentClass
#     instanceVars: var1:default var2
#     include: TraitName
#
#     method: name [
#       body
#     ]
#
#     method: name: arg [
#       body
#     ]
#
#     classMethod: name [
#       body
#     ]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHDIR="${TRASHDIR:-$HOME/.trashtalk/trash}"
COMPILED_DIR="$TRASHDIR/.compiled"

# Ensure compiled directory exists
mkdir -p "$COMPILED_DIR"

# ============================================
# Lexer - Tokenize input
# ============================================

declare -a TOKENS
declare -i TOKEN_POS=0

tokenize() {
    local input="$1"
    TOKENS=()
    TOKEN_POS=0

    local i=0
    local len=${#input}
    local token=""

    while ((i < len)); do
        local char="${input:i:1}"
        local next_char="${input:i+1:1}"

        case "$char" in
            # Whitespace - emit token if any, skip
            $' '|$'\t')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                ((i++))
                ;;
            # Newline - emit token and newline marker
            $'\n')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                TOKENS+=("NEWLINE")
                ((i++))
                ;;
            # Comment - skip to end of line
            '#')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                while ((i < len)) && [[ "${input:i:1}" != $'\n' ]]; do
                    ((i++))
                done
                ;;
            # Special characters
            '[')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                TOKENS+=("LBRACKET")
                ((i++))
                ;;
            ']')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                TOKENS+=("RBRACKET")
                ((i++))
                ;;
            '|')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                TOKENS+=("PIPE")
                ((i++))
                ;;
            '^')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                TOKENS+=("CARET")
                ((i++))
                ;;
            '.')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                TOKENS+=("DOT")
                ((i++))
                ;;
            ':')
                # Check for :=
                if [[ "$next_char" == "=" ]]; then
                    if [[ -n "$token" ]]; then
                        TOKENS+=("$token")
                        token=""
                    fi
                    TOKENS+=("ASSIGN")
                    ((i += 2))
                else
                    # Colon is part of keyword
                    token+="$char"
                    ((i++))
                fi
                ;;
            # String literals
            "'")
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    token=""
                fi
                ((i++))
                local str="'"
                while ((i < len)) && [[ "${input:i:1}" != "'" ]]; do
                    str+="${input:i:1}"
                    ((i++))
                done
                str+="'"
                ((i++))  # Skip closing quote
                TOKENS+=("STRING:$str")
                ;;
            # Numbers
            [0-9]|'-')
                if [[ "$char" == "-" && ! "$next_char" =~ [0-9] ]]; then
                    token+="$char"
                    ((i++))
                else
                    if [[ -n "$token" ]]; then
                        TOKENS+=("$token")
                        token=""
                    fi
                    local num="$char"
                    ((i++))
                    while ((i < len)) && [[ "${input:i:1}" =~ [0-9.] ]]; do
                        num+="${input:i:1}"
                        ((i++))
                    done
                    TOKENS+=("NUMBER:$num")
                fi
                ;;
            # Everything else - accumulate into token
            *)
                token+="$char"
                ((i++))
                ;;
        esac
    done

    # Emit final token
    if [[ -n "$token" ]]; then
        TOKENS+=("$token")
    fi
}

# Get current token
current_token() {
    if ((TOKEN_POS < ${#TOKENS[@]})); then
        echo "${TOKENS[TOKEN_POS]}"
    fi
}

# Advance to next token
advance() {
    ((TOKEN_POS++))
}

# Skip newlines
skip_newlines() {
    while [[ "$(current_token)" == "NEWLINE" ]]; do
        advance
    done
}

# Check if token matches expected
expect() {
    local expected="$1"
    local actual=$(current_token)
    if [[ "$actual" != "$expected" ]]; then
        echo "Parse error: expected '$expected', got '$actual'" >&2
        return 1
    fi
    advance
}

# ============================================
# Parser - Build AST
# ============================================

# Class AST structure (using associative arrays)
declare -A CLASS_AST
declare -a CLASS_METHODS
declare -a CLASS_CLASS_METHODS

parse_class() {
    CLASS_AST=()
    CLASS_METHODS=()
    CLASS_CLASS_METHODS=()

    skip_newlines

    # ClassName subclass: ParentClass
    local class_name=$(current_token)
    advance

    skip_newlines
    expect "subclass:"
    skip_newlines

    local parent_class=$(current_token)
    advance

    CLASS_AST[name]="$class_name"
    CLASS_AST[parent]="$parent_class"
    CLASS_AST[instanceVars]=""
    CLASS_AST[traits]=""

    skip_newlines

    # Parse class body
    while [[ -n "$(current_token)" ]]; do
        skip_newlines
        local token=$(current_token)

        case "$token" in
            "instanceVars:")
                advance
                parse_instance_vars
                ;;
            "include:")
                advance
                local trait=$(current_token)
                advance
                if [[ -n "${CLASS_AST[traits]}" ]]; then
                    CLASS_AST[traits]+=" $trait"
                else
                    CLASS_AST[traits]="$trait"
                fi
                ;;
            "method:")
                advance
                parse_method "instance"
                ;;
            "classMethod:")
                advance
                parse_method "class"
                ;;
            "NEWLINE")
                advance
                ;;
            "")
                break
                ;;
            *)
                echo "Unexpected token: $token" >&2
                advance
                ;;
        esac
    done
}

parse_instance_vars() {
    local vars=""
    skip_newlines

    while [[ -n "$(current_token)" ]]; do
        local token=$(current_token)

        # Stop at next declaration
        if [[ "$token" == "include:" || "$token" == "method:" || "$token" == "classMethod:" || "$token" == "NEWLINE" ]]; then
            break
        fi

        # Handle var:default pattern (var: followed by NUMBER: or value)
        if [[ "$token" == *: && "$token" != "include:" && "$token" != "method:" && "$token" != "classMethod:" ]]; then
            local var_name="${token%:}"
            advance
            local next_token=$(current_token)

            # Check if next token is a default value
            if [[ "$next_token" == NUMBER:* ]]; then
                local default_val="${next_token#NUMBER:}"
                if [[ -n "$vars" ]]; then
                    vars+=" ${var_name}:${default_val}"
                else
                    vars="${var_name}:${default_val}"
                fi
                advance
            elif [[ "$next_token" == STRING:* ]]; then
                local default_val="${next_token#STRING:}"
                default_val="${default_val//\'/}"  # Remove quotes
                if [[ -n "$vars" ]]; then
                    vars+=" ${var_name}:${default_val}"
                else
                    vars="${var_name}:${default_val}"
                fi
                advance
            else
                # No default, just var name
                if [[ -n "$vars" ]]; then
                    vars+=" $var_name"
                else
                    vars="$var_name"
                fi
            fi
        else
            # Plain variable name without colon
            if [[ -n "$vars" ]]; then
                vars+=" $token"
            else
                vars="$token"
            fi
            advance
        fi
    done

    CLASS_AST[instanceVars]="$vars"
}

parse_method() {
    local method_type="$1"  # "instance" or "class"

    # Parse method signature
    local method_name=""
    local method_args=()

    skip_newlines

    # First part of method name
    local first_part=$(current_token)
    advance

    # Check if it's a keyword method (ends with :)
    if [[ "$first_part" == *: ]]; then
        method_name="${first_part%:}"  # Remove trailing colon

        # Get first argument
        local arg=$(current_token)
        advance
        method_args+=("$arg")

        # Check for more keywords
        while [[ "$(current_token)" == *: && "$(current_token)" != "LBRACKET" ]]; do
            local keyword=$(current_token)
            keyword="${keyword%:}"  # Remove trailing colon
            method_name+="_$keyword"
            advance

            arg=$(current_token)
            advance
            method_args+=("$arg")
        done
    else
        # Simple method name (no args)
        method_name="$first_part"
    fi

    skip_newlines

    # Expect method body in brackets
    expect "LBRACKET"

    # Parse method body - reconstruct as readable text
    local body=""
    local bracket_depth=1
    local line=""

    while ((bracket_depth > 0)); do
        local token=$(current_token)

        case "$token" in
            "LBRACKET")
                line+="["
                ((bracket_depth++))
                ;;
            "RBRACKET")
                ((bracket_depth--))
                if ((bracket_depth > 0)); then
                    line+="]"
                fi
                ;;
            "NEWLINE")
                body+="$line"$'\n'
                line=""
                ;;
            "PIPE")
                line+="|"
                ;;
            "CARET")
                line+="^"
                ;;
            "ASSIGN")
                line+=" := "
                ;;
            "DOT")
                line+="."
                ;;
            STRING:*)
                local str="${token#STRING:}"
                # Convert single quotes to double quotes for bash
                str="${str//\'/\"}"
                line+="$str"
                ;;
            NUMBER:*)
                line+="${token#NUMBER:}"
                ;;
            *)
                if [[ -n "$line" && ! "$line" =~ [[:space:]]$ && "$token" != ":" ]]; then
                    line+=" "
                fi
                line+="$token"
                ;;
        esac
        advance
    done

    # Add any remaining line content
    if [[ -n "$line" ]]; then
        body+="$line"$'\n'
    fi

    # Store method info
    # Replace newlines with a placeholder since read stops at newlines
    local encoded_body="${body//$'\n'/__NL__}"
    local method_info="${method_name}"$'\x1f'"${method_args[*]}"$'\x1f'"${encoded_body}"

    if [[ "$method_type" == "class" ]]; then
        CLASS_CLASS_METHODS+=("$method_info")
    else
        CLASS_METHODS+=("$method_info")
    fi
}

# ============================================
# Code Generator - Emit Bash
# ============================================

generate_bash() {
    local class_name="${CLASS_AST[name]}"
    local parent="${CLASS_AST[parent]}"
    local instance_vars="${CLASS_AST[instanceVars]}"
    local traits="${CLASS_AST[traits]}"

    # Header
    cat << EOF
#!/bin/bash
# Generated by Trashtalk Compiler - DO NOT EDIT
# Source: $class_name.trash
# Generated: $(date -Iseconds)

__${class_name}__superclass="$parent"
__${class_name}__instanceVars="$instance_vars"
__${class_name}__traits="$traits"

EOF

    # Generate instance methods
    for method_info in "${CLASS_METHODS[@]}"; do
        IFS=$'\x1f' read -r method_name method_args method_body <<< "$method_info"
        generate_method "$class_name" "$method_name" "$method_args" "$method_body" "instance"
    done

    # Generate class methods
    for method_info in "${CLASS_CLASS_METHODS[@]}"; do
        IFS=$'\x1f' read -r method_name method_args method_body <<< "$method_info"
        generate_method "$class_name" "$method_name" "$method_args" "$method_body" "class"
    done
}

generate_method() {
    local class_name="$1"
    local method_name="$2"
    local method_args="$3"
    local method_body="$4"
    local method_type="$5"

    # Decode newlines in body
    method_body="${method_body//__NL__/$'\n'}"

    local func_name
    if [[ "$method_type" == "class" ]]; then
        func_name="__${class_name}__class__${method_name}"
    else
        func_name="__${class_name}__${method_name}"
    fi

    echo "${func_name}() {"

    # Declare arguments as locals
    local i=1
    for arg in $method_args; do
        echo "  local $arg=\"\$$i\""
        ((i++))
    done

    # Transform method body to bash
    local bash_body
    bash_body=$(transform_body "$method_body")

    # Indent body
    echo "$bash_body" | sed 's/^/  /'

    echo "}"
    echo ""
}

# Transform method body from DSL to bash
# This version handles bash-like expressions directly
transform_body() {
    local body="$1"
    local result=""
    local in_locals=0

    # Process body line by line (simpler approach)
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Skip empty lines
        [[ -z "$line" ]] && continue

        # Trim leading/trailing whitespace
        line="${line#"${line%%[![:space:]]*}"}"
        line="${line%"${line##*[![:space:]]}"}"

        [[ -z "$line" ]] && continue

        # Handle local variable declarations: | var1 var2 |
        if [[ "$line" =~ ^\|(.+)\|$ ]]; then
            local vars="${BASH_REMATCH[1]}"
            # Trim leading/trailing spaces from vars
            vars="${vars#"${vars%%[![:space:]]*}"}"
            vars="${vars%"${vars##*[![:space:]]}"}"
            result+="local $vars"$'\n'
            continue
        fi

        # Handle return: ^ expr
        if [[ "$line" =~ ^\^[[:space:]]*(.+)$ ]]; then
            local retval="${BASH_REMATCH[1]}"
            # Replace self with $_RECEIVER
            retval="${retval//self/\$_RECEIVER}"
            result+="echo $retval"$'\n'
            continue
        fi

        # Handle @ self message: pattern
        # @ self selector: arg  →  @ "$_RECEIVER" selector "arg"
        if [[ "$line" =~ ^@[[:space:]]+self[[:space:]]+(.+)$ ]]; then
            local msg="${BASH_REMATCH[1]}"
            # Parse keyword message: selector: arg
            if [[ "$msg" =~ ^([a-zA-Z]+):[[:space:]]*(.+)$ ]]; then
                local selector="${BASH_REMATCH[1]}"
                local arg="${BASH_REMATCH[2]}"
                result+="@ \"\$_RECEIVER\" $selector \"$arg\""$'\n'
            else
                # Unary message
                result+="@ \"\$_RECEIVER\" $msg"$'\n'
            fi
            continue
        fi

        # Handle assignment: var := expr
        if [[ "$line" =~ ^([a-zA-Z_][a-zA-Z0-9_]*)[[:space:]]*:=[[:space:]]*(.+)$ ]]; then
            local var="${BASH_REMATCH[1]}"
            local expr="${BASH_REMATCH[2]}"
            # Replace self with $_RECEIVER in expression
            expr="${expr//self/\$_RECEIVER}"
            result+="$var=$expr"$'\n'
            continue
        fi

        # Handle standalone function calls: _func args
        if [[ "$line" =~ ^_[a-zA-Z_]+[[:space:]]*.* ]]; then
            # Replace self with $_RECEIVER
            line="${line//self/\$_RECEIVER}"
            result+="$line"$'\n'
            continue
        fi

        # Default: pass through with self replacement
        line="${line//self/\$_RECEIVER}"
        result+="$line"$'\n'
    done <<< "$body"

    echo "$result"
}

# ============================================
# Main Compiler Entry Point
# ============================================

compile_file() {
    local source_file="$1"
    local output_file="$2"

    if [[ ! -f "$source_file" ]]; then
        echo "Error: Source file not found: $source_file" >&2
        return 1
    fi

    # Read source
    local source
    source=$(cat "$source_file")

    # Tokenize
    tokenize "$source"

    # Parse
    parse_class

    # Generate
    local output
    output=$(generate_bash)

    # Write output
    if [[ -n "$output_file" ]]; then
        echo "$output" > "$output_file"
        echo "Compiled: $source_file -> $output_file"
    else
        echo "$output"
    fi
}

compile_all() {
    local source_dir="${1:-$TRASHDIR}"

    for source_file in "$source_dir"/*.trash; do
        if [[ -f "$source_file" ]]; then
            local basename=$(basename "$source_file" .trash)
            local output_file="$COMPILED_DIR/$basename"
            compile_file "$source_file" "$output_file"
        fi
    done
}

# CLI interface
case "${1:-}" in
    "compile")
        shift
        compile_file "$@"
        ;;
    "compile-all")
        shift
        compile_all "$@"
        ;;
    "test")
        # Run compiler tests
        echo "Running compiler tests..."
        ;;
    *)
        echo "Trashtalk Compiler"
        echo "Usage:"
        echo "  $0 compile <source.trash> [output]"
        echo "  $0 compile-all [source_dir]"
        ;;
esac
