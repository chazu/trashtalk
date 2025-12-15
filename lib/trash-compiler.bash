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
declare -a TOKEN_POSITIONS  # Track character position of each token
declare -a TOKEN_LINES      # Track line number for each token
declare -i TOKEN_POS=0
declare SOURCE_TEXT=""  # Original source for raw extraction
declare SOURCE_FILE=""  # Source file name for error messages

tokenize() {
    local input="$1"
    TOKENS=()
    TOKEN_POSITIONS=()
    TOKEN_LINES=()
    TOKEN_POS=0
    SOURCE_TEXT="$input"  # Store for raw extraction

    local i=0
    local len=${#input}
    local token=""
    local token_start=0
    local current_line=1  # Track current line number

    while ((i < len)); do
        local char="${input:i:1}"
        local next_char="${input:i+1:1}"

        case "$char" in
            # Whitespace - emit token if any, skip
            $' '|$'\t')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                ((i++))
                token_start=$i
                ;;
            # Newline - emit token and newline marker
            $'\n')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                TOKENS+=("NEWLINE")
                TOKEN_POSITIONS+=("$i")
                TOKEN_LINES+=("$current_line")
                ((current_line++))
                ((i++))
                token_start=$i
                ;;
            # Comment - skip to end of line
            '#')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                while ((i < len)) && [[ "${input:i:1}" != $'\n' ]]; do
                    ((i++))
                done
                token_start=$i
                ;;
            # Special characters
            '[')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                TOKENS+=("LBRACKET")
                TOKEN_POSITIONS+=("$i")
                TOKEN_LINES+=("$current_line")
                ((i++))
                token_start=$i
                ;;
            ']')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                TOKENS+=("RBRACKET")
                TOKEN_POSITIONS+=("$i")
                TOKEN_LINES+=("$current_line")
                ((i++))
                token_start=$i
                ;;
            '|')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                TOKENS+=("PIPE")
                TOKEN_POSITIONS+=("$i")
                TOKEN_LINES+=("$current_line")
                ((i++))
                token_start=$i
                ;;
            '^')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                TOKENS+=("CARET")
                TOKEN_POSITIONS+=("$i")
                TOKEN_LINES+=("$current_line")
                ((i++))
                token_start=$i
                ;;
            '.')
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                TOKENS+=("DOT")
                TOKEN_POSITIONS+=("$i")
                TOKEN_LINES+=("$current_line")
                ((i++))
                token_start=$i
                ;;
            ':')
                # Check for :=
                if [[ "$next_char" == "=" ]]; then
                    if [[ -n "$token" ]]; then
                        TOKENS+=("$token")
                        TOKEN_POSITIONS+=("$token_start")
                        TOKEN_LINES+=("$current_line")
                        token=""
                    fi
                    TOKENS+=("ASSIGN")
                    TOKEN_POSITIONS+=("$i")
                    TOKEN_LINES+=("$current_line")
                    ((i += 2))
                    token_start=$i
                else
                    # Colon is part of keyword - track start if new token
                    if [[ -z "$token" ]]; then
                        token_start=$i
                    fi
                    token+="$char"
                    ((i++))
                fi
                ;;
            # String literals
            "'")
                if [[ -n "$token" ]]; then
                    TOKENS+=("$token")
                    TOKEN_POSITIONS+=("$token_start")
                    TOKEN_LINES+=("$current_line")
                    token=""
                fi
                local str_start=$i
                ((i++))
                local str="'"
                while ((i < len)) && [[ "${input:i:1}" != "'" ]]; do
                    str+="${input:i:1}"
                    ((i++))
                done
                str+="'"
                ((i++))  # Skip closing quote
                TOKENS+=("STRING:$str")
                TOKEN_POSITIONS+=("$str_start")
                TOKEN_LINES+=("$current_line")
                token_start=$i
                ;;
            # Numbers
            [0-9]|'-')
                if [[ "$char" == "-" && ! "$next_char" =~ [0-9] ]]; then
                    if [[ -z "$token" ]]; then
                        token_start=$i
                    fi
                    token+="$char"
                    ((i++))
                else
                    if [[ -n "$token" ]]; then
                        TOKENS+=("$token")
                        TOKEN_POSITIONS+=("$token_start")
                        TOKEN_LINES+=("$current_line")
                        token=""
                    fi
                    local num_start=$i
                    local num="$char"
                    ((i++))
                    while ((i < len)) && [[ "${input:i:1}" =~ [0-9.] ]]; do
                        num+="${input:i:1}"
                        ((i++))
                    done
                    TOKENS+=("NUMBER:$num")
                    TOKEN_POSITIONS+=("$num_start")
                    TOKEN_LINES+=("$current_line")
                    token_start=$i
                fi
                ;;
            # Everything else - accumulate into token
            *)
                if [[ -z "$token" ]]; then
                    token_start=$i
                fi
                token+="$char"
                ((i++))
                ;;
        esac
    done

    # Emit final token
    if [[ -n "$token" ]]; then
        TOKENS+=("$token")
        TOKEN_POSITIONS+=("$token_start")
        TOKEN_LINES+=("$current_line")
    fi
}

# Get current token
current_token() {
    if ((TOKEN_POS < ${#TOKENS[@]})); then
        echo "${TOKENS[TOKEN_POS]}"
    fi
}

# Get current token's position in source
current_token_pos() {
    if ((TOKEN_POS < ${#TOKEN_POSITIONS[@]})); then
        echo "${TOKEN_POSITIONS[TOKEN_POS]}"
    fi
}

# Get current token's line number
current_token_line() {
    if ((TOKEN_POS < ${#TOKEN_LINES[@]})); then
        echo "${TOKEN_LINES[TOKEN_POS]}"
    else
        echo "?"
    fi
}

# Format error location
error_location() {
    local line=$(current_token_line)
    if [[ -n "$SOURCE_FILE" ]]; then
        echo "$SOURCE_FILE:$line"
    else
        echo "line $line"
    fi
}

# Report a parse error with line number
parse_error() {
    local message="$1"
    local location=$(error_location)
    echo "Parse error at $location: $message" >&2
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
        parse_error "expected '$expected', got '$actual'"
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

    # ClassName subclass: ParentClass  OR  ClassName trait
    local class_name=$(current_token)
    advance

    skip_newlines

    local next=$(current_token)
    local parent_class=""
    local is_trait=0

    if [[ "$next" == "subclass:" ]]; then
        advance
        skip_newlines
        parent_class=$(current_token)
        advance
    elif [[ "$next" == "trait" ]]; then
        advance
        is_trait=1
    else
        parse_error "expected 'subclass:' or 'trait', got '$next'"
        return 1
    fi

    CLASS_AST[name]="$class_name"
    CLASS_AST[parent]="$parent_class"
    CLASS_AST[instanceVars]=""
    CLASS_AST[traits]=""
    CLASS_AST[is_trait]="$is_trait"
    CLASS_AST[requires]=""

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
            "requires:")
                advance
                skip_newlines
                # Get the path (might be a STRING token)
                local req_path=$(current_token)
                if [[ "$req_path" == STRING:* ]]; then
                    req_path="${req_path#STRING:}"
                    req_path="${req_path//\'/}"  # Remove quotes
                fi
                advance
                if [[ -n "${CLASS_AST[requires]}" ]]; then
                    CLASS_AST[requires]+=$'\n'"$req_path"
                else
                    CLASS_AST[requires]="$req_path"
                fi
                ;;
            "method:")
                advance
                parse_method "instance" "normal"
                ;;
            "rawMethod:")
                advance
                parse_method "instance" "raw"
                ;;
            "classMethod:")
                advance
                parse_method "class" "normal"
                ;;
            "rawClassMethod:")
                advance
                parse_method "class" "raw"
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
    local method_mode="${2:-normal}"  # "normal" or "raw"

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

    # Expect method body in brackets - get position of opening bracket
    expect "LBRACKET"

    # Get the position right after the opening bracket
    local body_start=$(current_token_pos)

    # Skip through tokens to find matching closing bracket, tracking depth
    local bracket_depth=1
    while ((bracket_depth > 0)); do
        local token=$(current_token)
        case "$token" in
            "LBRACKET") ((bracket_depth++)) ;;
            "RBRACKET") ((bracket_depth--)) ;;
        esac
        if ((bracket_depth > 0)); then
            advance
        fi
    done

    # Get position of closing bracket
    local body_end=$(current_token_pos)
    advance  # Move past the closing bracket

    # Extract raw body from source text
    local body_length=$((body_end - body_start))
    local body="${SOURCE_TEXT:body_start:body_length}"

    # Trim leading/trailing whitespace from body while preserving internal structure
    # Remove leading newline if present
    body="${body#$'\n'}"
    # Remove trailing whitespace
    body="${body%"${body##*[![:space:]]}"}"

    # Store method info
    # Replace newlines with a placeholder since read stops at newlines
    local encoded_body="${body//$'\n'/__NL__}"
    local method_info="${method_name}"$'\x1f'"${method_args[*]}"$'\x1f'"${encoded_body}"$'\x1f'"${method_mode}"

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
    local is_trait="${CLASS_AST[is_trait]:-0}"
    local requires="${CLASS_AST[requires]}"

    # Header
    if [[ "$is_trait" == "1" ]]; then
        cat << EOF
#!/bin/bash
# Generated by Trashtalk Compiler - DO NOT EDIT
# Source: $class_name.trash (trait)
# Generated: $(date -Iseconds)

__${class_name}__is_trait="1"

EOF
    else
        cat << EOF
#!/bin/bash
# Generated by Trashtalk Compiler - DO NOT EDIT
# Source: $class_name.trash
# Generated: $(date -Iseconds)

__${class_name}__superclass="$parent"
__${class_name}__instanceVars="$instance_vars"
__${class_name}__traits="$traits"

EOF
    fi

    # Source required files
    if [[ -n "$requires" ]]; then
        echo "# Required dependencies"
        while IFS= read -r req_path; do
            [[ -n "$req_path" ]] && echo "source \"$req_path\""
        done <<< "$requires"
        echo ""
    fi

    # Generate instance methods
    for method_info in "${CLASS_METHODS[@]}"; do
        IFS=$'\x1f' read -r method_name method_args method_body method_mode <<< "$method_info"
        generate_method "$class_name" "$method_name" "$method_args" "$method_body" "instance" "${method_mode:-normal}"
    done

    # Generate class methods
    for method_info in "${CLASS_CLASS_METHODS[@]}"; do
        IFS=$'\x1f' read -r method_name method_args method_body method_mode <<< "$method_info"
        generate_method "$class_name" "$method_name" "$method_args" "$method_body" "class" "${method_mode:-normal}"
    done
}

generate_method() {
    local class_name="$1"
    local method_name="$2"
    local method_args="$3"
    local method_body="$4"
    local method_type="$5"
    local method_mode="${6:-normal}"

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

    # Transform method body to bash (skip for raw methods)
    local bash_body
    if [[ "$method_mode" == "raw" ]]; then
        # Raw method - pass through unchanged, NO indentation (heredocs are whitespace-sensitive)
        echo "$method_body"
    else
        # Normal method - apply DSL transformations
        bash_body=$(transform_body "$method_body")
        # Indent body
        echo "$bash_body" | sed 's/^/  /'
    fi

    echo "}"
    echo ""
}

# Transform keyword message send to positional call
# @ Receiver key1: arg1 key2: arg2 → @ Receiver key1_key2 arg1 arg2
transform_keyword_message() {
    local receiver="$1"
    local msg="$2"

    local method_name=""
    local args=""
    local remaining="$msg"

    # Parse keyword: arg pairs
    while [[ "$remaining" =~ ^([a-zA-Z_][a-zA-Z0-9_]*):[[:space:]]*([^[:space:]]+|\"[^\"]*\"|\'[^\']*\')[[:space:]]*(.*) ]]; do
        local keyword="${BASH_REMATCH[1]}"
        local arg="${BASH_REMATCH[2]}"
        remaining="${BASH_REMATCH[3]}"

        if [[ -n "$method_name" ]]; then
            method_name+="_$keyword"
        else
            method_name="$keyword"
        fi

        if [[ -n "$args" ]]; then
            args+=" $arg"
        else
            args="$arg"
        fi
    done

    # Handle any remaining non-keyword args
    if [[ -n "$remaining" ]]; then
        remaining="${remaining#"${remaining%%[![:space:]]*}"}"
        if [[ -n "$remaining" ]]; then
            args+=" $remaining"
        fi
    fi

    echo "@ $receiver $method_name $args"
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

        # Trim leading/trailing whitespace for pattern matching
        local trimmed="${line#"${line%%[![:space:]]*}"}"
        trimmed="${trimmed%"${trimmed##*[![:space:]]}"}"

        [[ -z "$trimmed" ]] && continue

        # Handle local variable declarations: | var1 var2 |
        if [[ "$trimmed" =~ ^\|(.+)\|$ ]]; then
            local vars="${BASH_REMATCH[1]}"
            # Trim leading/trailing spaces from vars
            vars="${vars#"${vars%%[![:space:]]*}"}"
            vars="${vars%"${vars##*[![:space:]]}"}"
            result+="local $vars"$'\n'
            continue
        fi

        # Handle return: ^ expr
        if [[ "$trimmed" =~ ^\^[[:space:]]*(.+)$ ]]; then
            local retval="${BASH_REMATCH[1]}"
            # Replace self with $_RECEIVER
            retval="${retval//self/\$_RECEIVER}"
            result+="echo $retval"$'\n'
            continue
        fi

        # Handle @ message sends (both self and other receivers)
        if [[ "$trimmed" =~ ^@[[:space:]]+([^[:space:]]+)[[:space:]]+(.+)$ ]]; then
            local receiver="${BASH_REMATCH[1]}"
            local msg="${BASH_REMATCH[2]}"

            # Replace self with $_RECEIVER
            if [[ "$receiver" == "self" ]]; then
                receiver='"$_RECEIVER"'
            fi

            # Check if it's a keyword message (contains word:)
            if [[ "$msg" =~ [a-zA-Z_][a-zA-Z0-9_]*: ]]; then
                local transformed
                transformed=$(transform_keyword_message "$receiver" "$msg")
                result+="$transformed"$'\n'
            else
                # Unary or positional message - pass through
                result+="@ $receiver $msg"$'\n'
            fi
            continue
        fi

        # Handle assignment: var := expr
        if [[ "$trimmed" =~ ^([a-zA-Z_][a-zA-Z0-9_]*)[[:space:]]*:=[[:space:]]*(.+)$ ]]; then
            local var="${BASH_REMATCH[1]}"
            local expr="${BASH_REMATCH[2]}"
            # Check if the expression contains a keyword message
            if [[ "$expr" =~ ^\$\(@[[:space:]]+([^[:space:]]+)[[:space:]]+(.+)\)$ ]]; then
                local inner_receiver="${BASH_REMATCH[1]}"
                local inner_msg="${BASH_REMATCH[2]}"
                if [[ "$inner_receiver" == "self" ]]; then
                    inner_receiver='"$_RECEIVER"'
                fi
                if [[ "$inner_msg" =~ [a-zA-Z_][a-zA-Z0-9_]*: ]]; then
                    local transformed
                    transformed=$(transform_keyword_message "$inner_receiver" "$inner_msg")
                    expr="\$($transformed)"
                fi
            fi
            # Replace self with $_RECEIVER in expression
            expr="${expr//self/\$_RECEIVER}"
            result+="$var=$expr"$'\n'
            continue
        fi

        # Default: pass through with self replacement (use trimmed for consistent indentation)
        trimmed="${trimmed//self/\$_RECEIVER}"
        result+="$trimmed"$'\n'
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

    # Set source file for error messages
    SOURCE_FILE="$source_file"

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
