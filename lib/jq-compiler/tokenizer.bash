#!/usr/bin/env bash
# ==============================================================================
# Trashtalk Tokenizer
# ==============================================================================
# Converts .trash source files into a JSON array of tokens.
#
# Token Types:
#   IDENTIFIER  - Variable/class names (e.g., Counter, myVar, _private)
#   KEYWORD     - Identifiers ending with colon (e.g., method:, subclass:)
#   STRING      - Single-quoted strings (e.g., 'hello')
#   NUMBER      - Numeric literals (e.g., 42, -1, 3.14)
#   LBRACKET    - Left bracket [
#   RBRACKET    - Right bracket ]
#   PIPE        - Pipe character |
#   CARET       - Caret ^ (return)
#   AT          - At sign @ (message send)
#   ASSIGN      - Assignment operator :=
#   DOT         - Period . (statement terminator, future use)
#   NEWLINE     - Line break (preserved for error reporting)
#
# Output Format (JSON array):
#   [{"type": "IDENTIFIER", "value": "Counter", "line": 1, "col": 0}, ...]
#
# Usage:
#   ./tokenizer.bash <file.trash>
#   cat file.trash | ./tokenizer.bash
#
# Dependencies:
#   - jq (JSON processor)
# ==============================================================================

set -uo pipefail
# Note: We don't use -e because ((expr)) returns 1 when expr evaluates to 0,
# which would cause premature exit on things like ((col++)) when col=0

# ------------------------------------------------------------------------------
# Token accumulator
# ------------------------------------------------------------------------------
# We collect tokens in a bash array, then output as JSON at the end.
# This is more efficient than calling jo for each token.

declare -a TOKENS=()

# Add a token to the accumulator
# Arguments: type value line col
add_token() {
    local type="$1"
    local value="$2"
    local line="$3"
    local col="$4"

    # Use jq to create properly escaped JSON
    # jq handles special characters correctly with --arg
    local token
    token=$(jq -nc --arg type "$type" --arg value "$value" \
                   --argjson line "$line" --argjson col "$col" \
                   '{type: $type, value: $value, line: $line, col: $col}')
    TOKENS+=("$token")
}

# Output all tokens as JSON array
emit_tokens() {
    if [[ ${#TOKENS[@]} -eq 0 ]]; then
        echo "[]"
    else
        # Join tokens with commas and wrap in array
        local IFS=','
        echo "[${TOKENS[*]}]"
    fi
}

# ------------------------------------------------------------------------------
# Tokenizer
# ------------------------------------------------------------------------------

tokenize() {
    local input="$1"
    local len=${#input}
    local i=0
    local line=1
    local col=0

    while ((i < len)); do
        local char="${input:i:1}"
        local next="${input:i+1:1}"

        case "$char" in
            # ------------------------------------------------------------------
            # Whitespace (space, tab) - skip but track column
            # ------------------------------------------------------------------
            ' '|$'\t')
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Newline - emit token and update position
            # ------------------------------------------------------------------
            $'\n')
                add_token "NEWLINE" '\n' "$line" "$col"
                ((i++))
                ((line++))
                col=0
                ;;

            # ------------------------------------------------------------------
            # Hash - could be comment, symbol, array literal, or dict literal
            # ------------------------------------------------------------------
            '#')
                if [[ "$next" == "(" ]]; then
                    # Array literal: #(...)
                    add_token "HASH_LPAREN" "#(" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" == "{" ]]; then
                    # Dictionary literal: #{...}
                    add_token "HASH_LBRACE" "#{" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" =~ [a-zA-Z_] ]]; then
                    # Symbol: #symbolName
                    local sym_start=$((i + 1))
                    ((i++))  # skip #
                    ((col++))
                    while ((i < len)) && [[ "${input:i:1}" =~ [a-zA-Z0-9_] ]]; do
                        ((i++))
                        ((col++))
                    done
                    local symbol="${input:$sym_start:$((i - sym_start))}"
                    add_token "SYMBOL" "$symbol" "$line" "$((col - ${#symbol} - 1))"
                else
                    # Comment - capture as COMMENT token
                    local comment_start=$i
                    local comment_start_col=$col
                    while ((i < len)) && [[ "${input:i:1}" != $'\n' ]]; do
                        ((i++))
                        ((col++))
                    done
                    local comment="${input:$comment_start:$((i - comment_start))}"
                    add_token "COMMENT" "$comment" "$line" "$comment_start_col"
                    # Don't consume the newline - let it be tokenized
                fi
                ;;

            # ------------------------------------------------------------------
            # Single-character tokens
            # ------------------------------------------------------------------
            '[')
                if [[ "$next" == "[" ]]; then
                    add_token "DLBRACKET" "[[" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                else
                    add_token "LBRACKET" "[" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            ']')
                if [[ "$next" == "]" ]]; then
                    add_token "DRBRACKET" "]]" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                else
                    add_token "RBRACKET" "]" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            '|')
                if [[ "$next" == "|" ]]; then
                    add_token "OR" "||" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                else
                    add_token "PIPE" "|" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            '^')
                add_token "CARET" "^" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            '@')
                add_token "AT" "@" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            '.')
                add_token "DOT" "." "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Semicolon - statement separator in bash
            # ------------------------------------------------------------------
            ';')
                add_token "SEMI" ";" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Ampersand - background or && operator
            # ------------------------------------------------------------------
            '&')
                if [[ "$next" == "&" ]]; then
                    add_token "AND" "&&" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" == ">" ]]; then
                    # &> or &>> redirection
                    if [[ "${input:i+2:1}" == ">" ]]; then
                        add_token "REDIRECT" "&>>" "$line" "$col"
                        ((i += 3))
                        ((col += 3))
                    else
                        add_token "REDIRECT" "&>" "$line" "$col"
                        ((i += 2))
                        ((col += 2))
                    fi
                else
                    add_token "AMP" "&" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            # ------------------------------------------------------------------
            # Greater than - redirection or comparison
            # ------------------------------------------------------------------
            '>')
                if [[ "$next" == ">" ]]; then
                    add_token "REDIRECT" ">>" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" == "&" ]]; then
                    # >&2 style redirection
                    add_token "REDIRECT" ">&" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" == "=" ]]; then
                    add_token "GE" ">=" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                else
                    add_token "GT" ">" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            # ------------------------------------------------------------------
            # Less than - redirection or comparison
            # ------------------------------------------------------------------
            '<')
                if [[ "$next" == "<" ]]; then
                    add_token "HEREDOC" "<<" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" == "=" ]]; then
                    add_token "LE" "<=" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                else
                    add_token "LT" "<" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            # ------------------------------------------------------------------
            # Equal sign - assignment or comparison
            # ------------------------------------------------------------------
            '=')
                if [[ "$next" == "~" ]]; then
                    add_token "MATCH" "=~" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" == "=" ]]; then
                    add_token "EQ" "==" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                else
                    add_token "EQUALS" "=" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            # ------------------------------------------------------------------
            # Exclamation - negation or != comparison
            # ------------------------------------------------------------------
            '!')
                if [[ "$next" == "=" ]]; then
                    add_token "NE" "!=" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                else
                    add_token "BANG" "!" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            # ------------------------------------------------------------------
            # Colon - could be := (assign), :: (namespace sep), block param (:x), or keyword
            # ------------------------------------------------------------------
            ':')
                if [[ "$next" == "=" ]]; then
                    add_token "ASSIGN" ":=" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" == ":" ]]; then
                    # Namespace separator ::
                    add_token "NAMESPACE_SEP" "::" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                elif [[ "$next" =~ ^[a-zA-Z_] ]]; then
                    # Block parameter like :x or :each
                    local param_start=$((i + 1))
                    local param_col=$col
                    ((i++))  # skip the colon
                    ((col++))
                    # Consume the identifier
                    local param_name=""
                    while ((i < len)) && [[ "${input:i:1}" =~ ^[a-zA-Z0-9_]$ ]]; do
                        param_name+="${input:i:1}"
                        ((i++))
                        ((col++))
                    done
                    add_token "BLOCK_PARAM" "$param_name" "$line" "$param_col"
                else
                    # Bare colon - this shouldn't happen in valid syntax
                    # but we'll emit it as an error token
                    add_token "ERROR" ":" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            # ------------------------------------------------------------------
            # String literal (single-quoted)
            # ------------------------------------------------------------------
            "'")
                local str_start_col=$col
                local str="'"
                ((i++))
                ((col++))

                # Consume until closing quote
                while ((i < len)) && [[ "${input:i:1}" != "'" ]]; do
                    local c="${input:i:1}"
                    if [[ "$c" == $'\n' ]]; then
                        # String spans multiple lines - track position
                        ((line++))
                        col=0
                    else
                        ((col++))
                    fi
                    str+="$c"
                    ((i++))
                done

                # Consume closing quote
                if ((i < len)); then
                    str+="'"
                    ((i++))
                    ((col++))
                fi

                add_token "STRING" "$str" "$line" "$str_start_col"
                ;;

            # ------------------------------------------------------------------
            # Number (including negative)
            # ------------------------------------------------------------------
            [0-9])
                local num_start_col=$col
                local num="$char"
                ((i++))
                ((col++))

                # Consume digits
                while ((i < len)) && [[ "${input:i:1}" =~ [0-9] ]]; do
                    num+="${input:i:1}"
                    ((i++))
                    ((col++))
                done

                # Check for decimal point followed by digit (true floating point)
                if ((i < len)) && [[ "${input:i:1}" == "." ]] && [[ "${input:$((i+1)):1}" =~ [0-9] ]]; then
                    num+="${input:i:1}"  # consume the dot
                    ((i++))
                    ((col++))
                    # Consume remaining digits
                    while ((i < len)) && [[ "${input:i:1}" =~ [0-9] ]]; do
                        num+="${input:i:1}"
                        ((i++))
                        ((col++))
                    done
                fi

                add_token "NUMBER" "$num" "$line" "$num_start_col"
                ;;

            '-')
                # Check if this is a negative number (minus followed by digit with no space)
                if [[ "$next" =~ [0-9] ]]; then
                    local num_start_col=$col
                    local num="-"
                    ((i++))
                    ((col++))

                    # Consume digits
                    while ((i < len)) && [[ "${input:i:1}" =~ [0-9] ]]; do
                        num+="${input:i:1}"
                        ((i++))
                        ((col++))
                    done

                    # Check for decimal point followed by digit (true floating point)
                    if ((i < len)) && [[ "${input:i:1}" == "." ]] && [[ "${input:$((i+1)):1}" =~ [0-9] ]]; then
                        num+="${input:i:1}"  # consume the dot
                        ((i++))
                        ((col++))
                        # Consume remaining digits
                        while ((i < len)) && [[ "${input:i:1}" =~ [0-9] ]]; do
                            num+="${input:i:1}"
                            ((i++))
                            ((col++))
                        done
                    fi

                    add_token "NUMBER" "$num" "$line" "$num_start_col"
                else
                    # Minus operator (subtraction or unary minus)
                    add_token "MINUS" "-" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            # ------------------------------------------------------------------
            # Subshell or variable: $(...), $((arithmetic)), ${...}, or $var
            # ------------------------------------------------------------------
            '$')
                local sub_start_col=$col
                if [[ "${input:i+1:2}" == "((" ]]; then
                    # Arithmetic $((...)) - must check before subshell
                    local arith="\$(("
                    ((i += 3))
                    ((col += 3))
                    local paren_depth=2
                    while ((i < len)) && ((paren_depth > 0)); do
                        local c="${input:i:1}"
                        arith+="$c"
                        if [[ "$c" == "(" ]]; then
                            ((paren_depth++))
                        elif [[ "$c" == ")" ]]; then
                            ((paren_depth--))
                        fi
                        ((i++))
                        ((col++))
                    done
                    add_token "ARITHMETIC" "$arith" "$line" "$sub_start_col"
                elif [[ "$next" == "(" ]]; then
                    # Subshell $(...) - capture the whole thing
                    local sub="\$("
                    ((i += 2))
                    ((col += 2))
                    local paren_depth=1
                    while ((i < len)) && ((paren_depth > 0)); do
                        local c="${input:i:1}"
                        sub+="$c"
                        if [[ "$c" == "(" ]]; then
                            ((paren_depth++))
                        elif [[ "$c" == ")" ]]; then
                            ((paren_depth--))
                        fi
                        ((i++))
                        ((col++))
                    done
                    add_token "SUBSHELL" "$sub" "$line" "$sub_start_col"
                elif [[ "$next" == "{" ]]; then
                    # Parameter expansion ${...} - capture the whole thing
                    local var="\${"
                    ((i += 2))
                    ((col += 2))
                    local brace_depth=1
                    while ((i < len)) && ((brace_depth > 0)); do
                        local c="${input:i:1}"
                        var+="$c"
                        if [[ "$c" == "{" ]]; then
                            ((brace_depth++))
                        elif [[ "$c" == "}" ]]; then
                            ((brace_depth--))
                        fi
                        ((i++))
                        ((col++))
                    done
                    add_token "VARIABLE" "$var" "$line" "$sub_start_col"
                elif [[ "$next" == '!' || "$next" == '?' || "$next" == '$' || "$next" == '@' || "$next" == '*' || "$next" == '#' || "$next" == '-' ]]; then
                    # Special variables: $!, $?, $$, $@, $*, $#, $-
                    local var="\$${next}"
                    ((i += 2))
                    ((col += 2))
                    add_token "VARIABLE" "$var" "$line" "$sub_start_col"
                else
                    # Simple variable like $var or $1
                    local var="\$"
                    ((i++))
                    ((col++))
                    # Consume variable name
                    while ((i < len)) && [[ "${input:i:1}" =~ [a-zA-Z0-9_] ]]; do
                        var+="${input:i:1}"
                        ((i++))
                        ((col++))
                    done
                    add_token "VARIABLE" "$var" "$line" "$sub_start_col"
                fi
                ;;

            # ------------------------------------------------------------------
            # Parentheses (for arithmetic, grouping)
            # ------------------------------------------------------------------
            '(')
                # Check for (( arithmetic )) - bash arithmetic command (no $ prefix)
                if [[ "$next" == "(" ]]; then
                    local arith="(("
                    local arith_start_col=$col
                    ((i += 2))
                    ((col += 2))
                    local paren_depth=2
                    while ((i < len)) && ((paren_depth > 0)); do
                        local c="${input:i:1}"
                        arith+="$c"
                        if [[ "$c" == "(" ]]; then
                            ((paren_depth++))
                        elif [[ "$c" == ")" ]]; then
                            ((paren_depth--))
                        fi
                        ((i++))
                        ((col++))
                    done
                    add_token "ARITH_CMD" "$arith" "$line" "$arith_start_col"
                else
                    add_token "LPAREN" "(" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            ')')
                add_token "RPAREN" ")" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Curly braces - for dictionary literals and bash blocks
            # ------------------------------------------------------------------
            '{')
                add_token "LBRACE" "{" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            '}')
                add_token "RBRACE" "}" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Double-quoted strings - handles nested quotes in subshells
            # ------------------------------------------------------------------
            '"')
                local dstr_start_col=$col
                local dstr='"'
                ((i++))
                ((col++))
                local subshell_depth=0
                while ((i < len)); do
                    local c="${input:i:1}"
                    local next="${input:$((i+1)):1}"

                    # Check for subshell start: $(
                    if [[ "$c" == '$' && "$next" == '(' ]]; then
                        dstr+='$('
                        ((i += 2))
                        ((col += 2))
                        ((subshell_depth++))
                        continue
                    fi

                    # Check for subshell end: )
                    if [[ "$c" == ')' && subshell_depth -gt 0 ]]; then
                        dstr+=')'
                        ((i++))
                        ((col++))
                        ((subshell_depth--))
                        continue
                    fi

                    # If not in a subshell and we hit a quote, end the string
                    if [[ "$c" == '"' && subshell_depth -eq 0 ]]; then
                        break
                    fi

                    dstr+="$c"
                    ((i++))
                    ((col++))
                done
                if ((i < len)); then
                    dstr+='"'
                    ((i++))
                    ((col++))
                fi
                add_token "DSTRING" "$dstr" "$line" "$dstr_start_col"
                ;;

            # ------------------------------------------------------------------
            # Identifier or Keyword
            # ------------------------------------------------------------------
            [a-zA-Z_])
                local word_start_col=$col
                local word="$char"
                ((i++))
                ((col++))

                # Consume identifier characters
                while ((i < len)) && [[ "${input:i:1}" =~ [a-zA-Z0-9_] ]]; do
                    word+="${input:i:1}"
                    ((i++))
                    ((col++))
                done

                # Check if followed by colon (making it a keyword)
                # But NOT := (assignment) or :: (namespace separator)
                if [[ "${input:i:1}" == ":" && "${input:i+1:1}" != "=" && "${input:i+1:1}" != ":" ]]; then
                    word+=":"
                    ((i++))
                    ((col++))
                    # Check if immediately followed by a number (no whitespace) for varspec default
                    # e.g., value:42 should be a single token
                    # String defaults must use quotes: fieldC:'defaultValue'
                    if [[ "${input:i:1}" =~ [0-9] ]]; then
                        # Consume the numeric default value
                        while ((i < len)) && [[ "${input:i:1}" =~ [0-9] ]]; do
                            word+="${input:i:1}"
                            ((i++))
                            ((col++))
                        done
                    fi
                    add_token "KEYWORD" "$word" "$line" "$word_start_col"
                else
                    add_token "IDENTIFIER" "$word" "$line" "$word_start_col"
                fi
                ;;

            # ------------------------------------------------------------------
            # Forward slash - file paths, division, regex
            # ------------------------------------------------------------------
            '/')
                # Check if this is an absolute path (e.g., /dev/null, /tmp/file)
                local next="${input:$((i+1)):1}"
                if [[ "$next" =~ [a-zA-Z0-9_] ]]; then
                    # Looks like an absolute path - consume until whitespace or special char
                    local path_start=$i
                    local path_start_col=$col
                    local path=""
                    while [[ $i -lt $len ]]; do
                        local pc="${input:$i:1}"
                        # Path can contain: alphanumeric, underscore, slash, dot, dash
                        if [[ "$pc" =~ [a-zA-Z0-9_/.\-] ]]; then
                            path+="$pc"
                            ((i++))
                            ((col++))
                        else
                            break
                        fi
                    done
                    add_token "PATH" "$path" "$line" "$path_start_col"
                else
                    add_token "SLASH" "/" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
                ;;

            # ------------------------------------------------------------------
            # Question mark - regex quantifier, ternary
            # ------------------------------------------------------------------
            '?')
                add_token "QUESTION" "?" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Plus sign - arithmetic, regex quantifier
            # ------------------------------------------------------------------
            '+')
                add_token "PLUS" "+" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Asterisk - glob, multiplication, regex
            # ------------------------------------------------------------------
            '*')
                add_token "STAR" "*" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Comma - argument separator
            # ------------------------------------------------------------------
            ',')
                add_token "COMMA" "," "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Tilde - home directory
            # ------------------------------------------------------------------
            '~')
                add_token "TILDE" "~" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Percent - modulo
            # ------------------------------------------------------------------
            '%')
                add_token "PERCENT" "%" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Backslash - escape character
            # ------------------------------------------------------------------
            '\\')
                add_token "BACKSLASH" "\\\\" "$line" "$col"
                ((i++))
                ((col++))
                ;;

            # ------------------------------------------------------------------
            # Unknown character - emit as literal to preserve it
            # ------------------------------------------------------------------
            *)
                # Emit unknown characters as-is to preserve them
                add_token "LITERAL" "$char" "$line" "$col"
                ((i++))
                ((col++))
                ;;
        esac
    done
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main() {
    local input=""

    if [[ $# -gt 0 && -f "$1" ]]; then
        # Read from file
        input=$(cat "$1")
    elif [[ ! -t 0 ]]; then
        # Read from stdin
        input=$(cat)
    else
        echo "Usage: $0 <file.trash>" >&2
        echo "   or: cat file.trash | $0" >&2
        exit 1
    fi

    tokenize "$input"
    emit_tokens
}

# Run if executed directly (not sourced)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
