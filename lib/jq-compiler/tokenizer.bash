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
#   - perl (splits the source into characters once; see tokenize)
# ==============================================================================

set -uo pipefail
# Note: We don't use -e because ((expr)) returns 1 when expr evaluates to 0,
# which would cause premature exit on things like ((col++)) when col=0

# ------------------------------------------------------------------------------
# Token accumulator
# ------------------------------------------------------------------------------
# Collect token fields in a Bash array and serialize the batch once. Bash
# source/token values cannot contain NUL, so NUL separates fields without
# confusing embedded newlines, quotes, tabs, or other control characters.

declare -a TOKENS=()

# Count of unrecoverable lexical errors (e.g. unterminated strings). When > 0
# the tokenizer exits non-zero so the driver reports a clear failure instead of
# silently feeding a malformed token stream to the parser.
_TOKENIZER_ERRORS=0

# Add a token to the accumulator
# Arguments: type value line col
add_token() {
    TOKENS+=("$1" "$2" "$3" "$4")
}

# Output all tokens as JSON array
emit_tokens() {
    if [[ ${#TOKENS[@]} -eq 0 ]]; then
        echo "[]"
    else
        printf '%s\0' "${TOKENS[@]}" | jq -Rsc '
          split("\u0000") | .[:-1] | . as $fields |
          [range(0; length; 4) as $i |
            {type:$fields[$i], value:$fields[$i+1],
             line:($fields[$i+2]|tonumber), col:($fields[$i+3]|tonumber)}]'
    fi
}

# ------------------------------------------------------------------------------
# Character access
# ------------------------------------------------------------------------------
# Bash resolves ${string:offset:1} by walking the whole string on every
# expansion, so scanning a source with substring expansions costs O(n^2): the
# 50 KB Trash.trash spent 20-29 s of CPU in that loop. The scan below indexes
# an array of characters instead, which is constant time per access. The split
# is one Perl pass that groups UTF-8 sequences (lead byte plus continuation
# bytes) and passes every byte through unchanged, so token values, character
# columns, and multibyte LITERAL tokens are exactly what the substring scan
# produced. The scan runs under LC_ALL=C so identifier, number, and symbol
# classes are ASCII in every environment; the serializer never depends on the
# locale.

# Split $1 into the global array `chars`, one character per element.
_split_chars() {
    mapfile -d '' -t chars < <(printf '%s' "$1" | perl -0777 -ne \
        'print "$_\0" for /[\x00-\x7F]|[\xC0-\xFF][\x80-\xBF]*|[\x80-\xBF]/g')
}

# Set `ahead` to at most $2 characters of `chars` starting at index $1.
_chars_ahead() {
    local IFS=
    ahead="${chars[*]:$1:$2}"
}

# ------------------------------------------------------------------------------
# Tokenizer
# ------------------------------------------------------------------------------

tokenize() {
    local input="$1"
    local LC_ALL=C
    local -a chars=()
    local len i=0 line=1 col=0 char next ahead
    if [[ -n "$input" ]]; then
        _split_chars "$input"
        if [[ ${#chars[@]} -eq 0 ]]; then
            echo "Tokenizer error: could not split the source into characters (perl is required)" >&2
            ((_TOKENIZER_ERRORS++)) || true
            return 1
        fi
    fi
    len=${#chars[@]}

    while ((i < len)); do
        char="${chars[i]}"
        next="${chars[i+1]-}"

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
                elif [[ "$next" == [a-zA-Z_] ]]; then
                    # Symbol: #symbolName
                    local sym_start_col=$col
                    local symbol=""
                    ((i++))  # skip #
                    ((col++))
                    while ((i < len)) && [[ "${chars[i]}" == [a-zA-Z0-9_] ]]; do
                        symbol+="${chars[i]}"
                        ((i++))
                        ((col++))
                    done
                    add_token "SYMBOL" "$symbol" "$line" "$sym_start_col"
                else
                    # Comment - capture as COMMENT token
                    local comment=""
                    local comment_start_col=$col
                    while ((i < len)) && [[ "${chars[i]}" != $'\n' ]]; do
                        comment+="${chars[i]}"
                        ((i++))
                        ((col++))
                    done
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
                    if [[ "${chars[i+2]-}" == ">" ]]; then
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
            # Less than - redirection, comparison, or heredoc
            # ------------------------------------------------------------------
            '<')
                if [[ "$next" == "<" ]]; then
                    # Check for here-string (<<<)
                    local next2="${chars[i+2]-}"
                    if [[ "$next2" == "<" ]]; then
                        add_token "HERESTRING" "<<<" "$line" "$col"
                        ((i += 3))
                        ((col += 3))
                    else
                        # Heredoc: capture delimiter and body as single token
                        local heredoc_start_col=$col
                        local heredoc_start_line=$line
                        ((i += 2))  # skip <<
                        ((col += 2))

                        # Skip optional - for <<- (strip leading tabs)
                        local strip_tabs=""
                        if [[ "${chars[i]-}" == "-" ]]; then
                            strip_tabs="-"
                            ((i++))
                            ((col++))
                        fi

                        # Skip whitespace before delimiter
                        while ((i < len)) && [[ "${chars[i]}" == " " || "${chars[i]}" == $'\t' ]]; do
                            ((i++))
                            ((col++))
                        done

                        # Extract delimiter (may be quoted or unquoted)
                        local delim=""
                        local delim_char="${chars[i]-}"
                        if [[ "$delim_char" == "'" || "$delim_char" == '"' ]]; then
                            # Quoted delimiter - find closing quote
                            local quote_char="$delim_char"
                            ((i++))
                            ((col++))
                            while ((i < len)) && [[ "${chars[i]}" != "$quote_char" ]]; do
                                delim+="${chars[i]}"
                                ((i++))
                                ((col++))
                            done
                            ((i++))  # skip closing quote
                            ((col++))
                        else
                            # Unquoted delimiter - read until whitespace/newline
                            while ((i < len)) && [[ "${chars[i]}" == [a-zA-Z0-9_] ]]; do
                                delim+="${chars[i]}"
                                ((i++))
                                ((col++))
                            done
                        fi

                        # Skip to end of line (heredoc body starts on next line)
                        while ((i < len)) && [[ "${chars[i]}" != $'\n' ]]; do
                            ((i++))
                            ((col++))
                        done

                        # Skip the newline
                        if ((i < len)) && [[ "${chars[i]}" == $'\n' ]]; then
                            ((i++))
                            ((line++))
                            col=0
                        fi

                        # Read heredoc body until we find delimiter on its own line
                        local body=""
                        while ((i < len)); do
                            # Check if this line is the delimiter
                            local current_line=""
                            while ((i < len)) && [[ "${chars[i]}" != $'\n' ]]; do
                                current_line+="${chars[i]}"
                                ((i++))
                                ((col++))
                            done

                            # Check if line matches delimiter (with optional leading whitespace for <<-)
                            local trimmed_line="$current_line"
                            if [[ -n "$strip_tabs" ]]; then
                                trimmed_line="${current_line#"${current_line%%[!$'\t']*}"}"
                            fi

                            if [[ "$trimmed_line" == "$delim" ]]; then
                                # Found end delimiter - don't include it in body
                                # Skip the newline after delimiter if present
                                if ((i < len)) && [[ "${chars[i]}" == $'\n' ]]; then
                                    ((i++))
                                    ((line++))
                                    col=0
                                fi
                                break
                            else
                                # Add line to body (including newline)
                                body+="$current_line"
                                if ((i < len)) && [[ "${chars[i]}" == $'\n' ]]; then
                                    body+=$'\n'
                                    ((i++))
                                    ((line++))
                                    col=0
                                fi
                            fi
                        done

                        # Emit heredoc as single HEREDOC_BLOCK token
                        # Value format: "DELIM:body" where body preserves newlines
                        add_token "HEREDOC_BLOCK" "<<${strip_tabs}${delim}"$'\n'"${body}${delim}" "$heredoc_start_line" "$heredoc_start_col"
                    fi
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
                elif [[ "$next" == [a-zA-Z_] ]]; then
                    # Block parameter like :x or :each
                    local param_col=$col
                    ((i++))  # skip the colon
                    ((col++))
                    # Consume the identifier
                    local param_name=""
                    while ((i < len)) && [[ "${chars[i]}" == [a-zA-Z0-9_] ]]; do
                        param_name+="${chars[i]}"
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
            # String literal (single-quoted or triple-quoted)
            # ------------------------------------------------------------------
            "'")
                local str_start_col=$col

                local str_start_line=$line

                # Check for triple-quoted string '''...'''
                if [[ "${chars[i]}${chars[i+1]-}${chars[i+2]-}" == "'''" ]]; then
                    local str=""
                    local found_close=0
                    ((i += 3))
                    ((col += 3))

                    # Consume until closing '''
                    while ((i < len)); do
                        if [[ "${chars[i]}${chars[i+1]-}${chars[i+2]-}" == "'''" ]]; then
                            # Found closing delimiter
                            ((i += 3))
                            ((col += 3))
                            found_close=1
                            break
                        fi
                        local c="${chars[i]}"
                        if [[ "$c" == $'\n' ]]; then
                            ((line++))
                            col=0
                        else
                            ((col++))
                        fi
                        str+="$c"
                        ((i++))
                    done

                    if [[ $found_close -eq 0 ]]; then
                        echo "Tokenizer error: unterminated triple-quoted string starting at line $str_start_line, col $str_start_col" >&2
                        ((_TOKENIZER_ERRORS++)) || true
                    fi

                    add_token "TRIPLESTRING" "$str" "$line" "$str_start_col"
                else
                    # Regular single-quoted string
                    local str="'"
                    ((i++))
                    ((col++))

                    # Consume until closing quote
                    while ((i < len)) && [[ "${chars[i]}" != "'" ]]; do
                        local c="${chars[i]}"
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
                    else
                        echo "Tokenizer error: unterminated string literal starting at line $str_start_line, col $str_start_col" >&2
                        ((_TOKENIZER_ERRORS++)) || true
                    fi

                    add_token "STRING" "$str" "$line" "$str_start_col"
                fi
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
                while ((i < len)) && [[ "${chars[i]}" == [0-9] ]]; do
                    num+="${chars[i]}"
                    ((i++))
                    ((col++))
                done

                # Check for decimal point followed by digit (true floating point)
                if ((i < len)) && [[ "${chars[i]}" == "." ]] && [[ "${chars[i+1]-}" == [0-9] ]]; then
                    num+="${chars[i]}"  # consume the dot
                    ((i++))
                    ((col++))
                    # Consume remaining digits
                    while ((i < len)) && [[ "${chars[i]}" == [0-9] ]]; do
                        num+="${chars[i]}"
                        ((i++))
                        ((col++))
                    done
                fi

                add_token "NUMBER" "$num" "$line" "$num_start_col"
                ;;

            '-')
                # Check if this is a negative number (minus followed by digit with no space)
                if [[ "$next" == [0-9] ]]; then
                    local num_start_col=$col
                    local num="-"
                    ((i++))
                    ((col++))

                    # Consume digits
                    while ((i < len)) && [[ "${chars[i]}" == [0-9] ]]; do
                        num+="${chars[i]}"
                        ((i++))
                        ((col++))
                    done

                    # Check for decimal point followed by digit (true floating point)
                    if ((i < len)) && [[ "${chars[i]}" == "." ]] && [[ "${chars[i+1]-}" == [0-9] ]]; then
                        num+="${chars[i]}"  # consume the dot
                        ((i++))
                        ((col++))
                        # Consume remaining digits
                        while ((i < len)) && [[ "${chars[i]}" == [0-9] ]]; do
                            num+="${chars[i]}"
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
                if [[ "${chars[i+1]-}${chars[i+2]-}" == "((" ]]; then
                    # Arithmetic $((...)) - must check before subshell
                    local arith="\$(("
                    ((i += 3))
                    ((col += 3))
                    local paren_depth=2
                    while ((i < len)) && ((paren_depth > 0)); do
                        local c="${chars[i]}"
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
                        local c="${chars[i]}"
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
                        local c="${chars[i]}"
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
                    while ((i < len)) && [[ "${chars[i]}" == [a-zA-Z0-9_] ]]; do
                        var+="${chars[i]}"
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
                    local arith_start_i=$i
                    ((i += 2))
                    ((col += 2))
                    local paren_depth=2
                    while ((i < len)) && ((paren_depth > 0)); do
                        local c="${chars[i]}"
                        arith+="$c"
                        if [[ "$c" == "(" ]]; then
                            ((paren_depth++))
                        elif [[ "$c" == ")" ]]; then
                            ((paren_depth--))
                        fi
                        ((i++))
                        ((col++))
                    done
                    # A Bash arithmetic command always closes with "))". Anything
                    # else, such as ((a isEmpty) or: [b]), is nested Smalltalk
                    # grouping: emit a single LPAREN and rescan from the next char.
                    if [[ "$arith" == *"))" ]]; then
                        add_token "ARITH_CMD" "$arith" "$line" "$arith_start_col"
                    else
                        i=$arith_start_i
                        col=$arith_start_col
                        add_token "LPAREN" "(" "$line" "$col"
                        ((i++))
                        ((col++))
                    fi
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
                # A Bash sequence is one shell word, not a brace-delimited
                # block. Splitting {1..50} inserts spaces during raw emission
                # and silently turns fifty retries into four literal words.
                # A sequence is a handful of characters; 256 bounds the lookahead.
                local sequence_pattern='^\{(-?[0-9]+\.\.-?[0-9]+|[a-zA-Z]\.\.[a-zA-Z])(\.\.-?[0-9]+)?\}'
                _chars_ahead "$i" 256
                if [[ "$ahead" =~ $sequence_pattern ]]; then
                    local sequence="${BASH_REMATCH[0]}"
                    add_token "BASH_SEQUENCE" "$sequence" "$line" "$col"
                    ((i += ${#sequence}))
                    ((col += ${#sequence}))
                else
                    add_token "LBRACE" "{" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
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
                    local c="${chars[i]}"
                    next="${chars[i+1]-}"

                    # Escaped quotes/dollars belong to this token; they must
                    # not close the string or open a command substitution.
                    if [[ "$c" == '\' && -n "$next" ]]; then
                        dstr+="$c$next"
                        ((i += 2))
                        ((col += 2))
                        continue
                    fi

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
                while ((i < len)) && [[ "${chars[i]}" == [a-zA-Z0-9_] ]]; do
                    word+="${chars[i]}"
                    ((i++))
                    ((col++))
                done

                # Check if followed by colon (making it a keyword)
                # But NOT := (assignment) or :: (namespace separator)
                if [[ "${chars[i]-}" == ":" && "${chars[i+1]-}" != "=" && "${chars[i+1]-}" != ":" ]]; then
                    word+=":"
                    ((i++))
                    ((col++))
                    # Check if immediately followed by a number (no whitespace) for varspec default
                    # e.g., value:42 should be a single token
                    # String defaults must use quotes: fieldC:'defaultValue'
                    if [[ "${chars[i]-}" == [0-9] ]]; then
                        # Consume the numeric default value
                        while ((i < len)) && [[ "${chars[i]}" == [0-9] ]]; do
                            word+="${chars[i]}"
                            ((i++))
                            ((col++))
                        done
                    fi
                    add_token "KEYWORD" "$word" "$line" "$word_start_col"
                else
                    # All non-keyword identifiers (including test predicates like isFile, isEmpty, etc.)
                    # Test predicates are handled contextually in the expression parser
                    add_token "IDENTIFIER" "$word" "$line" "$word_start_col"
                fi
                ;;

            # ------------------------------------------------------------------
            # Forward slash - file paths, division, regex
            # ------------------------------------------------------------------
            '/')
                # Check if this is an absolute path (e.g., /dev/null, /tmp/file)
                if [[ "$next" == [a-zA-Z0-9_] ]]; then
                    # Looks like an absolute path - consume until whitespace or special char
                    local path_start_col=$col
                    local path=""
                    while ((i < len)); do
                        local pc="${chars[i]}"
                        # Path can contain: alphanumeric, underscore, slash, dot, dash
                        if [[ "$pc" == [a-zA-Z0-9_/.-] ]]; then
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
            # Tilde - home directory or ~= string inequality
            # ------------------------------------------------------------------
            '~')
                if [[ "$next" == "=" ]]; then
                    add_token "STR_NE" "~=" "$line" "$col"
                    ((i += 2))
                    ((col += 2))
                else
                    add_token "TILDE" "~" "$line" "$col"
                    ((i++))
                    ((col++))
                fi
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
    # The loop's own status is incidental (a trailing ((col++)) from 0 is 1);
    # only the character split above can fail the scan.
    return 0
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main() {
    local input=""

    if ! command -v perl >/dev/null 2>&1; then
        echo "Tokenizer error: perl is required to split source into characters" >&2
        return 1
    fi

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

    tokenize "$input" || return 1
    emit_tokens || return

    # Fail if any unrecoverable lexical errors were seen, so the driver can
    # report a clear failure rather than compiling a malformed token stream.
    if (( _TOKENIZER_ERRORS > 0 )); then
        return 1
    fi
}

# Run if executed directly (not sourced)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
