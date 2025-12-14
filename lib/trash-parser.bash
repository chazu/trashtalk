#!/bin/bash

# Objective-C style message parser for trash system
# Converts [Object method:arg1 with:arg2] to send Object method_with arg1 arg2
# Supports nested messages and @"string" literals

# Parse a message, handling nested brackets
parse_message() {
    local input="$1"

    # Handle nested messages first
    input=$(resolve_nested_messages "$input")

    # Remove outer brackets
    local content=$(echo "$input" | sed 's/^\[//; s/\]$//')

    # Split into receiver and message parts
    local receiver=$(echo "$content" | awk '{print $1}')
    local message_part=$(echo "$content" | cut -d' ' -f2-)

    # Handle different message types
    if [[ "$message_part" =~ : ]]; then
        # Keyword message: method:arg1 with:arg2
        parse_keyword_message "$receiver" "$message_part"
    elif [[ -n "$message_part" ]]; then
        # Unary message: method
        echo "send $receiver $message_part"
    else
        # Just receiver, return it
        echo "send Object identity $receiver"
    fi
}

# Resolve nested messages by evaluating inner brackets first
resolve_nested_messages() {
    local input="$1"
    local temp_file="/tmp/parse_$$"

    # Find innermost brackets and replace with temp variables
    local counter=0
    while [[ "$input" =~ \[[^\[\]]*\] ]]; do
        local inner_match="${BASH_REMATCH[0]}"
        local temp_var="__NESTED_${counter}__"

        # Parse the inner message and store result
        local inner_result=$(parse_message "$inner_match")
        echo "$temp_var=\"\$($inner_result)\"" >> "$temp_file"

        # Replace in input
        input="${input/$inner_match/$temp_var}"
        ((counter++))
    done

    # If we have nested results, we need to evaluate them
    if [[ -f "$temp_file" ]]; then
        # Source the temp variables and substitute back
        source "$temp_file"
        rm -f "$temp_file"

        # Substitute temp variables back
        for ((i=0; i<counter; i++)); do
            local temp_var="__NESTED_${i}__"
            local temp_val=$(eval echo \$$temp_var)
            input="${input/$temp_var/$temp_val}"
        done
    fi

    echo "$input"
}

# Parse keyword messages like method:arg1 with:arg2
parse_keyword_message() {
    local receiver="$1"
    local message="$2"

    # Handle @"string" literals first
    message=$(resolve_string_literals "$message")

    # Extract method name by replacing : with _
    local method_name=$(echo "$message" | sed 's/:[^:]*//g' | tr ' ' '_')

    # Extract arguments using a more robust parser
    local args=""
    local tokens=($message)  # Split into array
    local i=0

    while [[ $i -lt ${#tokens[@]} ]]; do
        local token="${tokens[$i]}"

        if [[ "$token" =~ :$ ]]; then
            # This is a keyword, next token is the argument
            ((i++))
            if [[ $i -lt ${#tokens[@]} ]]; then
                local arg="${tokens[$i]}"
                arg=$(resolve_argument "$arg")
                args="$args $arg"
            fi
        fi
        ((i++))
    done

    echo "send $receiver $method_name$args"
}

# Resolve @"string" literals to proper bash strings
resolve_string_literals() {
    local input="$1"

    # Replace @"..." with "..."
    # This is a simple approach - could be more sophisticated
    input=$(echo "$input" | sed 's/@"\([^"]*\)"/"\1"/g')

    echo "$input"
}

# Resolve an argument (variable lookup, string literal, or raw value)
resolve_argument() {
    local arg="$1"

    # Handle quoted strings
    if [[ "$arg" =~ ^\".*\"$ ]]; then
        echo "$arg"  # Keep as quoted string
    # Handle variables (start with letter or underscore)
    elif [[ "$arg" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ ]]; then
        # Check if it's a defined variable
        if [[ -n "${!arg:-}" ]]; then
            echo "\$$arg"  # Variable reference
        else
            echo "$arg"    # Treat as literal string
        fi
    # Everything else (including numbers) as literal
    else
        echo "$arg"
    fi
}

# Enhanced @ function that supports bracket syntax
@() {
    local input="$*"
    
    # Check if input starts with [
    if [[ "$input" =~ ^\[ ]]; then
        # Parse the bracketed expression
        local send_command=$(parse_message "$input")
        msg_debug "Parsed: $send_command"
        eval "$send_command"
    else
        # Fall back to original @ behavior
        if [ $# == 1 ]; then
            is_a Object
        fi
        msg_debug "Entrypoint: $*"
        send $*
    fi
}

# Test function
test_parser() {
    echo "=== Testing Enhanced Parser ==="

    # Test unary message
    echo "1. Unary message:"
    result=$(parse_message "[Object fooBar]")
    echo "   Input: [Object fooBar]"
    echo "   Output: $result"

    # Test keyword message
    echo -e "\n2. Keyword message:"
    result=$(parse_message "[httpService atUrl:https://example.com]")
    echo "   Input: [httpService atUrl:https://example.com]"
    echo "   Output: $result"

    # Test multiple keywords
    echo -e "\n3. Multiple keywords:"
    result=$(parse_message "[obj method:arg1 with:arg2]")
    echo "   Input: [obj method:arg1 with:arg2]"
    echo "   Output: $result"

    # Test string literals
    echo -e "\n4. String literals:"
    result=$(parse_message "[obj say:@\"hello world\"]")
    echo "   Input: [obj say:@\"hello world\"]"
    echo "   Output: $result"

    # Test numbers vs variables
    echo -e "\n5. Numbers and variables:"
    local myVar="test_value"
    result=$(parse_message "[array at:1]")
    echo "   Input: [array at:1]"
    echo "   Output: $result"

    result=$(parse_message "[obj set:myVar]")
    echo "   Input: [obj set:myVar] (myVar=$myVar)"
    echo "   Output: $result"

    # Test nested messages (simple case)
    echo -e "\n6. Nested messages:"
    result=$(parse_message "[array at:[counter getValue]]")
    echo "   Input: [array at:[counter getValue]]"
    echo "   Output: $result"

    echo -e "\n=== Parser Tests Complete ==="
}

# Run tests if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    test_parser
fi
