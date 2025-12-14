#\!/usr/bin/env bash
# BSFL with Quiet Mode Support and Automatic LLM Detection
# This script loads the original BSFL and adds quiet mode functionality

# Load the original BSFL
source ~/bash/bsfl.sh

# Check if quiet mode should be enabled
QUIET_MODE_ENABLED=false

# Method 1: Check BASH_QUIET environment variable (manual override)
if [[ "$BASH_QUIET" == "true" || "$BASH_QUIET" == "1" || "$BASH_QUIET" == "yes" || "$BASH_QUIET" == "y" ]]; then
    QUIET_MODE_ENABLED=true
fi

# Method 2: Check for CI/automation environment variables
if [[ -n "$CI" || -n "$GITHUB_ACTIONS" || -n "$JENKINS_URL" || -n "$BUILDKITE" ]]; then
    QUIET_MODE_ENABLED=true
fi

# Method 3: Detect LLM/AI coding assistants by environment variables
if [[ "$TERM_PROGRAM" == "vscode" ]]; then
    # VS Code terminal
    QUIET_MODE_ENABLED=true
elif [[ -n "$VSCODE_PID" || -n "$VSCODE_IPC_HOOK" ]]; then
    # VS Code environment variables
    QUIET_MODE_ENABLED=true
elif [[ -n "$CURSOR_TRACE_ID" ]]; then
    # Cursor IDE
    QUIET_MODE_ENABLED=true
elif [[ -n "$ANTHROPIC_API_KEY" || -n "$OPENAI_API_KEY" || -n "$CLAUDE_API_KEY" ]]; then
    # AI API keys present (likely AI tool environment)
    QUIET_MODE_ENABLED=true
fi

# Method 4: Detect LLM tools by parent process analysis
if [[ "$QUIET_MODE_ENABLED" != "true" ]]; then
    # Get parent process info
    if command -v ps >/dev/null 2>&1 && [[ -n "$PPID" ]]; then
        PARENT_PROCESS=$(ps -p $PPID -o comm= 2>/dev/null | tr -d ' ')
        PARENT_ARGS=$(ps -p $PPID -o args= 2>/dev/null)
        
        # Check if parent process is a known LLM tool
        case "$PARENT_PROCESS" in
            node)
                # Check if it's a known AI tool running on Node.js
                if echo "$PARENT_ARGS" | grep -qE "(auggie|cursor|claude|copilot|codeium|tabnine)"; then
                    QUIET_MODE_ENABLED=true
                fi
                ;;
            python|python3)
                # Check if it's a Python-based AI tool
                if echo "$PARENT_ARGS" | grep -qE "(claude|openai|anthropic|copilot|codeium|aider)"; then
                    QUIET_MODE_ENABLED=true
                fi
                ;;
            code|cursor|augment)
                # Direct execution by known AI tools
                QUIET_MODE_ENABLED=true
                ;;
        esac
        
        # Also check grandparent process if parent is a shell
        if [[ "$PARENT_PROCESS" =~ ^(bash|zsh|sh|fish)$ ]] && [[ -n "$PPID" ]]; then
            GRANDPARENT_PID=$(ps -p $PPID -o ppid= 2>/dev/null | tr -d ' ')
            if [[ -n "$GRANDPARENT_PID" ]]; then
                GRANDPARENT_PROCESS=$(ps -p $GRANDPARENT_PID -o comm= 2>/dev/null | tr -d ' ')
                GRANDPARENT_ARGS=$(ps -p $GRANDPARENT_PID -o args= 2>/dev/null)
                
                case "$GRANDPARENT_PROCESS" in
                    node)
                        if echo "$GRANDPARENT_ARGS" | grep -qE "(auggie|cursor|claude|copilot|codeium|tabnine)"; then
                            QUIET_MODE_ENABLED=true
                        fi
                        ;;
                    python|python3)
                        if echo "$GRANDPARENT_ARGS" | grep -qE "(claude|openai|anthropic|copilot|codeium|aider)"; then
                            QUIET_MODE_ENABLED=true
                        fi
                        ;;
                    code|cursor|augment)
                        QUIET_MODE_ENABLED=true
                        ;;
                esac
            fi
        fi
    fi
fi

# Method 5: Check for common LLM tool process names in the system
if [[ "$QUIET_MODE_ENABLED" != "true" ]]; then
    if command -v pgrep >/dev/null 2>&1; then
        # Check if any known LLM tools are running
        if pgrep -f "(auggie|cursor|claude|copilot|codeium|tabnine|aider)" >/dev/null 2>&1; then
            # Additional check: see if our shell is a descendant of these processes
            # This is more complex but helps avoid false positives
            if ps -eo pid,ppid | awk -v current=$$ '
                BEGIN { found = 0 }
                { parent[$1] = $2 }
                END {
                    pid = current
                    while (pid > 1 && pid in parent) {
                        pid = parent[pid]
                        if (pid == 1) break
                    }
                    # If we traced back to init without finding an LLM tool, we might still be in one
                    # This is a simplified check - in practice, you might want more sophisticated logic
                }
            '; then
                # For now, we'll be conservative and not enable quiet mode based on this alone
                # unless we have other indicators
                :
            fi
        fi
    fi
fi

# Debug output (only if DEBUG_QUIET_MODE is set)
if [[ "$DEBUG_QUIET_MODE" == "true" ]]; then
    echo "DEBUG: QUIET_MODE_ENABLED=$QUIET_MODE_ENABLED" >&2
    echo "DEBUG: TERM_PROGRAM=$TERM_PROGRAM" >&2
    echo "DEBUG: PARENT_PROCESS=$PARENT_PROCESS" >&2
    echo "DEBUG: PARENT_ARGS=$PARENT_ARGS" >&2
fi

# If quiet mode is enabled, override the message functions
if [[ "$QUIET_MODE_ENABLED" == "true" ]]; then
    # Override all message functions to be silent
    msg_info() { :; }
    msg_alert() { :; }
    msg_warning() { :; }
    msg_error() { :; }
    msg_debug() { :; }
    msg_notice() { :; }
    msg_ok() { :; }
    msg_not_ok() { :; }
    msg_failed() { :; }
    msg_success() { :; }
    msg_passed() { :; }
    msg_critical() { :; }
    msg_emergency() { :; }
    
    # Also override msg_status directly
    msg_status() { :; }
    
    # Export a variable so other scripts can detect quiet mode
    export BSFL_QUIET_MODE="true"
fi
