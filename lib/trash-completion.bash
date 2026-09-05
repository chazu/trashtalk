#!/usr/bin/env bash
# Bash completion for the @ (send) function.
# Source this file or add to your .bashrc after sourcing trash.bash.
#
# Also used by trash-repl via _trash_repl_complete (bind -x).

# ---------------------------------------------------------------------------
# Helpers: write candidates to stdout (used by both completion paths)
# ---------------------------------------------------------------------------

_trash_receivers_list() {
  local cur="${1:-}"
  local name
  while IFS= read -r name; do
    name=${name//__/::}
    [[ -z "$cur" || "$name" == "$cur"* ]] && printf '%s\n' "$name"
  done < <(@ Trash listObjects)

  # Shell variables that look like object IDs
  while IFS= read -r var; do
    local val="${!var-}"
    if [[ "$val" =~ ^[a-z][a-z0-9_]*_[[:xdigit:]-]+$ ]]; then
      local candidate="\$$var"
      [[ -z "$cur" || "$candidate" == "$cur"* ]] && echo "$candidate"
    fi
  done < <(compgen -v)
}

# Ask the public compiler-backed browser for selectors; completion never
# sources class artifacts or guesses selectors by reversing Bash function names.
_trash_methods_list() {
  local receiver="$1" class_name varname
  if [[ "$receiver" == '$'* ]]; then
    varname=${receiver#\$}
    [[ "$varname" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ ]] || return
    receiver=${!varname-}
    [[ -n "$receiver" ]] || return
    class_name=$(@ Runtime classFor: "$receiver" 2>/dev/null)
  elif [[ "$receiver" =~ ^[A-Z] && "$receiver" != *_* ]]; then
    class_name=$receiver
  else
    class_name=$(@ Runtime classFor: "$receiver" 2>/dev/null)
    [[ -n "$class_name" ]] || class_name=$receiver
  fi
  local -A seen=()
  _trash_selectors_recursive "$class_name"
}

_trash_selectors_recursive() {
  local class_name="$1" records parent
  [[ -n "$class_name" && "$class_name" != nil && -z "${seen[$class_name]:-}" ]] || return 0
  seen[$class_name]=1
  records=$(@ Trash symbolRecordsForClass: "$class_name") || return
  printf '%s\n' "$records" | jq -r '
    if .kind == "instance_method" or .kind == "class_method" then .selector
    elif .kind == "instance_variable" or .kind == "class_variable" then
      .variable as $v | ($v, ($v + ":"), ("get" + ($v[0:1] | ascii_upcase) + $v[1:]),
        ("set" + ($v[0:1] | ascii_upcase) + $v[1:] + ":"))
    else empty end'
  while IFS= read -r parent; do
    _trash_selectors_recursive "$parent"
  done < <(printf '%s\n' "$records" | jq -r 'select(.kind == "class" or .kind == "trait") | (.superclass // ""), .traits[]?')
}

_common_prefix() {
  local prefix="$1"
  shift
  for word in "$@"; do
    while [[ "${word#"$prefix"}" == "$word" ]]; do
      prefix="${prefix%?}"
      [[ -z "$prefix" ]] && echo "" && return
    done
  done
  echo "$prefix"
}

# ---------------------------------------------------------------------------
# Bash completion path (@ in a normal shell)
# ---------------------------------------------------------------------------

_trash_complete() {
  local cur="${COMP_WORDS[COMP_CWORD]}"

  if [[ $COMP_CWORD -eq 1 ]]; then
    COMPREPLY=($(compgen -W "$(_trash_receivers_list)" -- "$cur"))
    return
  fi

  local receiver="${COMP_WORDS[1]}"
  COMPREPLY=($(compgen -W "$(_trash_methods_list "$receiver")" -- "$cur"))
}

complete -F _trash_complete @

# ---------------------------------------------------------------------------
# REPL completion path (bind -x in trash-repl)
# ---------------------------------------------------------------------------

_trash_repl_complete() {
  local line="$READLINE_LINE"
  local point="$READLINE_POINT"
  local before="${line:0:$point}"
  local leading=""
  if [[ "$before" == "@ "* ]]; then leading="@ "; before=${before#"@ "}; fi

  # Tokenise what's before the cursor
  local -a words
  read -ra words <<< "$before"
  local nwords=${#words[@]}

  # Are we starting a new word (cursor after a space)?
  local cur=""
  if [[ "${before: -1}" == " " || $nwords -eq 0 ]]; then
    cur=""
    nwords=$((nwords + 1))
  else
    cur="${words[$((nwords - 1))]}"
  fi

  local -a candidates
  if [[ $nwords -le 1 ]]; then
    mapfile -t candidates < <(_trash_receivers_list "$cur" | sort -u)
  else
    local receiver="${words[0]}"
    mapfile -t candidates < <(_trash_methods_list "$receiver" | while IFS= read -r method; do [[ "$method" == "$cur"* ]] && printf '%s\n' "$method"; done | sort -u)
  fi

  local n=${#candidates[@]}
  [[ $n -eq 0 ]] && return

  local prefix="${leading}${before%"$cur"}"

  if [[ $n -eq 1 ]]; then
    local new="${candidates[0]} "
    READLINE_LINE="${prefix}${new}${line:$point}"
    READLINE_POINT=$(( ${#prefix} + ${#new} ))
  else
    local common
    common=$(_common_prefix "${candidates[@]}")
    echo ""
    printf '%s\n' "${candidates[@]}" | column -c "${COLUMNS:-80}"
    if [[ ${#common} -gt ${#cur} ]]; then
      READLINE_LINE="${prefix}${common}${line:$point}"
      READLINE_POINT=$(( ${#prefix} + ${#common} ))
    fi
  fi
}
