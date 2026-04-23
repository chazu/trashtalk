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
  local compiled_dir="${TRASHTALK_DIR:-${TRASHDIR:-.}}/trash/.compiled"

  if [[ -d "$compiled_dir" ]]; then
    while IFS= read -r -d '' file; do
      local name
      name=$(basename "$file")
      name="${name//__/::}"
      [[ -z "$cur" || "$name" == "$cur"* ]] && echo "$name"
    done < <(find "$compiled_dir" -maxdepth 1 -type f -print0 2>/dev/null)
  fi

  # Shell variables that look like object IDs
  while IFS= read -r var; do
    local val="${!var}"
    if [[ "$val" =~ ^[a-z]+_[A-F0-9-]+$ ]]; then
      local candidate="\$$var"
      [[ -z "$cur" || "$candidate" == "$cur"* ]] && echo "$candidate"
    fi
  done < <(compgen -v)
}

_trash_methods_list() {
  local receiver="$1"
  local cur="${2:-}"
  local class_name=""

  if [[ "$receiver" =~ ^[A-Z] ]]; then
    class_name="$receiver"
  elif [[ "$receiver" =~ ^\$ ]]; then
    local varname="${receiver#\$}"
    local instance_id="${!varname}"
    [[ -n "$instance_id" ]] && class_name=$(_class_from_instance_id "$instance_id" 2>/dev/null)
  else
    class_name=$(_class_from_instance_id "$receiver" 2>/dev/null)
  fi

  [[ -z "$class_name" ]] && return

  local compiled_name="${class_name//::/__}"
  local compiled_dir="${TRASHTALK_DIR:-${TRASHDIR:-.}}/trash/.compiled"
  local compiled_file="$compiled_dir/$compiled_name"

  [[ ! -f "$compiled_file" ]] && return

  source "$compiled_file" 2>/dev/null

  local is_class=0
  [[ "$receiver" =~ ^[A-Z] ]] && is_class=1

  _trash_methods_from_compiled "$compiled_name" "$is_class"

  # Trait methods
  local traits_var="__${compiled_name}__traits"
  local traits="${!traits_var:-}"
  for trait in $traits; do
    local trait_file="$compiled_dir/traits/$trait"
    if [[ -f "$trait_file" ]]; then
      source "$trait_file" 2>/dev/null
      _trash_methods_from_compiled "$trait" "$is_class"
    fi
  done

  # Superclass chain
  local super_var="__${compiled_name}__superclass"
  local superclass="${!super_var:-}"
  if [[ -n "$superclass" && "$superclass" != "nil" ]]; then
    _trash_methods_recursive "$superclass" "$is_class" "$compiled_dir"
  fi
}

_trash_methods_from_compiled() {
  local compiled_name="$1"
  local is_class="$2"

  declare -F | awk '{print $3}' | grep "^__${compiled_name}__" | grep -v "__class__" | while read -r func; do
    local method="${func#__${compiled_name}__}"
    echo "${method//_/:}"
  done

  if [[ "$is_class" -eq 1 ]]; then
    declare -F | awk '{print $3}' | grep "^__${compiled_name}__class__" | while read -r func; do
      local method="${func#__${compiled_name}__class__}"
      echo "${method//_/:}"
    done
  fi
}

_trash_methods_recursive() {
  local class_name="$1"
  local is_class="$2"
  local compiled_dir="$3"
  local compiled_name="${class_name//::/__}"
  local compiled_file="$compiled_dir/$compiled_name"

  [[ ! -f "$compiled_file" ]] && return

  source "$compiled_file" 2>/dev/null
  _trash_methods_from_compiled "$compiled_name" "$is_class"

  local super_var="__${compiled_name}__superclass"
  local superclass="${!super_var:-}"
  if [[ -n "$superclass" && "$superclass" != "nil" ]]; then
    _trash_methods_recursive "$superclass" "$is_class" "$compiled_dir"
  fi
}

_class_from_instance_id() {
  local instance_id="$1"
  local data
  data=$(_env_get "$instance_id" 2>/dev/null)
  if [[ -n "$data" ]]; then
    echo "$data" | jq -r '.class // empty' 2>/dev/null
    return
  fi
  local prefix="${instance_id%%_[A-F0-9-]*}"
  echo "${prefix^}"
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
    mapfile -t candidates < <(_trash_methods_list "$receiver" | grep "^${cur}" | sort -u)
  fi

  local n=${#candidates[@]}
  [[ $n -eq 0 ]] && return

  local prefix="${before%"$cur"}"

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
