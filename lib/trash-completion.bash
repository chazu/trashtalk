#!/usr/bin/env bash
# Bash completion for the @ (send) function
# Source this file or add to your .bashrc after sourcing trash.bash

_trash_complete() {
  local cur prev
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"

  # Position 1: completing receiver (class name or instance variable)
  if [[ $COMP_CWORD -eq 1 ]]; then
    _trash_complete_receivers
    return
  fi

  # Position 2+: completing method name
  if [[ $COMP_CWORD -ge 2 ]]; then
    local receiver="${COMP_WORDS[1]}"
    _trash_complete_methods "$receiver"
    return
  fi
}

_trash_complete_receivers() {
  local classes=()
  local compiled_dir="${TRASHDIR:-.}/.compiled"

  # Get class names from compiled directory
  if [[ -d "$compiled_dir" ]]; then
    while IFS= read -r -d '' file; do
      local name=$(basename "$file")
      # Convert namespaced names back to :: format
      name="${name//__/::}"
      classes+=("$name")
    done < <(find "$compiled_dir" -maxdepth 1 -type f -print0 2>/dev/null)
  fi

  # Also add any instance variables that look like object IDs
  # (This is a heuristic - variables containing _UUID patterns)
  while IFS= read -r var; do
    local val="${!var}"
    if [[ "$val" =~ ^[a-z]+_[A-F0-9-]+$ ]]; then
      classes+=("\$$var")
    fi
  done < <(compgen -v)

  COMPREPLY=($(compgen -W "${classes[*]}" -- "$cur"))
}

_trash_complete_methods() {
  local receiver="$1"
  local class_name=""
  local methods=()

  # Determine class name from receiver
  if [[ "$receiver" =~ ^[A-Z] ]]; then
    # It's a class name (possibly namespaced)
    class_name="$receiver"
  elif [[ "$receiver" =~ ^\$ ]]; then
    # It's a variable reference - try to get its value and extract class
    local varname="${receiver#\$}"
    local instance_id="${!varname}"
    if [[ -n "$instance_id" ]]; then
      # Extract class from instance ID (e.g., counter_UUID -> Counter)
      local prefix="${instance_id%%_[A-F0-9-]*}"
      # Handle namespaced: myapp_counter_UUID -> MyApp::Counter
      class_name=$(_class_from_instance_id "$instance_id" 2>/dev/null)
    fi
  else
    # Try treating it as a literal instance ID
    local prefix="${receiver%%_[A-F0-9-]*}"
    class_name=$(_class_from_instance_id "$receiver" 2>/dev/null)
  fi

  if [[ -z "$class_name" ]]; then
    return
  fi

  # Get methods for this class
  local compiled_name="${class_name//::/__}"
  local compiled_file="${TRASHDIR:-.}/.compiled/$compiled_name"

  if [[ -f "$compiled_file" ]]; then
    # Source the class to get function definitions
    source "$compiled_file" 2>/dev/null

    # Get instance methods
    while IFS= read -r func; do
      local method="${func#__${compiled_name}__}"
      # Skip class__ prefix methods for instance completion
      if [[ ! "$method" =~ ^class__ ]]; then
        # Convert method_name back to method: format for keyword methods
        method="${method//_/:}"
        methods+=("$method")
      fi
    done < <(declare -F | awk '{print $3}' | grep "^__${compiled_name}__" | grep -v "__class__")

    # Get class methods (if receiver is a class)
    if [[ "$receiver" =~ ^[A-Z] ]]; then
      while IFS= read -r func; do
        local method="${func#__${compiled_name}__class__}"
        method="${method//_/:}"
        methods+=("$method")
      done < <(declare -F | awk '{print $3}' | grep "^__${compiled_name}__class__")
    fi

    # Get trait methods
    local traits_var="__${compiled_name}__traits"
    local traits="${!traits_var:-}"
    for trait in $traits; do
      local trait_file="${TRASHDIR:-.}/.compiled/traits/$trait"
      if [[ -f "$trait_file" ]]; then
        source "$trait_file" 2>/dev/null
        while IFS= read -r func; do
          local method="${func#__${trait}__}"
          if [[ ! "$method" =~ ^class__ ]]; then
            method="${method//_/:}"
            methods+=("$method")
          fi
        done < <(declare -F | awk '{print $3}' | grep "^__${trait}__" | grep -v "__class__")

        # Trait class methods
        if [[ "$receiver" =~ ^[A-Z] ]]; then
          while IFS= read -r func; do
            local method="${func#__${trait}__class__}"
            method="${method//_/:}"
            methods+=("$method")
          done < <(declare -F | awk '{print $3}' | grep "^__${trait}__class__")
        fi
      fi
    done

    # Get superclass methods
    local super_var="__${compiled_name}__superclass"
    local superclass="${!super_var:-}"
    if [[ -n "$superclass" && "$superclass" != "nil" ]]; then
      local super_methods
      super_methods=$(_trash_get_methods_recursive "$superclass" "$receiver")
      methods+=($super_methods)
    fi
  fi

  # Remove duplicates and complete
  local unique_methods=($(printf '%s\n' "${methods[@]}" | sort -u))
  COMPREPLY=($(compgen -W "${unique_methods[*]}" -- "$cur"))
}

_trash_get_methods_recursive() {
  local class_name="$1"
  local receiver="$2"
  local compiled_name="${class_name//::/__}"
  local compiled_file="${TRASHDIR:-.}/.compiled/$compiled_name"

  if [[ ! -f "$compiled_file" ]]; then
    return
  fi

  source "$compiled_file" 2>/dev/null

  # Instance methods
  declare -F | awk '{print $3}' | grep "^__${compiled_name}__" | grep -v "__class__" | while read func; do
    local method="${func#__${compiled_name}__}"
    echo "${method//_/:}"
  done

  # Class methods if receiver is a class
  if [[ "$receiver" =~ ^[A-Z] ]]; then
    declare -F | awk '{print $3}' | grep "^__${compiled_name}__class__" | while read func; do
      local method="${func#__${compiled_name}__class__}"
      echo "${method//_/:}"
    done
  fi

  # Recurse to superclass
  local super_var="__${compiled_name}__superclass"
  local superclass="${!super_var:-}"
  if [[ -n "$superclass" && "$superclass" != "nil" ]]; then
    _trash_get_methods_recursive "$superclass" "$receiver"
  fi
}

_class_from_instance_id() {
  local instance_id="$1"
  # Try to get from environment first
  local data=$(_env_get "$instance_id" 2>/dev/null)
  if [[ -n "$data" ]]; then
    echo "$data" | jq -r '.class // empty' 2>/dev/null
    return
  fi
  # Fall back to inferring from prefix
  local prefix="${instance_id%%_[A-F0-9-]*}"
  # Capitalize first letter
  echo "${prefix^}"
}

# Register completion for @
complete -F _trash_complete @
