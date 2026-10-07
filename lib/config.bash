# User configuration: declared settings, preferences classes, the legacy
# TOML-subset file, and env overrides. Config.trash and Settings.trash expose
# these as primitives. Reads run on builtins only; see docs/settings-design.md
# for the resolution order and docs/config-design.md for the file format.

declare -ga _TRASH_CONFIG_KEYS=() _TRASH_SETTINGS_GROUPS=() _TRASH_PREFS_CLASSES=()
declare -gA _TRASH_CONFIG_ENV=() _TRASH_CONFIG_TYPE=() _TRASH_CONFIG_DEFAULT=() _TRASH_CONFIG_DOC=()
declare -gA _TRASH_CONFIG_GROUP=() _TRASH_SETTINGS_PREFIX=() _TRASH_SETTINGS_SOURCE=()
declare -gA _TRASH_PREFS_PARENT=() _TRASH_PREFS_HOST=() _TRASH_PREFS_SOURCE=() _TRASH_PREFS_HASH=()
declare -gA _TRASH_PREFS_VALUES=()
declare -gA _TRASH_CONFIG_FILE_VALUES=() _TRASH_CONFIG_FILE_LINE=()
declare -ga _TRASH_CONFIG_FILE_ORDER=() _TRASH_CONFIG_FILE_DUPES=() _TRASH_CONFIG_FILE_ERRORS=()
_TRASH_CONFIG_LOADED=''

# The build generates .settings.bash from every Settings group and Preferences
# class (see settings_table in lib/jq-compiler/protocols.jq). Its calls land here.
# Usage: _trash_config_declare key ENV_VAR type default description group
# type is string, integer (non-negative), boolean, or "enum:a b c".
_trash_config_declare() {
    _TRASH_CONFIG_KEYS+=("$1")
    _TRASH_CONFIG_ENV[$1]=$2 _TRASH_CONFIG_TYPE[$1]=$3
    _TRASH_CONFIG_DEFAULT[$1]=$4 _TRASH_CONFIG_DOC[$1]=$5 _TRASH_CONFIG_GROUP[$1]=${6:-}
}

# Usage: _trash_settings_group Group prefix source
_trash_settings_group() {
    _TRASH_SETTINGS_GROUPS+=("$1")
    _TRASH_SETTINGS_PREFIX[$1]=$2 _TRASH_SETTINGS_SOURCE[$1]=$3
}

# Usage: _trash_prefs_class Class parent host source source_hash
_trash_prefs_class() {
    _TRASH_PREFS_CLASSES+=("$1")
    _TRASH_PREFS_PARENT[$1]=$2 _TRASH_PREFS_HOST[$1]=$3
    _TRASH_PREFS_SOURCE[$1]=$4 _TRASH_PREFS_HASH[$1]=$5
}

# Usage: _trash_prefs_value Class key value
_trash_prefs_value() {
    _TRASH_PREFS_VALUES[$1|$2]=$3
}

_trash_config_table() {
    printf '%s\n' "${TRASHDIR:-$HOME/.trashtalk/trash}/.compiled/.settings.bash"
}

# Source the generated table once per process, and again after a build
# replaces it. Its first line carries a content hash, compared with a builtin
# read, so a current table costs no subprocess.
_trash_config_load() {
    local table header=''
    table=$(_trash_config_table)
    [[ ! -f $table ]] || IFS= read -r header < "$table" || true
    [[ -n $_TRASH_CONFIG_LOADED && $_TRASH_CONFIG_LOADED == "$table|$header" ]] && return 0
    _TRASH_CONFIG_KEYS=() _TRASH_SETTINGS_GROUPS=() _TRASH_PREFS_CLASSES=()
    _TRASH_CONFIG_ENV=() _TRASH_CONFIG_TYPE=() _TRASH_CONFIG_DEFAULT=() _TRASH_CONFIG_DOC=() _TRASH_CONFIG_GROUP=()
    _TRASH_SETTINGS_PREFIX=() _TRASH_SETTINGS_SOURCE=()
    _TRASH_PREFS_PARENT=() _TRASH_PREFS_HOST=() _TRASH_PREFS_SOURCE=() _TRASH_PREFS_HASH=() _TRASH_PREFS_VALUES=()
    if [[ -f $table ]]; then
        source "$table" || return 1
    fi
    _TRASH_CONFIG_LOADED="$table|$header"
}

# Force the next read to source the table, after this process rebuilt it.
_trash_config_unload() { _TRASH_CONFIG_LOADED=''; }

_TRASH_CONFIG_KEY_RE='^[[:space:]]*([A-Za-z][A-Za-z0-9_]*(\.[A-Za-z][A-Za-z0-9_]*)*)[[:space:]]*=[[:space:]]*(.*)$'
_TRASH_CONFIG_QUOTED_RE='^"([^"\\]*)"[[:space:]]*(#.*)?$'
_TRASH_CONFIG_BARE_RE='^(true|false|[+-]?[0-9]+)[[:space:]]*(#.*)?$'
_TRASH_CONFIG_BLANK_RE='^[[:space:]]*(#.*)?$'

# Sets _tc_file to the user file, or empty when user config is disabled. An
# explicit TRASHTALK_CONFIG is honored even when the default file is skipped.
_trash_config_file() {
    if [[ -n ${TRASHTALK_CONFIG:-} ]]; then
        _tc_file=$TRASHTALK_CONFIG
    elif [[ -n ${TRASHTALK_SKIP_USER_CONFIG:-} ]]; then
        _tc_file=''
    else
        _tc_file=${XDG_CONFIG_HOME:-$HOME/.config}/trashtalk/config
    fi
}

# Parse one line. Sets _tc_kind (blank, entry, error), and _tc_key,
# _tc_value, _tc_bare, _tc_comment for entries or _tc_error for errors.
_trash_config_parse_line() {
    local rest
    _tc_key='' _tc_value='' _tc_bare='' _tc_comment='' _tc_error=''
    if [[ $1 =~ $_TRASH_CONFIG_BLANK_RE ]]; then
        _tc_kind=blank; return
    fi
    _tc_kind=error
    if [[ $1 =~ ^[[:space:]]*\[ ]]; then
        _tc_error='table headers are not supported; write dotted keys such as jcode.model = "..."'
        return
    fi
    if [[ ! $1 =~ $_TRASH_CONFIG_KEY_RE ]]; then
        _tc_error='expected key = value'; return
    fi
    _tc_key=${BASH_REMATCH[1]} rest=${BASH_REMATCH[3]}
    if [[ $rest =~ $_TRASH_CONFIG_QUOTED_RE ]]; then
        _tc_value=${BASH_REMATCH[1]} _tc_comment=${BASH_REMATCH[2]}
    elif [[ $rest =~ $_TRASH_CONFIG_BARE_RE ]]; then
        _tc_value=${BASH_REMATCH[1]#+} _tc_comment=${BASH_REMATCH[2]} _tc_bare=1
    else
        _tc_error='value must be a "quoted string" without quotes or backslashes, an integer, or true/false'
        return
    fi
    _tc_kind=entry
}

# Read a config file into _TRASH_CONFIG_FILE_VALUES (last duplicate wins),
# _TRASH_CONFIG_FILE_LINE, _TRASH_CONFIG_FILE_ORDER, _TRASH_CONFIG_FILE_DUPES
# and _TRASH_CONFIG_FILE_ERRORS. Returns 1 when any line has a syntax error.
_trash_config_scan() {
    local line number=0
    declare -gA _TRASH_CONFIG_FILE_VALUES=() _TRASH_CONFIG_FILE_LINE=()
    declare -ga _TRASH_CONFIG_FILE_ORDER=() _TRASH_CONFIG_FILE_DUPES=() _TRASH_CONFIG_FILE_ERRORS=()
    [[ -f $1 ]] || return 0
    while IFS= read -r line || [[ -n $line ]]; do
        number=$((number + 1))
        _trash_config_parse_line "$line"
        case $_tc_kind in
            error) _TRASH_CONFIG_FILE_ERRORS+=("$1:$number: $_tc_error") ;;
            entry)
                if [[ -n ${_TRASH_CONFIG_FILE_LINE[$_tc_key]:-} ]]; then
                    _TRASH_CONFIG_FILE_DUPES+=("$_tc_key")
                else
                    _TRASH_CONFIG_FILE_ORDER+=("$_tc_key")
                fi
                _TRASH_CONFIG_FILE_VALUES[$_tc_key]=$_tc_value
                _TRASH_CONFIG_FILE_LINE[$_tc_key]=$number
                ;;
        esac
    done < "$1"
    ((${#_TRASH_CONFIG_FILE_ERRORS[@]} == 0))
}

_TRASH_CONFIG_LIB=${BASH_SOURCE[0]%/*}
_TRASH_PREFS_LINE_RE='^([[:space:]]*)([A-Za-z_][A-Za-z0-9_]*(::[A-Za-z_][A-Za-z0-9_]*)?)[[:space:]]+([A-Za-z][A-Za-z0-9]*):(.*)$'
_TRASH_PREFS_COMMENT_RE=$'^[[:space:]]*(\'[^\']*\'|"[^"]*"|[^[:space:]#]+)[[:space:]]*(#.*)$'
_TRASH_CLASS_NAME_RE='^[A-Z][A-Za-z0-9]*$'

_trash_config_declared() {
    [[ -n $1 && -n ${_TRASH_CONFIG_TYPE[$1]+set} ]]
}

# Throws ConfigurationError unless the key is declared.
_trash_config_require() {
    _trash_config_load || return 1
    _trash_config_declared "$1" && return 0
    if ((${#_TRASH_CONFIG_KEYS[@]} == 0)); then
        _throw ConfigurationError "No settings are compiled (missing $(_trash_config_table)); run make"
    else
        _throw ConfigurationError "Unknown configuration key: $1 (see: @ Config list)"
    fi
    return 1
}

# Usage: _trash_config_valid key value. Sets _tc_error when invalid.
_trash_config_valid() {
    local type=${_TRASH_CONFIG_TYPE[$1]} choice
    _tc_error=''
    case $type in
        string) [[ $2 != *$'\n'* ]] && return 0; _tc_error='must be a single line' ;;
        integer) [[ $2 =~ ^[0-9]+$ ]] && return 0; _tc_error='must be a non-negative integer' ;;
        boolean) [[ $2 == true || $2 == false ]] && return 0; _tc_error='must be true or false' ;;
        enum:*)
            for choice in ${type#enum:}; do
                [[ $2 == "$choice" ]] && return 0
            done
            _tc_error="must be one of: ${type#enum:}"
            ;;
    esac
    return 1
}

# Root preferences classes (direct subclasses of Preferences). Sets _tc_roots.
_trash_prefs_roots() {
    local name
    _tc_roots=()
    for name in "${_TRASH_PREFS_CLASSES[@]}"; do
        [[ -n ${_TRASH_PREFS_PARENT[$name]} ]] || _tc_roots+=("$name")
    done
}

# Sets _tc_prefs to the active preferences class, or empty when none applies:
# TRASHTALK_PREFERENCES, then the class whose host: matches this machine, then
# the only root class. TRASHTALK_SKIP_USER_CONFIG skips the last two.
_trash_prefs_active() {
    local host=${HOSTNAME%%.*} name
    _tc_prefs=''
    if [[ -n ${TRASHTALK_PREFERENCES:-} ]]; then
        if [[ -z ${_TRASH_PREFS_SOURCE[$TRASHTALK_PREFERENCES]+set} ]]; then
            _throw ConfigurationError "TRASHTALK_PREFERENCES names $TRASHTALK_PREFERENCES, which is not a compiled Preferences class"
            return 1
        fi
        _tc_prefs=$TRASHTALK_PREFERENCES
        return 0
    fi
    [[ -z ${TRASHTALK_SKIP_USER_CONFIG:-} ]] || return 0
    for name in "${_TRASH_PREFS_CLASSES[@]}"; do
        if [[ -n ${_TRASH_PREFS_HOST[$name]} && ${_TRASH_PREFS_HOST[$name],,} == "${host,,}" ]]; then
            _tc_prefs=$name
            return 0
        fi
    done
    _trash_prefs_roots
    ((${#_tc_roots[@]} != 1)) || _tc_prefs=${_tc_roots[0]}
    return 0
}

# Resolve one key. Sets _tc_value, _tc_source (env, preferences, file,
# default), _tc_env_name, and _tc_prefs_class; throws ConfigurationError for
# unknown keys, file syntax errors, and invalid values.
_trash_config_resolve() {
    local key=$1 var class
    _trash_config_require "$key" || return 1
    var=${_TRASH_CONFIG_ENV[$key]}
    _tc_env_name=$var _tc_prefs_class=''
    if [[ -n ${!var:-} ]]; then
        _tc_value=${!var} _tc_source=env
        if ! _trash_config_valid "$key" "$_tc_value"; then
            _throw ConfigurationError "$var=$_tc_value is invalid for $key: $_tc_error"
            return 1
        fi
        return 0
    fi
    # The build checked these values against their declarations.
    _trash_prefs_active || return 1
    class=$_tc_prefs
    while [[ -n $class ]]; do
        if [[ -n ${_TRASH_PREFS_VALUES[$class|$key]+set} ]]; then
            _tc_value=${_TRASH_PREFS_VALUES[$class|$key]} _tc_source=preferences _tc_prefs_class=$class
            return 0
        fi
        class=${_TRASH_PREFS_PARENT[$class]:-}
    done
    _trash_config_file
    if [[ -n $_tc_file ]]; then
        if ! _trash_config_scan "$_tc_file"; then
            _throw ConfigurationError "${_TRASH_CONFIG_FILE_ERRORS[0]}"
            return 1
        fi
        if [[ -n ${_TRASH_CONFIG_FILE_VALUES[$key]+set} ]]; then
            _tc_value=${_TRASH_CONFIG_FILE_VALUES[$key]} _tc_source=file
            if ! _trash_config_valid "$key" "$_tc_value"; then
                _throw ConfigurationError "$_tc_file:${_TRASH_CONFIG_FILE_LINE[$key]}: $key $_tc_error"
                return 1
            fi
            return 0
        fi
    fi
    _tc_value=${_TRASH_CONFIG_DEFAULT[$key]} _tc_source=default
}

# Sets _tc_source_label to a short description of where _tc_value came from.
_trash_config_source_label() {
    case $_tc_source in
        env) _tc_source_label="env $_tc_env_name" ;;
        preferences) _tc_source_label="preferences $_tc_prefs_class" ;;
        *) _tc_source_label=$_tc_source ;;
    esac
}

trash_config_at() {
    _trash_config_resolve "$1" || return 1
    printf '%s\n' "$_tc_value"
}

# The legacy config file's path, whether or not it exists.
trash_config_path() {
    _trash_config_file
    if [[ -n $_tc_file ]]; then
        printf '%s\n' "$_tc_file"
    else
        printf '%s\n' "${XDG_CONFIG_HOME:-$HOME/.config}/trashtalk/config"
    fi
}

# Print key, value, and source for each named key, in aligned columns.
_trash_config_rows() {
    local key width=0 value_width=0 i=0
    local -a values=() sources=()
    for key in "$@"; do
        _trash_config_resolve "$key" || return 1
        _trash_config_source_label
        values+=("$_tc_value") sources+=("$_tc_source_label")
        ((${#key} <= width)) || width=${#key}
        ((${#_tc_value} <= value_width)) || value_width=${#_tc_value}
    done
    for key in "$@"; do
        printf "%-${width}s  %-${value_width}s  %s\n" "$key" "${values[i]}" "${sources[i]}"
        i=$((i + 1))
    done
}

# Effective value and source of every key.
trash_config_list() {
    _trash_config_load || return 1
    _trash_config_rows "${_TRASH_CONFIG_KEYS[@]}"
}

# Render a value as a preference literal: integers and booleans bare, strings
# in single quotes, or double quotes when the value holds an apostrophe.
_trash_prefs_literal() {
    case ${_TRASH_CONFIG_TYPE[$1]} in
        integer | boolean) printf '%s' "$2" ;;
        *)
            if [[ $2 == *"'"* ]]; then printf '"%s"' "$2"; else printf "'%s'" "$2"; fi
            ;;
    esac
}

# A value a preference line can hold, or a ConfigurationError.
_trash_prefs_writable() {
    if ! _trash_config_valid "$1" "$2"; then
        _throw ConfigurationError "$1 $_tc_error"
        return 1
    fi
    if [[ $2 == *\\* || ( $2 == *"'"* && $2 == *'"'* ) ]]; then
        _throw ConfigurationError "$1 cannot contain a backslash, or both single and double quotes"
        return 1
    fi
}

# Follow symlinks so a rename replaces the file a dotfiles manager links to.
# Sets _tc_target.
_trash_config_target() {
    local link hops=0
    _tc_target=$1
    [[ $_tc_target == */* ]] || _tc_target=./$_tc_target
    while [[ -L $_tc_target ]]; do
        hops=$((hops + 1))
        ((hops <= 40)) || { _throw ConfigurationError "Too many symlinks at $1"; return 1; }
        link=$(readlink "$_tc_target") || return 1
        [[ $link == /* ]] || link=${_tc_target%/*}/$link
        _tc_target=$link
    done
}

# Compile one user class from its source and reload the settings table.
_trash_user_class_compile() {
    local name=$1 source=$2 output
    if ! output=$(TRASHTALK_DIR="${TRASHTALK_DIR:-${TRASHDIR%/trash}}" \
        bash "$_TRASH_CONFIG_LIB/jq-compiler/driver.bash" compile-cached "$source" "$TRASHDIR/.compiled/$name" 2>&1); then
        _tc_error=$output
        return 1
    fi
    _trash_config_unload
    unset "_SOURCED_COMPILED_CLASSES[$name]" 2>/dev/null || true
}

# Answer the preferences class to write: the named one, or the active one.
# Sets _tc_prefs.
_trash_prefs_target() {
    local file_values=0
    if [[ -n $1 ]]; then
        if [[ -z ${_TRASH_PREFS_SOURCE[$1]+set} ]]; then
            _throw ConfigurationError "$1 is not a compiled Preferences class (see: @ Config preferences)"
            return 1
        fi
        _tc_prefs=$1
        return 0
    fi
    _trash_prefs_active || return 1
    [[ -z $_tc_prefs ]] || return 0
    _trash_prefs_roots
    if [[ -n ${TRASHTALK_SKIP_USER_CONFIG:-} ]]; then
        _throw ConfigurationError 'User preferences are disabled by TRASHTALK_SKIP_USER_CONFIG; set TRASHTALK_PREFERENCES or name a class with at:put:in:'
    elif ((${#_tc_roots[@]} > 1)); then
        _throw ConfigurationError "No Preferences class is active: ${_tc_roots[*]} could each apply; set TRASHTALK_PREFERENCES or give the class for this machine host: ${HOSTNAME%%.*}"
    else
        _trash_config_file
        [[ -z $_tc_file ]] || ! _trash_config_scan "$_tc_file" || file_values=${#_TRASH_CONFIG_FILE_ORDER[@]}
        if ((file_values)); then
            _throw ConfigurationError "No Preferences class is active; move $_tc_file into one with: @ Config import: 'YourName'"
        else
            _throw ConfigurationError "No Preferences class is active; create one with: @ Trash newPreferencesClass: 'YourName' subclassing: 'Preferences'"
        fi
    fi
    return 1
}

# Rewrite a preferences class with key set to value, or removed when $4 is
# "reset", then recompile it. Comments, blank lines, and order are kept; the
# write is an atomic rename through any symlink, undone if the class no longer
# compiles.
_trash_prefs_rewrite() {
    local class=$1 key=$2 value=$3 mode=$4 group selector line replacement='' done_key='' comment
    local directory tmp backup
    local -a lines=() output=()
    group=${_TRASH_CONFIG_GROUP[$key]} selector=${key#*.}
    _trash_config_target "${_TRASH_PREFS_SOURCE[$class]}" || return 1
    if [[ ! -f $_tc_target ]]; then
        _throw ConfigurationError "The source of $class is missing: ${_TRASH_PREFS_SOURCE[$class]}"
        return 1
    fi
    mapfile -t lines < "$_tc_target" || return 1
    [[ $mode != set ]] || replacement="$group $selector: $(_trash_prefs_literal "$key" "$value")"
    for line in "${lines[@]}"; do
        if [[ $line =~ $_TRASH_PREFS_LINE_RE && ${BASH_REMATCH[2]} == "$group" && ${BASH_REMATCH[4]} == "$selector" ]]; then
            local indent=${BASH_REMATCH[1]} rest=${BASH_REMATCH[5]}
            comment=''
            [[ ! $rest =~ $_TRASH_PREFS_COMMENT_RE ]] || comment=${BASH_REMATCH[2]}
            if [[ $mode == set && -z $done_key ]]; then
                output+=("$indent$replacement${comment:+  $comment}")
            fi
            done_key=1
            continue
        fi
        output+=("$line")
    done
    if [[ $mode == set && -z $done_key ]]; then
        while ((${#output[@]})) && [[ -z ${output[-1]//[[:space:]]/} ]]; do unset 'output[-1]'; done
        output+=("  $replacement")
    fi
    [[ $mode != reset || -n $done_key ]] || return 0
    directory=${_tc_target%/*}
    tmp=$(mktemp "$directory/.preferences.XXXXXX") || return 1
    backup=$(mktemp "$directory/.preferences-backup.XXXXXX") || { rm -f "$tmp"; return 1; }
    if ! printf '%s\n' "${output[@]}" > "$tmp" || ! cp -p "$_tc_target" "$backup"; then
        rm -f "$tmp" "$backup"
        return 1
    fi
    mv -f "$tmp" "$_tc_target" || { rm -f "$tmp" "$backup"; return 1; }
    if ! _trash_user_class_compile "$class" "${_TRASH_PREFS_SOURCE[$class]}"; then
        mv -f "$backup" "$_tc_target"
        _throw ConfigurationError "$class no longer compiles, so the change was undone: $_tc_error"
        return 1
    fi
    rm -f "$backup"
}

_trash_config_warn_shadow() {
    local var=${_TRASH_CONFIG_ENV[$1]}
    [[ -z ${!var:-} ]] || echo "Warning: $var is set in the environment and overrides $1" >&2
}

# Usage: trash_config_put key value [PreferencesClass]
trash_config_put() {
    local key=$1 value=$2
    _trash_config_require "$key" || return 1
    _trash_prefs_writable "$key" "$value" || return 1
    _trash_prefs_target "${3:-}" || return 1
    _trash_prefs_rewrite "$_tc_prefs" "$key" "$value" set || return 1
    _trash_config_warn_shadow "$key"
}

# Usage: trash_config_reset key [PreferencesClass]
trash_config_reset() {
    _trash_config_require "$1" || return 1
    _trash_prefs_target "${2:-}" || return 1
    _trash_prefs_rewrite "$_tc_prefs" "$1" '' reset || return 1
    _trash_config_warn_shadow "$1"
}

# Group-side primitives. A group's generated setter calls trash_settings_put.
trash_settings_put() { trash_config_put "$1" "$2"; }

# Usage: trash_settings_reset Group name
trash_settings_reset() {
    _trash_config_load || return 1
    if [[ -z ${_TRASH_SETTINGS_PREFIX[$1]+set} ]]; then
        _throw ConfigurationError "$1 is not a settings group"
        return 1
    fi
    trash_config_reset "${_TRASH_SETTINGS_PREFIX[$1]}.$2"
}

# Every settings group with its prefix and number of settings.
trash_settings_groups() {
    local group key count unit width=0
    _trash_config_load || return 1
    for group in "${_TRASH_SETTINGS_GROUPS[@]}"; do
        ((${#group} <= width)) || width=${#group}
    done
    for group in "${_TRASH_SETTINGS_GROUPS[@]}"; do
        count=0
        for key in "${_TRASH_CONFIG_KEYS[@]}"; do
            [[ ${_TRASH_CONFIG_GROUP[$key]} != "$group" ]] || count=$((count + 1))
        done
        unit=settings
        ((count != 1)) || unit=setting
        printf "%-${width}s  %s  (%s %s)\n" "$group" "${_TRASH_SETTINGS_PREFIX[$group]}" "$count" "$unit"
    done
}

# Usage: trash_settings_describe Group. Each setting: effective value and
# source, then its description, type, default, and environment variable.
trash_settings_describe() {
    local group=$1 key type
    _trash_config_load || return 1
    if [[ -z ${_TRASH_SETTINGS_PREFIX[$group]+set} ]]; then
        _throw ConfigurationError "$group is not a settings group (see: @ Settings groups)"
        return 1
    fi
    printf '%s (prefix %s)\n' "$group" "${_TRASH_SETTINGS_PREFIX[$group]}"
    for key in "${_TRASH_CONFIG_KEYS[@]}"; do
        [[ ${_TRASH_CONFIG_GROUP[$key]} == "$group" ]] || continue
        _trash_config_resolve "$key" || return 1
        _trash_config_source_label
        type=${_TRASH_CONFIG_TYPE[$key]}
        [[ $type != enum:* ]] || type="one of: ${type#enum:}"
        printf '\n  %s = %s  (%s)\n' "${key#*.}" "$_tc_value" "$_tc_source_label"
        printf '    %s\n    %s, default %s, env %s\n' "${_TRASH_CONFIG_DOC[$key]}" "$type" \
            "$(_trash_prefs_literal "$key" "${_TRASH_CONFIG_DEFAULT[$key]}")" "${_TRASH_CONFIG_ENV[$key]}"
    done
}

# Every compiled preferences class: name, superclass, host, and source.
# The active class is marked with "*".
trash_config_preferences() {
    local name mark
    _trash_config_load || return 1
    _trash_prefs_active 2>/dev/null || _tc_prefs=''
    for name in "${_TRASH_PREFS_CLASSES[@]}"; do
        mark=' '
        [[ $name != "$_tc_prefs" ]] || mark='*'
        printf '%s %s  subclass of %s%s  %s\n' "$mark" "$name" "${_TRASH_PREFS_PARENT[$name]:-Preferences}" \
            "${_TRASH_PREFS_HOST[$name]:+, host ${_TRASH_PREFS_HOST[$name]}}" "${_TRASH_PREFS_SOURCE[$name]}"
    done
}

# Throws unless name can be a new class in trash/user/.
_trash_user_class_free() {
    local name=$1 path
    if [[ ! $name =~ $_TRASH_CLASS_NAME_RE ]]; then
        _throw ConfigurationError "A user class name is a capitalized identifier without a package: $name"
        return 1
    fi
    for path in "$TRASHDIR/user/$name.trash" "$TRASHDIR/$name.trash" "$TRASHDIR/traits/$name.trash" \
        "$TRASHDIR/.compiled/$name" "$TRASHDIR/.compiled/traits/$name"; do
        if [[ -e $path ]]; then
            _throw ConfigurationError "A class named $name already exists ($path); edit it with: @ Trash edit: $name"
            return 1
        fi
    done
}

# Write trash/user/<name>.trash with the given lines and compile it. On a
# compile failure the file is removed. Prints the source path.
_trash_user_class_write() {
    local name=$1 file
    shift
    _trash_user_class_free "$name" || return 1
    file=$TRASHDIR/user/$name.trash
    mkdir -p "$TRASHDIR/user" || return 1
    printf '%s\n' "$@" > "$file" || return 1
    if ! _trash_user_class_compile "$name" "$file"; then
        rm -f "$file"
        _throw ConfigurationError "$name did not compile: $_tc_error"
        return 1
    fi
    printf '%s\n' "$file"
}

# Usage: trash_user_class_create Name. A header-only class in trash/user/.
trash_user_class_create() {
    _trash_user_class_write "$1" "$1 subclass: Object"
}

# Check a preferences superclass name: Preferences or a compiled preferences class.
_trash_prefs_superclass() {
    _trash_config_load || return 1
    [[ $1 == Preferences || -n ${_TRASH_PREFS_SOURCE[$1]+set} ]] && return 0
    _throw ConfigurationError "$1 is neither Preferences nor a compiled preferences class (see: @ Config preferences)"
    return 1
}

# Usage: trash_preferences_create Name Superclass. Every setting appears
# commented out, so the new class changes nothing until a line is uncommented.
trash_preferences_create() {
    local name=$1 superclass=$2 key group='' type
    local -a lines=()
    _trash_prefs_superclass "$superclass" || return 1
    lines=("# $name - Trashtalk preferences. Uncomment a line to set it; values are literals:"
        "# 'text', integers, and true/false. See docs/settings-design.md."
        "$name subclass: $superclass"
        "  # host: name      (uncomment to make this the class for one machine)")
    for key in "${_TRASH_CONFIG_KEYS[@]}"; do
        if [[ ${_TRASH_CONFIG_GROUP[$key]} != "$group" ]]; then
            group=${_TRASH_CONFIG_GROUP[$key]}
            lines+=('' "  # --- $group ---")
        fi
        type=${_TRASH_CONFIG_TYPE[$key]}
        [[ $type != enum:* ]] || type="one of: ${type#enum:}"
        lines+=("  # ${_TRASH_CONFIG_DOC[$key]} ($type; env ${_TRASH_CONFIG_ENV[$key]})"
            "  # $group ${key#*.}: $(_trash_prefs_literal "$key" "${_TRASH_CONFIG_DEFAULT[$key]}")")
    done
    _trash_user_class_write "$name" "${lines[@]}"
}

# Usage: trash_config_import Name [Superclass]. A preferences class holding
# the legacy config file's values; the file itself is left in place.
trash_config_import() {
    local name=$1 superclass=${2:-Preferences} key value
    local -a lines=()
    _trash_prefs_superclass "$superclass" || return 1
    _trash_config_file
    if [[ -z $_tc_file || ! -f $_tc_file ]]; then
        _throw ConfigurationError "No config file to import at $(trash_config_path)"
        return 1
    fi
    if ! _trash_config_scan "$_tc_file"; then
        _throw ConfigurationError "${_TRASH_CONFIG_FILE_ERRORS[0]}"
        return 1
    fi
    lines=("# $name - Trashtalk preferences imported from $_tc_file."
        "$name subclass: $superclass")
    for key in "${_TRASH_CONFIG_FILE_ORDER[@]}"; do
        value=${_TRASH_CONFIG_FILE_VALUES[$key]}
        if ! _trash_config_declared "$key"; then
            echo "Warning: skipping unknown key $key" >&2
            continue
        fi
        _trash_prefs_writable "$key" "$value" || return 1
        lines+=("  ${_TRASH_CONFIG_GROUP[$key]} ${key#*.}: $(_trash_prefs_literal "$key" "$value")")
    done
    _trash_user_class_write "$name" "${lines[@]}" || return 1
    echo "Imported ${#_TRASH_CONFIG_FILE_ORDER[@]} settings. Once this class is active, remove $_tc_file." >&2
}

# Doctor report: one "ok|warn|bad<TAB>message" line per finding.
trash_config_check() {
    local key var message class hash finding=0
    local -a overlap=()
    _trash_config_load || return 1
    if ((${#_TRASH_CONFIG_KEYS[@]} == 0)); then
        printf 'bad\tNo settings are compiled (missing %s); run make\n' "$(_trash_config_table)"
    fi
    if ! _trash_prefs_active 2>/dev/null; then
        printf 'bad\tTRASHTALK_PREFERENCES names %s, which is not a compiled Preferences class\n' "$TRASHTALK_PREFERENCES"
        _tc_prefs=''
    elif [[ -n $_tc_prefs ]]; then
        printf 'ok\tPreferences: %s\n' "$_tc_prefs"
    elif [[ -z ${TRASHTALK_SKIP_USER_CONFIG:-} ]]; then
        _trash_prefs_roots
        if ((${#_tc_roots[@]} > 1)); then
            printf 'warn\tNo Preferences class is active: %s could each apply; set TRASHTALK_PREFERENCES or add host: %s\n' \
                "${_tc_roots[*]}" "${HOSTNAME%%.*}"
        else
            printf 'ok\tNo Preferences class; using defaults (start one with: @ Trash newPreferencesClass: '"'"'YourName'"'"' subclassing: '"'"'Preferences'"'"')\n'
        fi
    fi
    for class in "${_TRASH_PREFS_CLASSES[@]}"; do
        if [[ ! -f ${_TRASH_PREFS_SOURCE[$class]} ]]; then
            printf 'warn\tPreferences class %s has no source at %s; run make\n' "$class" "${_TRASH_PREFS_SOURCE[$class]}"
            continue
        fi
        hash=$(shasum -a 256 "${_TRASH_PREFS_SOURCE[$class]}" 2>/dev/null)
        [[ ${hash%% *} == "${_TRASH_PREFS_HASH[$class]}" ]] ||
            printf 'warn\tPreferences class %s changed since it was compiled; run make\n' "$class"
    done
    _trash_config_file
    _trash_config_scan "${_tc_file:-/dev/null/none}"
    if [[ -z $_tc_file ]]; then
        printf 'ok\tUser config file skipped (TRASHTALK_SKIP_USER_CONFIG)\n'
    elif [[ -e $_tc_file ]]; then
        for message in "${_TRASH_CONFIG_FILE_ERRORS[@]}"; do
            printf 'bad\tConfig syntax: %s\n' "$message"; finding=1
        done
        for key in "${_TRASH_CONFIG_FILE_ORDER[@]}"; do
            if ! _trash_config_declared "$key"; then
                printf 'warn\tUnknown config key %s at %s:%s\n' "$key" "$_tc_file" "${_TRASH_CONFIG_FILE_LINE[$key]}"
                finding=1
            elif ! _trash_config_valid "$key" "${_TRASH_CONFIG_FILE_VALUES[$key]}"; then
                printf 'bad\tConfig %s at %s:%s %s\n' "$key" "$_tc_file" "${_TRASH_CONFIG_FILE_LINE[$key]}" "$_tc_error"
                finding=1
            fi
            class=$_tc_prefs
            while [[ -n $class ]]; do
                if [[ -n ${_TRASH_PREFS_VALUES[$class|$key]+set} ]]; then overlap+=("$key"); break; fi
                class=${_TRASH_PREFS_PARENT[$class]:-}
            done
        done
        for key in "${_TRASH_CONFIG_FILE_DUPES[@]}"; do
            printf 'warn\tConfig key %s appears more than once in %s; the last one wins\n' "$key" "$_tc_file"
            finding=1
        done
        if ((${#overlap[@]})); then
            printf 'warn\t%s sets %s, which the config file %s also sets; the preferences win\n' \
                "$_tc_prefs" "${overlap[*]}" "$_tc_file"
            finding=1
        fi
        ((finding)) || printf 'ok\tConfig file %s (%s settings; move them with: @ Config import: '"'"'YourName'"'"')\n' \
            "$_tc_file" "${#_TRASH_CONFIG_FILE_ORDER[@]}"
    fi
    for key in "${_TRASH_CONFIG_KEYS[@]}"; do
        var=${_TRASH_CONFIG_ENV[$key]}
        [[ -n ${!var:-} ]] || continue
        if ! _trash_config_valid "$key" "${!var}"; then
            printf 'bad\t%s is invalid for %s: %s\n' "$var" "$key" "$_tc_error"
            continue
        fi
        class=$_tc_prefs
        while [[ -n $class && -z ${_TRASH_PREFS_VALUES[$class|$key]+set} ]]; do
            class=${_TRASH_PREFS_PARENT[$class]:-}
        done
        if [[ -n $class ]]; then
            printf 'warn\t%s overrides %s from preferences %s\n' "$var" "$key" "$class"
        elif [[ -n $_tc_file && -n ${_TRASH_CONFIG_FILE_VALUES[$key]+set} ]]; then
            printf 'warn\t%s overrides %s from the config file\n' "$var" "$key"
        fi
    done
}

# The active preferences class, or an empty line when none applies.
trash_preferences_active() {
    _trash_config_load || return 1
    _trash_prefs_active || return 1
    printf '%s\n' "$_tc_prefs"
}
