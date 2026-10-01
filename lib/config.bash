# User configuration: declared keys, the TOML-subset file, and env overrides.
# Config.trash exposes these as primitives. `trash_config_at` runs on builtins
# only; see docs/config-design.md for the resolution order and file format.

declare -ga _TRASH_CONFIG_KEYS=()
declare -gA _TRASH_CONFIG_ENV=() _TRASH_CONFIG_TYPE=() _TRASH_CONFIG_DEFAULT=() _TRASH_CONFIG_DOC=()
declare -gA _TRASH_CONFIG_FILE_VALUES=() _TRASH_CONFIG_FILE_LINE=()
declare -ga _TRASH_CONFIG_FILE_ORDER=() _TRASH_CONFIG_FILE_DUPES=() _TRASH_CONFIG_FILE_ERRORS=()

# Usage: _trash_config_declare key ENV_VAR type default description
# type is string, integer (non-negative), boolean, or "enum:a b c".
_trash_config_declare() {
    _TRASH_CONFIG_KEYS+=("$1")
    _TRASH_CONFIG_ENV[$1]=$2 _TRASH_CONFIG_TYPE[$1]=$3
    _TRASH_CONFIG_DEFAULT[$1]=$4 _TRASH_CONFIG_DOC[$1]=$5
}

# Enum values mirror Agent::Worker driverFor: and Decision::Target named:.
_trash_config_declare gusgus.profile TRASHTALK_GUSGUS_PROFILE 'enum:jcode maki codex shell' jcode \
    'Harness for new Gusgus sessions'
_trash_config_declare jcode.model TRASHTALK_JCODE_MODEL string gpt-5.6-terra 'Model for Jcode sessions'
_trash_config_declare codex.model TRASHTALK_CODEX_MODEL string gpt-5.6-terra 'Model for Codex sessions'
_trash_config_declare maki.model TRASHTALK_MAKI_MODEL string openai/gpt-5.6-terra \
    'Model for Maki sessions (an openai/ model)'
_trash_config_declare agent.controlWait TRASHTALK_CONTROL_WAIT integer 30 \
    'Seconds a stop or terminate waits behind a running agent tick'
_trash_config_declare decision.target TRASHTALK_DECISION_TARGET 'enum:jev clm-local clm-bc250 clm-prefer-bc250' jev \
    'Where typed decisions run'
_trash_config_declare jev.model TRASHTALK_JEV_MODEL string typesafe/jev-1.13 'OpenRouter model for Jev decisions'
_trash_config_declare clm.baseUrl CLM_BASE_URL string http://127.0.0.1:8700 'Local CLM endpoint'
_trash_config_declare clm.bc250Url CLM_BC250_URL string '' 'BC-250 CLM endpoint'
_trash_config_declare clm.model CLM_MODEL string clm-latest 'CLM model name'

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

_trash_config_declared() {
    [[ -n $1 && -n ${_TRASH_CONFIG_TYPE[$1]+set} ]]
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

# Resolve one key. Sets _tc_value and _tc_source (env, file, default) and
# _tc_env_name; throws ConfigurationError for unknown keys, file syntax
# errors, and invalid values.
_trash_config_resolve() {
    local key=$1 var
    if ! _trash_config_declared "$key"; then
        _throw ConfigurationError "Unknown configuration key: $key (see: @ Config list)"
        return 1
    fi
    var=${_TRASH_CONFIG_ENV[$key]}
    _tc_env_name=$var
    if [[ -n ${!var:-} ]]; then
        _tc_value=${!var} _tc_source=env
        if ! _trash_config_valid "$key" "$_tc_value"; then
            _throw ConfigurationError "$var=$_tc_value is invalid for $key: $_tc_error"
            return 1
        fi
        return 0
    fi
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

trash_config_at() {
    _trash_config_resolve "$1" || return 1
    printf '%s\n' "$_tc_value"
}

trash_config_path() {
    _trash_config_file
    if [[ -n $_tc_file ]]; then
        printf '%s\n' "$_tc_file"
    else
        printf '%s\n' "${XDG_CONFIG_HOME:-$HOME/.config}/trashtalk/config"
    fi
}

# Effective value, source, and description of every key.
trash_config_list() {
    local key width=0 value_width=0
    local -a values=() sources=()
    for key in "${_TRASH_CONFIG_KEYS[@]}"; do
        _trash_config_resolve "$key" || return 1
        [[ $_tc_source != env ]] || _tc_source="env $_tc_env_name"
        values+=("$_tc_value") sources+=("$_tc_source")
        ((${#key} <= width)) || width=${#key}
        ((${#_tc_value} <= value_width)) || value_width=${#_tc_value}
    done
    local i=0
    for key in "${_TRASH_CONFIG_KEYS[@]}"; do
        printf "%-${width}s  %-${value_width}s  %s\n" "$key" "${values[i]}" "${sources[i]}"
        i=$((i + 1))
    done
}

# Render a key's value as it is written in the file.
_trash_config_literal() {
    case ${_TRASH_CONFIG_TYPE[$1]} in
        integer | boolean) printf '%s' "$2" ;;
        *) printf '"%s"' "$2" ;;
    esac
}

# A commented config file with every key at its default.
trash_config_template() {
    local key type
    printf '%s\n' '# Trashtalk configuration. Uncomment a line to change a setting.' \
        '# Environment variables override this file. `@ Config list` shows effective values.' \
        '# Format: flat TOML. Strings are "quoted"; integers and true/false are bare.'
    for key in "${_TRASH_CONFIG_KEYS[@]}"; do
        type=${_TRASH_CONFIG_TYPE[$key]}
        printf '\n# %s.' "${_TRASH_CONFIG_DOC[$key]}"
        [[ $type != enum:* ]] || printf ' One of: %s.' "${type#enum:}"
        printf '\n# Environment override: %s\n# %s = %s\n' "${_TRASH_CONFIG_ENV[$key]}" "$key" \
            "$(_trash_config_literal "$key" "${_TRASH_CONFIG_DEFAULT[$key]}")"
    done
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

# Rewrite the user file with key set to value, or removed when $3 is "reset".
# Comments, blank lines, and order are kept; the write is an atomic rename.
_trash_config_rewrite() {
    local key=$1 value=$2 mode=$3 line replacement='' done_key='' number=0 directory tmp
    local -a lines=() output=()
    _trash_config_file
    if [[ -z $_tc_file ]]; then
        _throw ConfigurationError 'User configuration is disabled by TRASHTALK_SKIP_USER_CONFIG; set TRASHTALK_CONFIG to write a file'
        return 1
    fi
    _trash_config_target "$_tc_file" || return 1
    if [[ -f $_tc_target ]]; then
        mapfile -t lines < "$_tc_target" || return 1
    elif [[ $mode == reset ]]; then
        return 0
    fi
    if [[ $mode == set ]]; then
        replacement="$key = $(_trash_config_literal "$key" "$value")"
    fi
    for line in "${lines[@]}"; do
        number=$((number + 1))
        _trash_config_parse_line "$line"
        if [[ $_tc_kind == error ]]; then
            _throw ConfigurationError "$_tc_target:$number: $_tc_error"
            return 1
        fi
        if [[ $_tc_kind == entry && $_tc_key == "$key" ]]; then
            if [[ $mode == set && -z $done_key ]]; then
                output+=("$replacement${_tc_comment:+  $_tc_comment}")
            fi
            done_key=1
            continue
        fi
        output+=("$line")
    done
    [[ $mode != set || -n $done_key ]] || output+=("$replacement")
    [[ $mode != reset || -n $done_key ]] || return 0
    directory=${_tc_target%/*}
    mkdir -p "$directory" || return 1
    tmp=$(mktemp "$directory/.config.XXXXXX") || return 1
    if ((${#output[@]})); then
        printf '%s\n' "${output[@]}" > "$tmp" || { rm -f "$tmp"; return 1; }
    fi
    mv -f "$tmp" "$_tc_target" || { rm -f "$tmp"; return 1; }
}

_trash_config_warn_shadow() {
    local var=${_TRASH_CONFIG_ENV[$1]}
    [[ -z ${!var:-} ]] || echo "Warning: $var is set in the environment and overrides $1" >&2
}

trash_config_put() {
    local key=$1 value=$2
    if ! _trash_config_declared "$key"; then
        _throw ConfigurationError "Unknown configuration key: $key (see: @ Config list)"
        return 1
    fi
    if ! _trash_config_valid "$key" "$value"; then
        _throw ConfigurationError "$key $_tc_error"
        return 1
    fi
    if [[ $value == *[\"\\]* ]]; then
        _throw ConfigurationError "$key cannot contain double quotes or backslashes"
        return 1
    fi
    _trash_config_rewrite "$key" "$value" set || return 1
    _trash_config_warn_shadow "$key"
}

trash_config_reset() {
    if ! _trash_config_declared "$1"; then
        _throw ConfigurationError "Unknown configuration key: $1 (see: @ Config list)"
        return 1
    fi
    _trash_config_rewrite "$1" '' reset || return 1
    _trash_config_warn_shadow "$1"
}

# Doctor report: one "ok|warn|bad<TAB>message" line per finding.
trash_config_check() {
    local key var message finding=0
    _trash_config_file
    _trash_config_scan "${_tc_file:-/dev/null/none}"
    if [[ -z $_tc_file ]]; then
        printf 'ok\tUser config file skipped (TRASHTALK_SKIP_USER_CONFIG)\n'
    elif [[ ! -e $_tc_file ]]; then
        printf 'ok\tNo config file at %s; using defaults (start one with: @ Config template)\n' "$_tc_file"
    else
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
        done
        for key in "${_TRASH_CONFIG_FILE_DUPES[@]}"; do
            printf 'warn\tConfig key %s appears more than once in %s; the last one wins\n' "$key" "$_tc_file"
            finding=1
        done
        ((finding)) || printf 'ok\tConfig file %s (%s settings)\n' "$_tc_file" "${#_TRASH_CONFIG_FILE_ORDER[@]}"
    fi
    for key in "${_TRASH_CONFIG_KEYS[@]}"; do
        var=${_TRASH_CONFIG_ENV[$key]}
        [[ -n ${!var:-} ]] || continue
        if ! _trash_config_valid "$key" "${!var}"; then
            printf 'bad\t%s is invalid for %s: %s\n' "$var" "$key" "$_tc_error"
        elif [[ -n $_tc_file && -n ${_TRASH_CONFIG_FILE_VALUES[$key]+set} ]]; then
            printf 'warn\t%s overrides %s from the config file\n' "$var" "$key"
        fi
    done
}
