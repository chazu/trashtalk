# The sole HTTP/file boundary for typed decision services. Credentials are
# selected by environment-variable name and never appear in curl's argv; an
# empty name sends no credential. A busy single-slot service (HTTP 409) is
# retried three times with jittered backoff; no other failure is retried.
trash_decision_http() (
    local method=$1 url=$2 body=$3 credential=$4 required=$5 deadline=$6
    local key='' tmp status rc attempt
    if [[ -n $credential ]]; then
        [[ $credential =~ ^[A-Z][A-Z0-9_]*$ ]] || return 1
        key=${!credential:-}
    fi
    if [[ $required == true && -z $key ]] || [[ -n $key && ! $key =~ ^[a-zA-Z0-9_.-]+$ ]]; then
        _throw ConfigurationError "A valid $credential is required"
        return 1
    fi
    # Prevent curl-config injection and embedded credentials; redirects are not followed.
    if [[ ! $url =~ ^https?://[a-zA-Z0-9._-]+(:[0-9]+)?(/[a-zA-Z0-9._/-]*)?$ ]]; then
        _throw ConfigurationError 'Invalid decision endpoint URL'
        return 1
    fi
    umask 077
    tmp=$(mktemp -d "${TMPDIR:-/tmp}/trashtalk-decision.XXXXXX") || return 1
    trap 'rm -rf "$tmp"' EXIT
    printf '%s' "$body" > "$tmp/request.json"
    {
        printf '%s\n' 'silent' 'show-error'
        printf 'request = "%s"\nurl = "%s"\n' "$method" "$url"
        printf '%s\n' 'header = "Content-Type: application/json"'
        [[ -z $key ]] || printf 'header = "Authorization: Bearer %s"\n' "$key"
    } > "$tmp/curl.conf"
    local -a args=(--disable --config "$tmp/curl.conf" --connect-timeout 2 --max-time "$deadline"
                   --output "$tmp/response.json" --write-out '%{http_code}')
    [[ $method != POST ]] || args+=(--data-binary "@$tmp/request.json")
    for attempt in 0 1 2 3; do
        if status=$(curl "${args[@]}" 2>"$tmp/error"); then :; else
            rc=$?
            jq -cn --argjson code "$rc" --rawfile error "$tmp/error" \
                '{outcome:"transport_error",exit_code:$code,stderr:$error}'
            return 1
        fi
        [[ $status == 409 && $attempt -lt 3 ]] || break
        sleep "$(printf '0.%03d' $(( (200 << attempt) + RANDOM % 100 )))"
    done
    if [[ ! $status =~ ^2[0-9][0-9]$ ]]; then
        jq -cn --arg status "$status" --rawfile body "$tmp/response.json" \
            '{outcome:"http_error",status:($status|tonumber?),body:$body}'
        return 1
    fi
    if ! jq -e 'type == "object"' "$tmp/response.json" >/dev/null 2>&1; then
        jq -cn --rawfile body "$tmp/response.json" '{outcome:"response_shape_error",body:$body}'
        return 1
    fi
    cat "$tmp/response.json"
)
