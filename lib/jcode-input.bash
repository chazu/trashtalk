#!/usr/bin/env bash
# Private adapter control files, protected by flock(2). These are native input
# requests/receipts, not inbox messages. A closed adapter cannot accept input.
_jcode_input_lock() {
    exec {jcode_input_fd}>"$directory/input.lock" || return 1
    perl -e 'flock(STDIN, 2) or die "input lock: $!"' <&"$jcode_input_fd"
}
_jcode_input_unlock() { exec {jcode_input_fd}>&-; }
_jcode_input_ack() {
    jq -cn --argjson ok "$2" --arg message "$3" '{ok:$ok,message:$message}' > "$1.tmp"
    mv "$1.tmp" "$1"
}
_jcode_input_wait() {
    local receipt=$1 attempt
    for attempt in {1..200}; do
        if [[ -s "$receipt" ]]; then
            jq -r .message "$receipt"
            [[ $(jq -r .ok "$receipt") == true ]]
            return
        fi
        [[ ! -f "$directory/exit" ]] || break
        sleep .1
    done
    echo 'Session input was not acknowledged; inspect the conversation before retrying' >&2
    return 1
}

if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
    set -euo pipefail
    umask 077
    mode=$1 directory=$2
    if [[ "$mode" == await ]]; then
        _jcode_input_wait "$directory/first-input.ack"
        exit
    fi
    [[ "$mode" == send ]] || exit 2
    request=$(uuidgen)
    mkdir -p "$directory/inputs"
    jq -Rsc '{content:.}' > "$directory/inputs/$request.tmp"
    _jcode_input_lock
    if [[ ! -f "$directory/input-open" || -f "$directory/exit" ]]; then
        rm "$directory/inputs/$request.tmp"
        _jcode_input_unlock
        echo 'The session turn is finishing or unavailable; input was not sent. Try again' >&2
        exit 1
    fi
    mv "$directory/inputs/$request.tmp" "$directory/inputs/$request.request"
    _jcode_input_unlock
    if _jcode_input_wait "$directory/inputs/$request.ack"; then exit 0; fi
    # Withdraw only input the adapter has not claimed. A sent request remains
    # inspectable and is never replayed after an ambiguous disconnect.
    _jcode_input_lock
    rm -f "$directory/inputs/$request.request"
    _jcode_input_unlock
    exit 1
fi
