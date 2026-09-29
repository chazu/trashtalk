# OS boundaries shared by Process and Future. Tool owns launch and exit receipts.
_trash_remove_empty_directory() { rmdir -- "$1"; }

_trash_process_alive() {
    local pid=$1 directory=$2 birth current state
    [[ $pid =~ ^[0-9]+$ && -s $directory/pid.start ]] || { echo false; return; }
    birth=$(cat "$directory/pid.start") || return
    current=$(ps -p "$pid" -o lstart= 2>/dev/null) || { echo false; return; }
    state=$(ps -p "$pid" -o stat= 2>/dev/null) || { echo false; return; }
    if [[ -n $birth && $birth == "$current" && $state != *Z* ]]; then echo true; else echo false; fi
}

_trash_process_signal() {
    local pid=$1 directory=$2 sig=$3
    [[ $(_trash_process_alive "$pid" "$directory") == true ]] || return 0
    if ! kill "-$sig" -- "-$pid" 2>/dev/null; then
        # Completion can race the identity check; an already-exited process
        # needs no signal. Retain real failures against a still-live process.
        [[ $(_trash_process_alive "$pid" "$directory") == false ]] && return 0
        _throw ProcessError "Could not signal process $pid with $sig"
        return 1
    fi
    # SIGKILL cannot be observed by the supervisor itself.
    if [[ $sig == KILL || $sig == 9 ]]; then
        printf '137\n' > "$directory/exit.killed" && mv "$directory/exit.killed" "$directory/exit"
    fi
}

_trash_process_cleanup() {
    local directory=$1
    [[ -n $directory ]] || return 0
    rm -f -- "$directory/stdout.log" "$directory/stderr.log" "$directory/pid" \
        "$directory/pid.start" "$directory/exit" "$directory/exit.tmp" "$directory/exit.killed"
    rmdir -- "$directory"
}
