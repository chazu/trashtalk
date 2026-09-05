#!/usr/bin/env bash
# Delayed human progress at external-process boundaries. All rendering goes to
# the controlling terminal; stdout/stderr and child status remain untouched.
# Wrap only external/read-only work: the active renderer owns a subshell.

_trash_with_progress() {
    local label="$1"; shift
    local enabled=0
    case "${TRASHTALK_PROGRESS:-auto}" in
        1|on) enabled=1 ;;
        auto)
            if [[ $- == *i* || "${TRASHTALK_INTERACTIVE:-0}" == 1 ]]; then enabled=1; fi
            ;;
    esac
    # Capture boundaries redirect fd 0/1/2 before calling us. Interaction is
    # determined by the caller's mode and controlling TTY, not those data fds.
    if [[ "$enabled" == 1 && -n "$label" && -z "${_TRASH_PROGRESS_ACTIVE:-}" ]] &&
       { true >/dev/tty; } 2>/dev/null; then
        _trash_progress_run "$label" "$@"
    else
        "$@"
    fi
}

_trash_progress_run() (
    local label="$1"; shift
    local directory reporter status tty_fd
    local -x _TRASH_PROGRESS_ACTIVE=1
    exec {tty_fd}>/dev/tty || { "$@"; exit $?; }
    directory=$(mktemp -d "${TMPDIR:-/tmp}/trash-progress.XXXXXX") || { "$@"; exit $?; }
    _trash_progress_cleanup() {
        # A session leader exiting may send HUP while TERM cleanup is running.
        # Finish restoring the already-open terminal descriptor exactly once.
        trap '' INT TERM HUP
        if [[ -n "${reporter:-}" ]]; then
            kill -TERM "$reporter" 2>/dev/null || true
            wait "$reporter" 2>/dev/null || true
        fi
        if [[ -f "$directory/visible" ]]; then printf '\r\033[2K' >&"$tty_fd"; fi
        exec {tty_fd}>&-
        rm -rf "$directory"
    }
    trap _trash_progress_cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    (
        timer=''
        trap '[[ -z "$timer" ]] || kill "$timer" 2>/dev/null; exit 0' INT TERM HUP
        sleep 0.2 & timer=$!
        wait "$timer" || exit 0
        : >"$directory/visible"
        printf '\r%s…' "$label" >&"$tty_fd"
        # Wait interruptibly so fast commands never pay the display delay and
        # stopping the renderer leaves no timer processes behind.
        while true; do sleep 3600 & timer=$!; wait "$timer" || exit 0; done
    ) </dev/null >/dev/null 2>/dev/null &
    reporter=$!
    "$@"
    status=$?
    exit "$status"
)
