# OS-only helper: the raw compiler does not preserve Bash {fd} redirections.
# Keep dynamic descriptor allocation here, not in domain orchestration.
_trash_with_process_lock() (
    local TRASH_PROCESS_LOCK_FD status
    exec {TRASH_PROCESS_LOCK_FD}>"$1" || exit 1
    perl -MErrno=EWOULDBLOCK,EAGAIN -e 'exit 0 if flock(STDIN, 6); exit(($! == EWOULDBLOCK || $! == EAGAIN) ? 75 : 1)' <&"$TRASH_PROCESS_LOCK_FD"
    status=$?
    [[ "$status" != 75 ]] || exit 0
    [[ "$status" == 0 ]] || { echo 'Unable to lock worker store' >&2; exit 1; }
    @ "$2" "$3" "$4"
)

_trash_close_process_lock() {
    [[ -z "${TRASH_PROCESS_LOCK_FD:-}" ]] || exec {TRASH_PROCESS_LOCK_FD}>&-
}
