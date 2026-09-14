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

# Same contract, but queue behind the holder for up to $2 whole seconds. A
# user control operation must not lose a race against back-to-back worker
# ticks; the kernel grants the lock the moment the tick releases it. Timing
# out prints nothing and exits 0, exactly like a busy nonblocking attempt.
_trash_with_process_lock_wait() (
    local TRASH_PROCESS_LOCK_FD status
    [[ "$2" =~ ^[0-9]{1,4}$ ]] || { echo 'Lock wait must be whole seconds' >&2; exit 1; }
    exec {TRASH_PROCESS_LOCK_FD}>"$1" || exit 1
    perl -e '$SIG{ALRM} = sub { exit 75 }; alarm $ARGV[0]; exit 0 if flock(STDIN, 2); exit 1' "$2" <&"$TRASH_PROCESS_LOCK_FD"
    status=$?
    [[ "$status" != 75 ]] || exit 0
    [[ "$status" == 0 ]] || { echo 'Unable to lock worker store' >&2; exit 1; }
    @ "$3" "$4" "$5"
)

_trash_close_process_lock() {
    [[ -z "${TRASH_PROCESS_LOCK_FD:-}" ]] || exec {TRASH_PROCESS_LOCK_FD}>&-
}
