#!/usr/bin/env bash
# OS boundary: Bash cannot restore signals ignored when it started. Reset
# interrupt dispositions before exec so supervised children remain stoppable.
# Preserve HUP: detached callers deliberately ignore it to survive disconnects.
exec perl -e '
    $SIG{INT} = "DEFAULT";
    $SIG{QUIT} = "DEFAULT";
    $SIG{TERM} = "DEFAULT";
    exec @ARGV;
    die "exec interruptible process: $!";
' "$@"
