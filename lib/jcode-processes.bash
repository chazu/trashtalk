#!/usr/bin/env bash
# OS boundary for Jcode's reload-persistent Bash jobs. Native cancel does not
# kill these jobs. Admission and stop share a flock; each admitted tool starts
# in its own process group and records its PID birth before executing Bash.
set -euo pipefail
mode=$1 host=$2
shift 2
if [[ "$mode" == shell ]]; then
    exec perl -MPOSIX -MFcntl=:flock -e '
        my ($home,$bash,@args)=@ARGV;
        open my $lock, ">>", "$home/process.lock" or die $!;
        flock($lock, LOCK_EX) or die $!;
        exit 125 if -e "$home/stopping";
        POSIX::setsid() >= 0 or die "setsid: $!" if getpgrp(0) != $$;
        my $birth = qx(/bin/ps -p $$ -o lstart=); $birth =~ s/^\s+|\s+$//g;
        die "Cannot identify Bash process" unless length $birth;
        open my $record, ">", "$home/processes/$$" or die $!;
        print $record "$birth\n"; close $record;
        close $lock;
        exec {$bash} $bash,@args; die "exec Bash: $!";
    ' "$host" "$@"
fi
[[ "$mode" == stop ]] || exit 2
exec perl -MFcntl=:flock -MTime::HiRes=sleep -e '
    my $home=shift;
    open my $lock, ">>", "$home/process.lock" or die $!;
    flock($lock, LOCK_EX) or die $!;
    open my $gate, ">", "$home/stopping" or die $!; close $gate;
    sub processes {
        my %p;
        for (qx(/bin/ps -axo pid=,pgid=,lstart=,stat=)) {
            next unless /^\s*(\d+)\s+(\d+)\s+(.{24})\s+(\S+)/;
            $p{$1}=[$2,$3,$4] unless $4 =~ /Z/;
        }
        return %p;
    }
    my %snapshot=processes(); my @groups; my $uncertain=0;
    opendir my $dir, "$home/processes" or die $!;
    for my $pid (readdir $dir) {
        next unless $pid =~ /^[0-9]+$/; my $file="$home/processes/$pid";
        open my $f, "<", $file or die $!; chomp(my $birth=<$f>); close $f;
        if (exists $snapshot{$pid} && $snapshot{$pid}[0]==$pid && $snapshot{$pid}[1] eq $birth) {
            push @groups, $pid;
        } elsif (grep { $_->[0]==$pid } values %snapshot) {
            warn "Cannot establish ownership of remaining tool group $pid\n"; $uncertain=1;
        } else { unlink $file; }
    }
    closedir $dir;
    # The live group leader and its birth identify each admitted tool group.
    # SIGKILL terminates its descendants too, including foreground sleeps.
    for my $pid (@groups) {
        my %now=processes();
        if (exists $now{$pid} && $now{$pid}[0]==$pid && $now{$pid}[1] eq $snapshot{$pid}[1]) {
            kill "KILL", -$pid or die "Cannot stop tool group $pid: $!";
        } elsif (grep { $_->[0]==$pid } values %now) {
            warn "Tool group ownership changed before stop: $pid\n"; $uncertain=1;
        }
    }
    close $lock;
    for (1..50) {
        my %now=processes(); my $live=grep { my $g=$_; grep { $_->[0]==$g } values %now } @groups;
        exit($uncertain ? 1 : 0) unless $live;
        sleep .1;
    }
    die "Jcode tool processes remain after stop\n";
' "$host"
