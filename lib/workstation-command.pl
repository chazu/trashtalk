# Narrow POSIX process boundary: exact exec argv, wait status (including signal),
# inherited terminal/stdin, bounded optional pipe capture. Domain publication is
# a public Trashtalk message in trash-receipt, after the child is reaped.
use strict;
use warnings;
use Cwd qw(abs_path);
use POSIX qw(:sys_wait_h strftime);
use JSON::PP;
use IO::Select;
use Encode qw(decode FB_DEFAULT);
use Errno qw(EINTR);
my $root = shift @ARGV;
my ($cwd, $label, $capture) = ('', '', 0);
while (@ARGV && $ARGV[0] ne '--') {
    my $arg = shift @ARGV;
    if ($arg eq '--cwd' && @ARGV) { $cwd = shift @ARGV }
    elsif ($arg eq '--label' && @ARGV) { $label = shift @ARGV }
    elsif ($arg eq '--capture') { $capture = 1 }
    else { die "Usage: trash-command --cwd DIR --label SAFE-LABEL [--capture] -- PROGRAM [ARG ...]\n" }
}
die "A working directory, label and exact command argv are required\n"
    unless @ARGV && shift(@ARGV) eq '--' && @ARGV && length($cwd) && length($label);
$cwd = abs_path($cwd) // die "Working directory unavailable\n";
# Reject unsafe receipt metadata before running anything. Never include argv in diagnostics.
die "Invalid working directory or safe label\n" if !-d $cwd || length($cwd)>256 || length($label)>256
    || "$cwd$label" =~ /[\x00-\x1f\x7f]/;
sub timestamp { strftime('%Y-%m-%dT%H:%M:%SZ', gmtime) }
my $started = timestamp();
my (@pipes, $select);
if ($capture) {
    $select = IO::Select->new;
    for (1..2) { pipe(my $r, my $w) or die "Cannot create capture pipe\n"; push @pipes, [$r,$w]; $select->add($r) }
}
my $pid = fork();
defined $pid or die "Cannot start command\n";
if (!$pid) {
    if ($capture) {
        close $_->[0] for @pipes;
        open STDOUT, '>&', $pipes[0][1] or POSIX::_exit(126);
        open STDERR, '>&', $pipes[1][1] or POSIX::_exit(126);
        close $_->[1] for @pipes;
    }
    chdir $cwd or POSIX::_exit(126);
    { no warnings 'exec'; exec {$ARGV[0]} @ARGV }
    POSIX::_exit($!{ENOENT} ? 127 : 126);
}
# Signals addressed only to the supervisor reach its exact child. Terminal
# signals may already reach both in the foreground group; forwarding is harmless.
for my $sig (qw(INT TERM HUP QUIT)) { $SIG{$sig} = sub { kill $sig, $pid } }
my ($bytes, $status) = (0, undef);
$SIG{PIPE} = 'IGNORE';
if ($capture) {
    close $_->[1] for @pipes;
    my $stdout_fd = fileno($pipes[0][0]);
    my $drains = 0;
    while (1) {
        my @ready = $select->can_read(.05);
        last if defined($status) && (!@ready || ++$drains > 32);
        for my $fh (@ready) {
            my $n = sysread($fh, my $chunk, 8192);
            next if !defined($n) && $! == EINTR;
            if (!$n) { $select->remove($fh); close $fh; next }
            $bytes += $n; $bytes = 4097 if $bytes > 4097;
            my $out = fileno($fh) == $stdout_fd ? \*STDOUT : \*STDERR;
            print $out $chunk;
        }
        next if defined $status;
        my $done = waitpid($pid, WNOHANG);
        if ($done == $pid) { $status = $?; next }
        die "Cannot wait for command\n" if $done == -1 && $! != EINTR;
    }
    # Do not wait forever for a detached descendant that inherited a pipe.
    close $_->[0] for grep { defined fileno($_->[0]) } @pipes;
} else {
    while (1) { my $done=waitpid($pid,0); if ($done==$pid) { $status=$?; last } next if $! == EINTR; die "Cannot wait for command\n" }
}
$SIG{$_} = 'IGNORE' for qw(INT TERM HUP QUIT);
my $signal = $status & 127;
my $exit = $signal ? 128+$signal : $status >> 8;
my $summary = "Command exited with status $exit";
if ($capture) {
    # Deliberately conservative privacy policy: captured content is never sent
    # to persistence. Only its bounded byte count is projected. This redacts
    # arbitrary secrets, not merely credentials matching known token patterns.
    $summary .= "; output redacted (" . ($bytes > 4096 ? '4096+' : $bytes) . " bytes)";
}
my $outcome = JSON::PP->new->canonical->encode({workspace=>decode('UTF-8',$cwd,FB_DEFAULT),
    commandLabel=>decode('UTF-8',$label,FB_DEFAULT), exitCode=>$exit,
    startedAt=>$started, finishedAt=>timestamp(), summary=>$summary});
# Publication gets no child stdin and cannot change the command's outcome.
# Avoid shell interpolation and never pass command argv or captured output.
my $publisher = fork();
if (defined($publisher) && !$publisher) {
    open STDIN, '<', '/dev/null';
    open STDOUT, '>', '/dev/null';
    $ENV{TRASHTALK_DIR}=$root;
    { no warnings 'exec'; exec {'bash'} 'bash', "$root/bin/trash-receipt", '--outcome', $outcome }
    POSIX::_exit(1);
}
my $published = 0;
if (defined $publisher) {
    while (1) { my $done=waitpid($publisher,0); if ($done==$publisher) { $published=($?==0); last } next if $! == EINTR; last }
}
print STDERR "Command receipt publication failed; child outcome is unchanged\n" unless $published;
if ($signal) { $SIG{$_}='DEFAULT' for qw(INT TERM HUP QUIT PIPE); kill $signal, $$; POSIX::_exit(128+$signal) }
exit $exit;
