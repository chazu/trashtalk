#!/usr/bin/env bash
# Jcode native socket boundary: the stable v1 attach request cannot change cwd.
# Admit only an idle, exact session and verify its actual shell working directory
# before the normal API adapter may submit work. No model turn is requested here.
set -euo pipefail
exec perl -MIO::Socket::UNIX -MJSON::PP -e '
    use strict; use warnings;
    my ($socket,$session,$workspace)=@ARGV;
    my $connection=IO::Socket::UNIX->new(Type=>SOCK_STREAM,Peer=>$socket) or die "Jcode workspace socket: $!\n";
    $connection->autoflush(1);
    local $SIG{ALRM}=sub {die "Jcode workspace control timed out\n"}; alarm 30;
    sub send_request { print $connection encode_json($_[0])."\n" or die "Jcode workspace write: $!\n"; }
    sub receive_type {
        my ($type,$id)=@_;
        while (my $line=<$connection>) {
            my $event=eval {decode_json($line)};
            next unless ref($event) eq "HASH";
            die "Jcode workspace request failed: ".($event->{message}//"unknown error")."\n" if ($event->{type}//"") eq "error";
            next unless ($event->{type}//"") eq $type;
            next if defined($id) && ($event->{id}//-1)!=$id;
            return $event;
        }
        die "Jcode workspace connection closed\n";
    }
    send_request({type=>"subscribe",id=>1,target_session_id=>$session});
    send_request({type=>"state",id=>2});
    my $state=receive_type("state",2);
    die "Jcode workspace target is busy or changed\n" if $state->{is_processing} || ($state->{session_id}//"") ne $session;
    send_request({type=>"subscribe",id=>3,target_session_id=>$session,working_dir=>$workspace});
    send_request({type=>"state",id=>4});
    $state=receive_type("state",4);
    die "Jcode workspace target is busy or changed\n" if $state->{is_processing} || ($state->{session_id}//"") ne $session;
    send_request({type=>"input_shell",id=>5,command=>"pwd -P"});
    my $result=receive_type("input_shell_result",undef)->{result};
    die "Jcode did not apply the requested execution directory\n"
        unless ref($result) eq "HASH" && !$result->{failed_to_start}
            && ($result->{exit_code}//-1)==0 && ($result->{cwd}//"") eq $workspace;
    print encode_json({session_id=>$session,execution_workspace=>$workspace,verified=>JSON::PP::true})."\n";
    alarm 0;
' "$@"
