#!/usr/bin/env perl
# Filesystem boundary: identify logs, verify already-consumed bytes, and return
# only complete appended lines. JSON interpretation and presentation stay in jq.
use strict;
use warnings;
use JSON::PP qw(decode_json encode_json);
use Digest::SHA;
use Time::HiRes qw(stat);
my ($records_path, $previous_path, $root, $force, $lines_path, $manifest_path) = @ARGV;
sub json_file {
    my ($path) = @_;
    open my $fh, '<:raw', $path or die "$path: $!";
    local $/;
    return decode_json(<$fh>);
}
my $records = json_file($records_path);
my $previous = json_file($previous_path)->{files} // {};
my @runs = $root eq '' ? () : grep { /^[A-Za-z0-9_-]+$/ } @{$records->{conversation_runs} // []};
my $full = $force ? 1 : 0;
my (%files, %chunks);
sub consume {
    my ($run, $old) = @_;
    my $path = "$root/$run/conversation.jsonl";
    my @st = stat($path);
    return unless @st && -f _;
    my $meta = {dev=>$st[0], ino=>$st[1], size=>$st[7], mtime=>$st[9], ctime=>$st[10]};
    open my $fh, '<:raw', $path or die "$path: $!";
    my $sha = Digest::SHA->new(256);
    my $offset = 0;
    my $line = 0;
    my $reset = $old ? 1 : 0;
    if ($old && $old->{dev} == $meta->{dev} && $old->{ino} == $meta->{ino} && $old->{offset} <= $meta->{size}) {
        my $left = $old->{offset};
        while ($left > 0) {
            my $n = read($fh, my $buf, $left < 65536 ? $left : 65536);
            die "$path: read: $!" unless defined $n;
            last unless $n;
            $sha->add($buf); $left -= $n;
        }
        if (!$left && $sha->clone->hexdigest eq $old->{hash}) {
            $offset = $old->{offset}; $line = $old->{line}; $reset = 0;
        }
    }
    if (!$offset) { seek($fh, 0, 0) or die "$path: seek: $!"; $sha = Digest::SHA->new(256); }
    my $tail = '';
    my $left = $meta->{size} - $offset;
    while ($left > 0) {
        my $n = read($fh, my $buf, $left < 65536 ? $left : 65536);
        die "$path: read: $!" unless defined $n;
        last unless $n;
        $tail .= $buf; $left -= $n;
    }
    close $fh;
    # An unterminated record is retried from its starting byte next time.
    my $complete = substr($tail, 0, rindex($tail, "\n") + 1);
    $sha->add($complete);
    my $chunk = '';
    my @complete_lines = split /\n/, $complete, -1;
    pop @complete_lines;
    for my $raw (@complete_lines) {
        $chunk .= "$run\t" . (++$line) . "\t$raw\n";
    }
    $meta->{offset} = $offset + rindex($tail, "\n") + 1;
    $meta->{line} = $line;
    $meta->{hash} = $sha->hexdigest;
    return ($meta, $chunk, $reset);
}
for my $run (@runs) {
    my ($meta, $chunk, $reset) = consume($run, $force ? undef : $previous->{$run});
    next unless $meta;
    $files{$run} = $meta; $chunks{$run} = $chunk;
    $full ||= $reset;
}
$full ||= !!grep { !exists $files{$_} } keys %$previous;
if ($full && !$force) {
    %files = (); %chunks = ();
    for my $run (@runs) {
        my ($meta, $chunk) = consume($run, undef);
        next unless $meta;
        $files{$run} = $meta; $chunks{$run} = $chunk;
    }
}
open my $lines, '>:raw', $lines_path or die "$lines_path: $!";
print $lines $chunks{$_} // '' for @runs;
close $lines or die "$lines_path: $!";
open my $manifest, '>:raw', $manifest_path or die "$manifest_path: $!";
print $manifest encode_json({full=>$full ? JSON::PP::true : JSON::PP::false, files=>\%files});
close $manifest or die "$manifest_path: $!";
