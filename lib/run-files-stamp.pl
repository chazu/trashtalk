# Size/mtime stamp of every file under the given run directories, sorted.
# Usage: perl run-files-stamp.pl <run-root> <run-id>...
use strict; use warnings;
my $base = shift @ARGV;
for my $run (@ARGV) {
    for my $path (sort glob("$base/$run/*")) {
        my @s = stat $path;
        next unless @s;
        print "$path $s[7] $s[9]\n";
    }
}
