#!/usr/bin/env perl

use strict;
use warnings;
use 5.26.0;
use Scalar::Util qw/looks_like_number/;

sub main {
    # Read in arguments
    my $nargs = @ARGV;
    usage() unless ($#ARGV >= 0);
    my $file = $ARGV[0];
    my $rownum0 = $ARGV[1] // 0;  # Use first row (idx 0) if no row is given

    my @rows = lines_from_file($file); 
    $rownum0 = looks_like_number($rownum0) ? $rownum0 : 0;
    my $row0 = $rows[$rownum0] or die("Couldn't get row $rownum0 from $file");
    my $rowN = $rows[$#rows] or die("Couldn't get row $#rows from $file");
    chomp $row0;
    chomp $rowN;
    my $lenLongestStr = length ($#rows . "");
    printf "%$lenLongestStr"."d: %s\n", $rownum0, $row0;
    printf "%$lenLongestStr"."d: %s\n", $#rows, $rowN;
    1;
}

sub lines_from_file {
    my $file = shift;
    open( my $fh, '<', $file ) or die "Can't read file '$file' [$!]\n";
    my @rows = <$fh>;
    close $fh;
    return @rows;
}

sub usage {
    say "usage: $0 FILE [N]";
    say "    Print the Nth and last row from FILE";
    say "    displaying line number and file contents";
    exit 1;
}

main() or die("main failed");
