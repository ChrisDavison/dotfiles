#!/usr/bin/env perl

use strict;
use warnings;
use 5.26.0;
use Scalar::Util qw/looks_like_number/;

sub main{
    my $nargs = @ARGV;
    die ("Not enough args") unless ($nargs >= 1);
    my $file = $ARGV[0];
    open( my $fh, '<', $file ) or die "Can't read file '$file' [$!]\n";
    my @rows = <$fh>;
    my $rownum0 = $ARGV[1] // 0;
    $rownum0 = looks_like_number($rownum0) ? $rownum0 : 0;
    my $row0 = $rows[$rownum0] or die("Couldn't get row $rownum0 from $file");
    my $rowN = $rows[$#rows] or die("Couldn't get row $#rows from $file");
    chomp $row0;
    chomp $rowN;
    my $lenLongestStr = length ($#rows . "");
    printf "%$lenLongestStr"."d: %s\n", $rownum0, $row0;
    printf "%$lenLongestStr"."d: %s\n", $#rows, $rowN;
}

main() or die("main failed");
