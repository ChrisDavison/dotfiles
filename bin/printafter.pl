#!/usr/bin/env perl
use strict;
use warnings;

my $filename = $ARGV[0];
my $date = $ARGV[1];
open my $fh, $filename or die "Couldn't open $filename: $!";
my $print = 0;
while( my $line = <$fh> ){
    if (!$print && $line =~ m/^$date/) {
        $print = 1;
    }
    print $line if ($print == 1);
}