#!/usr/bin/env perl
use strict;
use warnings;

my $filename = $ARGV[0];
my $date = $ARGV[1];
print $filename, "\n";
print $date, "\n";
open my $fh, $filename or die "Couldn't open $filename: $!";
while( my $line = <$fh> ){
    last if $line =~ m/^$date/;
    print $line;
}