#!/usr/bin/env perl

use 5.24.0;
use strict;
use warnings;
use Switch;
use File::Touch;
use POSIX qw/strftime/;

my $filename = $ENV{"EXTERNAL_BRAIN"};

sub main {
    my $cmd = $ARGV[0] // "";
    my @args = @ARGV[1..$#ARGV];
    switch ($cmd) {
        case /add|a/    { add(@args) or die }
        case /view|v/   { view(@args) or die }
        case /clear|c/  { clear(@args) or die }
        case /delete|d/ { del(@args) or die }
        else            { usage(@args) }
    }
}

sub usage {
    say "usage: $0 CMD [options]";
    say;
    say "Options";
    say "    v,view             View all notes";
    say "    c,clear            Clear all notes";
    say "    a,add NOTE         Add a note";
    say "    d,delete ITEM#     Delete a note";
}

sub add {
    my $date = strftime("%Y%m%d", gmtime);
    my $msg = join(' ',@_);
    open( my $fh, '>>', $filename);
    say $fh "$date $msg";
    say "$date $msg";
    return 1;
}

sub view {
    die ("No capture file defined") unless (-e $filename);
    my @lines = read_file();
    my $i = 1;
    for (@lines) {
        printf "%3d %s", $i, $_;
        $i += 1;
    }
    return 1;
}

sub clear {
    unlink($filename);
    open( my $fh, '>', $filename);
    close $fh;
    say "$filename cleared";
    return 1;
}

sub del {
    my $itemnum = $_[0];
    my @lines = read_file();
    die ("Invalid ITEM# $itemnum") unless ($itemnum > 0 && $itemnum-1 <= $#lines);
    my $deleting = $lines[$itemnum-1];
    chomp $deleting;
    splice(@lines, $itemnum-1, 1);
    my $response = unbuffered_prompt("Delete: $deleting? (y|n) ");
    return if ($response =~ /n|N/ );
    open( my $fh, '>', $filename);
    print $fh @lines;
    close $fh;
    return 1;
}

sub unbuffered_prompt {
    my $msg = shift;
    $| = 1; 
    print "$msg: ";
    chomp(my $response = <STDIN>);
    $| = 0; 
    return $response;
}

sub read_file {
    open( my $fh, '<', $filename);
    my @lines = <$fh>;
    close $fh;
    return @lines;
}

main();
