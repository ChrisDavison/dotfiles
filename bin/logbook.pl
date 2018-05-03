#!/usr/bin/env perl

use 5.24.0;
use strict;
use warnings;
use Git::Repository;

my $cmd = shift;
for ($cmd) {
    when (/edit|e/)     { edit() or die; }
    when (/view|v/)     { view() or die; }
    when (/diff|d/)     { diff() or die; }
    when (/commit|c/)   { commit() or die; }
    when (/history|h/)  { history() or die; }
    when (/previous|p/) { previous() or die; }
    default             { usage() }
}

sub usage {
    say "usage: $0 CMD [OPTIONS]";
}

sub logbook_entry {
    my @time = localtime;
    my $year = $time[5] + 1900;
    my $mon  = $time[4] + 1;
    $mon = "0" . $mon if $mon < 10;
    my @abbr = qw/Mon Tue Wed Thu Fri Sat Sun/;
    my $day  = $time[3];
    $day = "0" . $day if $day < 10;
    my $wday = $abbr[ $time[6] ];
    my $dir  = $ENV{"LOGBOOK_DIR"};
    return "$dir$year/$year-$mon-$day--$wday.md";
}

sub edit {
    system( ( $ENV{"EDITOR"}, logbook_entry() ) )
      or die("Failed to edit logbook");
}

sub view {
    my $file = logbook_entry();
    if ( -e $file ) {
        system( ( "less", $file ) );
    }
    else {
        say "No logbook entry today";
    }
}

sub previous {
    ...
}

sub diff {
    ...
}

sub commit {
    ...
}

sub history {
    ...
}
