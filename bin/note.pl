#!/usr/bin/env perl

use strict;
use warnings;
use Switch;
use Modern::Perl;

sub main {
    my $cmd = $ARGV[0] // "";
    switch ($cmd) {
        case /add|a/ { add() or die }
        case /view|v/ { view() or die }
        case /clear|c/ { clear() or die }
        case /delete|d/ { del() or die }
        else { usage() }
    }
}

sub usage {
    say "usage: $0 CMD [options]";
}

sub add {
    say "add";
}

sub view {
    my $todofile = $ENV{"EXTERNAL_BRAIN"};
    if (! -e $todofile) {
        say "No capture file defined in env.";
        exit 1;
    }
    say "view";
}

sub clear {
    say "clear";
}

sub del {
    say "delete";
}

main();
