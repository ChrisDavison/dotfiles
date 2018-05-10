#!/usr/bin/env perl

use strict;
use warnings;
use 5.24.0;
use autodie;
use Cwd 'abs_path';

main();

sub main {
    my $fn = "$ENV{HOME}/.taskrc";
    open( my $fh, '<', $fn );
    my @taskfiles = <$fh>;
    close $fh;

    my $filter = shift @ARGV;

    for my $taskfile (@taskfiles) {
        chomp $taskfile;
        my @tasks = search_for_tasks($taskfile);
        if( $filter ) {
            @tasks = grep { /$filter/i } @tasks
        }
        my $prefix = "$taskfile: ";
        $prefix = "" if( $#taskfiles == 0 );
        for( @tasks ) {
            $_ =~ s/^#+ //g;
            $_ =~ s/^\*\*(.*)\*\*/$1 :: /g;
            print "$prefix$_";
        };
    }
}

sub search_for_tasks {
    my $fn = shift @_;
    # Replace '~' with actual home directory
    $fn =~ s/~/$ENV{HOME}/g;
    # Read all lines
    open( my $fh_task, '<', $fn );
    my @lines = <$fh_task>;
    close $fh_task;
    # Filter only those matching {#WORD-WORD}
    my @tasks = grep { /\{#[a-z]+-[a-z]+\}/ } @lines;
    return @tasks;
}