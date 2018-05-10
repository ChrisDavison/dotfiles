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

    for (@taskfiles) {
        my $fn = $_;
        $fn =~ s/~/$ENV{HOME}/g;
        open( my $fh_task, '<', $fn );
        my @lines = <$fh_task>;
        close $fh_task;
        my @tasks = grep { /\{#[a-z]+-[a-z]+\}/ } @lines;
        if( $filter ) {
            @tasks = grep { /$filter/i } @tasks
        }
        for( @tasks ) {
            $_ =~ s/^#+ //g;
            $_ =~ s/^\*\*(.*)\*\*/$1 :: /g;
            print $_;
        };
    }
}
