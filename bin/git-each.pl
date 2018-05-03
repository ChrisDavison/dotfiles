#!/usr/bin/env perl

use strict;
use warnings;
use 5.24.0;
use Git::Repository;

my $file = $ENV{"HOME"} . "/.repos";
open(my $fh, '<', $file);
my @repos = <$fh>;
close $fh;

my $first_repo = $repos[0];
chomp $first_repo;

for my $repo (@repos) {
    chomp $repo;
    my $st = check_status($repo);
    say $repo, $st;
}

sub check_status {
    my $repo = $_[0];
    my $r = Git::Repository->new( work_tree => $repo );
    my @output = $r->run( 'status' );
    return "clean" if grep { /nothing to commit/ } @output;
}
