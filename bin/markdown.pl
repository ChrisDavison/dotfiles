#!/usr/bin/env perl

use strict;
use warnings;
use 5.24.0;
use Switch;
use Cwd;

main;

sub main {
    my @args = @ARGV;
    my $cmd = shift @args;
    my $fn = shift @args;
    switch ($cmd) {
        case /tidy/ { tidy(1,0,$fn) or die }
        case /tidywrap/ { tidy(1,1,$fn) or die }
        case /tidynoref/ { tidy(0,0,$fn) or die }
        case /tidynorefwrap/ { tidy(0,1,$fn) or die }
        case /pdf/ { to_pdf($fn) or die }
        case /html/ { to_html($fn) or die }
    }
}

sub tidy {
    my ($use_ref, $wrap, $fn) = @_;
    my $fmt = "markdown_github-hard_line_breaks+yaml_metadata_block"
    $fmt = $fmt . "+tex_math-dollars+line_blocks"
    my @cmd=("-t", $fmt, "-s", "--atx-headers", "--normalize", $fn, "-o", $fn);
    push @cmd, "--columns=80" if $wrap;
    push @cmd, "--reference-links" if $use_ref;
    system( @cmd ) == 0 or die ("Failed to run command");
    1;
}

sub to_pdf {
    my $fn = shift @_; 
}

sub to_html {
    my $fn = shift @_; 
}
