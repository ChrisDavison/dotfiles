#!/usr/bin/env perl

use 5.24.0;
use strict;
use warnings;
use experimental qw/smartmatch/;
use File::Basename;
use Cwd 'abs_path';

my (undef, $script_dir) = fileparse(abs_path($0));
my $fn_ani= $script_dir . 'animals.txt';
my $fn_adj = $script_dir . 'adjectives.txt';

my $cmd = shift // "";

for ( $cmd ) {
    when ("")   { say get_codeword(); }
    when (/-c/) { say count_permutations(); }
    default { usage() }
}

sub count_permutations {
    my $poss = read_file_lines($fn_ani) * read_file_lines($fn_adj);
    return "Combinations: $poss"; 
}

sub usage {
    say "usage: $0 [-c|-h]";
    say "";
    say "Generate a docker-esque <adjective>-<animal> phrase";
    say "-c to get a count of possible combinations";
    say "-h|--help|help to display this message";
}

sub get_codeword {
    my $animal = random_line_from_file($fn_ani);
    my $adjective = random_line_from_file($fn_adj);
    return "$adjective-$animal";
}

sub read_file_lines {
    my $filename = shift;
    open( my $fh, '<', $filename );
    my @lines = <$fh>;
    close $fh;
    return @lines;
}

sub random_line_from_file {
    my $filename = shift;
    my @lines = read_file_lines($filename);
    my $num = int(rand(@lines));
    my $word = $lines[$num];
    chomp $word;
    return $word;
}
