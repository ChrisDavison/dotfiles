#!/usr/bin/env perl

use 5.24.0;
use strict;
use warnings;
use File::Basename;
use Cwd 'abs_path';

my (undef, $script_dir) = fileparse(abs_path($0));
my $fn_ani= $script_dir . 'animals.txt';
my $fn_adj = $script_dir . 'adjectives.txt';
my $animal = random_line_from_file($fn_ani);
my $adjective = random_line_from_file($fn_adj);
say "$adjective-$animal";

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
