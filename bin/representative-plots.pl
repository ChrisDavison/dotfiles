#!/usr/bin/env perl

use strict;
use warnings;
use autodie;
use 5.24.0;
use Cwd qw/abs_path cwd/;
use Data::Dumper;

main();

sub get_plot_path {
    my $config = "$ENV{HOME}/.rplotrc";
    open( my $f_config, '<', $config);
    my @lines = <$f_config>;
    close $f_config;
    return $lines[0];
}

sub main {
    my $path = get_plot_path();
    my $ref_filename = "$path/reference.md";
    unlink( $ref_filename ) if (-e $ref_filename);
    my @plotdirs = glob( "$path/*" );
    open( my $f_ref, '>', $ref_filename );
    for (@plotdirs) {    
        my $curdir = cwd;
        chdir $_;
        add_representative_plot_to_reference({
            PLOTNAME=>$_, 
            FH=>$f_ref,
            PATH=>$path
        });    
        chdir $curdir;
    }
    close $f_ref;
}

sub add_representative_plot_to_reference {
    my ($args) = shift @_;
    my $plotname = $args->{PLOTNAME};
    my $fh = $args->{FH};
    my $path = $args->{PATH};
    $plotname =~ s/$path\///g;
    say $plotname;
    my @files = glob( "$plotname/*" );
    my @img_files = grep { /png|jpg|jpeg|gif|pdf/ } @files;
    my @code_files = grep { /py/ } @files;
    my @data_files = grep { /csv/ } @files;
    
    say $fh "# $plotname\n";
    append_images({ FH=>$fh, FILES=>\@img_files, DIR => $path });
    append_code({ FH=>$fh, FILES=>\@code_files});
    append_data_heads({ FH=>$fh, FILES=>\@data_files});
    1;
}

sub append_images {
    my ($args) = @_;
    my $path = $args->{DIR};
    my $f_ref = $args->{FH};
    my @files = @{$args->{FILES}};
    my @as_img_link = map {
        my $relative_path = $_;
        $relative_path =~ s/$path\///g;
        "![](./$relative_path)"
    } @files;
    say $f_ref "## Images\n";
    say $f_ref $_ for (@as_img_link);
    1;
}

sub append_code {
    my ($args) = @_;
    my $f_ref = $args->{FH};
    my @files = @{$args->{FILES}};
    say $f_ref "\n## Code";
    for my $codefile (@files) {
        open( my $f_code, '<', $codefile );
        my @lines = <$f_code>;
        $codefile =~ s/\n//g;
        say $f_ref "\nFrom: $codefile";
        say $f_ref "\n```python";
        print $f_ref $_ for @lines;
        say $f_ref "\n```";
        close $f_code;
    }
    1;
}

sub append_data_heads {
    my ($args) = @_;
    my $fh = $args->{FH};
    my @files = @{$args->{FILES}};
    say $fh "\n## Data\n";
    for my $file (@files) {
        open( my $datfile, '<', $file );
        my @lines = <$datfile>;
        say $fh "\n\`$file\`\n";
        for my $line (@lines[0..5]) {
            chomp $line;
            say $fh "    $line";
        }
        close $datfile;
    }
    1;
}