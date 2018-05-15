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
    my $dir = $lines[0];
    chomp $dir;
    $dir =~ s/~/$ENV{HOME}/g;
    return $dir;
}

sub main {
    my $path = get_plot_path();
    my $ref_filename = "$path/reference.md";
    unlink( $ref_filename ) if (-e $ref_filename);
    my @plotdirs = glob( "$path/*" );
    @plotdirs = grep { !/TODO|README/ } @plotdirs;
    open( my $f_ref, '>', $ref_filename );
    add_metadata_to_reference({ PATH=>$path, FH=>$f_ref });
    for (@plotdirs) {    
        my $curdir = cwd;
        chdir $_;
        add_plot_to_reference({ PLOTNAME=>$_, FH=>$f_ref, PATH=>$path });    
        chdir $curdir;
    }
    close $f_ref;
    create_reference_docs({ FN_REF => $ref_filename, PATH => $path });
}

sub add_metadata_to_reference {
    my ($args) = shift @_;
    my $path = $args->{PATH};
    my $fh_ref = $args->{FH};

    open( my $f_readme, '<', "$path/README.md" );
    my @readme = <$f_readme>;
    close $f_readme;
    say $fh_ref $_ for @readme;
    say $fh_ref "\n";
    1;
}

sub create_reference_docs {
    my ($args) = shift @_;
    my $ref_filename = $args->{FN_REF};
    my $path = $args->{PATH};
    my $curdir = cwd;
    chdir $path;
    my @cmd = (
        "pandoc",
        $ref_filename,
        "--standalone",
        "--self-contained",
        "-s",
    );
    my @cmd_html = @cmd;
    my @cmd_epub = @cmd;
    my @cmd_pdf = @cmd;
    push @cmd_html, ("-c", "$ENV{HOME}/.dotfiles/css/github.css",
        "-o", "$ENV{HOME}/.test.html");
    push @cmd_epub, ("-o", "$ENV{HOME}/.test.epub");
    push @cmd_pdf, ("-o", "$ENV{HOME}/.test.pdf",
        "-V", "geometry:margin=1in");
    system( @cmd_html );
    system( @cmd_pdf );
    system( @cmd_epub );
    chdir $curdir;
    1;
}

sub add_plot_to_reference {
    my ($args) = shift @_;
    my $plotname = $args->{PLOTNAME};
    my $fh = $args->{FH};
    my $path = $args->{PATH};
    my $shortplotname = $plotname;
    $shortplotname =~ s/$path\///g;
    my @files = glob( "$plotname/*" );
    my @img_files = grep { /png|jpg|jpeg|gif|pdf/ } @files;
    my @code_files = grep { /py/ } @files;
    my @data_files = grep { /csv/ } @files;
    
    say $fh "# $shortplotname\n";
    append_images({ FH=>$fh, FILES=>\@img_files, DIR => $path });
    append_code({ FH=>$fh, FILES=>\@code_files, DIR=> $path});
    append_data_heads({ FH=>$fh, FILES=>\@data_files, DIR=> $path});
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
    my $path = $args->{DIR};
    say $f_ref "\n## Code";
    for my $codefile (@files) {
        open( my $f_code, '<', $codefile );
        my @lines = <$f_code>;
        $codefile =~ s/\n//g;
        $codefile =~ s/$path\///g;
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
    my $path = $args->{DIR};
    say $fh "\n## Data\n";
    for my $file (@files) {
        open( my $datfile, '<', $file );
        my @lines = <$datfile>;
        $file =~ s/$path\///g;
        say $fh "\n\`$file\`\n";
        for my $line (@lines[0..5]) {
            chomp $line;
            say $fh "    $line";
        }
        close $datfile;
    }
    1;
}