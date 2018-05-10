#!/usr/bin/env perl

use strict;
use warnings;
use Switch;
use 5.24.0;
use POSIX qw/strftime/;

my $dir = $ENV{"CAPTUREDIR"};

$SIG{INT} = \&save_to_file;

my @lines = ();

sub main {
    my $cmd = $ARGV[0] // "";
    my @args = @ARGV[1..$#ARGV];
    switch ($cmd) {
        case /list|l/ { list(@args) or die }
        case /del|d/  { del(@args) or die }
        case /view|v/ { view(@args) or die }
        case /h|help|u|usage/ { usage() or die }
        case /add|a/  { take_note() or die }
        else          { take_note() }
    }
    1;
}

sub usage {
    say "usage: $0 [CMD] [options]";
    say "";
    say "Commands:";
    say "    v,view             View all notes";
    say "    l,list             List notes";
    say "    a,add              Add a note (DEFAULT command)";
    say "    d,delete ITEM#     Delete a note";
    say "";
    say "While adding notes, Ctrl-C saves and exits";
    1;
}

sub take_note {
    say "Taking note";
    say "Ctrl-C to save and exit, Ctrl-D to exit without saving";
    say "";
    while (<>) {
        push @lines, $_;
    }
    1;
}

sub del {
    my $item = shift @_;
    my @files = glob( "$dir*" );
    die ("Bad ITEM#") unless (0 < $item and $item-1 <= $#files);
    my $del_line = $files[$item-1];
    my $response = unbuffered_prompt("Delete '$del_line'? (y|n)");
    return 1 if ($response =~ /n|N/);
    unlink($del_line);
    1;
}

sub unbuffered_prompt {
    my $msg = shift;
    $| = 1; 
    print "$msg: ";
    chomp(my $response = <STDIN>);
    $| = 0; 
    return $response;
}

sub list {
    my $filter = shift @_;
    my @files = glob( "$dir*" );
    my $i = 1;
    my @matching = grep { /$filter/ } @files;
    for (@matching) {
        printf "%3d %s\n", $i, $_;
        $i += 1;
    }
    1;
}

sub save_to_file {
    my $date = strftime("%Y-%m-%d", gmtime);
    my $first = $lines[0];
    die ("No content to write to file") unless $#lines > 0;
    chomp $first;
    $first =~ s/[^a-z0-9A-Z]/-/g;
    $first =~ s/-+$//g;
    $first = lc $first;
    my $title = "$date--$first.txt";
    my $filename = $dir . $title;
    shift @lines;
    say $filename;
    open ( my $fh, '>', $filename );
    print $fh $_ for @lines;
    close $fh;
    exit 0;
}

sub view {
    my $item = shift @_;
    my @files = glob( "$dir*" );
    die ("Bad ITEM#") unless (0 < $item and $item-1 <= $#files);
    open( my $fh, '<', $files[$item-1] );
    print for <$fh>;
    close $fh;
    1;
}

main;
