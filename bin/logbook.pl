#!/usr/bin/env perl

use 5.24.0;
use strict;
use warnings;
use Git::Repository;
use experimental qw/smartmatch/;
use File::Basename;
use DateTime::Format::Strptime qw();
use Cwd;

my $p = DateTime::Format::Strptime->new(pattern => "%F", on_error => 'croak');
my $logbook = logbook_entry();
my (undef, $dir) = fileparse($logbook);
my @files = glob( "$dir*.md" );

main();

sub main {
    my $cmd = shift @ARGV // "";
    for ($cmd) {
        when (/edit|e/)     { edit() or die; }
        when (/view|v/)     { view() or die; }
        when (/commit|c/)   { commit(@ARGV) or die; }
        when (/history|h/)  { history(@ARGV) or die; }
        when (/previous|p/) { previous() or die; }
        default             { usage() }
    }
}

sub usage {
    say "usage: $0 CMD [OPTIONS]";
    say "";
    say "Commands";
    say "    e,edit";
    say "    v,view";
    say "    c,commit";
    say "    h,history";
    say "    p,previous";
    exit 1;
}

sub logbook_entry {
    my @time = localtime;
    my $year = $time[5] + 1900;
    my $mon  = $time[4] + 1;
    $mon = "0" . $mon if $mon < 10;
    my @abbr = qw/Mon Tue Wed Thu Fri Sat Sun/;
    my $day  = $time[3];
    $day = "0" . $day if $day < 10;
    my $wday = $abbr[ $time[6]-1 ];
    my $dir  = $ENV{"LOGBOOK_DIR"};
    return "$dir$year/$year-$mon-$day--$wday.md";
}

sub edit {
    die ("No \$EDITOR defined") unless $ENV{"EDITOR"};
    system( ( $ENV{"EDITOR"}, $logbook ) ) == 0
      or die("Failed to edit logbook");
    exit 1;
}

sub view {
    if (-e $logbook) {
        system( ( "less", $logbook ) ) == 0 or die("Failed to view logbook");
    } else {
        say "No logbook entry today";
    }
    exit 1;
}

sub previous {
    my $n = (-e $logbook) ? 1 : 0;
    my $prev = $files[$#files-$n];
    system( ("less", $prev) ) == 0 or die("Failed to view previous logbooks");
}

sub commit {
    ...
    # msg="$1"
    # pushd "$LOGBOOK_DIR" || exit
    # git add "$logbook_today"
    # if [ "$msg" = "" ]; then
    #     git commit -m "Log $(date +%F)"
    # elif [ "$msg" = "i" ]; then
    #     git commit
    # else
    #     git commit -m "Log $msg"
    # fi 
    # popd || exit ;;
}

sub time_today {
    my (undef, undef, undef, $dd, $mm, $yy, @rest) = localtime();
    my $year = $yy + 1900;
    my $month = $mm + 1;
    return $p->parse_datetime("$year-$month-$dd");
}

sub datetime_from_filename {
    my $filename = shift;
    my ($parsed_filename) = $filename =~ m/.*(\d\d\d\d-\d\d-\d\d)--....md/g;
    return $p->parse_datetime($parsed_filename);
}

sub history {
    my $step = shift @_ // 7;
    my $fn_out = "$ENV{HOME}/.logbook_history";
    unlink $fn_out;
    open( my $fh_history, '>', $fn_out);
    for (@files[$#files-$step..$#files]){
        open (my $hist_file, '<', $_);
        my @contents = <$hist_file>;
        close( $hist_file );
        say $fh_history @contents;
    }
    close $fh_history;
    my @pd_cmd = ("pandoc", $fn_out, "-f", "gfm", "--mathjax", 
        "-M", "title=logbook", "--standalone", "--self-contained", "-o");
    my @cmd_epub = @pd_cmd;
    my @cmd_html = @pd_cmd;
    push @cmd_epub, ("$fn_out.epub");
    push @cmd_html, ("$fn_out.html", "--toc", "--toc-depth=2");
    my $css_fn = "$ENV{HOME}/.dotfiles/css/github.css";
    push(@cmd_html, ("-c", $css_fn)) if -e $css_fn;
    my $curdir = cwd;
    chdir $dir;
    system( @cmd_epub ) == 0 or die("Failed to create epub");
    system( @cmd_html ) == 0 or die("Failed to create html");
    chdir $curdir;
    exit 1;
}
