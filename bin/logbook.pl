#!/usr/bin/env perl

use 5.24.0;
use strict;
use warnings;
use Git::Repository;
use experimental qw/smartmatch/;
use File::Basename;
use DateTime::Format::Strptime qw();
use autodie;

my $p = DateTime::Format::Strptime->new(pattern => "%F", on_error => 'croak');
my $logbook = logbook_entry();
my (undef, $dir) = fileparse($logbook);
my @files = glob( "$dir*.md" );

main();

sub main {
    my $cmd = $ARGV[0];
    for ($cmd) {
        when (/edit|e/)     { edit() or die; }
        when (/view|v/)     { view() or die; }
        when (/commit|c/)   { commit() or die; }
        when (/history|h/)  { history() or die; }
        when (/previous|p/) { previous() or die; }
        default             { usage() }
    }
}

sub usage {
    say "usage: $0 CMD [OPTIONS]";
}

sub logbook_entry {
    my @time = localtime;
    my $year = $time[5] + 1900;
    my $mon  = $time[4] + 1;
    $mon = "0" . $mon if $mon < 10;
    my @abbr = qw/Mon Tue Wed Thu Fri Sat Sun/;
    my $day  = $time[3];
    $day = "0" . $day if $day < 10;
    my $wday = $abbr[ $time[6] ];
    my $dir  = $ENV{"LOGBOOK_DIR"};
    return "$dir$year/$year-$mon-$day--$wday.md";
}

sub edit {
    die ("No \$EDITOR defined") unless $ENV{"EDITOR"};
    system( ( $ENV{"EDITOR"}, $logbook ) )
      or die("Failed to edit logbook");
}

sub view {
    if (-e $logbook) {
        system( ( "less", $logbook ) );
    } else {
        say "No logbook entry today";
    }
}

sub previous {
    my $n = (-e $logbook) ? 1 : 0;
    my $prev = $files[$#files-$n];
    system( ("less", $prev) ) or die("Failed to view previous logbooks");
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
    my $step = $_[0] // 7;
    my $delta = DateTime::Duration->new( days => $step );
    my $today = time_today();
    my $target = $today - $delta;
    my @filename_datetimes = map { datetime_from_filename($_) } @files;
    my $fn_out = "$ENV{HOME}/.logbook_history";
    open( my $fh_history, '>', $fn_out);
    for (@files){
        if (datetime_from_filename($_) >= $target) {
            open (my $hist_file, '<', $_);
            my @contents = <$hist_file>;
            close( $hist_file );
            say $fh_history @contents;
        }
    }
    close $fh_history;
    system( 
        "pandoc",
        $fn_out,
        "-f",
        "gfm",
        "--mathjax",
        "--standalone",
        "-o",
        "$ENV{'HOME'}/log_history.epub",
        "-M",
        "title=logbook"
    );
    say "Created $fn_out.epub with last $step days logbooks";
}
