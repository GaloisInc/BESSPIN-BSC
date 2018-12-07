#!/usr/bin/perl
################################################################################

#  expiryMonth = (September) :: Month
#  expiryYear = (2004) :: Int
$expMonth = 1;
$expYear  = 2005;

# $fl = "$ENV{HOME}/bsc/src/comp/Expiry.hs";
$fl = $ARGV[0];
open (F,$fl) || "can\'t open $f, $!";
while (<F>) {
    if (/expiryMonth = \((\w+)\) :: Month/i) {
        $month = $1;
        # convert to a number
        if    ($month eq "January")   { $expMonth = 12;}
        elsif ($month eq "February")  { $expMonth = 1; }
        elsif ($month eq "March")     { $expMonth = 2; }
        elsif ($month eq "April")     { $expMonth = 3; }
        elsif ($month eq "May")       { $expMonth = 4; }
        elsif ($month eq "June")      { $expMonth = 5; }
        elsif ($month eq "July")      { $expMonth = 6; }
        elsif ($month eq "August")    { $expMonth = 7; }
        elsif ($month eq "September") { $expMonth = 8; }
        elsif ($month eq "October")   { $expMonth = 9;}
        elsif ($month eq "November")  { $expMonth = 10;}
        elsif ($month eq "December")  { $expMonth = 11;}
        else { die "Can\'t figure out what month $month is, "; }
    }
    elsif (/expiryYear = \((\d+)\) :: Int/) {
        $expYear = $1 - 2000;
    }
}
close F;

# really!
if ($month == 12) {
    $year--;
}

################################################################################
# get build and version numbers from ~/bsc/src/comp/Version.hs
# $fl = "$ENV{HOME}/bsc/src/comp/Version.hs";
$fl = $ARGV[1];
open (F,$fl) || "can\'t open $f, $!";
while (<F>) {
    if (/versionnum = \"(\S+)\"/) {
        $gVersion = $1;
        last;
    }
}
close F;

# open "blueview.tcl", fix necessary lines
$bv   = "blueview.tcl";
$bkup = "$bv~";

$date = `date`;
$host = `hostname`;
$who  = `whoami`;
chop $date;
chop $host;
chop $who;

system "cp -p $bv $bkup";
open (F,"$bkup") || die "can't open $bkup";
@lines = <F>;
close F;

open (R,"release.notes.txt") || die "can't open $bkup";
@notes = <R>;
close R;

open (F,">$bv")  || die "can't open >$bv";
foreach (@lines) {
    if (/^set gExpDate/) {
        print F "set gExpDate [list $expMonth $expYear]\n";

    } elsif (/^set gVersion/) {
        print F "set gVersion $gVersion\n";

    } elsif (/^\# build \d+ => /) {
        next; # toss old build lines

    } elsif (/^set gBuild (\d+)/) {
        $build = $1 + 1;
        print F "# build $build => {Built on '$date' on '$host' by '$who'}\n";
        print F "set gBuild $build\n";

        # add line to release notes
        open (R,">release.notes.txt") || die "can't open $bkup";
        print R "################################################################################\n";
        print R "# build $build => {Built on '$date' on '$host' by '$who'}\n\n";
        print R @notes;
        close R;

    } else {
        print  F $_;
    }
}
close F;

