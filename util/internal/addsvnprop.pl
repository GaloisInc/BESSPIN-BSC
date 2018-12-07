#!/usr/bin/perl --
# -*-Perl-*-
################################################################################
################################################################################
# Script to check for and add svn properties on
# files which shouod have Date/Revision fields.   
# these include src/lib/Verilog  and src/lib/BSVSource/...


$propToSet = "Date Revision";
$propRegex = "Date.*Revision";

foreach my $file (@ARGV) {
    doProp ($file)
}

sub doProp ($$) {
    my $file = $_[0];

    my $props =`svn propget svn:keywords  $file`;
    chop $props;

    if ($props !~ $propRegex) {
        system "svn propset svn:keywords \"$propToSet\" $file" ;
    }
    foreach my $cp ("Date", "Revision") {
        my $grep = system ("grep  -q -e '$cp' $file");
        if ( $grep) {
            print "$file is missing $cp\n";
        }
    }
}
