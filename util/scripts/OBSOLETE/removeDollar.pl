#!/usr/bin/perl

# usage: removeDollar.pl [-char=___] infile.v
#    copy original file to infile.v~

$verbose = 0;
$char     = '_';

use Getopt::Long;
GetOptions("verbose!"=>\$verbose,
           "char=s"=>\$char,
           "help!"=>\$help);

doUsage() if ($help);

doUsage() if ($#ARGV < 0); # need at least one argument

######################################################################
foreach $ofile ( @ARGV ) {
    system "cp $ofile $ofile~";

    # two pass, get the wire names first
    next unless open(FILE,$ofile);
    my @lines = <FILE>;
    close FILE;

    # get all the wire names
    for ($i=0; $i<$#lines; $i++) {
        $_ = $lines[$i];

        # only changing wires, but they can't collide with in/out signals
        if (s/^\s*(wire|reg)\s+//) {
            while (!/\;/) { $_ .= $lines[++$i]; }
            
            # now we have all the signal names, don't care about width
            s/\;//;
            s/\s+//g;
            s/^\[\d+:\d+\]//;

            my @fields = split(',');
            foreach $f (@fields) {
                $signal{$f} = $f if ($f =~ /.\$/);
            }
        }
    }

    # now I have all the signal names, so I can make sure we haven't
    #   created a signalname that overlaps with an existing signal
    #   (since I am assuming that _ will be popular)
    # TODO: also change the .info file?
    foreach $f ( sort keys %signal ) {
        if ($f =~ /\$/) {
            my $n = $f;
            my $nxx = $char;

            while (1) {
                $n =~ s/\$/$nxx/;
                if ($signal{$n} eq "") {
                    $signal{$f} = $n;
                    last;
                }
                if ($nxx eq '_') { $nxx = 'a'; }
                else             { $nxx++; }
            }
        }
    }

    # it's time to search and replace!
    my $alllines = join( '', @lines );     # get the whole file in one string
    
    # now global replace for each signal
    foreach $f ( sort keys %signal ) {
        if ($f =~ /\$/) {                  # not needed, but faster
            my $n = $signal{$f};           # get new signal name
            $f =~ s/\$/\\\$/;              # $ is tricky, don't let it translate
            $alllines =~ s/\b$f\b/$n/gm;       # change it everywhere
        }
    }

    # now write it back out
    open(OFILE, ">$ofile") || die "Can't open output file '$!', stopped";
    print OFILE $alllines;
    close OFILE;
}


######################################################################
sub doUsage {
    print "Usage: removeDollar.pl [-char=string] file.v [file2.v ...]\n";
    print "\n";
    print "         if -char not specified, default is '_'\n";
    print "\n";
    print "         output is original file in file.v~\n";
    print "                         changes in file.v\n";
    print "\n";    
    print "         scan verilog netlist generated by BSV,\n";
    print "         replace all $ characters in wire names with char\n";
    exit(-1);
}
