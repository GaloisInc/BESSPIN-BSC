#!/usr/bin/perl

# Script to convert Bluespec generated Verilog files 
# into strict Verilog 95 syntax.

# Copyright (c) 2005 Bluespec, Inc.
#  All rights reserverd


# scan library elements and local modules for 
# paramter info
## TODO -- add an argument which scans a user's
##         library in addition to Bluespec's


$BLUESPECDIR = $ENV{BLUESPECDIR};

$tmp = `ls $BLUESPECDIR/Verilog/*.v`;
@f = split(/\s+/, $tmp);

# get parameter definitions out of all the verilog files
foreach $file (@f) {
    next unless (open(FILE, $file));
    $pcnt = 0;
    $mod = "";
    while (<FILE>) {
        if (/^\s*module\s+([\w_]+)/) {
            $mod = $1;
            $knownmods{$mod} = True ;
        }
        elsif (/^\s+parameter\s+(\S+)\s+=/) {
            die "DANGER!" if ($mod eq "");
            $parameter{$mod,$pcnt++} = $1;
            ## print "Found $mod parameter $pcnt -> $1\n";
        }
    }
    close FILE;
}

# create output dir
system "mkdir -p v95";

# scan requested file(s) and put changes to v95/file.v
foreach $file (@ARGV) {
    next unless (open(FILE,$file));
    next unless (open(OFILE,">v95/$file.tmp"));
    
    while (<FILE>) {
        # v2k "Feature" #1 - "," rather than "or"
        # This is not an issue with bsc3.8.62 and later, and this
        # substitution is insufficient.
        s/ , / or /g if (/^\s+always/);

        # v2k "Feature" #2 - named parameters
        if (/\s+(\S+)\s+\#\(/) {
            my $mod = $1;
            if ( $knownmods{$mod} eq "" ) {
                print "The parameters for module $mod have not been found.\n" ;
                print "reference to ", $mod, " was found in $file\n" ;
                exit 1 ;

            }

            while (!/\;/) {   # read up until semi colon
                $_ .= <FILE>;
            }

            # get parameters with name values
            my @lst;
            for ($i=0; ($parameter{$mod,$i} ne "") ;$i++) {
                my $prm = $parameter{$mod,$i};
                push(@lst,$1) if (s/\.$prm\(([\S]+)\)//);
            }

            # generate sorted list according to defined parameters
            my $plist = join(',', @lst);
            s/\#\(.*?\)/\#($plist)/s;
        }

        # v2k "Feature" #3  use of $signed / $unsigned
        s/\$signed/\/\*\$signed\*\//g  ;
        s/\$unsigned/\/\*\$unsigned\*\//g  ;

        print OFILE $_;
    }

    close FILE;
    close OFILE;
    rename "v95/$file.tmp", "v95/$file" ;
}
