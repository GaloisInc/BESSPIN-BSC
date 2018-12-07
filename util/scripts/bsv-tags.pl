#!/usr/bin/perl 

# usage: 'bsv-tags [*.bsv] > TAGS'

if ($#ARGV < 0) {
    # if no arguments given, just look at all the .bsv files!!
    @ARGV = <*.bsv>;
}

# module mult_W1W2W3mult_W1W2W321,582
# tag string^?name^A<line>,<character>

foreach $file ( @ARGV ) {
    if (open(FILE,$file)) {
        my @lines = (); # lines for this file
        my $char = 0;   # keep track of the number of characters parsed so far
        my $tchars = 0; # total number of characters in this group of etag stuff
        my $line = 0;   # keep track of the line number inside file

        while (<FILE>) {
            if (/^(\s*module\s+([\w_]+))/) {
                my $thisline = "$1$2^A$line,$char\n";
                push(@lines, $thisline);
                
                $tchars += length($thisline);
            }

            # use a regex to parse whatever you want
            elsif (/^(\s*function\s+\S+\s+([\w_]+))/) {
                my $thisline = "$1$2^A$line,$char\n";
                push(@lines, $thisline);
                $tchars += length($thisline);
            }

            $line++;
            $char += length($_);  
        }

        print "\n";
        print "$file,$tchars\n";
        print @lines;
    }
}
