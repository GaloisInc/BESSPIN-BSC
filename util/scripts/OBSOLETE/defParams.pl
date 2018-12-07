#!/usr/bin/perl -pi~

# in our generated verilog we don't use # much
#  and only for parameters is it '#('
if (/ \#\(/) {
    
    # make sure I've read in all the parameters (they may be multiline)
    while (!/\) /) {
        $_ .= <>;
    }

    # now we have all the parameters
    if (s/ \#\(([^\)]*)\)\s+([\w_]+)/ $2/m) {
        my $params = $1;
        my $inst   = $2;
        my $firstLine = $_;
        
        print $_;
        
        # undent a bit ?
        # my $len    = length($params);
        
        # split parameters, save them until we see ';'
        if ($params =~ m@/\*@) {
            # -v95 switch was used.. format = /*name*/value
            # split parameters, save them until we see ';'
            $params =~ s/\s+//g;    # remove all spaces
            $params =~ s@\*/@=@g;   # change '*/' => '='  (* needs escape)
            $params =~ s@/\*@@g;    # change '/*' => ''
        } else {
            #-v2k used .. format = .name(value)
            $params =~ s/\s+//g;    # remove all spaces
            $params =~ s@\)@@g;     # change ')' => ''
            $params =~ s@\.@@g;     # change '.' => ''
            $params =~ s@\(@=@g;    # change '(' => '='
        }
        
        # break into separate fields
        my @f = split(',', $params);
        
        # skip ahead until we see ';'
        while (<>) {
            print $_;
            last if (/\;/) 
            }
        
        foreach $i ( @f ) {
            print "  defparam $inst.$i ;\n";
        }
        
        $_ = '';
    }
    else {
        # failed to parse properly, so print a warning and leave it as is?
        print "// WARNING: $0 failed to change to defParam\n";
        print $_;
    }
}
