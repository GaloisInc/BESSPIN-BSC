#!/usr/bin/perl -w

foreach $file (@ARGV) {
    $wrk  = "$file.wrk";

    open(IFILE,"$file") || die "can't open input file, stopped";
    open(OFILE,">$wrk")  || die "can't open input file, stopped";

    while (<IFILE>) {

        s@\(\d+\'d\d+\)@\(\)@ unless (/\.(addr_width|data_width|lo|hi)\(/);

        if (/^\s*initial/) {
            $_ = <IFILE>;
            if (/^\s*begin/) {
                # skip until end
                while (<IFILE>) {
                    last if (/^\s*end/);
                }
            }
        }

        elsif (/^\s*(reg|wire) /) {
            $_ = &readUptoSemi( $_ ) if (!/;/);

            print OFILE $_;

            # get the size of everything
            s/\s+//g;
            s/^(reg|wire)//;
            my $sz = 1;
            if (s/^\[(\d+):(\d+)\]//) {
                $h = $1;
                $l = $2;
                $sz = $h - $l + 1;
            }

            # split out the variables
            s/\;//g;
            my @vars = split(',', $_);
            foreach $v (@vars) {
                $gSize{$v} = $sz;
            }

            $_ = '';
        }

        elsif (/^\s*always\s*\@\s*\(posedge CLK\)/) {
            # collect all the lines that match this begin end
            $depth = 0;
            my @regs;
            while (<IFILE>) {
                if (/^\s*begin/) {
                    $depth++;
                    next;
                }
                elsif (/^\s*end/) {
                    $depth--;
                    next;
                }
                elsif (/^\s*if / && / \<\= /) {
                    push(@regs, $_);
                }

                if ($depth == 0) {
                    last;
                }
            }

            foreach (@regs) {
                # if (currentSrc$EN) currentSrc <= currentSrc$D_IN;
                s/(\s+|if|\()//g;
                s/(\)|<=|;)/,/g;

                my @f = split(',');

                $din  = $f[0];
                $dout = $f[1];
                $en   = $f[2];

                $din  =~ s/(\(|\))//g;
                $en   =~ s/\;//g;

                $w = $gSize{$dout};
                if ($w eq "") {
                    print "WARNING: No size for wire $dout";
                } else {
                    print OFILE "  RegN#(.width($w)) i_reg_$din (.CLK(CLK),.RST_N(RST_N), .D_OUT($dout), .D_IN($din), .EN($en) );\n";
                }
            }
        }

        elsif (/^\s+always/) {
            s/ , / or /g;
            print OFILE $_;
        }

        else {
            print OFILE $_;
        }
    }

    system "estimator $wrk";
    unlink $wrk;

    close IFILE;
    close OFILE;

}


############################################################
sub readUptoSemi {
    my ($ln) = @_;

    while ($ln .= <IFILE>) {
        return $ln if ($ln =~ /;/);
    }
}
