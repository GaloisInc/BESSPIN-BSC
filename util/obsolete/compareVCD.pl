#!/usr/bin/perl
################################################################################

# use Getopt::Long;
# GetOptions("o"=>\$oflag,
#             "verbose!"=>\$verboseornoverbose,
#            "string=s"=>\$stringmandatory,
#             "optional:s",\$optionalstring,
#             "int=i"=> \$mandatoryinteger,
#             "optint:i"=> \$optionalinteger,
#             "float=f"=> \$mandatoryfloat,
#             "optfloat:f"=> \$optionalfloat);

# print "oflag $oflag\n" if $oflag;
# print "verboseornoverbose $verboseornoverbose\n" if $verboseornoverbose;
# print "stringmandatory $stringmandatory\n" if $stringmandatory;
# print "optionalstring $optionalstring\n" if $optionalstring;
# print "mandatoryinteger $mandatoryinteger\n" if $mandatoryinteger;
# print "optionalinteger $optionalinteger\n" if $optionalinteger;
# print "mandatoryfloat $mandatoryfloat\n" if $mandatoryfloat;
# print "optionalfloat $optionalfloat\n" if $optionalfloat;

$version = "0.1";
print "compareVCD version $version\n";

$vcdFile1 = $ARGV[0];
$vcdFile2 = $ARGV[1];

getSymbolNames("1",$vcdFile1);
getSymbolNames("2",$vcdFile2);

compareCommonSignals();

######################################################################
sub getSymbolNames {
    my ($name,$vcdFile) = @_;
    local *VCD;
    open(VCD,"$vcdFile") || die "Unable to open first vcdfile '$vcdFile', stopped";
    my @path = ();
    my $pth;
    my $header = 1;
    my $scnt = 0;
    my $vcnt = 0;
    my $time = 0;

    foreach (<VCD>) {
        # get name -> symbol conversion

        s/\s+$//;

        if ($header == 1) {
            s/\s+/ /g;
            s/^\s+//g;

            if (/^\$scope module (\S+) \$end/) {
                push(@path, $1);
                $pth = join("/",@path);
            } elsif (/^\$upscope/) {
                pop(@path);
                $pth = join("/",@path);
                
            } elsif (/^\$var (reg|wire) (\d+) (\S+) (\S+) \$end/) {
                my $wire = $4;
                my $sym  = $3;
                $symbol{"$name:$pth/$wire"} = $sym;
                $signal{"$name:$sym"} = "$pth/$wire";
                $sigcnt{"$name:$pth/$wire"}++;
                $scnt++;

            } elsif (/^\$enddefinitions/) {
                $header = 0;
            } else {
                ;
            }

        } else { # elsif ($header == 0) 
            if (/^\$dumpvars/) {
                ; # skip this

            } elsif (/^\#(\d+)$/) {
                $time = $1;
                $timehits{"$time"}++;

            } elsif (/^([01xz])(\S+)$/) {
                $sig = $signal{"$name:$2"};
                $value{"$name:$time:$sig"} = $1;
                $vcnt++;
            } elsif (/^(b[01xz])+ (\S+)$/) {
                $sig = $signal{"$name:$2"};
                $value{"$name:$time:$sig"} = $1;
                $vcnt++;
            }
        }
    }
    close VCD;
    
    print "Info $vcdFile has $scnt signals and $vcnt value changes\n";
}



######################################################################
sub numerical {
    return $a <=> $b;
}

sub compareCommonSignals {
    my $time;

    # find matching signal names
    foreach $item (keys %sigcnt) {
        my ($name,$sig) = split(":", $item);
        if (($name == "1") && ($sigcnt{"2:$sig"} ne 0)) {
            $checkSignal{$sig} = 1;
        } elsif (($name == "2") && ($sigcnt{"2:$sig"} ne 0)) {
            $checkSignal{$sig} = 1;
        } elsif ($sigcnt{"1:$sig"} ne 0) {
            push(@onlyIn1, "  $sig");
        } elsif ($sigcnt{"2:$sig"} ne 0) {
            push(@onlyIn2, "  $sig");
        }
    }

    foreach $time (sort numerical keys %timehits) {
        foreach $sig (keys %checkSignal) {
            # at each time slice check for signal we care about
            my $val1  = $value{"1:$time:$sig"};
            my $val2  = $value{"2:$time:$sig"};
            if ($val1 ne $val2) {
                push( @mismatches, "  $sig at $time, vcd1 had '$val1', vcd2 had '$val2'");
            }
        }
    }

    if ($#mismatches >= 0) {
        print "######################################################################\n";
        print "Mismatching common signals\n";
        print join("\n",  @mismatches);
        print "\n";
    } else {
        print "Common signals match exactly\n";
    }


    # now for signals we skipped
    if ($#onlyIn1 >= 0) {
        print "######################################################################\n";
        print "Signals only in $vcdFile1\n";
        print join("\n",  @onlyIn1);
        print "\n";
    }

    if ($#onlyIn2 >= 0) {
        print "######################################################################\n";
        print "Signals only in $vcdFile2\n";
        print join("\n",  @onlyIn2);
        print "\n";
    }

}
