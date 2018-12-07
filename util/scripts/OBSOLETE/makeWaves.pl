#!/usr/bin/perl
################################################################################
$path = ".";
$top  = "/main/top";

use Getopt::Long;
GetOptions(
           "verbose!"=>\$verbose,
           "top:s"=>\$top,
           "path:s"=>\$path
           );

if ($#ARGV != 0) {
    print "Usage:  makeWaves.pl [-p obj[:.:]] [-top vpath] top.v > generated.rc\n";
    print "           -path list   = colon separated directory list\n";
    print "                            to be scanned for *.bo files\n";
    print "                            (default is .)\n";    
    print "\n";
    print "           -top vpath   = inst path of top level verilog block\n";
    print "                            being parsed in target simulation\n";
    print "                            (default is /main/top)\n";    
    print "\n";
    print "           top.v        = top level verilog file \n";
    print "\n";
    print "           generated.rc = nWave suitable waveform file\n";
    exit(1);
}

# use . as associative array separator.. makes it easier in the case
# of AxiDefines.AxiID to parse, unparse, etc.
$; = '.';

$size{"Prelude.Bool"} = 1;
$size{"Prelude.bit"} = 1;

######################################################################
######################################################################
######################################################################
######################################################################
sub parseFile {
    my ($inst,$fl) = @_;

    $fl = &findFile($fl);
    return if ($fl eq "");
    return if ($alreadyParsed{$fl} == 1);

    local (*FILE);
    return unless (open(FILE,"$fl"));
    # print "Scan $fl...\n";

    my $ind = "  " x $#group;

    my $ginst = $inst;
    $ginst =~ s@/@__@g;
    $ginst =~ s@^__@@g;

    if ($#group == 0) {
        push( @signals,  $ind . "addGroup \"$ginst\" -e FALSE" );
    } else {
        push( @signals,  $ind . "addSubGroup \"$ginst\" -e FALSE" );
    }

    # $alreadyParsed{$fl} = 1;

    while (<FILE>) {
        if (s@^\s*//\s*nWave:\s*group\s+(\w+)@@) {
            push( @signals,  $ind ."  addSubGroup \"$1\" -e FALSE" );
            push(@group, $1);
        }
        elsif (s@^\s*//\s*nWave:\s*endgroup@@) {
            push( @signals,  $ind ."  endSubGroup" );
            pop(@group);
        }

        elsif (s@^\s*//\s*nWave:\s*@@) {
            my $ind = "  " x $#group;

            s/\s+//g;
            my @sigs = split(',');
            foreach $item ( @sigs ) {
                if ($item =~ /:/) {
                    my ($sig, $gname, $typ) = split(':', $item);


                    if ($structItems{$typ} ne "") {
                        # we have some structure or enum renaming to do
                        push( @signals,  $ind ."  addSubGroup \"$sig\" -e FALSE" );
                        printStruct(  $typ, $sig, $inst, ($ind . "    ") );
                        push( @signals,  $ind ."  endSubGroup" );
                    } else {
                        # sorry, no struct or enum information found
                        push( @signals,  $ind ."  addSignal -h 15 -UNSIGNED -HEX $inst/$item" );
                    }
                }
                else {
                    push( @signals,  $ind ."  addSignal -h 15 -UNSIGNED -HEX $inst/$item" );
                }
            }
        }

        elsif (m@^\s*(\w+)\s+(\w+)\s*\(@) {
            my $mod     = $1;
            my $modInst = $2;
            next if ($mod eq "module");
            # print ";;; Module: $inst $mod $modInst\n" if ($verbose);
            push( @group, $modInst );
            &parseFile ( "$inst/$modInst", "obj/$mod.v" );
            pop ( @group );
        }
    }

    if ($#group != 0) {
        push( @signals,  $ind . "endSubGroup" );
    }

    close FILE;
}

######################################################################
######################################################################
######################################################################
######################################################################
sub findFile {
    my ($fl) = @_;

    return $fl if (-e "$fl");

    foreach $dir ( @dirs ) {
        return "$dir/$fl" if (-e "$dir/$fl");        
    }
    return "";
}

######################################################################
######################################################################
######################################################################
######################################################################
# make directory of all the relevant files?
sub printStruct {
    my ($item, $sig,$ipath,$ind) = @_;
    my ($mod,$name) = split('\.', $item);
    my $last        = $structItems{$item};
    my $totalSize   = $size{$item} -1 ;
    my $h = $totalSize;
    my $i;

    # print "; $mod,$name\n";
    for ($i=0; $i <= $last; $i++) {
        # get type of this index
        # get size of it
        # print bus stuff, and aliases if need be

        my $type = $structType {$mod,$name,$i};
        my $sz   = $size{$type};
        my ($modf,$fldf)  = split('\.', $structField{$mod,$name,$i});
        my $typ  = $structType{$mod,$name,$i};
        my $l = $h - $sz + 1;

        # generate new name, using same old inst path
        my $new = $sig;
        $new =~ s@/[^/]+$@/@;
        $new .= $fldf;

        # make structure fields all purty like...
        if ($alias{$typ} ne "" ) {
            # get name from structure field!
            push( @renames, "addRenameSig \"$ipath/$new\" \"$ipath/$sig\[$h:$l\]\"" );
            push( @signals, ($ind . "aliasname $typ") );
            push( @signals, ($ind . "addSignal       -UNSIGNED -ASC $ipath/$new") );

            # fallback or no renaming?
            # push( @signals, ($ind . "addSignal         -UNSIGNED -ASC $ipath/$sig\[$h:$l\]") );
        }
        else {
            # get name from structure field!
            push( @renames, "addRenameSig \"$ipath/$new\" \"$ipath/$sig\[$h:$l\]\"" );
            push( @signals, ($ind . "addSignal -h 15 -UNSIGNED -HEX $ipath/$new") );

            # fallback or no renaming?
            # push( @signals, ($ind . "addSignal -h 15 -UNSIGNED -HEX $ipath/$sig\[$h:$l\]") );
        }

        $h = $l - 1;
    }
}


######################################################################
######################################################################
######################################################################
######################################################################
# find all the .bi files in the search path

my @biFiles = ();
my @dirs = split(":", $path);
foreach $dir ( @dirs ) {
    foreach $fl (<$dir/*.bi>) {
        push ( @biFiles, $fl );
    }
}

foreach $file (@biFiles) {
    next unless (open(FILE,"$file"));
    # read it all in!
    @lines = <FILE>;
    close FILE;

    my $all = join('', @lines);
    
    # setup for easy parsing
    $all =~ s/\;/ \;/g;
    $all =~ s/\n/ /g;
    $all =~ s/\s+/ /g;
    
    # damn haskell ;)
    $all =~ s@(\¶|\®|\¡|\)|\()@@g;
    
    my @word = split(' ', $all);
    
    # and get ride of dem crazy haskel characters
    my $i = 0;
    my $module = "";

    while ($i <$#word) {

        # signature AxiDefines where {
        if ($word[$i] eq "signature") {
            $module = $word[$i+1];
            $i += 4;
        }

        # type  AxiDefines.AxiAddr :: *  = Prelude.UInt 12;
        elsif (($word[$i]   eq "type") && ($word[$i+2] eq "::") &&
               ($word[$i+3] eq "*")    && ($word[$i+4] eq "=")  && ($word[$i+7] eq ";")) {
            my ($mod,$name) = split('\.', $word[$i+1]);
            my ($lib,$type) = split('\.', $word[$i+5]); # Prelude.UInt
            my $size        = $word[$i+6]; # 12 

            # if it's a prelude type, then we have the size already)
            if ($lib eq "Prelude") {
                print "Size: $mod.$name = $size\n" if ($verbose);
                $size{$mod,$name} = $size;
            }
            else {
                die "ERROR: unable to parse type: $mod.$name";
            }

            $i += 8;            # success
        }

        # data AxiDefines.AxiSize :: * =
        #     AxiDefines.Burst1 |
        #            "
        #     AxiDefines.Burst128 ;
        elsif (($word[$i]   eq "data") && ($word[$i+2] eq "::") &&
               ($word[$i+3] eq "*")    && ($word[$i+4] eq "=")) {
            my ($mod,$name) = split('\.', $word[$i+1]);

            $i += 5;
            my $inx = 0;
            do {
                my ($modv,$label) = split('\.', $word[$i++]);
                die "$modv ne $mod for data type, stopped" if ($modv ne $mod);
                print "Text: $mod.$name.$inx = $label\n"  if ($verbose);
                $data{$mod,$name,$inx} = $label;
                $alias{$mod,$name} = $inx++;
            } while ($word[$i++] ne ";");

            # calculate how many bits that was roof(log($inx)/(log(2));
            $size{$mod,$name} = int ((log ($inx) / log(2)) + 0.999999);
            print "Text: $mod.$name size = $size{$mod,$name}\n"  if ($verbose);
        }

        # struct (AxiDefines.AxiRWAddr :: *) = {
        #    AxiDefines.id :: AxiDefines.AxiId ;
        #          "
        #    AxiDefines.op :: AxiDefines.AxiOp
        # } ;
        elsif (($word[$i]   eq "struct") && ($word[$i+2] eq "::") &&
               ($word[$i+3] eq "*")      && ($word[$i+4] eq "=") && ($word[$i+5] eq "{")) {
            my ($mod,$name) = split('\.', $word[$i+1]);

            $i += 6;
            my $inx = 0;
            do {
                my ($md,$fld) = split('\.',$word[$i++]); # AxiDefines.mId
                $i++;                    # ::
                my $type  = $word[$i++]; # AxiDefines.AxiId
                $structField{$mod,$name,$inx} = "$name.$fld";
                $structType {$mod,$name,$inx} = $type;
                $structItems{$mod,$name} = $inx;
                $size{$mod,$name} += $size{$type};

                if ($size{$type} > 0) {
                    print "Struct: $mod.$name.$inx = $field => $type ($sz)\n" if ($verbose);
                }
                else {
                    die "out of order: size not defined yet: $mod.$name.$inx = $field => $type";
                }
                $inx++;
            } while ($word[$i++] ne "}");

            $i++;               # bump past final semi colon
        }
        
        else {
            # not sure what we are looking at... find next ; and start over (or EOF
            while ($i < $#word) {
                last if ($word[$i++] eq ";");
            }
        }
    }
}

######################################################################
######################################################################
######################################################################
######################################################################
# no go and make sure we have sizes for everything!
foreach $item ( keys %alias ) {
    my $last = $alias{$item};
    my ($mod,$name) = split('\.', $item);
    push( @aliases, "aliasmapname $item" );
    for ($i=0; $i <= $last; $i++) {
        my $label = $data{$item,$i};
        push( @aliases, "  nalias $label  $i  NULL" );
    }
}

# use . as associative array separator.. makes it easier in the case
# of AxiDefines.AxiID to parse, unparse, etc.

$; = '.';
@searchDirs = split(":", $path);

push( @group, "top" );

&parseFile( $top , findFile( $ARGV[0] ) );

print "Magic 271485  \n";
print "Revision 6.0v3 \n";
print "signalSpacing 3 \n";
print "cursor 2670.000000 \n";
print "marker 0.000000 \n";
print "top 0 \n";
print "COMPLEX_EVENT_BEGIN\n";
print "COMPLEX_EVENT_END\n";
print "curSTATUS ByChange\n";

print "\n";
print join("\n", @aliases );
print "\n";
print "\n";

print join("\n", @renames );
print "\n";
print "\n";

print join("\n", @signals );
print "\n";
print "\n";

print "; grid status\n";
print "; gridSignal signal_name\n";
print "; gridEdge 0 | 1\n";
print "; gridCount\n";
print "; gridCycleTime from to cycleTime\n";
