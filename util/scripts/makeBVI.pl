#!/usr/bin/perl
################################################################################
# Author: Steve Allen (sallen@bluespec.com)
# history: 1.0 - always enable, always ready - no parameters yet a good start
################################################################################

use Getopt::Long;
GetOptions( "debug" => \$debug,
            "mod:s" => \$mod,
            "ifc:s" => \$ifc,
            "help"  => \$help,
            );

$version = "1.0";

if ($help || ($#ARGV !=0)) {
    &usage();
    exit(-1);
}

my $verilog = $ARGV[0];
open(FILE,"$verilog.v") || die "Can't open input file '$verilog', stopped ";

######################################################################
# get input verilog file from stdin
while (<FILE>) {
    if (m@^\s*(function|task)@) {
        last;
    }
    elsif (m@^\s*parameter\s+([\w_]+)\s*=@) {
        $vparam{$1} = 1;
    }
    elsif (s@^\s*(input|output)@@) {
        my $dir = $1;
        s@//.*$@@g;
        s@\s+@ @g;
        s@^\s@@g;
        if (s@\[\s*(\d+)\s*:\s*(\d+)\s*\]\s*@@) { # remove size if it is there 
            $size = $1 - $2 + 1;
        } elsif (s@\[\s*(\w+)-1\s*:\s*(\d+)\s*\]\s*@@) {
            $size = $1;         # parameterized string!
            my $orig = $size;
            $size =~ tr/A-Z/a-z/; # lowercase so BSV doesn't panic ;)
            $params{$size} = $orig;
        } else {
            $size = 1;
        }
        s@(\s|\;)@@g;           # remove spaces, semicolon
        my @sigs = split(',');
        foreach $pin (@sigs) {
            $pinsize{$pin} = $size;
            $pindir{$pin}  = $dir;

            my $pintype = ($size =~ /^\d+/) ? "Bit#($size)" : $size;
            if ($pin =~ /^(clk|clock|h_clk)$/i) {
                # print "// found clock = $pin\n";
                $clk = $pin;
            } elsif ($pin =~ /^(rst|rst_n|reset|reset_n)$/i) {
                # print "// found reset = $pin\n";
                $rst = $pin;
            } elsif ($dir eq "input") {
                push (@inputs, "m_$pin");
                $methodIfc  .= sprintf("  method %-15s  %-10s ( $pintype p_$pin );\n", "Action", "m_$pin" ) ;
                $methodDefs .= sprintf("    method %-15s  %-10s ( %s ) enable((*inhigh*)EN);\n", " ", "m_$pin", $pin ) ;
            } elsif ($dir eq "output") {
                push (@outputs, "m_$pin");
                $methodIfc  .= sprintf("  method %-15s  %-10s ();\n", $pintype, "m_$pin" );
                $methodDefs .= sprintf("    method %-15s  %-10s ();\n", $pin, "m_$pin" );
            }
        }
    }
}

# create schedule...talk about optimistic
# but let user fix this as needed
{
    my @allm = @inputs;
    push( @allm, @outputs );
    $schedule  = "    schedule (" . join(',', @allm) . ") CF\n";
    $schedule .= "             (" . join(',', @allm) . ");\n";
}


$ifc     = "IfcName"      if ($ifc eq "");
$mod     = "mkModule"     if ($mod eq "");
$verilog = "verilogBlock" if ($verilog eq "");

############################################################
print "// Auto generated by makeBVI.pl on " . `date`;

# any param not defined at the interface, is defined on the module
foreach $item (keys %params) {
    delete $vparam{$params{$item}};
}
my @modparamlist;
my @paramlist = ();
foreach $item (keys %vparam) {
    push (@paramlist, "    parameter $item = fromInteger(i_$item);");
    push (@modparamlist, "Integer i_$item");
}

$modparam = '#(' . (join(',', sort @modparamlist)) . ') '
    if ($#modparamlist >= 0);

############################################################
my @parlist = ();
foreach $item (keys %params) {
    push (@parlist, "type $item");
    push (@paramlist, "    parameter $params{$item} = valueof(n_$item);");
}
if ($#parlist >= 0) {
    my $ifclist = join(',', @parlist);
    print "interface $ifc#( $ifclist );\n";
    $ifcdef = $ifclist;
    $ifcdef =~ s/type ([\w_]+)/Bit#(n_$1)/g;
    $ifcdef = "$ifc#( $ifcdef )";
} else {
    print "interface $ifc;\n";
    $ifcdef = $ifc;
}

print $methodIfc;
print "endinterface\n";
print "\n";
print "import \"BVI\" $verilog =\n";
print "  module $mod$modparam ( $ifcdef );\n";
print "    default_clock clk($clk) ;\n";
print "    default_reset rst($rst) ;\n";
print "\n";
print join("\n", @paramlist);
print "\n\n";
print $methodDefs;
print $schedule;
print "  endmodule\n";

#    method enq((*reg*)D_IN) enable(ENQ);
#    method deq() enable(DEQ);
#    method (*reg*)D_OUT first;
#    method FULL_N   notFull;
#    method FULL_N i_notFull;
#    method (* reg *)EMPTY_N   notEmpty;
#    method (* reg *)EMPTY_N i_notEmpty;
#    method clear() enable(CLR);
#
#    schedule deq CF (enq, i_notEmpty, i_notFull) ;
#    schedule enq CF (deq, first, i_notEmpty, i_notFull) ;
#    schedule (first, notEmpty, notFull) CF
#               (first, i_notEmpty, i_notFull, notEmpty, notFull) ;
#    schedule (i_notEmpty, i_notFull) CF
#               (clear, first, i_notEmpty, i_notFull, notEmpty, notFull) ;
#    schedule (clear, deq, enq) SBR clear ;
#    schedule first SB (clear, deq) ;
#    schedule (notEmpty, notFull) SB (clear, deq, enq) ;


######################################################################
sub usage {

print <<"HELP"

Usage: makeBVI.pl [switches] verilogmod

  switches:
     -ifc=ifcname      => ifcname is the bluespec interface name desired
                          defaults to "IfcName" if not specified
     -mod=modname      => modname is the bluespec module name desired
                          defaults to "mkModule" if not specified
     verilogmod        => module name of verilog block (assumes verilogmod.v
                          is a file in the local directory)
  so with this call

      makeBVI.pl -ifc=Foo -mod=mkFoo verfile

  will be used in a .bsv file like so:

      Foo uut <- mkFoo; // calls verfile.v, module verfile
HELP
}
