#!/usr/bin/perl

use Getopt::Long;

# Instruction Trace and Event Exception fields
$coredata = "mkBridge.scemi_dut_dutIfc.socA.core.cpu.cpu_wCoreDataOut_wget";
$coredata_valid = "mkBridge.scemi_dut_dutIfc.socA.core.cpu.cpu_wCoreDataOut_whas";

# Memory Read fields
$mem_read_data = "mkBridge.scemi_dut_dutIfc.socA.core.cpu.fDRResponse.fDRResponse_D_OUT_struct.data";
$mem_read_data_deq = "mkBridge.scemi_dut_dutIfc.socA.core.cpu.fDRResponse.fDRResponse_DEQ";
$mem_read_request = "mkBridge.scemi_dut_dutIfc.socA.core.cpu.cpu_fDRRequest_rw_enq_wget";
$mem_read_request_valid = "mkBridge.scemi_dut_dutIfc.socA.core.cpu.cpu_fDRRequest_rw_enq_whas";

# Memory Write fields
$mem_write_request = "mkBridge.scemi_dut_dutIfc.socA.core.cpu.cpu_fDWRequest_rw_enq_wget";
$mem_write_request_valid = "mkBridge.scemi_dut_dutIfc.socA.core.cpu.cpu_fDWRequest_rw_enq_whas";

# Register Trace fields
$gpr_writes0 = "mkBridge.scemi_dut_dutIfc.socA.core.regfile.regfile_writes_wget";
$gpr_writes0_valid = "mkBridge.scemi_dut_dutIfc.socA.core.regfile.regfile_writes_whas";
$gpr_writes1 = "mkBridge.scemi_dut_dutIfc.socA.core.regfile.regfile_writes_1_wget";
$gpr_writes1_valid = "mkBridge.scemi_dut_dutIfc.socA.core.regfile.regfile_writes_1_whas";
$spsr_write = "mkBridge.scemi_dut_dutIfc.socA.core.regfile.regfile_spsrwrite_wget";
$spsr_write_valid = "mkBridge.scemi_dut_dutIfc.socA.core.regfile.regfile_spsrwrite_whas";
$cpsr_write = "mkBridge.scemi_dut_dutIfc.socA.core.regfile.regfile_cpsrwrite_wget";
$cpsr_write_valid = "mkBridge.scemi_dut_dutIfc.socA.core.regfile.regfile_cpsrwrite_whas";

$noasm = 0;
$noit = 0;
$nort = 0;
$noet = 0;
$nomt = 0;

%h2b = (0 => "0000",
        1 => "0001", 2 => "0010", 3 => "0011",
        4 => "0100", 5 => "0101", 6 => "0110",
        7 => "0111", 8 => "1000", 9 => "1001",
        a => "1010", b => "1011", c => "1100",
        d => "1101", e => "1110", f => "1111",
    );

help() if ( @ARGV < 1 or
	     ! GetOptions('help|?' => \$help,
			  'no-asm' => \$noasm,
			  'no-it' => \$noit,
			  'no-rt' => \$nort,
			  'no-et' => \$noet,
			  'no-mt' => \$nomt,
			  'coredata:s' => \$coredata,
			  'coredata_valid:s' => \$coredata_valid,
			  'mem_read_data:s' => \$mem_read_data,
			  'mem_read_data_deq:s' => \$mem_read_data_deq,
			  'mem_read_request:s' => \$mem_read_request,
			  'mem_read_request_valid:s' => \$mem_read_request_valid,
			  'mem_write_request:s' => \$mem_write_request,
			  'mem_write_request_valid:s' => \$mem_write_request_valid,
			  'gpr_writes0:s' => \$gpr_writes0,
			  'gpr_writes0_valid:s' => \$gpr_writes0_valid,
			  'gpr_writes1:s' => \$gpr_writes1,
			  'gpr_writes1_valid:s' => \$gpr_writes1_valid,
			  'spsr_write:s' => \$spsr_write,
			  'spsr_write_valid:s' => \$spsr_write_valid,
			  'cpsr_write:s' => \$cpsr_write,
			  'cpsr_write_valid:s' => \$cpsr_write_valid,
                          'debug'              => \$debug
                          )
	     or defined $help );

open(C, "$ARGV[0]") || file_not_found();

# check if we have access to arm-aahmfp-eabi-objdump
system "touch .tarmac.test";
@foo = `arm-aahmfp-eabi-objdump -b binary -D -m arm .tarmac.test`;
if ($? == -1) {
    unlink .tarmac.test;
    print STDERR "Warning: Unable to access arm-aahmfp-eabi-objdump executable.  Check your \$PATH or the instruction disassembly will not be written out.\n";
    $no_objdump = 1;
}
else {
    unlink .tarmac.test;
    $no_objdump = 0;
}

$enddef = 0;
$addr = 0;
$taken_value = "IT";

sub file_not_found {
    print STDERR "Error: vcd file $ARGV[0] is not found.\n\n";
    usage();
}

sub usage {

    print STDERR "Usage: tarmacgen [vcd file]\n";
    print STDERR "       '--help'\n";
    print STDERR "       '--no-asm' do not output disassebly code.\n";
    print STDERR "       '--no-it' do not output instruction traces.\n";
    print STDERR "       '--no-rt' do not output register traces.\n";
    print STDERR "       '--no-et' do not output event traces.\n";
    print STDERR "       '--no-mt' do not output memory traces.\n";
    print STDERR "       '--coredata <path> (default: $coredata)'\n";
    print STDERR "       '--coredata_valid <path> (default: $coredata_valid)'\n";
    print STDERR "       '--mem_read_data <path> (default: $mem_read_data)'\n";
    print STDERR "       '--mem_read_data_deq <path> (default: $mem_read_data_deq)'\n";
    print STDERR "       '--mem_read_request <path> (default: $mem_read_request)'\n";
    print STDERR "       '--mem_read_request_valid <path> (default: $mem_read_request_valid)'\n";
    print STDERR "       '--mem_write_request <path> (default: $mem_write_request)'\n";
    print STDERR "       '--mem_write_request_valid <path> (default: $mem_write_request_valid)'\n";
    print STDERR "       '--gpr_writes0 <path> (default: $gpr_writes0)'\n";
    print STDERR "       '--gpr_writes0_valid <path> (default: $gpr_writes0_valid)'\n";
    print STDERR "       '--gpr_writes1 <path> (default: $gpr_writes1)'\n";
    print STDERR "       '--gpr_writes1_valid <path> (default: $gpr_writes1_valid)'\n";
    print STDERR "       '--spsr_write <path> (default: $spsr_write)'\n";
    print STDERR "       '--spsr_write_valid <path> (default: $spsr_write_valid)'\n";
    print STDERR "       '--cpsr_write <path> (default: $cpsr_write)'\n";
    print STDERR "       '--cpsr_write_valid <path> (default: $cpsr_write_valid)'\n";
    exit;
}

if ($help == 1) {
    help();
}

sub help {
    print STDERR "\ntarmacgen is a tool for generating tarmac.log file from vcd file\n";

    usage();
    exit;
}

sub endian {
    my $opcode = $_[0];
    my $len = length($opcode);
    my $eopcode = "";

    while ($len > 0) {
	$len = $len-2;
	$eopcode = $eopcode.substr($opcode, $len, 2);
    }
    return $eopcode;
}

sub upper_32_hex {
    my $bnum = $_[0];
    my $len = length($bnum);
    my $b32 = substr($bnum, 0, $len-32);
    return sprintf('%X', oct("0b".$b32));
}

sub lower_32_hex {
    my $bnum = $_[0];
    my $len = length($bnum);
    my $b32 = substr($bnum, $len-32);
    return sprintf('%X', oct("0b".$b32));
}

sub get_bits {
    my $bnum = $_[0];    # The binary text
    my $numbits = $_[1]; # The number of bits of original bits
    my $from = $_[2];    # Extract from this index (based on front padding zeros)
    my $to = $_[3];      # Extract to this index (based on front padding zeros)
    my $b32;
    my $len = length($bnum);
    my $num_padding_zeros = $numbits-$len;
    my $newfrom = $from - $num_padding_zeros;
    my $newto = $to - $num_padding_zeros;
    if ($newfrom < 0) {
	$newfrom = 0;
    }
    if ($newto < 0) {
	$b32 = "0";
    }
    else {
	$b32 = substr($bnum, $newfrom, $newto-$newfrom+1);
	#print "in get_bits $bnum $newfrom $newto\n";
    }
    #print "get_bits: $b32\n";
    return $b32;
}

sub get_mode {
    my $bnum = $_[0];
    my $bnum_value = "";

    if ($bnum eq "10000") {
	$bnum_value = "";
    }
    elsif ($bnum eq "10001") {
	$bnum_value = "_fiq";
    }
    elsif ($bnum eq "10010") {
	$bnum_value = "_irq";
    }
    elsif ($bnum eq "10011") {
	$bnum_value = "_svc";
    }
    elsif ($bnum eq "10111") {
	$bnum_value = "_abt";
    }
    elsif ($bnum eq "11011") {
	$bnum_value = "_und";
    }
    elsif ($bnum eq "11111") {
	$bnum_value = "_sys";
    }
    return $bnum_value;
}

sub get_size {
    my $num = $_[0];
    my $size_value = sprintf('%d', oct("0b".$num));
    my $size;

    if ($size_value == 0) {
	$size = 1;
    }
    elsif ($size_value == 1) {
	$size = 2;
    }
    elsif ($size_value == 2) {
	$size = 4;
    }
    elsif ($size_value == 3) {
	$size = 8;
    }
    elsif ($size_value == 4) {
	$size = 16;
    }
    elsif ($size_value == 5) {
	$size = 32;
    }
    elsif ($size_value == 6) {
	$size = 64;
    }
    elsif ($size_value == 7) {
	$size = 128;
    }

    return $size;
}

$found_instruction = 0;
$found_exception = 0;

$found_coredata_valid = 0;
$found_mem_read = 0;
$found_mem_read_request = 0;
$found_mem_read_request_valid = 0;
$found_mem_write = 0;
$found_mem_write_request = 0;
$found_mem_write_request_valid = 0;
$found_gpr_writes0 = 0;
$found_gpr_writes0_valid = 0;
$found_gpr_writes1 = 0;
$found_gpr_writes1_valid = 0;
$found_spsr_write = 0;
$found_spsr_write_valid = 0;
$found_cpsr_write = 0;
$found_cpsr_write_valid = 0;
$tick = 0;
$addr = 0;
$atd = "A";

while (<C>) {

    $line = $_;

    if ($line =~ /^\$enddefinitions/) {
	if ($coredata_symbol eq "") {
	    print STDERR "The given coredata $coredata is not found in the vcd file.\n";
	    exit;
	}
	if ($coredata_valid_symbol eq "") {
	    print STDERR "The given coredata valid $coredata_valid is not found in the vcd file.\n";
	    exit;
	}
	if ($mem_read_data_symbol eq "") {
	    print STDERR "The given memory read data $mem_read_data is not found in the vcd file.\n";
	    exit;
	}
	if ($mem_read_data_deq_symbol eq "") {
	    #print STDERR "The given memory read data dequeue $mem_read_data_deq is not found in the vcd file.\n";
	    #exit;
	}
	if ($mem_read_request_symbol eq "") {
	    print STDERR "The given memory read request $mem_read_request is not found in the vcd file.\n";
	    exit;
	}
	if ($mem_read_request_valid_symbol eq "") {
	    print STDERR "The given memory read request valid $mem_read_request_valid is not found in the vcd file.\n";
	    exit;
	}
	if ($mem_write_request_symbol eq "") {
	    print STDERR "The given memory write request $mem_write_request is not found in the vcd file.\n";
	    exit;
	}
	if ($mem_write_request_valid_symbol eq "") {
	    print STDERR "The given memory write request valid $mem_write_request_valid is not found in the vcd file.\n";
	    exit;
	}
	if ($gpr_writes0_symbol eq "") {
	    print STDERR "The given writes0(GPRWrite) $gpr_writes0 is not found in the vcd file.\n";
	    exit;
	}
	if ($gpr_writes0_valid_symbol eq "") {
	    print STDERR "The given writes0_valid $gpr_writes0_valid is not found in the vcd file.\n";
	    exit;
	}
	if ($gpr_writes1_symbol eq "") {
	    print STDERR "The given writes1(GPRWrite) $gpr_writes1 is not found in the vcd file.\n";
	    exit;
	}
	if ($gpr_writes1_valid_symbol eq "") {
	    print STDERR "The given writes1_valid $gpr_writes1_valid is not found in the vcd file.\n";
	    exit;
	}
	if ($spsr_write_symbol eq "") {
	    print STDERR "The given spsrwrite $spsrwrite is not found in the vcd file.\n";
	    exit;
	}
	if ($spsr_write_valid_symbol eq "") {
	    print STDERR "The given spsrwrite_valid $spsrwrite_valid is not found in the vcd file.\n";
	    exit;
	}
	if ($cpsr_write_symbol eq "") {
	    print STDERR "The given cpsrwrite $cpsrwrite is not found in the vcd file.\n";
	    exit;
	}
	if ($cpsr_write_valid_symbol eq "") {
	    print STDERR "The given cpsrwrite_valid $cpsrwrite_valid is not found in the vcd file.\n";
	    exit;
	}
	$enddef = 1;
    }
    elsif ($line =~ /$scope\s+module\s+(\S+)\s+/) {
	$current = $1;
	if ($sofar eq "") {
	    $sofar = $current;
	}
	else {
	    $sofar = "$sofar.$current";
	    #print "Sofar $sofar\n";
	}
    }
    elsif ($line =~ /\$upscope\s+\$end/) {
	$sofar = $1 if $sofar =~ /(\S+)[\.\/]\w+/;
    }
    elsif ($line =~ /\$var\s+(wire|reg)\s+(\d+)\s+(\S+)\s+(\w+)\s+\$end/) {
	$signal = $4;
	$symbol = $3;
	$fullname = "$sofar.$signal";
	$fullnames = $fullname;
	$fullnames =~ s|\.|/|g; #substitute . with /
	#print "fullnames $fullnames\n";
	#print "Check $fullname $pc\n";
	if ($fullname eq $coredata || $fullnames eq $coredata) {
            $coredata_symbol = $symbol;
        }
	if ($fullname eq $coredata_valid || $fullnames eq $coredata_valid) {
            $coredata_valid_symbol = $symbol;
        }
	if ($fullname eq $mem_read_data || $fullnames eq $mem_read_data) {
            $mem_read_data_symbol = $symbol;
	    #print "read data symbol $symbol\n";
        }
	if ($fullname eq $mem_read_data_deq || $fullnames eq $mem_read_data_deq) {
            $mem_read_data_deq_symbol = $symbol;
        }
	if ($fullname eq $mem_read_request || $fullnames eq $mem_read_request) {
            $mem_read_request_symbol = $symbol;
        }
	if ($fullname eq $mem_read_request_valid || $fullnames eq $mem_read_request_valid) {
            $mem_read_request_valid_symbol = $symbol;
        }
	if ($fullname eq $mem_write_request || $fullnames eq $mem_write_request) {
            $mem_write_request_symbol = $symbol;
        }
	if ($fullname eq $mem_write_request_valid || $fullnames eq $mem_write_request_valid) {
            $mem_write_request_valid_symbol = $symbol;
        }
	if ($fullname eq $gpr_writes0 || $fullnames eq $gpr_writes0) {
            $gpr_writes0_symbol = $symbol;
        }
	if ($fullname eq $gpr_writes0_valid || $fullnames eq $gpr_writes0_valid) {
            $gpr_writes0_valid_symbol = $symbol;
        }
	if ($fullname eq $gpr_writes1 || $fullnames eq $gpr_writes1) {
            $gpr_writes1_symbol = $symbol;
        }
	if ($fullname eq $gpr_writes1_valid || $fullnames eq $gpr_writes1_valid) {
            $gpr_writes1_valid_symbol = $symbol;
        }
	if ($fullname eq $spsr_write || $fullnames eq $spsr_write) {
            $spsr_write_symbol = $symbol;
	    #print "spsr symbol $symbol\n";
        }
	if ($fullname eq $spsr_write_valid || $fullnames eq $spsr_write_valid) {
            $spsr_write_valid_symbol = $symbol;
	    #print "spsr valid symbol $symbol\n";
        }
	if ($fullname eq $cpsr_write || $fullnames eq $cpsr_write) {
            $cpsr_write_symbol = $symbol;
        }
	if ($fullname eq $cpsr_write_valid || $fullnames eq $cpsr_write_valid) {
            $cpsr_write_valid_symbol = $symbol;
        }
    }
    # This is when we find the next clock tick in the vcd file
    elsif ($line =~ /^#(\d+)/) {
	if ($opcode_valid eq "1" && $found_coredata_valid == $tick) {
	    $found_instruction = 1;
	}

	if ($found_instruction == 1 && $noit == 0 && $tick > 0) {
	    printf "%010d", $tick;
	    print " ns";
	    print " $taken_value ";
	    printf "(%08s:0) ", $pc;
	    printf "%08s ", $pc;

            if ($atd eq 'T') {
                $tmp = hex2bin($opcode_hex);
                if    ($tmp =~ /^11100/) { $opsize = 16; }
                elsif ($tmp =~ /^111/)   { $opsize = 32; }
                else                     { $opsize = 16; }
                
                if ($opsize == 16) {
                    $opcode_hex =~ s/\S{4}$/    /; # remove last 4 chars
                    print " $opcode_hex $atd";
                } else {
                    print " $opcode_hex $atd";
                }
            } else {
                print " $opcode_hex $atd";
                $opsize = 32;
            }

	    if ($no_objdump == 0 && $noasm == 0) {
		print ": ";
		my @output;

		if ($atd eq 'T') {
                    $endian_opcode = byte_swap($opcode_hex);
                    system("echo $pc: $endian_opcode | xxd -r -ps > file.bin");
		    @output = `arm-aahmfp-eabi-objdump -b binary -D -m arm -M force-thumb file.bin`;
		} else {
                    $endian_opcode = endian($opcode_hex);
                    system("echo $pc: $endian_opcode | xxd -r -ps > file.bin");
		    @output = `arm-aahmfp-eabi-objdump -b binary -D -m arm file.bin`;
		}
                $h = '[0-9a-fA-F]';
		foreach $l (@output) {
		    next if ($l =~ /file.bin:.*/);
                    if ($atd eq "T") {
                        if ($l =~ /^\s*4:\s+($h+) ($h+)\s+(.*)/i) {
                            print "    $3";
                            last;
                        }
                        elsif ($l =~ /^\s*4:\s+($h{4})\s+(.*)/i) {
                            print "    $2";
                            last;
                        }
                    } else {
                        if ($l =~ /^\s*4:\s+($h+)\s+(.*)/i) {
                            print "    $2";
                            last;
                        }
                    }
		}
	    }
	    print "\n";
	}
	if ($found_exception == 1 && $noet == 0 && $tick > 0) {
	    printf "%010d", $tick;
	    print " ns";
	    print " E ";
	    printf "%08s ", $pc;
	    printf "00000000 00000000 ";
	    print "$exception_value\n";
	}
	if ($found_mem_read == 1 && $nomt == 0 && $tick > 0) {
	    printf "%010d", $tick;
	    print " ns";
	    print " MNR";
	    print "$mem_read_size_value";
	    print "____ ";
	    printf "(%08s:%08s) ", $pc, $mem_read_adr_value;
	    printf "%08s %08s %08s", $mem_read_adr_value, $mem_read_data1, $mem_read_data2;
	    print "\n";
	    $found_mem_read = 0;
	}
	if ($found_mem_write == 1 && $nomt == 0 && $tick > 0) {
	    printf "%010d", $tick;
	    print " ns";
	    print " MNW";
	    print "$mem_write_size_value";
	    print "____ ";
	    printf "(%08s:%08s) ", $pc, $mem_write_adr_value;
	    printf "%08s %08s %08s", $mem_write_adr_value, $mem_write_data1, $mem_write_data2;
	    print "\n";
	    $found_mem_write = 0;
	}
	if ($found_gpr_writes0 == 1 && $found_gpr_writes0_valid == 1 && $nort == 0 && $tick > 0) {
	    printf "%010d", $tick;
	    print " ns";
	    print " R r";
	    print "$write0_num_value";
	    print "$write0_mode_value ";
	    printf "%08s", $write0_data_value;
	    print "\n";
	    #print "write0 r$write0_num_value$write0_mode_value $write0_data_value $found_gpr_writes0 $found_gpr_writes0_valid\n";
	}
	if ($found_gpr_writes1 == 1 && $found_gpr_writes1_valid == 1 && $nort == 0 && $tick > 0) {
	    printf "%010d", $tick;
	    print " ns";
	    print " R r";
	    print "$write1_num_value";
	    print "$write1_mode_value ";
	    printf "%08s", $write1_data_value;
	    print "\n";
	    #print "write1 r$write1_num_value$write1_mode_value $write1_data_value $found_gpr_writes1 $found_gpr_writes1_valid\n";
	}
	if ($found_spsr_write == 1 && $found_spsr_write_valid == 1 && $nort == 0 && $tick > 0) {
	    printf "%010d", $tick;
	    print " ns";
	    print " R ";
	    print "$spsr_write_num_value";
	    print "$spsr_write_mode_value ";
	    printf "%08s", $spsr_write_data_value;
	    print "\n";
	}
	if ($found_cpsr_write == 1 && $found_cpsr_write_valid == 1 && $nort == 0 && $tick > 0) {
	    printf "%010d", $tick;
	    print " ns";
	    print " R ";
	    print "$cpsr_write_num_value";
	    print "$cpsr_write_mode_value ";
	    printf "%08s", $cpsr_write_data_value;
	    print "\n";
	}
	$found_exception = 0;
	$found_instruction = 0;
	$found_coredata_valid = 0;
	$found_gpr_writes0 = 0;
	$found_gpr_writes0_valid = 0;
	$found_gpr_writes1 = 0;
	$found_gpr_writes1_valid = 0;
	$found_spsr_write = 0;
	$found_spsr_write_valid = 0;
	$found_cpsr_write = 0;
	$found_cpsr_write_valid = 0;
	$tick = $1;
    }
    # This is when we find 1 bit binary output in the vcd file
    elsif ($line =~ /^(\d)(\S+)/) {
	if ($2 eq $coredata_valid_symbol) {
	    if ($1 == 1) {
		$found_coredata_valid = $tick;
		print "coredata valid @ $tick\n" if ($debug);
	    }
	}
	if ($2 eq $mem_read_request_valid_symbol) {
	    if ($1 == 1) {
		$found_mem_read_request_valid = $tick;
		if ($found_mem_read_request == $tick) {
		    $found_mem_read_request_and_valid = 1;
		    $mem_read_adr_value = sprintf('%08x', oct("0b".$read_address_bits));
		    $mem_read_size_value = get_size($read_size_bits);
		}
	    }
	}
	if ($2 eq $mem_read_data_deq_symbol) {
	    if ($1 == 1) {
		if ($found_mem_read_request_and_valid == 1) {
		    $found_mem_read = 1;
		    $found_mem_read_request_and_valid = 0;
		}
	    }
	}
	if ($2 eq $mem_write_request_valid_symbol) {
	    if ($1 == 1) {
		$found_mem_write_request_valid = $tick;
		if ($found_mem_write_request == $tick) {
		    $mem_write_data1 = lower_32_hex($write_data_bits);
		    $mem_write_data2 = upper_32_hex($write_data_bits);
		    $mem_write_adr_value = sprintf('%08x', oct("0b".$write_address_bits));
		    $mem_write_size_value = get_size($write_size_bits);
		    $found_mem_write_request = 0;
		    $found_mem_write_request_valid = 0;
		    $found_mem_write = 1;
		}
	    }
	    #print "$tick Write Request Valid $1\n";
	}
	if ($2 eq $gpr_writes0_valid_symbol) {
	    $found_gpr_writes0_valid = $1;
	}
	if ($2 eq $gpr_writes1_valid_symbol) {
	    $found_gpr_writes1_valid = $1;
	}
	if ($2 eq $spsr_write_valid_symbol) {
	    $found_spsr_write_valid = $1;
	    #print "Found spsr valid $1 $2\n";
	}
	if ($2 eq $cpsr_write_valid_symbol) {
	    $found_cpsr_write_valid = $1;
	}
    }
    # This is when we find arbitary length binary output in the vcd file
    elsif ($line =~ /^b([0-1]+)\s+(\S+)/) {
	if ($2 eq $coredata_symbol) {

	    print "coredata $1 @ $tick\n" if ($debug);
	    # pc
	    $pc_bits = get_bits($1, 101, 0, 31);
	    $pc = sprintf('%X', oct("0b".$pc_bits));

	    # cpsr
	    $cpsr_bits = get_bits($1, 101, 32, 63);
	    $cpsr_tbit = get_bits($cpsr_bits, 32, 26, 26);
	    if ($cpsr_tbit eq "1") {
		$atd = "T";
	    }
	    else {
		$atd = "A";
	    }

	    # opcode
	    $opcode_valid = get_bits($1, 101, 64, 64);
	    $opcode_bits = get_bits($1, 101, 65, 96);
	    $opcode_hex = sprintf('%08x', oct("0b".$opcode_bits));

	   # if ($opcode_valid eq "1") {
	   #	$found_instruction = 1;
	   # }

	    # taken
	    $taken = get_bits($1, 101, 97, 97);
	    if ($taken eq "1") {
		$taken_value = "IT";
	    }
	    else {
		$taken_value = "IS";
	    }

	    # exception
	    $exception_bits = get_bits($1, 101, 98, 100);
	    if ($exception_bits eq "000") {
		$exception_value = "CoreEvent_Undef";
	    }
	    elsif ($exception_bits eq "001") {
		$exception_value = "CoreEvent_Undef";
	    }
	    elsif ($exception_bits eq "010") {
		$exception_value = "CoreEvent_Undef";
		$found_exception = 1;
	    }
	    elsif ($exception_bits eq "011") {
		$exception_value = "CoreEvent_SWI";
		$found_exception = 1;
	    }
	    elsif ($exception_bits eq "100") {
		$exception_value = "CoreEvent_PrefetchAbort";
		$found_exception = 1;
	    }
	    elsif ($exception_bits eq "101") {
		$exception_value = "CoreEvent_DataAbort";
		$found_exception = 1;
	    }
	    elsif ($exception_bits eq "110") {
		$exception_value = "CoreEvent_IRQ";
		$found_exception = 1;
	    }
	    elsif ($exception_bits eq "111") {
		$exception_value = "CoreEvent_FIQ";
		$found_exception = 1;
	    }
	    
	    $pc = sprintf('%X', oct("0b".$pc_bits));
	}
	if ($2 eq $mem_read_data_symbol) {
	    $mem_read_data1 = lower_32_hex($1);
	    $mem_read_data2 = upper_32_hex($1);
	    if ($found_mem_read_request_and_valid == 1) {
		$found_mem_read = 1;
		$found_mem_read_request_and_valid = 0;
	    }
	    #print "$tick Response\n";
	}
	if ($2 eq $mem_read_request_symbol) {
	    $found_mem_read_request = $tick;
	    $read_address_bits = get_bits($1, 145, 41, 72);
	    $read_size_bits = get_bits($1, 145, 34, 36);
	    if ($found_mem_read_request_valid == $tick) {
		$found_mem_read_request_and_valid = 1;
		$mem_read_adr_value = sprintf('%08x', oct("0b".$read_address_bits));
		$mem_read_size_value = get_size($read_size_bits);
	    }
	    #print "$tick Request($read_address_bits, $read_size_bits)\n";
	}
	if ($2 eq $mem_write_request_symbol) {
	    $found_mem_write_request = $tick;
	    $write_data_bits = get_bits($1, 145, 81, 144);
	    $write_address_bits = get_bits($1, 145, 41, 72);
	    $write_size_bits = get_bits($1, 145, 34, 36);
	    if ($found_mem_write_request_valid == $tick) {
		$mem_write_data1 = lower_32_hex($write_data_bits);
		$mem_write_data2 = upper_32_hex($write_data_bits);
		$mem_write_adr_value = sprintf('%08x', oct("0b".$write_address_bits));
		$mem_write_size_value = get_size($write_size_bits);
		$found_mem_write_request = 0;
		$found_mem_write_request_valid = 0;
		$found_mem_write = 1;
	    }
	    #print "$tick Write Request adr: $write_address_bits\n";
	}
	if ($2 eq $gpr_writes0_symbol) {
	    #print "Found writes0 activity $1 $2\n";
	    $write0_num = get_bits($1, 41, 0, 3);
	    $write0_num_value = sprintf('%d', oct("0b".$write0_num));
	    $write0_mode = get_bits($1, 41, 4, 8);
	    $write0_mode_value = get_mode($write0_mode);
	    $write0_data = get_bits($1, 41, 9, 40);
	    $write0_data_value = sprintf('%08x', oct("0b".$write0_data));
	    #print "Found writes0 num $write0_num $write0_num_value mode $write0_mode $write0_mode_value data $write0_data_value\n";
	    $found_gpr_writes0 = 1;
	}
	if ($2 eq $gpr_writes1_symbol) {
	    #print "Found writes1 activity $1 $2\n";
	    $write1_num = get_bits($1, 41, 0, 3);
	    $write1_num_value = sprintf('%d', oct("0b".$write1_num));
	    $write1_mode = get_bits($1, 41, 4, 8);
	    $write1_mode_value = get_mode($write1_mode);
	    $write1_data = get_bits($1, 41, 9, 40);
	    $write1_data_value = sprintf('%08x', oct("0b".$write1_data));
	    #print "Found writes1 num $write1_num_value mode $write1_mode_value data $write1_data_value\n";
	    $found_gpr_writes1 = 1;
	}
	if ($2 eq $spsr_write_symbol) {
	    $gpr_valid = get_bits($1, 48, 0, 0);
	    $psr_valid = get_bits($1, 48, 5, 5);
	    if ($gpr_valid eq "1" && $psr_valid eq "1") {
		$spsr_write_num = get_bits($1, 48, 1, 4);
		$spsr_write_num_value = sprintf('r%d', oct("0b".$spsr_write_num));
		$spsr_mode = get_bits($1, 48, 6, 10);
		$spsr_mode_value = get_mode($spsr_mode);
	    }
	    elsif ($gpr_valid eq "0"&& $psr_valid eq "1") {
		$spsr_write_num_value = "spsr";
		$spsr_mode = get_bits($1, 48, 6, 10);
		$spsr_mode_value = get_mode($spsr_mode);
	    }
	    elsif ($gpr_valid eq "1"&& $psr_valid eq "0") {
		$spsr_write_num = get_bits($1, 48, 1, 4);
		$spsr_write_num_value = sprintf('r%d', oct("0b".$spsr_write_num));
		$spsr_mode = get_bits($1, 48, 6, 10);
		$spsr_mode_value = get_mode($spsr_mode);
	    }
	    $spsr_write_data = get_bits($1, 48, 27, 48);
	    $spsr_write_data_value = sprintf('%08x', oct("0b".$spsr_write_data));
	    $found_spsr_write = 1;
	}
	if ($2 eq $cpsr_write_symbol) {
	    $gpr_valid = get_bits($1, 48, 0, 0);
	    $psr_valid = get_bits($1, 48, 5, 5);
	    if ($gpr_valid eq "1" && $psr_valid eq "1") {
		$cpsr_write_num = get_bits($1, 48, 1, 4);
		$cpsr_write_num_value = sprintf('r%d', oct("0b".$cpsr_write_num));
		$cpsr_mode = get_bits($1, 48, 6, 10);
		$cpsr_mode_value = get_mode($cpsr_mode);
	    }
	    elsif ($gpr_valid eq "0"&& $psr_valid eq "1") {
		$cpsr_write_num_value = "cpsr";
		$cpsr_mode = get_bits($1, 48, 6, 10);
		$cpsr_mode_value = get_mode($cpsr_mode);
	    }
	    elsif ($gpr_valid eq "1"&& $psr_valid eq "0") {
		$cpsr_write_num = get_bits($1, 48, 1, 4);
		$cpsr_write_num_value = sprintf('r%d', oct("0b".$cpsr_write_num));
		$cpsr_mode = get_bits($1, 48, 6, 10);
		$cpsr_mode_value = get_mode($cpsr_mode);
	    }
	    $cpsr_write_data = get_bits($1, 48, 27, 48);
	    $cpsr_write_data_value = sprintf('%08x', oct("0b".$cpsr_write_data));
	    $found_cpsr_write = 1;
	}
    }
    else {
	#print $line;
    }
}
close(C);

sub byte_swap {
    my ($val) = @_;
    my @f = split(//,$val);
    my $res = "";
    $res .= $f[2];
    $res .= $f[3];
    $res .= $f[0];
    $res .= $f[1];
    $res .= $f[6];
    $res .= $f[7];
    $res .= $f[4];
    $res .= $f[5];
    return $res;
}		    

sub hex2bin {
    my ($hex,$sz) = @_;
    ($bin = $hex) =~ s/(.)/$h2b{lc $1}/g;
    $pad = $sz - length($bin);
    $bin = "0"x$pad . $bin;
    return $bin;
}
