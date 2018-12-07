#!/usr/bin/perl

if ($ARGV[0] eq "-help") {
    help();
}

open(C, "$ARGV[0]") || file_not_found();

$clk = $ARGV[1];
$enddef = 0;
$tick = 0;
@block = ();

if ($clk eq "") {
    print STDERR "Name of controlled clock is not given.\n";
    usage();
}

sub file_not_found {
    print STDERR "Original vcd file $ARGV[0] is not found.\n";
    usage();
}

sub usage {

    print STDERR "Usage: cclock_view.pl [original vcd file] [full path name of the relevant cclock] > [new vcd filename]\n";
    exit;
}

sub help {
    print STDERR "\ncclock_view.pl is a tool for modifying a vcd file to move the data in original uclock domain to be in a new given cclock domain.  All the clock ticks of the original uclock are stripped out and new clock ticks are inserted as the given relevant cclock is toggled.  Multiple toggling of a signal is now possible within a cclock period and maybe incorrectly displayed by the vcd viewer.\n\n";

    usage();
    exit;
}

#my @lines = <C>;

#foreach $line (@lines) {

while (<C>) {

    $line = $_;

    if ($line =~ /^\$enddefinitions/) {
	if ($symbol eq "") {
	    print STDERR "The given controlled clock $clk is not found in the vcd file.\n";
	    exit;
	}
	$enddef = 1;
	print $line;
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
	print $line;
    }
    elsif ($line =~ /\$upscope\s+\$end/) {
	$sofar = $1 if $sofar =~ /(\S+)[\.\/]\w+/;
	print $line;
    }
    elsif ($line =~ /\$var\s+(wire|reg)\s+1\s+(\S+)\s+(\w+)\s+\$end/) {
	$signal = $3;
	$fullname = "$sofar.$signal";
	if ($fullname eq $clk) {
	    $symbol = munge_symbol($2);
	    #print "Found symbol $symbol for $fullname\n";
	}
	print $line;
    }
    elsif ($line =~ /^#\d+/) {
	if ($foundClk == 1) {
	    print "#$tick\n";
	    $tick++;
	    $foundClk = 0;
	}
	if (scalar(@block) > 0) {
	    foreach $line (@block) {
		print $line;
	    }
	    @block = ();
	}
    }
    elsif ($line =~ /^\d$symbol$/) {
	$foundClk = 1;
	#print "Found a tick: $symbol $line";
	push (@block, $line);
    }
    else {
	if ($enddef == 1) {
	    push (@block, $line);
	}
	else {
	    print $line;
	}
    }
}
close(C);

if ($foundClk == 1) {
    print "#$tick\n";
    $tick++;
    $foundClk = 0;
}
if (scalar(@block) > 0) {
    foreach $line (@block) {
	print $line;
    }
    @block = ();
}


sub munge_symbol {
    my $sym = $_[0];

    $sym =~ s/\\/\\\\/g;
    $sym =~ s/\$/\\\$/g;
    $sym =~ s/\+/\\\+/g;
    $sym =~ s/\./\\\./g;
    $sym =~ s/\*/\\\*/g;
    $sym =~ s/\?/\\\?/g;
    $sym =~ s/\^/\\\^/g;
    $sym =~ s/\|/\\\|/g;
    $sym =~ s/\[/\\\[/g;
    $sym =~ s/\]/\\\]/g;
    $sym =~ s/\(/\\\(/g;
    $sym =~ s/\)/\\\)/g;
    $sym =~ s/\,/\\\,/g;
    $sym =~ s/\'/\\\'/g;
    $sym =~ s/\"/\\\"/g;

    return $sym;
}


