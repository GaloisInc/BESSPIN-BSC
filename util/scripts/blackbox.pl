#!/usr/bin/perl --
# -*-Perl-*-
################################################################################
################################################################################

$vlog_file = shift;
$outfile   = shift;

unless($vlog_file && $outfile) {
    print STDERR "\nERROR (blackbox) : Missing input verilog file or output location\n\n";
    print STDERR "Usage: blackbox <input verilog> <black box out file>\n\n";
    exit -1;
}

open(FILE, $vlog_file);
my @lines = <FILE>;
close(FILE);

my $inmodule = 0;
my $inports = 0;
my @newlines;
foreach my $line (@lines) {
    if ($line =~ m/^\s*module\s*[a-zA-Z0-9_\$]+\s*\(\s*\.(\S+)\(([a-zA-Z0-9_\$]+)\)/) {
	$inmodule = 1;
	$inports = 1;
	push @newlines, $line;
    } elsif ($line =~ m/^\s*module\s+(\S+)/) {
	$inmodule = 1;
	$inports = 1;
	push @newlines, $line;
    } elsif ($line =~ m/.*?\)\;/ && $inmodule && $inports) {
	push @newlines, $line;
	$inports = 0;
    } elsif ($line =~ m/\s*(assign|wire|reg|always)/ && $inmodule && !$inports) {
	$inmodule = 0;
    } elsif ($line =~ m/\s*endmodule/) {
	push @newlines, $line;
    } elsif ($inmodule) {
	push @newlines, $line;
    }
}

# write out the new version
open(OFILE, ">${outfile}") or die("Could not create output file: $!\n");
print OFILE @newlines;
close(OFILE);

1;
