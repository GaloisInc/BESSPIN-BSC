#!/usr/bin/perl

$pwd = `pwd`;
chop($pwd);

for $info (@ARGV) {

    $v = $info;
    $v =~ s/\.info$/\.v/;

    @conv = ();
    
    # read verilog file
    open(VFILE,"$v") || die "Can't open $v ";
    $line = 1;
    foreach $_ (<VFILE>) {
        @ch = split('', $_);
        $col = 0;
        # create array
        foreach $c (@ch) {
            push (@conv, "$line.$col");
            $col++;
        }
        $line++;
    }
    close (VFILE);
    
    # cleanup info file
    system "cp $info $info~";
    open(IFILE,"$info") || die "Can't open $info ";
    @lines = <IFILE>;
    close IFILE;
    
    open(OFILE,">$info") || die "Can't open $info ";
    foreach $_ (@lines) {
        s@$pwd/@@g;
        # next if (/\"UNNAMED\"/);
        if  (! /^\(add-bsv-id-info/) {
            print OFILE $_;
            next;
        }

        s@/\"@\"@g;
        s@$pwd/@@;
        
        @f = split(/\s+/);
        if ($f[4] == 1) {
            ($ln,$col) = split(/\./, $conv[$f[5]]);
            $f[4] = $ln;
            $f[5] = $col;
            print OFILE join(' ', @f) . "\n"; 
        }
        else {
            print OFILE $_;
        }
    }
}
