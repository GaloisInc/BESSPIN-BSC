#!/usr/bin/perl
################################################################################

# Warning: "Zebu.bsv", line 81, column 29: (P0200)
#   No scheduling annotation given between method `readyForClockNegEdge' and
#   method `readyForClock' Assuming conflict.

# turn all of these into schedule lines to be added to your import BVI :)

while (<>) {
    if (/Warning: .* line (\d+), column (\d+): \(P0200\)/) {
        $ln = $1;
        $col = $2;
        $line = <> . <>;

        $line =~ s/(\`|\'|\n|\)|\()//g;
        $line =~ s/methods/method/g;
        $line =~ s/\s+/ /g;
        $line =~ s/, /,/g;

        if ($line =~ /between method (\S+) and method (\S+) Assuming (\S+)\./) {
            @ins = ($1);
            @outs = ($2);
            $order = $3;
        } else {
            die "can't figure out '$line'";
        }

        $inputs  = join(",", @ins);
        $outputs = join(",", @outs);
        if ($order eq "conflict") {
            $type = "C";
        } elsif ($order eq "conflict-free") {
            $type = "CF";
        } elsif ($order eq "sequenced-before") {
            $type = "SB";
        } else {
            die "don't know what '$order' is";
        }

        print "  schedule ( $inputs ) $type ( $outputs ); // line $ln col $col\n";
    }
}
