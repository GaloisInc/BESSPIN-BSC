#!/usr/bin/perl

# usage: ReduceV.pl foo.v
#        original copied to foo.v~

# reduce this redundent stuff from the output.v file
#	     start_elementInstr_BITS_15_TO_12___d302 != 4'd7 &&
#	     start_elementInstr_BITS_15_TO_12___d302 != 4'd8 &&
#	     start_elementInstr_BITS_15_TO_12___d302 != 4'd9 &&
#	     start_elementInstr_BITS_15_TO_12___d302 == 4'd10 ;

my $name = "";
my $sz   = 0;

foreach $file ( @ARGV ) {
    my $backup = "$file~";
    system "cp -p $file $backup";

    open(FILE, ">$file") || next;
    open(BACKUP,"$file~") || next;

    my @lines  = <BACKUP>;
    my $before = $#lines;
    my $after  = 0;

    # loop backwards for lines like this 
    # reduce this
    #	     start_elementInstr_BITS_15_TO_12___d302 != 4'd7 &&
    #	     start_elementInstr_BITS_15_TO_12___d302 != 4'd8 &&
    #	     start_elementInstr_BITS_15_TO_12___d302 != 4'd9 &&
    #	     start_elementInstr_BITS_15_TO_12___d302 == 4'd10 ;
    for ($i=$#lines-1; $i >= 0; $i--) {

        # look for start_elementInstr_BITS_15_TO_12___d302 == 4'd10 ;
        if ($lines[$i] =~ /^\s+([\w_]+) == (\d+)\'d(\d+) &&\s*$/) {
            $name = $1;
            $sz   = $2;
            $val  = $3;
            $after++;
        } 

        # if the name and != matches last name we may be able to delete it
        # if it's the same name, last was ==, this is != seems okay
        elsif ($lines[$i] =~ /^\s+([\w_]+) != (\d+)\'d(\d+) &&\s*$/) {
            if (($1 eq $name) && ($2 == $sz) && (--$val == $3)) {
                $lines[$i] = '';  # just delete the line
            } else {
                $name = "";
                $sz = 0;
                $after++;
            }
        } 

        # changed line, so reset 
        else {
            $name = "";
            $sz   = 0;
            $after++;
        }
    }

    my $percent = 100.0 - (($after * 100) / $before);
    printf "File $file, before $before, after $after, reduced by %4.2f%%\n", $percent;
    print FILE @lines;

    close FILE;
    close BACKUP;
}
