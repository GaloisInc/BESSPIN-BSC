#!/usr/bin/perl
################################################################################

$verbose = 0;

use Getopt::Long;
GetOptions(
            "verbose"=>\$verbose
           );

$info = $ARGV[0]; # top level

do parseInfo( "/main/top", $info );

$html = $info;
s/.info$/.html/g;

do printHTML( $html );

######################################################################
######################################################################
sub parseInfo {
    my ($hier, $info) = @_;

    return if ($alreadyRead{$info} == 1);
    $alreadyRead{$info} = 1;

    local *FILE;
    my $ln;
    return unless (open(FILE,$info));

    while ($ln = <FILE>) {
        next unless ($ln =~ /^\(/);
        
        if ($ln =~ /^\(compiler\-version \"(\S+)\"/) {  $compiler_version = $1; }
        elsif ($ln =~ /^\(build\-version \"(\S+)\"/) {  $build_version = $1; }
        elsif ($ln =~ /^\(bluespec\-dir \"(\S+)\"/)  {  $bluespec_dir = $1; }
        elsif ($ln =~ /^\(bsv-inst/) {
            # get lines until parens match
            for (;;) {
                # count right and left parens
                $lcnt = ($ln =~ s/\(/\(/g);
                $rcnt = ($ln =~ s/\)/\)/g);
                last if ($lcnt == $rcnt);
                $ln .= <FILE>;
            }



            $ln = readUntilMatchingParen( FILE, $ln );
        }
        elsif ($ln =~ /^\(add\-bsv\-id\-info/) {
            my $x;
            my @f = split(" ", $ln);
            foreach $x (@f) {
                $x =~ s/\"//g;
            }

            my ($n, $mod, $inst, $vlin, $char, $vmod,  $blin, $bchar, $bmod, $typ) = @f;

            if (($typ eq "FLATTENED_DEFOF") ||($typ eq "DEFOF")) {
                next unless (-e "$mod.info");
                $defof{$mod,$inst} = @f;
            }

            elsif (($typ eq "BINSTOF") ||($typ eq "INSTOF")) {
                $instof{$mod,$inst} = @f;
                next unless (-e "$mod.info");
                do parseInfo( "$hier/$inst", "$mod.info" );
            }

            # INST or BINST
            # NET or ASSIGN
        }
    }
}


######################################################################
######################################################################
sub printHTML {
    my ($html) = @_;

}

