#!/usr/bin/perl --
# -*-Perl-*-

##################################################################################
# Parse command line and assign defaults
##################################################################################

# $vlog_file = shift @ARGV ;
# $mod_file  = shift @ARGV ;

# $vlog_file || &usage ; 
# $mod_file  || &usage ;

##################################################################################
# Open file and process 
##################################################################################

use File::Basename;

$ProgName	= $0;

usage() if GetCommandLine('-f: file_0 file_1:');

if ($opt_f ne '') {
    if (-e $opt_f) {
	open (L_FILE, "$opt_f")  || die "FATAL: Cannot open '$opt_f'\n$!\n" ;
    } else {
	die "FATAL: The file '$opt_f' does not exist.\n$!\n" ;
    }

    while (<L_FILE>)
    {
	$current = $_;
	if ($current =~ /(\W*)(\S+)\W*$/) 
	{
	    $vlog_file = "$1$2";
	    if (-e $vlog_file) {
		($base, $path, $suffix) = fileparse($vlog_file);
		system "mkdir -p .ee_tmp";
		$tmp_file = ".ee_tmp/$base";
		system "/bin/cp $vlog_file $tmp_file";

		if (-e $file_0) {
		    if (-d $file_0) {
			open (VLOG_FILE, "$tmp_file")  || die "FATAL: Cannot open '$tmp_file'\n$!\n" ;		
			open (MOD_FILE, ">$file_0/$base") || die "FATAL: Cannot open '$file_0/base'\n$!\n" ;

			while (<VLOG_FILE>)
			{
			    $current = $_;
			    &process_one_line ;
			    print MOD_FILE $current;
			}
			close (MOD_FILE) ;
			close (VLOG_FILE) ;

##		system "/bin/rm $tmp_file";
		    } else {
			die "FATAL: '$file_0' is not a directory\n$!\n" ;
		    }
		} else {
		    die "FATAL: The directory '$file_0' does not exist\n$!\n" ;
		}

	    } else {
		printf stdout "WARNING: the file '$vlog_file' does not exist.\n"
	    }
	}
    }

    close (L_FILE) ;
    exit 0 ;
}

system "/bin/cp $file_0 $file_0.orig";

open (VLOG_FILE, "$file_0.orig")  || die "FATAL: Cannot open '$file_0.orig'\n$!\n" ;
open (MOD_FILE, ">$file_1") || die "FATAL: Cannot open '$file_1'\n$!\n" ;


while (<VLOG_FILE>)
{

    $current = $_;
    &process_one_line ;
    print MOD_FILE $current;
}
close (MOD_FILE) ;
close (VLOG_FILE) ;

system "/bin/rm $file_0.orig";
system "/bin/rm -rf .ee_tmp";

exit 0 ;

sub process_one_line
{

    $changed = 1;
    while ($changed)
    {
	$changed = 0;
	## printf stdout "ENTER: $current";
	if ($current =~ /^(\W*)wire(\W+)(.*)$/) 
	{
	    $current = "$1(* KEEP = \"TRUE\" *) wire$2$3\n";
	    next;
	}
	if ($current =~ /^(\W*)reg(\W+)(.*)$/) 
	{
	    $current = "$1(* KEEP = \"TRUE\" *) reg$2$3\n";
	    next;
	}
	if ($current =~ /^(\W*)output(\W+)reg(\W+)(.*)$/) 
	{
	    $current = "$1(* KEEP = \"TRUE\" *) output$2reg$3$4\n";
	    next;
	}
	if ($current =~ /^.*\{.*\}.*$/)
	{
	    next;
	}
	if ($current =~ /^(\W*assign\W+\S+\W+=\W+)'.*$/)
	{
	    next;
	}
	if ($current =~ /^(\W*assign\W+\S+\W+=\W+)(\S+)(\W*;\W*\/\/.*)$/)
	{
	    $current = "$1\($2 ^ 0\)$3\n";
	    next;
	}

	if ($current =~ /^(\W*assign\W+\S+\W+=\W+)(\S+)(\W*;\W*)$/)
	{
##	    $current = "$1~\(~$2\)$3\n";
	    $current = "$1\($2 ^ 0\)$3\n";
	    next;
	}
    }
}
	
##################################################################################
# Sub-routine 
##################################################################################

sub usage
{
    die "

Usage:	$ProgName <vlog_file> <vlog_file_mod>\n\n" ;

}

#* ---------------------------- sub GetCommandLine 4.0 ------------------------ *#
# last modified : 94-10-18
#
# multi-char switch requiring argument
# multi-char switch
# single-char switch requiring argument
# multiple single-char switches bunched with one -
# single-char switch
# single-char array switch requiring argument
# required arguments
# optional arguments
# array arguments
# optional array arguments
#
# Process command-line switches with switch clustering on single character switches.
# Will also look for arguments which may or may not be optional and
#   will be assigned in order of appearance on command line.
# Pass GetCommandLine a string of arguments and switch names
# For each switch found, sets $opt_{x} (where x is the switch name) to the value of the
#   argument, or is left undefined if no argument.  Single character switches which take an
#   argument don't care whether or not there is a space between the switch and the argument.
#   Switches requiring no argument have $opt_{x} (where x is the switch name) set to 1.
# For each argument found, sets ${argument} to the value on the command line
#   or is left undefined if no argument are defined on command line.
# An array argument slurps all following command-line argument into array_name ... 
#   unless there is a required argument following the array argument.
# No optional arguments may follow an array argument.
#
# Usage:
#      &GetCommandLine('-a -b: -cde -fgh: foo bar:') ;
#               -a     is an optional switch                            (anywhere on cmd-line)
#               -b:    is an optional switch which requires an argument (anywhere on cmd-line)
#                      does not require a space between switch and argument
#               -c@    is an optional switch which requires an argument (anywhere on cmd-line)
#                      can occur multiple times on cmd-line. sets @opt_c
#                      does not require a space between switch and argument
#               -def   is an optional multi-character switch            (anywhere on cmd-line)
#               -ghi:  is an optional switch which requires an argument (anywhere on cmd-line)
#                foo   is an arguement that is required (error returned if not found)
#                bar:  is an optional argument (obviously, all optional arguments are required
#                      to exist after all required arguments.)
#                @bam  is an array argument
#                @bam: is an optional array argument 
#
# returns number of errors found (0 if OK)
#
sub GetCommandLine
{
    local($num_errors)  = 0 ;
    local($search_list, @search_list, $arg, $this, $that, $i, $j, $k, $array, $optional) ;

    $search_list = @_[0] ;
    $search_list =~ s/[\s,\|]+/ /g ;                  ## clean up demarcations
    $search_list =~ s/^\s+// ;
    $search_list =~ s/\s+$// ;
    @search_list = split(/ /, $search_list) ;         ## split input arg into search_list
    print "## @ARGV\n"        if ($debugd) ;

    ## [1] loop through search list to find switch arrays ##
    print "## (1) \'@search_list\' <@ARGV>\n" if ($debugd) ;
    for ($i=0 ; $i <= $#search_list ; $i++)
    {
        $this = $search_list[$i] ;                    ## next switch or argument
        if ($this =~ /^-(\w+)@/)                      ## single-char switch, multiply defined?
        {
            $that = $1 ;
	    print "[1]# $that <@ARGV>\n" if ($debugd) ;
	    splice(@search_list, $i, 1) ;             ## remove switch from search_list
	    $i-- ;
            for ($j=0 ; $j <= $#ARGV ; $j++)          ## search through command line
            {
                if ($ARGV[$j] =~ /^-($that)(.*)/)     ## does cmd-line have this switch?
                {
                    $arg = $2 ;                       ## if no space between switch & arg
                    splice(@ARGV, $j, 1) ;            ## remove switch{arg} from ARGV
                    if ($arg eq "")                   ## assume there is a space
                    {
                        if (($ARGV[$j] =~ /^-/) || ($ARGV[$j] eq ""))
                        {
                            print STDERR "Switch $this requires an argument\n" ;
                            $num_errors++ ;
                        }
                        else
                        {
                            $arg = $ARGV[$j] ;        ## next field was clean argument
                            splice(@ARGV, $j, 1) ;    ## remove arg from ARGV
                        }
                    }
                    eval "push(\@opt_$that,\$arg) ;" ;
                    eval "print \"\\\@opt_$that set to (\@opt_$that)\n\"" if $debugc ;
		    $j-- ;
                }
            }
	}
    }

    ## [2] loop through search list to find multi-char switch requiring argument ##
    print "## (2) \'@search_list\' <@ARGV>\n" if ($debugd) ;
    for ($i=0 ; $i <= $#search_list ; $i++)
    {
        $this  = $search_list[$i] ;                   ## next switch or argument
        if ($this =~ /^-(\w\w+):/)                    ## multi-char switch w/ argument?
        {
            $that = $1 ;
	    print "[2]# $that <@ARGV>\n" if ($debugd) ;
	    splice(@search_list, $i, 1) ;             ## remove switch from search_list
	    $i-- ;
            for ($j=0 ; $j <= $#ARGV ; $j++)          ## search through command line
            {
                if ($ARGV[$j] =~ /^-$that/)           ## does cmd-line have this switch?
                {
                    splice(@ARGV, $j, 1) ;            ## remove switch{arg} from ARGV
                    if (($ARGV[$j] =~ /^-/) || ($ARGV[$j] eq ""))
                    {
                        print STDERR "Switch $this requires an argument\n" ;
                        $num_errors++ ;
                    }
                    else
                    {
                        eval "\$opt_$that = \$ARGV[\$j];" ;
                        eval "print \"opt_$that set to (\$opt_$that)\n\"" if $debugc ;
                        splice(@ARGV, $j, 1) ;        ## remove arg from ARGV
                    }
                }
            }
        }
    }

    ## [3] loop through search list to find multi-char switch ##
    print "## (3) \'@search_list\' <@ARGV>\n" if ($debugd) ;
    for ($i=0 ; $i <= $#search_list ; $i++)
    {
        $this = $search_list[$i] ;                    ## next switch or argument
        if ($this =~ /^-(\w\w+)/)                     ## multi-char switch w/ argument?
        {
            $that = $1 ;
	    next if $that =~ /\@/ ;
	    print "[3]# $that <@ARGV>\n" if ($debugd) ;
	    splice(@search_list, $i, 1) ;             ## remove switch from search_list @@@@
	    $i-- ;
            for ($j=0 ; $j <= $#ARGV ; $j++)          ## search through command line
            {
                if ($ARGV[$j] =~ /^-$that$/)          ## does cmd-line have this switch?
                {
                    eval "\$opt_$that = 1 ;" ;
                    eval "print \"opt_$that set to (\$opt_$that)\n\"" if $debugc ;
                    splice(@ARGV, $j, 1) ;            ## remove switch from ARGV
                    last ;
                }
            }
        }
    }

    ## [4] loop through search list to find single-char switches requiring argument ##
    print "## (4) \'@search_list\' <@ARGV>\n" if ($debugd) ;
    for ($i=0 ; $i <= $#search_list ; $i++)
    {
        $this = $search_list[$i] ;                    ## next switch or argument
        if ($this =~ /^-(\w):/)                       ## single-char switch w/ argument?
        {
            $that = $1 ;
	    print "[4]# $that <@ARGV>\n" if ($debugd) ;
	    splice(@search_list, $i, 1) ;             ## remove switch from search_list
	    $i-- ;
            for ($j=0 ; $j <= $#ARGV ; $j++)          ## search through command line
            {
                if ($ARGV[$j] =~ /^-($that)(.*)/)     ## does cmd-line have this switch?
                {
                    $arg = $2 ;                       ## if no space between switch & arg
                    splice(@ARGV, $j, 1) ;            ## remove switch{arg} from ARGV
                    if ($arg eq "")                   ## assume there is a space
                    {
                        if (($ARGV[$j] =~ /^-/) || ($ARGV[$j] eq ""))
                        {
                            print STDERR "Switch $this requires an argument\n" ;
                            $num_errors++ ;
                        }
                        else
                        {
                            $arg = $ARGV[$j] ;        ## next field was clean argument
                            splice(@ARGV, $j, 1) ;    ## remove arg from ARGV
                        }
                    }
                    eval "\$opt_$that = \$arg ;" ;
                    eval "print \"opt_$that set to (\$opt_$that)\n\"" if $debugc ;

                    last ;
                }
            }
        }
    }

    ## [5] loop through search list to find single-char switches that are bunched together ##
    print "## (5) \'@search_list\' <@ARGV>\n" if ($debugd) ;
    for ($i=0 ; $i <= $#search_list ; $i++)
    {
        $this = $search_list[$i] ;                    ## next switch or argument
        if ($this =~ /^-(\w@*)$/)                     ## single-char switch?
        {
            $that = $1 ;
	    next if $that =~ /\@/ ;
	    print "[5]# $that <@ARGV>\n" if ($debugd) ;
	    splice(@search_list, $i, 1) ;             ## remove switch from search_list
	    $i-- ;
            for ($j=0 ; $j <= $#ARGV ; $j++)          ## search through command line
            {
                if ($ARGV[$j] =~ /(^-\w*)$that(\w*)/) ## is this switch in a bunch of switches?
                {
		    $before = $1 ;
		    $after  = $2 ;
                    eval "\$opt_$that = 1 ;" ;
                    eval "print \"(bunch) opt_$that set to (\$opt_$that)\n\"" if $debugc ;
		    $ARGV[$j] =~ s/$before$that$after/$before$after/ ;
		    splice(@ARGV, $j, 1) if ($ARGV[$j] eq '-') ;
                    last ;
                }
            }
        }
    }

    ## [6] loop through search list to find plain single-char switches ##
    print "## (6) \'@search_list\' <@ARGV>\n" if ($debugd) ;
    for ($i=0 ; $i <= $#search_list ; $i++)
    {
        $this = $search_list[$i] ;                    ## next switch or argument
        if ($this =~ /^-(\w{2,}@*)/)                     ## plain-old switch
        {
            $that = $1 ;
	    next if $that =~ /\@/ ;
	    print "[6]# $that <@ARGV>\n" if ($debugd) ;
            for ($j=0 ; $j <= $#ARGV ; $j++)            ## search through command line
            {
                if ($ARGV[$j] =~ /^-$that/)           ## does cmd-line have this switch?
                {
                    eval "\$opt_$that = 1 ;" ;
                    eval "print \"opt_$that set to (\$opt_$that)\n\"" if $debugc ;
                    splice(@ARGV, $j, 1) ;            ## remove switch from ARGV
		    splice(@search_list, $i, 1) ;     ## remove switch from search_list
		    $i-- ;
                    last ;
                }
            }
        }
    }

    ## check for illegal switches ##
    for ($j=0 ; $j<=$#ARGV ; $j++)
    {
        if ($ARGV[$j] =~ /^-/)
        {
            print STDERR "Unknown switch : $ARGV[$j]\n" ;
            $num_errors++ ;
	    splice(@ARGV, $j, 1) ;    ## remove arg from ARGV
	    $j-- ;
        }
    }

    for ($i=0 ; $i <= $#search_list ; $i++)
    {
        $this = $search_list[$i] ; ## next switch or argument

	$OK       = 0 ;
	$this     =~ /^(@*)(\w*)(:*)$/ ; ## is this argument optional or an array?
	$array    = $1  ;	         ## is this an array argument
	$that     = $2 ;	         ## name of variable
	$optional = $3 ;	         ## optional flag

	if ($array)
	{
	    ## first determine if there are any arguments after the array ##
	    for($j=$i+1, $num_save=0 ; $j <= $#search_list ; $j++)
	    {
		$num_save++ if ($search_list[$j] !~ /^-/) ;
	    }

	    $j = 0 ;
	    $k = 0 ;
	    while($j <= ($#ARGV - $num_save)) ## search through command line
	    {
		eval "\$$that[\$k] = \$ARGV[\$j];"; ## set global variable to value
		eval "print \"\$that[\$k] set to (\$$that[\$k])\n\"" if $debugc ;
		splice(@ARGV, $j, 1) ; ## remove argument from ARGV
		$k++ ;
		$OK++ ;
	    }
	}
	else
	{
	    for ($j=0 ; $j<=$#ARGV ; $j++) ## search through command line
	    {
		eval "\$$that = \$ARGV[\$j];"; ## set global variable to value
		eval "print \"\$that set to (\$$that)\n\"" if $debugc ;
		splice(@ARGV, $j, 1) ; ## remove argument from ARGV
		$OK++ ;
		last ;
	    }
	}

	if (! $OK)		## if not found on command line
	{
	    if (! $optional)	## only complain if not optional
	    {
		print STDERR "Missing required argument for : $that\n" ;
		$num_errors++ ;
	    }
	    else
	    {
		print "optional argument $that left undefined\n" if $debugc ;
	    }
	}

    }
    
    ## check for Extra arguments ##
    for ($j=0 ; $j<=$#ARGV ; $j++)
    {
	print STDERR "Extra argument : $ARGV[$j]\n" ;
	$num_errors++ ;
    }

    return $num_errors ;
}

