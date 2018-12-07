#!/usr/bin/perl --
# -*-Perl-*-
#

# ESEComp-only options; filter these out before passing args to bsc
@ESECOMPARGS = qw/
	      -g@
	      -x:
	      -bsv:
	      /;

# 2-arg options, plus args that apply to both ESEComp and bsc
@BSCFILTER = qw/
		-verbose
		-D:
		-I:
		-L:
		-bdir:
		-e:
		-i:
		-l:
		-licenseWarning:
		-o:
		-p:
		-scheduler-effort:
		-simdir:
		-steps:
		-steps-max-interval:
		-steps-warn-interval:
		-unspecified-to:
		-vdir:
		-vsim:
		file
		@other_files:
		/;

################################################################################
###    $args =~ s/$_\s+.+? //;

################################################################################

use File::Basename;

################################################################################
###
################################################################################

$ProgName	= $0;

$BaseName = basename($ProgName,"");
$DirName = dirname($ProgName,"");

$bluedir = $ENV{BLUESPECDIR};

# These tests are redundant since the bin/bsc script tests these for us.

if ($bluedir eq "") {
    print STDERR "Error: The environment variable \"BLUESPECDIR\" is not set.\n" ;
    exit 1 ;
}

if (! -d $bluedir) {
    print STDERR "Error: \$BLUESPECDIR does not exist ($BLUESPECDIR)";
    exit 1 ;
}

$env_options = $ENV{BSC_OPTIONS};

################################################################################
### Parse command line.
################################################################################

$include_env = 1;
for ($j=0 ; $j <= $#ARGV ; $j++) { 
  if ($ARGV[$j] =~ /^\-no\-env/ ) {	
    splice(@ARGV, $j, 1) ;	
    $include_env = 0;
    $j--;
  }
}

# The arguments need to be escaped
#$args = join (" ", @ARGV);
$args = "";
foreach (@ARGV) {
    $args .= " " . quotemeta $_ ;
}

# Remove all options between +RTS and -RTS, inclusive
$in_rts = 0;
for ($j=0 ; $j <= $#ARGV ; $j++) { ## search through command line
  if ($ARGV[$j] =~ /^\-RTS/ ) {	## does cmd-line have this switch?
    $in_rts = 0;
    splice(@ARGV, $j, 1) ;	## remove switch{arg} from ARGV
    $j--;
  }
  elsif ($ARGV[$j] =~ /^\+RTS/ || $in_rts) { ## does cmd-line have this switch?
    $in_rts = 1;
    splice(@ARGV, $j, 1) ;	## remove switch{arg} from ARGV
    $j--;
  }
}

#usage() if GetCommandLine('-vsim: -g@ -bsv: -verbose -bsv_only file other_files:');
$bscfilter = join (" ", (@BSCFILTER, @ESECOMPARGS));

# First, just get the filename and ESEComp-specific args
$status = GetCommandLine($bscfilter);

#if ($opt_help || $opt_h) {
#  usage();
#}

if ($include_env == 1) {
    $args = "$env_options $args";
}


################################################################################
### BSV or ESE?
################################################################################

if (defined ($opt_x)) {
  if (lc ($opt_x) eq lc("ese")) {
    $is_ese = 1;
  }
  elsif (lc ($opt_x) eq lc("bsv")) {
    $is_bsv = 1;
  }
  else {
    print STDERR "ERROR: The language $opt_x is not suppported by bsc\n" ;
    exit -1 ;
  }
}

if (defined $file) {
  my $no_hpp = 0;
  my $yes_hpp = 0;
  my $f;
  foreach $f ($file, @other_files) {
    if ($f =~ m/\.(hpp|h|H|hxx|hh|h\+\+)$/) {
      $yes_hpp = 1;
    }
    else {
      $no_hpp = 1;
    }
  }

  if ($yes_hpp && $no_hpp) {
    print STDERR "ERROR: bsc does not yet support mixing ESE files with any other input files\n" ;
    exit -1 ;
  }

  if ($yes_hpp) {
    $is_ese = 1;
  }
  else {
    $is_bsv = 1;
  }
}
else {
  $is_bsv = 1;		# Default to BSV
}

# Call bsc to do all the non-ESE stuff.  Later, we can do more interesting stuff.
if ($is_bsv) {
  if (`uname -m` =~ /64$/) {
    $bsc = "$bluedir/bin/linux64/bsc";
  }

  else {
    $bsc = "$bluedir/bin/linux32/bsc";
  }
#  print "Executing $bsc $args\n";
#  exit 1;
  if (! -e "$bsc") {
    print_err ("bsc not found [$bsc]\n");
    exit(-1);
  }

# Is NOT ESEComp, run bsc with the original args.

  $status = system ("$bsc $args");
#  print "Status: $status\n";
# need to divide by 256 to get the exit code 
# because the actual value is returned from the wait syscall 
  exit ($status / 256);
}

# If ESEComp, continue...

# remove the ESEComp and file args from the commandline so we can pass the remainder to bsc
$args =~ s/$file(?!.* -)//;
foreach (@other_files) {
  $args =~ s/$_(?!.* -)//;
}

foreach (@ESECOMPARGS) {
  if (s/(\:$)//) {		# option with argument
    $args =~ s/$_\s+.+? //;
  }
  elsif (s/(\@$)//) {
    $args =~ s/$_\s+.+? //g;
  }
  else {
    $args =~ s/$_//;
  }
}

################################################################################
### Set the executable names
################################################################################

if ($ENV{CXX_ESECOMP} eq "") {
    $cxx = "g++"
    } 
else {
    $cxx = $ENV{CXX_ESECOMP}
}

if ($ENV{CCP_ESECOMP} eq "") {
    $ccp = "$ENV{BLUESPECDIR}/bin/eseparse"
    } 
else {
    $ccp = $ENV{CCP_ESECOMP}
}

if ($ENV{BSC_ESECOMP} eq "") {
#    $bsc = "$ENV{BLUESPECDIR}/../bin/bsc"
      $bsc = "$ENV{BLUESPECDIR}/bin/bsc"
    } 
else {
    $bsc = $ENV{BSC_ESECOMP}
}

################################################################################
### Check that required environment variables are set.
################################################################################

if ($ENV{SYSTEMC} eq "") {
    print STDERR "ERROR : The environment variable \"SYSTEMC\" is not defined.\n" ;
    exit -1 ;
}

################################################################################
### Check that the source file exists.
################################################################################

if (!(-e $file)) {

    print STDERR "ERROR : The file $file doesn't exist!\n" ;
    exit -1 ;
}

if ($opt_g[0] eq "") {
    print STDERR "ERROR : No module to synthesize has been specified with the -g flag.\n";
    print STDERR "        At least one synthesized module must be specified.\n\n";
    usage ();
}

if ($opt_bsv eq "") {
    $bsv_file = "esecomp.bsv"
} else {
    $bsv_file = $opt_bsv
}

################################################################################
###
################################################################################

if ($opt_verbose == 1) {
    print STDERR "NOTICE: Using gcc compiler \"$cxx\". \n";
    print STDERR "NOTICE: Using bsc compiler \"$bsc\". \n";
    print STDERR "NOTICE: Using ese parser \"$ccp\". \n";
}

################################################################################
### 
################################################################################

$allfiles = join (":", ($file, @other_files));
$parse_command = "$cxx -x c++ -fsyntax-only -DESE_COMP -I$ENV{SYSTEMC}/include -I$ENV{BLUESPECDIR}/include $file";
$expand_command = "$cxx -x c++ -E -DESE_COMP -I$ENV{BLUESPECDIR}/include/esl/include_3.4 -I$ENV{BLUESPECDIR}/include/esl/include_3.4/i486-linux -I$ENV{SYSTEMC}/include -I$ENV{BLUESPECDIR}/include $file";
$ccp_command = "$ccp -tr permissive,nowarnings -g $opt_g[0] -file $allfiles -o $bsv_file esecomp.i";
$bsc_command = "$bsc -u -verilog -esecomp -no-show-compiles $args $bsv_file";
$bsc_help = "$bsc -help";

print STDERR "Parsing ESE file $file ... \n";
if ($opt_verbose == 1) {
    print STDERR "Executing \"$parse_command\".\n";
}

#system($parse_command) == 0
#    or die "system @parse_command failed: $?";

&filter_gcc_epp_msg ($parse_command, "/dev/null");

print STDERR "Parsing ESE file $file complete.\n\n";

print STDERR "Expanding macros in ESE file $file ... \n";
if ($opt_verbose == 1) {
    print STDERR "Executing \"$expand_command\".\n";
}

#system($expand_command) == 0
#    or die "system @expand_command failed: $?";

&filter_gcc_epp_msg ($expand_command, "esecomp.i");

print STDERR "Expanding macros in ESE file $file complete.\n\n";

print STDERR "Typechecking and expanding before synthesis ... \n";
if ($opt_verbose == 1) {
    print STDERR "Executing \"$ccp_command\".\n";
}
system($ccp_command) == 0
    or die "Command failed: \"$ccp_command\" [$?]";
print STDERR "Typechecking and expanding complete.\n\n";

if ($opt_bsv_only eq "") {
    print STDERR "Synthesizing Verilog95 ... \n";
    if ($opt_verbose == 1) {
	print STDERR "Executing \"$bsc_command\".\n";
    }
    system($bsc_command) == 0
	or die "Command failed: \"$bsc_command\" [$?]";
    print STDERR "Synthesizing Verilog95 complete.\n\n";
}

################################################################################
### Utilities
################################################################################

sub print_err {
  $msg = shift;
  print STDERR $msg;
}

sub filter_gcc_epp_msg {
  my $cmd = shift;
  my $dest = shift;
  my $errmsg = `$cmd  2>&1 1>$dest`;		# Run the command

  # Remove "missing .epp" file errors in message
  $errmsg =~ s/In file included from .+?\n(.+:\n)*.+?\.epp: No such file or directory\n//mg;
  $errmsg =~ s/^.+?\.epp: No such file or directory\n//mg;
  $errmsg =~ s/^\w+\.epp: .+?\n//mg;

  if ($errmsg ne "") {
    print_err ($errmsg."\n");
    print_err ("ESEComp parsing halted due to C++ syntax errors.\n");
    exit 1;
  }
}


sub usage
  {
    system("$bsc -help");
    exit(0);
  }

sub usage1
{
    print STDERR "****************************************************************************\n";
    print STDERR "*** $BaseName synthesizes a Verilog95 RTL implementation for the specified\n";
    print STDERR "*** ESE file(s).\n";
    print STDERR "****************************************************************************\n";
    print STDERR "*** Usage:\n";
    print STDERR "***   $BaseName [-vsim simulator_name] [-verbose] [-g mod_name] [-bsv_only] [-bsv bsv_file_name] file [other_files]\n";
    print STDERR "****************************************************************************\n";
    print STDERR "*** Notes:\n";
    print STDERR "***   1) other_files is a colon separated string (i.e. foo:baz:/bar/zow)\n";
    print STDERR "***   2) -g specifies the module to be synthesized.  To specify more than\n";
    print STDERR "***      one module, use multiple -g flags (i.e. -g foo -g bar).\n";
    print STDERR "***   3) -bsv_only creates only the bsv file. bsc is not run.\n";
    print STDERR "****************************************************************************\n";
    print "bsc_help is $bsc_help\n";
    system("$bsc -help");
    die "Please try again.\n";
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
        if ($this =~ /^-(\w[\w-]+):/)                 ## multi-char switch w/ argument?
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

# ehm 11/28/06 : remove this whole loop; bsc doesn't support bunched options
#     ## [5] loop through search list to find single-char switches that are bunched together ##
#     print "## (5) \'@search_list\' <@ARGV>\n" if ($debugd) ;
#     for ($i=0 ; $i <= $#search_list ; $i++)
#     {
#         $this = $search_list[$i] ;                    ## next switch or argument
#         if ($this =~ /^-(\w@*)$/)                     ## single-char switch?
#         {
#             $that = $1 ;
# 	    next if $that =~ /\@/ ;
# 	    print "[5]# $that <@ARGV>\n" if ($debugd) ;
# 	    splice(@search_list, $i, 1) ;             ## remove switch from search_list
# 	    $i-- ;
#             for ($j=0 ; $j <= $#ARGV ; $j++)          ## search through command line
#             {
#                 if ($ARGV[$j] =~ /(^-\w*)$that(\w*)/) ## is this switch in a bunch of switches?
#                 {
# 		    $before = $1 ;
# 		    $after  = $2 ;
#                     eval "\$opt_$that = 1 ;" ;
#                     eval "print \"(bunch) opt_$that set to (\$opt_$that)\n\"" if $debugc ;
# 		    $ARGV[$j] =~ s/$before$that$after/$before$after/ ;
# 		    splice(@ARGV, $j, 1) if ($ARGV[$j] eq '-') ;
#                     last ;
#                 }
#             }
#         }
#     }

    ## [6] loop through search list to find plain single-char switches ##
    print "## (6) \'@search_list\' <@ARGV>\n" if ($debugd) ;
    for ($i=0 ; $i <= $#search_list ; $i++)
    {
        $this = $search_list[$i] ;                    ## next switch or argument
# ehm 11/28/06 :       if ($this =~ /^-(\w{2,}@*)/)                     ## plain-old switch
        if ($this =~ /^-(\w@?)/)                     ## ehm 11/28/06 : one and only one character
        {
            $that = $1 ;
	    next if $that =~ /\@/ ;
	    print "[6]# $that <@ARGV>\n" if ($debugd) ;
	    splice(@search_list, $i, 1) ;             ## ehm 11/28/06 : remove switch from search_list
	    $i-- ;
            for ($j=0 ; $j <= $#ARGV ; $j++)            ## search through command line
            {
                if ($ARGV[$j] =~ /^-$that/)           ## does cmd-line have this switch?
                {
                    eval "\$opt_$that = 1 ;" ;
                    eval "print \"opt_$that set to (\$opt_$that)\n\"" if $debugc ;
                    splice(@ARGV, $j, 1) ;            ## remove switch from ARGV
#ehm 11/28/06 :	    splice(@search_list, $i, 1) ;     ## remove switch from search_list
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
#            print STDERR "Unknown switch : $ARGV[$j]\n" ;
            $num_errors++ ;
	    push @EXTRA, $ARGV[$j]; # Save this for bassing to bsc
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
#		eval "\$$that[\$k] = \$ARGV[\$j];"; ## set global variable to value
		eval "\$".$that."[\$k] = \$ARGV[\$j];"; ## set global variable to value
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
#		print STDERR "Missing required argument for : $that\n" ;
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
#	print STDERR "Extra argument : $ARGV[$j]\n" ;
	$num_errors++ ;
    }

    return $num_errors ;
}

