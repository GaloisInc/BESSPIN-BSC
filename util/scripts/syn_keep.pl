 #! -*- perl -*- -w
eval 'exec perl -S $0 ${1+"$@"}'
  if 0;

################################################################################
# Script         : syn_keep.pl
#
# Author         : Todd Snyder
# Created        : 03/18/2009
#
# Description    : Attaches a syn_keep attribute to each WILL_FIRE_ and CAN_FIRE_
#                  declaration
#
# Copyright      : Copyright (c) 2009, Bluespec Inc. ALL RIGHTS RESERVED
#
################################################################################

use strict;
use English;
use Getopt::Long;
use IO::File;
use POSIX qw(strftime);

# Global Variables
$OUTPUT_AUTOFLUSH        = 1;

# Initialize the random number generator
srand(time ^ $$ ^ unpack "%L*", `ps axww | gzip -f`);

# Locally Generated Global Variables
my $DEBUG_MODE           = 0;
my $DATE                 = "";
my %ENVIRONMENT          = ();

my %PINS                 = ();
my %SIGNALS              = ();

# Command Line Definitions
sub CMDLINE_OPTION       { 0 }
sub CMDLINE_VARIABLE     { 1 }
sub CMDLINE_DESCRIPTION  { 2 }
sub CMDLINE_EXTRAINFO    { 3 }

############################################################
# COMMAND_LINE OPTIONS
#  This block defines the commandline options as well as
#  the documentation that shows up when -help is added to
#  the commandline.  The <NULL> parameter is used so that
#  the entry is not added to the GetOptions call.  The 
#  fields after the description are for use by options which
#  take arguments. 
############################################################
my $USAGE                = "Usage: $0 [<options> ...] <input file> ...\n";
my %CMDOPTIONS           = 
    (
     "Normal",
     [
      ['help',            '\&show_help',       "Shows this screen."],
      ],
     "HIDDEN",
     [
      ['debug',           '\$DEBUG_MODE',      "RUN IN DEBUG MODE"],
      ],
     );

############################################################
# process_command_line_options()
#
# reads the CMDOPTIONS hash and fills out a call to
# GetOptions.  Also initializes some keys in the 
# ENVIRONMENT hash.
#
############################################################
sub process_command_line_options 
{
    my %opthash  = ();
    my @optarray = ();

    my $cmdarray;
    my $category;
    my $cmdoption;
    my $cmdhashoption;
    my $variable;
    my $useshort;

    foreach $category (keys %CMDOPTIONS) {
	foreach $cmdarray (@{$CMDOPTIONS{$category}}) {
	    $cmdoption   = @$cmdarray[&CMDLINE_OPTION];
	    $variable    = @$cmdarray[&CMDLINE_VARIABLE];

	    # NULL variables allow documentation but not a new command option
	    next if ($variable eq "<NULL>");

	    # the option for the hash should not have the =[is][@%] options
	    # that can be used with GetOpt.
	    ($cmdhashoption = $cmdoption) =~ s/^([^=]+).*/$1/;

	    # resolve the pointer to the variable.  
	    $opthash{$cmdhashoption} = eval $variable;

	    push @optarray, $cmdoption;	    
	}
    }

    # Process the options
    &Getopt::Long::GetOptions(\%opthash, @optarray) or die("$0 failed parsing command-line options");    

    # Fill Out the Date String
    $DATE = strftime "%a %b %e %H:%M:%S %Y", localtime;    

    # Setup Environment Settings after command-line variables are set
    &debug("Date:        %s\n", $DATE);
}

############################################################
# show_help()
#
# Displays the help screen for this script.
#
############################################################
sub show_help 
{
    my $cmdarray;
    my $category;
    my $cmdoption;
    my $variable;
    my $description;
    my $extra;

    # Display Usage information
    printf $USAGE, "\n";

    foreach $category (keys %CMDOPTIONS) {

	# Do not display hidden options in help
	next if $category eq "HIDDEN";

	# Display the option category
	printf "\n%s Options:\n", $category;

	foreach $cmdarray (@{$CMDOPTIONS{$category}}) {
	    $cmdoption   = @$cmdarray[&CMDLINE_OPTION];
	    $variable    = @$cmdarray[&CMDLINE_VARIABLE];
	    $description = @$cmdarray[&CMDLINE_DESCRIPTION];

	    # Remove garbage needed for GetOptions after the true option name
	    $cmdoption =~ s/^([^=]+).*/$1/;

	    # Display extra information if available
	    $extra = (scalar(@$cmdarray) > &CMDLINE_EXTRAINFO) ? @$cmdarray[&CMDLINE_EXTRAINFO] : "";

	    # Display the option and description
	    if ($extra ne "") {
		printf "   -%-35s - %s\n", "$cmdoption $extra", $description;
	    } else {
		printf "   -%-35s - %s\n", $cmdoption, $description;
	    }
	}
    }
    printf "\n\n";
    exit(0);
}

############################################################
# run(command)
#
# Executes supplied commandline and returns stdout.
# Can return an array, a scalar, or undefined if stdout
# collect no data.
#
############################################################
sub run 
{
    my @output;

    # Run the command capturing stdout
    open(SYS, "@_|") or die "Failed to run command: @_\n$!\n";
    while(<SYS>) {
	push @output, $_;
    }
    close(SYS);

    if (!@output) {
	return undef;
    } elsif (wantarray) {
	return @output;
    } else {
	return $output[0];
    }
}

############################################################
# send_mail(subject, body, recipients...)
# 
# Formats an email message and sends an email with the given
# subject and body to the list of recipients.
#
############################################################
sub send_mail 
{
    my $subject    = shift;
    my $body       = shift;
    my @recipients = @_;

    # assemble a comma separated list of recipients
    my $tolist     = join(", ", @recipients);

    # locate the sendmail program
    my $sendmail   = `which sendmail`;
    chomp($sendmail);

    # Send the mail.
    open(SENDMAIL, "|$sendmail -t -oi");
    print SENDMAIL "To: $tolist\n";
    print SENDMAIL "Subject: $subject\n\n";
    print SENDMAIL "$body\n";
    close(SENDMAIL);
}

############################################################
# message(fmt, ...)
#
# Prints a printf formatted string to stdout.
#
############################################################
sub message 
{
    my $fmt    = shift;
    my @vars   = @_;

    printf $fmt, @vars;
}

############################################################
# inform(fmt, ...)
#
# Prints a printf formatted string to stdout.
#
############################################################
sub inform 
{
    my $fmt    = shift;
    my @vars   = @_;

    printf "INFORM: $fmt", @vars;
}

############################################################
# debug(fmt, ...)
#
# Prints a printf formatted string to stdout.
#
############################################################
sub debug 
{
    my $fmt    = shift;
    my @vars   = @_;

    if ($DEBUG_MODE) {
	printf "DEBUG: $fmt", @vars;
    }
}

############################################################
# log(handle, fmt, ...)
#
# Prints a printf formatted string to the supplied handle
# and to stdout.
#
############################################################
sub log 
{
    my $handle = shift;
    my $fmt    = shift;
    my @vars   = @_;

    if (defined $handle) {
	printf $handle "$fmt", @vars;
    }
    printf "$fmt", @vars;
}

############################################################
# warning(fmt, ...)
#
# Prints a printf formatted string to stdout.
#
############################################################
sub warning 
{
    my $fmt    = shift;
    my @vars   = @_;

    printf "WARNING: $fmt", @vars;
}

############################################################
# error(fmt, ...)
#
# Prints a printf formatted string to stderr.
#
############################################################
sub error 
{
    my $fmt    = shift;
    my @vars   = @_;

    printf STDERR "ERROR: $fmt", @vars;
}

############################################################
# fatal(fmt, ...)
#
# Prints a printf formatted string to stderr.
#
############################################################
sub fatal 
{
    my $fmt    = shift;
    my @vars   = @_;

    printf STDERR "FATAL ERROR: $fmt", @vars;
    exit -1;
}

############################################################
# internal(fmt, ...)
#
# Prints a printf formatted string to stderr.
#
############################################################
sub internal 
{
    my $fmt    = shift;
    my @vars   = @_;

    printf STDERR "INTERNAL ERROR: $fmt", @vars;
}

############################################################
# link_file
#
# Creates a soft link in the specified directory
#
############################################################
sub link_file 
{
    my $source_file = shift;
    my $dest_file   = shift or undef;

    if (!defined $source_file || $source_file eq "") {
	&fatal("Attempting to link to a (null) file!");
    }

    if (!defined $dest_file) {
	$dest_file = ".";
    }

    # prevent any errors or warnings from appearing to the screen.
    # Instead verify that the file properly exists below
    &run("ln -s $source_file $dest_file");

    # Verify the file properly exists
    my $file_to_check = ($dest_file eq ".") ? basename($source_file) : $dest_file;

    if (!-f($file_to_check)) {
	&fatal("The File $file_to_check was not linked to the source file $source_file!");
    }
}

############################################################
# open_file
#
# Opens a file and returns the file handle used.
#
############################################################
sub open_file
{
    my $filename  = shift;
    my $mode      = shift;

    if (!defined $filename || $filename eq "") {
	&fatal("Must specify a file or stream to open!\n");
    }

    if (!defined $mode) {
	$mode = "r";
    }

    return new IO::File($filename, $mode);
}

############################################################
# close_file
#
# Closes an opened file
#
############################################################
sub close_file
{
    my $filehandle = shift;

    # If the filehandle is no longer valid or the user forgot 
    # to supply the handle, just quit
    return if (!defined $filehandle);

    # close the file handle
    $filehandle->close();
}

############################################################
# get_random_value
#
# Creates a random value based on the given bit size
#
############################################################
sub get_random_value
{
    my $bits   = shift;
    my $maxVal = shift;

    my $localValue;

    # Reassign max value if requested
    if (!defined $maxVal) {
	$maxVal = 2 ** $bits - 1;
    }

    do
    {
	$localValue = rand(); # generates a float from 0 to 1.0
	$localValue *= 2 ** $bits; # so shift the fraction bits to int
	$localValue &= 2 ** $bits-1;
    } while($localValue > $maxVal);

    return $localValue;
}


################################################################################
################################################################################
################################################################################

############################################################
# main
#
# Entry point into the script.
#
############################################################
sub main 
{
    # Read in command-line options and setup environment
    &process_command_line_options;

    # Process all verilog files
    foreach my $outfile (@ARGV) {
	system "cp $outfile ${outfile}~";

	# read the file
	next unless open(FILE, $outfile);
	my @lines = <FILE>;
	close(FILE);
	
	# Locate each of the signals involved
	my @newlines;
	foreach my $line (@lines) {
	    if ($line =~ m/^\s*wire\s+((WILL|CAN)_FIRE_[A-Za-z0-9_\$]+)([\,\;])/) {
		push @newlines, "  (* syn_keep = \"yes\" *)\n";
		push @newlines, $line;
	    } else {
		push @newlines, $line;
	    }
	}

	# write out the new version
	open(OFILE, ">${outfile}") or die("Could not create output file: $!\n");
	print OFILE @newlines;
	close(OFILE);
    }
}

&main;
1;

