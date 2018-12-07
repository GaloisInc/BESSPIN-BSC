// ================================================================
// Copyright (c) 2017 Cadence Design Systems, Inc.
// All Rights Reserved.
// ================================================================

// This program is essentially a script performing a series of
// tasks:

// Normal operation:
//      mkdir -p transactors
//      bluetcl t_gen.tcl

//      build emu_$(HW_TARGET)
//      build -t gen_cxx_includes xactor_lib

//      mv transactors/rtl/ptm_top.v{,.orig}
//      $(BLUESPECDIR)/tcllib/bluespec/CXGen_adjust_top.tcl     \
//              xtor_clk                                        \
//              transactors/rtl/ptm_top.v.orig                  \
//              transactors/rtl/ptm_top.v

//      $(BLUESPECDIR)/tcllib/bluespec/CXGen_blackbox.tcl       \
//              transactors/rtl/ptm_bridge.v                    \
//              ptm_bridge.vg                                   \
//              ptm_bridge.xdc                                  \
//              xilinx/ptm_bridge_utilization_synth.rpt         \
//              transactors/rtl/ptm_bridge_bb.v
//      cp $(BOARDDIR)/ptm_bridge.xdc transactors/rtl/

// With command-line argument --clean it performs, instead:
//      $(RM) -rf transactors xilinx libptm_bridge.so ptm_bridge.params
//      $(RM) -rf project.bld project.cfg


// ================================================================

#define _XOPEN_SOURCE 500

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#include <stdbool.h>
#include <ctype.h>
#include <string.h>
#include <stdint.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <getopt.h>
#include <ftw.h>

// Uncomment this line to add verbosity for development debug
//
//#define DEBUG

// ================================================================
// Hardware target default designator for Protium

static char hw_target_default [] = "dnvuf4a";

// ================================================================
// Print help

void fprint_help (FILE *fpo, char *argv0)
{
    fprintf (fpo, "Usage:    %s  <flags>\n", argv0);
    fprintf (fpo, "Flags:\n");
    fprintf (fpo, "    -h, -H, --help         Print this help message\n");
    fprintf (fpo, "    --hw-target  <name>    Build for <name> (default '%s')\n", hw_target_default);
    fprintf (fpo, "    --clean                Delete generated files, restoring dir to original state\n");
#ifdef DEBUG
    fprintf (fpo, "    --dry-run              Just show commands that would be executed,\n");
    fprintf (fpo, "                               don't actually execute them\n");
#endif
}


// ================================================================
// Value of environment variable BLUESPECDIR

static char *BLUESPECDIR;

// ================================================================
// Utility: concatenate strings into a new malloc'd result

char *string_concat (const char *s1, const char *s2)
{
    // malloc the result
    int len = strlen (s1) + strlen (s2);
    char *result = (char *) malloc (len + 1);
    if (result == NULL) {
	fprintf (stderr, "Internal error: unables to malloc for string concatenation\n");
	fprintf (stderr, "    string 1 = '%s'\n", s1);
	fprintf (stderr, "    string 2 = '%s'\n", s2);
	exit (EXIT_FAILURE);
    }

    // Copy the strings
    int j, k;
    k = 0;
    for (j = 0; j < strlen (s1); j++) {
	result [k] = s1 [j];
	k++;
    }
    for (j = 0; j < strlen (s2); j++) {
	result [k] = s2 [j];
	k++;
    }
    result [k] = 0;
    return result;
}

// ================================================================
// Utility: copy a string into a new malloc'd result and convert to uppercase

char *string_toupper (const char *s)
{
    // malloc the result
    int len = strlen (s);
    char *result = (char *) malloc (len + 1);
    if (result == NULL) {
	fprintf (stderr, "Internal error: unables to malloc for string toupper\n");
	fprintf (stderr, "    string = '%s'\n", s);
	exit (EXIT_FAILURE);
    }

    // Copy the string
    int j, k;
    k = 0;
    for (j = 0; j < len; j++) {
	result [k] = toupper( s [j] );
	k++;
    }
    result [k] = 0;
    return result;
}

// ================================================================
// parse_cmd_line_args ()
// Return 0 on success, -1 on failure

static struct option long_options[] = {
    // name          has_arg              flag    val
    {"help",         no_argument,         0,      'h' },
    {"hw-target",    required_argument,   0,       0  },
    {"clean",        no_argument,         0,       0  },
#ifdef DEBUG
    {"dry-run",      no_argument,         0,       0  },
#endif
    {0,              0,                   0,       0  }
};

// Short options.
static char short_options [] = "hH";

extern char *optarg;    // value of option arg
extern int   optind;    // argv [optind] is next element to be processed (1..)
extern int   opterr;    // 
extern int   optopt;    // Holds unrecognized option

int
parse_cmd_line_args (int argc, char *argv [],
		     /* outputs */
		     char **p_hw_target,
		     int  *p_dry_run,
		     int  *p_clean)
{
    int c;

    *p_dry_run = false;
    *p_clean   = false;
    while (1) {
	int option_index = 0;

	c = getopt_long (argc, argv, short_options, long_options, & option_index);

	if (c == -1)
	    break;

	switch (c) {
	case 0:
	    // --hw-target
	    if (option_index == 1) {
		assert (optarg != NULL);
		int len = strlen (optarg);
		char *p = (char *) malloc (len);
		if (p == NULL) {
		    fprintf (stderr,
			     "Internal error: unable to malloc %0d bytes to hold"
			     " '--hw-target' command-line arg\n",
			     len);
		    fprintf (stderr, "    --hw-target arg is: '%s'\n", optarg);
		    return -1;
		}
		strcpy (p, optarg);
		*p_hw_target = p;
		break;
	    }
	    // --clean
	    else if (option_index == 2) {
		*p_clean = true;
		break;
	    }
	    // --dry-run
	    else {
		assert (option_index == 3);
		*p_dry_run = true;
		break;
	    }

	case '?':    // Unrecognized option
	    return -1;

	case 'h':
	case 'H':
	    fprint_help (stdout, argv [0]);
	    return -1;

	default:
	    fprintf (stderr, "Unrecognized option '%c' (ASCII %0d)\n", c, c);
	    return -1;
	}
    }

    if (optind < argc) {
	fprintf (stderr, "Unknown extra args on command line\n");
	while (optind < argc)
	    printf("%s ", argv [optind++]);
	printf("\n");
	return -1;
    }

    return 0;
}

// ================================================================
// Run command

void fprint_cmd_and_argv (FILE *fp, char *message, char *cmd, char *argv [])
{
    int j;

#ifdef DEBUG
    // Print command and args
    fprintf (fp, "%s:\n  %s", message, cmd);
    for (j = 1; argv [j] != NULL; j++)
	fprintf (fp, " %s", argv [j]);
    fprintf (fp, "\n");
#else
    // Only print the message
    fprintf (fp, "%s\n", message);
#endif
}

int run_command (int dry_run, int silent, char *cmd, char *argv [])
{
    // DEBUG fprint_cmd_and_argv (stdout, "Parent.run_command", cmd, argv);

    int wstatus;
    pid_t parent_pid = getpid ();
    pid_t child_pid = fork ();

#ifdef DEBUG
    silent = 0;
#endif

    if (child_pid != 0) {
	// This is the parent; wait for child to finish
	pid_t pid = waitpid (child_pid, & wstatus, 0);
	if (WIFEXITED (wstatus) && (WEXITSTATUS (wstatus) == 0))
	    return 0;
	else {
#ifdef DEBUG
	    fprint_cmd_and_argv (stderr, "Child process did not terminate normally",
				 cmd, argv);
	    fprintf (stdout, "Child process exit status %0d\n", WEXITSTATUS (wstatus));
#endif
	    return -1;
	}
    }
    else {
	// This is the child; execute the command

	if (silent) {
	    freopen("/dev/null", "r", stdin);
	    freopen("/dev/null", "w", stdout);
	    //freopen("/dev/null", "w", stderr);
	}

	if (dry_run) {
	    fprint_cmd_and_argv (stdout, "Child (dry run only): would execute", cmd, argv);
	    exit (EXIT_SUCCESS);
	}
	else {
#ifdef DEBUG
	    fprint_cmd_and_argv (stdout, "Child: executing", cmd, argv);
#endif

	    // Execute the command as a child process
	    // Note: execvp looks up cmd in the path, and uses the current environment
	    execvp (cmd, argv);

	    // We only reach here if execvp failed.
#ifdef DEBUG
	    fprint_cmd_and_argv (stderr, "Child: Unable to execute", cmd, argv);
#else
	    fprintf (stderr, "ERROR\n");
#endif

	    exit (EXIT_FAILURE);
	}
    }
}

// ****************************************************************
// ****************************************************************
// ** SECTION: Tasks for normal operation                        **
// ****************************************************************
// ****************************************************************

// ----------------
// Command:
// mkdir -p transactors

int do_task_A (int dry_run, char *hw_target)
{
    char *cmd     = "/bin/mkdir";
    char *argv [] = { "mkdir",
		      "-p",
		      "transactors",
		      0 };
    int silent = 1;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------
// Command:
// bluetcl t_gen.tcl

int do_task_B (int dry_run, char *hw_target)
{
    char *cmd    = "bluetcl";
    char *argv[] = { "bluetcl",
		     "t_gen.tcl",
		     0 };
    int silent = 0;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------
// Command:
// build emu_$(HW_TARGET)

int do_task_C (int dry_run, char *hw_target)
{
    char *cmd     = "build";
    char *argv [] = { "build",
		      // Use this for testing without running Vivado
		      // "-t",
		      // "prepare_project_files",
		      string_concat ("emu_", hw_target),
		      0 };
    int silent = 1;

    printf ("\nBuilding BlackBox...\n");
    return run_command (dry_run, silent, cmd, argv);
}

// ----------------
// Command:
// build -t gen_cxx_includes xactor_lib

int do_task_D (int dry_run, char *hw_target)
{
    char *cmd     = "build";
    char *argv [] = { "build",
		      "-t",
		      "gen_cxx_includes",
		      "xactor_lib",
		      0};
    int silent = 1;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------
// Command:
// mv transactors/rtl/ptm_top.v{,.orig}

int do_task_E (int dry_run, char *hw_target)
{
    char *cmd     = "mv";
    char *argv [] = { "mv",
		      "transactors/rtl/ptm_top.v",
		      "transactors/rtl/ptm_top.v.orig",
		      0 };
    int silent = 1;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------
// Command:
// $(BLUESPECDIR)/tcllib/bluespec/CXGen_adjust_top.tcl	\
//         xtor_clk					\
//         transactors/rtl/ptm_top.v.orig		\
//         transactors/rtl/ptm_top.v

int do_task_F (int dry_run, char *hw_target)
{
    char *cmd     = string_concat (BLUESPECDIR, "/tcllib/bluespec/CXGen_adjust_top.tcl");
    char *argv [] = { "CXGen_adjust_top.tcl",
		      "xtor_clk",
		      "transactors/rtl/ptm_top.v.orig",
		      "transactors/rtl/ptm_top.v",
		      0 };
    int silent = 1;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------
// Command:
// $(BLUESPECDIR)/tcllib/bluespec/CXGen_blackbox.tcl	 \
//         transactors/rtl/ptm_bridge.v				 \
//         ptm_bridge.vg					 \
//         ptm_bridge.xdc					 \
//         xilinx/ptm_bridge_utilization_synth.rpt		 \
//         transactors/rtl/ptm_bridge_bb.v

int do_task_G (int dry_run, char *hw_target)
{
    char *cmd     = string_concat (BLUESPECDIR, "/tcllib/bluespec/CXGen_blackbox.tcl");
    char *argv [] = { "CXGen_blackbox.tcl",
		      "transactors/rtl/ptm_bridge.v",
		      "ptm_bridge.vg",
		      "ptm_bridge.xdc",
		      "xilinx/ptm_bridge_utilization_synth.rpt",
		      "transactors/rtl/ptm_bridge_bb.v",
		      0 };
    int silent = 1;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------
// Command:
// cp $(BOARDDIR)/ptm_bridge.xdc transactors/rtl/

int do_task_H (int dry_run, char *hw_target)
{
    char *srcfile = string_concat (BLUESPECDIR,
				   string_concat ("/board_support/bluenoc/dini/",
						  string_concat(string_toupper(hw_target),
								"/ptm_bridge.xdc")));
    char *cmd     = "cp";
    char *argv [] = { "cp",
		      srcfile,
		      "transactors/rtl/",
		      0 };
    int silent = 1;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------------------------------------------------------

int do_tasks_normal (int dry_run, char *hw_target)
{
    int status;

    status = do_task_A (dry_run, hw_target);
    if (status != 0) return status;

    status = do_task_B (dry_run, hw_target);
    if (status != 0) return status;

    status = do_task_C (dry_run, hw_target);
    if (status != 0) return status;

    status = do_task_D (dry_run, hw_target);
    if (status != 0) return status;

    status = do_task_E (dry_run, hw_target);
    if (status != 0) return status;

    status = do_task_F (dry_run, hw_target);
    if (status != 0) return status;

    status = do_task_G (dry_run, hw_target);
    if (status != 0) return status;

    status = do_task_H (dry_run, hw_target);
    if (status != 0) return status;
}

// ****************************************************************
// ****************************************************************
// **    SECTION: Tasks for --clean                              **
// ****************************************************************
// ****************************************************************

// ----------------
// Command:
// $(RM) -rf transactors xilinx libptm_bridge.so ptm_bridge.params

int do_task_clean_A (int dry_run, char *hw_target)
{
    char *cmd = "rm";
    char *argv [] = { "rm",
		      "-rf",
		      "transactors",
		      "xilinx",
		      "libptm_bridge.so",
		      "ptm_bridge.params",
		      0 };
    int silent = 0;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------
// Command:
// $(RM) -rf project.bld project.cfg

int do_task_clean_B (int dry_run, char *hw_target)
{
    char *cmd = "rm";
    char *argv [] = { "rm",
		      "-rf",
		      "project.bld",
		      "project.cfg",
		      0 };
    int silent = 0;

    return run_command (dry_run, silent, cmd, argv);
}

// ----------------------------------------------------------------

int do_tasks_clean (int dry_run, char *hw_target)
{
    int status;

    status = do_task_clean_A (dry_run, hw_target);
    if (status != 0) return status;

    status = do_task_clean_B (dry_run, hw_target);
    if (status != 0) return status;
}

// ****************************************************************
// ****************************************************************
// ** SECTION: Attaching Copyright Notices to newly              **
// **          generated files.                                  **
// ****************************************************************
// ****************************************************************

// ================================================================
// Copyright notice text

const char *copyright_notice [] =
    { "================================================================",
      "Copyright (c) 2017 Cadence Design Systems, Inc.",
      "All Rights Reserved.",
      "Please see license for terms of use.",
      "================================================================",
      NULL    // Last element of array must be NULL
    };

// ================================================================
// Size of buffer while reading a file during copyright-notice addition

#define BUFMAX 4096

// ================================================================
// Utility function: case-insensitive version of strstr().
// Assumes needle_lc is a lower-case string.
// Note: GNU has a function strcasestr() for this, but we don't want
// to rely on that.

char *strcasestr_ours (char *haystack, const char *needle_lc)
{
    // Make a lower-case copy of haystack (until null-termination, if any)
    char haystack_lc [BUFMAX];
    int j;
    for (j = 0; j < BUFMAX; j++) {
	haystack_lc [j] = tolower (haystack [j]);
	if (haystack_lc [j] == 0)
	    break;
    }

    char *p = strstr (haystack_lc, needle_lc);
    if (p == NULL)
	return NULL;
    else
	return haystack + (p - haystack_lc);
}

// ================================================================
// Attach copyright notice to front of text file
// if it does not already have a copyright notice in it.
// If 'comment_start' is not NULL, it is prefixed to each copyright notice line.

void attach_copyright_notice (const char *filename, char *comment_start)
{
    FILE *fpi, *fpo;
    char tmp_filename [512];
    char bak_filename [512];
    int status, j;

    char buffer [BUFMAX];

    // Open existing file for reading
    fpi = fopen (filename, "r");
    if (fpi == NULL) {
	fprintf (stderr,
		 "Internal error: unable to open file '%s' during Copyright Notice addition\n",
		 filename);
	perror (NULL);
	return;
    }

    // Open new temporary file for writing
    sprintf (tmp_filename, "%s_TEMPORARY", filename);
    fpo = fopen (tmp_filename, "w");
    if (fpo == NULL) {
	fprintf (stderr,
		 "Internal error: unable to open file '%s' during Copyright Notice addition\n",
		 tmp_filename);
	perror (NULL);
	fclose (fpi);
	return;
    }

    // Write copyright notice into new temporary file
    for (j = 0; copyright_notice [j] != NULL; j++) {
	if (comment_start != NULL) fprintf (fpo, "%s ", comment_start);
	fprintf (fpo, "%s\n", copyright_notice [j]);
    }
    // A final blank line
    fprintf (fpo, "\n");

    // Copy existing file into new temporary file
    // Search each line (case-insensitive) for 'Copyright'
    // and if found, abandon this, i.e., leave input file as is.
    // Note: won't work on very long lines where the word 'Copyright'
    // straddles the buffer boundary, but we expect that normal
    // uses of the word 'Copyright' are typically within the first 80
    // chars of a line.
    while (1) {
	char *p = fgets (buffer, BUFMAX, fpi);
	if (p == NULL) break;

	// If already has 'copyright', abandon this file.
	char *p1 = strcasestr_ours (buffer, "copyright");
	if (p1 != NULL) {
#ifdef DEBUG
	    fprintf (stdout, "This file already has a copyright notice: '%s'", filename);
#endif
	    fclose (fpi);
	    fclose (fpo);

	    // Delete the temporary file
	    unlink (tmp_filename);
	    return;
	}

	fputs (buffer, fpo);
    }

    // Close existing file
    status = fclose (fpi);
    if (status != 0) {
	fprintf (stderr,
		 "ERROR: unable to close file '%s' during Copyright Notice addition\n",
		 filename);
	perror (NULL);
	return;
    }

    // Close new temporary file
    status = fclose (fpo);
    if (status != 0) {
	fprintf (stderr,
		 "ERROR: unable to close file '%s' during Copyright Notice addition\n",
		 tmp_filename);
	perror (NULL);
	return;
    }

    // Rename old file to temporary BAK filename (will be deleted shortly)
    sprintf (bak_filename, "%s_BAK", filename);
    status = rename (filename, bak_filename);
    if (status != 0) {
	fprintf (stderr,
		 "ERROR: unable to rename file '%s' to '%s' during Copyright Notice addition\n",
		 filename, bak_filename);
	perror (NULL);
	return;
    }

    // Rename new temporary file to old filename
    status = rename (tmp_filename, filename);
    if (status != 0) {
	fprintf (stderr,
		 "ERROR: unable to rename file '%s' to '%s' during Copyright Notice addition\n",
		 tmp_filename, filename);
	perror (NULL);

	// Restore BAK file to original filename
	status = rename (bak_filename, filename);
	if (status != 0) {
	    fprintf (stderr,
		     "ERROR: unable to rename file '%s' to '%s' during Copyright Notice addition\n",
		     bak_filename, filename);
	    perror (NULL);
	}
	return;
    }

    // Delete old file (which now has bak_filename)
    status = unlink (bak_filename);
    if (status != 0) {
	fprintf (stderr, "ERROR: unable to delete file '%s'during Copyright Notice addition\n",
		 bak_filename);
	perror (NULL);
	// We don't return -1 here, even though it leaves the BAK file as garbage,
	// the principal objective has been met
    }
    return;
}

// ================================================================
// Utility function: test if a filename (path) has a given suffix

typedef struct {
    char *file_type;
    char *comment_starter;
} FileTypeInfo;

FileTypeInfo std_file_types [] =
    { { ".v",        "//" },
      { ".c",        "//" },
      { ".cpp",      "//" },
      { ".cxx",      "//" },
      { ".h",        "//" },
      { ".hpp",      "//" },
      { ".hxx",      "//" },
      { ".tcl",      "#" },
      { ".cfg",      "#" },
      { ".bsv",      "//" },
      0
};

int std_type_of_file (const char *fpath)
{
    int j;
    int len1 = strlen (fpath);

    for (j = 0; std_file_types [j].file_type != NULL; j++) {
	int len2 = strlen (std_file_types [j].file_type);
	if (strcmp (& (fpath [len1 - len2]), std_file_types [j].file_type) == 0)
	    return j;
    }
    return -1;
}

// ================================================================
// Process a file in the recursive directory traversal,
// i.e., if it is a candidate for copyright addition, do so

int process_a_file (const char *fpath,
		    const struct stat *sb,
		    int tflag,
		    struct FTW *ftwbuf)
{
    int status;

    if (tflag == FTW_F) {    // ordinary file, not dir or symlink etc
	int file_type_index = std_type_of_file (fpath);

	if (file_type_index >= 0) {
#ifdef DEBUG
	    fprintf (stdout, "Add %s to %s\n", std_file_types [file_type_index].comment_starter, fpath);
#endif
	    attach_copyright_notice (fpath, std_file_types [file_type_index].comment_starter);
	}
#ifdef DEBUG
	else
	    fprintf (stdout, "Skip: %s\n", fpath);
#endif
    }
    return 0;           /* tells nftw() to continue */
}

// ================================================================
// Traverse directory recursively, visiting all files
// and applying 'process_a_file()' to each file visited.

int add_copyrights (const char *root)
{
    int nopenfd = 20;          // max open file descriptors used
    int flags   = FTW_PHYS;    // don't follow physical pointers

    int status = nftw (root, process_a_file, nopenfd, flags);
    if (status != 0)
	perror ("add_copyrights.nftw");
    return status;
}

// ****************************************************************
// ****************************************************************
// ** SECTION: LICENSE CHECKING                                  **
// ****************************************************************
// ****************************************************************
// To be filled in by Cadence.

int check_license (void)
{
    fprintf (stdout, "Placeholder: check license here\n");
    return 0;
}

// ****************************************************************
// ****************************************************************
// ** SECTION: MAIN                                              **
// ****************************************************************
// ****************************************************************

// Actual hardware target (default or otherwise)

char *hw_target;

// ================================================================
// main()

int
main (int argc, char **argv)
{
    int status, clean, dry_run, len;

    fprintf (stdout, "================================================================\n");
    fprintf (stdout, "Running %s\n", argv [0]);
    fprintf (stdout, "Copyright (c) 2017 Cadence Design Systems, Inc.\n");
    fprintf (stdout, "Please see license for terms of use\n");
    fprintf (stdout, "================================================================\n");

    // Check license
    status = check_license ();
    if (status != 0) exit (EXIT_FAILURE);

    // Parse command-line args
    hw_target = NULL;
    status = parse_cmd_line_args (argc, argv, & hw_target, & dry_run, & clean);
    if (status != 0) exit (EXIT_FAILURE);
    if (hw_target == NULL)
	hw_target = & (hw_target_default [0]);

    // If --clean
    if (clean) {
	status = do_tasks_clean (dry_run, hw_target);
	if (status != 0) exit (EXIT_FAILURE);
    }

    // Normal operation
    else {
	fprintf (stdout, "Using: hw-target = '%s'\n", hw_target);

	BLUESPECDIR = getenv ("BLUESPECDIR");
	if (BLUESPECDIR == NULL) {
	    fprintf (stderr, "ERROR: Environment variable BLUESPECDIR is not set\n");
	    exit (EXIT_FAILURE);
	}

	// Do the tasks
	status = do_tasks_normal (dry_run, hw_target);

	// Add copyrights to all generated files,
	// even if do_tasks_normal did not succeed.
	add_copyrights ("transactors");
	add_copyrights ("xilinx");

	if (status != 0) {
	  fprintf (stderr, "ERROR\n");
	  exit (EXIT_FAILURE);
	}

	printf ("Created file: transactors/rtl/ptm_top.v\n");
	printf ("Created file: transactors/rtl/ptm_bridge_bb.v\n");
	printf ("Created file: transactors/rtl/ptm_bridge.xdc\n");
	printf ("Created file: xilinx/ptm_bridge.vg\n");
    }
    exit (EXIT_SUCCESS);
}
