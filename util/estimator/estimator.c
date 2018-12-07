/* -*-  Mode:C; c-basic-offset:4 -*- */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

#include "utils.h"
#include "parser.h"
#include "graph.h"
#include "analysis.h"

// ****************************************************************
// main() and friends

#define BANNER "estimator, v 0.2\n" \
               "Copyright (c) 2005 Bluespec, Inc.  All rights reserved.\n"

// ----------------------------------------------------------------
// setupOutputFile()

FILE *
setupOutputFile (char *inputFilename, char *outputFilename)
{

#define BUFSIZE 1024

    char buf [BUFSIZE];
    static char outputFileExtension[] = ".analysis";
    int  j, k;
    FILE  *fpo;

    if (outputFilename == NULL) {
	// Create an output filename: input filename w. extenation .analysis
	strncpy (buf, inputFilename, BUFSIZE-1);
	buf[BUFSIZE-1] = 0;

	// Scan to find the end of the string
	for (j = 0; buf[j] != 0; j++);
	// Scan back to find the '.' separating the file extension, if any
	k = j-1;
	while ((buf[k] != '.') && (k >= 0))
	    k--;

	// Establish a position for the new filename extension
	if (k < 0) k = j;
	if (k > (BUFSIZE - strlen (outputFileExtension) - 1))
	    k = BUFSIZE - strlen (outputFileExtension) - 1;

	strcpy (& (buf [k]), outputFileExtension);
    }
    else {
	strncpy (buf, outputFilename, BUFSIZE-1);
	buf[BUFSIZE-1] = 0;
    }

    fpo = fopen (buf, "w");
    if (fpo == NULL) {
	fprintf (stderr, "Error: can't open file %s for output\n", buf);
	exit (1);
    }
    printf ("Analysis will be written to file:    %s\n", buf);

    return fpo;
} // setupOutputFile ()

// ----------------------------------------------------------------
// printUsage()

void
printUsage (FILE *fp, int argc, char *argv[], int exitval)
{
    fprintf (fp, "Usage:    %s  <flags>  <foo.v filename>\n", argv[0]);
    fprintf (fp, "\n");
    fprintf (fp, "(flags may appear anywhere on the command line)\n");
    fprintf (fp, "\n");
    fprintf (fp, "Args:\n");
    fprintf (fp, "<foo.v filename>       Input file (bsc-generated verilog file)\n");
    fprintf (fp, "\n");
    fprintf (fp, "Flags:\n");
    fprintf (fp, "-h/-help/--help        Print this help message\n");
    fprintf (fp, "-o <filename>          Output filename\n");
    fprintf (fp, "                         Default is input filename with .analysis extension\n");
    fprintf (fp, "-npaths  all/<n>       Print all/<n> longest paths (default is 10)\n");
    fprintf (fp, "-nfanouts  all/<n>     Print all/<n> largest-fanout nets (default is 10)\n");
    fprintf (fp, "\n");
    fprintf (fp, "Debugging flags\n");
    fprintf (fp, "-v/-v1, -v2            Set verbosity level (default 0)\n");
    fprintf (fp, "-show-parse            Show parsed input\n");
    fprintf (fp, "-show-graph            Show graph built from input\n");

    exit (exitval);
} // printUsage()

// ----------------------------------------------------------------
// main()

int
main (int    argc,
      char  *argv[])
{
    int         j, numErrs, nPaths, nFanouts;
    AST         *modp;
    ModuleGraph *moduleGraphp;
    FILE        *fpo;
    char        *s;
    Bool         showParse, showGraph;

    printf ("%s", BANNER);

    // ----------------
    // Help
    if (findflag (& argc, argv, "-h", FLAG_CASE_INSENSITIVE, FLAG_KEEP) ||
	findflag (& argc, argv, "-help", FLAG_CASE_INSENSITIVE, FLAG_KEEP) ||
	findflag (& argc, argv, "--help", FLAG_CASE_INSENSITIVE, FLAG_KEEP)) {
	printUsage (stdout, argc, argv, EXIT_NORMAL);
    }

    // ----------------
    // Verbosity
    verbosity = 0;
    if (findflag (& argc, argv, "-v", FLAG_CASE_INSENSITIVE, FLAG_REMOVE))
	verbosity = 1;
    else if (findflag (& argc, argv, "-v1", FLAG_CASE_INSENSITIVE, FLAG_REMOVE))
	verbosity = 1;
    else if (findflag (& argc, argv, "-v2", FLAG_CASE_INSENSITIVE, FLAG_REMOVE))
	verbosity = 2;
    if (verbosity > 0) printf ("Verbosity level %d\n", verbosity);

    // ----------------
    // Misc flags
    showParse = (findflag (& argc, argv, "-show-parse", FLAG_CASE_INSENSITIVE, FLAG_REMOVE) != 0);
    showGraph = (findflag (& argc, argv, "-show-graph", FLAG_CASE_INSENSITIVE, FLAG_REMOVE) != 0);

    // ----------------
    // Get nPaths, if present
    j = findflag (& argc, argv, "-npaths", FLAG_CASE_INSENSITIVE, FLAG_REMOVE);
    if (j == 0)
	nPaths = 10;
    else {
	if (strcmp (argv [j], "all") == 0)
	    nPaths = -1;
	else
	    nPaths = atoi (argv [j]);
	removeflag (& argc, argv, j);
    }

    // ----------------
    // Get nFanouts, if present
    j = findflag (& argc, argv, "-nfanouts", FLAG_CASE_INSENSITIVE, FLAG_REMOVE);
    if (j == 0)
	nFanouts = 10;
    else {
	if (strcmp (argv [j], "all") == 0)
	    nFanouts = -1;
	else
	    nFanouts = atoi (argv [j]);
	removeflag (& argc, argv, j);
    }

    // ----------------
    // Get output file name, if present
    j = findflag (& argc, argv, "-o", FLAG_CASE_INSENSITIVE, FLAG_REMOVE);
    if (j == 0)
	s = NULL;
    else {
	s = argv [j];
	removeflag (& argc, argv, j);
    }

    // ----------------
    // Final cmd line arg check
    if (argc < 2) {
	fprintf (stderr, "Error: missing .v filename on command line\n");
	printUsage (stderr, argc, argv, EXIT_ERROR);
    }
    else if (argc > 2) {
	fprintf (stderr, "Error: extra stuff on command line\n    ");
	for (j = 0; j < argc; j++)
	    fprintf (stderr, " %s", argv [j]);
	fprintf (stderr, "\n");
	printUsage (stderr, argc, argv, EXIT_ERROR);
    }

    // ----------------
    // Setup output file

    fpo = setupOutputFile (argv [1], s);

    // ----------------

    numErrs = 0;
    modp  = NULL;

    // testLexer (argv[1]);    // DEBUG

    // ---- Parse the input file
    printf ("Parsing ...\n");
    modp = parseFile (argv [1]);
    if (modp == NULL) goto DONE;
    if (showParse)
	fprintAST (stdout, 0, modp);

    // ---- Build graph data structure
    printf ("Building graph ...\n");
    moduleGraphp = buildGraph (modp);
    if (showGraph)
	fprintGraph (stdout, moduleGraphp);

    // ---- Print misc. header info
    fprintHeaders (fpo, BANNER, argv[1]);

    // ---- Analyse area and print report
    printf ("Analysing area ...\n");
    analyseArea (fpo, moduleGraphp);

    // ---- Analyse paths and print report
    printf ("Analysing paths ...\n");
    analysePaths (fpo, moduleGraphp, nPaths);

    // ---- Analyse nets and print report
    printf ("Analysing nets ...\n");
    analyseNets (fpo, moduleGraphp, nFanouts);

    // ---- Print misc. trailer info
    fprintTrailers (fpo);

    goto DONE;

 DONE:
    fclose (fpo);

    if (verbosity > 1)
	print_checked_malloc_stats (stdout, verbosity);

    return 0;
} // main ()

// ****************************************************************
