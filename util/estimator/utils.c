/* -*-	Mode:C; c-basic-offset:4 -*- */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

#include "utils.h"

/* ================================================================
 * Global variable to control chattiness
 */

int verbosity;

/* ================================================================
 * ASSERT1_FUNCTION()
 * Our own version of assert, that calls exit() instead of abort()
 */

void
assert1_function (Bool           cond,
		  char          *source_file,
		  unsigned int   source_line,
		  char          *message)
{
    if (cond) return;

    fprintf (stderr, "***** Assertion failed at %s, line %d\n", source_file, source_line);
    fprintf (stderr, "%s\n", message);
    exit (1);
} /* assert1_function() */

/* ================================================================
 * Malloc and free with some accounting
 */

static
int num_mallocs = 0,
    num_mallocd_bytes = 0,
    num_frees = 0,
    num_freed_bytes = 0;

static
int num_mallocs_prev = 0,
    num_mallocd_bytes_prev = 0,
    num_frees_prev = 0,
    num_freed_bytes_prev = 0;

/* ---- Will print out info on each malloc and free */
/* #define TRACE_MALLOCS */

/* ----------------------------------------------------------------
 * Infrastructure to keep track of mallocs/frees and make sure they
 * are matched correctly.
 */

#define DEBUG_MALLOC

#ifdef DEBUG_MALLOC

typedef struct MallocRecord {
    void  *address;
    int    size;          /* If malloc'd-but-not-freed */
    char  *clientName;    /* last client's name */
    int    numMallocs;    /* Positive: malloc'd-but-not-freed; negative: freed */
} MallocRecord;

#define MAX_MALLOC_RECORDS (1<<20)

static
MallocRecord  mallocRecords [MAX_MALLOC_RECORDS];
static
int  numMallocRecords = 0;

/* ----------------
 * fprintMallocRecords()
 */

static
void
fprintMallocRecords (FILE *fpOut, Bool all)
{
    int   j, n;
    Bool  active;
    Bool  first;

    first = TRUE;
    for (j = 0; j < numMallocRecords; j++) {
	n = mallocRecords [j].numMallocs;
	active = (n > 0);
	n = ABS (n);
	if (active || all) {
	    if (first) {
		fprintf (fpOut, "Malloc records\n");
		fprintf (fpOut, "    # Lives  Status  Address      Size  Last Client\n");
		first = FALSE;
	    }
	    fprintf (fpOut, "%5d  %4d  %6s  %p  %6d  %s\n",
		     j,
		     n,
		     (active ? "Active" : "Free  "),
		     mallocRecords [j].address,
		     mallocRecords [j].size,
		     mallocRecords [j].clientName);
	}
    }
} /* fprintMallocRecords() */

/* ----------------
 * findMallocRecord()
 * Uses binary search to find a malloc record for a given pointer address.
 * Returns an index:
 * If there is a match, return the index of the match
 * If there is no match, return the index where it would be inserted
 */

static
Bool
findMallocRecord (void *address, int *ixP)
{
    Bool  found;
    int   lo, hi, mid;

    found = FALSE;
    lo    = 0;
    hi    = numMallocRecords - 1;
    while ((! found) && (lo <= hi)) {
	mid = (lo + hi) / 2;
	if (address < mallocRecords [mid].address)
	    hi = mid - 1;
	else if (address > mallocRecords [mid].address)
	    lo = mid + 1;
	else {
	    found = TRUE;
	    lo    = mid;
	    break;
	}
    }

#if 0
    printf ("----------------------------------------------------------------\n");
    printf ("findMallocRecords: %s, ix = %d\n", (found ? "Reuse" : "New"), lo);
    fprintMallocRecords (stdout);
    printf ("----------------------------------------------------------------\n");
#endif

    *ixP = lo;
    return found;
} /* findMallocRecord() */

/* ----------------
 * insertMallocRecord()
 * insert a record at index ix, pushing subsequent records down
 */

static
void
insertMallocRecord (int ix)
{
    int  j;

    if (numMallocRecords == MAX_MALLOC_RECORDS) {
	fprintf (stderr, "insertMallocRecord: too many malloc records (%d)\n",
		 numMallocRecords);
	assert (FALSE);
	/* ***** TODO: instead of quitting, toss an old entry and re-use;
	 * Of course, this loses accuracy about the tossed entry, but that's
	 * better than quitting.
	 */
    }
    for (j = numMallocRecords - 1; j >= ix; j--) {
	mallocRecords [j+1] = mallocRecords [j];
    }
    numMallocRecords++;
} /* insertMallocRecord() */

#endif

/* ----------------
 * checked_malloc()
 * Malloc, with wrapper testing for success and printing an err msg otherwise
 */

void *
checked_malloc (int size, char *clientName)
{
    void *p;

#ifdef DEBUG_MALLOC
    Bool  found;
    int   ix;
#endif

    p = (void *) malloc (size);
    if (p == NULL) {
	fprintf (stderr, "Error: couldn't malloc %d bytes for %s\n", size, clientName);
	assert (FALSE);
    }

#ifdef DEBUG_MALLOC
    found = findMallocRecord (p, & ix);
    if (! found) {
	insertMallocRecord (ix);
	mallocRecords [ix].numMallocs = 1;
    }
    else if (mallocRecords [ix].numMallocs > 0) {
	fprintf (stderr,
		 "Error: checked_malloc (): Block at 0x%p already allocated\n", p);
	fprintf (stderr,
		 "       Was allocated by %s at size %d\n",
		 mallocRecords [ix].clientName, mallocRecords [ix].size);
	fprintf (stderr,
		 "       Now allocated by %s at size %d\n", clientName, size);
	assert (FALSE);
    }
    else {
	mallocRecords [ix].numMallocs = 1 + (- mallocRecords [ix].numMallocs);
    }
    mallocRecords [ix].address    = p;
    mallocRecords [ix].size       = size;
    mallocRecords [ix].clientName = clientName;
#endif

    num_mallocs++;
    num_mallocd_bytes += size;

#ifdef TRACE_MALLOCS
    printf ("%p checked_malloc (%d, %s)\n", p, size, clientName);
#endif

    return p;
} /* checked_malloc() */
    
/* ----------------
 * checked_free()
 * Free, with wrapper testing for success and printing an err msg otherwise
 */

void
checked_free (void *p, int size, char *clientName)
{
#ifdef DEBUG_MALLOC
    Bool  found;
    int   ix;
#endif

#ifdef DEBUG_MALLOC
    found = findMallocRecord (p, & ix);
    if (! found) {
	fprintf (stderr,
		 "Error: checked_free(): Block at 0x%p was never allocated\n", p);
	assert (FALSE);
    }
    else if (mallocRecords [ix].numMallocs < 0) {
	fprintf (stderr,
		 "Error: checked_free(): Block at 0x%p already free.\n", p);
	fprintf (stderr,
		 "       Was freed at size %d by %s\n",
		 mallocRecords [ix].size,
		 mallocRecords [ix].clientName);
	fprintf (stderr,
		 "       Now freed at size %d by %s\n",
		 size, clientName);
	assert (FALSE);
    }
    else if (mallocRecords [ix].size != size) {
	fprintf (stderr, "Error: checked_free(): Block at 0x%p size error\n", p);
	fprintf (stderr,
		 "       Was malloc'd at size %d by %s\n",
		 mallocRecords [ix].size, mallocRecords [ix].clientName);
	fprintf (stderr,
		 "       Now    freed at size %d by %s\n",
		 size, clientName);
	assert (FALSE);
    }
    else {
	mallocRecords [ix].numMallocs = (- mallocRecords [ix].numMallocs);
    }
    mallocRecords [ix].clientName = clientName;
#endif

    num_frees++;
    num_freed_bytes += size;
#ifdef TRACE_MALLOCS
    printf ("%p checked_free (%d, %s)\n", p, size, clientName);
#endif
    free (p);
} /* checked_free() */
    
/* ----------------
 * print_checked_malloc_stats()
 */

void
print_checked_malloc_stats (FILE *fp, int verbosity)
{
    if (verbosity > 0)
	fprintf (fp, "Mallocs %d (%d B), frees %d (%d B), diff %d (%d B) Total\n",
		 num_mallocs, num_mallocd_bytes,
		 num_frees,   num_freed_bytes,
		 num_mallocs - num_frees,
		 num_mallocd_bytes - num_freed_bytes);
    if (verbosity > 1) {
	fprintf (fp, "Mallocs %d (%d B), frees %d (%d B), diff %d (%d B) Incremental\n",
		 num_mallocs - num_mallocs_prev,
		 num_mallocd_bytes - num_mallocd_bytes_prev,
		 num_frees - num_frees_prev,
		 num_freed_bytes - num_freed_bytes_prev,
		 num_mallocs - num_mallocs_prev - (num_frees - num_frees_prev),
		 num_mallocd_bytes - num_mallocd_bytes_prev -
		 (num_freed_bytes - num_freed_bytes_prev));
    }

    num_mallocs_prev       = num_mallocs;
    num_mallocd_bytes_prev = num_mallocd_bytes;
    num_frees_prev         = num_frees;
    num_freed_bytes_prev   = num_freed_bytes;

#ifdef DEBUG_MALLOC
    if (verbosity > 2) {
	fprintMallocRecords (fp, FALSE);
    }
#endif
} /* print_checked_malloc_stats() */

/* ================================================================
 * Freelists
 */

struct Freelist {
    int    sizeB;
    struct Freelist_cell  *head;
};

typedef struct Freelist_cell {
    struct Freelist_cell  *next;
} Freelist_cell;

/* ----------------------------------------------------------------
 * FREELIST_MAKE()
 * Allocates and initializes a freelist for objects of given size
 */

Freelist
freelist_make (int  sizeB)
{
    Freelist  listp;

    if (sizeB < sizeof (Freelist_cell)) {
	fprintf (stderr, "Error (freelist_make) sizeB arg is %d; increasing it to %d\n",
		 sizeB,
		 sizeof (Freelist_cell));
	sizeB = sizeof (Freelist_cell);
    }

    listp        = checked_malloc (sizeof (struct Freelist), "freelist_make");
    listp->sizeB = sizeB;
    listp->head  = NULL;
    return listp;
} /* freelist_make () */

/* ----------------------------------------------------------------
 * FREELIST_MALLOC()
 */

void *
freelist_malloc (Freelist  listp)
{
    void *objp;

    assert (listp != NULL);

    if (listp->head != NULL) {
	objp        = listp->head;
	listp->head = listp->head->next;
    }
    else {
	objp = checked_malloc (listp->sizeB, "freelist_malloc");
    }
    return objp;
} /* freelist_malloc() */

/* ----------------------------------------------------------------
 * FREELIST_FREE()
 */

void
freelist_free (void  *objp, Freelist listp)
{
    Freelist_cell  *cell_p;

    assert (listp != NULL);

    cell_p       = (Freelist_cell *) objp;
    cell_p->next = listp->head;
    listp->head  = cell_p;
} /* freelist_free() */

/* ================================================================
 * FPRINTF_INT64_t()
 * Prints a 64-bit int, optionally with commas between 10^3 groups
 */

#define MAX_DIGITS  1024

void
fprintf_INT64_t (FILE *fp, NumberStyle  nStyle,  char  *text,  INT64_t  x)
{
    char  buf [MAX_DIGITS];
    int   j, n;
    Bool  neg;

    fprintf (fp, "%s", text);
    if (x == 0) {
	fputc ('0', fp);
	return;
    }

    neg = (x < 0);
    if (neg) x = -x;

    n = 0;
    while (x > 0) {
	if (n >= MAX_DIGITS) {
	    /* ---- give up; use standard fprintf */
	    fprintf (fp, "%" LL "d", x);
	    return;
	}

	buf [n] = (x % 10) + '0';
	n++;
	x = x / 10;
    }
    if (neg) fputc ('-', fp);

    for (j = n-1; j >= 0; j--) {
	fputc (buf[j], fp);
	if ((nStyle == WITH_COMMAS) && ((j % 3) == 0) && (j > 0))
	    fputc (',', fp);
    }
} /* fprintf_INT64_t() */

/* ================================================================
 * SSCANF_INT64_t()
 * Parses a 64-bit int from a string, ignoring any commas that may
 * be present for grouping.
 * Return value represents "number of successful scan conversions"
 * i.e., 0 if fail, 1 if success, like sscanf.
 * Also returns the result (via result_p), and
 *     if newpos_p is not NULL, the index just after the last digit
 *
 * Note parsing from pos 2 in "At 123,456, he gave up"
 * returns 123456, with newpos_p pointing at the second comma,
 * i.e., the second comma is not part of the integer.
 */

int
sscanf_INT64_t (char *buf, int pos, INT64_t *result_p, int  *newpos_p)
{
    int      pos_last_digit;
    INT64_t  result;
    Bool     isNeg;

    /* ---- Skip leading blanks */
    while ((buf [pos] == ' ') || (buf [pos] == '\t')) pos++;

    /* ---- Gather sign, if present */
    isNeg = FALSE;
    if (buf [pos] == '-') {
	isNeg = TRUE;
	pos++;
    }

    if (! isdigit (buf[pos])) return 0;

    result = 0;
    while (TRUE) {
	if (buf [pos] == ',') {
	    pos++;
	    continue;
	}
	else if (isdigit (buf [pos])) {
	    result = (result * 10) + buf [pos] - '0';
	    pos_last_digit = pos;
	    pos++;
	}
	else
	    break;
    }
    if (isNeg) result = -result;
    *result_p = result;
    if (newpos_p != NULL) *newpos_p = pos_last_digit + 1;
    return 1;
} /* sscanf_INT64_t() */

/* ================================================================
 * FPRINT_INDENT()
 * Prints n spaces
 */

void
fprint_indent (FILE *fp, int  n)
{
    int j;

    for (j = 0; j < n; j++) fputc (' ', fp);
} /* fprint_indent() */

/* ================================================================
 * fprintAllCaps()
 * Print a string converting all alpha chars to uppercase
 */

void
fprintAllCaps (FILE *fpOut, char *pre, char *s, char *post)
{
    if (pre != NULL) fprintf (fpOut, "%s", pre);

    for ( ; *s != 0 ; s++)
	fputc (toupper (*s), fpOut);

    if (post != NULL) fprintf (fpOut, "%s", post);
} /* fprintAllCaps() */

/* ================================================================
 * fprintFixedWidthHex()
 * Print an integer in hex format with given width
 */

void
fprintFixedWidthHex (FILE     *fpOut,
		     char     *preS,
		     int       bitWidth,
		     UINT64_t  n,
		     char     *postS)
{
    char      format [16];
    int       hexWidth;
    UINT64_t  shift;

    if (preS != NULL) fprintf (fpOut, "%s", preS);

    shift = 1;
    n = (n & ((shift << bitWidth) - 1));

    hexWidth = 1 + ((bitWidth - 1) / 4);
    sprintf (format, "0x%%0%d" LL "x", hexWidth);
    fprintf (fpOut, format, n);

    if (postS != NULL) fprintf (fpOut, "%s", postS);
} /* fprintFixedWidthHex() */

/* ================================================================
 * fprintVerilogHex()
 * Print an integer in Verilog hex format (e.g., 24'h0006BF)
 */

void
fprintVerilogHex (FILE *fpOut,
		  char *preS,
		  int bitWidth,
		  UINT64_t n,
		  char *postS)
{
    char  format [16];
    int   hexWidth;
    UINT64_t  shift;

    if (preS != NULL) fprintf (fpOut, "%s", preS);

    shift = 1;
    n = (n & ((shift << bitWidth) - 1));

    hexWidth = 1 + ((bitWidth - 1) / 4);
    sprintf (format, "%d'h%%0%d" LL "x", bitWidth, hexWidth);
    fprintf (fpOut, format, n);

    if (postS != NULL) fprintf (fpOut, "%s", postS);
} /* fprintVerilogHex() */

/* ================================================================
 * Resizable arrays
 */

typedef struct ArrayDescr {
    int  N;
    int  Max;
    int  elemSize;
} ArrayDescr;

/* ----------------------------------------------------------------
 * arrayIsFull()
 * Checks if current # elems in array == allocated size of array
 */

Bool
arrayIsFull (void *arr)
{
    ArrayDescr *adp;

    assert (arr != NULL);

    adp = arr;
    return (adp [-1].N == adp [-1].Max);
} /* arrayIsFull() */

/* ----------------------------------------------------------------
 * arrayMax()
 * The max number of elements in the currently allocated size of array
 */

int
arrayMax (void *arr)
{
    ArrayDescr *adp;

    assert (arr != NULL);

    adp = arr;
    return adp[-1].Max;
} /* arrayMax() */

/* ----------------------------------------------------------------
 * arrayElemSize()
 * The size of each element in the array
 */

int
arrayElemSize (void *arr)
{
    ArrayDescr *adp;

    assert (arr != NULL);

    adp = arr;
    return adp[-1].elemSize;
} /* arrayElemSize() */

/* ----------------------------------------------------------------
 * arrayN()
 * Number of elements currently in an array
 */

int
arrayN (void *arr)
{
    ArrayDescr *adp;

    assert (arr != NULL);

    adp = arr;
    return adp[-1].N;
} /* arrayN() */

/* ----------------------------------------------------------------
 * arrayN_set()
 * sets the number of elements currently in an array
 */

void
arrayN_set (void *arr, int N)
{
    ArrayDescr *adp;

    assert (arr != NULL);

    adp = arr;
    adp[-1].N = N;
} /* arrayN_set() */

/* ----------------------------------------------------------------
 * allocArray()
 * Create an array with an initial Max number of elems
 */

void *
allocArray (int   Max,
	    int   elemSize,
	    char *clientName)
{
    ArrayDescr *adp;

    adp = checked_malloc (sizeof (ArrayDescr) + (Max * elemSize), clientName);

    adp [0].N        = 0;
    adp [0].Max      = Max;
    adp [0].elemSize = elemSize;

    return adp + 1;
} /* allocArray() */

/* ----------------------------------------------------------------
 * freeArray()
 */

void
freeArray (void *arr, char *clientName)
{
    ArrayDescr *adp;

    assert (arr != NULL);

    adp = arr;
    adp = adp - 1;
    checked_free (adp,
		  sizeof (ArrayDescr) + (adp [0].Max * adp [0].elemSize),
		  clientName);
} /* freeArray() */

/* ----------------------------------------------------------------
 * copyArray()
 * make and return a copy of a given array with MAX elements
 */

void *
copyArray (void  *arr,
	   char  *clientName)
{
    void       *newArr;

    assert (arr != NULL);

    newArr = allocArray (arrayMax (arr), arrayElemSize (arr), clientName);

    memcpy (newArr, arr, arrayN (arr) * arrayElemSize (arr));

    arrayN_set (newArr, arrayN (arr));

    return newArr;
} /* copyArray() */

/* ----------------------------------------------------------------
 * growArray()
 * given an array with MAX elements,
 * allocate a new array with    MAX + DELTAMAX     elems
 * copy common contents from old array into new array
 * free the old array,
 * return the new array.
 */

void *
growArray (void  *arr,
	   int    deltaMax,
	   char  *clientName)
{
    void       *newArr;

    assert (arr != NULL);
    assert (deltaMax > 0);

    newArr = allocArray (arrayMax (arr) + deltaMax, arrayElemSize (arr), clientName);

    memcpy (newArr, arr, arrayN (arr) * arrayElemSize (arr));

    arrayN_set (newArr, arrayN (arr));

    freeArray (arr, clientName);

    return newArr;
} /* growArray() */

/* ----------------------------------------------------------------
 * appendArrayElement()
 * Given an array with N elements,
 * creates a new N+1'st element.
 * If datap != NULL, assigns the data into the new element.
 * Grows the array storage, if necessary, by deltaMax elements, using growArray().
 * Returns a pointer to the (possibly new) array.
 */

void *
appendArrayElement (void  *arr,
		    void  *datap,
		    int    deltaMax,
		    char  *clientName)
{
    int    n, elemSize;
    char  *p;

    n = arrayN (arr);
    if (n == arrayMax (arr))
	arr = growArray (arr, deltaMax, clientName);

    arrayN_set (arr, n + 1);

    if (datap != NULL) {
	elemSize = arrayElemSize (arr);
	p        = arr;

	memcpy (p + n * elemSize, datap, elemSize);
    }

    return arr;
} /* appendArrayElement() */

/* ================================================================
 * Polling for input on stdin
 * Returns true if something can be read from stdin, false otherwise
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

Bool
poll_stdin (void)
{
    fd_set          rfds;
    struct timeval  tv;
    int             retval;

    /* ---- Watch stdin (fd 0) to see when it has input. */
    FD_ZERO(& rfds);
    FD_SET(0, & rfds);

    /* ---- Wait zero seconds. */
    tv.tv_sec =  0;
    tv.tv_usec = 0;

    retval = select(1, & rfds, NULL, NULL, & tv);

    return retval;
} /* poll_stdin() */

/* ================================================================
 * Time of day
 */

#include <time.h>

void
fprintTimeOfDay (FILE *fp, Bool LocalElseUTC)
{
    time_t     t;
    char      *cp, *cp1;

    /* Get seconds since the Epoch (00:00:00 UTC, Jan 1 1970) */
    t = time (NULL);
    if (LocalElseUTC)
	cp = asctime (localtime (& t));
    else
	cp = asctime (gmtime (& t));

    /* ---- Kill the annoying \n that asctime() introduced\n */
    for (cp1 = cp; *cp1 != 0; cp1++) {
	if (*cp1 == '\n') {
	    *cp1 = 0;
	    break;
	}
    }

    fprintf (fp, "%s", cp);
} /* fprintTimeOfDay() */

/* ================================================================
 * COMMAND-LINE FLAG PROCESSING
 */

/* ----------------------------------------------------------------
 * REMOVEFLAG()
 * Updates argc and argv by removing the j'th entry
 */

void
removeflag (int   *argc_p,
	    char  *argv[],
	    int   j)
{
    int k;

    for (k = j+1; k < *argc_p; k++)
	argv[k-1] = argv[k];
    *argc_p = *argc_p - 1;
} /* removeflag() */

/* ----------------------------------------------------------------
 * FINDFLAG()
 * Looks for a flag in argv, from argv[1] onwards.
 * If found, returns its index.
 * If not found, returns 0
 * Aassumption: program name is at argv[0], so flag is never at index 0.
 * If REMOVE_IT is TRUE, also removes it from argv.
 */

int
findflag (int   *argc_p,
	  char  *argv[],
	  char  *flag,
	  Bool   caseSensitive,
	  Bool   remove_it)
{
    int j;

    for (j = 1; j < *argc_p; j++) {
	if ((caseSensitive && (strcmp (flag, argv[j]) == 0)) ||
	    ((! caseSensitive) && (strcasecmp (flag, argv[j]) == 0))) {
	    if (remove_it) removeflag (argc_p, argv, j);
	    return j;
	}
    }
    /* ---- Flag not found */
    return 0;
} /* findflag() */

/* **************************************************************** */
