/* -*-	Mode:C; c-basic-offset:4 -*- */

/* ================================================================
 * Generic definitions and routines
 */

typedef int Bool;

#define TRUE   1
#define FALSE  0

/* ---------------- */

#define VERBOSE 0

#if (VERBOSE == 0)
#define DPRINTF(x)
#define DPRINTF2(x)

#elif (VERBOSE == 1)
#define DPRINTF(x) printf x
#define DPRINTF2(x)

#else
#define DPRINTF(x) printf x
#define DPRINTF2(x) printf x
#endif

extern
int verbosity;

/* ---------------- */

#define EXIT_NORMAL  0
#define EXIT_ERROR   1

/* ---------------- */

typedef unsigned int  UINT;

typedef unsigned char  UINT8_t;

typedef unsigned short UINT16_t;

typedef unsigned int  UINT32_t;
typedef          int  INT32_t;

typedef unsigned long long  UINT64_t;
typedef          long long  INT64_t;

#define ALL_ONES_64       0xFFFFFFFFFFFFFFFFULL

/* ---- This modifier is used in printf format strings for 64b entities */
#define  LL  "ll"

/* ---------------- */

#define streq(s1,s2) (strcmp (s1,s2) == 0)

#define MIN(x1,x2) (((x1) <= (x2)) ? (x1) : (x2))
#define MAX(x1,x2) (((x1) >= (x2)) ? (x1) : (x2))

#define  MSB_IS0(x)  ((x & 0x80000000) == 0)
#define  MSB_IS1(x)  ((x & 0x80000000) != 0)

#define  LSB_IS0(x)  ((x & 0x1) == 0)
#define  LSB_IS1(x)  ((x & 0x1) != 0)

#define  ABS(x)  (((x) < 0) ? (- (x)) : (x))

/* ================================================================
 * ASSERT1_FUNCTION()
 * Our own version of assert, that calls exit() instead of abort()
 */

extern
void
assert1_function (Bool           cond,
		  char          *source_file,
		  unsigned int   source_line,
		  char          *message);

/* ================================================================
 * Malloc and free with some accounting
 */

/* ----------------
 * CHECKED_MALLOC()
 * Malloc, with wrapper testing for success and printing an err msg otherwise
 */

extern
void *
checked_malloc (int size, char *clientName);

/* ----------------
 * CHECKED_FREE()
 * Free, with wrapper testing for success and printing an err msg otherwise
 */

extern
void
checked_free (void *p, int size, char *clientName);

/* ----------------
 * print_checked_malloc_stats()
 */

extern
void
print_checked_malloc_stats (FILE *fp, int verbosity);

/* ================================================================
 * Freelists
 */

typedef struct Freelist  *Freelist;

/* ----------------------------------------------------------------
 * FREELIST_MAKE()
 * Allocates and initializes a freelist for objects of given size
 */

extern
Freelist
freelist_make (int  sizeB);

/* ----------------------------------------------------------------
 * FREELIST_MALLOC()
 */

extern
void *
freelist_malloc (Freelist  listp);

/* ----------------------------------------------------------------
 * FREELIST_FREE()
 */

extern
void
freelist_free (void  *objp, Freelist listp);

/* ================================================================
 * FPRINTF_INT64_t()
 * Prints a 64-bit int, optionally with commas between 10^3 groups
 */

typedef enum NumberStyle_e {
    WITH_COMMAS,
    WITHOUT_COMMAS
} NumberStyle;

extern
void
fprintf_INT64_t (FILE *fp,  NumberStyle  nStyle,  char  *text,  INT64_t  x);

/* ================================================================
 * SSCANF_INT64_t()
 * Parses a 64-bit int from a string, ignoring any commas that may
 * be present for grouping.
 */

extern
int
sscanf_INT64_t (char *buf, int pos, INT64_t *result_p, int  *newpos_p);

/* ================================================================
 * FPRINT_INDENT()
 * Prints n spaces
 */

extern
void
fprint_indent (FILE *fp, int  n);

/* ================================================================
 * fprintAllCaps()
 * Print a string converting all alpha chars to uppercase
 */

void
fprintAllCaps (FILE *fpOut, char *pre, char *s, char *post);

/* ================================================================
 * fprintFixedWidthHex()
 * Print an integer in hex format with given width
 */

extern
void
fprintFixedWidthHex (FILE     *fpOut,
		     char     *preS,
		     int       bitWidth,
		     UINT64_t  n,
		     char     *postS);

/* ================================================================
 * fprintVerilogHex()
 * Print an integer in Verilog hex format (e.g., 24'h0006BF)
 */

extern
void
fprintVerilogHex (FILE     *fpOut,
		  char     *preS,
		  int       bitWidth,
		  UINT64_t  n,
		  char     *postS);

/* ================================================================
 * Resizable arrays
 */

/* ----------------------------------------------------------------
 * arrayIsFull()
 * Checks if current # elems in array == allocated size of array
 */

extern
Bool
arrayIsFull (void *arr);

/* ----------------------------------------------------------------
 * arrayMax()
 * The max number of elements in the currently allocated size of array
 */

extern
int
arrayMax (void *arr);

/* ----------------------------------------------------------------
 * arrayElemSize()
 * The size of each element in the array
 */

extern
int
arrayElemSize (void *arr);

/* ----------------------------------------------------------------
 * arrayN()
 * Number of elements currently in an array
 */

extern
int
arrayN (void *arr);

/* ----------------------------------------------------------------
 * arrayN_set()
 * sets the number of elements currently in an array
 */

extern
void
arrayN_set (void *arr, int N);

/* ----------------------------------------------------------------
 * allocArray()
 * Create an array with an initial Max number of elems
 */

extern
void *
allocArray (int   Max,
	    int   elemSize,
	    char *clientName);

/* ----------------------------------------------------------------
 * freeArray()
 */

extern
void
freeArray (void *arr, char *clientName);

/* ----------------------------------------------------------------
 * copyArray()
 * make and return a copy of a given array with MAX elements
 */

extern
void *
copyArray (void  *arr,
	   char  *clientName);

/* ----------------------------------------------------------------
 * growArray()
 * given an array with MAX elements,
 * allocate a new array with    MAX + DELTAMAX     elems
 * copy common contents from old array into new array
 * free the old array,
 * return the new array.
 */

extern
void *
growArray (void  *arr,
	   int    deltaMax,
	   char  *clientName);

/* ----------------------------------------------------------------
 * appendArrayElement()
 * Given an array with N elements,
 * creates a new N+1'st element.
 * If datap != NULL, assigns the data into the new element.
 * Grows the array storage, if necessary, by deltaMax elements, using growArray().
 * Returns a pointer to the (possibly new) array.
 */

extern
void *
appendArrayElement (void  *arr,
		    void  *datap,
		    int    deltaMax,
		    char  *clientName);

/* ================================================================
 * Polling for input on stdin
 * Returns true if something can be read from stdin, false otherwise
 */

extern
Bool
poll_stdin (void);

/* ================================================================
 * Time of day
 */

extern
void
fprintTimeOfDay (FILE *fp, Bool LocalElseUTC);

/* ================================================================
 * COMMAND-LINE FLAG PROCESSING
 */

#define FLAG_REMOVE  TRUE
#define FLAG_KEEP    FALSE

#define FLAG_CASE_SENSITIVE    TRUE
#define FLAG_CASE_INSENSITIVE  FALSE

/* ----------------------------------------------------------------
 * REMOVEFLAG()
 * Updates argc and argv by removing the j'th entry
 */

extern
void
removeflag (int   *argc_p,
	    char  *argv[],
	    int   j);

/* ----------------------------------------------------------------
 * FINDFLAG()
 * Looks for a flag in argv, from argv[1] onwards.
 * If found, returns its index.
 * If not found, returns 0
 * Assumption: program name is at argv[0], so flag is never at index 0.
 * If REMOVE_IT is TRUE, also removes it from argv.
 */

extern
int
findflag (int   *argc_p,
	  char  *argv[],
	  char  *flag,
	  Bool   caseSensitive,
	  Bool   remove_it);

/* **************************************************************** */
