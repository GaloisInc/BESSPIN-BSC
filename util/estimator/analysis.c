/* -*-  Mode:C; c-basic-offset:4 -*- */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <time.h>
#include <math.h>

extern
double round (double x);

#include "utils.h"
#include "parser.h"
#include "graph.h"
#include "analysis.h"

// ****************************************************************
// Misc.

// ----------------------------------------------------------------
// Integer Log Function (1 + index of left-most non-zero bit)

static
int
intLog (int width)
{
    int  cost;

    for (cost = 0; width != 0; cost++)
	width = width >> 1;

    return cost;
} // intLog()

// ----------------------------------------------------------------
// Cost of an operator instance
// TODO: This will ultimately have to be table-driven, with the table
// loaded from an external file

static
int
opInstCost (ModuleGraph *mgp, int nodeIx)
{
    int     cost, netIx, width;
    Token  *tokp;

    tokp = mgp->opInsts [nodeIx].node;
    assert (tokp->tokType == TokExpr);

    switch (tokp->op) {

    // zero cost
    case OpBitSelect1:
    case OpBitSelect2:
    case OpBitConcat:
    case OpBitReplicate:
    case OpConst: {
	cost = 0;
	break;
    }

    // constant, independent of arg width
    case OpBitOr:
    case OpBitExOr:
    case OpBitAnd:
    case OpBitNot: {
	cost = 1;
	break;
    }

    case OpLShift:
    case OpRShift:
    case OpTimes:
    case OpIntDiv:
    case OpMod: {
	netIx = mgp->opInsts [nodeIx].ports [1].netIx;
        if (mgp->nets [netIx].upperBound != INVALID) {
            width = mgp->nets [netIx].upperBound - mgp->nets [netIx].lowerBound + 1;
            cost = intLog (width) * width * 3;
        }
        else
            cost = 1;
	break;
    }

    case OpLT:
    case OpLE:
    case OpGE:
    case OpGT:
    case OpPlus:
    case OpMinus: {
	netIx = mgp->opInsts [nodeIx].ports [1].netIx;
        if (mgp->nets [netIx].upperBound != INVALID) {
            width = mgp->nets [netIx].upperBound - mgp->nets [netIx].lowerBound + 1;
            cost = intLog (width) * 3;
        }
        else
            cost = 1;
	break;
    }

    // Log width of (either) input (all inputs are same width, so use port 1)
    case OpLogicalOr:
    case OpLogicalAnd:
    case OpLogicalNot:
    case OpEQ:
    case OpNE: {
	netIx = mgp->opInsts [nodeIx].ports [1].netIx;
        if (mgp->nets [netIx].upperBound != INVALID) {
            width = mgp->nets [netIx].upperBound - mgp->nets [netIx].lowerBound + 1;
            cost = intLog(width);
        }
        else
            cost = 1;
	break;
    }

    case OpCondExpr: {
	// Log cost, for testing whether predicate (port 1) == 0
	netIx = mgp->opInsts [nodeIx].ports [1].netIx;
	assert (mgp->nets [netIx].upperBound != INVALID);
	width = mgp->nets [netIx].upperBound - mgp->nets [netIx].lowerBound + 1;
	cost = intLog (width);

	// Plus 2 for muxing the arms (AND-OR)
	cost += 2;
	break;
    }

    case OpCase: {
	// Log cost, for testing whether discriminating expr (port 1) == each case value
	netIx = mgp->opInsts [nodeIx].ports [1].netIx;
	assert (mgp->nets [netIx].upperBound != INVALID);
	width = mgp->nets [netIx].upperBound - mgp->nets [netIx].lowerBound + 1;
	cost = intLog (width);

	// Plus 2 for muxing the arms (AND-OR)
	cost += 2;
	break;
    }
    }
    return cost;
} // opInstcost

// ----------------------------------------------------------------
// fprintHeaders()

void
fprintHeaders (FILE *fpo, char *banner, char *inputFileName)
{
    struct tm *utcTimep;
    time_t  t;

    fprintf (fpo, banner);

    t = time (NULL);
    utcTimep   = gmtime (& t);

    fprintf (fpo, "\n");
    fprintf (fpo, "Analysis of %s created at:    UTC %s", inputFileName, asctime (utcTimep));

    fprintf (fpo, "----------------------------------------------------------------\n");
    fprintf (fpo, "NOTES:\n");
    fprintf (fpo, "    Operators are printed as: \n");
    fprintf (fpo, "            <op> (<line>,<col>) [<I/O pin>]\n");
    fprintf (fpo, "    [0] represents the output pin of an operator \n");
    fprintf (fpo, "    [1], [2], ... represent the inputs pin of an operator \n");
    fprintf (fpo, "    e.g.,   && [2] (379.44)\n");
    fprintf (fpo, "    represents input pin 2 of the '&&' operator on line 379, col 44 of the source\n");
} // fprintHeaders()

// ----------------------------------------------------------------
// fprintTrailers()

void
fprintTrailers (FILE *fpo)
{
    fprintf (fpo, "----------------------------------------------------------------\n");
} // fprintTrailers

// ----------------------------------------------------------------
// fprintModuleInstPort()

static
void
fprintModuleInstPort (FILE *fpo, ModuleGraph *mgp, int nodeIx, int portIx)
{
    AST   *astp;
    List  *xs;
    Token *tokp;

    astp = mgp->moduleInsts[nodeIx].node;
    assert (astp->tag == TagModuleInst);

    xs   = listIndex (astp->u.moduleInstAttrs.ports, portIx);
    tokp = xs->head;

    if (nodeIx != 0)
	fprintf (fpo, "%s #(...) %s(... .%s() ...)",
		 astp->u.moduleInstAttrs.moduleName->tokSVal,
		 astp->u.moduleInstAttrs.moduleInstName->tokSVal,
		 tokp->tokSVal);
    else {
	switch (mgp->moduleInsts [nodeIx].ports [portIx].portDir) {
	case PORT_UNKNOWN_DIR: fprintf (fpo, "Module I/O port"); break;
	case PORT_INPUT:       fprintf (fpo, "Module output port"); break;
	case PORT_OUTPUT:      fprintf (fpo, "Module input port"); break;
	}
	fprintf (fpo, " %s", tokp->tokSVal);
    }
} // fprintModulePortInfo()

// ----------------------------------------------------------------
// fprintNetInfo()

static
void
fprintNetInfo (FILE *fpo, ModuleGraph *mgp, int netIx)
{
    int     n;
    Token  *tokp;

    if (netIx != INVALID) {
	fprintf (fpo, "net");

	// Print upper and lower bounds
	if (mgp->nets[netIx].upperBound != INVALID) {
	    fprintf (fpo, " [%d:%d]",
		     mgp->nets[netIx].upperBound,
		     mgp->nets[netIx].lowerBound);
	}
	else
	    fprintf (fpo, " [?:?]");

	// Print wire names of this net, if any
	if (arrayN (mgp->nets[netIx].wireNames) > 0) {
	    for (n = 0; n < arrayN (mgp->nets[netIx].wireNames); n++) {
		tokp = mgp->nets[netIx].wireNames[n];
		assert (tokp != NULL);
		assert (tokp->tokType == TokWord);
		fprintf (fpo, "  %s", tokp->tokSVal);
	    }
	}
    }
} // fprintNetInfo()

// ----------------------------------------------------------------
// fprintOpInfo()

static
void
fprintOpInfo (FILE *fpo, ModuleGraph *mgp, int nodeIx, int portIx)
{
    Token *tokp;

    tokp = mgp->opInsts[nodeIx].node;
    assert (tokp->tokType == TokExpr);

    fprintf (fpo, "op ");
    fprintOp (fpo, tokp->op, 15);
    fprintf (fpo, " (%d.%d)", tokp->tokLine, tokp->tokCol);
    if (portIx == 0)
	fprintf (fpo, " output");
    else
	fprintf (fpo, " input %d", portIx);
    fprintf (fpo, " cost %d", opInstCost (mgp, nodeIx));
} // fprintOpInfo()

// ****************************************************************
// Area analysis

// ----------------------------------------------------------------
// Analyse area and print results

void
analyseArea (FILE *fpo, ModuleGraph *mgp)
{
    int     j, nFlops, *opHistogram;
    AST    *astp;
    Token  *tokp;
    List   *xs;
    Bool    other_moduleInsts_exist;

    nFlops = 0;
    other_moduleInsts_exist = FALSE;

    // Compute total area of directly instantiated registers
    // Look at first parameter of each RegN/RegA/RegUN module instance,
    // which is the 'width' of the register

    for (j = 1; j < arrayN (mgp->moduleInsts); j++) {
	// We start at j == 1 because [0] is the I_O_Env module

	astp = mgp->moduleInsts[j].node;

	if ((strcmp (astp->u.moduleInstAttrs.moduleName->tokSVal, "RegN") == 0) ||
	    (strcmp (astp->u.moduleInstAttrs.moduleName->tokSVal, "RegA") == 0) ||
	    (strcmp (astp->u.moduleInstAttrs.moduleName->tokSVal, "RegUN") == 0)) {

	    // Get first param (register width)
	    xs = astp->u.moduleInstAttrs.params->head;
	    // xs is a 2-list of [formal, actual]: get the actual
	    tokp = xs->tail->head;
	    assert (tokp->tokType == TokInt);

	    // Accumulate it
	    nFlops += tokp->tokIVal;
	}
	else
	    other_moduleInsts_exist = TRUE;
    }

    fprintf (fpo, "----------------------------------------------------------------\n");
    fprintf (fpo, "AREA:\n");
    fprintf (fpo, "  Number of flops directly in RegN/RegA/RegUN instances:    %d\n", nFlops);

    if (! other_moduleInsts_exist) {
	fprintf (fpo, "  No other module instances\n");
    }
    else {
	fprintf (fpo, "  Plus area within the following module instances:\n");

	for (j = 1; j < arrayN (mgp->moduleInsts); j++) {
	    // We start at j == 1 because [0] is the I_O_Env module

	    astp = mgp->moduleInsts[j].node;

	    if ((strcmp (astp->u.moduleInstAttrs.moduleName->tokSVal, "RegN") == 0) ||
		(strcmp (astp->u.moduleInstAttrs.moduleName->tokSVal, "RegA") == 0) ||
		(strcmp (astp->u.moduleInstAttrs.moduleName->tokSVal, "RegUN") == 0))
		continue;
	    else {
		fprintf (fpo, "    %s #(...)  %s (...);    //    (%d.%d)\n",
			 astp->u.moduleInstAttrs.moduleName->tokSVal,
			 astp->u.moduleInstAttrs.moduleInstName->tokSVal,
			 astp->u.moduleInstAttrs.moduleInstName->tokLine,
			 astp->u.moduleInstAttrs.moduleInstName->tokCol);
	    }
	}
    }

    fprintf (fpo, "  Number of nets:                  %d\n", arrayN (mgp->nets));
    fprintf (fpo, "  Number of operator instances:    %d\n", arrayN (mgp->opInsts));

    // Operator histogram
    opHistogram = allocArray (LAST_OPCODE + 1, sizeof(int), "analyseArea/opHistogram");
    for (j = 0; j <= LAST_OPCODE; j++)
	opHistogram [j] = 0;
    for (j = 0; j < arrayN (mgp->opInsts); j++) {
	tokp = mgp->opInsts [j].node;
	opHistogram [tokp->op] ++;
    }
    fprintf (fpo, "  Operator instance-count histogram:\n");
    fprintf (fpo, "    Operator      Number of instances\n");
    for (j = 0; j <= LAST_OPCODE; j++) {
	if (opHistogram[j] > 0) {
	    fprintf (fpo, "    ");
	    fprintOp (fpo, j, 12);
	    fprintf (fpo, "    %6d\n", opHistogram [j]);
	}
    }
    freeArray(opHistogram, "analyseArea/free opHistogram");
} // analyseArea()

// ****************************************************************
// Path length analysis

typedef struct {
    int    nodeIx;
    int    inPortIx;
    int    outPortIx;
} PathNode;

typedef struct {
    int        weightedPathLength;
    PathNode  *pathNodes;
} Path;

static
Path       *paths;          // array of paths
static
PathNode   *pathNodeStack;  // array of PathNodes

// ----------------------------------------------------------------
// Comparison function passed to qsort(), for sorting the array of Paths

static
int
pathCompare (const void *vp1, const void *vp2)
{
    const Path *p1, *p2;

    p1 = vp1;
    p2 = vp2;

    if (p1->weightedPathLength > p2->weightedPathLength)
	return -1;
    else if (p1->weightedPathLength == p2->weightedPathLength)
	return 0;
    else
	return 1;
} // pathCompare

// ----------------------------------------------------------------
// Computing a path's weighted path length

static
int
computeWeightedPathLength (ModuleGraph *mgp, Path *paths, int pathIx)
{
    int     j, nodeIx, wpl;

    wpl = 0;
    for (j = 0; j < arrayN (paths[pathIx].pathNodes); j++) {
	nodeIx = paths[pathIx].pathNodes[j].nodeIx;

	// Start of path
	if (j == 0)
	    continue;

	// End of path
	else if (j == (arrayN (paths[pathIx].pathNodes) - 1))
	    continue;

	// Other points on the path (operators)
	else {
	    wpl += opInstCost (mgp, nodeIx);
	}
    }

    return wpl;
} // computeWeightedPathLength()

// ----------------------------------------------------------------
// Printing paths

static
void
fprintPath (FILE *fpo, ModuleGraph *mgp, Path *paths, int pathIx)
{
    int     j, nodeIx, netIx;

    fprintf (fpo, "  Path %d (len %d)\n", pathIx, paths[pathIx].weightedPathLength);

    // First, print summary (start and end points only)
    // Start of path
    j = 0;
    nodeIx = paths[pathIx].pathNodes[j].nodeIx;
    fprintf (fpo, "  start: ");
    fprintModuleInstPort (fpo, mgp, nodeIx, paths[pathIx].pathNodes[j].outPortIx);
    fprintf (fpo, "\n");

    // End of path
    j = arrayN (paths[pathIx].pathNodes) - 1;
    nodeIx = paths[pathIx].pathNodes[j].nodeIx;
    fprintf (fpo, "  end:");
    fprintModuleInstPort (fpo, mgp, nodeIx, paths[pathIx].pathNodes[j].inPortIx);
    fprintf (fpo, "\n");

    // Then, print detail
    fprintf (fpo, "  detail:\n");
    for (j = 0; j < arrayN (paths[pathIx].pathNodes); j++) {
	nodeIx = paths[pathIx].pathNodes[j].nodeIx;

	// Start of path
	if (j == 0) {
	    fprintf (fpo, "    start: ");
	    fprintModuleInstPort (fpo, mgp, nodeIx, paths[pathIx].pathNodes[j].outPortIx);
	    fprintf (fpo, "\n");

	    netIx = INVALID;
	}

	// End of path
	else if (j == (arrayN (paths[pathIx].pathNodes) - 1)) {
	    // Net into the node
	    netIx = mgp->moduleInsts[nodeIx].ports[paths[pathIx].pathNodes[j].inPortIx].netIx;
	    fprintf (fpo, "      ");
	    fprintNetInfo (fpo, mgp, netIx);
	    fprintf (fpo, "\n");

	    // The node itself
	    fprintf (fpo, "    end:");
	    fprintModuleInstPort (fpo, mgp, nodeIx, paths[pathIx].pathNodes[j].inPortIx);
	    fprintf (fpo, "\n");
	}

	// Intermediate points on the path
	else {
	    // Net into the operator
	    netIx = mgp->opInsts[nodeIx].ports[paths[pathIx].pathNodes[j].inPortIx].netIx;
	    fprintf (fpo, "      ");
	    fprintNetInfo (fpo, mgp, netIx);
	    fprintf (fpo, "\n");

	    // The operator itself
	    fprintf (fpo, "    ");
	    fprintOpInfo (fpo, mgp, nodeIx, paths[pathIx].pathNodes[j].inPortIx);
	    fprintf (fpo, "\n");
	}
    }
} /* fprintPath() */

// ----------------------------------------------------------------

static
void
pathNodeStackPush (int nodeIx, int inPortIx, int outPortIx, int stackDepth)
{
    if (stackDepth == arrayMax (pathNodeStack)) {
	pathNodeStack = growArray (pathNodeStack, 100, "addNewPath/grow");
    }

    pathNodeStack [stackDepth].nodeIx    = nodeIx;
    pathNodeStack [stackDepth].inPortIx  = inPortIx;
    pathNodeStack [stackDepth].outPortIx = outPortIx;
    arrayN_set (pathNodeStack, 1 + stackDepth);
} // pathNodeStackPush()

// ----------------------------------------------------------------

static
Path *
addNewPath (Path * paths)
{
    int n;

    n = arrayN (paths);
    if (n == arrayMax (paths)) {
	paths = growArray (paths, 100, "addNewPath/grow");
    }
    arrayN_set (paths, 1 + n);

    paths [n].pathNodes = allocArray (100, sizeof (PathNode), "addNewPath/pathNodes");

    return paths;
} // addNewPath()

// ----------------------------------------------------------------

static
void
buildPaths (ModuleGraph  *mgp,
	    NodeType      nodeType,
	    int           nodeIx,
	    int           portIx,
	    int           stackDepth)
{
    AST    *astp;
    int     netIx, pinIx;

    if (nodeType == MODULEINST) {
	if (mgp->moduleInsts[nodeIx].ports[portIx].portDir == PORT_OUTPUT) {
	    // start of path

	    astp = mgp->moduleInsts[nodeIx].node;

#if 0
	    printf ("DEBUG (buildPaths): start of path at %s port %d\n",
		    astp->u.moduleInstAttrs.moduleInstName->tokSVal,
		    portIx);
#endif

	    pathNodeStackPush (nodeIx, INVALID, portIx, stackDepth);

	    // set up the net to follow recursively
	    netIx = mgp->moduleInsts [nodeIx].ports [portIx].netIx;
	    assert (mgp->moduleInsts [nodeIx].ports [portIx].pinIx == 0);
	}
	else {
	    assert (mgp->moduleInsts[nodeIx].ports[portIx].portDir == PORT_INPUT);
	    // end of path

	    astp = mgp->moduleInsts[nodeIx].node;

#if 0
	    printf ("DEBUG (buildPaths): end of path at %s port %d\n",
		    astp->u.moduleInstAttrs.moduleInstName->tokSVal,
		    portIx);
#endif

	    pathNodeStackPush (nodeIx, portIx, INVALID, stackDepth);
	    paths = addNewPath (paths);
	    paths [arrayN (paths) - 1].pathNodes = copyArray (pathNodeStack,

							      "buildPaths/copyArray");
	    // paths [arrayN (paths) - 1].weightedPathLength = stackDepth + 1;
	    paths [arrayN (paths) - 1].weightedPathLength =
		computeWeightedPathLength (mgp, paths, arrayN (paths) - 1);
	    return;
	}
    }

    else {
	assert (nodeType == OPINST);

#if 0
	printf ("DEBUG (buildPaths): path operator ");
	fprintOp (stdout, mgp->opInsts[nodeIx].node->op, 0);
	printf (" port %d\n", portIx);
#endif

	pathNodeStackPush (nodeIx, portIx, 0, stackDepth);

	// set up the net to follow from the output (port 0) of the operator
	netIx = mgp->opInsts[nodeIx].ports[0].netIx;
	assert (mgp->opInsts[nodeIx].ports[0].pinIx == 0);
    }

    // recurse, i.e., follow the driven net's fanout
#if 0
    printf ("DEBUG (buildPaths): recursing on net %d\n", netIx);
#endif
    for (pinIx = 1; pinIx < arrayN (mgp->nets[netIx].pins); pinIx++) {
	buildPaths (mgp,
		    mgp->nets[netIx].pins[pinIx].nodeType,
		    mgp->nets[netIx].pins[pinIx].nodeIx,
		    mgp->nets[netIx].pins[pinIx].portIx,
		    stackDepth + 1);
    }
} // buildPaths()

// ----------------------------------------------------------------
// Build paths, analyse them, and print results

void
analysePaths (FILE *fpo, ModuleGraph *mgp, int nPaths)
{
    AST        *astp;
    int         j, p, totPathLength, cum;
    int        *pathLengthHistogram;

    paths         = allocArray (100, sizeof (Path),     "analysePaths/paths");
    pathNodeStack = allocArray (100, sizeof (PathNode), "analysePaths/stack");

    for (j = 0; j < arrayN (mgp->moduleInsts); j++) {
	astp = mgp->moduleInsts[j].node;
#if 0
	printf ("DEBUG (analysePaths): analysing paths from instance %s of module %s\n",
		astp->u.moduleInstAttrs.moduleInstName->tokSVal,
		astp->u.moduleInstAttrs.moduleName->tokSVal);
#endif

	for (p = 0; p < arrayN (mgp->moduleInsts[j].ports); p++) {
	    if (mgp->moduleInsts[j].ports[p].portDir == PORT_OUTPUT)
		// Build paths starting from this moduleInst output port
		buildPaths (mgp, MODULEINST, j, p, 0);
	}
    }

    // Sort the paths by descending path length
    qsort (paths,  arrayN (paths),  sizeof (Path),  pathCompare);

    // Initialize the path-length histogram
    pathLengthHistogram = allocArray (paths[0].weightedPathLength + 1,
				      sizeof (int),
				      "analysePaths/pathLengthHistogram");
    arrayN_set (pathLengthHistogram, paths[0].weightedPathLength + 1);
    for (j = 0; j <= paths[0].weightedPathLength; j++)
	pathLengthHistogram[j] = 0;

    // Show the paths (at most the 10 longest paths)
    // and compute path length histogram
    fprintf (fpo, "----------------------------------------------------------------\n");
    fprintf (fpo, "PATHS:\n");
    fprintf (fpo, "  There are %d paths (NOTE: ignoring paths from constants).\n", arrayN (paths));
    if (nPaths != 0) {
	if (nPaths > 0)
	    fprintf (fpo, "  Top %d path-length paths", nPaths);
	else
	    fprintf (fpo, "  All paths");
	fprintf (fpo, " (in descending path-length order):\n");
    }

    totPathLength = 0;
    for (j = 0; j < arrayN (paths); j++) {
	pathLengthHistogram[paths[j].weightedPathLength]++;
	totPathLength += paths [j].weightedPathLength;

	// Show only top nPath nets, if nPaths is specified (>= 0)
	if ((nPaths >= 0) && (j + 1 > nPaths))
	    continue;

	fprintPath (fpo, mgp, paths, j);
    }

    // Standard deviation of path length
    {
	double avg, sumdiffsq, diff, stddev;

	avg       = totPathLength / arrayN (paths);
	sumdiffsq = 0;
	for (j = 0; j < arrayN (paths); j++) {
	    diff = paths [j].weightedPathLength - avg;
	    sumdiffsq += diff * diff;
	}
	stddev = sqrt (sumdiffsq / arrayN (paths));

	fprintf (fpo, "Average path length is = %6.2f    Std. dev. = %6.2f\n", avg, stddev);
    }

    // Show the path-length histogram
    fprintf (fpo, "Path length histogram:\n");
    fprintf (fpo, "    Length:   # of paths       cumulative\n");
    cum = 0;
    for (j = arrayN (pathLengthHistogram) - 1; j >=0 ; j--)
	if (pathLengthHistogram [j] > 0) {
	    double fCumPct = (100.0 * cum) / (0.0 + arrayN (paths));
	    int   iCumPct = round (fCumPct);
	    cum += pathLengthHistogram [j];
	    fprintf (fpo, "    %6d: %12d %12d (%d %%)\n", j, pathLengthHistogram [j], cum, iCumPct);
	}
    freeArray (pathLengthHistogram, "analysePaths/free pathLengthHistogram");
} // analysePaths

// ****************************************************************
// Net fanout analysis

static
ModuleGraph *mgpTmp;        // used by netFanoutCompare()
static
int         *sortedNets;    // array of netIxs

// ----------------
// Comparison function passed to qsort() routine

static
int
netFanoutCompare (const void *vp1, const void *vp2)
{
    const int  *ip1, *ip2;
    int         netIx1, netIx2, f1, f2;

    ip1 = vp1;
    ip2 = vp2;
    netIx1 = *ip1;
    netIx2 = *ip2;

    f1 = arrayN (mgpTmp->nets[netIx1].pins);
    f2 = arrayN (mgpTmp->nets[netIx2].pins);

    if (f1 > f2)       return -1;
    else if (f1 == f2) return 0;
    else               return 1;
} // netFanoutCompare()

// ----------------------------------------------------------------
// Analyse nets and print results

void
analyseNets (FILE *fpo, ModuleGraph *mgp, int nFanouts)
{
    int     j, k, p, n, totNetFanout;
    Token  *tokp;
    int    *fanoutHistogram;

    // Create sortedNets array = indexes of nets, sorted by decreasing fanout
    sortedNets = allocArray (arrayN (mgp->nets), sizeof (int), "fprintGraph/alloc sortedNets");
    arrayN_set (sortedNets, arrayN (mgp->nets));
    for (j = 0; j < arrayN (sortedNets); j++)
	sortedNets[j] = j;
    mgpTmp = mgp;
    qsort (sortedNets, arrayN (sortedNets), sizeof (int), netFanoutCompare);

    // Initialize histogram of fanouts
    fanoutHistogram = allocArray (arrayN (mgp->nets[sortedNets[0]].pins),
				  sizeof (int),
				  "analyseNets/fanoutHistogram");
    arrayN_set (fanoutHistogram, arrayN (mgp->nets[sortedNets[0]].pins));
    for (j = 0; j < arrayN (fanoutHistogram); j++)
	fanoutHistogram [j] = 0;

    fprintf (fpo, "----------------------------------------------------------------\n");
    fprintf (fpo, "NETS:\n");
    fprintf (fpo, "  There are %d nets.", arrayN (mgp->nets));
    if (nFanouts != 0) {
	if (nFanouts >= 0)
	    fprintf (fpo, " Top %d fanout nets", nFanouts);
	else
	    fprintf (fpo, "  All nets");
	fprintf (fpo, " (in descending fanout order):\n");
    }

    totNetFanout = 0;
    for (k = 0; k < arrayN (sortedNets); k++) {

	// update fanout histogram
	j = sortedNets [k];
	fanoutHistogram [arrayN (mgp->nets[j].pins) - 1]++;
	totNetFanout += arrayN (mgp->nets[j].pins) - 1;

	// Show only top nFanout nets, if nFanouts is specified (>= 0)
	if ((nFanouts >= 0) && (k + 1 > nFanouts)) continue;

	fprintf (fpo, "  Net %d (fanout %d)", j, arrayN (mgp->nets[j].pins) - 1);

	// Print upper and lower bounds, if any
	if (mgp->nets[j].upperBound != INVALID) {
	    fprintf (fpo, "    [%d:%d]",  mgp->nets[j].upperBound,  mgp->nets[j].lowerBound);
	}

	// Print wire names of this net, if any
	if (arrayN (mgp->nets[j].wireNames) > 0) {
	    fprintf (fpo, "  ");
	    for (n = 0; n < arrayN (mgp->nets[j].wireNames); n++) {
		tokp = mgp->nets[j].wireNames[n];
		assert (tokp != NULL);
		assert (tokp->tokType == TokWord);
		fprintf (fpo, " %s", tokp->tokSVal);
	    }
	}
	fprintf (fpo, "\n");

	// Print source and sinks of this net
	for (p = 0; p < arrayN (mgp->nets[j].pins); p++) {
	    if (p == 0)
		fprintf (fpo, "      From:\n");
	    else if (p == 1)
		fprintf (fpo, "      To:\n");
	    fprintf (fpo, "          ");
	    if (mgp->nets[j].pins[p].nodeIx != INVALID) {
		if (mgp->nets[j].pins[p].nodeType == MODULEINST) {
		    fprintModuleInstPort (fpo, mgp,
					  mgp->nets[j].pins[p].nodeIx,
					  mgp->nets[j].pins[p].portIx);
		}
		else {
		    fprintOpInfo (fpo, mgp,
				  mgp->nets[j].pins[p].nodeIx,
				  mgp->nets[j].pins[p].portIx);
		}
	    }
	    fprintf (fpo, "\n");
	}
    }
    freeArray (sortedNets, "fprintGraph/free sortedNets");

    fprintf (fpo, "Averge net fanout: %d\n", totNetFanout / arrayN (mgp->nets));

    // Print fanout histogram
    fprintf (fpo, "Fanout histogram\n");
    fprintf (fpo, "    Fanout:  # of nets\n");
    for (j = arrayN (fanoutHistogram) - 1; j >= 0; j--)
	if (fanoutHistogram [j] > 0)
	    fprintf (fpo, "    %6d: %6d\n", j, fanoutHistogram [j]);

    freeArray (fanoutHistogram, "fprintGraph/free fanoutHistogram");

} // analyseNets()

// ****************************************************************
