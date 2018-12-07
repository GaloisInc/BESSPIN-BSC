/* -*-  Mode:C; c-basic-offset:4 -*- */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>

#include "utils.h"
#include "parser.h"
#include "graph.h"

// ****************************************************************
// Building a module circuit graph

// ================================================================
// Load ModuleInsts

static
void
loadModuleInsts (AST *moduleASTp, ModuleGraph *mgp)
{
    int      N, M, mj, pj;
    List    *xs, *ys;
    AST     *astp;
    Token   *tokp, *tokp2;
    PortDir  pdir;

    assert (moduleASTp != NULL);
    assert (moduleASTp->tag == TagModule);
    assert (mgp != NULL);

    // Count the number of module instances
    mj = 0;
    for (xs = moduleASTp->u.moduleAttrs.stmts; xs != NULL; xs = xs->tail) {
	astp = xs->head;
	if (astp->tag == TagModuleInst) mj++;
    }
    N = mj;

    // Allocate the module insts array
    // Note: one extra dummy 'I/O ENV' module inst for this module's own I/O ports
    mgp->moduleInsts = allocArray (N+1,
				   sizeof (Node),
				   "loadModuleInsts/moduleInsts");
    arrayN_set (mgp->moduleInsts, N+1);

    // ----------------
    // Load the I/O ENV module inst from this module's own I/O ports

    // Create a name for the I/O ENV module inst
    tokp = allocASTToken ("loadModuleInst/ENV module name");
    tokp->tokLine = moduleASTp->u.moduleAttrs.moduleName->tokLine;
    tokp->tokCol  = moduleASTp->u.moduleAttrs.moduleName->tokCol;
    tokp->tokType = TokWord;
    tokp->tokSVal = checked_malloc (1 + strlen ("I_O_Env"),
				    "loadModuleInst/I_O_ENV tokSVal");
    strcpy (tokp->tokSVal, "I_O_Env");

    // Create the I/O env module inst
    astp = checked_malloc (sizeof (AST), "loadModuleInsts/I_O_ENV moduleInst");
    astp->tag                              = TagModuleInst;
    astp->u.moduleInstAttrs.moduleName     = tokp;
    astp->u.moduleInstAttrs.params         = NULL;
    astp->u.moduleInstAttrs.moduleInstName = tokp;

    // Use the port list, then replicate them into 2-lists for formal/actual pairs
    astp->u.moduleInstAttrs.ports          = moduleASTp->u.moduleAttrs.ports;
    for (xs = astp->u.moduleInstAttrs.ports; xs != NULL; xs = xs->tail)
	xs->head = list2 (xs->head, xs->head,                              // (formal, actual)
			  "loadModuleInst/I_O_ENV formal-and-actual");

    // Load it into the graph at location 0
    mgp->moduleInsts[0].node = astp;

    M = listLength (astp->u.moduleInstAttrs.ports);
    mgp->moduleInsts[0].ports = allocArray (M,
					    sizeof (Port),
					    "loadModuleInst/I/O env ports");
    arrayN_set (mgp->moduleInsts[0].ports, M);
    xs = astp->u.moduleInstAttrs.ports;
    for (pj = 0; pj < M; pj++) {
	mgp->moduleInsts[0].ports[pj].portName = xs->head;
	mgp->moduleInsts[0].ports[pj].portDir  = PORT_UNKNOWN_DIR;
	mgp->moduleInsts[0].ports[pj].netIx    = -1;
	xs = xs->tail;
    }

    // Initialize I/O port directions
    for (xs = moduleASTp->u.moduleAttrs.stmts; xs != NULL; xs = xs->tail) {
	astp = xs->head;
	if (astp->tag != TagDecl) continue;

	if      (strcmp (astp->u.declAttrs.declKw->tokSVal, "input") == 0)
	    pdir = PORT_OUTPUT;
	else if (strcmp (astp->u.declAttrs.declKw->tokSVal, "output") == 0)
	    pdir = PORT_INPUT;
	else
	    continue;

	for (ys = astp->u.declAttrs.ides; ys != NULL; ys = ys->tail) {
	    tokp = ys->head;
	    for (pj = 0; pj < arrayN (mgp->moduleInsts[0].ports); pj++) {
		tokp2 = mgp->moduleInsts[0].ports[pj].portName->head;
		if (strcmp (tokp->tokSVal, tokp2->tokSVal) == 0) {
		    mgp->moduleInsts[0].ports[pj].portDir = pdir;
#if 0
		    printf ("DEBUG (loadModuleInst): I/O Env port %s (%d) direction is %d\n",
			    tokp2->tokSVal, pj, pdir);
#endif
		    continue;
		}
	    }
	}
    }
    
    // Load the module instances into the array
    mj = 1;
    for (xs = moduleASTp->u.moduleAttrs.stmts; xs != NULL; xs = xs->tail) {
	astp = xs->head;
	if (astp->tag == TagModuleInst) {
	    mgp->moduleInsts[mj].node = astp;
	    M = listLength (astp->u.moduleInstAttrs.ports);
	    mgp->moduleInsts[mj].ports = allocArray (M,
						     sizeof (Port),
						     "loadModuleInst/ports");
	    arrayN_set (mgp->moduleInsts[mj].ports, M);

	    // Load the ports array
	    ys = astp->u.moduleInstAttrs.ports;
	    for (pj = 0; pj < M; pj++) {
		mgp->moduleInsts[mj].ports[pj].portName = ys->head;
		mgp->moduleInsts[mj].ports[pj].portDir  = PORT_UNKNOWN_DIR;
		mgp->moduleInsts[mj].ports[pj].netIx    = -1;
		ys = ys->tail;
	    }
	    mj++;
	}
    }
} // loadModuleInsts();

// ================================================================
// Initialize wire symbol table
// Every wire symbol, eventually, is set to a netIx

typedef struct {
    Token      *name;          // TokWord
    AST        *decl;          // Original declaration containing this symbol
    int         netIx;         // INVALID means uninitalized, else nj
} WireSymbol;

// ----------------

static
WireSymbol *
createWireSymbolTable (AST *moduleASTp)
{
    int          mj, N;
    List        *xs, *ys;
    AST         *astp;
    WireSymbol  *wireSymbols;

    // Count symbols
    mj = 0;
    for (xs = moduleASTp->u.moduleAttrs.stmts; xs != NULL; xs = xs->tail) {
	astp = xs->head;
	if (astp->tag != TagDecl) continue;
	for (ys = astp->u.declAttrs.ides; ys != NULL; ys = ys->tail)
	    mj++;
    }
    N = mj;

#if 0
    printf ("DEBUG (createWireSymboltable): Number of wire symbols is %d\n", N);
#endif

    // Load wire symbols into table
    wireSymbols = allocArray (N, sizeof (WireSymbol), "createWireSymbolTable/wireSymbols");
    arrayN_set (wireSymbols, N);
    mj = 0;
    for (xs = moduleASTp->u.moduleAttrs.stmts; xs != NULL; xs = xs->tail) {
	astp = xs->head;
	if (astp->tag != TagDecl) continue;
	for (ys = astp->u.declAttrs.ides; ys != NULL; ys = ys->tail) {
	    wireSymbols[mj].name   = ys->head;
	    wireSymbols[mj].decl   = astp;
	    wireSymbols[mj].netIx  = INVALID;
	    mj++;
	}
    }
    return wireSymbols;
} // createWireSymbolTable

// ----------------------------------------------------------------
// findSymbol()
// Looks for a symbol in the symbol table and returns its index

static
int
findSymbol (WireSymbol *wireSymbols, Token *tokp)
{
    int j;

    assert (tokp != NULL);
    assert (tokp->tokType == TokWord);

    for (j = 0; j < arrayN (wireSymbols); j++) {
	if (strcmp (tokp->tokSVal, wireSymbols[j].name->tokSVal) == 0) {

#if 0
	    printf ("DEBUG (findSymbol): identifier %s is symbol %d, netIx = %d\n",
		    tokp->tokSVal, j, wireSymbols[j].netIx);
#endif

	    return j;
	}
    }
    fprintf (stderr, "Error (findSymbol): no such wire symbol: %s",
	     tokp->tokSVal);
    exit (1);
} // findSymbol()

// ----------------------------------------------------------------
// createPin()
// Create a new pin for specified net, and return its index

static
int
createPin (ModuleGraph *mgp, int netIx)
{
    int  n;

    // Grow the pins array for this net, if necessary
    n = arrayN (mgp->nets[netIx].pins);
    if (n == arrayMax (mgp->nets[netIx].pins))
	mgp->nets[netIx].pins = growArray (mgp->nets[netIx].pins, 100, "createPin/growArray");
    arrayN_set (mgp->nets[netIx].pins, n+1);

    return n;
} // createPin()

// ----------------------------------------------------------------
// addWireNameToNet()
// Add a new wire name to the specified net

static
void
addWireNameToNet (ModuleGraph *mgp, int netIx, Token *tokp)
{
    int  n;

    assert (tokp != NULL);
    assert (tokp->tokType == TokWord);

    n = arrayN (mgp->nets[netIx].wireNames);
    if (n == arrayMax (mgp->nets[netIx].wireNames)) {
	mgp->nets[netIx].wireNames = growArray (mgp->nets[netIx].wireNames,
						5,
						"addWireNameToNet/growArray");
    }
    arrayN_set (mgp->nets[netIx].wireNames, n+1);

    mgp->nets[netIx].wireNames[n] = tokp;
} // addWireNameToNet()

// ----------------------------------------------------------------
// createNet()
// Create a new net, and return its index

static
int
createNet (ModuleGraph  *mgp)
{
    int  netIx;

    // Grow the net array if necessary
    netIx = arrayN (mgp->nets);
    if (netIx == arrayMax (mgp->nets))
	mgp->nets = growArray (mgp->nets, 100, "createNet/growArray");
    arrayN_set (mgp->nets, netIx + 1);

    // Create the pins for this new net
    mgp->nets[netIx].pins = allocArray (10, sizeof (Pin), "createNet/pins");
    // Reserve one pin for the driver of the net
    arrayN_set (mgp->nets[netIx].pins, 1);
    mgp->nets[netIx].pins[0].nodeIx = INVALID;

    // Create the wireNames array for this new net
    mgp->nets[netIx].wireNames = allocArray (1, sizeof (Token *), "createNet/wireNames");

    mgp->nets[netIx].upperBound = INVALID;
    mgp->nets[netIx].lowerBound = INVALID;

#if 0
    printf ("DEBUG (createNet): created net %d\n", netIx);
#endif

    return netIx;
} // createNet()

// ----------------------------------------------------------------
// createOpInst()
// Create a general operator with NP ports, and return its index

static
int
createOpInst (ModuleGraph *mgp, int np)
{
    int     opIx;

    // Grow the opInsts array, if necessary
    opIx = arrayN (mgp->opInsts);
    if (opIx == arrayMax (mgp->opInsts))
	mgp->opInsts = growArray (mgp->opInsts, 100, "createConstOp/growArray");
    arrayN_set (mgp->opInsts, opIx+1);

    // Create the op's ports
    mgp->opInsts[opIx].ports = allocArray (np, sizeof (Port), "createOpInst/ports");
    arrayN_set (mgp->opInsts[opIx].ports, np);

    return opIx;
} // createOpInst

// ----------------------------------------------------------------
// createConstOpInst()
// Create a constant operator with the given constant, and return its index

static
int
createConstOpInst (ModuleGraph *mgp, Token *constp)
{
    static char constantOutputString[] = "ConstantOutput";
    int     opIx;
    Token  *opp, *outPortNamep;

    // Create the opinst with 1 port (the output port)
    opIx = createOpInst (mgp, 1);

    // Create a Token for the operator
    opp = allocASTToken ("createConstOpInst/opp");
    opp->tokLine            = constp->tokLine;
    opp->tokCol             = constp->tokCol;
    opp->tokType            = TokExpr;
    opp->op                 = OpConst;
    opp->args               = list1 (constp, "createConstOpInst/args");

    mgp->opInsts[opIx].node = opp;

    // Create a Token for the output port name
    outPortNamep = allocASTToken ("createConstOpInst/outPortNamep");
    outPortNamep->tokLine         = constp->tokLine;
    outPortNamep->tokCol          = constp->tokCol;
    outPortNamep->tokType         = TokWord;
    outPortNamep->tokSVal         = & (constantOutputString[0]);

    // Initialize the output port
    mgp->opInsts[opIx].ports[0].portName = list2 (outPortNamep,
						  outPortNamep,
						  "createConstOpInst/formal-actual");
    mgp->opInsts[opIx].ports[0].portDir  = PORT_OUTPUT;
    mgp->opInsts[opIx].ports[0].netIx    = INVALID;
    mgp->opInsts[opIx].ports[0].pinIx    = INVALID;

    return opIx;
} // createConstOpInst

// ----------------------------------------------------------------
// createNoOpInst()
// Create a no-op operator, and return its index

static
int
createNoOpInst (ModuleGraph *mgp, int tokLine, int tokCol)
{
    static char noOpInputString[]  = "NoOpInput";
    static char noOpOutputString[] = "NoOpOutput";
    int     opIx;
    Token  *opp, *inPortNamep, *outPortNamep;

    // Create the op with 2 ports (1 output, 1 input)
    opIx = createOpInst (mgp, 2);

    // Create a Token for the operator
    opp = allocASTToken ("createNoOpInst/opp");
    opp->tokLine         = tokLine;
    opp->tokCol          = tokCol;
    opp->tokType         = TokExpr;
    opp->op              = OpConst;
    opp->args            = NULL;
    mgp->opInsts[opIx].node = opp;

    // Create a Token for the input port name
    inPortNamep = allocASTToken ("createNoOpInst/input portNamep");
    inPortNamep->tokLine         = tokLine;
    inPortNamep->tokCol          = tokCol;
    inPortNamep->tokType         = TokWord;
    inPortNamep->tokSVal         = & (noOpInputString[0]);

    // Create a Token for the output port name
    outPortNamep = allocASTToken ("createNoOpInst/output portNamep");
    outPortNamep->tokLine         = tokLine;
    outPortNamep->tokCol          = tokCol;
    outPortNamep->tokType         = TokWord;
    outPortNamep->tokSVal         = & (noOpOutputString[0]);

    // Initialize the input and output ports
    mgp->opInsts[opIx].ports[0].portName = list2 (outPortNamep, outPortNamep,
						  "createNoOpInst/output formal-actual");
    mgp->opInsts[opIx].ports[0].portDir  = PORT_OUTPUT;
    mgp->opInsts[opIx].ports[0].netIx    = INVALID;
    mgp->opInsts[opIx].ports[0].pinIx    = INVALID;

    mgp->opInsts[opIx].ports[1].portName = list2 (inPortNamep, inPortNamep,
						  "createNoOpInst/input formal-actual");
    mgp->opInsts[opIx].ports[1].portDir  = PORT_INPUT;
    mgp->opInsts[opIx].ports[1].netIx    = INVALID;
    mgp->opInsts[opIx].ports[1].pinIx    = INVALID;

    return opIx;
} // createNoOpInst

// ----------------------------------------------------------------
// processExpr()
// Process an expression
// outputNetIx, if not INVALID, specifies output net of expr
// Returns output net of this expr
// (which will be same as incoming, if not INVALID)

static
int
processExpr (ModuleGraph   *mgp,
	     WireSymbol    *wireSymbols,
	     int            outputNetIx,
	     Token         *exprp)    
{
    int    j, opIx, portIx, netIx, pinIx;
    List  *xs;

    assert (exprp != NULL);

#if 0
    printf ("DEBUG (processExpr): ");
    fprintToken (stdout, exprp, 0);
    printf ("\n");
#endif

    switch (exprp->tokType) {
    case TokInt: {

#if 0
	printf ("DEBUG (processExpr): Constant\n");
#endif

	// Create a 'constant' operator containing the constant
	opIx = createConstOpInst (mgp, exprp);

	// Create a new net for the output, if necessary
	if (outputNetIx == INVALID)
	    outputNetIx = createNet (mgp);

	// Connect output port (port 0) of the op to the output net's pin 0
	mgp->opInsts[opIx].ports[0].portName = NULL;
	mgp->opInsts[opIx].ports[0].portDir  = PORT_OUTPUT;
	mgp->opInsts[opIx].ports[0].netIx    = outputNetIx;
	mgp->opInsts[opIx].ports[0].pinIx    = 0;

	// Connect the output net to the operator's port 0
	mgp->nets[outputNetIx].pins[0].nodeType = OPINST;
	mgp->nets[outputNetIx].pins[0].nodeIx   = opIx;
	mgp->nets[outputNetIx].pins[0].portIx   = 0;

	return outputNetIx;
    }

    case TokWord: {

#if 0
	printf ("DEBUG (processExpr): Identifier. outputNetIx = %d\n", outputNetIx);
#endif

	// Find the identifier in the symbol table
	j = findSymbol (wireSymbols, exprp);

	if ((outputNetIx == INVALID) && (wireSymbols[j].netIx != INVALID)) {
	    // Just return the identifier's netix
	    outputNetIx = wireSymbols[j].netIx;
	}
	else if ((outputNetIx != INVALID) && (wireSymbols[j].netIx == INVALID)) {
	    // Just set the identifier's net to the outputNetix
	    wireSymbols[j].netIx = outputNetIx;
	    addWireNameToNet (mgp, outputNetIx, exprp);
	}
	else if ((outputNetIx == INVALID) && (wireSymbols[j].netIx == INVALID)) {
	    // create a net, set the input ide to that, and return it
	    outputNetIx          = createNet (mgp);
	    wireSymbols[j].netIx = outputNetIx;
	    addWireNameToNet (mgp, outputNetIx, exprp);
	}
	else { // ((outputNetIx != INVALID) && (wireSymbols[j].netIx != INVALID))
	    // Create a 'NoOp' operator
	    opIx = createNoOpInst (mgp, exprp->tokLine, exprp->tokCol);

	    // Connect output port (0) of this no-op to the output net pin 0
	    mgp->opInsts[opIx].ports[0].netIx = outputNetIx;
	    mgp->opInsts[opIx].ports[0].pinIx = 0;

	    mgp->nets[outputNetIx].pins[0].nodeType = OPINST;
	    mgp->nets[outputNetIx].pins[0].nodeIx   = opIx;
	    mgp->nets[outputNetIx].pins[0].portIx   = 0;

	    // Create a new pin for the input net
	    pinIx = createPin (mgp, wireSymbols[j].netIx);
	    // and connect input port (1) of this no-op to the new input net pin
	    mgp->opInsts[opIx].ports[1].netIx = wireSymbols[j].netIx;
	    mgp->opInsts[opIx].ports[1].pinIx = pinIx;

	    mgp->nets[wireSymbols[j].netIx].pins[pinIx].nodeType = OPINST;
	    mgp->nets[wireSymbols[j].netIx].pins[pinIx].nodeIx   = opIx;
	    mgp->nets[wireSymbols[j].netIx].pins[pinIx].portIx   = 1;
	}
	return outputNetIx;
    }

    case TokExpr: {

#if 0
	printf ("DEBUG (processExpr): operation\n");
#endif

	// Create an opInst with ports for 1 output and all inputs
	opIx = createOpInst (mgp, 1 + listLength (exprp->args));
	mgp->opInsts[opIx].node = exprp;

	// Process operator inputs, and connect them
	portIx = 1;
	for (xs = exprp->args; xs != NULL; xs = xs->tail) {

#if 0
	    printf ("DEBUG (processExpr): processing arg %d of ", portIx - 1);
	    fprintToken (stdout, exprp, 0);
	    printf ("\n");
#endif

	    netIx = processExpr (mgp, wireSymbols, INVALID, xs->head);
	    pinIx = createPin (mgp, netIx);

	    // Connect operator port to net
	    mgp->opInsts[opIx].ports[portIx].portName = NULL;
	    mgp->opInsts[opIx].ports[portIx].portDir  = PORT_INPUT;
	    mgp->opInsts[opIx].ports[portIx].netIx    = netIx;
	    mgp->opInsts[opIx].ports[portIx].pinIx    = pinIx;

	    // Connect net to operator port
	    mgp->nets[netIx].pins[pinIx].nodeType = OPINST;
	    mgp->nets[netIx].pins[pinIx].nodeIx   = opIx;
	    mgp->nets[netIx].pins[pinIx].portIx   = portIx;

	    portIx++;
	}

#if 0
	printf ("DEBUG (processExpr): processing output of operator ");
	fprintToken (stdout, exprp, 0);
	printf ("\n");
#endif

	// Create a new net for the output, if necessary
	if (outputNetIx == INVALID)
	    outputNetIx = createNet (mgp);

	// Connect the output port (port 0) of the operator to net pin 0
	mgp->opInsts[opIx].ports[0].portName = NULL;
	mgp->opInsts[opIx].ports[0].portDir  = PORT_OUTPUT;
	mgp->opInsts[opIx].ports[0].netIx    = outputNetIx;
	mgp->opInsts[opIx].ports[0].pinIx    = 0;

	// Connect the output net to the operator port
	mgp->nets[outputNetIx].pins[0].nodeType = OPINST;
	mgp->nets[outputNetIx].pins[0].nodeIx   = opIx;
	mgp->nets[outputNetIx].pins[0].portIx   = 0;

	return outputNetIx;
    }

    default: {
	fprintf (stderr, "Internal Error (processExpr): unknown expr token type %d\n", exprp->tokType);
	exit (1);
    }
    }
    return 0;
} // processExpr()

// ================================================================
// Width inference for nets

// ----------------------------------------------------------------
// computeOpOutputWidth()
// Given a ref to an op inst (nodeIx),
// check that input operand widths are ok
// and, if possible, compute output width [ub:lb]
// If not possible, return INVALID for output width.
// Return [0:0] for single wires

static
void
computeOpOutputWidth (ModuleGraph  *mgp,
		      int           nodeIx,
		      int          *ubp,
		      int          *lbp)
{
    Token *tokp;
    int    ub, lb;

    tokp = mgp->opInsts [nodeIx].node;
    assert (tokp->tokType = TokExpr);

    switch (tokp->op) {
    case OpConst: {
	*ubp = INVALID;
	*lbp = INVALID;
    }
    case OpLogicalOr:
    case OpLogicalAnd:
    case OpLogicalNot:
    case OpEQ:
    case OpNE:
    case OpLT:
    case OpLE:
    case OpGE:
    case OpGT: {
	int portIx;

	// Check all inputs (ports [1]...) have same width
	ub = lb = INVALID;
	for (portIx = 1; portIx < arrayN (mgp->opInsts [nodeIx].ports); portIx++) {
	    int netIx = mgp->opInsts [nodeIx].ports [portIx].netIx;
	    if (mgp->nets [netIx].upperBound != INVALID) {
		if (ub != INVALID) {
		    assert ((ub - lb + 1) ==
			    (mgp->nets [netIx].upperBound -
			     mgp->nets [netIx].lowerBound + 1));
		}
		else {
		    ub = mgp->nets [netIx].upperBound;
		    lb = mgp->nets [netIx].lowerBound;
		}

	    }
	}
	// Output width is 1 wire
	*ubp = 0;
	*lbp = 0;
	break;
    }

    case OpBitOr:
    case OpBitExOr:
    case OpBitAnd:
    case OpBitNot:
    case OpPlus:
    case OpMinus:
    case OpTimes:
    case OpIntDiv:
    case OpMod: {
	int  portIx;

	// Check all inputs (ports [1]...) have same width
	ub = lb = INVALID;
	for (portIx = 1; portIx < arrayN (mgp->opInsts [nodeIx].ports); portIx++) {
	    int netIx = mgp->opInsts [nodeIx].ports [portIx].netIx;
	    if (mgp->nets [netIx].upperBound != INVALID) {
		if (ub != INVALID) {
		    assert ((ub - lb + 1) ==
			    (mgp->nets [netIx].upperBound -
			     mgp->nets [netIx].lowerBound + 1));
		}
		else {
		    ub = mgp->nets [netIx].upperBound;
		    lb = mgp->nets [netIx].lowerBound;
		}

	    }
	}
	// Output width is input width
	*ubp = ub;
	*lbp = lb;
	break;
    }

    case OpLShift:
    case OpRShift: {
	// Output width is same as first input (port [1])
	int netIx = mgp->opInsts [nodeIx].ports [1].netIx;
	*ubp = mgp->nets [netIx].upperBound;
	*lbp = mgp->nets [netIx].lowerBound;
	break;
    }

    case OpBitSelect1: {
	// Output width is 1 wire
	*ubp = 0;
	*lbp = 0;
	break;
    }

    case OpBitSelect2: {
	// Output width is specified in args 1 and 2
	Token *tokArg1p, *tokArg2p;

	tokArg1p = listIndex (tokp->args, 1);
	assert (tokArg1p->tokType == TokInt);
	ub = tokArg1p->tokIVal;

	tokArg2p = listIndex (tokp->args, 2);
	assert (tokArg2p->tokType == TokInt);
	lb = tokArg2p->tokIVal;

	*ubp = ub - lb;
	*lbp = 0;
	break;
    }

    case OpBitConcat: {
	// Sum up all input widths, if available
	int portIx;

	ub = lb = 0;
	for (portIx = 1; portIx < arrayN (mgp->opInsts [nodeIx].ports); portIx++) {
	    int netIx = mgp->opInsts [nodeIx].ports [portIx].netIx;
	    if (mgp->nets [netIx].upperBound != INVALID) {
		ub += (mgp->nets [netIx].upperBound
		       - mgp->nets [netIx].lowerBound + 1);
	    }
	    else {
		ub = lb = INVALID;
		break;
	    }
	}
	// Output width is input width
	if (ub != INVALID) {
	    *ubp = ub - 1;
	    *lbp = 0;
	}
	else {
	    *ubp = ub;
	    *lbp = lb;
	}
	break;
    }

    case OpBitReplicate: {
	// Arg 1 (port [2]) is the value to replicate
	int netIx = mgp->opInsts [nodeIx].ports [2].netIx;
	if (mgp->nets [netIx].upperBound != INVALID) {
	    int    w, n;
	    Token *tokArg0p;

	    w = (mgp->nets [netIx].upperBound
		 - mgp->nets [netIx].lowerBound + 1);

	    // Arg 0 is the replication factor
	    tokArg0p = listIndex (tokp->args, 0);
	    assert (tokArg0p->tokType == TokInt);
	    n = tokArg0p->tokIVal;

	    *ubp = n * w - 1;
	    *lbp = 0;
	}
	else {
	    *ubp = *lbp = INVALID;
	}
	break;
    }

    case OpCondExpr: {
	int  portIx, netIx;

	// Must have 3 inputs
	assert (arrayN (mgp->opInsts [nodeIx].ports) == 4);

	// Arg 0 (port [1]) must have width 1
	netIx = mgp->opInsts [nodeIx].ports [1].netIx;
	if (mgp->nets [netIx].upperBound != INVALID) {
	    assert ((mgp->nets [netIx].upperBound
		     - mgp->nets [netIx].lowerBound + 1) == 1);
	}

	// Args 1 and 2 (ports [2] and [3]) should have same width
	ub = lb = INVALID;
	for (portIx = 2; portIx < 4; portIx++) {
	    netIx = mgp->opInsts [nodeIx].ports [portIx].netIx;
	    if (mgp->nets [netIx].upperBound != INVALID) {
		if (ub != INVALID) {
		    assert ((ub - lb + 1) ==
			    (mgp->nets [netIx].upperBound -
			     mgp->nets [netIx].lowerBound + 1));
		}
		else {
		    ub = mgp->nets [netIx].upperBound;
		    lb = mgp->nets [netIx].lowerBound;
		}

	    }
	}
	// Output width is same as input 1 or 2
	*ubp = ub;
	*lbp = lb;
	break;
    }

    case OpCase: {
	int  portIx;

	// Arg 0 (port [1]) is the discriminating expr; ignore it
	// Check all remaining inputs have same width
	ub = lb = INVALID;
	for (portIx = 2; portIx < arrayN (mgp->opInsts [nodeIx].ports); portIx++) {
	    int netIx = mgp->opInsts [nodeIx].ports [portIx].netIx;
	    if (mgp->nets [netIx].upperBound != INVALID) {
		if (ub != INVALID) {
		    assert ((ub - lb + 1) ==
			    (mgp->nets [netIx].upperBound -
			     mgp->nets [netIx].lowerBound + 1));
		}
		else {
		    ub = mgp->nets [netIx].upperBound;
		    lb = mgp->nets [netIx].lowerBound;
		}

	    }
	}
	// Output width is input width
	*ubp = ub;
	*lbp = lb;
	break;
    }
    }
} // computeOpOutputWidth()

// ----------------------------------------------------------------
// Given a reference to a module inst or an op inst (nodeIx),
// if it is not the end of the path,
// infer the width of the net at its output port (portIx)
// and recursively, via its output net, go to following module insts/op insts

static
void
inferWidths (ModuleGraph  *mgp,
	     WireSymbol   *wireSymbols,
	     NodeType      nodeType,
	     int           nodeIx,
	     int           portIx)
{
    int     j, netIx, pinIx, ub, lb;

    if (nodeType == MODULEINST) {
	// start of path
	if (mgp->moduleInsts[nodeIx].ports[portIx].portDir == PORT_OUTPUT) {
#if 0
	    {
		AST  *astp;
		astp = mgp->moduleInsts[nodeIx].node;
		printf ("DEBUG (inferWidths): start of path at %s port %d\n",
			astp->u.moduleInstAttrs.moduleInstName->tokSVal,
			portIx);
	    }
#endif
	    // set up the net for recursion
	    netIx = mgp->moduleInsts [nodeIx].ports [portIx].netIx;
	    assert (mgp->moduleInsts [nodeIx].ports [portIx].pinIx == 0);
	    // set up expected net width
	    ub = INVALID;
	    lb = INVALID;
	}

	// end of path; done
	else {
	    assert (mgp->moduleInsts[nodeIx].ports[portIx].portDir == PORT_INPUT);
#if 0
	    {
		AST    *astp;
		astp = mgp->moduleInsts[nodeIx].node;
		printf ("DEBUG (inferWidths): end of path at %s port %d\n",
			astp->u.moduleInstAttrs.moduleInstName->tokSVal,
			portIx);
	    }
#endif
	    return;
	}
    }
    // op inst
    else {
	assert (nodeType == OPINST);

#if 0
	{
	    Token *tokp;
	    tokp = mgp->opInsts[nodeIx].node;
	    printf ("DEBUG (inferWidths): path operator ");
	    fprintOp (stdout, tokp->op, 0);
	    printf (" port %d\n", portIx);
	}
#endif

	// set up the net to follow from the output (port 0) of the operator
	// and the widths of the output
	netIx = mgp->opInsts[nodeIx].ports[0].netIx;
	assert (mgp->opInsts[nodeIx].ports[0].pinIx == 0);

	computeOpOutputWidth (mgp, nodeIx, & ub, & lb);
    }

    // ---------------- Process the output net, and recursively traverse

    // If the net already has a width, it's already done; quit
    if (mgp->nets [netIx].upperBound != INVALID) {
	// if there is an expected width, check equality
	if (ub != INVALID) {
	    assert (ub == mgp->nets [netIx].upperBound);
	    assert (lb == mgp->nets [netIx].lowerBound);
	}
	return;
    }

    // If the net is associated with any symbols, unify their
    // widths with the expected width
    for (j = 0; j < arrayN (mgp->nets[netIx].wireNames); j++) {
	int  wsj  = findSymbol (wireSymbols, mgp->nets[netIx].wireNames[j]);
	AST *astp = wireSymbols[wsj].decl;
	if (ub == INVALID) {
	    if (astp->u.declAttrs.upperBound == NULL) {
		ub = 0;
		lb = 0;
	    }
	    else {
		ub = astp->u.declAttrs.upperBound->tokIVal;
		lb = astp->u.declAttrs.lowerBound->tokIVal;
	    }
	}
	else {
	    if (astp->u.declAttrs.upperBound == NULL) {
		assert (ub == 0);
		assert (lb == 0);
	    }
	    else {
		assert (ub == astp->u.declAttrs.upperBound->tokIVal);
		assert (lb == astp->u.declAttrs.lowerBound->tokIVal);
	    }
	}
    }

#if 0
    printf ("DEBUG (inferWidths): net %d width is [%d:%d]\n", netIx, ub, lb);
#endif

    // Set this net's width
    if (ub != INVALID) {
	mgp->nets [netIx].upperBound = ub;
	mgp->nets [netIx].lowerBound = lb;
    }

#if 0
    printf ("DEBUG (inferWidths): recursively following net %d\n", netIx);
#endif

    // recursively follow net's fanout pins
    for (pinIx = 1; pinIx < arrayN (mgp->nets[netIx].pins); pinIx++) {
	inferWidths (mgp,
		     wireSymbols,
		     mgp->nets[netIx].pins[pinIx].nodeType,
		     mgp->nets[netIx].pins[pinIx].nodeIx,
		     mgp->nets[netIx].pins[pinIx].portIx);
    }
} // inferWidths()

// ================================================================
// buildGraph()
// Builds the circuit graph for a module from its parse tree, and returns it

ModuleGraph *
buildGraph (AST *moduleASTp)
{
    AST           *astp, *assignp;
    Token         *tokp, *rhsp;
    List          *xs, *ys;
    ModuleGraph   *mgp;
    WireSymbol    *wireSymbols;
    int            j, netIx, m, p, pinIx;

    assert (moduleASTp->tag == TagModule);

    mgp = checked_malloc (sizeof (ModuleGraph), "buildGraph/module graph");
    mgp->parseTree = moduleASTp;

    // ----------------
    // Alloc mgp->moduleInsts and load them, including the [0]'th one for
    // this module's I/O port list

    loadModuleInsts (moduleASTp, mgp);

    // Alloc wireSymbol table and load it with all declared identifiers
    wireSymbols = createWireSymbolTable (moduleASTp);

    // Create initially empty arrays of opInsts and nets
    mgp->opInsts = allocArray (100, sizeof (Node), "buildGraph/opInsts");
    mgp->nets    = allocArray (100, sizeof (Net) , "buildGraph/nets");

    // Process all assigns and caseMuxes.
    for (xs = moduleASTp->u.moduleAttrs.stmts; xs != NULL; xs = xs->tail) {
	astp = xs->head;

	assert (astp != NULL);
	assert (astp->tag != TagModule);

	// Ignore all but assigns and casemuxes
	if ((astp->tag == TagDecl) ||
	    (astp->tag == TagModuleInst))
	    continue;

#if 0
	printf ("----------------------------------------------------------------\n");
	printf ("DEBUG (buildGraph): processing assign/casemux stmt\n");
	fprintAST (stdout, 4, astp);
#endif

	if (astp->tag == TagCaseMux) {

	    // ``Convert'' casemuxes to assignment statements.
	    // Make an assignment statement:    lhs = (OpCase eDiscr eRHS ... eRHS)
	    // where, in the OpCase TokExpr, the tokSVal field contains the original AST
	    // Then, process the assignment stmt as usual

	    rhsp = allocASTToken ("buildGraph/caseMux conversion rhs");
	    rhsp->tokLine = astp->u.caseMuxAttrs.lhs->tokLine;
	    rhsp->tokCol  = astp->u.caseMuxAttrs.lhs->tokCol;
	    rhsp->tokSVal = (char *) astp;
	    rhsp->tokType = TokExpr;
	    rhsp->op      = OpCase;
	    ys = cons (astp->u.caseMuxAttrs.expr,
		       astp->u.caseMuxAttrs.rhss,
		       "buildGraph/caseMux conversion cons");
	    rhsp->args    = ys;

	    assignp = checked_malloc (sizeof (AST), "buildGraph/caseMux conversion");
	    assignp->tag                          = TagAssignStmt;
	    assignp->u.assignStmtAttrs.lhs        = astp->u.caseMuxAttrs.lhs;
	    assignp->u.assignStmtAttrs.rhs        = rhsp;

	    astp = assignp;

#if 0
	    printf ("----------------------------------------------------------------\n");
	    printf ("DEBUG (buildGraph): casemux converted to assign:\n");
	    fprintAST (stdout, 4, astp);
#endif
	}

	assert (astp->tag == TagAssignStmt);

	j = findSymbol (wireSymbols, astp->u.assignStmtAttrs.lhs);
	netIx = processExpr (mgp,
			     wireSymbols,
			     wireSymbols[j].netIx,
			     astp->u.assignStmtAttrs.rhs);
	if (wireSymbols[j].netIx == INVALID) {
	    wireSymbols[j].netIx = netIx;
	    if (netIx != INVALID)
		addWireNameToNet (mgp, netIx, wireSymbols[j].name);
	}
	else {
	    assert (netIx == wireSymbols[j].netIx);
	}
    }

    // ---------------- Fill in info on all ports of all moduleInsts
    for (m = 0; m < arrayN (mgp->moduleInsts); m++) {
	// For all ports p
	astp = mgp->moduleInsts[m].node;
	assert (astp->tag = TagModuleInst);

#if 0
	printf ("DEBUG (buildGraph): filling in ports of module %s\n",
		astp->u.moduleInstAttrs.moduleName->tokSVal);
#endif

	for (p = 0; p < arrayN (mgp->moduleInsts[m].ports); p++) {
	    // Lookup port's net in wireSymbol table
	    tokp = listIndex (mgp->moduleInsts[m].ports[p].portName, 1);
	    if (tokp != NULL) {
		j    = findSymbol (wireSymbols, tokp);
		netIx = wireSymbols[j].netIx;
	    }
	    else {
		// No connection to this port; ignore it
		continue;
	    }

	    // CLK and RST_N will have netIx invalid on moduleInst 0 (I/O ports)
	    if (netIx == INVALID) {
		// assert (m == 0);
                if (m == 0) {
                    netIx = createNet (mgp);
                    wireSymbols[j].netIx = netIx;
                    addWireNameToNet (mgp, netIx, tokp);
                    
                    // Connect net.pin0 to the module.port
                    mgp->nets[netIx].pins[0].nodeType = MODULEINST;
                    mgp->nets[netIx].pins[0].nodeIx   = m;
                    mgp->nets[netIx].pins[0].portIx   = p;
                    // Connect module.port to net.pin0
                    mgp->moduleInsts[m].ports[p].portDir = PORT_OUTPUT;
                    mgp->moduleInsts[m].ports[p].netIx   = netIx;
                    mgp->moduleInsts[m].ports[p].pinIx   = 0;
                }
	    }

	    // If net's drive pin is undefined then this module port is an
	    // output port driving the net
	    else if ((mgp->moduleInsts[m].ports[p].portDir == PORT_OUTPUT) ||
		     ((mgp->moduleInsts[m].ports[p].portDir == PORT_UNKNOWN_DIR) &&
		      (mgp->nets[netIx].pins[0].nodeIx == INVALID))) {
		// Connect net.pin0 to the module.port
		mgp->nets[netIx].pins[0].nodeType = MODULEINST;
		mgp->nets[netIx].pins[0].nodeIx   = m;
		mgp->nets[netIx].pins[0].portIx   = p;
		// Connect module.port to net.pin0
		mgp->moduleInsts[m].ports[p].portDir = PORT_OUTPUT;
		mgp->moduleInsts[m].ports[p].netIx   = netIx;
		mgp->moduleInsts[m].ports[p].pinIx   = 0;
	    }

	    // else this module port is an input port driven by the net
	    else {
		// Create a new pin for the net
		pinIx = createPin (mgp, netIx);

		// Connect net.pin0 to the module.port
		mgp->nets[netIx].pins[pinIx].nodeType = MODULEINST;
		mgp->nets[netIx].pins[pinIx].nodeIx   = m;
		mgp->nets[netIx].pins[pinIx].portIx   = p;
		// Connect module.port to net.pin0
		mgp->moduleInsts[m].ports[p].portDir = PORT_INPUT;
		mgp->moduleInsts[m].ports[p].netIx   = netIx;
		mgp->moduleInsts[m].ports[p].pinIx   = pinIx;
	    }
	}
    }

    // ---------------- Infer widths of all nets
    for (j = 0; j < arrayN (mgp->moduleInsts); j++) {
	for (p = 0; p < arrayN (mgp->moduleInsts[j].ports); p++) {
	    if (mgp->moduleInsts[j].ports[p].portDir == PORT_OUTPUT)
		// infer widths forward from this output port
		inferWidths (mgp, wireSymbols, MODULEINST, j, p);
	}
    }
    
    return mgp;
} // buildGraph()

// ****************************************************************
// Printing graphs

// ----------------
// fprintPortName()
// Argument fa is a 2-list: [formal::Token, actual::Tokan]

void
fprintPortName (FILE *fpo, List *fa)
{
    Token *formalp, *actualp;

    formalp = fa->head;
    if (formalp != NULL)
	fprintf (fpo, ".%s(", formalp->tokSVal);

    actualp = fa->tail->head;
    if (actualp != NULL)
	fprintf (fpo, "%s", actualp->tokSVal);

    if (formalp != NULL)
	fprintf (fpo, ")");
} // fprintPortName

// ----------------
// fprintPortDir()

void
fprintPortDir (FILE *fpo, PortDir pt)
{
    switch (pt) {
    case PORT_UNKNOWN_DIR: fprintf (fpo, "unknown_dir"); break;
    case PORT_INPUT      : fprintf (fpo, "input");   break;
    case PORT_OUTPUT     : fprintf (fpo, "output");  break;
    }
} // fprintPortDir

// ----------------------------------------------------------------
// Print the nets in a graph

static
void
fprintNets (FILE *fpo, ModuleGraph *mgp)
{
    int     j, p, n;
    AST    *astp;
    Token  *tokp;
    List   *xs;

    fprintf (fpo, "There are %d nets.\n", arrayN (mgp->nets));
    for (j = 0; j < arrayN (mgp->nets); j++) {

	fprintf (fpo, "  Net %d (fanout %d)", j, arrayN (mgp->nets[j].pins) - 1);

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
		    astp = mgp->moduleInsts[mgp->nets[j].pins[p].nodeIx].node;
		    assert (astp->tag == TagModuleInst);
		    xs   = listIndex (astp->u.moduleInstAttrs.ports, mgp->nets[j].pins[p].portIx);
		    tokp = xs->head;
		    fprintf (fpo, "%s #(...) %s(... .%s() ...)",
			     astp->u.moduleInstAttrs.moduleName->tokSVal,
			     astp->u.moduleInstAttrs.moduleInstName->tokSVal,
			     tokp->tokSVal);
		}
		else {
		    tokp = mgp->opInsts[mgp->nets[j].pins[p].nodeIx].node;
		    assert (tokp->tokType == TokExpr);
		    fprintOp (fpo, tokp->op, 5);
		    fprintf (fpo, " [%d]", mgp->nets[j].pins[p].portIx);
		    fprintf (fpo, " (%d.%d)", tokp->tokLine, tokp->tokCol);
		}
	    }
	    fprintf (fpo, "\n");
	}
    }
} // fprintNets()

// ----------------------------------------------------------------
// Print graph (used for debugging only)

void
fprintGraph (FILE *fpo, ModuleGraph *mgp)
{
    int     j, p;
    AST    *astp;
    Token  *tokp;

    fprintf (fpo, "Module instances\n");
    for (j = 0; j < arrayN (mgp->moduleInsts); j++) {
	astp = mgp->moduleInsts[j].node;
	fprintf (fpo, "  instance %s of module %s\n",
		 astp->u.moduleInstAttrs.moduleInstName->tokSVal,
		 astp->u.moduleInstAttrs.moduleName->tokSVal);

	for (p = 0; p < arrayN (mgp->moduleInsts[j].ports); p++) {
	    fprintf (fpo, "    ");
	    fprintPortName (fpo, mgp->moduleInsts[j].ports[p].portName);
	    fprintf (fpo, " ");
	    fprintPortDir (fpo, mgp->moduleInsts[j].ports[p].portDir);
	    fprintf (fpo, " ");
	    if (mgp->moduleInsts[j].ports[p].netIx == INVALID)
		fprintf (fpo, "Unconnected\n");
	    else
		fprintf (fpo, "Net %d\n", mgp->moduleInsts[j].ports[p].netIx);
	}
    }

    fprintf (fpo, "Op instances\n");
    for (j = 0; j < arrayN (mgp->opInsts); j++) {
	tokp = mgp->opInsts[j].node;
	fprintf (fpo, "  op ");
	fprintOp (fpo, tokp->op, 5);
	fprintf (fpo, " (%d.%d)\n", tokp->tokLine, tokp->tokCol);

	fprintf (fpo, "   ");
	for (p = 0; p < arrayN (mgp->opInsts[j].ports); p++) {
	    fprintf (fpo, " %d:[", p);
	    // fprintPortDir (fpo, mgp->opInsts[j].ports[p].portDir);
	    // fprintf (fpo, ",");
	    if (mgp->opInsts[j].ports[p].netIx == INVALID)
		fprintf (fpo, "nc");
	    else
		fprintf (fpo, "%d.%d",
			 mgp->opInsts[j].ports[p].netIx,
			 mgp->opInsts[j].ports[p].pinIx);
	    fprintf (fpo, "]");
	}
	fprintf (fpo, "\n");
    }

    fprintf (fpo, "Nets\n");
    fprintNets (fpo, mgp);
} // fprintGraph()

// ****************************************************************
