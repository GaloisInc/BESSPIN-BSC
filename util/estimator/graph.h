/* -*-  Mode:C; c-basic-offset:4 -*- */

// ----------------------------------------------------------------
// Invalid indexes, port directions, node types, ...

#define INVALID  -1

// ----------------------------------------------------------------
// Nodes (module instances and operator instances)

// We'll have two arrays of Nodes
// One array of module instances
// One array of operator instances

typedef enum { MODULEINST, OPINST } NodeType;

typedef enum { PORT_UNKNOWN_DIR, PORT_INPUT, PORT_OUTPUT } PortDir;

typedef struct {
    List       *portName;    // from original parse tree: list2 (Token *formal, Token *actual)
    PortDir     portDir;
    int         netIx;       // index of net to which it is connected (init PORT_UNCONNECTED)
    int         pinIx;       // index of pin in net to which it is connected
} Port;

typedef struct {
    void     *node;      // *AST (module instance) or *Token (operator instance)
    Port     *ports;     // array of Ports
} Node;

// ----------------------------------------------------------------
// Edges (connections)

// ----------------
// A Pin is one connection of a Net
// and connects to one port of a moduleInst or opInst

typedef struct {
    NodeType  nodeType;
    int       nodeIx;    // index of node (moduleInst or opInst) to which it is connected
    int       portIx;    // index of port of node to which it is connected
} Pin;

// ----------------
// A Net is a collection of Pins (1 input, >= 0 outputs)
// and a collection of >= 0 wire names

typedef struct {
    Pin     *pins;           // [0] is source; rest are sinks
    Token  **wireNames;      // Array of pointers to TokWords
    int      upperBound;     // u in [u:l]
    int      lowerBound;     // l in [u:l]
} Net;

// ----------------------------------------------------------------
// A ModuleGraph represents a module

// Note: by convention, moduleInsts[0] represents the ports of this module
//       i.e., moduleInsts[0].outputs are the inputs of this module, and
//       i.e., moduleInsts[0].inputs are the outputs of this module

typedef struct {
    AST   *parseTree;     // The original parse tree
    Node  *moduleInsts;   // Sub-modules instantiated inside this module
    Node  *opInsts;       // Operator instances
    Net   *nets;          // Wires
} ModuleGraph;


/* ****************************************************************
 * buildGraph()
 * Builds the circuit graph from the parse tree
 */

extern
ModuleGraph *
buildGraph (AST *moduleASTp);

/* ****************************************************************
 * Printing graphs
 */

// ----------------
// fprintPortName()
// Argument fa is a 2-list: [formal::Token, actual::Tokan]

extern
void
fprintPortName (FILE *fpo, List *fa);

// ----------------
// fprintPortDir()

extern
void
fprintPortDir (FILE *fpo, PortDir pt);

// ----------------------------------------------------------------
// Print graph (used for debugging only)

extern
void
fprintGraph (FILE *fpo, ModuleGraph *mgp);

// ****************************************************************
