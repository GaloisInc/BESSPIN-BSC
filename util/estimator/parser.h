/* -*-  Mode:C; c-basic-offset:4 -*- */

// ****************************************************************
// Lexer

typedef enum TokType {
    TokWord,
    TokDescription,
    TokInt,

    TokLBracket,
    TokRBracket,
    TokLParen,
    TokRParen,
    TokLBrace,
    TokRBrace,
    TokColon,
    TokSemicolon,
    TokPeriod,
    TokComma,
    TokHash,
    TokAt,
    TokQuestionMark,
    TokEqual,
    TokEQ,
    TokNE,
    TokLT,
    TokLTLT,
    TokLE,
    TokGE,
    TokGT,
    TokGTGT,
    TokPlus,
    TokMinus,
    TokTimes,
    TokPercent,
    TokSlash,
    TokAmp,
    TokAmpAmp,
    TokCaret,
    TokVBar,
    TokVBarVBar,
    TokTilde,
    TokBang,
    TokEOF,
    TokExpr,
    TokErr
} TokType;

#define  MAX_STRING_LEN      0x1000

/* ----------------------------------------------------------------
 * testLexer()
 * Just reads the tokens from a file and prints them out.
 */

extern
void
testLexer (char *filename);

/* ****************************************************************
 * Lists
 */

typedef struct List {
    void        *head;
    struct List *tail;
} List;

/* ----------------
 * cons()
 */

extern
List *
cons (void *h, List *t, char *callerName);

/* ----------------
 * list2()
 */

extern
List *
list1 (void *x1, char *callerName);

/* ----------------
 * list2()
 */

extern
List *
list2 (void *x1, void *x2, char *callerName);

/* ----------------
 * list3()
 */

extern
List *
list3 (void *x1, void *x2, void *x3, char *callerName);

/* ----------------
 * listLength()
 */

extern
int
listLength (List *lp);

/* ----------------
 * listIndex()
 * Returns n'th element of list (0-indexed)
 */

extern
void *
listIndex (List *lp, int n);

/* ----------------
 * listLast()
 * Returns last cons of a non-empty list
 */

extern
List *
listLast (List *lp);

/* ----------------
 * listJoin()
 * With side effect: modifies l1's last tail, if l1 is not null.
 * Does not cons.
 * Returns ptr to joined list.
 */

extern
List *
listJoin (List *lp1, List *lp2);

/* ----------------
 * listChopLast()
 * Drops last cons from a non-empty list.
 * Does not cons.
 * Returns head of chopped list.
 */

extern
List *
listChopLast (List *lp);

/* ----------------
 * listReverseInSitu()
 * Reverses a list, in place (i.e., no consing)
 */

extern
List *
listReverseInSitu (List *lp);

// ****************************************************************
// Parser

typedef enum Op {
    OpLogicalOr,
    OpLogicalAnd,

    OpBitOr,
    OpBitExOr,
    OpBitAnd,

    OpEQ,
    OpNE,

    OpLT,
    OpLE,
    OpGE,
    OpGT,

    OpLShift,
    OpRShift,

    OpPlus,
    OpMinus,

    OpTimes,
    OpIntDiv,
    OpMod,

    OpLogicalNot,
    OpBitNot,

    OpBitSelect1,
    OpBitSelect2,
    OpBitConcat,
    OpBitReplicate,
    OpCondExpr,
    OpConst,
    OpCase
} Op;

#define LAST_OPCODE OpCase

typedef struct Token {
    int            tokLine;
    int            tokCol;
    TokType        tokType;
    char          *tokSVal;
    INT64_t        tokIVal;
    int            tokIWidth;
    int            tokIBase;
    Bool           tokIVerilogFormat;

    // TokExprs have an op and a list of args
    Op             op;
    List          *args;        // List of Tokens
} Token;

typedef enum ASTTag {
    TagModule,
    TagDecl,
    TagModuleInst,
    TagAssignStmt,
    TagCaseMux
} ASTTag;

typedef struct AST {
    ASTTag   tag;
    union {
	struct {
	    Token    *moduleName;        // TokWord
	    List     *ports;             // List of TokWords
	    List     *stmts;             // List of ASTs
	} moduleAttrs;

	struct {
            Token    *declKw;            // TokWord: "input", "output", "wire", "reg"
	    Token    *upperBound;        // TokInt         u in [u:l]
	    Token    *lowerBound;        // TokInt         l in [u:l]
	    List     *ides;              // List of TokWords
	} declAttrs;

	struct {
            Token    *moduleName;        // TokWord
	    List     *params;            // List of (TokWord, TokInt)
            Token    *moduleInstName;    // TokWord
	    List     *ports;             // List of (TokWord,TokWord)
	} moduleInstAttrs;

	struct {
	    Token    *lhs;               // TokWord
	    Token    *rhs;               // TokExpr
	} assignStmtAttrs;

	struct {
	    List     *sensitivities;     // List of TokWord
	    Token    *expr;              // TokWord, discriminating expr
	    Token    *lhs;               // TokWord, the assigned var from any clause
	                                 //   Note: all clauses assign the same var
	    List     *matchess;          // List of (List of TokInt), 1 list per clause
	                                 //   For each clause, (List of TokInt) is
	                                 //   - list of integers for matching that clause,
                                         //   - NULL for 'default'
	    List     *rhss;              // List of TokExpr, 1 rhs expr per clause
	} caseMuxAttrs;
    } u;
} AST;

/* ----------------------------------------------------------------
 * allocASTToken()
 * Allocates a dynamic token.
 * Client will set its type and contents
 */

extern
Token *
allocASTToken (char *clientName);

/* ----------------------------------------------------------------
 * fprintTokenLocation()
 * Prints the location of a dynamic token
 */

extern
void
fprintTokenLocation (FILE *fp, Token  *tokp);

/* ----------------------------------------------------------------
 * fprintOp()
 * Prints an expression opcode
 */

extern
void
fprintOp (FILE *fp, Op  op, int formatWidth);

/* ----------------------------------------------------------------
 * fprintToken()
 * Prints a dynamic token
 */

extern
void
fprintToken (FILE *fp, Token  *tokp, int verbosity);

/* ----------------------------------------------------------------
 * parseFile()
 * This is the main external function called to parse a trex2 file.
 */

extern
AST *
parseFile (char *filename);

/* ----------------------------------------------------------------
 * fprintAST()
 * For debugging: prints out an AST
 */

extern
void
fprintAST (FILE *fp, int indent, AST *astp);

/* ----------------------------------------------------------------
 * testParser()
 * For debugging: just parses the file and prints out the AST
 */

extern
void
testParser (char *filename);

/* **************************************************************** */
