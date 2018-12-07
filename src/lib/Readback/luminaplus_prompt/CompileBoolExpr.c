// Copyright (c) 2015 Bluespec, Inc.    All Rights Reserved.
// Author: Rishiyur Nikhil

// ================================================================

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

// ================================================================
// There is only one exported function from this file:

extern int compileBoolExpr (char *buf, uint16_t *lutp);

// This is a component for host-side testbenches that use Bluespec's
// FPGA readback capabilities.

// Input 'buf' should contains a standard C null-terminated string of
// no more than 1023 characters. The string is an expression (grammar
// below) and represents an arbitrary boolean function of four boolean
// variables A, B, C and D.
//
// The int function result is 0 if there were no syntax errors,
// and 1 otherwise.

// If there are no syntax errors, then output '*lutp' is a 16-bit
// value representing the output of the function for every possible
// combination of the four boolean variables.  Reading (D,C,B,A) as a
// 4-bit value J, the function output is bit J *lutp.
//
// The string can be any expression in the following grammar:
//
//    E ::= E binary_op E
//       |  unary_op E
//       |  '(' E ')'
//       |  variable
//
// Operators, from highest precedence to lowest, are:
//    !, ~         unary not
//    ==, !=       binary equality, inequality
//    &&, &        and
//    ^            xor
//    ||, |        or
//
// Variables are the letters A, B, C and D (upper or lower case).
//
// Note: All the data in these expressions are just 1-bit values.
// There's no semantic difference between ! and ~, && and &, || and |,
//    ^ and !=
//    the redundancy is just for convenience.

// ================================================================

#define LINEBUFSIZE 1024

// ================================================================
// Lexer

typedef enum { False, True } Bool;

typedef enum { EOS,
	       LETTER,
	       LPAREN, RPAREN,
	       OR,
	       XOR,
	       AND,
	       EQ, NOTEQ,
	       NOT,
	       BOGUS } Token;

typedef struct {
    char *buf;
    int   cursor;
    int   tokenValid;
    Token token;
    char  tokenValue;
} LexerState;

static
void printToken (FILE *fp, char *pre, Token token, char tokenValue, char *post)
{
    fprintf (fp, "%s", pre);
    switch (token) {
      case EOS:    fprintf (fp, "EOS"); break;
      case LETTER: fprintf (fp, "LETTER:%c", tokenValue); break;
      case LPAREN: fprintf (fp, "LPAREN"); break;
      case RPAREN: fprintf (fp, "RPAREN"); break;
      case EQ:     fprintf (fp, "EQ"); break;
      case NOTEQ:  fprintf (fp, "NOTEQ"); break;
      case NOT:    fprintf (fp, "NOT"); break;
      case AND:    fprintf (fp, "AND"); break;
      case OR:     fprintf (fp, "OR"); break;
      case XOR:    fprintf (fp, "XOR"); break;
      case BOGUS:  fprintf (fp, "BOGUS"); break;
      default:     fprintf (fp, "<?Unknown token type? %0d>", token); break;
    }
    fprintf (fp, "%s", post);
}

static
void printLexerState (FILE *fp, char *pre, LexerState *statep, char *post)
{
    fprintf (fp, "%sLexerState{cursor %0d,", pre, statep->cursor);
    if (! statep->tokenValid)
	fprintf (fp, "LexerState: Invalid_Token");
    else
	printToken (fp, "", statep->token, statep->tokenValue, "");
    fprintf (fp, "}%s", post);
}

static
void skipWhiteSpace (LexerState *statep)
{
    while (isspace (statep->buf [statep->cursor]))
	statep->cursor = statep->cursor + 1;
}

static
void getNextToken (LexerState *statep)
{
    skipWhiteSpace (statep);
    statep->tokenValid = True;
    if ((statep->buf [statep->cursor] == 0) || (statep->buf [statep->cursor] == '\n')) {
	statep->token = EOS;
    }
    else if ((statep->buf [statep->cursor] == 'A') ||
	(statep->buf [statep->cursor] == 'B') ||
	(statep->buf [statep->cursor] == 'C') ||
	(statep->buf [statep->cursor] == 'D')) {
	statep->token      = LETTER;
	statep->tokenValue = statep->buf [statep->cursor];
	statep->cursor     = statep->cursor + 1;
    }
    else if ((statep->buf [statep->cursor] == 'a') ||
	(statep->buf [statep->cursor] == 'b') ||
	(statep->buf [statep->cursor] == 'c') ||
	(statep->buf [statep->cursor] == 'd')) {
	statep->tokenValid = True;
	statep->token      = LETTER;
	statep->tokenValue = 'A' + (statep->buf [statep->cursor] - 'a');
	statep->cursor     = statep->cursor + 1;
    }
    else if (statep->buf [statep->cursor] == '(') {
	statep->tokenValid = True;
	statep->token  = LPAREN;
	statep->cursor = statep->cursor + 1;
    }
    else if (statep->buf [statep->cursor] == ')') {
	statep->token  = RPAREN;
	statep->cursor = statep->cursor + 1;
    }
    else if (statep->buf [statep->cursor] == '=') {
	if (statep->buf [statep->cursor + 1] == '=')
	    statep->cursor = statep->cursor + 1;
	statep->token  = EQ;
	statep->cursor = statep->cursor + 1;
    }
    else if (statep->buf [statep->cursor] == '!') {
	if (statep->buf [statep->cursor + 1] == '=') {
	    statep->token  = NOTEQ;
	    statep->cursor = statep->cursor + 2;
	}
	else {
	    statep->token  = NOT;
	    statep->cursor = statep->cursor + 1;
	}
    }
    else if (statep->buf [statep->cursor] == '~') {
	statep->token  = NOT;
	statep->cursor = statep->cursor + 1;
    }
    else if (statep->buf [statep->cursor] == '&') {
	if (statep->buf [statep->cursor + 1] == '&')
	    statep->cursor = statep->cursor + 1;
	statep->token  = AND;
	statep->cursor = statep->cursor + 1;
    }
    else if (statep->buf [statep->cursor] == '|') {
	if (statep->buf [statep->cursor + 1] == '|')
	    statep->cursor = statep->cursor + 1;
	statep->token  = OR;
	statep->cursor = statep->cursor + 1;
    }
    else if (statep->buf [statep->cursor] == '^') {
	statep->token  = XOR;
	statep->cursor = statep->cursor + 1;
    }
    else {
	statep->token = BOGUS;
    }
}

// ================================================================
// Parser
// Parses the expression and simultaneously emits a reverse-polish bytecode
// for evaluation the expression.

// The parse stack holds continuations within parseBoolExrp()

static uint32_t parseStack [LINEBUFSIZE];
static int      parseToS;

static
void printParseStack (FILE *fp, char *label)
{
    int j;
    fprintf (fp, "%s", label);
    if (parseToS == 0)
	fprintf (fp, "  ParseStack is empty\n");
    else {
	fprintf (fp, "  ParseStack [0..%0d]: ", parseToS-1);
	for (j = 0; j < parseToS; j++)
	    fprintf (fp, "{%0d}", parseStack [j]);
	fprintf (fp, "\n");
    }
}

static
void pushParseStack (uint32_t x)
{
    if (parseToS == LINEBUFSIZE) {
	fprintf (stderr, "ERROR: parseStack overflow; expression too deep\n");
	exit (1);
    }
    parseStack [parseToS] = x;
    parseToS++;
}

static char bytecode [LINEBUFSIZE];
static int  pcmax;

static
void printBytecode (FILE *fp)
{
    int j;
    fprintf (fp, "Bytecode:");
    for (j = 0; j < pcmax;  j++) {
	printToken (fp, " ", bytecode [j], bytecode [j+1], "");
	if (bytecode [j] == LETTER) j++;
    }
    fprintf (fp, "\n");
}

static
void emit (Token t, int tokenVal)
{
    bytecode [pcmax] = t;
    pcmax++;
    if (t == LETTER) {
	bytecode [pcmax] = tokenVal;
	pcmax++;
    }
    // printToken (stdout, "    EMIT: ", t, tokenVal, "\n");
}

// Continuations within parseBoolExpr,
// corresponding to the 'dot's in the grammar shown below

typedef enum { c_return,
	       c0_a, c0_b,    // e0 ::= e1 . || e0 .
	       c1_a, c1_b,    // e1 ::= e2 .  ^ e1 .
	       c2_a, c2_b,    // e2 ::= e3 . && e2 .
	       c3_a, c3_b,    // e3 ::= e4 . {==, !=} e3 .
	       c4_a,          // e4 ::= ! e5 .
	                      //     |  e5
	       c5_a           // e5 ::= A/B/C/D
	                      //     |   ( e0 . )
} Continuation;

// ----------------
// The main parse function.
// Returns 0 if successful (no syntax errors); bytecode is valid
// Returns 1 if there are any syntax errors

static
int parseBoolExpr (char *buf)
{
    LexerState state, *statep;
    state.cursor = 0;
    state.tokenValid = False;
    state.buf = buf;
    statep = & state;

    parseToS = 0;
    pcmax = 0;
    pushParseStack (c_return);
    goto e0;

    // ----------------
  e0:
    pushParseStack (c0_a);
    goto e1;

  e0_a:
    if (! statep->tokenValid) getNextToken (& state);
    // printLexerState (stdout, "e0_a:", statep, "\n");
    if (statep->token == OR) {
	statep->tokenValid = False;
	pushParseStack (c0_b);
	goto e0;
    }
    else
	goto goto_continuation;

  e0_b:
    emit (OR, 0);
    goto goto_continuation;

    // ----------------
  e1:
    pushParseStack (c1_a);
    goto e2;

  e1_a:
    if (! statep->tokenValid) getNextToken (& state);
    // printLexerState (stdout, "e1_a:", statep, "\n");
    if (statep->token == XOR) {
	statep->tokenValid = False;
	pushParseStack (c1_b);
	goto e1;
    }
    else
	goto goto_continuation;

  e1_b:
    emit (XOR, 0);
    goto goto_continuation;

    // ----------------
  e2:
    pushParseStack (c2_a);
    goto e3;

  e2_a:
    if (! statep->tokenValid) getNextToken (& state);
    // printLexerState (stdout, "e2_a:", statep, "\n");
    if (statep->token == AND) {
	statep->tokenValid = False;
	pushParseStack (c2_b);
	goto e2;
    }
    else
	goto goto_continuation;

  e2_b:
    emit (AND, 0);
    goto goto_continuation;

    // ----------------
  e3:
    pushParseStack (c3_a);
    goto e4;

  e3_a:
    if (! statep->tokenValid) getNextToken (& state);
    // printLexerState (stdout, "e3_a:", statep, "\n");
    if ((statep->token == EQ) || (statep->token == NOTEQ)) {
	statep->tokenValid = False;
	pushParseStack (statep->token);
	pushParseStack (c3_b);
	goto e3;
    }
    else
	goto goto_continuation;

  e3_b:
    if (parseStack [parseToS] == EQ)
	emit (EQ, 0);
    else
	emit (NOTEQ, 0);
    parseToS--;
    goto goto_continuation;

    // ----------------
  e4:
    if (! statep->tokenValid) getNextToken (& state);
    // printLexerState (stdout, "e4_a:", statep, "\n");
    if (statep->token == NOT) {
	statep->tokenValid = False;
	pushParseStack (c4_a);
	goto e4;
    }
    else
	goto e5;

  e4_a:
    emit (NOT, 0);
    goto goto_continuation;

    // ----------------
  e5:
    if (! statep->tokenValid) getNextToken (& state);
    // printLexerState (stdout, "e5:", statep, "\n");
    if (statep->token == LETTER) {
	statep->tokenValid = False;
	emit (statep->token, statep->tokenValue);
	goto goto_continuation;
    }
    else if (statep->token == LPAREN) {
	statep->tokenValid = False;
	pushParseStack (c5_a);
	goto e0;
    }

  e5_a:
    if (! statep->tokenValid) getNextToken (& state);
    // printLexerState (stdout, "e5_a:", statep, "\n");
    if (statep->token == RPAREN) {
	statep->tokenValid = False;
	goto goto_continuation;
    }
    else {
	fprintf (stderr, "SYNTAX ERROR: Expecting a right-paren\n");
	return 1;
    }

    // ----------------
  goto_continuation:
    // printParseStack (stdout, "Parse stack at 'goto_continuation'\n");
    if (parseToS == 0) {
	fprintf (stderr, "INTERNAL ERROR: parse stack underflow\n");
	return 1;
    }
    parseToS--;
    switch (parseStack [parseToS]) {
      case c_return: goto final_return;
      case c0_a: goto e0_a;
      case c0_b: goto e0_b;
      case c1_a: goto e1_a;
      case c1_b: goto e1_b;
      case c2_a: goto e2_a;
      case c2_b: goto e2_b;
      case c3_a: goto e3_a;
      case c3_b: goto e3_b;
      case c4_a: goto e4_a;
      case c5_a: goto e5_a;
      default: {
	  fprintf (stderr, "INTERNAL ERROR: goto_continuation: unknown %0d\n", parseStack [parseToS]);
	  return 1;
      }
    }

    // ----------------
  final_return:
    if (! statep->tokenValid) getNextToken (& state);
    // printLexerState (stdout, "return:", statep, "\n");
    if (statep->token == EOS)
	return 0;
    else {
	fprintf (stderr, "ERROR: junk after expression\n");
	return 1;
    }
}

// ================================================================
// Caculating the LUT
// This part runs (interprets) the bytecode for all 16 combinations of (d,c,b,a)
// and returns a 16-bit vector of the results.

static Bool evalStack [LINEBUFSIZE];
static int  evalToS;

static
void printEvalStack (FILE *fp)
{
    int j;
    if (evalToS == 0)
	fprintf (fp, "EvalStack is empty\n");
    else {
	fprintf (fp, "EvalStack [0..%0d]: ", evalToS-1);
	for (j = 0; j < evalToS; j++)
	    fprintf (fp, " %0d", evalStack [j]);
	fprintf (fp, "\n");
    }
}

static
void pushEvalStack (char c, Bool b)
{
    if (evalToS == LINEBUFSIZE) {
	fprintf (stderr, "ERROR: evalStack overflow; expression too deep\n");
	exit (1);
    }
    // fprintf (stdout, "evalStack [%0d ++] = (%c, %0d)", evalToS, c, b);
    evalStack [evalToS] = b;
    evalToS++;
}

// Runs the bytecode for a particular choice of (d,c,b,a)
// and returns the result

static
Bool evalExpr (int d, int c, int b, int a)
{
    int pc;
    evalToS = 0;
    for (pc = 0; pc < pcmax;) {
	// printEvalStack (stdout);

	if (bytecode [pc] == LETTER) {
	    switch (bytecode [pc+1]) {
	      case 'A': pushEvalStack ('A', a); break;
	      case 'B': pushEvalStack ('B', b); break;
	      case 'C': pushEvalStack ('C', c); break;
	      case 'D': pushEvalStack ('D', d); break;
	    }
	    pc += 2;
	}
	else if (bytecode [pc] == NOT) {
	    assert (evalToS > 0);
	    evalStack [evalToS-1] = ! (evalStack [evalToS-1]);
	    pc++;
	}
	else if (bytecode [pc] == EQ) {
	    assert (evalToS > 1);
	    evalStack [evalToS-2] = (evalStack [evalToS-1] == evalStack [evalToS-2]);
	    evalToS--;
	    pc++;
	}
	else if (bytecode [pc] == NOTEQ) {
	    assert (evalToS > 1);
	    evalStack [evalToS-2] = (evalStack [evalToS-1] != evalStack [evalToS-2]);
	    evalToS--;
	    pc++;
	}
	else if (bytecode [pc] == AND) {
	    assert (evalToS > 1);
	    evalStack [evalToS-2] = (evalStack [evalToS-1] && evalStack [evalToS-2]);
	    evalToS--;
	    pc++;
	}
	else if (bytecode [pc] == OR) {
	    assert (evalToS > 1);
	    evalStack [evalToS-2] = (evalStack [evalToS-1] || evalStack [evalToS-2]);
	    evalToS--;
	    pc++;
	}
	else if (bytecode [pc] == XOR) {
	    assert (evalToS > 1);
	    evalStack [evalToS-2] = (evalStack [evalToS-1] ^ evalStack [evalToS-2]);
	    evalToS--;
	    pc++;
	}
    }
    assert (evalToS == 1);
    return evalStack [0];
}

// Runs the bytecode for all 16 combinations of (d,c,b,a) and returns
// and returns a 16-bit value representing the results.

static
uint16_t calculate_lut (void)
{
    int a, b, c, d;
    uint16_t lut = 0;
    int j;

    for (j = 15; j >= 0; j--) {
	d = (j >> 3) & 0x1;
	c = (j >> 2) & 0x1;
	b = (j >> 1) & 0x1;
	a = (j >> 0) & 0x1;
	int v = evalExpr (d, c, b, a);
	lut = ((lut << 1) | v);
    }
    return lut;
}

// ================================================================
// This is the only function exported from this file externally.
// (See comments at top of this file.)

int compileBoolExpr (char *buf, uint16_t *lutp)
{
    if (parseBoolExpr (buf) != 0) return 1;

    // printBytecode (stdout);

    *lutp = calculate_lut ();
    return 0;
}

// ================================================================
// Standalone test.
// Prompts for an expression, reads in the expression string,
// runs compileBoolExpr() on it and prints the final LUT

#ifdef TEST

int main (int argc, char *argv[])
{
    uint16_t lut;
    char linebuf [LINEBUFSIZE], *linebufp;
    int j;

    linebufp = & (linebuf [0]);
    while (True) {
	fprintf (stdout, "test> ");
	linebufp = fgets (& (linebuf [0]), LINEBUFSIZE-1, stdin);
	if (linebufp == NULL) break;
	linebuf [LINEBUFSIZE-1] = 0;    // force a null-termination

	if (compileBoolExpr (linebufp, & lut) == 0) {
	    // Print the bit vector by itself
	    fprintf (stdout, "LUT = ");
	    for (j = 15; j >= 0; j--) {
		fprintf (stdout, "%0d", (lut >> j) & 0x1);
		if ((j != 0) && (j % 4 == 0))
		    fprintf (stdout, "_");
	    }
	    fprintf (stdout, "\n");

	    // Print the whole truth table
	    fprintf (stdout, "    (d,c,b,a) => v\n");
	    for (j = 0; j < 16; j++)
		fprintf (stdout, "    (%0d,%0d,%0d,%0d) => %0d\n",
			 (j >> 3) & 0x1,
			 (j >> 2) & 0x1,
			 (j >> 1) & 0x1,
			 (j >> 0) & 0x1,
			 (lut >> j) & 0x1);
	}
    }
}

#endif
