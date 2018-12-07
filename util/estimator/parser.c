/* -*-  Mode:C; c-basic-offset:4 -*- */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>

#include "utils.h"
#include "parser.h"

/* ****************************************************************
 * Forward declarations etc.
 */

static
void
lexAnItem (void);

static
Token *
parseExprRec (int precedence);

#define MIN_PRECEDENCE   1
#define MAX_PRECEDENCE  12

/* ****************************************************************
 * Lexer
 */

#define MAX_INPUT_LINE_LEN        4096

static FILE    *fp;
static char     filename[MAX_INPUT_LINE_LEN];
static int      lineNum;
static int      colNum;

static char     inputLine [MAX_INPUT_LINE_LEN];
static int      inputLineCursor, inputChar;
static TokType  tokType;
static char     tokSVal [MAX_STRING_LEN];
static int      tokSValLen;
static INT64_t  tokIVal;
static int      tokIWidth, tokIBase, tokLine, tokCol;
static Bool     tokIVerilogFormat;

/* ----------------------------------------------------------------
 * fprintCurTok()
 */

static
void
fprintCurTok (FILE *fp)
{
    switch (tokType) {
    case TokWord: {
	tokSVal [MIN (tokSValLen, MAX_STRING_LEN - 1)] = 0;
	fprintf (fp, "'%s\n", tokSVal);
	break;
    }
    case TokDescription: {
	tokSVal [MIN (tokSValLen, MAX_STRING_LEN - 1)] = 0;
	fprintf (fp, "\"%s\"\n", tokSVal);
	break;
    }
    case TokInt: {
	tokSVal [MIN (tokSValLen, MAX_STRING_LEN - 1)] = 0;
	fprintf (fp,
		 "'%s' (TokInt width %d, base %d, val %" LL"d, 0x%" LL "x, Verilog format %c)\n",
		 tokSVal, tokIWidth, tokIBase, tokIVal, tokIVal,
		 (tokIVerilogFormat ? 'Y' : 'N'));
	break;
    }
    case TokLBracket:     { fprintf (fp, "'['\n");       break; }
    case TokRBracket:     { fprintf (fp, "']'\n");       break; }
    case TokLParen:       { fprintf (fp, "'('\n");       break; }
    case TokRParen:       { fprintf (fp, "')'\n");       break; }
    case TokLBrace:       { fprintf (fp, "'{'\n");       break; }
    case TokRBrace:       { fprintf (fp, "'}'\n");       break; }
    case TokColon:        { fprintf (fp, "':'\n");       break; }
    case TokSemicolon:    { fprintf (fp, "';'\n");       break; }
    case TokPeriod:       { fprintf (fp, "'.'\n");       break; }
    case TokComma:        { fprintf (fp, "','\n");       break; }
    case TokHash:         { fprintf (fp, "'#'\n");       break; }
    case TokAt:           { fprintf (fp, "'@'\n");       break; }
    case TokQuestionMark: { fprintf (fp, "'?'\n");       break; }
    case TokEqual:        { fprintf (fp, "'='\n");       break; }
    case TokEQ:           { fprintf (fp, "'=='\n");      break; }
    case TokNE:           { fprintf (fp, "'!='\n");      break; }
    case TokLT:           { fprintf (fp, "'<'\n");       break; }
    case TokLTLT:         { fprintf (fp, "'<<'\n");      break; }
    case TokLE:           { fprintf (fp, "'<='\n");      break; }
    case TokGE:           { fprintf (fp, "'>='\n");      break; }
    case TokGT:           { fprintf (fp, "'>'\n");       break; }
    case TokGTGT:         { fprintf (fp, "'>>'\n");      break; }
    case TokPlus:         { fprintf (fp, "'+'\n");       break; }
    case TokMinus:        { fprintf (fp, "'='\n");       break; }
    case TokTimes:        { fprintf (fp, "'*'\n");       break; }
    case TokPercent:      { fprintf (fp, "'%%'\n");      break; }
    case TokSlash:        { fprintf (fp, "'/'\n");       break; }
    case TokAmp:          { fprintf (fp, "'&'\n");       break; }
    case TokAmpAmp:       { fprintf (fp, "'&&'\n");      break; }
    case TokCaret:        { fprintf (fp, "'^'\n");       break; }
    case TokVBar:         { fprintf (fp, "'|'\n");       break; }
    case TokVBarVBar:     { fprintf (fp, "'||'\n");      break; }
    case TokTilde:        { fprintf (fp, "'~'\n");       break; }
    case TokBang:         { fprintf (fp, "'!'\n") ;      break; }
    case TokEOF:          { fprintf (fp, "TokEOF\n");          break; }
    case TokErr:          { fprintf (fp, "TokErr\n");          break; }

    case TokExpr: {
	fprintf (stderr, "Error: tokType is TokExpr, which is illegal here\n");
	exit (1);
    }
    }
} /* fprintCurTok() */

/* ----------------------------------------------------------------
 * printFileStack()
 */

static
void
printFileStack (FILE  *fpOut)
{
    fprintf (fpOut, "  file %s, line %d, col %d\n",
	     filename,
	     lineNum,
	     colNum);
} /* printFileStack() */

/* ----------------------------------------------------------------
 * fprintErrPos()
 */

static
void
fprintErrPos (FILE *fpOut)
{
    int j;

    if (inputLine [0] == EOF)
	fprintf (fpOut, "  EOF\n");
    else
	fprintf (fpOut, "  %s", inputLine);
    for (j = 1; j < colNum; j++) fprintf (fpOut, " ");
    fprintf (fpOut, "  ^\n");
    printFileStack (fpOut);
} /* fprintErrPos() */

/* ----------------------------------------------------------------
 * openFile()
 */

static
void
openFile (char *filename)
{
    fp = fopen (filename, "r");
    if (fp == NULL) {
	fprintf (stderr, "***** Error (openFile) Unable to open file: %s\n", filename);
	printFileStack (stderr);
	exit (1);
    }

    strcpy (filename, filename);
    lineNum     = 1;
    colNum      = 0;

    inputLineCursor = 0;
    inputLine [inputLineCursor] = 0;
    inputChar = ' ';
} /* openFile() */

/* ----------------------------------------------------------------
 * getNextChar()
 * Get next input character
 * Skip regions bracketed by
 *     '// synopsys translate_off' and
 *     '// synopsys translate_on'
 */

static
void
getNextChar (void)
{
    char  *p1, *p2;
    int    j;

    if (inputChar == EOF) return;

    if (inputChar == '\n') {
	lineNum++;
	colNum = 1;
    }
    else if (inputChar == '\t') {
	j       = colNum;
	j      += ((((j - 1) / 8) + 1) * 8 + 1);
	colNum += j;
    }
    else
	colNum++;

    if (inputLine [inputLineCursor] == 0) {
	inputLineCursor = 0;

	// Loop, skipping 'translate_off/translate_on' regions if any
	p1 = fgets (inputLine, MAX_INPUT_LINE_LEN, fp);
	while (TRUE) {
	    if (p1 == NULL) break;

	    for (p2 = p1; (*p2 == ' ') || (*p2 == '\t'); p2++);
	    if (strncmp (p2,
			 "// synopsys translate_off",
			 strlen("// synopsys translate_off")) != 0) break;

	    // Skip lines until translate_on
	    while (TRUE) {
		p1 = fgets (inputLine, MAX_INPUT_LINE_LEN, fp);
		lineNum++;
		if (p1 == NULL) break;

		for (p2 = p1; (*p2 == ' ') || (*p2 == '\t'); p2++);
		if (strncmp (p2,
			     "// synopsys translate_on",
			     strlen ("// synopsys translate_on")) == 0) {
		    p1 = fgets (inputLine, MAX_INPUT_LINE_LEN, fp);
		    lineNum++;
		    break;
		}
	    }
	}
	if (p1 == NULL) {
	    inputLine [0] = EOF;
	    inputLine [1] = 0;
	}
    }

    assert (inputLine [inputLineCursor] != 0);

    inputChar = inputLine [inputLineCursor];
    inputLineCursor++;

    /*
    printf ("Lex: Line %d Col %d Char 0x%x",
	    fileStack [fileStackPtr - 1].lineNum,
	    fileStack [fileStackPtr - 1].colNum,
	    inputChar);
    if (inputChar >= ' ') printf ("(%c)", inputChar);
    printf ("\n");
    */
} /* getNextChar() */

/* ----------------------------------------------------------------
 * lookaheadChar()
 * Lookahead at next input character
 */

static
int
lookaheadChar (void)
{
    int  ch;

    if (inputChar == EOF) ch = EOF;
    else ch = inputLine [inputLineCursor];

    return ch;
} /* lookaheadChar() */

/* ----------------------------------------------------------------
 * skipWhiteSpace()
 */

static
void
skipWhiteSpace (void)
{
    while (TRUE) {
	if (inputChar == EOF) break;

	if (inputChar <= ' ') {
	    getNextChar ();
	}
	else if ((inputChar == '/') && (lookaheadChar () == '/')) {
	    while ((inputChar != '\n') && (inputChar != EOF))
		getNextChar ();
	    getNextChar ();
	}
	else
	    break;
    }
} /* skipWhiteSpace() */

/* ----------------------------------------------------------------
 * skipToEOL()
 */

static
void
skipToEOL (void)
{
    while ((inputChar != EOF) && (inputChar != '\n')) 
	getNextChar ();

    getNextChar ();
} /* skipToEOL() */

/* ----------------------------------------------------------------
 * appendToTokSVal()
 */

static
void
appendToTokSVal (char *tokSVal, int *tokSValLenP, int  ch)
{
    int tokSValLen;

    tokSValLen = *tokSValLenP;
    if (tokSValLen >= MAX_STRING_LEN - 1) {
	if (tokSValLen == MAX_STRING_LEN - 1) {
	    fprintf (stderr,
		     "Very long lexical token (> %d chars); ignoring characters from this point\n",
		     tokSValLen);
	    fprintErrPos (stderr);
	}
    }
    else
	tokSVal [tokSValLen] = ch;
    tokSValLen++;
    *tokSValLenP = tokSValLen;
} /* appendToTokSVal() */

/* ----------------------------------------------------------------
 * lexToEOL()
 * Lex all text to end of line (trimming leading and trailing blanks)
 * (used for error recovery)
 */

/*
static
void
lexToEOL (void)
{
    int lenMinusTrailingWhitespace;

    while ((inputChar == ' ') || (inputChar == '\t'))
	getNextChar ();

    lenMinusTrailingWhitespace = 0;
    tokSValLen = 0;
    while (TRUE) {
	if (inputChar == '\n') {
	    getNextChar ();
	    tokType = TokWord;
	    tokSValLen = lenMinusTrailingWhitespace;
	    return;
	}
	appendToTokSVal (tokSVal, & tokSValLen, inputChar);
	if (inputChar > ' ') lenMinusTrailingWhitespace = tokSValLen;
	getNextChar ();
    }
}  // lexToEOL()
*/

/* ----------------------------------------------------------------
 * lexStringDescription()
 * Lexes a chip, register or field 'description',
 * enclosed in double-quotes.
 * Current inputChar is character after first doublequote
 */

static
void
lexStringDescription (void)
{
    char    sVal [MAX_STRING_LEN];
    int     sValLen;

    sValLen = 0;

    while (TRUE) {
	/* ---- Invariant: at char after opening double-quote */
	while (TRUE) {
	    if (inputChar == '\"') {
		getNextChar ();
		break;
	    }

	    if (inputChar == '\\') {
		getNextChar ();
		switch (inputChar) {
		case 'n': appendToTokSVal (sVal, & sValLen, '\n'); break;
		case 'r': appendToTokSVal (sVal, & sValLen, '\r'); break;
		case 't': appendToTokSVal (sVal, & sValLen, '\t'); break;
		default:  appendToTokSVal (sVal, & sValLen, inputChar); break;
		}
		getNextChar ();
	    }
	    else {
		appendToTokSVal (sVal, & sValLen, inputChar);
		getNextChar ();
	    }
	}

	/* ---- Invariant: at char after closing double-quote */
	skipWhiteSpace ();
	if (inputChar != '\"')
	    break;
	else
	    getNextChar ();
    }
    tokType    = TokDescription;
    strcpy (tokSVal, sVal);
    tokSValLen = sValLen;
} /* lexStringDescription() */

/* ----------------------------------------------------------------
 * lexWord()
 * Lex a word.  Current inputChar is '_' or alphanumeric
 */

static
void
lexWord (void)
{
    char    wordSVal [MAX_STRING_LEN];
    int     wordSValLen;

    wordSValLen  = 0;
    appendToTokSVal (wordSVal, & wordSValLen, inputChar);
    getNextChar ();
    while (TRUE) {
	if (isalpha (inputChar) ||
            (inputChar == '_') ||
            isdigit (inputChar) ||
            (inputChar == '$')) {
	    appendToTokSVal (wordSVal, & wordSValLen, inputChar);
	    getNextChar ();
	}
	else
	    break;
    }
    wordSVal [wordSValLen] = 0;

    /* ---- copy out from local copy to global copy */
    tokType = TokWord;
    strcpy (tokSVal, wordSVal);
    tokSValLen = wordSValLen;

    /*
    fprintf (stdout, "lexWord: %s\n", tokSVal);
    */
} /* lexWord() */

/* ----------------------------------------------------------------
 * Numbers
 */

/* ----------------
 * baseVal()
 */

static
int
baseVal (int ch, int base)
{
    switch (base) {
    case 2: {
	if      (ch == '0') return 0;
	else if (ch == '1') return 1;
	break;
    }
    case 8: {
	if (('0' <= ch) && (ch <= '7')) return ch - '0';
	break;
    }
    case 10: {
	if (('0' <= ch) && (ch <= '9')) return ch - '0';
	break;
    }
    case 16:{
	if (('0' <= ch) && (ch <= '9')) return ch - '0';
	if (('a' <= ch) && (ch <= 'f')) return ch - 'a' + 10;
	if (('A' <= ch) && (ch <= 'F')) return ch - 'A' + 10;
	break;
    }
    }
    return -1;
} /* baseVal */

/* ----------------
 * lexUnsignedInt()
 * Read an  integer with given base.  inputChar is the first digit char.
 */

static
void
lexUnsignedInt (int base)
{
    int  v;

    assert ((base == 2) || (base == 8) || (base == 10) || (base == 16));

    tokIVal     = 0;
    while (TRUE) {
	v = baseVal (inputChar, base);
	if (v < 0) return;
	appendToTokSVal (tokSVal, & tokSValLen, inputChar);
	tokIVal = tokIVal * base + v;

	getNextChar ();
    }
} /* lexUnsignedInt() */

/* ----------------
 * lexInt()
 * Read an integer.  inputChar is a digit char.
 */

static
void
lexInt (void)
{
    tokSValLen = 0;

    tokIWidth         = 0;
    tokIBase          = 10;
    tokIVerilogFormat = FALSE;
    lexUnsignedInt (10);

    if (inputChar == 'x') {
	tokIBase  = 16;
	appendToTokSVal (tokSVal, & tokSValLen, inputChar);
	getNextChar ();
	lexUnsignedInt (16);
    }
    else if (inputChar == '\'') {
	/* ---- Verilog radix format */
	tokIWidth         = tokIVal;
	tokIVerilogFormat = TRUE;
	appendToTokSVal (tokSVal, & tokSValLen, inputChar);
	getNextChar ();
	if ((inputChar == 'h') || (inputChar == 'H')) {
	    tokIBase = 16;
	    appendToTokSVal (tokSVal, & tokSValLen, inputChar);
	    getNextChar ();
	    lexUnsignedInt (16);
	}
	else if ((inputChar == 'o') || (inputChar == 'O')) {
	    tokIBase = 8;
	    appendToTokSVal (tokSVal, & tokSValLen, inputChar);
	    getNextChar ();
	    lexUnsignedInt (8);
	}
	else if ((inputChar == 'd') || (inputChar == 'd')) {
	    tokIBase = 10;
	    appendToTokSVal (tokSVal, & tokSValLen, inputChar);
	    getNextChar ();
	    lexUnsignedInt (10);
	}
	else if ((inputChar == 'b') || (inputChar == 'B')) {
	    tokIBase = 2;
	    appendToTokSVal (tokSVal, & tokSValLen, inputChar);
	    getNextChar ();
	    lexUnsignedInt (2);
	}
	else {
	    fprintf (stderr, "Illegal number syntax. Expecting an 'h' or 'o' or 'd' or 'b' after the tick\n");
	    fprintErrPos (stderr);
	    skipToEOL ();
	    tokType = TokErr;
	    return;
	}
    }

    tokType = TokInt;
} /* lexInt() */

/* ================================================================
 * lexAnItem()
 */

static
void
lexAnItem (void)
{
    skipWhiteSpace ();

    tokType           = TokErr;
    tokSValLen        = 0;
    tokIVal           = 0;
    tokIWidth         = 0;
    tokIBase          = 0;
    tokIVerilogFormat = FALSE;
    tokLine           = lineNum;
    tokCol            = colNum;

    if (inputChar == EOF) { tokType = TokEOF;       goto DONE; }
    if (inputChar == ':') { tokType = TokColon;     getNextChar (); goto DONE; }
    if (inputChar == ';') { tokType = TokSemicolon; getNextChar (); goto DONE; }
    if (inputChar == '.') { tokType = TokPeriod;    getNextChar (); goto DONE; }
    if (inputChar == ',') { tokType = TokComma;     getNextChar (); goto DONE; }
    if (inputChar == '#') { tokType = TokHash;      getNextChar (); goto DONE; }
    if (inputChar == '@') { tokType = TokAt;        getNextChar (); goto DONE; }
    if (inputChar == '?') { tokType = TokQuestionMark;  getNextChar (); goto DONE; }
    if (inputChar == '*') { tokType = TokTimes;     getNextChar (); goto DONE; }
    if (inputChar == '%') { tokType = TokPercent;   getNextChar (); goto DONE; }
    if (inputChar == '~') { tokType = TokTilde;     getNextChar (); goto DONE; }

    if (inputChar == '^') { tokType = TokCaret;     getNextChar (); goto DONE; }

    if (inputChar == '[') { tokType = TokLBracket;  getNextChar (); goto DONE; }
    if (inputChar == ']') { tokType = TokRBracket;  getNextChar (); goto DONE; }
    if (inputChar == '(') { tokType = TokLParen;    getNextChar (); goto DONE; }
    if (inputChar == ')') { tokType = TokRParen;    getNextChar (); goto DONE; }
    if (inputChar == '{') { tokType = TokLBrace;    getNextChar (); goto DONE; }
    if (inputChar == '}') { tokType = TokRBrace;    getNextChar (); goto DONE; }
    if (inputChar == '/') { tokType = TokSlash;     getNextChar (); goto DONE; }
    if (inputChar == '-') { tokType = TokMinus;     getNextChar (); goto DONE; }
    if (inputChar == '+') { tokType = TokPlus;      getNextChar (); goto DONE; }

    if (inputChar == '=') {
	getNextChar ();
	if (inputChar == '=') {
	    tokType = TokEQ;
	    getNextChar ();
	}
	else
	    tokType = TokEqual;
    }
    else if (inputChar == '<') {
	getNextChar ();
	if (inputChar == '=') {
	    tokType = TokLE;
	    getNextChar ();
	}
	else if (inputChar == '<') {
	    tokType = TokLTLT;;
	    getNextChar ();
	}
	else
	    tokType = TokLT;
    }
    else if (inputChar == '>') {
	getNextChar ();
	if (inputChar == '=') {
	    tokType = TokGE;
	    getNextChar ();
	}
	else if (inputChar == '>') {
	    tokType = TokGTGT;
	    getNextChar ();
	}
	else
	    tokType = TokGT;
    }
    else if (inputChar == '!') {
	getNextChar ();
	if (inputChar == '=') {
	    tokType = TokNE;
	    getNextChar ();
	}
	else
	    tokType = TokBang;
    }
    else if (inputChar == '&') {
	getNextChar ();
	if (inputChar == '&') {
	    tokType = TokAmpAmp;
	    getNextChar ();
	}
	else
	    tokType = TokAmp;
    }
    else if (inputChar == '|') {
	getNextChar ();
	if (inputChar == '|') {
	    tokType = TokVBarVBar;
	    getNextChar ();
	}
	else
	    tokType = TokVBar;
    }

    else if (isdigit (inputChar))
	lexInt ();

    else if (inputChar == '\"') {
	getNextChar ();
	lexStringDescription ();
    }

    else if ((inputChar == '_') || isalpha (inputChar))
	lexWord ();

    goto DONE;

    fprintf (stderr, "Illegal character: ");
    if (inputChar >= ' ') fprintf (stderr, "'%c'", inputChar);
    fprintf (stderr, " (0x%x)\n", inputChar);
    fprintErrPos (stderr);
    skipToEOL ();
    tokType = TokErr;

 DONE:
    /*
    fprintf (stdout, "<-- lexAnItem: "); fprintCurTok (stdout);
    */
    return;
} /* lexAnItem() */

/* ----------------------------------------------------------------
 * initLexer()
 */

static
void
initLexer (char *filename)
{
    openFile (filename);
} /* initLexer() */

/* ----------------------------------------------------------------
 * testLexer()
 * Just reads the tokens from a file and prints them out.
 */

void
testLexer (char *filename)
{
    initLexer (filename);

    while (TRUE) {
	lexAnItem ();
	fprintCurTok (stdout);
	if (tokType == TokEOF) break;
	if (tokType == TokErr) break;
    }
} /* testLexer() */

/* ****************************************************************
 * Lists
 */

/* ----------------
 * cons()
 */

List *
cons (void *h, List *t, char *callerName)
{
    List *l;

    l = checked_malloc (sizeof (List), callerName);
    l->head = h;
    l->tail = t;
    return l;
} /* cons() */

/* ----------------
 * list1()
 */

List *
list1 (void *x1, char *callerName)
{
    return cons (x1, NULL, callerName);
} /* list1() */

/* ----------------
 * list2()
 */

List *
list2 (void *x1, void *x2, char *callerName)
{
    return cons (x1,
		 cons (x2,
		       NULL,
		       callerName),
		 callerName);
} /* list2() */

/* ----------------
 * list3()
 */

List *
list3 (void *x1, void *x2, void *x3, char *callerName)
{
    return cons (x1,
		 cons (x2,
		       cons (x3, NULL, callerName),
		       callerName),
		 callerName);
} /* list3() */

/* ----------------
 * listLength()
 */

int
listLength (List *lp)
{
    int  n;

    n = 0;
    while (lp != NULL) {
	n++;
	lp = lp->tail;
    }

    return n;
} /* listLength() */

/* ----------------
 * listIndex()
 * Returns n'th element of list (0-indexed)
 */

void *
listIndex (List *lp, int n)
{
    assert (lp != NULL);
    if (n > 0)
	return listIndex (lp->tail, n-1);
    else
	return lp->head;
} /* listIndex() */

/* ----------------
 * listLast()
 * Returns last cons of a non-empty list
 */

List *
listLast (List *lp)
{
    assert (lp != NULL);

    while (lp->tail != NULL) lp = lp->tail;

    return lp;
} /* listLast() */

/* ----------------
 * listJoin()
 * With side effect: modifies l1's last tail, if l1 is not null.
 * Does not cons.
 * Returns ptr to joined list.
 */

List *
listJoin (List *lp1, List *lp2)
{
    List  *lp1a;

    if (lp1 == NULL) return lp2;

    lp1a = lp1;
    while (lp1a->tail != NULL) lp1a = lp1a->tail;
    lp1a->tail = lp2;

    return lp1;
} /* listJoin() */

/* ----------------
 * listChopLast()
 * Drops last cons from a non-empty list.
 * Does not cons.
 * Returns head of chopped list.
 */

List *
listChopLast (List *lp)
{
    List *lp1;
    assert (lp != NULL);

    if (lp->tail == NULL) return NULL;

    lp1 = lp;
    while (lp1->tail->tail != NULL) lp1 = lp1->tail;
    lp1->tail = NULL;

    return lp;
} /* listChopLast() */

/* ----------------
 * listReverseInSitu()
 * Reverses a list, in place (i.e., no consing)
 */

List *
listReverseInSitu (List *lp)
{
    List *lp1, *lp2, *lp3;

    if (lp == NULL) return NULL;

    lp1       = lp;
    lp2       = lp->tail;
    lp1->tail = NULL;
    while (lp2 != NULL) {
	lp3 = lp2->tail;
	lp2->tail = lp1;
	lp1 = lp2;
	lp2 = lp3;
    }
    return lp1;
} /* listReverseInSitu() */

/* ****************************************************************
 * Parser
 */

typedef enum KeywordNum {
    KW_MODULE,
    KW_ENDMODULE,
    KW_INPUT,
    KW_OUTPUT,
    KW_REG,
    KW_WIRE,
    KW_ASSIGN,
    KW_ALWAYS,
    KW_CASE,
    KW_ENDCASE,
    KW_DEFAULT,
    KW_OR,

    NUM_KWS    // dummy, to count number of keywords
} KeywordNum;

typedef struct KeywordDescr {
    KeywordNum  kwNum;
    char        kwString[40];
} KeywordDescr;

static
KeywordDescr keywords[] = {
    { KW_MODULE,             "module" },
    { KW_ENDMODULE,          "endmodule" },
    { KW_INPUT,              "input" },
    { KW_OUTPUT,             "output" },
    { KW_REG,                "reg" },
    { KW_WIRE,               "wire" },
    { KW_ASSIGN,             "assign" },
    { KW_ALWAYS,             "always" },
    { KW_CASE,               "case" },
    { KW_ENDCASE,            "endcase" },
    { KW_DEFAULT,            "default" },
    { KW_OR,                 "or" },
    { NUM_KWS,               "dummy" }
};

static
jmp_buf  readerSetjmpEnv;

/* ----------------------------------------------------------------
 * allocASTToken()
 * Allocates a dynamic token.
 * Client will set its type and contents
 */

Token *
allocASTToken (char *clientNameS)
{
    Token *tokP;

    tokP = checked_malloc (sizeof (Token), clientNameS);
    tokP->tokLine           = -1;
    tokP->tokCol            = -1;
    tokP->tokType           = TokErr;  /* ---- caller should replaced this */
    tokP->tokSVal           = NULL;
    tokP->tokIVal           = 0;
    tokP->tokIWidth         = 0;
    tokP->tokIBase          = 10;
    tokP->tokIVerilogFormat = FALSE;
    tokP->args              = NULL;
    return tokP;
} /* allocASTToken() */

/* ----------------------------------------------------------------
 * mkASTToken()
 * Creates a dynamic token from the current token info.
 */

static
Token *
mkASTToken (void)
{
    Token *tokP;

    tokP = checked_malloc (sizeof (Token), "mkASTToken/tokP");
    tokP->tokLine        = tokLine;
    tokP->tokCol         = tokCol;
    tokP->tokType        = tokType;

    if ((tokType == TokWord) ||
	(tokType == TokDescription)) {
	tokSVal [tokSValLen] = 0;
	tokP->tokSVal = checked_malloc (tokSValLen + 1, "mkASTToken/tokSVal");
	strncpy (tokP->tokSVal, tokSVal, tokSValLen + 1);
    }
    else {
	tokP->tokSVal = NULL;
    }
    tokP->tokIVal           = tokIVal;
    tokP->tokIWidth         = tokIWidth;
    tokP->tokIBase          = tokIBase;
    tokP->tokIVerilogFormat = tokIVerilogFormat;
    tokP->args              = NULL;

    /*
    fprintf (stdout, "<-- mkASTToken() ");
    fprintToken (stdout, tokP, 1);
    fprintf (stdout, "\n");
    */

    return tokP;
} /* mkASTToken() */

/* ----------------------------------------------------------------
 * fprintTokenLocation()
 * Prints the location (file, line, col) of a dynamic token
 */

void
fprintTokenLocation (FILE *fp, Token  *tokp)
{
    if (tokp->tokLine > 0)
	fprintf (fp, "at Line %d, Col %d",
		 tokp->tokLine, tokp->tokCol);    
} /* fprintTokenLocation() */

/* ----------------------------------------------------------------
 * fprintOp()
 * Prints an expression opcode within a field of formatWidth
 */

void
fprintOp (FILE *fp, Op  op, int formatWidth)
{
    int j, n;

    switch (op) {
    case OpLogicalOr:  fprintf (fp, "||");  n = 2; break;
    case OpLogicalAnd: fprintf (fp, "&&");  n = 2; break;

    case OpBitOr:      fprintf (fp, "|");   n = 1; break;
    case OpBitExOr:    fprintf (fp, "^");   n = 1; break;
    case OpBitAnd:     fprintf (fp, "&");   n = 1; break;

    case OpEQ:         fprintf (fp, "==");  n = 2; break;
    case OpNE:         fprintf (fp, "!=");  n = 2; break;

    case OpLT:         fprintf (fp, "<");   n = 1; break;
    case OpLE:         fprintf (fp, "<=");  n = 2; break;
    case OpGE:         fprintf (fp, ">=");  n = 2; break;
    case OpGT:         fprintf (fp, ">");   n = 1; break;

    case OpLShift:     fprintf (fp, "<<");  n = 2; break;
    case OpRShift:     fprintf (fp, ">>");  n = 2; break;

    case OpPlus:       fprintf (fp, "+");   n = 1; break;
    case OpMinus:      fprintf (fp, "-");   n = 1; break;

    case OpTimes:      fprintf (fp, "*");   n = 1; break;
    case OpMod:        fprintf (fp, "%%");  n = 1; break;
    case OpIntDiv:     fprintf (fp, "/");   n = 1; break;

    case OpBitNot:     fprintf (fp, "~");   n = 1; break;
    case OpLogicalNot: fprintf (fp, "!");   n = 1; break;

    case OpCondExpr     : fprintf (fp, "?:"); n = 2; break;

    case OpBitSelect1   : fprintf (fp, "bitselect1");   n = strlen ("bitselect1");   break;
    case OpBitSelect2   : fprintf (fp, "bitselect2");   n = strlen ("bitselect2");   break;
    case OpBitConcat    : fprintf (fp, "bitconcat");    n = strlen ("bitconcat");    break;
    case OpBitReplicate : fprintf (fp, "bitreplicate"); n = strlen ("bitreplicate"); break;
    case OpConst        : fprintf (fp, "const");        n = strlen ("const");        break;
    case OpCase         : fprintf (fp, "case");         n = strlen ("case");         break;
    }
    for (j = n+1; j <= formatWidth; j++)
	fprintf (fp, " ");
} /* fprintOp() */

/* ----------------------------------------------------------------
 * fprintToken()
 * Prints a dynamic token
 * Verbosity 1 adds hex, width, base to ints
 * Verbosity 2 adds token types, line, col.
 */

void
fprintToken (FILE *fp, Token  *tokp, int verbosity)
{
    List  *xs;

    if (verbosity > 1) {
	fprintf (fp, "[Token: ");
	fprintTokenLocation (fp, tokp);
	fprintf (fp, ", ");
    }

    switch (tokp->tokType) {
    case TokWord: {
	if (verbosity > 1) fprintf (fp, "Word: ");
	fprintf (fp, "%s", tokp->tokSVal);
	break;
    }
    case TokDescription: {
	if (verbosity > 1) fprintf (fp, "Description: ");
	fprintf (fp, "\"%s\"", tokp->tokSVal);
	break;
    }
    case TokInt: {
	if (verbosity == 0) {
	    if (tokp->tokIBase == 16)
		if (tokp->tokIVerilogFormat)
		    fprintVerilogHex (fp, NULL, tokp->tokIWidth, tokp->tokIVal, NULL);
		else
		    fprintf (fp, "0x%08" LL "x", tokp->tokIVal);
	    else
		fprintf (fp, "%" LL"d", tokp->tokIVal);
	}
	else if (verbosity == 1)
	    fprintf (fp, "%" LL"d (0x%" LL "x width %d base %d, Verilog format %c)",
		     tokp->tokIVal, tokp->tokIVal, tokp->tokIWidth, tokp->tokIBase,
		     (tokp->tokIVerilogFormat ? 'Y' : 'N'));
	else
	    fprintf (fp, "Int %" LL"d (0x%" LL "x width %d base %d, Verilog format %c)",
		     tokp->tokIVal, tokp->tokIVal, tokp->tokIWidth, tokp->tokIBase,
		     (tokp->tokIVerilogFormat ? 'Y' : 'N'));
	break;
    }
    case TokLBracket:   { fprintf (fp, "[");   break; }
    case TokRBracket:   { fprintf (fp, "]");   break; }
    case TokLParen:     { fprintf (fp, "(");   break; }
    case TokRParen:     { fprintf (fp, ")");   break; }
    case TokLBrace:     { fprintf (fp, "{");   break; }
    case TokRBrace:     { fprintf (fp, "}");   break; }
    case TokColon:      { fprintf (fp, ":");   break; }
    case TokSemicolon:  { fprintf (fp, ";");   break; }
    case TokPeriod:     { fprintf (fp, ".");   break; }
    case TokComma:      { fprintf (fp, ",");   break; }
    case TokHash:       { fprintf (fp, "#");   break; }
    case TokAt:         { fprintf (fp, "@");   break; }
    case TokQuestionMark: { fprintf (fp, "?");   break; }
    case TokEqual:      { fprintf (fp, "=");   break; }
    case TokEQ:         { fprintf (fp, "==");  break; }
    case TokNE:         { fprintf (fp, "!=");  break; }
    case TokLT:         { fprintf (fp, "<");   break; }
    case TokLE:         { fprintf (fp, "<=");  break; }
    case TokGE:         { fprintf (fp, ">=");  break; }
    case TokGT:         { fprintf (fp, ">");   break; }
    case TokPlus:       { fprintf (fp, "+");   break; }
    case TokMinus:      { fprintf (fp, "-");   break; }
    case TokTimes:      { fprintf (fp, "*");   break; }
    case TokPercent:    { fprintf (fp, "%%");  break; }
    case TokAmp:        { fprintf (fp, "&");   break; }
    case TokAmpAmp:     { fprintf (fp, "&&");  break; }
    case TokCaret:      { fprintf (fp, "^");   break; }
    case TokVBar:       { fprintf (fp, "|");   break; }
    case TokVBarVBar:   { fprintf (fp, "||");  break; }
    case TokTilde:      { fprintf (fp, "~");   break; }
    case TokBang:       { fprintf (fp, "!");   break; }
    case TokLTLT:       { fprintf (fp, "<<");  break; }
    case TokGTGT:       { fprintf (fp, ">>");  break; }
    case TokSlash:      { fprintf (fp, "/");  break; }
    case TokExpr: {
	fprintf (fp, "(");
	fprintOp (fp, tokp->op, 0);
	for (xs = tokp->args; xs != NULL; xs = xs->tail) {
	    fprintf (fp, " ");
	    fprintToken (fp, xs->head, verbosity);
	}
	fprintf (fp, ")");
	break;
    }
    case TokEOF:      { fprintf (fp, "EOF");    break; }
    case TokErr:      { fprintf (fp, "Error");    break; }
    }
    if (verbosity > 1)
	fprintf (fp, "]");
} /* fprintToken() */

/* ================================================================
 * Parsing functions
 */

typedef AST * (ParserFn) (void);

/* ----------------------------------------------------------------
 * Parsing expressions
 */

/* ----------------------------------------------------------------
 * Globals to hold chip-level attributes
 */

/* ----------------
 * Precedence table
 */

typedef struct OpTableEntry {
    TokType  tokType;
    int      precedence;
    Op       op;
} OpTableEntry;

static
OpTableEntry  opTable[] = {

    /* WARNING: ensure "#define MIN_PRECEDENCE  1" earlier in this file */
    { TokVBarVBar,      2, OpLogicalOr },
    { TokAmpAmp,        3, OpLogicalAnd },
    { TokVBar,          4, OpBitOr },

    { TokCaret,         5, OpBitExOr },

    { TokAmp,           6, OpBitAnd },

    { TokEQ,            7, OpEQ },
    { TokNE,            7, OpNE },

    { TokLT,            8, OpLT },
    { TokLE,            8, OpLE },
    { TokGE,            8, OpGE },
    { TokGT,            8, OpGT },

    { TokLTLT,          9, OpLShift },
    { TokGTGT,          9, OpRShift },

    { TokPlus,         10, OpPlus },
    { TokMinus,        10, OpMinus },

    { TokTimes,        11, OpTimes },
    { TokPercent,      11, OpMod },
    { TokSlash,        11, OpIntDiv },

    { TokBang,         12, OpLogicalNot },
    { TokTilde,        12, OpBitNot },
    /* WARNING: ensure "#define MAX_PRECEDENCE 12" earlier in this file */

    { TokEOF, 0, 0 }        /* Dummy sentinel value */
};

/* ----------------
 * binOpPrecedence()
 */

static
int
binOpPrecedence (TokType  tokType,  Op *opP)
{
    int j;

    for (j = 0; TRUE; j++) {
	if ((opTable [j].tokType == TokEOF) || (opTable [j].tokType == tokType))
	    break;
    }
    *opP = opTable [j].op;
    return opTable [j].precedence;
} /* binOpPrecedence() */

/* ----------------
 * parseFactor()
 */

static
Token *
parseFactor (void)
{
    Token    *ep, *ep1, *ep2, *ep3;
    Op        op;
    List     *es;

    if (tokType == TokLParen) {
	lexAnItem ();
	ep = parseExprRec (MIN_PRECEDENCE);
	if (tokType == TokRParen)
	    lexAnItem ();
	else if (tokType == TokQuestionMark) {
	    ep1 = ep;
	    ep  = mkASTToken ();
	    lexAnItem ();
	    ep2 = parseExprRec (MIN_PRECEDENCE);
	    if (tokType != TokColon) return NULL;
	    lexAnItem ();
	    ep3 = parseExprRec (MIN_PRECEDENCE);
	    if (tokType != TokRParen) return NULL;
	    lexAnItem ();
	    ep->op = OpCondExpr;
	    ep->tokType = TokExpr;
	    ep->args = list3 (ep1, ep2, ep3, "parseFactor/condExpr");
	}
	else
	    return NULL;
    }
    else if (tokType == TokLBrace) {
	ep = mkASTToken ();
	lexAnItem ();
	es = NULL;
	while (TRUE) {
	    es = listJoin (es, list1 (parseExprRec (MIN_PRECEDENCE), "parseFactor/bitConcat"));
	    if (tokType == TokComma) {
		lexAnItem ();
		continue;
	    }
	    if (tokType == TokRBrace) {
		lexAnItem ();
		break;
	    }
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting an ',' or '}' in a concatenation expression\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
	if (es->tail == NULL) {
	    // Trivial (1-ary) bit-concats are elided
	    ep = es->head;    // TODO: old ep should be free'd
	}
	else {
	    ep->op = OpBitConcat;
	    ep->tokType = TokExpr;
	    ep->args = es;
	}
    }
    else if ((tokType == TokInt) || (tokType == TokWord)) {
	ep = mkASTToken ();
	lexAnItem ();
    }
    else if ((tokType == TokMinus) || (tokType == TokPlus)) {
	ep = NULL;
	if (tokType == TokMinus) {
	    ep = mkASTToken ();
	    ep1 = mkASTToken ();
	}

	lexAnItem ();
	ep2 = parseFactor ();

	if (ep == NULL)
	    ep = ep2;
	else {
	    /* ---- Convert ep, ep1 and fill arg fields */
	    ep->op      = OpMinus;
	    ep->tokType = TokExpr;

	    ep1->tokType   = TokInt;
	    ep1->tokIWidth = 1;
	    ep1->tokIBase  = 10;
	    ep1->tokIVerilogFormat = FALSE;
	    ep1->tokIVal = 0;

	    ep->args    = list2 (ep1, ep2, "parseFactor/prefix minus");
	}
    }
    else if ((tokType == TokTilde) || (tokType == TokBang)) {
	op = ((tokType == TokTilde) ? OpBitNot : OpLogicalNot);
	ep = mkASTToken ();

	lexAnItem ();
	ep2 = parseFactor ();

	/* ---- Convert ep and fill arg fields */
	ep->op      = op;
	ep->tokType = TokExpr;
	ep->args    = list1 (ep2, "parseFactor/prefixop");
    }
    else {
	fprintf (stderr, "***** Error: at lexical token: ");
	fprintCurTok (stderr);
	fprintf (stderr, "      Expecting an integer, variable, or left paren in an expression\n");
	fprintErrPos (stderr);
	exit (1);
    }

    if (tokType == TokLBrace) {
	ep1 = ep;
	// TODO: change this assertion to an error-check and report
	assert (ep1->tokType == TokInt);

	ep  = mkASTToken ();
	lexAnItem ();

	ep2 = parseExprRec (MIN_PRECEDENCE);
	if (tokType != TokRBrace) return NULL;
	lexAnItem ();

	ep->op   = OpBitReplicate;
	ep->tokType = TokExpr;
	ep->args = list2 (ep1, ep2, "parseFactor/bitreplicate");
    }

    return ep;
} /* parseFactor () */

/* ----------------
 * parseBitSelect()
 */

static
Token *
parseBitSelect (void)
{
    Token    *ep1, *ep2, *ep3, *ep;

    ep1 = parseFactor ();

    if (tokType != TokLBracket)
	return ep1;

    lexAnItem ();
    ep2 = parseExprRec (MIN_PRECEDENCE);
    if (tokType == TokRBracket) {
	ep = mkASTToken ();
	lexAnItem ();
	ep->op = OpBitSelect1;
	ep->tokType = TokExpr;
	ep->args = list2 (ep1, ep2, "parseBitSelect/bitselect1");
	return ep;
    }

    if (tokType != TokColon) {
	fprintf (stderr, "***** Error: at lexical token: ");
	fprintCurTok (stderr);
	fprintf (stderr, "      Expecting ':' in a [u:l] bit selection\n");
	fprintErrPos (stderr);
	exit (1);
    }

    lexAnItem ();
    ep3 = parseExprRec (MIN_PRECEDENCE);
    if (tokType == TokRBracket) {
	ep = mkASTToken ();
	lexAnItem ();
	ep->op = OpBitSelect2;
	ep->tokType = TokExpr;
	ep->args = list3 (ep1, ep2, ep3, "parseBitSelect/bitselect2");
	return ep;
    }

    fprintf (stderr, "***** Error: at lexical token: ");
    fprintCurTok (stderr);
    fprintf (stderr, "      Expecting ']' in a [u:l] bit selection\n");
    fprintErrPos (stderr);
    exit (1);
} /* parseBitSelect () */

/* ----------------
 * parseExprRec()
 */

static
Token *
parseExprRec (int precedence)
{
    Token  *ep1, *ep2, *ep3;
    Op      op;

    if (precedence > MAX_PRECEDENCE)
	ep1 = parseBitSelect ();
    else {
	ep1 = parseExprRec (precedence + 1);
	while (TRUE) {
	    if (binOpPrecedence (tokType, & op) == precedence) {
		ep2 = mkASTToken ();
		lexAnItem ();

		ep3 = parseExprRec (precedence + 1);

		/* ---- Convert ep2 and fill arg fields */
		ep2->op      = op;
		ep2->tokType = TokExpr;
		ep2->args    = list2 (ep1, ep3, "parseExprRec");
		ep1          = ep2;
	    }
	    else
		break;
	}
    }
    return ep1;
} /* parseExprRec() */

/* ----------------
 * parseExpr()
 * Note: each operator parsed will get either this tokIWidth and tokIBase,
 * or that of the most recent integer parsed.
 */

static
Token *
parseExpr (void)
{
    return parseExprRec (MIN_PRECEDENCE);
} /* parseExpr() */

/* ----------------------------------------------------------------
 * parseKeyword()
 * Look for one of the keywords amongst NKWS in KWDS[] array.
 * Return index in array, or -1 if not found.
 * Does not advance the lexical token.
 */

static
int
parseKeyword (KeywordDescr kwds[], int nkws)
{
    int j, cmp;

    for (j = 0; j < nkws; j++) {
	cmp = strcasecmp (tokSVal, kwds [j].kwString);
	if (cmp == 0) return kwds[j].kwNum;
    }
    return -1;
} /* parseKeyWord() */

/* ----------------------------------------------------------------
 * gobblePunctuation ()
 * Looks for a specific punctuation token, and advances
 */

#define MUST_GOBBLE      TRUE
#define MAY_GOBBLE  FALSE

static
Bool
gobblePunctuation (TokType expectedTokType, Bool must)
{
    Token  tok;

    if (tokType == expectedTokType) {
	lexAnItem ();
	return TRUE;
    }
    else {
	if (must) {
	    tok.tokType = expectedTokType;
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting: ");
	    fprintToken (stderr, & tok, 0);
	    fprintf (stderr, "\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
	return FALSE;
    }
} /* gobblePunctuation() */

/* ----------------------------------------------------------------
 * gobbleKeyword ()
 * Looks for a specific keyword token, and advances
 */

static
Bool
gobbleKeyword (KeywordNum  expectedKw, Bool must)
{
    if (parseKeyword (& keywords [expectedKw], 1) == expectedKw) {
	lexAnItem ();
	return TRUE;
    }
    else {
	if (must) {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting keyword: %s\n", keywords [expectedKw].kwString);
	    fprintErrPos (stderr);
	    exit (1);
	}
	return FALSE;
    }
} /* gobbleKeyword() */

/* ----------------------------------------------------------------
 * parseDecl()
 * Parses a declaration
 * Argument is the declaration keyword (input/output/wire/reg)
 * Current token is just past the declaration keyword
 */

static
AST *
parseDecl (Token *declKwp)
{
    Token  *ub, *lb;
    List   *ws;
    AST    *declp;

    // Optional [ub:lb] range
    if (! gobblePunctuation (TokLBracket, MAY_GOBBLE)) {
	ub = NULL;
	lb = NULL;
    }
    else {
	if (tokType != TokInt) {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting integer upper bound in a [ub:lb] range\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
	ub = mkASTToken ();
	lexAnItem ();

	gobblePunctuation (TokColon, MUST_GOBBLE);

	if (tokType != TokInt) {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting integer lower bound in a [ub:lb] range\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
	lb = mkASTToken ();
	lexAnItem ();

	gobblePunctuation (TokRBracket, MUST_GOBBLE);
    }

    // Identifiers being declared
    ws = NULL;
    while (TRUE) {
	if (tokType != TokWord) {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting an identifier in a declaration list\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
	ws = listJoin (ws, list1 (mkASTToken (), "parseDecl"));
	lexAnItem ();
	if (gobblePunctuation (TokComma,     MAY_GOBBLE)) continue;
	else if (gobblePunctuation (TokSemicolon, MAY_GOBBLE)) break;
	else {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting a ',' or ';' in a declaration list\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
    }

    declp = checked_malloc (sizeof (AST), "parseDecl");
    declp->tag                          = TagDecl;
    declp->u.declAttrs.declKw           = declKwp;
    declp->u.declAttrs.upperBound       = ub;
    declp->u.declAttrs.lowerBound       = lb;
    declp->u.declAttrs.ides             = ws;

    return declp;
} /* parseDecl() */

/* ----------------------------------------------------------------
 * parseModuleInst()
 * Parses a module instantiation statement:
 *     <ide> #(...<params>...) <ide> (.<ide>(<ide), ... ,.<ide>(<ide));
 * Current token is just past the initial identifier, which has
 * been passed in as the arg modNamep
 */

static
AST *
parseModuleInst (Token *modNamep)
{
    Token  *paramFormalp, *paramActualp,
	   *modInstNamep,
	   *portFormalp, *portActualp;
    List   *params, *ports;
    AST    *modInstp;

    // Optional parameters
    params = NULL;
    if (gobblePunctuation (TokHash, MAY_GOBBLE)) {
	gobblePunctuation (TokLParen, MUST_GOBBLE);
	while (TRUE) {
	    if (gobblePunctuation (TokPeriod, MAY_GOBBLE)) {
		if (tokType != TokWord) {
		    fprintf (stderr, "***** Error: at lexical token: ");
		    fprintCurTok (stderr);
		    fprintf (stderr, "      Expecting formal parameter name in module instance parameter list\n");
		    fprintErrPos (stderr);
		    exit (1);
		}
		paramFormalp = mkASTToken ();
		lexAnItem ();
		gobblePunctuation (TokLParen, MUST_GOBBLE);
		if (tokType != TokInt) {
		    fprintf (stderr, "***** Error: at lexical token: ");
		    fprintCurTok (stderr);
		    fprintf (stderr, "      Expecting integer in module instance parameter list\n");
		    fprintErrPos (stderr);
		    exit (1);
		}
		paramActualp = mkASTToken ();
		lexAnItem ();
		gobblePunctuation (TokRParen, MUST_GOBBLE);
	    }
	    else if (tokType == TokInt) {
		paramFormalp = NULL;
		paramActualp = mkASTToken ();
		lexAnItem ();
	    }
	    else {
		fprintf (stderr, "***** Error: at lexical token: ");
		fprintCurTok (stderr);
		fprintf (stderr, "      Expecting integer in module instance parameter list\n");
		fprintErrPos (stderr);
		exit (1);
	    }

	    params = listJoin (params,
			       list1 (list2 (paramFormalp, paramActualp, "parseModInst/param"),
				      "parseModInst/params"));

	    if (gobblePunctuation (TokComma, MAY_GOBBLE)) continue;
	    else
		break;
	}
	gobblePunctuation (TokRParen, MUST_GOBBLE);
    }

    // Module instance name
    if (tokType != TokWord) {
	fprintf (stderr, "***** Error: at lexical token: ");
	fprintCurTok (stderr);
	fprintf (stderr, "      Expecting module instance name\n");
	fprintErrPos (stderr);
	exit (1);
    }
    modInstNamep = mkASTToken ();
    lexAnItem ();

    // Module instance port connections
    gobblePunctuation (TokLParen, MUST_GOBBLE);
    ports = NULL;
    while (TRUE) {
	// Formal port name
	gobblePunctuation (TokPeriod, MUST_GOBBLE);

	if (tokType != TokWord) {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting module instance port formal name\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
	portFormalp = mkASTToken ();
	lexAnItem ();

	// Actual port wire
	gobblePunctuation (TokLParen, MUST_GOBBLE);
	if (tokType == TokWord) {
	    portActualp = mkASTToken ();
	    lexAnItem ();
	}
	else {
	    // Actual port wire missing, i.e., no connection
	    portActualp = NULL;
	}
	gobblePunctuation (TokRParen, MUST_GOBBLE);
	
	ports = listJoin (ports,
			  list1 (list2 (portFormalp, portActualp, "parseModuleInst/port_list2"),
				 "parseModuleInst/port"));

	if (gobblePunctuation (TokComma, MAY_GOBBLE)) continue;
	break;
    }
    gobblePunctuation (TokRParen, MUST_GOBBLE);
    gobblePunctuation (TokSemicolon, MUST_GOBBLE);

    modInstp = checked_malloc (sizeof (AST), "parseModuleInst");
    modInstp->tag                              = TagModuleInst;
    modInstp->u.moduleInstAttrs.moduleName     = modNamep;
    modInstp->u.moduleInstAttrs.params         = params;
    modInstp->u.moduleInstAttrs.moduleInstName = modInstNamep;
    modInstp->u.moduleInstAttrs.ports          = ports;

    return modInstp;
} /* parseModuleInst() */

/* ----------------------------------------------------------------
 * parseAssign()
 * Parses an 'assign <ide> = <expr>' stmt
 * Current token is just past the 'assign' keyword
 */

static
AST *
parseAssign (void)
{
    Token  *lhsp, *rhsp;
    AST    *assignp;

    if (tokType != TokWord) {
	fprintf (stderr, "***** Error: at lexical token: ");
	fprintCurTok (stderr);
	fprintf (stderr, "      Expecting lhs identifier of assign statement\n");
	fprintErrPos (stderr);
	exit (1);
    }
    lhsp = mkASTToken ();
    lexAnItem ();

    gobblePunctuation (TokEqual, MUST_GOBBLE);

    rhsp = parseExpr ();

    gobblePunctuation (TokSemicolon, MUST_GOBBLE);

    assignp = checked_malloc (sizeof (AST), "parseAssign");
    assignp->tag                          = TagAssignStmt;
    assignp->u.assignStmtAttrs.lhs        = lhsp;
    assignp->u.assignStmtAttrs.rhs        = rhsp;

    return assignp;
} /* parseAssign() */

/* ----------------------------------------------------------------
 * parseCaseMux()
 * Parses an case-multiplexor:
 *     always @ (<ide> or ... or <ids>)
 *     case (<ide X>)
 *       <int> : <ide Y0> = <expr E0>;
 *       ...
 *       <int> : <ide YN> = <expr EN>;
 *       default <ide YD> = <expr ED>;
 *     endcase
 * Current token is just past the 'always' keyword
 */

static
AST *
parseCaseMux (void)
{
    Token  *x, *exprp, *lhsp, *rhsp;
    List   *sensitivities, *rhss, *matches, *matchess;
    AST    *caseMuxp;

    gobblePunctuation (TokAt, MUST_GOBBLE);

    // sensitivity list
    gobblePunctuation (TokLParen, MUST_GOBBLE);
    sensitivities = NULL;
    while (TRUE) {
	if (tokType != TokWord) {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting identifier in 'always' sensitivity list\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
	x = mkASTToken ();
	lexAnItem ();
	sensitivities = listJoin (sensitivities,
				  list1 (x, "parseCaseMux/sensitivities"));

	if (gobbleKeyword (KW_OR, MAY_GOBBLE)) continue;
	else
	    break;
    }
    gobblePunctuation (TokRParen, MUST_GOBBLE);

    gobbleKeyword (KW_CASE, MUST_GOBBLE);

    // discriminating expression
    gobblePunctuation (TokLParen, MUST_GOBBLE);

    /*
    if (tokType != TokWord) {
	fprintf (stderr, "***** Error: at lexical token: ");
	fprintCurTok (stderr);
	fprintf (stderr, "      Expecting discriminating identifier of case statement\n");
	fprintErrPos (stderr);
	exit (1);
    }
    exprp = mkASTToken ();
    lexAnItem ();
    */

    exprp = parseExpr ();
    gobblePunctuation (TokRParen, MUST_GOBBLE);

    // case clauses
    lhsp = NULL;
    rhss = NULL;
    matchess = NULL;
    while (TRUE) {
	// matching integers, of 'default' keyword
	matches = NULL;
	if (tokType == TokInt) {
	    while (TRUE) {
		matches = listJoin (matches,
				    list1 (mkASTToken (), "parseCaseMux/matches"));
		lexAnItem ();
		if (gobblePunctuation (TokComma, MAY_GOBBLE))
		    continue;
		else
		    break;
	    }
	    gobblePunctuation (TokColon, MUST_GOBBLE);
	}
	else if (gobbleKeyword (KW_DEFAULT, MAY_GOBBLE)) {
	}
	else
	    break;

	matchess = listJoin (matchess,
			     list1 (matches, "parseCaseMux/matchess"));

	// Variable being assigned
	if (tokType != TokWord) {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting lhs identifier of a case arm\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
	// update lhsp only once (all should be same variable)
	if (lhsp == NULL)
	    lhsp = mkASTToken ();
	lexAnItem ();

	gobblePunctuation (TokEqual, MUST_GOBBLE);

	// RHS expression
	rhsp = parseExpr ();
	rhss = listJoin (rhss,
			 list1 (rhsp, "parseCaseMux/rhs"));

	gobblePunctuation (TokSemicolon, MUST_GOBBLE);
    }

    gobbleKeyword (KW_ENDCASE, MUST_GOBBLE);
    gobblePunctuation (TokSemicolon, MAY_GOBBLE);

    caseMuxp = checked_malloc (sizeof (AST), "parseCaseMux");
    caseMuxp->tag                          = TagCaseMux;
    caseMuxp->u.caseMuxAttrs.sensitivities = sensitivities;
    caseMuxp->u.caseMuxAttrs.expr          = exprp;
    caseMuxp->u.caseMuxAttrs.lhs           = lhsp;
    caseMuxp->u.caseMuxAttrs.matchess      = matchess;
    caseMuxp->u.caseMuxAttrs.rhss          = rhss;

    return caseMuxp;
} /* parseCaseMux() */

/* ----------------------------------------------------------------
 * parseModule()
 * parses a module-endmodule construct
 */

static
AST *
parseModule (void)
{
    AST    *modp;
    Token  *moduleNamep, *w, *declKw, *subModNamep;
    List   *ports, *stmts;

    lexAnItem ();

    // Module keyword
    gobbleKeyword (KW_MODULE, MUST_GOBBLE);

    // Module name
    if (tokType != TokWord) {
	fprintf (stderr, "***** Error: at lexical token: ");
	fprintCurTok (stderr);
	fprintf (stderr, "      Expecting a module name (identifier)\n");
	fprintErrPos (stderr);
	exit (1);
    }
    moduleNamep = mkASTToken ();
    lexAnItem ();

    // Module ports
    ports = NULL;
    gobblePunctuation (TokLParen, MUST_GOBBLE);
    while (TRUE) {
	if (tokType == TokWord) {
	    w = mkASTToken ();
	    lexAnItem ();
	    ports = listJoin (ports, list1 (w, "parseModule"));
	}
	if      (gobblePunctuation (TokComma,  MAY_GOBBLE)) continue;
	else if (gobblePunctuation (TokRParen, MAY_GOBBLE)) break;
	else {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting a comma or right paren in port list\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
    }
    gobblePunctuation (TokSemicolon, MUST_GOBBLE);

    // Module statements
    stmts = NULL;
    while (TRUE) {
	if (gobbleKeyword (KW_ENDMODULE, MAY_GOBBLE)) break;

	else if ((parseKeyword (& keywords [KW_INPUT], 1)  == KW_INPUT) ||
		 (parseKeyword (& keywords [KW_OUTPUT], 1) == KW_OUTPUT) ||
		 (parseKeyword (& keywords [KW_WIRE], 1)   == KW_WIRE) ||
		 (parseKeyword (& keywords [KW_REG], 1)    == KW_REG)) {
	    declKw = mkASTToken ();
	    lexAnItem();
	    stmts = listJoin (stmts,
			      list1 (parseDecl (declKw), "parseModule/decl"));
	}

	else if (gobbleKeyword (KW_ASSIGN, MAY_GOBBLE)) {
	    stmts = listJoin (stmts,
			      list1 (parseAssign (), "parseModule/assign"));
	}

	else if (gobbleKeyword (KW_ALWAYS, MAY_GOBBLE)) {
	    stmts = listJoin (stmts,
			      list1 (parseCaseMux (), "parseModule/case-mux"));
	}

	else if (tokType == TokWord) {
	    subModNamep = mkASTToken ();
	    lexAnItem ();
	    stmts = listJoin (stmts,
			      list1 (parseModuleInst (subModNamep), "parseModule/moduleInst"));
	}

	else {
	    fprintf (stderr, "***** Error: at lexical token: ");
	    fprintCurTok (stderr);
	    fprintf (stderr, "      Expecting a module's top-level statement\n");
	    fprintErrPos (stderr);
	    exit (1);
	}
    }

    modp = checked_malloc (sizeof (AST), "parseModule/result");
    modp->tag                                = TagModule;
    modp->u.moduleAttrs.moduleName           = moduleNamep;
    modp->u.moduleAttrs.ports                = ports;
    modp->u.moduleAttrs.stmts                = stmts;
    return modp;
} /* parseModule()  */

/* ----------------------------------------------------------------
 * parseFile()
 * This is the main external function called to parse an input file.
 */

AST *
parseFile (char *filename)
{
    AST *modp;

    initLexer (filename);

    if (setjmp (readerSetjmpEnv) == 0)
	modp = parseModule ();
    else
	modp = NULL;

    return modp;
} /* parseFile() */

/* ================================================================
 * Printing out ASTs
 */

/* ----------------------------------------------------------------
 * fprintASTDecl()
 * AST has TagDecl
 */

static
void
fprintASTDecl (FILE *fp, int indent, AST *astp)
{
    List   *lp;

    assert (astp->tag == TagDecl);

    fprint_indent (fp, indent);
    fprintf (fp, "%s ", astp->u.declAttrs.declKw->tokSVal);

    if (astp->u.declAttrs.upperBound != NULL) {
	fprintf (fp, "[");
	fprintToken (fp, astp->u.declAttrs.upperBound, 0);
	fprintf (fp, ":");
	fprintToken (fp, astp->u.declAttrs.lowerBound, 0);
	fprintf (fp, "] ");
    }

    lp = astp->u.declAttrs.ides;
    while (lp != NULL) {
	fprintToken (fp, lp->head, 0);
	lp = lp->tail;
	if (lp != NULL)
	    fprintf (fp, ",");
    }
    fprintf (fp, ";\n");
} /* fprintASTDecl() */

/* ----------------------------------------------------------------
 * fprintASTModuleInst()
 * AST has TagDecl
 */

static
void
fprintASTModuleInst (FILE *fp, int indent, AST *astp)
{
    List *xs, *ys;

    assert (astp->tag == TagModuleInst);

    fprint_indent (fp, indent);

    fprintf (fp, "%s ", astp->u.moduleInstAttrs.moduleName->tokSVal);

    // parameters
    xs = astp->u.moduleInstAttrs.params;
    if (xs != NULL) {
	fprintf (fp, "#(");
	while (xs != NULL) {
	    ys = xs->head;
	    if (ys->head != NULL) {
		fprintf (fp, ".");
		fprintToken (fp, ys->head, 0);    // formal name
		fprintf (fp, "(");
	    }
	    fprintToken (fp, ys->tail->head, 0);  // actual val
	    if (ys->head != NULL)
		fprintf (fp, ")");

	    xs = xs->tail;
	    if (xs != NULL) fprintf (fp, ",");
	}
	fprintf (fp, ") ");
    }

    // instance name
    fprintf (fp, "%s ", astp->u.moduleInstAttrs.moduleInstName->tokSVal);

    // ports
    fprintf (fp, "(");
    xs = astp->u.moduleInstAttrs.ports;
    while (xs != NULL) {
	ys = xs->head;
	fprintf (fp, ".");
	fprintToken (fp, ys->head, 0);        // formal name
	fprintf (fp, "(");
	if (ys->tail->head != NULL)
	    fprintToken (fp, ys->tail->head, 0);  // actual val
	fprintf (fp, ")");

	xs = xs->tail;
	if (xs != NULL) fprintf (fp, ",");
    }
    fprintf (fp, ");\n");
} /* fprintASTModuleInst() */

/* ----------------------------------------------------------------
 * fprintASTAssignStmt()
 * AST has TagAssignStmt
 */

static
void
fprintASTAssignStmt (FILE *fp, int indent, AST *astp)
{
    assert (astp->tag == TagAssignStmt);

    fprint_indent (fp, indent);
    fprintf (fp, "assign ");

    fprintToken (fp, astp->u.assignStmtAttrs.lhs, 0);

    fprintf (fp, " = ");

    fprintToken (fp, astp->u.assignStmtAttrs.rhs, 0);

    fprintf (fp, ";\n");
} /* fprintASTAssignStmt() */

/* ----------------------------------------------------------------
 * fprintASTCaseMux()
 * AST has TagCaseMux
 */

static
void
fprintASTCaseMux (FILE *fp, int indent, AST *astp)
{
    int     n;
    List   *xs, *xss, *ys;
    Token  *xp;

    assert (astp->tag == TagCaseMux);

    fprint_indent (fp, indent);
    fprintf (fp, "always @ ");

    // sensitivity list
    fprintf (fp, "(");
    xs = astp->u.caseMuxAttrs.sensitivities;
    while (xs != NULL) {
	fprintToken (fp, xs->head, 0);
	xs = xs->tail;
	if (xs != NULL) fprintf (fp, " or ");
    }
    fprintf (fp, ")\n");

    // 'case' and discriminating expr
    fprint_indent (fp, indent);
    fprintf (fp, "case (");
    fprintToken (fp, astp->u.caseMuxAttrs.expr, 0);
    fprintf (fp, ")\n");

    // clauses, including 'default' clause if any
    ys = astp->u.caseMuxAttrs.rhss;
    for (xss = astp->u.caseMuxAttrs.matchess; xss != NULL; xss = xss->tail) {
	fprint_indent (fp, indent + 2);

	if (xss->head == NULL) {
	    fprintf (fp, "default ");
	}
	else {
	    // for each matching integer
	    for (xs = xss->head; xs != NULL; xs = xs->tail) {
		xp = xs->head;
		assert (xp->tokType == TokInt);
		n = xp->tokIVal;
		fprintf (fp, "%d", n);
		if (xs->tail != NULL) fprintf (fp, ",");
	    }
	    fprintf (fp, ": ");
	}

	fprintToken (fp, astp->u.caseMuxAttrs.lhs, 0);
	fprintf (fp, " = ");
	fprintToken (fp, ys->head, 0);
	fprintf (fp, ";\n");

	ys = ys->tail;
    }
    assert (ys == NULL);

    fprint_indent (fp, indent);
    fprintf (fp, "endcase\n");
} /* fprintASTCaseMux() */

/* ----------------------------------------------------------------
 * fprintASTModule()
 * AST has TagModule
 */

static
void
fprintASTModule (FILE *fp, int indent, AST *astp)
{
    int     j;
    List   *lp;

    assert (astp->tag == TagModule);

    fprint_indent (fp, indent);
    fprintf (fp, "module %s (\n", astp->u.moduleAttrs.moduleName->tokSVal);

    j = indent + strlen ("module  (") + strlen (astp->u.moduleAttrs.moduleName->tokSVal);

    lp = astp->u.moduleAttrs.ports;
    while (lp != NULL) {
	fprint_indent (fp, j);
	fprintToken (fp, lp->head, 0);
	lp = lp->tail;
	if (lp != NULL) fprintf (fp, ",\n");
	else            fprintf (fp, ");\n");
    }

    lp = astp->u.moduleAttrs.stmts;
    while (lp != NULL) {
	fprintAST (fp, indent + 4, lp->head);
	lp = lp->tail;
    }

    fprint_indent (fp, indent);
    fprintf (fp, "endmodule\n");
} /* fprintASTModule() */

/* ----------------------------------------------------------------
 * fprintAST()
 * For debugging: prints out an AST
 */

void
fprintAST (FILE *fp, int indent, AST *astp)
{
    /* ---- Module */
    if (astp->tag == TagModule) {
	fprintASTModule (fp, indent, astp);
    }

    /* ---- Decl */
    else if (astp->tag == TagDecl) {
	fprintASTDecl (fp, indent, astp);
    }

    /* ---- Module instantiation */
    else if (astp->tag == TagModuleInst) {
	fprintASTModuleInst (fp, indent, astp);
    }

    /* ---- Assign */
    else if (astp->tag == TagAssignStmt) {
	fprintASTAssignStmt (fp, indent, astp);
    }

    /* ---- Case mux */
    else if (astp->tag == TagCaseMux) {
	fprintASTCaseMux (fp, indent, astp);
    }

    else {
	fprintf (stderr, "Error (fprintAST) Internal error, unknown AST tag: %d\n", astp->tag);
    }
} /* fprintAST() */

/* ****************************************************************
 * testParser()
 * For debugging: just parses the file and prints out the AST
 */

void
testParser (char *filename)
{
    AST  *modp;

    modp = parseFile (filename);

    if (modp != NULL)
	fprintAST (stdout, 0, modp);
} /* testParser() */

/* **************************************************************** */
