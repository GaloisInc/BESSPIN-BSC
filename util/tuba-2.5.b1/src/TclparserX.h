/* TclparserX.h
 * 
 * aug 98
 * 
 */

class Tstring;

enum BOOL {
  FALSE = 0,
  TRUE = 1
};

extern "C" {
  extern char* geteos();
  extern char* geteof();
}

class Tclparser {
 public:
   
  Tclparser(const char* code, int start_lineno);
  /* This constructor is for parsing blocks of code that are surrounded 
     with either braces {}, quotes "", or brackets [] or nothing. 
     The constructor 
     will remove any blocking characters before parsing and store them
     Use <code>getBlockStart()</code> and <code>getBlockEnd()</code> to 
     retrieve these characeters.
   */
   
  ~Tclparser();
  /* frees up resources used by the parser object */
  
  Tstring gettok();
  /* Returns the next token in the code. Code and text inside {} and ""
     and [] are all considered one token.
     <p>
     Two special tokens: <code>EOS</code> and <code>EOF</code>.
     <br>
     <code>EOS</code> is the <em>end-of-statement</em> token, which terminates 
     a single Tcl statement.
     <br>
     <code>EOF</code> is the <em>end-of-file</em> token, signifying that 
     there are no more tokens left to read in the parser object.
   */

  char getBlockStart() const;
  /* return the character that started the code block. This will either
     be a brace {, quote ", bracket [, or a blank signifying that the
     code was not a block.
  */

  char getBlockEnd() const;
  /* return the character that ended the code block. This will either
     be a brace }, quote ", bracket ], or a blank signifying that the
     code was not a block.
  */
  
  int getlineno() const;
  /* Returns the line number that the current token started on. Note that
     a token may span multiple lines.
   */
   
 protected:

 private:
  
  Tstring begin();
  Tstring begin2(const Tstring& tok);
  Tstring brace();
  Tstring bracket();
  Tstring quote();
  Tstring comment();
  Tstring escape();
  
  char* code_;
  char* pc_;
  int lineno_;
  int linestart_;
  BOOL eos_;
  char bs_;
  char be_;
};

