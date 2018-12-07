#include <string.h>
#include <stdlib.h>
#include "Tclparser.h"
#include "Tstring.h"


Tclparser::Tclparser(const char* code, int start_lineno)
{
  switch(*code) {
    case '{':
    case '[':
    case '"':
      // set the variables
      bs_ = *code;
      be_ = code[strlen(code)-1];
      
      // save only the code inside the block
      code_ = strdup(code+1);
      code_[strlen(code_)-1] = '\0';
      break;

    default:
      bs_ = ' ';
      be_ = ' ';
      code_ = strdup(code);
      break;
  }
  pc_ = 0;
  lineno_ = start_lineno;
  eos_ = FALSE;
}

Tclparser::~Tclparser()
{
  free(code_);
}

int
Tclparser::getlineno() const
{
  return linestart_;
}

char
Tclparser::getBlockStart() const
{
  return bs_;
}

char 
Tclparser::getBlockEnd() const
{
  return be_;
}

Tstring
Tclparser::gettok()
{ 
 // state machine variables
  int inbrace = 0;
  int inbrack = 0;
  BOOL eatws = TRUE;
  BOOL inescape = FALSE;

  int brackcontext[100];
  int inquote = 0;

  Tstring token;
  Tstring eof = geteof();
  Tstring eos = geteos();
  delimiters tokdel = unset;
  
  char c;
  
  if(pc_ == 0)
    pc_ = code_;
  else if(*pc_ == '\0') {
    return eof;
  }
  else if(!eos_)
    pc_++;

  // test for end of statement
  if(eos_) {
    eos_ = FALSE;
    return eos;
  }
  
  linestart_ = lineno_;
  
  // now keep reading characters until we find something interesting
  for(;*pc_ != '\0';pc_++) {
    // swallow any leading whitespace
    if(eatws && (*pc_ == ' ' || *pc_ == '\t'))
      continue;
    
    c = *pc_;

    // special handling for encoded escaped newlines
    if(c == '\x7f')
      continue;
    
    if(inescape) {
      inescape = FALSE;
      if(*pc_ == '\n') {
        lineno_++;
        
        if(eatws) {
          tokdel = unset;
          linestart_ = lineno_;
          continue;
        }
        
        // if we're waiting for a space, this is it
        if(tokdel == ws)
          return token;
          
        // if we haven't specified a token yet, then ignore the newline
        // and eat leading whitespace
        if(tokdel == unset) {
          eatws = TRUE;
          linestart_ = lineno_;
          
          // reset the token delimiter
          tokdel = unset;
          
          // fix for defect #13; need to turn these things into spaces
          token += " ";
        } else {
          token += "\\";
          token += *pc_;
        }
      } else {
        token += "\\";
        token += *pc_;
        eatws = FALSE;
      }
      continue;
    }
    
    if(*pc_ == '\\') {
      inescape = TRUE;
      // if we haven't set the token delimiter yet, then it is a whitespace
      if(tokdel == unset) { tokdel = ws; }
      continue;
    }
    
    eatws = FALSE;
    
    if(tokdel == nl && *pc_ != '\n') {
      token += *pc_;
      continue;
    }
    
    switch(*pc_) {
      case '#':
        if(token.length() == 0) {
          // only a newline (nonescaped) can delimit a comment
          tokdel = nl;
        }
        token += *pc_;
        break;
        
       case '\n':
          lineno_++;
          // fall thru
       case ';':
          // if there is no token delimiter set yet, then this is an EOS
          if(tokdel == unset) {
            return eos;
          }
          
          // if whitespace is a token delimiter, then we've reached the end
          // of a token
          if(tokdel == ws || (tokdel == nl && c == '\n')) {
            if(token.length()) {
              eos_ = TRUE;
              return token;
            } else {
              return eos;
            }
          } else {
            token += *pc_;
          }
          
          break;
          
        case '\t':
        case ' ':
          // if whitespace is a token delimiter, then we've reached the end
          // of a token
          if(tokdel == ws) {
            return token;
          } else {
            token += c;
          }
          
          break;
          
        case '"':
          token += '"';
		  inquote = 1 - inquote;
          if(tokdel == unset) {
            tokdel = qt;
          } 
          else if(tokdel == qt) {
            return token;
          }
          break;
          
        case '{':
          token += '{';
          if(tokdel == unset)
            tokdel = br;
          if(tokdel == br)
            inbrace++;
          break;

        case '}':
          token += '}';
          if(tokdel == unset)
            tokdel = ws;
            
          if(tokdel == br) {
            inbrace--;
            if(inbrace == 0)
              return token;
          }
          break;

        case '[':
          token += '[';
          if(tokdel == unset || tokdel == ws) 
            tokdel = sq;
            
          if(tokdel != br) {
            inbrack++;
			brackcontext[inbrack] = inquote;
		  }
          break;

        case ']':
          token += ']';
          if(tokdel == unset)
            tokdel = ws;
            
          if(tokdel != br) {
		    if(brackcontext[inbrack] == inquote)
              inbrack--;
	      if(inbrack < 0)
		inbrack = 0;
          }

          // if we're terminating the bracket, reset the delimiter back to ws
          if(tokdel == sq && inbrack == 0)
            tokdel = ws;
            
          break;

        default: 
          token += *pc_;
          if(tokdel == unset)
            tokdel = ws;
          break;
    }
  }
  
  // if no token was created, return eof
  if(token.length() == 0)
    return eof;
  else
    return token;
}

extern "C" {
  char* geteos()
  {
    return "\x01EOS\x01";
  }

  char* geteof()
  {
    return "\x01EOF\x01";
  }
}

