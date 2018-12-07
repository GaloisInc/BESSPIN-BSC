#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "TclparserX.h"
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
  pc_ = code_;
  linestart_ = lineno_ = start_lineno;
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
  Tstring token;
  
  if(*pc_ == '\0') {
    token = geteof();
  }

  // test for end of statement
  else if(eos_) {
    eos_ = FALSE;
    token = geteos();
  }
  else {
    token = begin();
  }
  
  if(getenv("DEBUG") != 0) {
    printf("line: %d token: '%s'\n", getlineno(), token.data());
  }
  
  return token;
}

Tstring
Tclparser::begin()
{
  char c;
  Tstring ec;
  Tstring token;
  
  while(1) {
    c = *pc_++;
    
    if(c == '\0') {
      return geteof();
    }
      
    switch(c) {
      case ' ':
      case '\t' :
        // eat leading whitespace
        continue;
        
      case '\n':
        // eat leading newlines, after bumping line counter
        lineno_++;
        
        // fall thru
        
      case ';':
        return geteos();
        
      case '\\':
        ec = escape();
        if(ec == " " || ec == "\\\n") {
          // whitespace
          continue;
        }
        else {
          linestart_ = lineno_;
          return begin2(ec);
        }
        break;
        
      case '#':
        linestart_ = lineno_;
        return comment();
        
      default:
        linestart_ = lineno_;
        token = "";
        token += c;
        return begin2(token);
    }
  }
}

Tstring
Tclparser::begin2(const Tstring& token)
{
  char c;
  Tstring es;
  Tstring tok;
  Tstring t(token);
  
  c = *(t.data());
  
  while(1) {
    switch(c) {
      case ' ':
      case '\t':
        // token delimiter
        return tok;
        break;
      
      case ';':
        eos_ = TRUE;
        return tok;
        break;
        
      case '\n':
        // statement terminator
        lineno_++;
        eos_ = TRUE;
        return tok;
        break;
        
      case '\x7b':
        tok += brace();
        break;
      
      case '\x5b':
        tok += bracket();
        break;
        
      case '\"':
        // a quote can appear in the middle of text and not have to have
        // an ending quote.
        if(tok.length() == 0)
          tok += quote();
        else
          tok += c;
          
        break;
        
      case '\\':
        if(t.length() > 1) {
          tok += t;
          t = "";
        }
        else {
          es = escape();
          if(es == " " || es == "\\\n") {
            // this is a whitespace token terminator
            return tok;
          }
          else {
            tok += es;
            break;
          }
        }
        break;
        
      default:
        tok += t;
        break;
    }

    if(*pc_ == '\0')
      return tok;
    else {
      c = *pc_++;
      t = "";
      t += c;
    }
  }
}

Tstring
Tclparser::brace()
{
  Tstring token("\x7b");
  
  char c;
  
  while(1) {
    c = *pc_++;
    if(c == '\0') {
      char msg[50];
      sprintf(msg, "premature EOF while inside brace block around line %d", lineno_);
      throw msg;
    }

    switch(c) {
      case '\x7d':
        token += c;
        return token;
        break;
        
      case '\x7b':
        token += brace();
        break;
        
      case '\n':
        lineno_++;
        token += c;
        break;
        
      case '\\':
        token += escape();
        break;
        
      default:
        token += c;
        break;
    }
  }
}

Tstring
Tclparser::quote()
{
  Tstring token("\"");
  char c;
  
  while(1) {
    c = *pc_++;
    if(c == '\0')
      throw "premature EOF while inside quote";
      
    switch(c) {
      case '"':
        token += c;
        return token;
        break;
        
      case '\x5b':
        token += bracket();
        break;
        
      case '\n':
        lineno_++;
        token += c;
        break;
        
      case '\\':
        token += escape();
        break;
        
      default:
        token += c;
        break;
    }
  }
}

Tstring
Tclparser::bracket()
{
  Tstring token("\x5b");
  char c;
  
  while(1) {
    c = *pc_++;
    if(c == '\0')
      throw "premature EOF while inside bracket";
      
    switch(c) {
      case '\x5d':
        token += c;
        return token;
        break;
        
      case '\"':
        if(isspace(*(pc_-2)))
          token += quote();
        else
          token += c;
        break;
        
      case '\x5b':
        token += bracket();
        break;
        
      case '\x7b':
        token += brace();
        break;
        
      case '\n':
        lineno_++;
        token += c;
        break;
        
      case '\\':
        token += escape();
        break;
        
      default:
        token += c;
        break;
    }
  }
}

Tstring
Tclparser::comment()
{
  Tstring token("#");
  char c;
  
  while(1) {
    if (*pc_ == '\0')
      return token;
    else
      c = *pc_++;
      
    switch(c) {
      case '\n':
        eos_ = TRUE;
        lineno_++;
        return token;
        break;
        
      case '\\':
        token += escape();
        break;
        
      default:
        token += c;
        break;
    }
  }
}

Tstring
Tclparser::escape()
{
  char c;
  Tstring token;
  
  if (*pc_ == '\0')
    return token;
  else
    c = *pc_++;

  switch(c) {
    case '\x7f':
      // skip over this, it is our encoded escape newline
      pc_++;

      // fall thru

    case '\n':
      lineno_++;
      token = "\\\n";
      break;

    default:
      token += '\\';
      token += c;
        break;
  }
  
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

