include "Tstring.h"
include "TclparserX.h"
include <stdlib.h>

package tclparser 0.1

class Tclparser {
  constructor { {char* code} {int start_line} }
  destructor
  method Tstring gettok {}
  method int getlineno {}
  method char getBlockStart {}
  method char getBlockEnd {}
}

function char* geteos {}
function char* geteof {}
