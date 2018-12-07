#include "Types.h"
#include <iostream>
#include "string.h"
#include <list>
#include <functional>
#include <algorithm>

using namespace std;


// returns a lower case version of a string 
string tolower (const string & s)
{
  string d (s);

  transform (d.begin (), d.end (), d.begin (), (int(*)(int)) tolower);
  return d;
}  // end of tolower

// returns an upper case version of a string 
string toupper (const string & s)
{
  string d (s);

  transform (d.begin (), d.end (), d.begin (), (int(*)(int)) toupper);
  return d;
}   // end of toupper


void removeAllSpaces(std::string &str)
{
    std::string temp;
    for (unsigned int i = 0; i < str.length(); i++)
      if (str[i] != ' ') temp += str[i];
    str = temp;
}

const BString getSignalName(const netHandle h){
  BString sig;
  size_t colon;
  colon = h.find_first_of(':');
  if (colon)
    sig = h.substr(0, colon);
  else
    sig = h;

  return sig;
}

void qualifySpecialChar(BString &h){
  BString sig;
  size_t spc;
  sig = h;
  spc = sig.find_first_of('$');
  while (spc != BString::npos) {
    sig = sig.replace(spc, 1, "\\$");
    spc = sig.find_first_of('$', spc+2);
  }
  
  h = sig;
}

const BString getWidthString(const netHandle h){
  BString sig;
  size_t colon;
  colon = h.find_first_of(':');
  if (colon)
    sig = h.substr(colon+1);
  else
    sig = "";

  printf("getWidthString %s %d\n", sig.c_str(), (int)colon);
  return sig;
}

// Builds a string from stringlist, 
// eg. [foo, bar, baz] --> {foo, bar, baz}
//     [] -> {}
void buildConcatString (const BStringList &sigs, BString &concat)
{
  unsigned int sz = sigs.size();
  if (sz == 0) {
    concat = "{}";
  }
  else if (sz == 1) {
    concat = sigs.front();
  }
  else {
    BStringList::const_iterator iter = sigs.begin();
    concat = "{" ;
    concat += *iter;
    for (++iter ; iter != sigs.end(); ++iter) {
      concat += ", " + *iter;
    }
    concat += "}" ;
  }
}

void splitInstanceName (const BString &full, BString & up, BString & thisinst)
{
  BString::size_type delim = full.rfind ("/");
  if (delim == BString::npos) {
    up = "";
    thisinst = full ;
  } else {
    up = full.substr(0,delim);
    thisinst = full.substr(delim+1);
  }
}

const char * getStrFromDir ( const DirectionE d )
{
  switch (d) {
  case d_input: return "input";
  case d_output: return "output";
  case d_inout: return "inout";
  }
  return "UNKNOWN_DIRECTION";
}

char * itoa (int i)
{
  char str_val [32] ;
  snprintf (str_val, sizeof (str_val), "%d", i) ;

  /* Use strdup to duplicate str_val which is currently on the stack. */
  return strdup (str_val) ;
}

const AddedPortList& getPushedPort(const AddedPortList & ports, const BString & label)
{

  AddedPortList* matches = new AddedPortList();
  for (AddedPortList::const_iterator iter = ports.begin(); iter != ports.end(); ++ iter) {
    const PushedPort &prt = *iter;
    if (0 == prt.m_portName.compare(label)) {
      matches->push_back(prt);
    }
  }
  return *matches;
}

std::string ToString(CosimFlavor flavor)
{

  std::string label;

  if (flavor == Observe) {
    label = "Observe";
  } else if (flavor == Replace) {
    label = "Replace";
  } else {
    label = "Undefined";
  }
  return label;

}


// can't overload based on output type so:
void FromString(CosimFlavor &flavor, std::string label)
{
  if (toupper(label) == "OBSERVE") {
    flavor = Observe;
  } else if (toupper(label) == "REPLACE") {
    flavor = Replace;
  }
}

