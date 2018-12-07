
#include "tcl.h"
#include "TclUtils.h"
#include <string.h>
#include <algorithm>

// Class for conversion from C++ objects to tcl structure
template <class T>
class ToTclObj {
protected:
  struct Tcl_Interp *m_interp;
  struct Tcl_Obj    *m_list;

public:
  ToTclObj (struct Tcl_Interp *interp)
    : m_interp(interp)
    , m_list(0)
  {
    m_list = Tcl_NewListObj (0,0);
  }

  // used by for_each call below and the purpose for this class
  void operator() (const T &s) {
    Tcl_Obj *o = toTclObj(m_interp, s);
    Tcl_ListObjAppendElement (m_interp, m_list, o);
  }
  struct Tcl_Obj * getResultList () const {
    return m_list;
  }
};


Tcl_Obj * toTclObj (Tcl_Interp *interp, const int i){
  Tcl_Obj *o = Tcl_NewIntObj (i);
  return o;
}
Tcl_Obj * toTclObj (Tcl_Interp *interp, const char *str){
  Tcl_Obj *o = Tcl_NewStringObj (str, strlen(str));
  return o;
}
Tcl_Obj * toTclObj (Tcl_Interp *interp, const BString &s){
  Tcl_Obj *o = Tcl_NewStringObj (s.c_str(), s.size());
  return o;
}
Tcl_Obj * toTclObj (Tcl_Interp *interp, const BStringList &sl){
  ToTclObj<BString> cvt(interp);
  cvt = for_each (sl.begin(), sl.end(), cvt);
  return cvt.getResultList() ;
}
Tcl_Obj * toTclObj (Tcl_Interp *interp, const void *v){
  Tcl_Obj *o = Tcl_NewLongObj ((long) v);
  return o;
}
Tcl_Obj * toTclObj (Tcl_Interp *interp, const std::pair<BString,BString> sp) {
  Tcl_Obj * l = Tcl_NewListObj (0,0);
  Tcl_ListObjAppendElement (interp, l, toTclObj(interp, sp.first));
  Tcl_ListObjAppendElement (interp, l, toTclObj(interp, sp.second));
  return l;
}


Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const Parameter &p)
{
  return Tcl_ObjPrintf("%s=%s", p.m_name.c_str(), p.m_value.c_str());
}
Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const ModuleTerminal &m) 
{
  return Tcl_ObjPrintf(".%s(%s)", m.m_portName.c_str(), m.m_netName.c_str());
}
Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const ParameterList &sl)
{
  ToTclObj<Parameter> cvt(interp);
  cvt = for_each (sl.begin(), sl.end(), cvt);
  return cvt.getResultList() ;
};
Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const ModuleTerminalList &sl) 
{
  ToTclObj<ModuleTerminal> cvt(interp);
  cvt = for_each (sl.begin(), sl.end(), cvt);
  return cvt.getResultList() ;
}

// Pushed Port
Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const PushedPort &p) 
{
  Tcl_Obj * l = Tcl_NewListObj (0,0);
  Tcl_ListObjAppendElement (interp, l, toTclObj(interp, p.m_portName));
  Tcl_ListObjAppendElement (interp, l, toTclObj(interp, p.m_uniqPortName));
  Tcl_ListObjAppendElement (interp, l, toTclObj(interp, p.m_broadcast));
  Tcl_ListObjAppendElement (interp, l, toTclObj(interp, p.m_width));
  return l;
}
Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const AddedPortList &sl) 
{
  ToTclObj<PushedPort> cvt(interp);
  cvt = for_each (sl.begin(), sl.end(), cvt);
  return cvt.getResultList() ;
}


Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const std::list< std::pair<BString, BString> > &sl) 
{
  ToTclObj<std::pair<BString, BString> > cvt(interp);
  cvt = for_each (sl.begin(), sl.end(), cvt);
  return cvt.getResultList() ;
}

// A nested list containing upto these 3 elements...
Tcl_Obj * toTclList (struct Tcl_Interp *interp, struct Tcl_Obj *o1, struct Tcl_Obj *o2, struct Tcl_Obj *o3) {
  Tcl_Obj *l = Tcl_NewListObj (0,0);
  if (o1) {Tcl_ListObjAppendElement (interp, l, o1);}
  if (o2) {Tcl_ListObjAppendElement (interp, l, o2);}
  if (o3) {Tcl_ListObjAppendElement (interp, l, o3);}
  return l;
}


// Conversion to TCL results
void toTclResult (Tcl_Interp *interp, const char *str) {
  Tcl_Obj *r = Tcl_NewStringObj (str, strlen(str) );
  Tcl_SetObjResult (interp, r);
}
void toTclResult (Tcl_Interp *interp, const BString &str) {
  Tcl_Obj *r = toTclObj(interp, str);
  Tcl_SetObjResult (interp, r);
}
void toTclResult (Tcl_Interp *interp, const BStringList &sl) {
  Tcl_Obj *r = toTclObj (interp, sl);
  Tcl_SetObjResult (interp, r);
}
void toTclResult (Tcl_Interp *interp, const int i) {
  Tcl_Obj *r = Tcl_NewIntObj (i);
  Tcl_SetObjResult (interp, r);
}
void toTclResult (Tcl_Interp *interp, const void *v) {
  Tcl_Obj *r = toTclObj (interp, v);
  Tcl_SetObjResult (interp, r);
}
void toTclResult (Tcl_Interp *interp, Tcl_Obj *o) {
  Tcl_SetObjResult (interp, o);
}

// These functions treat the result as a list.
void appendTclResult (Tcl_Interp *interp, const char *str) {
  Tcl_Obj *r = Tcl_NewStringObj (str, strlen(str) );
  Tcl_Obj *res = Tcl_GetObjResult(interp);  
  Tcl_ListObjAppendElement(interp, res, r);
}
void appendTclResult (Tcl_Interp *interp, const BString &str) {
  Tcl_Obj *r = toTclObj(interp, str);
  Tcl_Obj *res = Tcl_GetObjResult(interp);
  Tcl_ListObjAppendElement(interp, res, r);
}
void appendTclResult (struct Tcl_Interp *interp, Tcl_Obj *o) {
  Tcl_Obj *res = Tcl_GetObjResult(interp);
  Tcl_ListObjAppendElement(interp, res, o);
}

void appendTclResultError (struct Tcl_Interp *interp, const BString &str) {
  Tcl_Obj *res = Tcl_GetObjResult(interp);
  Tcl_AppendStringsToObj(res, " ", str.c_str(), (char*)NULL);
}
void appendTclResultError (struct Tcl_Interp *interp, const char * str) {
  Tcl_Obj *res = Tcl_GetObjResult(interp);
  Tcl_AppendStringsToObj(res, " ", str, (char*)NULL);
}



void dumpArguments (Tcl_Interp *interp, const cmd_struct cmds[], const char * given )
{
  Tcl_AppendResult(interp, "wrong argument should be one of:\n",  (char *) NULL);
  for (const cmd_struct *p = & cmds[0];  p->cname ; ++p) {
    Tcl_AppendResult(interp, "  ", given, " ",
                     p->cname, " ", p->help, "\n", (char *) NULL);
  }
}
void dumpArguments (Tcl_Interp *interp, const cmd_struct_funptr cmds[], const char * given )
{
  Tcl_AppendResult(interp, "wrong arguments: should be one of:\n",  (char *) NULL);
  for (const cmd_struct_funptr *p = & cmds[0];  p->cname ; ++p) {
    Tcl_AppendResult(interp, "  ", given, " ",
                     p->cname, " ", p->help, "\n", (char *) NULL);
  }
}

int getInts (struct Tcl_Interp *interp, Tcl_Obj *obj, IntList & intlist)
{
  int lobjc, newval;
  Tcl_Obj **lobjv;
  int stat = Tcl_ListObjGetElements (interp, obj, &lobjc, &lobjv);
  for (int i = 0; stat == TCL_OK && i < lobjc; ++i ) {

    stat = Tcl_GetIntFromObj (interp, lobjv[i], &newval);
    if (stat == TCL_OK) {
      intlist.push_back (newval);
    }
  }
  return stat;
}


int getInts (struct Tcl_Interp *interp, Tcl_Obj *obj, IntSet & intset)
{
  int lobjc, newval;
  Tcl_Obj **lobjv;
  int stat = Tcl_ListObjGetElements (interp, obj, &lobjc, &lobjv);
  for (int i = 0; stat == TCL_OK && i < lobjc; ++i ) {

    stat = Tcl_GetIntFromObj (interp, lobjv[i], &newval);
    if (stat == TCL_OK) {
      intset.insert (newval);
    }
  }
  return stat;
}

// Get a string or a stringList
int getString (struct Tcl_Interp *interp, Tcl_Obj *obj, BString &stringout )
{
  char *s = Tcl_GetStringFromObj (obj, NULL);
  stringout = s;
  return TCL_OK;
}
int getStringList (struct Tcl_Interp *interp, Tcl_Obj *obj, BStringList &stringout )
{
  int lobjc;
  Tcl_Obj **lobjv;
  int stat = Tcl_ListObjGetElements (interp, obj, &lobjc, &lobjv);
  for (int i = 0; stat == TCL_OK && i < lobjc; ++i ) {
    BString s;
    stat = getString (interp, lobjv[i], s);
    if (stat == TCL_OK) {
      stringout.push_back (s);
    }
  }
  return stat;
}
