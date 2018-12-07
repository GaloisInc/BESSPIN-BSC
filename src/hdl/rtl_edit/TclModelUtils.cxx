#include "TclModelUtils.h"
#include <iostream>

#include "HdlUtils.h"
#include "TclUtils.h"

using namespace std;

// Common code interfacing Verific and Tcl
VeriModule *tc_findModuleTcl (Tcl_Interp *interp, VeriModule *from, const BString &path, BString & foundpath)
{
  VeriModule *found = HdlUtils::findModuleFromPath(from, path, foundpath);
  if (found == 0 && !path.empty()) {
    BString errMsg;
    errMsg = "No object found with name '" + path + "'";
    appendTclResultError (interp, errMsg);
  }
  return found;
}

VeriModule *tc_findModuleByNameTcl (Tcl_Interp *interp, const BString &name)
{
  VeriModule *module = HdlUtils::findModuleByName(name.c_str());
  if (module == 0 && !name.empty()) {
    BString errMsg;
    errMsg = "No module found with name '" + name + "'";
    appendTclResultError (interp, errMsg);
  }
  return module;
}

// Check that the name is simple -- no spaces
int tc_extractFileName (Tcl_Interp *interp, Tcl_Obj * o, BString & n)
{
  BString errMsg;
  int ret = TCL_ERROR;
  char *s = Tcl_GetStringFromObj (o, NULL);
  n = s;
  char *ps = s;
  while (*ps != 0) {
    char x = *ps; ps++;
    if (isspace(x) || iscntrl(x)) {
      ret = TCL_ERROR;
      break;
    } else {
      ret = TCL_OK;
    }
  }

  std::ifstream file;
  file.open(n.c_str());
  if (file.fail()) {
    errMsg = "File " + n + " not found.";
    appendTclResultError (interp, errMsg);
    return TCL_ERROR;
  }
  else {
    file.close();
    ret = TCL_OK;
  }
  return ret;
}

// Check that the name is simple -- no spaces
int tc_extractSimpleName (Tcl_Obj * o, BString & n)
{
  int ret = TCL_ERROR;
  char *s = Tcl_GetStringFromObj (o, NULL);
  n = s;
  char *ps = s;
  while (*ps != 0) {
    char x = *ps; ps++;
    if (isspace(x) || iscntrl(x)) {
      ret = TCL_ERROR;
      break;
    } else {
      ret = TCL_OK;
    }
  }
  return ret;
}

// Check that the token is a simple number
int tc_extractSimpleNumber (Tcl_Obj * o, int & n)
{
  int ret = Tcl_GetIntFromObj (NULL, o, &n);
  return ret;
}


// Check that the token is a simple number
int tc_extractSimplePositiveNumber (Tcl_Obj * o, int & n)
{
  int ret = Tcl_GetIntFromObj (NULL, o, &n);

  if (ret == TCL_OK)
    if (n < 0)
      return TCL_ERROR;

  return ret;
}


// Check that the tcl object is an instance and that the instance exists
// name returned in instName
int tc_extractInstance (Tcl_Interp *interp, Tcl_Obj * o, instHandle & instName)
{
  int ret = TCL_OK;
  char *s = Tcl_GetStringFromObj (o, NULL);
  BString errMsg, found;
  VeriModule *module = tc_findModuleTcl(interp, NULL, s, found);
  if (module == 0) {
    errMsg = "while looking for module '" ;
    errMsg += + s ;
    errMsg += "'." ;
    appendTclResultError (interp, errMsg);
    ret = TCL_ERROR;
  }
  instName = found;
  return ret;
}

// Lookup for signal in *o at instance inst
int tc_extractSignal (Tcl_Interp *interp, Tcl_Obj * o, BString &sig)
{
  int ret = TCL_OK;
  char *s = Tcl_GetStringFromObj (o, NULL);
  BString errMsg, found;
  VeriModule *module = tc_findModuleTcl(interp, NULL, s, found);
  if (module == 0) {
    errMsg = "while looking for module '" ;
    errMsg += + s ;
    errMsg += "'." ;
    appendTclResultError (interp, errMsg);
    ret = TCL_ERROR;
  } else {
    VeriIdDef *net = HdlUtils::findSignalInModuleByName(module, sig);
    if (net == 0) {
      errMsg = "while looking for net '" ;
      errMsg += + sig.c_str() ;
      errMsg += " in module ";
      errMsg += + s ;
      errMsg += "'." ;
      appendTclResultError (interp, errMsg);
      ret = TCL_ERROR;
    }
  }
  return ret;
}

// Extracts a list of signals which must in the specific instance,
// returns a list of signals (sigs) and possibly an error messasge
// returns the width of the signals in the list
int tc_extractSignals (Tcl_Interp *interp, Tcl_Obj * o, const instHandle &inst, netCollection & sigs, BString &errMsg, unsigned int &width)
{
  int ret = TCL_OK;
  sigs.clear();
  width = 0;
  int objc;
  Tcl_Obj *objPtr;
  unsigned int thiswidth = 1;
  ret = Tcl_ListObjLength (interp, o, & objc );
  string sig;
  std::list<BString>::iterator sitr;

  BString found;
  VeriModule *module = tc_findModuleTcl(interp, NULL, inst, found);
  if (module == 0) {
    errMsg = "while looking for module '" ;
    errMsg += + inst.c_str() ;
    errMsg += "'." ;
    appendTclResultError (interp, errMsg);
    ret = TCL_ERROR;
  } else {
    for (int i=0; i < objc; ++i) {
      ret = Tcl_ListObjIndex(interp, o, i, &objPtr);
      sig = Tcl_GetStringFromObj (objPtr, NULL);
      if (TCL_OK != ret) {
	errMsg = "cannot scan signal list";
	break;
      }
      VeriIdDef *net = HdlUtils::findSignalInModuleByName(module, sig);
      // get signal width
      if (net) {
        sigs.push_back(sig);
        int m,l;
        thiswidth = net->GetPackedWidth(&m,&l);
        width += thiswidth;
      } else {
	ret = TCL_ERROR;
	errMsg = "Signal: '" + sig + "' cannot be found in instance " + inst;
	break;
      }
    }
  }
  
  return ret;
}

// Extracts a list of signals which must in the specific instance,
// returns a list of signals (sigs) and possibly an error messasge
// returns the width of the signals in the list
int tc_extractSignalsOfModule (Tcl_Interp *interp, Tcl_Obj * o, const BString &modname, netCollection & sigs, BString &errMsg)
{
  int ret = TCL_OK;
  sigs.clear();
  int objc;
  Tcl_Obj *objPtr;
  //unsigned int thiswidth = 1;
  ret = Tcl_ListObjLength (interp, o, & objc );
  string sig, sigm, sigsl;
  std::list<BString>::iterator sitr;
  VeriIdDef *net;

  BString found;
  VeriModule *module = tc_findModuleByNameTcl(interp, modname);
  if (module == 0) {
    errMsg = "while looking for module '" ;
    errMsg += + modname.c_str() ;
    errMsg += "'." ;
    appendTclResultError (interp, errMsg);
    ret = TCL_ERROR;
  } else {
    for (int i=0; i < objc; ++i) {
      ret = Tcl_ListObjIndex(interp, o, i, &objPtr);
      if (TCL_OK != ret) {
	errMsg = "cannot scan signal list";
	break;
      }
      sig = Tcl_GetStringFromObj (objPtr, NULL);
      size_t pos = sig.find(':');
      if (pos != string::npos) {

	//sigsl = sig.substr(0, pos);
	sigm = sig.substr(pos+1);

	net = HdlUtils::findSignalInModuleByName(module, sigm);
	// get signal width
	if (net) {
	  sigs.push_back(sig);
	  int m,l;
	  net->GetPackedWidth(&m,&l);
	} else {
	  ret = TCL_ERROR;
	  errMsg = "Signal: '" + sigm + "' cannot be found in module " + modname;
	  break;
	}
      }
      else {

	VeriIdDef *net = HdlUtils::findSignalInModuleByName(module, sig);
	// get signal width
	if (net) {
	  sigs.push_back(sig);
	  int m,l;
	  net->GetPackedWidth(&m,&l);
	} else {
	  ret = TCL_ERROR;
	  errMsg = "Signal: '" + sig + "' cannot be found in module " + modname;
	  break;
	}
      }
    }
  }
  
  return ret;
}

// Extracts a list of signals which must in the specificed instance,
// returns a list of signals (sigs) and possibly an error messasge
// returns the width of the signals in the list
int tc_extractSignalsFromPattern (Tcl_Interp *interp, Tcl_Obj * o, const instHandle &inst,
				  netCollection & expr, netCollection & sigs,
				  BString &errMsg, unsigned int &width, SignalType signaltype)
{
  int ret = TCL_OK;
  sigs.clear();
  width = 0;
  int objc;
  Tcl_Obj *objPtr;
  unsigned int thiswidth = 1;
  ret = Tcl_ListObjLength (interp, o, & objc );
  string sig;
  std::list<BString>::iterator sitr;

  BString found;
  VeriModule *module = tc_findModuleTcl(interp, NULL, inst, found);
  if (module == 0) {
    errMsg = "while looking for module '" ;
    errMsg += + inst.c_str() ;
    errMsg += "'." ;
    appendTclResultError (interp, errMsg);
    ret = TCL_ERROR;
  } else {
    for (int i=0; i < objc; ++i) {
      ret = Tcl_ListObjIndex(interp, o, i, &objPtr);
      sig = Tcl_GetStringFromObj (objPtr, NULL);
      if (TCL_OK != ret) {
	errMsg = "cannot scan signal list";
	break;
      }
      expr.push_back(sig);
      HdlUtils::findSignalsInModuleByExpression(module, sig, sigs, thiswidth, signaltype);
      // get signal width
      if (thiswidth > 0) {
	width += thiswidth;
      } else {
	ret = TCL_ERROR;
	errMsg = "Signal: '" + sig + "' cannot be found in instance " + inst;
	break;
      }
    }
  }
  
  return ret;
}

// Extracts a list of signals which must in the specificed instance,
// returns a list of signals (sigs) and possibly an error messasge
// returns the width of the signals in the list
int tc_extractSignal (Tcl_Interp *interp, Tcl_Obj * o, const instHandle &inst, BString & signal, unsigned int &width)
{
  int ret = TCL_OK;
  width = 0;
  BString found;
  BString errMsg;

  VeriModule *module = tc_findModuleTcl(interp, NULL, inst, found);
  if (module == 0) {
    errMsg = "while looking for module '" ;
    errMsg += + inst.c_str() ;
    errMsg += "'." ;
    appendTclResultError (interp, errMsg);
    ret = TCL_ERROR;
  } else {
    signal = Tcl_GetStringFromObj (o, NULL);
    VeriIdDef *net = HdlUtils::findSignalInModuleByName(module, signal);

    if (net) {
        int m,l;
        width = net->GetPackedWidth(&m,&l);
    } else {
      errMsg = "Signal: '" + signal + "' cannot be found in path " + inst;
      appendTclResultError (interp, errMsg);
      ret = TCL_ERROR;
    }
  }

  return ret;
}

// Looking for sign in *o at instance inst
int tc_extractPort (Tcl_Interp *interp, const instHandle &insth, BString &portname)
{
  int ret = TCL_OK;
  BString errMsg, found;
  VeriModule *module = tc_findModuleTcl(interp, NULL, insth, found);
  if (module == 0) {
    errMsg = "while looking for module '" ;
    errMsg += insth ;
    errMsg += "'." ;
    appendTclResultError (interp, errMsg);
    ret = TCL_ERROR;
    return ret;
  }
  VeriIdDef *port = HdlUtils::findPortOfModuleByName(module, portname);
  if (port == 0) {
    return TCL_ERROR;
  }
  return ret;
}

// Extracts a list of paths which must in the specificed pattern,
// returns a list of paths and possibly an error messasge
int tc_extractPathsFromPattern (Tcl_Interp *interp, const BString &pattern, BStringList &paths) 
{
  int ret = TCL_OK;
  paths.clear();

  HdlUtils::findPathsFromPattern(pattern, paths);
    
  if (paths.size() == 0) {
    BString errMsg;
    errMsg = "No module or path found from pattern '" + pattern + "'";
    appendTclResultError (interp, errMsg);
    ret = TCL_ERROR;
  }

  return ret;
}

// Extract signaltype
int tc_extractSignalType (Tcl_Obj * o, SignalType & t)
{
  size_t begin, end;
  int ret = TCL_OK;
  BString token;
  BString signaltype;
  int temp_type = 0;

  if (o)
    signaltype = Tcl_GetStringFromObj (o, NULL);
  else {
    t = CktModAny;
    return ret;
  }
  begin = signaltype.find_first_not_of(' ');
  end = signaltype.find_first_of(' ', begin);

  while (begin != BString::npos) {

    token = signaltype.substr(begin, end-begin);

    if (token == "Any")
      temp_type = CktModAny;
    else if (token == "Flop")
      temp_type |= CktModFlop;
    else if (token == "Reg")
      temp_type |= CktModReg;
    else if (token == "Input")
      temp_type |= CktModInput;
    else if (token == "Output")
      temp_type |= CktModOutput;
    else
      ret = TCL_ERROR;

    if (end == BString::npos)
      begin = BString::npos;
    else
      begin = signaltype.find_first_not_of(' ', end);
    if (begin != BString::npos)
      end = signaltype.find_first_of(' ', begin);
  }

  if (ret == TCL_OK) {
    t = (SignalType)temp_type;
  }

  return ret;
}
