
#include "EditCompile.h"
#include <iostream>
#include "HdlUtils.h"
#include "VeriConstVal.h"
#include "VeriScope.h"
#include "veri_tokens.h"

#include "tcl.h"

using namespace std ;

const unsigned int EditCompileBase::cmdWidth = 19;
const unsigned int EditCompileBase::dataWidth = 32;

// Static function -- entry point
int EditCompile::compileChanges(CktEdits::ModList_t &modlist, BString & errMsg)
{
  int tclRet = TCL_OK ;
  EditCompile editor(modlist, errMsg);

  int stat = 0;
  stat = stat || editor.partitionPass();
  stat = stat || editor.pass0();
  stat = stat || editor.pass12();
  if (stat == 0 ) {
    modlist.clear();
    // Return the changes to the global state
    modlist.insert ( modlist.end(), editor.m_instSet.begin(), editor.m_instSet.end());
  }
  else {
    // Leave things unchanged if there is an error
    tclRet = TCL_ERROR ;
  }
  return tclRet;
}


// Constructor
EditCompile::EditCompile (CktEdits::ModList_t & modlist, BString &errMsg)
  : m_instSet()
  , m_errMsg(errMsg)
{
  for (CktEdits::ModList_t::iterator iter = modlist.begin(); iter != modlist.end(); ++iter) {
    // remove any compiled items to allow recompiles
    if ( (*iter)->isUserEdit() ){
      m_instSet.insert(*iter);
    }
  }
}

// Debug function...
void EditCompile::dump () const
{
  for (ModInstSetIter_t i = m_instSet.begin(); i != m_instSet.end() ; ++i) {
    // Use the tcl dump visitor here.
    cerr << (*i)->getKey() << "  " << (*i)->getInstName() << endl;
  }
}




/////////////////////////////////////////////////////////////////////////////////////////
// Edit Compile Base class
// Constructor
EditCompileBase::EditCompileBase (const BString & pathName)
  : m_newItems ()
  , m_pathName(pathName)
  , m_veriModule(0)
  , m_probeSink(false)
  , m_topInst(false)
  , m_addedIdentifiers()
{
  BString foundpath;
  m_veriModule = HdlUtils::findModuleFromPath(NULL, pathName, foundpath);
  //HdlUtils::findClockSignalInModule(m_veriModule, m_clkName);

  BString::size_type delim  = pathName.rfind("/");

  m_topInst = delim == 0 || delim == BString::npos;

  std::list<VeriInstId *> insts;
  unsigned int found = HdlUtils::findInstsByMasterName (m_veriModule, "ProbeHook", insts);

  m_probeSink = (found == 1);


  // It is possible that probes are sinked at some other level
  // e.g. our fabric.
  // TODO -- look at this hier for a inst with ProbeHook
  // m_probeSink = delim == 0 || delim == BString::npos;
  // cerr << "Edit compile " << pathName << " delim " << delim << " PS: " << m_probeSink << endl;

}

void EditCompileBase::setError (const BString & msg)
{
  if (! m_errMsg.empty()) {
    m_errMsg += "\n";
  }
  m_errMsg += msg;
}


bool EditCompileBase::isUniqueIdentifier(const BString & name, bool checkAdded) const
{
  bool unique = true;
  // TODO
  // Check if m_pathName/rootname exists in model
  if (unique && checkAdded) {
    unique = (m_addedIdentifiers.find (name) == m_addedIdentifiers.end());
  }
  if (unique && checkAdded) {
    VeriModule* module = getMasterModule();
    VeriIdDef* existing = module->GetScope()->Find(name.c_str());
    if (existing) {
      unique = false;
    }
  }
  return unique;
}

// returns a legal non-conflicting identifier for this hier, using rootname as the base
BString EditCompileBase::getUniqueIdentifier ( const BString & rootname) const
{
  char buf[320];
  unsigned int i = 0;
  BString uname(rootname);
  while ( ! isUniqueIdentifier(uname, true) )  {
    snprintf(buf, 320, "%s_%u", rootname.c_str(), ++i);
    uname = buf;
  }
  return uname;
}

// returns a legal non-conflicting identifier for this hier, using rootname as the base
BString EditCompileBase::getUniqueIdentifier ( const BString & rootname, bool add)
{

  const BString uname = getUniqueIdentifier (rootname);
  if (add) {
    addIdentifier(uname);
  }
  return uname;
}

const BString & EditCompileBase::getSharedIdentifier (const BString & rootname, bool add)
{
  IdMapIter f =  m_addedIdentifiers.find (rootname);
  if (f != m_addedIdentifiers.end() ) {
    return (*f).second ;
  }
  BString newName = getUniqueIdentifier (rootname, add);
  pair<IdMapIter,bool> iloc = m_addedIdentifiers.insert ( pair<BString,BString> (rootname, newName));
  BString & ret = iloc.first->second;
  return ret;
}

const BString & EditCompileBase::getSharedIdentifier (const BString & rootname)
{

  return getSharedIdentifier(rootname, false);

}
void EditCompileBase::addIdentifier(const BString &name)
{
  m_addedIdentifiers.insert ( pair<BString,BString> (name, name));
}

bool EditCompileBase::addNet(const BString &nname, unsigned int w)
{

  bool is_new = (m_addedNets.find(nname) == m_addedNets.end());
  if (is_new) {
    addNewItem (new AddNet (pathName(), nname, w));
    m_addedNets.insert (nname);
    return true;
  }
  return false;
}

ModuleTerminalList EditCompileBase::createCosimTerminalList (VeriModule* module)
{
  ModuleTerminalList mtlist;
  unsigned it;
  VeriIdDef *port;
  FOREACH_ARRAY_ITEM(module->GetPorts(), it, port) {
    if (port->IsInput()) {
      VeriRange* range = port->GetDimensionAt(0);

      int size = 1;
      if (range) {

	range->StaticReplaceConstantExpr(1);

	int min = 0, max = 0;


	min = range->GetLsbOfRange();
	max = range->GetMsbOfRange();

	if (min > max) {
	  min = range->GetMsbOfRange();
	  max = range->GetLsbOfRange();
	}

	size = max - min + 1;
      }
//      printf("SIZE: %d\n", size);
      BString ssize = itoa(size);
      ssize += "'b0";
      mtlist.push_back (ModuleTerminal (port->GetName(), ssize));
    } else {
      mtlist.push_back (ModuleTerminal (port->GetName(), ""));
    }
  }
  return mtlist;
}

ParameterList EditCompileBase::createCosimParameterList (Verific::VeriModule* module)
{
  ParameterList plist;
  Array *params = module->GetParameters();
  unsigned itema;
  VeriIdDef *para;
  FOREACH_ARRAY_ITEM (params, itema, para) {
    VeriExpression *expr = para->GetInitialValue();
    //VeriConst *exprC = expr->ConstCast();

    char * val = expr->Image();
    plist.push_back(Parameter (para->Name(), val));
  }
  return plist;
}
