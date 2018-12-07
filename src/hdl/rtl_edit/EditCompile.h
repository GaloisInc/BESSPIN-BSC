// Copyright 2010 Bluespec Inc. All rights reserved
#pragma once

#include <map>
#include "CktEdits.h"
#include "ScanPath.h"
namespace Verific {class VeriModule; class VeriExpression;}

typedef std::multiset<CktMod *, CompareInstName> ModInstSet_t ;
typedef ModInstSet_t::iterator ModInstSetIter_t ;
typedef std::pair<ModInstSetIter_t, ModInstSetIter_t> ModInstSetPairIter_t;
typedef std::map<unsigned int, ScanData*> ScanMap_t;

// This class compile the changes from abstract changes, such as addprobe to
// detailed changes such as addport, addinstance addassign, etc.
// This code is independent of the Netlist model

class EditCompile {
 protected:

 public:
  static int compileChanges(CktEdits::ModList_t &, BString & errMsg);
 protected:
  EditCompile (CktEdits::ModList_t &, BString & errMsg);
 protected:
  ModInstSet_t   m_instSet;     // Copy of working set
  BString      & m_errMsg ;     // place for error messages

  // Set an error message for return
  void setError (const BString & msg);
 private:
  void dump() const;
  int partitionPass();
  int pass0();
  int pass12();

};


// Common Base class for Edit Compiler visitors
class EditCompileBase : public CktModVisitor {
private:
  typedef std::map<BString,BString> IdMap;
  typedef IdMap::iterator IdMapIter;

  CktEdits::ModList_t m_newItems ; // New items to add to this instance
  BString m_pathName;
  class Verific::VeriModule *    m_veriModule;

  bool  m_probeSink;               // probes are connected to xactor here
  bool  m_topInst;
  BString m_errMsg;
  std::map<BString,BString>  m_addedIdentifiers;
  std::set<BString>          m_addedNets;
protected:
  EditCompileBase (const BString &inst);
public:
  virtual ~EditCompileBase () {};

protected:
  void addNewItem(CktMod *cm) { cm->setCompiled() ; m_newItems.push_back(cm); }
  void addNewItem(CktMod *cm, ModSet ms) { cm->setCompiled() ; cm->setModSet(ms) ; m_newItems.push_back(cm); }
  const BString & pathName() const { return m_pathName; }
  Verific::VeriModule * getMasterModule() const { return m_veriModule ; }
  bool isProbeSink() const { return m_probeSink;}
  bool isTopInst() const { return m_topInst; }

  bool isUniqueIdentifier(const BString & name, bool checkAdded) const;
  // returns a legal non-conflicting identifier for this hier, using rootname as the base
  BString getUniqueIdentifier ( const BString & rootname) const;
  BString getUniqueIdentifier ( const BString & rootname, bool add);
  // Get a shared identifier name.  The property of this function is that
  // subsequent calls always return the same name
  const BString & getSharedIdentifier (const BString & rootname);
  const BString & getSharedIdentifier (const BString & rootname, bool add);

  void addIdentifier(const BString &name);
  // Set an error message for return
  void setError (const BString & msg) ;
  bool addNet(const BString &nname, unsigned int w);
  ModuleTerminalList createCosimTerminalList (Verific::VeriModule* module);
  ParameterList createCosimParameterList (Verific::VeriModule* module);

  static const unsigned int cmdWidth;
  static const unsigned int dataWidth;

public:

  void setPathName(const BString &pn) { m_pathName = pn; }
  CktEdits::ModListIter_t newFirst() {return m_newItems.begin();};
  CktEdits::ModListIter_t newEnd()   {return m_newItems.end();}

  const BString & getError () const { return m_errMsg; }

  // Derived processing function
  virtual int process(ModInstSetPairIter_t &) = 0;
  virtual int novisitor(class CktMod *cm) = 0;

};

