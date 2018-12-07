// Copyright 2010 Bluespec Inc. All rights reserved
#pragma once

#include <list>
#include <map>
#include <string>
#include "CktEdits.h"
#include "VeriModule.h"
#include "TextBasedDesignMod.h"
#include "veri_yacc.h"

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif

using namespace std;

// This visitor class applies all the changes to the netlist model

class ApplyChanges : public CktModVisitor {

  typedef std::list<VeriModule* > VeriModuleList;
  typedef std::list<VeriModule* >::iterator VeriModuleListIterator;
  typedef std::pair<string, ModSet> ModSetPath;
  typedef std::map<BString, ChangeInstModuleName*> ChangeInstModuleNameMap;
  typedef std::map<BString, AddInstance*> AddInstanceMap;

  std::list<VeriModule*> m_module_list;
  std::map<VeriModule *, int> already_written;
  std::map<VeriModule *, string> m_new_modname;
  std::map<string, unsigned int> m_content_map;
  std::map<ModSetPath, unsigned int> m_path_map;
  std::map<unsigned int, unsigned int> m_key_map;
  ChangeInstModuleNameMap              m_change_map;
  AddInstanceMap                       m_add_map; // every added instance

  VeriModule *m_module;
  unsigned int m_pass;
  unsigned int m_textbased;
  TextBasedDesignMod m_tbdm;
  string m_output_dir;
  linefile_type m_last_port_linefile;
  linefile_type m_last_declare_linefile;
  string m_path_info;

 public:

  ApplyChanges () :m_module(0), m_pass(0), m_textbased(1), m_tbdm(0), m_path_info(""){ }
  ApplyChanges (const char* dir) :m_module(0), m_pass(0), m_textbased(1), m_tbdm(dir),
    m_output_dir(dir), m_path_info("") { }
  virtual ~ApplyChanges() {}

  virtual int novisitor (class CktMod *cm) {
    fprintf (stderr, "ApplyChanges for %s %u is not defined!\n", cm->getInstName().c_str(), cm->getKey());
    return 0;
  }
  virtual int visit(class CktMod *m);
  virtual int visit(class ChangeModuleName *m);
  virtual int visit(class ChangeInstModuleName *m);
  virtual int visit(class AddPort *m);
  virtual int visit(class AddInstanceConnection *m);
  virtual int visit(class RenameInstanceConnection *m);
  virtual int visit(class AddNet *m);
  virtual int visit(class AddSimpleAssign *m);
  virtual int visit(class AddInstance *m);
  virtual int visit(class RmInstance *m);
  virtual int visit(class RmBody *m);
  virtual int visit(class RmReg *m);
  virtual int visit(class RmNet *m);
  virtual int visit(class RmSimpleAssign *m);
  virtual int visit(class ChangeAssignRHS *m);
  virtual int visit(class RmCode *m);
  virtual int visit(class UpdateParam *m);
  virtual int visit(class AddCosim *cm);
  virtual int visit(class ReplaceModule *m);
  virtual int visit(class RenameSignal *m);
  virtual int visit(class ChangeFragmentFile *m);

  // These will apply the modification to the parsed tree
  int parsedTreeApply(class ChangeModuleName *);
  int parsedTreeApply(class ChangeInstModuleName *);
  int parsedTreeApply(class AddPort *);
  int parsedTreeApply(class AddInstanceConnection *);
  int parsedTreeApply(class RenameInstanceConnection *);
  int parsedTreeApply(class AddNet *);
  int parsedTreeApply(class AddSimpleAssign *);
  int parsedTreeApply(class AddInstance *);
  int parsedTreeApply(class RmInstance *);
  int parsedTreeApply(class RmBody *);
  int parsedTreeApply(class RmReg *);
  int parsedTreeApply(class RmNet *);
  int parsedTreeApply(class RmSimpleAssign *);
  int parsedTreeApply(class ChangeAssignRHS *);
  int parsedTreeApply(class RmCode *);
  int parsedTreeApply(class UpdateParam *);
  int parsedTreeApply(class RenameSignal *);

  // These will apply the modification using text based method
  int textBasedApply(class ChangeModuleName *);
  int textBasedApply(class ChangeInstModuleName *);
  int textBasedApply(class AddPort *);
  int textBasedApply(class AddInstanceConnection *);
  int textBasedApply(class RenameInstanceConnection *);
  int textBasedApply(class AddNet *);
  int textBasedApply(class AddSimpleAssign *);
  int textBasedApply(class AddInstance *);
  int textBasedApply(class RmInstance *);
  int textBasedApply(class RmBody *);
  int textBasedApply(class RmReg *);
  int textBasedApply(class RmNet *);
  int textBasedApply(class RmSimpleAssign *);
  int textBasedApply(class ChangeAssignRHS *);
  int textBasedApply(class RmCode *);
  int textBasedApply(class UpdateParam *);
  int textBasedApply(class RenameSignal *);

  // These do not require any action for the apply changes
  virtual int visit (class AddProbe *cm) {return 0;}
  virtual int visit (class AddBsvProbes *cm) {return 0;}
  virtual int visit (class AddScan *cm) {return 0;}
  virtual int visit (class AddCapture *cm) {return 0;}
  virtual int visit (class AddTrigger *cm) {return 0;}
  virtual int visit (class ProbeMux *cm) {return 0;}
  virtual int visit (class ProbeConnection *cm) {return 0;}
  virtual int visit (class ProbeBus *cm) {return 0;}
  virtual int visit (class ScannedInstance *cm) {return 0;}
  virtual int visit (class ProbedInstance *cm) {return 0;}
  virtual int visit (class DrawOutSignal *cm) {return 0;}
  virtual int visit (class DrawInSignal *cm) {return 0;}
  virtual int visit (class CosimFill *cm) {return 0;}
  virtual int visit (class Partition *cm) {return 0;}
  virtual int visit (class GenSceMi *cm) {return 0;}

  void setTextBased(unsigned int value) { m_textbased = value; }
  int getTextBased() { return m_textbased; }
  void setModuleContext(VeriModule *module);
  VeriModule* getModuleContext() { return m_module; }
  unsigned int nextPass() { return ++m_pass;}
  bool morePasses() const { return m_pass < 4; };
  void clearModifications(VeriModule *module);
  string writeTextBasedChanges(VeriModule *module, BString & path, ModSet & modset, bool & is_top);
  void writeParsedTreeChanges(BStringList &written_filenames, const char *suffix = 0);
  void markModuleForWrite(VeriModule *module) { m_module_list.push_back(module); }
  void setLastPortLinefile();
  linefile_type getLastComment(VeriModule *module);
  int addPathInfo(BString info);
  int writePathInfo();
  bool currentPassIs(const unsigned int p0) { return (m_pass == p0); }
  void textRenameSignal(VeriTreeNode* node, const BString & name, const BString & name_new);
  void textAddSignal(VeriModule* module, const BString & name, const unsigned & size, const unsigned & type = VERI_WIRE);
};
