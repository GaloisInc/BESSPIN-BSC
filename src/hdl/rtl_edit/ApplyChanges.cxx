// Copyright 2010 Bluespec Inc. All rights reserved

#include <dirent.h>
#include <sys/stat.h>
#include "ApplyChanges.h"
#include "HdlUtils.h"
#include "VeriModule.h"
#include "VeriLibrary.h"
#include "VeriExpression.h"
#include "VeriMisc.h"
#include "VeriId.h"
#include "veri_tokens.h"
#include "VeriConstVal.h"
#include "VeriScope.h"
#include "VeriModuleItem.h"
#include "VeriUtil_Stat.h"
#include "VeriNodeInfo.h"
#include "string.h"

#include <boost/regex.hpp>

int ApplyChanges::visit(class CktMod *) { return -1; }

int ApplyChanges::visit(class ChangeModuleName *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class ChangeInstModuleName *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class AddPort *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class AddInstanceConnection *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class RenameInstanceConnection *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class AddNet *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class AddSimpleAssign *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class AddInstance *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class RmInstance *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class RmBody *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class RmReg *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class RmNet *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class RmSimpleAssign *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class ChangeAssignRHS *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class RmCode *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class UpdateParam *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::visit(class ChangeFragmentFile *m)
{
  if (!currentPassIs(3)) return 0;

  //"sed -e 's/top./top.fpgaA.top_mkBridge_EDITED./' vlog_dut/scemilink.vlog_fragment > vlog_edited/scemilink.vlog_fragment"}}

  BString ifilename( m->getInputDir());
  ifilename += "/";
  ifilename += "scemilink.vlog_fragment";
  ifstream infile(ifilename.c_str());
  if (infile.fail()) {
    cerr << "ChageFragmentFile:: unable to open input file " << ifilename << endl;
    return 1;
  }

  BString ofilename( m_output_dir);
  ofilename += "/";
  ofilename += "scemilink.vlog_fragment";
  ofstream outfile(ofilename.c_str());
  if (outfile.fail()) {
    cerr << "ChageFragmentFile:: unable to open output file " << ofilename << endl;
    return 1;
  }

  BString oldname (m->getOldName());
  size_t oldlen = oldname.length();

  BString linestr;
  getline(infile,linestr);  // Get the frist line from the file, if any.
  while ( infile ) {
    size_t pos = linestr.find(oldname);
    if (pos != string::npos) {
      // replace oldname with new name
      linestr.replace(pos, oldlen, m->getNewName());
    }

    outfile << linestr << std::endl ;
    linestr.clear();
    getline(infile,linestr);    // next line
  }

  infile.close();
  outfile.close();

  return 0;
}

// Collect info for the scan.map file
// this is the same either TextedBased or ParsedTree
int ApplyChanges::visit(class AddCosim *m)
{
  char buf[1024];
  if (!currentPassIs(1)) return 0;

  cerr << "Adding scan chain info for cosim probe " << m->getName() << endl;

  ScanDataList* datalist = m->getData();
  for (ScanDataList::iterator iter = (*datalist).begin(); iter != (*datalist).end(); ++iter) {
    ScanData & data = (**iter);


    sprintf(buf, "p: %d %d %d\n", m->getKey(), data.getChain(), data.getWidth());
//    sprintf(buf, "p: %d %d\n", data.getChain(), data.getWidth());
    addPathInfo(buf);

  sprintf(buf, "d: %d %s\n", m->getKey(), m->getDef().c_str());
//  sprintf(buf, "d: %s\n", m->getDef().c_str());
  addPathInfo(buf);
  sprintf(buf, "i: %d %s\n", m->getKey(), m->getInst().c_str());
//  sprintf(buf, "i: %d %s\n", m->getKey(), "/top");
  addPathInfo(buf);

  if (data.getFlavor() == Inputs || data.getFlavor() == InputsBB) {
    sprintf(buf, "c: /%s\n", m->getClock().c_str());
    addPathInfo(buf);
  }

  ScanPath* path = data.getPath();
  addPathInfo(path->ToString("", "\n", "r: "));
  }
  return 0;
}

// this is the same either TextedBased or ParsedTree
int ApplyChanges::visit(class ReplaceModule *m)
{
  if (!currentPassIs(0)) return 0;

  VeriModule* module = veri_file::GetModule(m->getName().c_str());

  if (!module) {
    cerr << "Unable to resolve module " << m->getName().c_str() << endl;
    return 0;
  }

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::visit(ReplaceModule): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  std::string new_module_name = m_new_modname[m_module];

  m_module = module;
  setLastPortLinefile();
  clearModifications(m_module);

  m_new_modname[m_module] = new_module_name;

  return 1;

}

int ApplyChanges::visit(class RenameSignal *m)
{
  return (m_textbased ? textBasedApply(m) : parsedTreeApply(m));
}

int ApplyChanges::parsedTreeApply(class ChangeModuleName *modification)
{
  if (!currentPassIs(1)) return 0;
  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(ChangeModuleName): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  //printf("Change Module Name: from %s to %s\n", m_module->Name(),
  // modification->getName().c_str());

  VeriIdDef *id = m_module->GetId();

  if (id == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(ChangeModuleName): module "
	 << modification->getInstName().c_str() << " has no VeriIdDef."
	 << endl;
    return 0;
  }

  VeriLibrary *lib = m_module->GetLibrary() ;
  VeriScope *scope = m_module->GetScope() ;
  VeriScope *container_scope = (scope) ? scope->Upper() : 0 ;

  // First remove the module/id from its container:
  if (lib) {
    (void) lib->DetachModule(m_module) ;
  } else if (container_scope) {
    // Nested module:
    (void) container_scope->Undeclare(id) ;
  }

  id->SetName(Strings::save(modification->getName().c_str()));

  // Then, back insert into its container:
  if (lib) {
    (void) lib->AddModule(m_module) ;
  } else if (container_scope) {
    // Nested module:
    (void) container_scope->Declare(id) ;
  }
  
  if (scope) {
    scope->Declare(id);
    id->SetLocalScope(scope);
  }

  return 1;
}

int ApplyChanges::textBasedApply(class ChangeModuleName *modification)
{
  if (!currentPassIs(1)) return 0;
  if (m_module == NULL) {
    cerr << "Error ApplyChanges::textBasedApply(ChangeModuleName): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  std::string new_module_name = m_new_modname[m_module];
  //printf("Change Module Name: from %s to %s\n", m_module->Name(), modification->getName().c_str());
  
  if (0 == new_module_name.compare(modification->getName())) {
    //    printf("SKIP\n");
    //    return 1;
  }

  VeriIdDef *id = m_module->GetId();

  // Form a pattern for replacing
  /*
  string modname_tmp = m_module->Name();
  size_t pos = modname_tmp.find("_orig");
  string modname = modname_tmp.substr(0,pos);
  //cout << "modname " << modname << endl;
  string pat("(\\s*)(");
  pat += modname;
  pat += ")(\\s*.*)";
  cout << "Replace pattern " << pat << endl;
  const boost::regex rpattern(pat,
			      boost::regex_constants::icase|boost::regex_constants::perl);
  std::string newpat("\\1");
  newpat += modification->getName();
  newpat += "\\3";
  cout << "New pattern " << newpat << endl;
  */

  // Get the file position
  linefile_type start_linefile = id->StartingLinefile();
  
  // Get the text
  //string linetext = m_tbdm.GetText(start_linefile, 1);

  // Make substitution
  //string newline = boost::regex_replace(linetext, rpattern, newpat);
  // Replace the text in the file
  m_tbdm.Replace(start_linefile, modification->getName().c_str());

  m_new_modname[m_module] = modification->getName();

  ModSetPath pair  = ModSetPath(modification->getInstName(), modification->getModSet());
  m_path_map[pair] = modification->rangeKey();
    
  return 1;
}

int ApplyChanges::parsedTreeApply(class ChangeInstModuleName *modification)
{
  if (!currentPassIs(2)) return 0;
  unsigned ret = 1;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(ChangeInstModuleName): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  //printf("Change ModuleInst Name: from %s to %s\n", modification->getContainingInst().c_str(),
  //	 modification->getName().c_str());

  // Get the instance identifier
  VeriIdDef *instance_id = m_module->FindDeclared(modification->getContainingInst().c_str());

  // No instance named 'instance_name' exists
  if (!instance_id || !instance_id->IsInst()) {
    cerr << "Error ApplyChanges::parsedTreeApply(ChangeInstModuleName): instance "
	 << modification->getContainingInst().c_str() 
	 << " of module "
	 << modification->getInstName().c_str() << " is not found."
	 << endl;
    return 0 ;
  }

  VeriModuleInstantiation *moduleInst = instance_id->GetModuleInstance();
  if (moduleInst == NULL) ret = 0;

  if (ret) {
    VeriName *vname = moduleInst->GetModuleNameRef();
    if (vname == NULL)
      ret = 0;
    else
      vname->SetName(Strings::save(modification->getName().c_str()));
  }

  if (ret == 0) {
    cerr << "Error ApplyChanges::parsedTreeApply(ChangeInstModuleName): name change of instance "
	 << modification->getInstName().c_str() 
	 << " failed."
	 << endl;
    return 0;
  }

  return ret;
}

int ApplyChanges::textBasedApply(class ChangeInstModuleName *modification)
{
  if (!currentPassIs(2)) return 0;
  unsigned ret = 1;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::textBasedApply(ChangeInstModuleName): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // printf("Change ModuleInst Name: from %s to %s\n", modification->getContainingInst().c_str(),
  // modification->getName().c_str());

  // Get the instance identifier
  VeriIdDef *instance_id = m_module->FindDeclared(modification->getContainingInst().c_str());

  if (modification->newInst()){
    BString key = modification->getInstName();
    key += "/";
    key +=  modification->getContainingInst();
    m_change_map[key] = modification;
  }

  // No instance named 'instance_name' exists
  if (!instance_id || !instance_id->IsInst()) {
    if (!modification->newInst()) {
      cerr << "Error ApplyChanges::textBasedApply(ChangeInstModuleName): instance "
	   << modification->getContainingInst().c_str() 
	   << " of module "
	   << modification->getInstName().c_str() << " is not found."
	   << endl;
    }
    return 0 ;
  }

  VeriModuleInstantiation *moduleInst = instance_id->GetModuleInstance();
  if (moduleInst == NULL) ret = 0;

  if (ret) {
    VeriName *vname = moduleInst->GetModuleNameRef();
    if (vname == NULL)
      ret = 0;
    else {
      /*
      // Form a pattern for replacing
      string modname = vname->GetName();
      //cout << "modname " << modname << endl;
      string pat("(\\s*)(");
      pat += modname;
      pat += ")(\\s*.*)";
      //cout << "Replace pattern " << pat << endl;
      const boost::regex rpattern(pat,
				  boost::regex_constants::icase|boost::regex_constants::perl);
      std::string newpat("\\1");
      newpat += modification->getName();
      newpat += "\\3";
      //cout << "New pattern " << newpat << endl;
      */

      // Get the file position
      linefile_type start_linefile = vname->StartingLinefile();
      //linefile_type end_linefile = vname->EndingLinefile();
      
      // Get the text
      //string linetext = m_tbdm.GetText(start_linefile, end_linefile, 1);
      
      // Make substitution
      //string newline = boost::regex_replace(linetext, rpattern, newpat);

      unsigned int suffix = modification->getSuffix();

      std::string name_new = modification->getName();
      if (suffix > 0) {

	std::map<unsigned int, unsigned int>::iterator iter = m_key_map.find(suffix);

	if(iter == m_key_map.end())
	  {
	    name_new += "_";
	    name_new += itoa(suffix);
	  }
	else 
	  {
	    unsigned int mapped = (*iter).second;
	    name_new += "_";
	    name_new += itoa(mapped);
	  }
      }
      
      // Replace the text in the file
      m_tbdm.Replace(start_linefile, name_new.c_str());
    }
  }

  if (ret == 0) {
    cerr << "Error ApplyChanges::textBasedApply(ChangeInstModuleName): name change of instance "
	 << modification->getInstName().c_str() 
	 << " failed."
	 << endl;
    return 0;
  }

  return ret;
}

int ApplyChanges::parsedTreeApply(class AddPort *port)
{
  if (!currentPassIs(1)) return 0;
  unsigned portdir;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddPort): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  printf("AddPort1\n");

  switch (port->getDirection()) {

  case d_input:
    portdir = VERI_INPUT;
    break;

  case d_output:
    portdir = VERI_OUTPUT;
    break;

  case d_inout:
    portdir = VERI_INOUT;
    break;

  default:
    cerr << "Warning ApplyChanges::parsedTreeApply(AddPort): unknown port direction: "
	 << port->getDirection()
	 << endl;
    break;
  }

  VeriRange *range = HdlUtils::mkVeriRangeFromWidth(port->getWidth());
  VeriDataType *type = new VeriDataType(0, 0, range);
  VeriIdDef *vport = m_module->AddPort(port->getPortName().c_str(), portdir, type);

  if (vport) {
    m_module_list.push_back(m_module);
  }
  else {
    cerr << "Error ApplyChanges::parsedTreeApply(AddPort): failed adding port "
	 << port->getPortName().c_str() << " to module "
	 << m_module->Name() << endl;

    return 0;
  }

  return 1;
}

int ApplyChanges::textBasedApply(class AddPort *port)
{
  string portdir;

  if (!currentPassIs(1)) return 0;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::textBasedApply(AddPort): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Get the port connection array of the first top level module.
  Array *port_connects = m_module->GetPortConnects() ;

  VeriExpression *last_port = (port_connects && port_connects->Size()) ? (VeriExpression*)port_connects->GetLast() : 0 ;
  if (!last_port) {
    cerr << "Warning ApplyChanges::parsedTreeApply(AddPort): Cannot find any port on module"
	 << m_module->Name()
	 << endl;
    return 0;
  }

  linefile_type ending_linefile = last_port->EndingLinefile() ;

  switch (port->getDirection()) {
    
  case d_input:
    portdir = "input";
    break;
    
  case d_output:
    portdir = "output";
    break;
    
  case d_inout:
    portdir = "inout";
    break;
    
  default:
    cerr << "Warning ApplyChanges::parsedTreeApply(AddPort): unknown port direction: "
	 << port->getDirection()
	 << endl;
    return 0;
  }

  // Here we are inserting a port reference after a specific port, so we have to insert a ','
  // before the new port name. Please note that we don't check whether a port with this name
  // (new_port) is already there in the module or not.
  //cerr << "AddPort " << port->getPortName() << " module: " << m_module->Name() << endl;
  if (last_port->IsAnsiPortDecl()) {
    string portdecl = ", ";
    portdecl += portdir;
    portdecl += " ";
    if (port->getWidth() > 1) {
      std::stringstream out;
      out << (port->getWidth()-1);
      portdecl += "[" + out.str() + ":0] ";
    }
    portdecl += port->getPortName();

    // It's an ANSI port declaration, so just insert another ANSI port
    m_tbdm.InsertAfter(ending_linefile, portdecl.c_str());

  } else {
    string portdecl = ", ";
    portdecl += port->getPortName();

    // Its a non-ANSI port declaration, so insert the declaration both in port list and module item
    m_tbdm.InsertAfter(ending_linefile, portdecl.c_str());
    
    // Call InsertBefore of 'DesignModDir' utility
    portdecl = portdir;
    portdecl += " ";
    if (port->getWidth() > 1) {
      std::stringstream out;
      out << (port->getWidth()-1);
      portdecl += "[" + out.str() + ":0] ";
    }
    portdecl += port->getPortName();
    portdecl += ";\n";
    m_tbdm.InsertBefore(m_last_port_linefile, portdecl.c_str());
  }

  return 1;
}

int ApplyChanges::parsedTreeApply(class AddNet *net)
{
  if (!currentPassIs(2)) return 0;
  VeriIdDef *signal;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddNet): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  VeriRange *range = HdlUtils::mkVeriRangeFromWidth(net->getWidth());
  signal = m_module->AddSignal(net->getName().c_str(), new VeriDataType(VERI_WIRE, 0, range), 0);

  if (signal) {
    m_module_list.push_back(m_module);
  }
  else {
    cerr << "Error ApplyChanges::parsedTreeApply(AddNet): failed adding net "
	 << net->getName().c_str() << " to module "
	 << m_module->Name() << endl;

    return 0;
  }

  return 1;
}

int ApplyChanges::textBasedApply(class AddNet *net)
{
  string netdecl;

  if (!currentPassIs(2)) return 0;

  //cerr << "AddNet " << net->getName() << " module: " << m_module->Name() << endl;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::textBasedApply(AddNet): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  if (HdlUtils::findSignalInModuleByName(m_module, net->getName()))
    return 1;

  // Get the port connection array of the first top level module.
  Array *port_connects = m_module->GetPortConnects() ;

  VeriExpression *last_port = (port_connects && port_connects->Size()) ? (VeriExpression*)port_connects->GetLast() : 0 ;

  // Conditional means that the port declaration in the module definition already
  // has input/output declaraion, we cannot add a wire here or it would be syntax error.
  if (!net->getConditional() || (last_port && !last_port->IsAnsiPortDecl())) {

    linefile_type lf = m_module->Linefile();
    linefile_type end_lf = (lf) ? lf->Copy() : 0 ;
    if (end_lf) {
      // Set the start line/col to be at 'e' of "endmodule" keyword:
      end_lf->SetLeftLine(end_lf->GetRightLine()) ;
      end_lf->SetLeftCol(end_lf->GetRightCol()-9) ;
    }
    
    // Call InsertBefore of 'DesignModDir' utility
    netdecl += "wire ";
    if (net->getWidth() > 1) {
      std::stringstream out;
      out << (net->getWidth()-1);
      netdecl += "[" + out.str() + ":0] ";
    }
    netdecl += net->getName();
    netdecl += ";\n";
    m_tbdm.InsertBefore(m_last_port_linefile, netdecl.c_str());
    //m_tbdm.InsertBefore(end_lf, netdecl.c_str());
  }

  return 1;
}

int ApplyChanges::parsedTreeApply(class AddInstanceConnection *modification)
{
  unsigned ret;

  if (!currentPassIs(2)) return 0;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddInstanceConnection): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Get the instance identifier
  VeriIdDef *instance_id = m_module->FindDeclared(modification->getContainingInst().c_str());
  VeriInstId *inst;

  // No instance named 'instance_name' exists
  if (!instance_id || !instance_id->IsInst()) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddInstanceConnection): instance "
	 << modification->getContainingInst().c_str() 
	 << " of module "
	 << m_module->Name() << " is not found."
	 << endl;
    return 0 ;
  }
  else
    inst = static_cast<class VeriInstId*>(instance_id);

  string value = modification->getPortExpr();
  string portname = modification->getPortName();

  // Get the style of the port print
  VariablePrintStyle style = HdlUtils::getPortConnectsPrintStyle(inst);

  if (style == HdlKeyValuePairStyle) {
    ret = HdlUtils::addPortRefKeyValue(m_module, inst, portname,
				       value);
  }                      
  else if (style == HdlPositionalStyle) {
    ret = HdlUtils::addPortRefPositional(m_module, inst, value);
  }
  
  if (ret == 0) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddInstanceConnection): add portref to instance "
	 << modification->getInstName().c_str() 
	 << " failed."
	 << endl;
    return 0;
  }

  return 1;
}

int ApplyChanges::textBasedApply(class AddInstanceConnection *modification)
{
  string portdecl;
  VeriInstId *inst;

  if (!currentPassIs(2)) return 0;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddInstanceConnection): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Get the instance identifier
  VeriIdDef *instance_id = m_module->FindDeclared(modification->getContainingInst().c_str());

  // No instance named 'instance_name' exists
  if (!instance_id || !instance_id->IsInst()) {
    cerr << "Error ApplyChanges::textBasedApply(AddInstanceConnection): instance "
	 << modification->getContainingInst().c_str() 
	 << " of module "
	 << modification->getInstName().c_str() << " is not found."
	 << endl;
    return 0 ;
  } else {
    inst = static_cast<class VeriInstId*>(instance_id);
  }

  Array *port_connects = inst->GetPortConnects();
  VeriExpression *last_port = (port_connects && port_connects->Size()) ? (VeriExpression*)port_connects->GetLast() : 0 ;
  if (!last_port) {
    cerr << "Warning ApplyChanges::textBasedApply(AddInstanceConnection): Cannot find any port on instance"
	 << modification->getContainingInst().c_str() 
	 << endl;
    return 0;
  }

  // Get the starting location of first module item
  linefile_type ending_linefile = last_port->EndingLinefile() ;

  // Get the style of the port print
  VariablePrintStyle style = HdlUtils::getPortConnectsPrintStyle(inst);
  string value = modification->getPortExpr();
  string portname = modification->getPortName();

  if (style == HdlKeyValuePairStyle) {
    portdecl = ", .";
    portdecl = portdecl+portname+"("+value+")";
  }                      
  else if (style == HdlPositionalStyle) {
    portdecl = ", ";
    portdecl += value;
  }

  // Call InsertBefore of 'DesignModDir' utility
  m_tbdm.InsertAfter(ending_linefile, portdecl.c_str());
  
  return 1;
}

int ApplyChanges::parsedTreeApply(class RenameInstanceConnection *modification)
{
  return 0;
}

int ApplyChanges::textBasedApply(class RenameInstanceConnection *modification)
{
  string portdecl;
  VeriInstId *inst;

  if (!currentPassIs(2)) return 0;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddInstanceConnection): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Get the instance identifier
  VeriIdDef *instance_id = m_module->FindDeclared(modification->getContainingInst().c_str());

  // No instance named 'instance_name' exists
  if (!instance_id || !instance_id->IsInst()) {
    cerr << "Error ApplyChanges::textBasedApply(AddInstanceConnection): instance "
	 << modification->getContainingInst().c_str() 
	 << " of module "
	 << modification->getInstName().c_str() << " is not found."
	 << endl;
    return 0 ;
  } else {
    inst = static_cast<class VeriInstId*>(instance_id);
  }

  Array *port_connects = inst->GetPortConnects();
  VeriExpression *last_port = 0;

  unsigned it;
  VeriExpression *expr;
  BString instportname;
  FOREACH_ARRAY_ITEM(port_connects, it, expr) {
    if (expr->GetClassId() == ID_VERIPORTCONNECT) {
      instportname = expr->NamedFormal();
      if (instportname == modification->getPortName()) {
	last_port = expr;
      }
    }
  }

  if (!last_port) {
    cerr << "Warning ApplyChanges::textBasedApply(AddInstanceConnection): Cannot find any port on instance"
	 << modification->getContainingInst().c_str() 
	 << endl;
    return 0;
  }

  // Get the starting location of first module item
  linefile_type start_linefile = last_port->StartingLinefile() ;

  // Get the style of the port print
  VariablePrintStyle style = HdlUtils::getPortConnectsPrintStyle(inst);
  string value = modification->getPortExpr();
  string portname = modification->getPortName();

  if (style == HdlKeyValuePairStyle) {
    portdecl = ".";
    portdecl = portdecl+portname+"("+value+")";
  }                      
  else if (style == HdlPositionalStyle) {
    portdecl = "";
    portdecl += value;
  }

  // Call InsertBefore of 'DesignModDir' utility
  m_tbdm.Replace(start_linefile, portdecl.c_str());
  
  return 1;
}

int ApplyChanges::parsedTreeApply(class AddSimpleAssign *assignment)
{
  if (!currentPassIs(2)) return 0;
  Array *assign_list = new Array (1) ;

  //printf("AddSimpleAssign\n");

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddSimpleAssign): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  VeriExpression *lhs = new VeriIdRef(Strings::save(assignment->getLeftHandSide().c_str()));
  VeriExpression *rhs = new VeriIdRef(Strings::save(assignment->getRightHandSide().c_str()));

  VeriNetRegAssign *assign = new VeriNetRegAssign(lhs, rhs);

  assign_list->InsertLast(assign);

  VeriContinuousAssign  *continuous_assignment = new  VeriContinuousAssign(0, 0, assign_list) ;

  // Resolve reference, links VeriIdRef to VeriIdDef
  continuous_assignment->Resolve(m_module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV) ;

  m_module->AddModuleItem(continuous_assignment);

  return 1;
}

int ApplyChanges::textBasedApply(class AddSimpleAssign *assignment)
{
  if (!currentPassIs(2)) return 0;

  //printf("AddSimpleAssign\n");

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddSimpleAssign): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  string assign = "  assign " + assignment->getLeftHandSide() + " = " +
    assignment->getRightHandSide() + ";\n";

  linefile_type lf = m_module->Linefile();
  linefile_type end_lf = (lf) ? lf->Copy() : 0 ;
  if (end_lf) {
    // Set the start line/col to be at 'e' of "endmodule" keyword:
    end_lf->SetLeftLine(end_lf->GetRightLine()) ;
    end_lf->SetLeftCol(end_lf->GetRightCol()-9) ;
  }
    
  m_tbdm.InsertBefore(end_lf, assign.c_str());

  return 1;
}

int ApplyChanges::parsedTreeApply(class AddInstance *instAction)
{
  if (!currentPassIs(2)) return 0;
  VeriModuleInstantiation *moduleInst;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddInstance): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Construct the param values array
  ParameterListIterator pit;
  ParameterList &list = instAction->getParamList();
  Array *param_values = new Array;

  for(pit = list.begin(); pit != list.end(); pit++) {
    
    VeriExpression *newparam = new VeriPortConnect(Strings::save((*pit).m_name.c_str()),
						   new VeriIntVal(atoi((*pit).m_value.c_str())));
    if (newparam == NULL) {
      cerr << "Error ApplyChanges::parsedTreeApply(AddInstance): cannot add param "
	   << (*pit).m_name.c_str() << " on module "
	   << m_module->Name() << "."
	   << endl;
      return 0;
    }
    param_values->InsertLast(newparam);
  }

  // Construct the inst (VeriInstId)
  VeriInstId *inst;
  inst = new VeriInstId(Strings::save(instAction->getLocalInstanceName().c_str()), 0, 0);


  // Convert 'instantiated_component_name' into a VeriName
  VeriIdRef *instantiated_module = 
    new VeriIdRef(Strings::save(instAction->getModuleName().c_str()));

  // Construct VeriModuleInstantiation
  moduleInst = new VeriModuleInstantiation(instantiated_module, 0,
					   param_values, inst, 0);

  if (moduleInst) {
    m_module->AddModuleItem(moduleInst);
    m_module_list.push_back(m_module);  // Put module in a list to be written out later
  }
  else {
    cerr << "Error ApplyChanges::parsedTreeApply(AddInstance): failed adding instance "
	 << instAction->getLocalInstanceName().c_str() << " to module "
	 << instAction->getModuleName().c_str() << endl;

    return 0;
  }

  // Add new terminal from modification specifications
  ModuleTerminalIterator mterm_it;
  unsigned ret;

  // Get the style of the port print
  VariablePrintStyle style = HdlUtils::getPortConnectsPrintStyle(inst);

  ModuleTerminalList &mterm_list = instAction->getPortList();
  for(mterm_it = mterm_list.begin(); mterm_it != mterm_list.end(); mterm_it++) {

    string portname = (*mterm_it).m_portName;
    string netname = (*mterm_it).m_netName;

    if (style == HdlKeyValuePairStyle) {
      ret = HdlUtils::addPortRefKeyValue(m_module, inst, portname,
					 netname);
    }                      
    else if (style == HdlPositionalStyle) {
      ret = HdlUtils::addPortRefPositional(m_module, inst, netname);
    }
    else ret = 0;

    if (!ret)
      cerr << "Error ApplyChanges::parsedTreeApply(AddInstance): failed adding portref "
	   << (*mterm_it).m_portName.c_str() << " to instance "
	   << instAction->getLocalInstanceName().c_str() << " in module "
	   << instAction->getModuleName().c_str() << endl;
  }

  return 1;
}

int ApplyChanges::textBasedApply(class AddInstance *instAction)
{
  if (!currentPassIs(3)) return 0;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(AddInstance): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  BString key = instAction->getInstName();
  key += "/";
  key += instAction->getLocalInstanceName();

  AddInstanceMap::iterator add_iter = m_add_map.find(key);

  if(add_iter != m_add_map.end()) {
    printf("Instance %s already added. Skipping AddInstance.\n", key.c_str());
    return 0;
  }

  m_add_map[key] = instAction;

  ChangeInstModuleNameMap::iterator iter = m_change_map.find(key);

  string modname      = instAction->getModuleName();
  unsigned int suffix = instAction->getSuffix();
  if(iter == m_change_map.end()) {
  } else {
    ChangeInstModuleName* c = (*iter).second;
    modname = c->getName();
    suffix  = c->getSuffix();
  }

  if (suffix > 0) {

    std::map<unsigned int, unsigned int>::iterator iter = m_key_map.find(suffix);

    if(iter == m_key_map.end())
      {
	modname += "_";
	modname += itoa(suffix);
      }
    else
      {
	unsigned int mapped = (*iter).second;
	modname += "_";
	modname += itoa(mapped);
      }
  }

  // Module instantiation statement
  string addinst = "\n  " + modname;

  // Add new terminal from modification specifications
  ModuleTerminalIterator mterm_it;
  ParameterListIterator param_it;
  unsigned ret = 1;

  // Get the style of the port print
  VariablePrintStyle style = HdlKeyValuePairStyle;

  // Parameter list
  ParameterList &param_list = instAction->getParamList();
  int firstTime = 1;
  for(param_it = param_list.begin(); param_it != param_list.end(); param_it++) {

    string paramname = (*param_it).m_name;
    string paramvalue = (*param_it).m_value;

    if (firstTime) {

      addinst += " #(";
      firstTime = 0;
    } else {
      addinst += ", ";
    }

    if (instAction->getOnePortPerLine() == true)
      addinst += "\n    ";

    if (style == HdlKeyValuePairStyle) {
      addinst += "." + paramname + "(" + paramvalue + ")";
    }                      
    else if (style == HdlPositionalStyle) {
      addinst += paramvalue;
    }
    else ret = 0;
    
    if (!ret)
      cerr << "Error ApplyChanges::textBasedApply(AddInstance): failed adding parameter "
	   << (*param_it).m_name.c_str() << " to instance "
	   << instAction->getLocalInstanceName().c_str() << " in module "
	   << instAction->getModuleName().c_str() << endl;
  }
  if (firstTime == 0)
    addinst += ")";
  addinst += " " + instAction->getLocalInstanceName();

  // Terminal list
  ModuleTerminalList &mterm_list = instAction->getPortList();
  firstTime = 1;
  for(mterm_it = mterm_list.begin(); mterm_it != mterm_list.end(); mterm_it++) {

    string portname = (*mterm_it).m_portName;
    string netname = (*mterm_it).m_netName;

    if (firstTime) {

      addinst += " (";
      firstTime = 0;
    } else {
      addinst += ", ";
    }

    if (instAction->getOnePortPerLine() == true)
      addinst += "\n    ";

    if (style == HdlKeyValuePairStyle) {
      addinst += "." + portname + "(" + netname + ")";
    }                      
    else if (style == HdlPositionalStyle) {
      addinst += netname;
    }
    else ret = 0;
    
    if (!ret)
      cerr << "Error ApplyChanges::textBasedApply(AddInstance): failed adding portref "
	   << (*mterm_it).m_portName.c_str() << " to instance "
	   << instAction->getLocalInstanceName().c_str() << " in module "
	   << instAction->getModuleName().c_str() << endl;
  }

  if (firstTime == 0)
    addinst += ");\n\n";
  else
    addinst += "();\n\n";


  linefile_type lf = m_module->Linefile();
  linefile_type end_lf = (lf) ? lf->Copy() : 0 ;
  if (end_lf) {
    // Set the start line/col to be at 'e' of "endmodule" keyword:
    end_lf->SetLeftLine(end_lf->GetRightLine()) ;
    end_lf->SetLeftCol(end_lf->GetRightCol()-9) ;
  }

  m_tbdm.InsertBefore(end_lf, addinst.c_str());

  return 1;
}

linefile_type ApplyChanges::getLastComment(VeriModule *module)
{
  Array *comments = module->GetComments();

  VeriCommentNode *last_comment = (comments && comments->Size()) ?
    (VeriCommentNode*)comments->GetLast() : 0 ;

  if (!last_comment)
    return 0;

  linefile_type comment_linefile = last_comment->EndingLinefile();

  // Get the starting location of first module item
  linefile_type ending_linefile = module->EndingLinefile() ;

  //cerr << "comment lineno: " << comment_linefile->GetRightLine() << " moduleitem lineno: "
  //     << ending_linefile->GetRightLine() << endl;
  //cerr << "comments size: " << comments->Size() << endl;
  //cerr << "comment: " << m_tbdm.GetText(comment_linefile, 1);

  if (comment_linefile->GetRightLine() >= ending_linefile->GetRightLine())
    if (comments->Size() >= 2) {
      //cerr << "here" << endl;
      last_comment = (VeriCommentNode*)comments->At(comments->Size()-2);
      //cerr << "last comment address " << (void*)last_comment << endl;

      //cerr << "last comment lineno: " << last_comment->EndingLinefile()->GetRightLine()
      //   << endl;
    }

  if (last_comment)
    return last_comment->EndingLinefile();

  return 0;
}

int ApplyChanges::parsedTreeApply(class RmInstance *instAction)
{
  if (!currentPassIs(2)) return 0;
  unsigned val;

  //printf("RmInstance\n");

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(RmInstance): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  val = m_module->RemoveInstance(instAction->getLocalInstanceName().c_str());

  if (val) {
    m_module_list.push_back(m_module);
  }
  else {
    cerr << "Error ApplyChanges::parsedTreeApply(RmInstance): failed removing instance "
	 << instAction->getLocalInstanceName().c_str() << " to module "
	 << m_module->Name() << endl;

    return 0;
  }

  return 1;
}

int ApplyChanges::textBasedApply(class RmInstance *instAction)
{
  if (!currentPassIs(2)) return 0;
  
  //printf("RmInstance\n");

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::textBasedApply(RmInstance): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Get the instance identifier
  VeriIdDef *instance_id = m_module->FindDeclared(instAction->getLocalInstanceName().c_str());
  //VeriInstId *inst;

  // No instance named 'instance_name' exists
  if (!instance_id || !instance_id->IsInst()) {
    cerr << "Error ApplyChanges::textBasedApply(RmInstance): instance "
	 << instAction->getLocalInstanceName().c_str() 
	 << " of module "
	 << m_module->Name() << " is not found."
	 << endl;
    return 0 ;
  }
  //else
  //  inst = static_cast<class VeriInstId*>(instance_id);


  VeriModuleInstantiation *moduleInst = instance_id->GetModuleInstance();

  // Get the file position
  linefile_type start_linefile = moduleInst->StartingLinefile();
  //linefile_type end_linefile = moduleInst->EndingLinefile();
  
  // Remove the statement
  m_tbdm.Remove(start_linefile);

  return 1;
}

int ApplyChanges::parsedTreeApply(class RmBody *action)
{
  if (!currentPassIs(2)) return 0;

  return 0;
}

int ApplyChanges::textBasedApply(class RmBody *action)
{
  if (!currentPassIs(3)) return 0;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::textBasedApply(RmBody): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  linefile_type lf = m_module->Linefile();
  linefile_type end_lf = (lf) ? lf->Copy() : 0 ;
  if (end_lf) {
    end_lf->SetLeftLine(end_lf->GetRightLine()) ;
  }
  m_tbdm.InsertBefore(end_lf, "");

  lf = m_module->Linefile();
  end_lf = (lf) ? lf->Copy() : 0 ;
  if (end_lf) {
    // Set the start line/col to be at 'e' of "endmodule" keyword:
    end_lf->SetRightCol(0) ;
  }
  m_tbdm.Remove(m_last_port_linefile, end_lf);

  m_tbdm.InsertAfter(end_lf, "endmodule");

  return 1;
}

int ApplyChanges::parsedTreeApply(class RmReg *action)
{
  if (!currentPassIs(2)) return 0;
  return 0;
}

int ApplyChanges::textBasedApply(class RmReg *action)
{
  if (!currentPassIs(2)) return 0;

  //printf("In textBasedApply for RmReg\n");

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(RmReg): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  //printf("MODULE %s\n", m_module->Name());

  //printf("Now looking for signal %s\n", action->getRegName().c_str());
  VeriIdDef *id = HdlUtils::findSignalInModuleByName(m_module, action->getRegName());
  if (id && id->IsReg()) {

    linefile_type start_linefile = id->StartingLinefile();
    linefile_type end_linefile = start_linefile->Copy();
    start_linefile->SetLeftCol(0);
    end_linefile->SetRightLine(end_linefile->GetRightLine()+1);
    end_linefile->SetRightCol(1);
    m_tbdm.Remove(start_linefile, end_linefile);
  }
  else
    printf("Not found\n");

  return 1;
}

int ApplyChanges::parsedTreeApply(class RmNet *action)
{
  if (!currentPassIs(2)) return 0;
  return 0;
}

int ApplyChanges::parsedTreeApply(class RmSimpleAssign *action)
{
  if (!currentPassIs(2)) return 0;
  return 0;
}

int ApplyChanges::textBasedApply(class RmSimpleAssign *action)
{
  if (!currentPassIs(2)) return 0;

  //printf("In textBasedApply for RmNet\n");

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(RmSimpleAssign): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  Array *items = m_module->GetModuleItems() ;
  unsigned it;
  VeriModuleItem *item;
  //VeriIdRef *ref ;
  BString lstring, rstring, expstring;
  std::stringstream st;
  
  FOREACH_ARRAY_ITEM(items, it, item) {
    if (item->GetClassId() == ID_VERICONTINUOUSASSIGN) {
      VeriContinuousAssign *cont_assign = dynamic_cast<VeriContinuousAssign *>(item) ;
      Array *assigns = (cont_assign) ? cont_assign->GetNetAssigns() : 0 ;
      if (!assigns) continue;
      
      SignalVisitor v ;
      unsigned i ;
      VeriNetRegAssign *assign ;
      FOREACH_ARRAY_ITEM(assigns, i, assign) {
	if (!assign) continue ;
	//printf("\nAssign statement: ");
	//assign->PrettyPrint(cout, 0);
	//cout << endl;
	VeriExpression *lval = assign->GetLValExpr() ;
	VeriExpression *rval = assign->GetRValExpr() ;
	if ((lval == NULL) || (rval == NULL))
	  continue;
	
	// Get the lhs value
	lval->Accept(v);
	st.str("");
	lval->PrettyPrint(st, 0);
	lstring = st.str();
	v.ResetSignals();
	
	if (lstring == action->getNetName()) {
	  linefile_type start_linefile = item->StartingLinefile();
	  linefile_type end_linefile = start_linefile->Copy();
	  start_linefile->SetLeftCol(0);
	  end_linefile->SetRightLine(end_linefile->GetRightLine()+1);
	  end_linefile->SetRightCol(1);
	  m_tbdm.Remove(start_linefile, end_linefile);
	}
      }
    }
  }
  return 0;
}

int ApplyChanges::textBasedApply(class RmNet *action)
{
  if (!currentPassIs(2)) return 0;

  //printf("In textBasedApply for RmNet\n");

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(RmNet): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Get the scope of this module :
  VeriScope *module_scope = m_module->GetScope() ;
  
  // Now iterate over the declared identifiers ((VeriIdDef*s) in the hash table (scope->DeclArea()) in this scope :
  MapIter mi ;
  VeriIdDef *id ;
  char *id_name ; 
  FOREACH_MAP_ITEM(module_scope->DeclArea(), mi, &id_name, &id) {

    // Rule out all identifiers declared here, except for 'nets' :
    if (!(id->IsNet() || id->IsReg())) continue ;
    
    if (action->getNetName() == id->Name()) {

      printf("RmNet Found signal %s\n", action->getNetName().c_str());
      linefile_type start_linefile = id->StartingLinefile();
      linefile_type end_linefile = start_linefile->Copy();
      start_linefile->SetLeftCol(0);
      end_linefile->SetRightLine(end_linefile->GetRightLine()+1);
      end_linefile->SetRightCol(1);
      m_tbdm.Remove(start_linefile, end_linefile);
    }
  }

  Array *items = m_module->GetModuleItems() ;
  unsigned it;
  VeriModuleItem *item;
  //VeriIdRef *ref ;
  BString lstring, rstring, expstring;
  std::stringstream st;
  
  FOREACH_ARRAY_ITEM(items, it, item) {
    if (item->GetClassId() == ID_VERICONTINUOUSASSIGN) {
      VeriContinuousAssign *cont_assign = dynamic_cast<VeriContinuousAssign *>(item) ;
      Array *assigns = (cont_assign) ? cont_assign->GetNetAssigns() : 0 ;
      if (!assigns) continue;
      
      SignalVisitor v ;
      unsigned i ;
      VeriNetRegAssign *assign ;
      FOREACH_ARRAY_ITEM(assigns, i, assign) {
	if (!assign) continue ;
	//printf("\nAssign statement: ");
	//assign->PrettyPrint(cout, 0);
	//cout << endl;
	VeriExpression *lval = assign->GetLValExpr() ;
	VeriExpression *rval = assign->GetRValExpr() ;
	if ((lval == NULL) || (rval == NULL))
	  continue;
	
	// Get the lhs value
	lval->Accept(v);
	st.str("");
	lval->PrettyPrint(st, 0);
	lstring = st.str();
	v.ResetSignals();
	
	if (lstring == action->getNetName()) {
	  linefile_type start_linefile = item->StartingLinefile();
	  linefile_type end_linefile = start_linefile->Copy();
	  start_linefile->SetLeftCol(0);
	  end_linefile->SetRightLine(end_linefile->GetRightLine()+1);
	  end_linefile->SetRightCol(1);
	  m_tbdm.Remove(start_linefile, end_linefile);
	}
      }
    }
  }
  return 0;
}

int ApplyChanges::parsedTreeApply(class ChangeAssignRHS *action)
{
  if (!currentPassIs(2)) return 0;
  return 0;
}

int ApplyChanges::textBasedApply(class ChangeAssignRHS *action)
{
  if (!currentPassIs(2)) return 0;
 
  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(RmNet): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  Array *items = m_module->GetModuleItems() ;
  unsigned it;
  VeriModuleItem *item;
  VeriIdRef *ref ;
  BString lstring, rstring, expstring;
  std::stringstream st;

  FOREACH_ARRAY_ITEM(items, it, item) {
    if (item->GetClassId() == ID_VERICONTINUOUSASSIGN) {
      VeriContinuousAssign *cont_assign = dynamic_cast<VeriContinuousAssign *>(item) ;
      Array *assigns = (cont_assign) ? cont_assign->GetNetAssigns() : 0 ;
      if (!assigns) continue;
      
      SignalVisitor v ;
      unsigned i ;
      VeriNetRegAssign *assign ;
      FOREACH_ARRAY_ITEM(assigns, i, assign) {
	if (!assign) continue ;
	VeriExpression *lval = assign->GetLValExpr() ;
	VeriExpression *rval = assign->GetRValExpr() ;
	if ((lval == NULL) || (rval == NULL))
	  continue;

	// Get the lhs value
	lval->Accept(v);
	st.str("");
	lval->PrettyPrint(st, 0);
	lstring = st.str();
	v.ResetSignals();

	// If left hand side is not the one we want then go to the next
	if (lstring != action->getLHS()) continue;

	// Now do the rhs
	rval->Accept(v);
	if (v.NumSignals() < 1) { // Const case
	  if (rval->IsConst() || rval->IsConstExpr()) {
	    st.str("");
	    rval->PrettyPrint(st, 0);
	    rstring = st.str();
	    if (rstring == action->getRHS()) {
	      linefile_type start_linefile = rval->StartingLinefile();
	      m_tbdm.Replace(start_linefile, action->getNewRHS().c_str());
	    }
	  }
	  else {
	    st.str("");
	    assign->PrettyPrint(st, 0);
	    assign->Info("Something wrong, unrecognized Assignment Statement: %s",
			 st.str().c_str());
	  }
	}
	else if (v.NumSignals() == 1) { // 
	  FOREACH_ARRAY_ITEM(v.GetSignals(), i, ref) {
	    st.str("");
	    ref->PrettyPrint(st, 0);
	    rstring = st.str();
	  }
	}
	else {

	  st.str("");
	  rval->PrettyPrint(st, 0);
	  expstring = st.str();
	  printf("RHS multiple signals\n");

	  FOREACH_ARRAY_ITEM(v.GetSignals(), i, ref) {
	    st.str("");
	    ref->PrettyPrint(st, 0);
	    rstring = st.str();
	  }
	}
	v.ResetSignals() ;
      }
    }
  }


  //linefile_type start_linefile = id->StartingLinefile();
  //m_tbdm.Replace(start_linefile, action->getNewRHS().c_str());
  
  return 0;
}

int ApplyChanges::parsedTreeApply(class RmCode *instAction)
{
  if (!currentPassIs(2)) return 0;
  return 0;
}

int ApplyChanges::textBasedApply(class RmCode *instAction)
{
  if (!currentPassIs(2)) return 0;

  const VeriModuleItem* item = instAction->getItem();
  linefile_type start_linefile = item->StartingLinefile();
  m_tbdm.InsertBefore(start_linefile, "/* ");
  m_tbdm.InsertAfter(start_linefile, "*/ ");
  return 0;
}

int ApplyChanges::parsedTreeApply(class UpdateParam *modification)
{
  if (!currentPassIs(2)) return 0;

  //printf("UpdateParam: %s %s\n", modification->getName().c_str(),
  //modification->getValue().c_str());

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(UpdateParam): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Get the module item list of the module
  VeriIdDef *param = m_module->GetParam(modification->getName().c_str());
    
  if (param == 0) {
    cerr << "Error ApplyChanges::parsedTreeApply(UpdateParam): something wrong, "
         << "param " << modification->getName() << " does not exist in module "
	 << m_module->Name() << "." << endl;
    return 0;
  }

  VeriConst *expr = HdlUtils::mkVeriConst(modification->getValue());

  if (expr)
    param->SetInitialValue(expr);
  else
    return 0;
  
  return 1;
}

int ApplyChanges::textBasedApply(class UpdateParam *modification)
{
  if (!currentPassIs(2)) return 0;

  //printf("UpdateParam: %s %s\n", modification->getName().c_str(),
  // modification->getValue().c_str());
  
  if (m_module == NULL) {
    cerr << "Error ApplyChanges::textBasedApply(UpdateParam): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // Get the module item list of the module
  VeriIdDef *param = m_module->GetParam(modification->getName().c_str());
    
  if (param == 0) {
    cerr << "Error ApplyChanges::parsedTreeApply(UpdateParam): param "
         << modification->getName() << " does not exist in module "
	 << m_module->Name() << "." << endl;
    return 0;
  }

  VeriExpression *expr = param->GetInitialValue();

  // Get the file position
  linefile_type start_linefile = expr->StartingLinefile();
  linefile_type end_linefile = expr->EndingLinefile();
  
  // Replace the text in the file
  m_tbdm.Replace(start_linefile, end_linefile, modification->getValue().c_str());

  return 1;
}

void ApplyChanges::clearModifications(VeriModule *module)
{
  linefile_type start_linefile = module->StartingLinefile();
  unsigned file_id = start_linefile->GetFileId();

  m_tbdm.ClearModifications(file_id);
}

string ApplyChanges::writeTextBasedChanges(VeriModule *module, BString & path, ModSet & modset, bool & is_top)
{
  string out_filename;
  std::ofstream out_file;

  // If this is not text based mode then return
  if (m_textbased == 0)
    return "";

  // First try to open the directory
  DIR *dp;
  if((dp  = opendir(m_output_dir.c_str())) == NULL) {
    if(mkdir(m_output_dir.c_str(), 0777) != 0) {
      cerr << "Error opening directory " << m_output_dir << endl;
      return "";
    }
  }
  else
    closedir(dp);

  string new_module_name = m_new_modname[m_module];

  linefile_type module_lf = module->StartingLinefile();
  linefile_type begin_lf;
  linefile_type prev_mod_lf = module->GetPreviousModuleLineFile();
  if (prev_mod_lf) {
    begin_lf = prev_mod_lf->Copy();

    //Set the start line/col to be at the end of prev module:
    if (prev_mod_lf->GetRightLine() == 1)
      begin_lf->SetLeftLine(1);
    else
      begin_lf->SetLeftLine(prev_mod_lf->GetRightLine()+1);
    begin_lf->SetLeftCol(1);
    begin_lf->SetRightLine(module_lf->GetLeftLine());
    begin_lf->SetRightCol(1);
    
    //cerr << "Here is content from the end of prev module to this module: " << module->Name() << endl;
    //cerr << m_tbdm.GetText(begin_lf, 0);
    
    //cerr << "End of content\n";

  } else { // This is the first module in the file
    begin_lf = module_lf->Copy();
    //Set to the start of the file
    begin_lf->SetLeftLine(1);
    begin_lf->SetLeftCol(1);
    begin_lf->SetRightLine(module_lf->GetLeftLine());
    begin_lf->SetRightCol(1);
    
    //cerr << "Here is content from the beginning of the file to this module: " << module->Name() << endl;
    //cerr << m_tbdm.GetText(begin_lf, 0);

    //cerr << "End of beginning section\n";
  }

  VeriIdDef *id = m_module->GetId();
  linefile_type id_linefile = id->StartingLinefile();

  ModSetPath pair  = ModSetPath(path, modset);
  std::map<ModSetPath, unsigned int>::iterator iter = m_path_map.find(pair);

  unsigned int key;
  if (iter == m_path_map.end())
    key = 0;
  else
    key = (*iter).second;

  // printf("MOD: %d %s\n", key, name_current);

//   const boost::regex rpattern(name_current);

//  m_tbdm.Remove(id_linefile);

  char *comment = m_tbdm.GetText(begin_lf, 0);

  //printf("writeTextBasedChanges comment: %s\n", comment);
  //printf("writeTextBasedChanges %s\n", out_filename.c_str());
  char *file_content = m_tbdm.GetText(module_lf, 1);

 //  string generic = boost::regex_replace(string(file_content), rpattern, string(" module_|XXX"));

  bool is_new = true;

  if (key != 0) {

    is_new = false;

    std::map<string, unsigned int>::iterator iter = m_content_map.find(file_content);

    if(iter == m_content_map.end())
      {
	is_new = true;
	m_content_map[file_content] = key;
	if (!is_top) {
	  std::string suffix = "_";
	  suffix += itoa(key);
	  m_tbdm.Replace(id_linefile, suffix.c_str());
	}
	// printf("MISS: %d\n", key);
      }
    else
      {
	int match_key = (*iter).second;
	//	printf("HIT! %d\n", match_key);
	m_key_map[key] = match_key;
      }
  }

  if (is_new) 
    {
      char *file_content = m_tbdm.GetText(module_lf, 1);

      out_filename = m_output_dir + "/" + new_module_name;
      if (!is_top && key != 0) {
	out_filename += "_";
	out_filename += itoa(key);
      }
      BString fn = module_lf->GetFileName();
      if (fn.find(".sv") != string::npos) {
	out_filename += ".sv";
      }
      else {
	out_filename += ".v";
      }

      // Write out the new content
      out_file.open(out_filename.c_str(), ios_base::out);

      out_file << comment;
      out_file << file_content;
      out_file << "\n";
      out_file.close();

      return out_filename;

    } 
  else 
    {
      return string("");
    }
}


void ApplyChanges::writeParsedTreeChanges(BStringList &written_filenames,
					  const char *suffix)
{
  VeriModule *module;
  string outfile_name;
  std::ofstream out_file;

  //printf("writeChanges\n");

  // If this is not parsed tree mode then return
  if (m_textbased)
    return;

  // First try to open the directory
  DIR *dp;
  if((dp  = opendir(m_output_dir.c_str())) == NULL) {
    if(mkdir(m_output_dir.c_str(), 0777) != 0) {
      cerr << "Error opening directory " << m_output_dir << endl;
      return;
    }
  }
  else
    closedir(dp);

  // Delete duplicate VeriModule in the list
  m_module_list.unique();
  
  VeriModuleListIterator it;
  for (it = m_module_list.begin(); it != m_module_list.end(); it++) {
    
    module = (*it);
    
    if (out_file.is_open())
      out_file.close();
    
    outfile_name = m_output_dir;
    outfile_name += "/";
    outfile_name += module->GetName();
    if (suffix)
      outfile_name += suffix;
    if (module->IsSystemVeri())
      outfile_name += ".sv";
    else {
      outfile_name += ".v";
    }
    // Pass back the filename
    written_filenames.push_back(outfile_name);
    
    if (already_written[module] == 0) {
      out_file.open(outfile_name.c_str(), ios_base::out);
      already_written[module] = 1;
    } else
      out_file.open(outfile_name.c_str(), ios_base::app);
    
    if (!out_file.is_open())
      cerr << "Output file " << outfile_name.c_str() << " cannot be opened." << endl;
    
    module->PrettyPrint(out_file, 0);
    out_file.close();
  }
  
  //} else {  // Text based manipulation
  
  // Overwrite all the modified files: if they do not belong to the secure diretory this
  // will not overwrite the file, instead will generate an warning message, otherwise it
  // will overwrite the file as usual.
  //m_tbdm.WriteDesign(0, m_output_dir.c_str());
  
  // Remove all analyzed modules
  //veri_file::RemoveAllModules() ;
  //}
}

void ApplyChanges::setModuleContext(VeriModule *module)
{
  m_module = module;
  m_pass = 0;
  setLastPortLinefile();

  //Array *comments = module->GetComments();
  
  //VeriCommentNode *c;
  //unsigned i;
  
  //c = (VeriCommentNode*)comments->GetFirst();

  //linefile_type begin_lf = c->StartingLinefile();
  //linefile_type module_lf = module->StartingLinefile();
  //linefile_type end_lf = begin_lf->Copy();

  // Set the start line/col to be at 'e' of "endmodule" keyword:
  //end_lf->SetRightLine(module_lf->GetLeftLine()) ;
  //end_lf->SetRightCol(module_lf->GetLeftCol()) ;

  //cerr << m_tbdm.GetText(end_lf, 1);


  //FOREACH_ARRAY_ITEM(comments, i, c) {
    
  //  if (c) {
  //    linefile_type lf = c->Linefile();
      
  //    cerr << endl;
  //    cerr << "comment " << i << ": " << m_tbdm.GetText(lf, 1);
  //  }
  //}
}

void ApplyChanges::setLastPortLinefile()
{
  // To insert input declaration in file as the first module item we have to get handle
  // to module items and location of starting point of existing first module item.

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::setLastPortLinefile(): something wrong, "
         << "module was not previously set."
	 << endl;
    return;
  }
  
  m_last_port_linefile = m_last_declare_linefile = NULL;
  
  Array *ports = m_module->GetPorts();
  VeriIdDef *portid;
  unsigned lineno;
  unsigned maxportlineno = 0;
  unsigned lastmodlineno;
  int i;
  linefile_type port_linefile;
  linefile_type module_linefile;
  //linefile_type item_linefile;
  
  module_linefile = m_module->EndingLinefile();
  lastmodlineno = module_linefile->GetRightLine();

  // Find the line number of the last port
  FOREACH_ARRAY_ITEM(ports, i, portid) {
    
    if (portid) {
      port_linefile = portid->StartingLinefile() ;
      
      if (port_linefile) {
	lineno = port_linefile->GetLeftLine();
	if ((lineno < lastmodlineno) && (lineno > maxportlineno))
	  maxportlineno = lineno;
      }
    }
  }
    
  // Find the line number of the last declaration
  //  Declaration is either; Port, Net, Reg, or Param.
  Array *items = m_module->GetModuleItems() ;
  VeriModuleItem *item = NULL;
  
  /*
    FOREACH_ARRAY_ITEM(items, i, item) {
    
    if (item->GetId()->IsPort() || item->GetId()->IsNet() || item->GetId()->IsReg()
    || item->GetId()->IsParam()) {
    
    item_linefile = item->StartingLinefile() ;
    lineno = item_linefile->GetLeftLine();
    if (lineno > maxdeclarelineno)
    maxdeclarelineno = lineno;
    }
    }
  */

  unsigned min_item_lineno = 99999999;
  //unsigned min_declare_lineno = 99999999;
  
  FOREACH_ARRAY_ITEM(items, i, item) {
    
    // Find the linefile right after the last port
    if ((lineno = item->StartingLinefile()->GetLeftLine()) > maxportlineno) {
      if (lineno < min_item_lineno) {
	min_item_lineno = lineno;
	
	// This is the location of first item after the last existing ports
	m_last_port_linefile = item->StartingLinefile() ;
	break;
      }
    }
    
    // Now find the linefile right after last declaration
    /*
      if ((item->GetId()->IsPort() || item->GetId()->IsNet() || item->GetId()->IsReg()
      || item->GetId()->IsParam()) && (lineno > maxdeclarelineno)) {
      if (lineno < min_declare_lineno) {
      min_declare_lineno = lineno;
      
      // This is the location of first item after the last existing ports
      m_last_declare_linefile = item->StartingLinefile() ;
      }
      }
    */
  }
  
  if (!item) {
    cerr << "Error ApplyChanges::setLastPortLinefile(): "
	 << "cannot find any module item in this module "
	 << m_module->Name() << "." << endl;
    return;
  }
}

int ApplyChanges::parsedTreeApply(class RenameSignal *instAction)
{
  if (!currentPassIs(2)) return 0;
  //  unsigned val;

  if (m_module == NULL) {
    cerr << "Error ApplyChanges::parsedTreeApply(RenameSignal): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  // no-op for now
//   val = m_module->RemoveAssignment(instAction->getLocalAssignmentName().c_str());

//   if (val) {
//     m_module_list.push_back(m_module);
//   }
//   else {
//     cerr << "Error ApplyChanges::parsedTreeApply(RenameSignal): failed removing instance "
// 	 << instAction->getLocalAssignmentName().c_str() << " to module "
// 	 << m_module->Name() << endl;

//     return 0;
//   }

  return 1;
}

int ApplyChanges::textBasedApply(class RenameSignal *instAction)
{
  if (!currentPassIs(2)) return 0;
  
  if (m_module == NULL) {
    cerr << "Error ApplyChanges::textBasedApply(RenameSignal): something wrong, "
         << "module was not previously set."
	 << endl;
    return 0;
  }

  if (instAction->includeDecls()) {

    textRenameSignal(m_module, instAction->getName(), instAction->getNameNew());

  } else {

    Array *items = m_module->GetModuleItems() ;
    VeriModuleItem *item = NULL;

    int i;
    FOREACH_ARRAY_ITEM(items, i, item) {
      if (item->IsDataDecl()) continue;
      textRenameSignal(item, instAction->getName(), instAction->getNameNew());
    }

    VeriIdDef* def = HdlUtils::findSignalInModuleByName(m_module, instAction->getName());

    unsigned int size = HdlUtils::getSignalSize(def);

    if (def->IsReg()) {
      textAddSignal(m_module, instAction->getNameNew(), size, VERI_REG);
    } else {
      textAddSignal(m_module, instAction->getNameNew(), size, VERI_WIRE);
    }
  }
  return 1;
}

void ApplyChanges::textRenameSignal(VeriTreeNode* node, const BString & name, const BString & name_new)
{
  
  IdRefVisitor ref_visitor(name);
  node->Accept(ref_visitor);

  VeriIdRef* ref;
  foreachSetRef(ref_visitor._refs, it) {
    ref = (VeriIdRef*) *it;
    printf("REF: %s\n", HdlUtils::getImage(ref));
    linefile_type start_linefile = ref->StartingLinefile();
    m_tbdm.Replace(start_linefile, name_new.c_str());
  }
}

void ApplyChanges::textAddSignal(VeriModule* module, const BString & name, const unsigned & size, const unsigned & type)
{

  BString netdecl = "";

  linefile_type lf = module->Linefile();
  linefile_type end_lf = (lf) ? lf->Copy() : 0 ;
  if (end_lf) {
    // Set the start line/col to be at 'e' of "endmodule" keyword:
    end_lf->SetLeftLine(end_lf->GetRightLine()) ;
    end_lf->SetLeftCol(end_lf->GetRightCol()-9) ;
  }
    
  // Call InsertBefore of 'DesignModDir' utility
  if (type == VERI_REG) {
    netdecl += "reg ";
  } else {
    netdecl += "wire ";
  }
  if (size > 1) {
    netdecl += "[";
    netdecl += itoa(size-1);
    netdecl += ":0] ";
  }
  netdecl += name;
  netdecl += ";\n";
  m_tbdm.InsertBefore(m_last_port_linefile, netdecl.c_str());

}

////////////////////////////////////////////////////////////////////////////////
//
////////////////////////////////////////////////////////////////////////////////

int ApplyChanges::addPathInfo(BString info)
{
  m_path_info += info;
  return 0;
}

int ApplyChanges::writePathInfo()
{

  std::string out_filename;
  std::ofstream out_file;

  // First try to open the directory
  DIR *dp;
  if((dp  = opendir(m_output_dir.c_str())) == NULL) {
    if(mkdir(m_output_dir.c_str(), 0777) != 0) {
      cerr << "Error opening directory " << m_output_dir << endl;
      return 1;
    }
  }
  else
    closedir(dp);

  out_filename = m_output_dir + "/path.map";

  out_file.open(out_filename.c_str(), ios_base::out);

  out_file <<  m_path_info.c_str();
  out_file << "\n";
  out_file.close();

  return 0;

}


