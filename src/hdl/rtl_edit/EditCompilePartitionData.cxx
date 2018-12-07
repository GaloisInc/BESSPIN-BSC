#include <string.h>
//#include <math.h>
#include <iostream>
//#include <sstream>
#include <cstring>

#include "EditCompile.h"
#include "EditCompilePartition.h"
#include "HdlUtils.h"

#include "tcl.h"

#define MAX_LINE_SIZE 1023

size_t match_name(BString &portName, BString &specName);


const char *ConnBundle::getFromModule()
{
  return m_fromFpga->getModule()->Name();
}

const char *ConnBundle::getToModule()
{
  return m_toFpga->getModule()->Name();
}

int ConnBundle::getNextConn(ConnectionIterator &itr, unsigned int &fabricLSB,
			    unsigned int &fabricMSB, unsigned int width)
{
  //printf("GetNextConn\n");
  // If there is no more then return -1
  if (m_ConnItr == endConnection()) {
    //printf("EndConn\n");
    itr = endConnection();
    return -1;
  }
  
  // There is enough left in this channel
  if (m_BitsLeft > width) {
    fabricLSB = m_ConnItr->getWidth() - m_BitsLeft;
    fabricMSB = fabricLSB + width - 1;
    m_BitsLeft -= width;
    //printf("goodConn1\n");
    itr = m_ConnItr;
    return 0;
  }
  // There is not enough left in this channel
  else {
    int dutleft;

    fabricLSB = m_ConnItr->getWidth() - m_BitsLeft;
    fabricMSB = m_ConnItr->getWidth() - 1;
    //printf("goodConn2\n");
    itr = m_ConnItr;
    m_ConnItr++;
    dutleft = width - m_BitsLeft;
    m_BitsLeft = m_ConnItr->getWidth();
    /*
    if (m_OutDPConnItr == (m_OutDPCBListItr->second)->endConnection()) {
      m_OutDPCBListItr++;
      if (m_OutDPCBListItr != endOutDPConnBundle()) {
	m_OutDPConnItr = (m_OutDPCBListItr->second)->beginConnection();
	m_OutDPBitsLeft = m_OutDPConnItr->getWidth();
      }
    }
    */
    return dutleft;
  }

  itr = endConnection();
  return -1;
}

int ConnBundle::getNextConn(ConnectionIterator &itr, unsigned int &fabricLSB,
			    unsigned int &fabricMSB)
{
  unsigned int width;

  //printf("GetNextConn\n");
  // If there is no more then return -1
  if (m_ConnItr == endConnection()) {
    //printf("EndConn\n");
    itr = endConnection();
    return -1;
  }
  
  // There is enough left in this channel
  width = m_BitsLeft;
  if (m_BitsLeft > width) {
    fabricLSB = m_ConnItr->getWidth() - m_BitsLeft;
    fabricMSB = fabricLSB + width - 1;
    m_BitsLeft -= width;
    //printf("goodConn1\n");
    itr = m_ConnItr;
    return 0;
  }
  // There is not enough left in this channel
  else {
    int dutleft;

    fabricLSB = m_ConnItr->getWidth() - m_BitsLeft;
    fabricMSB = m_ConnItr->getWidth() - 1;
    //printf("goodConn2\n");
    itr = m_ConnItr;
    m_ConnItr++;
    dutleft = width - m_BitsLeft;
    if (m_ConnItr != endConnection())
      m_BitsLeft = m_ConnItr->getWidth();
    return dutleft;
  }

  itr = endConnection();
  return -1;
}

void ConnBundle::initializeConnections()
{
  //printf("Init bundle\n");
  m_ConnItr = beginConnection();
  if (m_ConnItr != endConnection())
    m_BitsLeft = m_ConnItr->getWidth();
  else
    m_BitsLeft = 0;
  m_BitsSoFar = 0;
}

PartitionData::PartitionData(const BString &name, PartitionSpec *spec)
{
  m_spec = spec;
  m_fpga = spec->getBoardSpec()->findFpgaByLetterSuffix(name[0]);
  m_default = 0;
  m_current_instance = NULL;
  m_containing_module = NULL;
}

const char *PartitionData::getModuleName()
{
  if (m_current_instance)
    return m_current_instance->GetInstantiatedModule()->Name();
  return NULL;
}

// Destructor
PartitionSpec::~PartitionSpec()
{
  PartitionDataIterator dItr;

  dItr = m_partitiondatas.begin();
  while (dItr != m_partitiondatas.end()) {

    delete dItr->second;
  }
}

PartitionData *PartitionSpec::findOrCreatePartitionData(const BString &name)
{
  PartitionData *partdata;
  PartitionDataIterator dItr;
  Fpga *fpga;
  
  dItr = m_partitiondatas.find(name);

  if (dItr == m_partitiondatas.end()) {
    partdata = new PartitionData(name, this);
    m_partitiondatas[name] = partdata;
    fpga = getBoardSpec()->findFpgaByLetterSuffix(name[0]);
    partdata->setFpga(fpga);
    fpga->assignPartitionData(partdata);
  }
  else
    partdata = dItr->second;

  return partdata;
}

// Insert instance into a partition
int PartitionSpec::insertSpecInstance(const BString &partname, const BString &instname)
{
  PartitionData *partdata;
  size_t stloc;

  partdata = findOrCreatePartitionData(partname);

  //printf("partdata: %p %s\n", partdata, instname.c_str());
  partdata->setHierInstanceName(instname);
  if (instname == "DEFAULT") {
    partdata->setDefault(1);
    m_default_partdata = partdata;
    return 1;
  }
  else if (partdata->setVerific() == 0)
    return 0;

  // Now set the containing module
  BString path;
  stloc = instname.find_last_of('/');
  path = instname.substr(0, stloc);
  if (path.length() < m_containing_module_path.length())
    m_containing_module_path = path;

  InstancePartitionMapIterator itr;

  itr = m_instpart_map.find(instname);
  if (itr == m_instpart_map.end()) {
    m_instpart_map[instname] = partname;
    partdata->insertInstance(instname);

    return 1;
  }

  return 0;
}

// Constructor given a spec file
PartitionSpec::PartitionSpec()
{
  m_board_spec = NULL;
  m_default_partdata = NULL;
  m_stat = 1;
  m_lineno = 0;
}

// do fabric
int PartitionSpec::parseFixedTerminals(std::ifstream &file, char *lineBuf)
{
  const char *delimiter = " \n\t:";
  char *token;
  BString portname, value, modname, partname;
  PartitionData *partdata;
  VeriModule *module;
  VeriIdDef *port;
  int status = 1;

  //printf("parseFixedTerminals1 %s %s\n", partname.c_str(), modname.c_str());
  token = strtok(0, delimiter);
  if (token)
    partname = token;
  else {
    cerr << "Error PartitionSpec::parsePartition(): blank fpga name on line "
	 << m_lineno << " of file " << m_module_specfile.c_str()
	 << "." << endl;
    status = 0;
  }

  //printf("parseFixedTerminals2 %s %s\n", partname.c_str(), modname.c_str());
  token = strtok(0, delimiter);
  if (token)
    modname = token;
  
  if (getBoardSpec()->findFpgaByLetterSuffix(partname[0]) == NULL) {
    cerr << "Error PartitionSpec::parsePartition(): invalid fpga name on line "
	 << m_lineno << " of file " << m_module_specfile.c_str()
	 << ". FPGA with the designation " << partname[0]
	 << " is not found. " << endl;
    status = 0;
  }

  partdata = findOrCreatePartitionData(partname);

  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    m_lineno++;
    token = strtok(lineBuf, delimiter);
    //printf("parsing1 line %d token %s\n", m_lineno, token);

    // Check comment
    if (token == NULL) continue;
    if (strncmp(token, "//", 2) == 0) continue;
    
    if (!strcmp(token, "port")) {
      portname = strtok(0, delimiter);
      value = strtok(0, delimiter);
      if (modname != "") {
	module = HdlUtils::findModuleByName(modname.c_str());
	if (module) {
	  port = HdlUtils::findPortOfModuleByName(module, portname);
	  if (port == NULL) {
	    size_t pos = portname.find_first_of('*');
	    if (pos == string::npos) {
	      cerr << "Error PartitionSpec::parseFixedTerminals(): port " << portname
		   << " of module " << modname << " does not exist." << endl;
	      status = 0;
	    }
	  }
	}
	partdata->addFixedTerminal(modname, portname, value);
	//printf("Found fixed %s modname %s\n", portname.c_str(), modname.c_str());
      }
      else {
	cerr << "Error PartitionSpec::parsePartition(): invalid module name on line "
	     << m_lineno << " of file " << m_module_specfile.c_str()
	     << ". FPGA with the designation " << partname[0]
	     << " has no module name. " << endl;
	status = 0;
      }
    } else if (!strcmp(token, "single-ended")) {
      portname = strtok(0, delimiter);
      value = "PLACE_HOLDER"; //strtok(0, delimiter);
      partdata->addSingleEndedTerminal(portname, value);
      //printf("Found single-ended %s\n", portname.c_str());
    } else if (!strcmp(token, "single-ended-data")) {
      portname = strtok(0, delimiter);
      value = "PLACE_HOLDER"; //strtok(0, delimiter);
      partdata->addSingleEndedDataTerminal(portname, value);
      //printf("Found single-ended-data %s\n", portname.c_str());
    }
    else if (!strcmp(token, "endmodule")) {
      return 1;
    } else {
      cerr << "Error PartitionSpec::parseFixedTerminals(): bad statement in module spec file on line "
	   << m_lineno << endl;
      status = 0;
    }
  }

  return status;
}

int PartitionSpec::parsePartition(std::ifstream &file, char *lineBuf)
{
  const char *delimiter = " \n\t,";
  char *token;
  BString fpga, inst, name, signal;
  PartitionData *partdata;
  int status = 1;

  //printf("herE\n");
  fpga = token = strtok(0, delimiter);
  //printf("herE1 %s\n", token);

  if (fpga == "") {
    cerr << "Error PartitionSpec::parsePartition(): invalid partition name on line "
	 << m_lineno << endl;
    status = 0;
  }

  if (getBoardSpec()->findFpgaByLetterSuffix(fpga[0]) == NULL) {
    cerr << "Error PartitionSpec::parsePartition(): invalid partition name on line "
	 << m_lineno << " of file " << m_module_specfile.c_str()
	 << ". FPGA with the designation " << fpga[0]
	 << " is not found. " << endl;
    status = 0;
  }


  // Read in each line.  Right now assuming each line consists of either:
  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    m_lineno++;
    token = strtok(lineBuf, delimiter);
    //printf("Token %s\n", token);

    if (token) {

      // Check comment
      if (token == NULL) continue;
      if (strncmp(token, "//", 2) == 0) continue;
    
      if (!strcmp(token, "assign")) {

	name = strtok(0, delimiter);
	signal = strtok(0, delimiter);
	partdata = findOrCreatePartitionData(fpga);
	partdata->addAssign(name.c_str(), signal.c_str());
      }
      else if (!strcmp(token, "instance")) {
	inst = strtok(0, delimiter);
	//printf("here instance %s %s\n", fpga.c_str(), inst.c_str());
	if (insertSpecInstance(fpga, inst) == 0)
	  return 0;
      }
      else if (!strcmp(token, "endpartition")) {
	return status;
      }
    }
  }
 
  return status;
}

int PartitionSpec::parseModuleSpecFile(const char *filename)
{
  const char *delimiter = " \n\t,:";
  std::ifstream file;
  char lineBuf[MAX_LINE_SIZE+1];
  char *token;
  int status = 1;

  //printf("Parsing design specfile %s...\n", filename);

  m_module_specfile = filename; 
  m_lineno = 0;

  file.open(filename);
  if (file.fail()) {
    std::cerr << "ERROR PartitionSpec::parseModuleSpecFile(): unable to open module spec file "
	      << filename
              << "." << std::endl;
    return 0;
  }

  // Read in each line.  Right now assuming each line consists of either:
  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    m_lineno++;
    token = strtok(lineBuf, delimiter);
    //printf("parsing line %d token %s\n", m_lineno, token);
    if (token && (strncmp(token, "//", 2) != 0)) { // Check comment
      //printf("Token %s\n", token);
      if (!strcmp(token, "module")) {
	
	// Do fixed terminals
	status = parseFixedTerminals(file, lineBuf);
      }
      else if (!strcmp(token, "partition")) {

	// Partition
	status = parsePartition(file, lineBuf);
      }
    }
  }

  PartitionData *partdata = NULL;
  PartitionDataIterator dItr;
  BString foundpath;

  for (dItr = beginPartitionData();
       dItr != endPartitionData();
       dItr++) {
    partdata = dItr->second;
    if (!partdata->isDefault())
      status = collectAndPopulatePortsData(partdata);
  }
  
  //printf("DONE parsing module spec file\n");

  file.close();
  return status;
}

// Find the ports and add it to the inputs and outputs of the PartitionData
int PartitionSpec::collectAndPopulatePortsData(PartitionData *pdata)
{
  PortVisitor visitor;
  unsigned i;
  int status = 1;
  VeriExpression *expr, *value;
  std::ostringstream strm;
  std::string portname;
  VeriIdDef *port_def;
  BString foundpath;
  int width;

  //printf("CollectAndPopulate1 %s\n", pdata->getFpga()->getModuleName());

  VeriInstId *inst_id = pdata->getCurrentVerificInstance();
  
  //printf("CollectAndPopulate %p\n", inst_id);

  if (inst_id == NULL) {
    return 1;
  }

  Array *port_connects = inst_id->GetPortConnects();


  FOREACH_ARRAY_ITEM(port_connects, i, expr) {
    
    value = expr->GetConnection();
    strm.str("");
    value->PrettyPrint(strm, 0);
    if (expr->GetClassId() == ID_VERIPORTCONNECT) {
      portname = expr->NamedFormal();
      port_def = HdlUtils::findPortOfInstByName(inst_id, portname);
      width = HdlUtils::findWidthOfSignal(port_def);
      if (port_def->IsInput()) {
	pdata->addInput(port_def->Name(), strm.str(), width);
	//printf("Add input1 %s %s\n", port_def->Name(), strm.str().c_str());
      }
      else if (port_def->IsOutput()) {
	//printf("Add output1 %s %s\n", port_def->Name(), strm.str().c_str());
	pdata->addOutput(port_def->Name(), strm.str(), width);
      }
      else if (port_def->IsInout()) {
	//printf("Add inout2 %s %s\n", port_def->Name(), strm.str().c_str());
	pdata->addInout(port_def->Name(), strm.str(), width);
      }
      else {
	fprintf(stderr, "EditCompilePP::collectAndPopulatePortsData(): Cannot partition across on unknown port type for port %s (positional)", port_def->Name());
	status = 0;
      }
    } else if (expr->IsIdRef()) {
      VeriModule *module_def = inst_id->GetInstantiatedModule();
      port_def = HdlUtils::findPortOfModuleByPosition(module_def, i);
      width = HdlUtils::findWidthOfSignal(port_def);
      if (width == 0) width = 1;
      if (port_def->IsInput()) {
	//printf("Add input2 %s %s\n", port_def->Name(), strm.str().c_str());
	pdata->addInput(port_def->Name(), strm.str(), width);
      }
      else if (port_def->IsOutput()) {
	//printf("Add output2 %s %s\n", port_def->Name(), strm.str().c_str());
	pdata->addOutput(port_def->Name(), strm.str(), width);
      }
      else if (port_def->IsInout()) {
	//printf("Add inout2 %s %s\n", port_def->Name(), strm.str().c_str());
	pdata->addInout(port_def->Name(), strm.str(), width);
      }
      else {
	fprintf(stderr, "EditCompilePP::collectAndPopulatePortsData(): Cannot partition across on unknown port type for port %s (positional)", port_def->Name());
	status = 0;
      }
    }
  }
  //printf("Done collecting\n");
  return status;
}

int PartitionData::setVerific()
{
  size_t stloc;
  BString foundpath;

  BString fullname = m_hier_inst_name;
  stloc = fullname.find_last_of('/');
  BString path = fullname.substr(0, stloc);

  //printf("setverific2\n");
  setHierPathName(path);

  m_containing_module = HdlUtils::findModuleFromPath(NULL, path, foundpath);

  setContainingModulePath(foundpath.c_str());
  
  if (m_containing_module == NULL) {
    std::cout << "ERROR PartitionData::setVerific: module is not found from path "
	      << path.c_str() << std::endl;
    return 0;
  }

  BString inst_name = fullname.substr(stloc+1);
  m_current_instance = HdlUtils::findInstInModuleByName(m_containing_module, inst_name);
  getPartitionSpec()->addPartitionedInst(m_current_instance);

  if (m_current_instance == NULL) {
    std::cout << "ERROR PartitionData::setVerific: instance is not found from path "
	      << fullname.c_str() << std::endl;
    return 0;
  }


  //printf("setverific5 %p\n", m_current_instance);
  PartitionData *pdata = getPartitionSpec()->getDefaultPartData();
  if (pdata != NULL)
    pdata->setDefaultVerific(fullname);

  //printf("Set default current instance %s %p\n", fullname.c_str(), m_current_instance);
  return 1;
}

int PartitionData::setDefaultVerific(BString &path)
{
  BString foundpath, modpath;
  size_t slash;
  
  //printf("Setdefaultverific\n");
  if (path[0] == '/')
    slash = path.find_first_of('/', 1);
  else
    slash = path.find_first_of('/');

  if (slash != string::npos)
    modpath = path.substr(0, slash);
  else
    modpath = path;

  setHierPathName(path);

  m_containing_module = HdlUtils::findModuleFromPath(NULL, modpath, foundpath);

  m_containing_module_path = modpath;
  //printf("Set current instance %s %p %p\n", modpath.c_str(), m_containing_module, this);

  if (m_containing_module == NULL) {
    std::cerr << "ERROR PartitionData::setDefaultVerific: module is not found from path "
	      << modpath.c_str() << std::endl;
    return 0;
  }
  return 1;
}

const char *PartitionData::getContainingModulePrefix()
{
  if (m_containing_module_prefix != "")
    return m_containing_module_prefix.c_str();
  
  m_containing_module_prefix = getContainingModulePath();

  if (m_containing_module_prefix[0] == '/')
    m_containing_module_prefix = m_containing_module_prefix.substr(1);

  std::replace(m_containing_module_prefix.begin(), m_containing_module_prefix.end(), '/', '_');

  m_containing_module_path = m_containing_module_prefix;

  return m_containing_module_prefix.c_str();
}

const char *PartitionData::getPartitionedInstName()
{
  if (m_current_instance)
    return m_current_instance->Name();
  return NULL;
}

// Traverse all terminal values and see if we can find a match with "*" as wildcard
void PartitionData::getFixedTerminalValue(BString &modName, BString &portName, BString &retName)
{
  std::map<std::string, std::string>::iterator itr;
  BString name1, name2;
  size_t pos1, pos2;
  BString modname = modName;

  retName = "";

  // If modname has _EDITED in it then remove it for matching
  size_t pos;
  pos = modname.find("_EDITED");
  if (pos != string::npos) {
    //printf("MODNAME before %s\n", modname.c_str());
    modname = modName.substr(0, pos);
    //printf("MODNAME after %s\n", modname.c_str());
  }
  // Exact match first
  retName = m_module_table[modname][portName];
  if (retName != "")
    return;

  itr = m_module_table[modname].begin();
  while (itr !=  m_module_table[modname].end()) {

    name1 = itr->first;
    name2 = itr->second;
    
    // if match then construct portName
    pos1 = match_name(portName, name1);
    if (pos1 == 0) {
      retName = name2;
      return;
    }
    else if (pos1 != std::string::npos) {

      pos2 = name2.find_first_of('*');
      retName = name2.substr(0, pos2) + portName.substr(pos1);
      return;
    }
    itr++;
  }
}

// Traverse all terminal values and see if we can find a match with "*" as wildcard
void PartitionData::getFixedTerminalValue(const char *modname, const char *portName,
					  BString &retName)
{
  std::map<std::string, std::string>::iterator itr;
  BString name1, name2;
  size_t pos1, pos2;
  BString pname = portName;

  retName = "";
  itr = m_module_table[modname].begin();
  while (itr !=  m_module_table[modname].end()) {

    name1 = itr->first;
    name2 = itr->second;
    
    // if match then construct portName
    pos1 = match_name(pname, name1);
    if (pos1 == 0) {
      retName = name2;
      return;
    }
    else if (pos1 != std::string::npos) {

      pos2 = name2.find_first_of('*');
      retName = name2.substr(0, pos2) + pname.substr(pos1);
      return;
    }
    itr++;
  }
}

ModuleTerminal *PartitionData::findTerminal(const char *name)
{
  ModuleTerminalIterator tItr;
  ModuleTerminalList &inputs = getInputList();
  
  //printf("Find terminal %s\n", name);
  for (tItr = inputs.begin(); tItr != inputs.end(); tItr++) {

    //printf("Input: %s\n", tItr->m_netName.c_str());
    if (tItr->m_netName == name)
      return &(*tItr);
  }

  ModuleTerminalList &outputs = getOutputList();

  for (tItr = outputs.begin(); tItr != outputs.end(); tItr++) {
    //printf("Output: %s\n", tItr->m_netName.c_str());
    if (tItr->m_netName == name)
      return &(*tItr);
  }

  return NULL;
}

void Fpga::setModule(VeriModule *module)
{
  m_module = module;
  setLetterSuffix();
  m_module_path = BString("/") + module->Name();
}

void Fpga::setLetterSuffix()
{
  BString l = m_module->Name();
  m_fpga_letter = l[l.length()-1];
  m_fpga_letter = toupper(m_fpga_letter);
}

const char *Fpga::getModuleName()
{
  return m_module->Name();
}

VeriModule *Fpga::getIOModule()
{
  return m_io_inst->GetInstantiatedModule();
}

void Fpga::addInSEConn(Fpga *fromFpga, const char *fromPort, const char *toPort,
		       const char *fromNet, const char *toNet, unsigned int width)
{
  ConnBundle *c = m_in_se_connection_bundles[fromFpga->getModuleName()];

  if (c == NULL) {
    c = new ConnBundle(fromFpga, this);
    m_in_se_connection_bundles[fromFpga->getModuleName()] = c;
    fromFpga->m_out_se_connection_bundles[getModuleName()] = c;
  }

  //printf("AddInSEConn from %s to %s %p\n", fromFpga->getModuleName(), getModuleName(), c);
  c->addConnection(fromPort, toPort, fromNet, toNet, width, SINGLE_ENDED);
  c->incrementWidth(width);
  c->setUnused(c->getWidth());
}

void Fpga::addOutSEConn(Fpga *toFpga, const char *fromPort, const char *toPort,
			const char *fromNet, const char *toNet, unsigned int width)
{
  ConnBundle *c = m_out_se_connection_bundles[toFpga->getModuleName()];

  if (c == NULL) {
    c = new ConnBundle(this, toFpga);
    m_out_se_connection_bundles[toFpga->getModuleName()] = c;
    toFpga->m_in_se_connection_bundles[getModuleName()] = c;
  }

  //printf("AddOutSEConn to %s from %s %p\n", toFpga->getModuleName(), getModuleName(), c);
  c->addConnection(fromPort, toPort, fromNet, toNet, width, SINGLE_ENDED);
  c->incrementWidth(width);
  c->setUnused(c->getWidth());
}

void Fpga::addInDPConn(Fpga *fromFpga, const char *fromPort, const char *toPort,
		       const char *fromNet, const char *toNet, unsigned int width)
{
  ConnBundle *c = m_in_dp_connection_bundles[fromFpga->getModuleName()];

  if (c == NULL) {
    c = new ConnBundle(fromFpga, this);
    m_in_dp_connection_bundles[fromFpga->getModuleName()] = c;
    fromFpga->m_out_dp_connection_bundles[getModuleName()] = c;
  }

  c->addConnection(fromPort, toPort, fromNet, toNet, width, DIFF_PAIR);
  c->incrementWidth(width);
  c->setUnused(c->getWidth());
}

void Fpga::addOutDPConn(Fpga *toFpga, const char *fromPort, const char *toPort,
			const char *fromNet, const char *toNet, unsigned int width)
{
  ConnBundle *c = m_out_dp_connection_bundles[toFpga->getModuleName()];

  if (c == NULL) {
    c = new ConnBundle(this, toFpga);
    m_out_dp_connection_bundles[toFpga->getModuleName()] = c;
    toFpga->m_in_dp_connection_bundles[getModuleName()] = c;
  }

  c->addConnection(fromPort, toPort, fromNet, toNet, width, DIFF_PAIR);
  c->incrementWidth(width);
  c->setUnused(c->getWidth());
}

int Fpga::getNextInSEConn(Fpga *fromFpga, ConnectionIterator &itr, unsigned int &fabricLSB,
			  unsigned int &fabricMSB, unsigned int width)
{
  ConnBundle *cb;

  cb = fromFpga->findSEConnBundleToFpga(this);

  if (cb)
    return cb->getNextConn(itr, fabricLSB, fabricMSB, width);
  else
    return -1;
}


int Fpga::getNextOutSEConn(Fpga *toFpga, ConnectionIterator &itr, unsigned int &fabricLSB,
			   unsigned int &fabricMSB, unsigned int width)
{
  ConnBundle *cb;

  //printf("getNextOutSEConn %d\n", width);
  cb = findSEConnBundleToFpga(toFpga);

  if (cb)
    return cb->getNextConn(itr, fabricLSB, fabricMSB, width);

  else
    return -1;
}
/*
unsigned Fpga::getNumberOfUnusedSEConn(Fpga *toFpga)
{
  ConnBundle *cb;

  cb = findSEConnBundleToFpga(toFpga);

  if (cb)
    return cb->getUnused();
  else
    return 0;
}

unsigned Fpga::getNumberOfUnusedDPConn(Fpga *toFpga)
{
  ConnBundle *cb;

  cb = findDPConnBundleToFpga(toFpga);

  if (cb)
    return cb->getUnused();
  else
    return 0;
}
*/

int Fpga::getNextInDPConn(Fpga *fromFpga, ConnectionIterator &itr, unsigned int &fabricLSB,
			  unsigned int &fabricMSB, unsigned int width)
{
  ConnBundle *cb;

  cb = fromFpga->findDPConnBundleToFpga(this);

  if (cb)
    return cb->getNextConn(itr, fabricLSB, fabricMSB, width);

  else
    return -1;
}

int Fpga::getNextOutDPConn(Fpga *toFpga, ConnectionIterator &itr, unsigned int &fabricLSB,
			   unsigned int &fabricMSB, unsigned int width)
{
  ConnBundle *cb;

  //printf("getNextOutDPConn %d\n", width);
  cb = findDPConnBundleToFpga(toFpga);

  if (cb)
    return cb->getNextConn(itr, fabricLSB, fabricMSB, width);
  else
    return -1;
}

void Fpga::initializeCrossFpgaConnections()
{
  ConnBundleIterator cbitr;
  ConnBundle *cb;
    
  //printf("Init cross fpga ports for %s\n", getModuleName());
  for (cbitr = beginOutDPConnBundle();
       cbitr != endOutDPConnBundle();
       cbitr++) {
    
    //printf("next out dp conn\n");
    cb = cbitr->second;
    cb->initializeConnections();
  }
  for (cbitr = beginOutSEConnBundle();
       cbitr != endOutSEConnBundle();
       cbitr++) {
    
    //printf("next out se conn\n");
    cb = cbitr->second;
    cb->initializeConnections();
  }

  //printf("Done init\n");
}

// do fabric
void FpgaBoardSpec::parseFabric(std::ifstream &file, char *lineBuf)
{
  const char *delimiter = " \n\t,:";
  BString signal;
  char *token;
  //SpecFileMode mode;
  char fpga_letter;
  Fpga *fpga;
  int len;
  BString modname;
  Fabric *fabric;

  token = strtok(0, delimiter);
  len = strlen(token);
  fpga_letter = token[len-1];
  modname = strtok(0, delimiter);

  if (modname == "") {
    std::cerr << "ERROR FpgaBoardSpec::parseFabric(): invalid fabric statement in file "
	      << m_filename.c_str() << " on line " << m_lineno << std::endl;
    m_stat = 0;
    return;
  }

  fpga = findFpgaByLetterSuffix(fpga_letter);
  if (fpga == NULL)
    fpga = createFpga(modname);
  fabric = fpga->getFabric();

  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);

    m_lineno++;
    token = strtok(lineBuf, delimiter);
    //printf("parsing line %d token %s\n", m_lineno, token);
    if (token && (strncmp(token, "//", 2) != 0)) { // Check comment

      if (!strcmp(token, "assign-to")) {
	signal = strtok(0, delimiter);
	fabric->addAssignToSignal(signal);
      }
      else if (!strcmp(token, "single-ended-clock")) {
	signal = strtok(0, delimiter);
	fabric->addSingleEndedClock(signal);
      }
      else if (!strcmp(token, "endfabric")) {
	//mode = UNKNOWN;
	return;
      }
      else {
	std::cerr << "ERROR FpgaBoardSpec::parseFabric(): invalid fabric statement in file "
		  << m_filename.c_str() << " on line " << m_lineno << std::endl;
	m_stat = 0;
	return;
      }
    }
  }
  //mode = UNKNOWN;
}

// do connection
void FpgaBoardSpec::parseConnection(std::ifstream &file, char *lineBuf)
{
  const char *delimiter = " \n\t,";
  BString astr, sigA, sigB, portA, portB;
  BString connA, connB;
  BString io, connect_type, macro, macrotype;
  char *token;
  size_t colon;
  SpecFileMode mode;
  Fpga *fromFpga, *toFpga;
  unsigned int widthA, widthB;

  fromFpga = toFpga = NULL;

  // io1
  macrotype = strtok(0, delimiter);
  //printf("macrotype %s\n", macrotype.c_str());

  if (macrotype == "from") {
    astr = strtok(0, delimiter);
    //printf("astr %s\n", astr.c_str());
    colon = astr.find_first_of(':');
    if (colon != string::npos) {
      macro = astr.substr(0, colon);
      io = astr.substr(colon+1);
      mode = IO_CONNECTION_MODE;
      //printf("from io macro %s\n", macro.c_str());
      //printf("io %s\n", io.c_str());
      fromFpga = createFpga(macro);
      if (fromFpga == NULL) {
	m_stat = 0;
	return;
      }
      setIOInstance(fromFpga, io);
      //printf("set io instance\n");
    } else {
      macro = astr;
      mode = DIRECT_CONNECTION_MODE;
      //printf("from macro %s\n", macro.c_str());
      //printf("create macro %s\n", macro.c_str());
      fromFpga = createFpga(macro);
      if (fromFpga == NULL) {
	m_stat = 0;
	return;
      }
    }
  }
  
  // io2
  macrotype = strtok(0, delimiter);
  //printf("macrotype2 %s\n", macrotype.c_str());
  if (macrotype == "to") {
    astr = strtok(0, delimiter);
    colon = astr.find_first_of(':');
    if (colon != string::npos) {
      macro = astr.substr(0, colon);
      io = astr.substr(colon+1);
      //printf("to io macro %s\n", macro.c_str());
      //printf("io2 %s\n", io.c_str());
      toFpga = createFpga(macro);
      if (toFpga == NULL) {
	m_stat = 0;
	return;
      }
      setIOInstance(toFpga, io);
    } else {
      macro = astr;
      mode = DIRECT_CONNECTION_MODE;
      //printf("to macro %s\n", macro.c_str());
      //printf("create macro %s\n", macro.c_str());
      toFpga = createFpga(macro);
      if (toFpga == NULL) {
	m_stat = 0;
	return;
      }
    }
    
  } else {
    std::cerr << "ERROR FpgaBoardSpec::parseConnection(): invalid connection in file "
	      << m_filename.c_str() << " on line " << m_lineno << std::endl;
    m_stat = 0;
    return;
  }
	

  while (!file.eof()){
    
    file.getline(lineBuf, MAX_LINE_SIZE);
    
    m_lineno++;
    token = strtok(lineBuf, delimiter);
    //printf("parsing1 line %d token %s mode %d\n", m_lineno, token, mode);
    if (token && (strncmp(token, "//", 2) != 0)) { // Check comment


      if (!strcmp(token, "endconnection")) {
	//printf("END connection\n");
	mode = UNKNOWN;
	return;
      }

      if (!strcmp(token, "port")) {
      
	portA = strtok(0, delimiter);
	portB = strtok(0, delimiter);
	//printf("Port %s %s %s\n", token, portA.c_str(), portB.c_str());
      
	if (mode == IO_CONNECTION_MODE) {
	  //std::cerr << "io conn mode fpgab " << toFpga->getModuleName() << endl;
	  HdlUtils::findConnectionNameOfPort(fromFpga->getIOInstance(), portA, sigA);
	  HdlUtils::findConnectionNameOfPort(toFpga->getIOInstance(), portB, sigB);
	  //printf("Found %s %s\n", sigA.c_str(), sigB.c_str());
	}
	else {
	  sigA = portA;
	  sigB = portB;
	}
	//printf("Mode %d\n", mode);
	if (sigA == "") {
	  std::cerr << "ERROR FpgaBoardSpec::parseConnection(): unable to find connection of port "
		    << portA << " in file "
		    << m_filename.c_str() << " on line " << m_lineno
		    << std::endl;
	  m_stat = 0;
	  return;
	}
	if (sigB == "") {
	  std::cerr << "ERROR FpgaBoardSpec::parseConnection(): unable to find connection of port "
		    << portB << " in file "
		    << m_filename.c_str() << " on line " << m_lineno
		    << std::endl;
	  m_stat = 0;
	  return;
	}
      
	//printf("find signal %s in fpga %s\n", sigA.c_str(), fromFpga->getModuleName());
	VeriIdDef *idA = HdlUtils::findSignalInModuleByName(fromFpga->getModule(), sigA);
	//printf("find signal %s in fpga %s\n", sigB.c_str(), toFpga->getModuleName());
	VeriIdDef *idB = HdlUtils::findSignalInModuleByName(toFpga->getModule(), sigB);
	//printf("Found2 %p %p\n", idA, idB);
	
	if (idA == NULL) {
	  std::cerr << "ERROR FpgaBoardSpec::parseConnection(): invalid port statement in file "
		    << m_filename.c_str() << " on line " << m_lineno
		    << ", signal " << sigA << " is not found in module "
		    << fromFpga->getModule()->Name() << std::endl;
	  m_stat = 0;
	  return;
	}
	if (idB == NULL) {
	  std::cerr << "ERROR FpgaBoardSpec::parseConnection(): invalid port statement in file "
		    << m_filename.c_str() << " on line " << m_lineno
		    << ", signal " << sigB << " is not found in module "
		    << toFpga->getModule()->Name() << std::endl;
	  m_stat = 0;
	  return;
	}
      
	widthA = HdlUtils::findWidthOfSignal(idA);
	widthB = HdlUtils::findWidthOfSignal(idB);
	//printf("Found3 %d %d\n", widthA, widthB);
	if (widthA != widthB) {
	  std::cerr << "ERROR FpgaBoardSpec::parseConnection(): the width of port "
		    << sigA << " in module "
		    << fromFpga->getIOModule()->Name() << " is not equal to the width of port "
		    << sigB << " in module " << toFpga->getIOModule()->Name()
		    << " in file " << m_filename.c_str() << " on line number " << m_lineno
		    << std::endl;
	  m_stat = 0;
	  return;
	}
	  
	if (mode == IO_CONNECTION_MODE) {
	  HdlUtils::findConnectionNameOfPort(fromFpga->getIOInstance(), portA, connA);
	  HdlUtils::findConnectionNameOfPort(toFpga->getIOInstance(), portB, connB);
	  //printf("Found4 %s %s\n", connA.c_str(), connB.c_str());
	}
	else {
	  connA = sigA;
	  connB = sigB;
	}
      
	if (mode == IO_CONNECTION_MODE) {
	  idA = HdlUtils::findPortOfModuleByName(fromFpga->getIOModule(), portA);
	  if (idA->IsInput()) {
	    
	    toFpga->addInDPConn(fromFpga, portA.c_str(), portB.c_str(),
				connA.c_str(), connB.c_str(), widthA);
	  }
	  else if (idA->IsOutput()) {
	    toFpga->addOutDPConn(fromFpga, portB.c_str(), portA.c_str(),
				 connB.c_str(), connA.c_str(), widthA);
	  }
	  else {
	    std::cerr << "Port has no type\n";
	  }
	}
	else {
	  idA = HdlUtils::findPortOfModuleByName(fromFpga->getModule(), portA);
	  if (idA->IsInput()) {
	    fromFpga->addInSEConn(toFpga, sigA.c_str(), portA.c_str(),
				  connB.c_str(), connA.c_str(), widthA);
	    //printf("AddInSE %s(%s) from %s to %s\n", portA.c_str(), sigA.c_str(),
	    //	   fromFpga->getModuleName(), toFpga->getModuleName());
	  }
	  else if (idA->IsOutput()) {
	    fromFpga->addOutSEConn(toFpga, portA.c_str(), sigA.c_str(),
				  connA.c_str(), connB.c_str(), widthA);
	    //printf("AddOutSE %s(%s) %s %s from %s to %s\n", portA.c_str(), sigA.c_str(),
	    //	   connA.c_str(), connB.c_str(),
	    //	   fromFpga->getModuleName(), toFpga->getModuleName());
	  }
	  else {
	    std::cerr << "Port has no type\n";
	  }
	}
      } else {
	std::cerr << "ERROR FpgaBoardSpec::parseConnection(): invalid token "
		  << token << " in file "
		  << m_filename.c_str() << " on line " << m_lineno << "." << std::endl;
	m_stat = 0;
	return;
      }
    }
  }
}

// Board constructor
FpgaBoardSpec::FpgaBoardSpec(const char *specfile)
{
  const char *delimiter = " \n\t,";
  std::ifstream file;
  char lineBuf[MAX_LINE_SIZE+1];
  char *token;
  BString foundpath;
  //SpecFileMode mode = UNKNOWN;
  
  m_filename = specfile;
  m_lineno = 0;
  m_stat = 1;

  //printf("Parsing board specfile %s...\n", specfile);

  file.open(specfile);
  if (file.fail()) {
    BString bfile = getenv("BLUESPECDIR");
    if ((m_filename == "7002") || (m_filename == "7006") || (m_filename == "7406")) {
      bfile = bfile + "/" + "board_support/classic/dini/" + specfile + "/" +
	specfile + "_board.spec";
      file.clear();             // Clear error status
      file.open(bfile.c_str());
      if (file.fail()) {
	std::cerr << "ERROR: FpgaBoardSpec::FpgaBoardSpec() unable to open spec file " << bfile
		  << "." << std::endl;
	m_stat = 0;
	return;
      }
      std::cout << "Using default board spec file " << bfile
		<< "." << std::endl;
    }
    else {
      std::cerr << "ERROR: FpgaBoardSpec::FpgaBoardSpec() unrecognized board spec file or type "
		<< m_filename
		<< "." << std::endl;
      m_stat = 0;
      return;
    }
  }

  // Read in each line.  Right now assuming each line consists of either:
  // connection from module:ioinstance to module:ioinstance
  // connect signal1 signal2
  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    m_lineno++;
    token = strtok(lineBuf, delimiter);
    //printf("parsing line %d token %s mode %d\n", m_lineno, token, mode);
    if (token && (strncmp(token, "//", 2) != 0)) { // Check comment
      // connection
      //printf("Token %s\n", token);
      if (!strcmp(token, "connection")) {
	
	// do connection
	parseConnection(file, lineBuf);
	
      } else if (!strcmp(token, "fabric")) {
	
	//mode = FABRIC_MODE;
	
	// do fabric
	parseFabric(file, lineBuf);
	
      } else {
	
	std::cerr << "ERROR FpgaBoardSpec::FpgaBoardSpec(): invalid connection in file "
		  << specfile << " on line " << m_lineno << std::endl;
	m_stat = 0;
	return;
      }
    }
  }
}

// Create Fpga object
Fpga *FpgaBoardSpec::findFpgaByLetterSuffix(const char l)
{
  FpgaIterator fpgaItr;
  Fpga *fpga;
  for (fpgaItr = beginFpga();
       fpgaItr != endFpga();
       fpgaItr++) {

    fpga = fpgaItr->second;
    if (l == fpga->getLetterSuffix()) {
      return fpga;
    }
  }
  return NULL;
}

// Create Fpga object
Fpga *FpgaBoardSpec::createFpga(BString &modname)
{
  BString foundpath;
  VeriModule *module;
  Fpga *fpga;
  BString modpath;
  
  //printf("create fpga for %s\n", modname.c_str());
  if (modname[0] != '/')
    modpath = "/" + modname;
  else
    modpath = modname;
  
  fpga = findFpga(modname.c_str());
  if (fpga)
    return fpga;

  module = HdlUtils::findModuleFromPath(NULL, modpath, foundpath);	
  if (module == NULL) {
    std::cerr << "Error FpgaBoardSpec::createFpga(): module for board specification in file "
	      << getSpecFile() << " on line " << m_lineno
	      << " (module " << modname
	      << " not found)" << "." << std::endl;
    return 0;
  }

  fpga = new Fpga(this, module);
  m_fpgas[modname] = fpga;

  return fpga;
}

// Set io instance

int FpgaBoardSpec::setIOInstance(Fpga *fpga, BString &io_instname)
{
  //printf("set io instance %s in %s\n", io_instname.c_str(), fpga->getModule()->Name());
  fpga->setIOInstance(HdlUtils::findInstInModuleByName(fpga->getModule(), io_instname));
  if (fpga->getIOInstance() == NULL) {
    std::cerr << "ERROR FpgaBoardSpec::setIOInstance(): invalid token in file "
              << getSpecFile() << " on line " << m_lineno
              << " (io instance " << io_instname
              << " not found)" << "." << std::endl;
    return 0;
  }
 
  //printf("set io instance %s\n", io_instname.c_str());
  if (fpga->getIOModule() == NULL) {
    std::cerr << "ERROR FpgaBoardSpec::setIOInstance(): invalid token in file "
	      << getSpecFile() << " on line " << m_lineno
	      << ", no verilog module found for "
	      << fpga->getIOInstance()->Name() << "." << std::endl;
    return 0;  
}

  //printf("set io instance %s\n", io_instname.c_str());
  veri_file::GetModule(fpga->getIOModule()->Name()); // Make sure verific load the module

  return 1;
}
  
void FpgaBoardSpec::initializeFpgas()
{
  FpgaIterator fpgaItr;
  Fpga *fpga;

  for (fpgaItr = beginFpga(); fpgaItr != endFpga(); fpgaItr++) {

    fpga = fpgaItr->second;
    fpga->initializeCrossFpgaConnections();
  }
}
 
void PartitionData::addAssignedSETerminal(const BString &portName, const BString &netName,
					  int width)
{
  AssignmentIterator itr;    

  if (getMergingNetCount(netName.c_str()) > 0) return;

  //printf("addAssignedSETerminal port: %s net: %s\n", portName.c_str(), netName.c_str());
  itr = m_assigned_se_terminals_netname.find(portName);
  if (itr == m_assigned_se_terminals_netname.end()) {
    m_assigned_se_terminals.push_back ( ModuleTerminal (portName, netName, d_input, width) );
    m_assigned_se_terminals_netname[portName] = netName;
    addMergingNet(netName.c_str());
  }
}

void PartitionData::addMergingNet(const char *netname)
{
  int num = m_net_map[netname];
  m_net_map[netname] = ++num;
}
