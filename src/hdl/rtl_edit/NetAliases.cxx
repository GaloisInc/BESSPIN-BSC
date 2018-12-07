// Copyright 2010 Bluespec Inc. All rights reserved

#include "VeriNodeInfo.h"
#include "HdlUtils.h"
#include "Map.h"
#include "VeriModule.h"     // Definition of a VeriModule and VeriPrimitive
#include "VeriId.h"         // Definitions of all identifier definition tree nodes
#include "VeriExpression.h" // Definitions of all verilog expression tree nodes
#include "VeriModuleItem.h" // Definitions of all verilog module item tree nodes
#include "VeriStatement.h"  // Definitions of all verilog statement tree nodes
#include "VeriTreeNode.h"
#include "veri_tokens.h"
#include "VeriScope.h"      // Symbol table of locally declared identifiers
#include "getopt.h"

// Map for signal name and id pair (for finding by name)
typedef multimap<string, unsigned int, less<string> > SignalMap;
typedef multimap<string, unsigned int, less<string> >::value_type SignalIDPair;
typedef multimap<string, unsigned int, less<string> >::iterator SignalIDPairPtr;

// Map for id and signal name pair (for finding by id)
typedef multimap<unsigned int, string, less<unsigned int> > IDMap;
typedef multimap<unsigned int, string, less<unsigned int> >::value_type IDSignalPair;
typedef multimap<unsigned int, string, less<unsigned int> >::iterator IDSignalPairPtr;

static const unsigned int major_rev = 1;
static const unsigned int minor_rev = 1;

static SignalMap GlobalSignalMap;
static IDMap GlobalIDMap;
static unsigned int NextVcdId = 0;

bool insertSignal(string &signal, unsigned long id)
{
  //printf("Before Inserting signal %s ID %d\n", signal.c_str(), (int)id);
  if (GlobalSignalMap.find(signal) == GlobalSignalMap.end()) {
    //printf("Inserting1 signal %s ID %d\n", signal.c_str(), (int)id);
    GlobalSignalMap.insert(SignalIDPair(signal, id));
    GlobalIDMap.insert(IDSignalPair(id, signal));
    return true;
  }
  return false;
}

bool findParentSourceIdOfModuleInputNode(OccCell *cell, VeriNodeInfo *node,
					 unsigned int &refid)
{
  OccCell *parent;
  string nodename, portname, tmp;
  VeriNodeInfo *parentsink;
  SignalIDPairPtr refItr;
  string src_hier_name;

  parent = cell->parent();
  VeriNodeInfoTable *parentTable = VeriNodeInfoTable::load(parent->module());
  tmp = cell->name();
  portname = node->getSourceName(); 
  nodename = tmp + "_" + portname;

  //cout << " check parent module input netname " << nodename << endl;
  parentsink = parentTable->findSink(nodename.c_str());
  if (parentsink == NULL) {
    cerr << "Something wrong1 in findParentSourceIdOfNode(): cannot find " << nodename << endl;
    return false;
  }

  //cout << "Trying to find parent ModuleInput connection " << src_hier_name << endl;
  src_hier_name = string(parent->hierName()) + "/" + parentsink->getSinkName(); 
  refItr = GlobalSignalMap.find(src_hier_name);
  if (refItr == GlobalSignalMap.end()) {
    cerr << "Something wrong2 in findParentSourceIdOfNode(): cannot find " << src_hier_name
	 << " node type " << node->getType() << " parent sink type " << parentsink->getType() 
	 << " tmp " << tmp << " ptr " << (void*)parentsink << endl;
    return false;
  }
  else
    refid = refItr->second;

  //cout << "Found parent ModuleInput connection " << src_hier_name << " id " << refid << endl;
  
  return true;
}

bool findParentSourceIdOfModuleOutputNode(OccCell *cell, VeriNodeInfo *node,
					  unsigned int &refid)
{
  OccCell *parent;
  string nodename, portname, tmp;
  VeriNodeInfo *parentsink;
  SignalIDPairPtr refItr;
  string src_hier_name;

  parent = cell->parent();
  VeriNodeInfoTable *parentTable = VeriNodeInfoTable::load(parent->module());
  tmp = cell->name();
  portname = node->getSourceName(); 
  nodename = tmp + "_" + portname;

  //cout << "find parent of module output node " << nodename << endl;
  parentsink = parentTable->findSource(nodename.c_str());

  if (parentsink == NULL) {
    //cerr << "Something wrong3 in findParentSourceIdOfNode(): cannot find " << nodename << endl;
    return false;
  }

  //cout << "parent sink node " << parentsink->getSourceName() << " node type " << parentsink->getType() << endl;
  src_hier_name = string(parent->hierName()) + "/" + parentsink->getSourceName(); 
  refItr = GlobalSignalMap.find(src_hier_name);
  if (refItr == GlobalSignalMap.end()) {
    cerr << "Something wrong4 in findParentSourceIdOfNode(): cannot find " << src_hier_name 
	 << " node type " << node->getType() << endl;
    return false;
  }
  else
    refid = refItr->second;

  return true;
}

void insertAllSinks(VeriNodeInfo *src, const char *hiername, unsigned int refid)
{
  VeriNodeInfoListIterator sinkItr;
  VeriNodeInfo *sink;
  string hier_source_name;
  string hier_sink_name;

  if ((src->getType() != NodeInfoConstant) &&
      (src->getType() != NodeInfoExpression)) {
    hier_source_name = string(hiername) + "/" + src->getSourceName(); 
    //cout << " insertSignal1 on src " << hier_source_name << " " << refid << " " << src->getType() << endl;
    insertSignal(hier_source_name, refid);
  }
    
  if ((src->getType() != NodeInfoExpressionSrc) &&
      //(src->getType() != NodeInfoExpression) &&
      (src->getType() != NodeInfoAlwaysBlockAssign)) {
    hier_sink_name = string(hiername) + "/" + src->getSinkName(); 
    //cout << " insertSignal2 on sink " << hier_sink_name << " " << refid << " " << src->getType() << endl;
    insertSignal(hier_sink_name, refid);
  }
  //cout << " insertAllSinks " << hier_sink_name << " " << refid << " " << (void*)src << endl;

  sinkItr = src->sinkBegin();
  while (sinkItr != src->sinkEnd()) {

    sink = *sinkItr;

    //cout << " found a node5 src:" << sink->getSourceName() << " sink:" << sink->getSinkName() << " " << sink->getType() 
    //	 << " ptr " << (void*)sink << endl;
    
    if ((sink == src) ||
	(sink->getType() == NodeInfoExpression) ||
	(sink->getType() == NodeInfoExpressionSrc) ||
	(sink->getType() == NodeInfoAlwaysBlockAssign)) {
      sinkItr++;
      continue;
    }

    hier_sink_name = string(hiername) + "/" + sink->getSinkName(); 
    
    //cout << " found a node6 src:" << sink->getSourceName() << " sink:" << sink->getSinkName() << " " << sink->getType() 
    //	   << " ptr " << (void*)sink << endl;
    
    insertSignal(hier_sink_name, refid);

    sinkItr++;
  }
  //cout << endl;
}

void propagateOutputNodeIDToTop(std::string &hier_sink_name, unsigned int up_id)
{
  SignalIDPairPtr sig_id_pair_ptr;
  IDSignalPairPtr id_sig_pair_ptr;
  unsigned int below_id;
  string key;

  sig_id_pair_ptr = GlobalSignalMap.find(hier_sink_name);
  if (sig_id_pair_ptr == GlobalSignalMap.end())
    return;

  below_id = sig_id_pair_ptr->second;

  pair<IDSignalPairPtr, IDSignalPairPtr> range;
  range = GlobalIDMap.equal_range(up_id);
  for (id_sig_pair_ptr = range.first; id_sig_pair_ptr != range.second; id_sig_pair_ptr++) {

    key = id_sig_pair_ptr->second;
    sig_id_pair_ptr = GlobalSignalMap.find(key);
    //cout << "Changing upper level net " << key << " to new ID " << below_id 
    //	 << " from old ID " << sig_id_pair_ptr->second << endl;
    GlobalSignalMap.erase(sig_id_pair_ptr);
    GlobalSignalMap.insert(SignalIDPair(key, below_id));
    GlobalIDMap.insert(IDSignalPair(below_id, key));
  }
  range = GlobalIDMap.equal_range(up_id);
  GlobalIDMap.erase(range.first, range.second);
}

void buildNetAliases(OccCell *cell)
{
  OccCellIterator childItr;
  std::map<VeriNodeInfo*, bool> nodeDoneTable;

  //cout << "\n**buildNetAliases for cell " << cell->name() << endl;

  // Build verinode table
  VeriNodeInfoTable *table = VeriNodeInfoTable::load(cell->module());

  if (table == NULL) return;

  VeriNodeInfoListIterator srcItr;
  SignalIDPairPtr refItr;
  VeriNodeInfo *src, *vsrc;
  string hier_source_name, hier_sink_name, src_hier_name;
  unsigned int refid;
  bool found_refid;

  // First merge nets to those at the top from module input
  //cout << "First phase ModuleInput" << endl;
  srcItr = table->nodeBegin();
  while (srcItr != table->nodeEnd()) {

    src = *srcItr;
    // Skip expression and constant
    if ((src->getType() == NodeInfoExpression) ||
	(src->getType() == NodeInfoExpressionSrc) ||
	(src->getType() == NodeInfoConstant)) {
      srcItr++;
      continue;
    }

    //cout << "found a node1 " << src->getSourceName() << " " << src->getType() << endl;

    // Merge only if this is a child
    if (!cell->isTop()) {

      // This is coming in from the top
      if (VeriNodeInfoTable::isModuleInputNode(src)) {

	//cout << " found a module input node " << src->getSourceName() << endl;
	if (findParentSourceIdOfModuleInputNode(cell, src, refid)) {

	  //cout << "  found a refid " << refid << endl;
	  // Put all sources and sinks in the table
	  vsrc = src->getVirtualSource();
	  if (vsrc) {

	    if (nodeDoneTable[vsrc]) {
	      srcItr++;
	      continue;
	    }
	    nodeDoneTable[vsrc] = true;
	    insertAllSinks(vsrc, cell->hierName(), refid); 
	  }
	  else
	    insertAllSinks(src, cell->hierName(), refid); 
	}
      }
    }
    srcItr++;
  }

  // Next merge to the top from module output
  VeriNodeInfoListIterator sinklItr;
  VeriNodeInfo *sink;

  //cout << "Second phase ModuleOutput" << endl;;
  sinklItr = table->nodeBegin();
  while (sinklItr != table->nodeEnd()) {

    sink = *sinklItr;
    if (sink==NULL) {
      sinklItr++;
      continue;
    }

    //cout << "found a node2 " << sink->getSourceName() << " " << sink->getType() << endl;
    //cout << "found a node2a " << sink->getSinkName() << " " << sink->getType() << endl;

    // Merge only if this is a child
    if (!cell->isTop()) {

      // This is coming out to the parent module
      if (VeriNodeInfoTable::isModuleOutputNode(sink)) {
	
	//cout << "found an output node src:" << sink->getSourceName() << " sink:" 
	//     << sink->getSinkName() << " " << sink->getType() << endl;


	if (findParentSourceIdOfModuleOutputNode(cell, sink, refid)) {

	  // If this output node is common with any of the input node above
	  // then we have a feedthrough and we have to propagate the refid
	  // upward everywhere
	  hier_sink_name = string(cell->hierName()) + "/" + sink->getSinkName(); 
	  //cout << "Here " << hier_sink_name << endl;
	  if ((string(sink->getSinkName()) != "") &&
	      GlobalSignalMap.find(hier_sink_name) != GlobalSignalMap.end()) {
	    //cout << "propageOutNodeID from " << refid << endl;
	    propagateOutputNodeIDToTop(hier_sink_name, refid);
	  }
	  else { // Otherwise take the ID from the top and merge it down

	    //cout << " found top level connected node " << refid << endl;
	    src = sink->getVirtualSource();
	    if (src) {
	      //cout << " found virtual source " << src->getSourceName() << " " << refid << endl;
	      if (nodeDoneTable[src]) {
		sinklItr++;
		continue;
	      }
	      nodeDoneTable[src] = true;
	      //cout << "insert all sinks of src" << endl;
	      insertAllSinks(src, cell->hierName(), refid); 
	    }
	    else {
	      //cout << "insert all sinks of sink" << endl;
	      insertAllSinks(sink, cell->hierName(), refid); 
	    }
	  }
	}
      }
    }
    sinklItr++;
  }

  VeriNodeInfoIterator sinkItr;

  // Now go through all the nodes and put them in the table
  sinkItr = table->sinkBegin();
  while (sinkItr != table->sinkEnd()) {

    sink = sinkItr->second;

    if ((sink == NULL) ||
	(sink->getType() == NodeInfoExpressionSrc)) {
      sinkItr++;
      continue;
    }

    src = sink->getVirtualSource();
    if (src && src->getType() != NodeInfoExpressionSrc) {

      if (nodeDoneTable[src]) {
	sinkItr++;
	continue;
      }
      nodeDoneTable[src] = true;

      //cout << " found virtual source2 " << src->getSourceName() << " " << src->getType() << endl;
      hier_source_name = string(cell->hierName()) + "/" + src->getSourceName(); 
      refItr = GlobalSignalMap.find(hier_source_name);
      if (refItr != GlobalSignalMap.end()) {
	//cout << " go to insert all sinks " << hier_source_name << endl;
	refid = refItr->second;
	insertAllSinks(src, cell->hierName(), refid);
      }
      else {
	insertAllSinks(src, cell->hierName(), NextVcdId++);
      }
    }
    else {
      //cout << " regular sink " << sink->getSinkName() << " " << sink->getType() << endl;
      hier_source_name = string(cell->hierName()) + "/" + sink->getSourceName(); 
      hier_sink_name = string(cell->hierName()) + "/" + sink->getSinkName(); 
      refItr = GlobalSignalMap.find(hier_source_name);
      if ((refItr != GlobalSignalMap.end()) &&
	  (sink->getType() != NodeInfoAlwaysBlockAssign)) {
	refid = refItr->second;
	insertSignal(hier_sink_name, refid);
      }
      else {
	if (insertSignal(hier_sink_name, NextVcdId))
	  NextVcdId++;
      }
    }
    sinkItr++;
  }

  // Now go through all the nodes one more time and look for unassigned src
  sinkItr = table->sinkBegin();
  while (sinkItr != table->sinkEnd()) {

    sink = sinkItr->second;
    if (sink == NULL) {
      sinkItr++;
      continue;
    }
    
    if (sink->getType() != NodeInfoConstant) {
      hier_source_name = string(cell->hierName()) + "/" + sink->getSourceName(); 
      if (GlobalSignalMap.find(hier_source_name) == GlobalSignalMap.end()) {
	//printf("Inserting2 unassigned source %s ID %d\n", hier_source_name.c_str(), (int)NextVcdId);
	GlobalSignalMap.insert(SignalIDPair(hier_source_name, NextVcdId));
	GlobalIDMap.insert(IDSignalPair(NextVcdId++, hier_source_name));
      }
    }
    if (sink->getType() != NodeInfoExpressionSrc) {
      hier_sink_name = string(cell->hierName()) + "/" + sink->getSinkName(); 
      if (GlobalSignalMap.find(hier_sink_name) == GlobalSignalMap.end()) {
	//printf("Inserting3 unassigned sink %s ID %d\n", hier_sink_name.c_str(), (int)NextVcdId);
	GlobalSignalMap.insert(SignalIDPair(hier_sink_name, NextVcdId));
	GlobalIDMap.insert(IDSignalPair(NextVcdId++, hier_sink_name));
      }
    }
    sinkItr++;
  }
  //cout << "Done phase" << endl << endl;

  childItr = cell->childBegin();
  while (childItr != cell->childEnd()) {
    
    buildNetAliases(childItr->second);
    childItr++;
  }
}

/// replace characters invalid for the vcd with underscore
std::string munge_name(const std::string & str)
{
    std::string out = str;
    size_t pos = 0;

    while (true)
    {
        pos = out.find_first_of(" :", pos);
	if (pos == std::string::npos)
	{
  	    break;
	}
	else
	{
	    out.at(pos) = '_';
	}
    }

    return out;
}

void writeString(FILE *file, const char* fmt ...)
{
  va_list ap;
  va_start(ap,fmt);
  vfprintf(file,fmt,ap);
  va_end(ap);
}

void writeData(FILE *file, const void* data,
	       unsigned int size, unsigned int num)
{
  if (fwrite(data, size, num, file) != num)
    perror("writeData");
}

void vcdWriteID(FILE *file, unsigned int num)
{
  char buf[6];
  char* cptr = buf + 6;
  unsigned int len = 0;
  *(--cptr) = '\0';
  do {
    *(--cptr) = '!' + (num % 94);
    num = num / 94;
    ++len;
  } while (num > 0);
  writeData(file, cptr, sizeof(char), len);
  //cout << " ID: " << cptr << endl;
}

unsigned int vcdWriteRegDef(FILE *file, const char* name, unsigned int width, unsigned int vcdid)
{
  writeString(file, "$var reg %d ", width);
  vcdWriteID(file, vcdid);

  string tmpname = munge_name(name);

  writeString(file, " %s $end\n", tmpname.c_str());
  return vcdid;
}

unsigned int vcdWriteWireDef(FILE *file, const char* name, unsigned int width, unsigned int vcdid)
{
  writeString(file, "$var wire %d ", width);
  vcdWriteID(file, vcdid);

  string tmpname = munge_name(name);

  writeString(file, " %s $end\n", tmpname.c_str());
  return vcdid;
}

void writeSignalsDefinition(FILE *file, OccCell *cell)
{
  // Get the module
  VeriModule *module = cell->module();

  if (module == NULL) return;

  // Get the scope of this module :
  VeriScope *module_scope = module->GetScope() ;
 
  // Now iterate over the declared identifiers ((VeriIdDef*s) in the hash table (scope->DeclArea()) in this scope :
  int m,l;
  MapIter mi ;
  VeriIdDef *id ;
  char *id_name ;
  int width;
  SignalIDPairPtr signalPtr;
  FOREACH_MAP_ITEM(module_scope->DeclArea(), mi, &id_name, &id) {
    // Rule out all identifiers declared here, except for 'nets' :

    //cerr << "netid -- " ;  id->PrettyPrint (cerr, 1);  cerr << endl;

    if (!(id->IsNet() || id->IsReg())) continue ;
    
    // Here, 'id' is a 'net', declared in the module
    // Use the extended VhdlIdDef class API (in file VeriId.h) to do what you want
    string path = cell->hierName();
    //printf("Path %s\n", path.c_str());
    string name = path + "/" + id->Name();
    //printf("name %s\n", name.c_str());
    width = id->GetPackedWidth(&m, &l);

    signalPtr = GlobalSignalMap.find(name);
    if (signalPtr != GlobalSignalMap.end()) {

      //cout << "Write signal " << id->Name() << " ID " << signalPtr->second << endl;
      if (id->IsReg())
	vcdWriteRegDef(file, id->Name(), width, signalPtr->second);
      else
	vcdWriteWireDef(file, id->Name(), width, signalPtr->second);
    }
    else {

      //cerr << "Something wrong, signal " << name << " is not found in Table" << endl; 
    }
  }
}

void writeHierarchicalVcdDefinition(FILE *file, OccCell *cell)
{
  OccCellIterator childItr;
  OccCell *child;

  // down scope
  fputs("$scope module ", file);
  fputs(cell->name(), file);
  fputs(" $end\n", file);
  
  // Write signals
  writeSignalsDefinition(file, cell);

  // Recursive write children
  for (childItr = cell->childBegin(); childItr != cell->childEnd(); childItr++) {

    child = childItr->second;
    if (child && child->module())
      writeHierarchicalVcdDefinition(file, child);
  }
  fputs("$upscope $end\n", file);
}

void writeHierarchicalVcdDefinitionWithAliases(OccCell *top, const char *vcdfile, const char *timescale)
{
  FILE *file;

  printf("\nWriting output file %s...", vcdfile);

  // Open vcd file
  file = fopen(vcdfile, "w");
  
  if (timescale == NULL)
    timescale = "1 s";

  time_t t = time(NULL);
  writeString(file, "$date\n\t%s$end\n", ctime(&t));
  writeString(file, "$version\n");
  writeString(file, "\tBluespec Hierachical VCD dumper %d.%d\n", major_rev, minor_rev);
  writeString(file, "$end\n");
  writeString(file, "$timescale\n\t%s\n$end\n", timescale);

  writeHierarchicalVcdDefinition(file, top);

  fputs("$enddefinitions $end\n", file);
  fflush(file);
}

void print_help (const char* progname)
{
  /* Spaces and tabs are significant in this message; they're chosen so the
     message aligns properly both in a tty and in a Windows message box.
     Please try to preserve them; otherwise the output is very hard to read
     when using vprocess.  */
  fprintf(stdout, "Usage: %s [OPTIONS] VERILOG_FILE...\n\
       The following OPTIONS are accepted:\n", progname);
  fprintf(stdout, " -help, -h': help\n");
  fprintf(stdout, " -topmod <modname>, -e <modname>: specify top module name\n");
  fprintf(stdout, " -ydir <dir>, -e <dir>: specify verilog library dir\n");
  fprintf(stdout, " -define <DEF>, -D <DEF>: define verilog macro\n");
  fprintf(stdout, " -outfile <file>, -o <file>: specify the output file name\n");
  fprintf(stdout, " -verbose, -V: specify verbose mode\n");
}

// Main program for writing hierarchical VCD file definition

int main (int argc, char *argv[])
{
  int stat;
  string errstring;
  string outfilename;

  //int opt_errors_encountered = 0;
  char buf[320];
  optind = 0;

  //printf("Num args %d\n", argc);
  struct option longopts[] =
    {
      { "help",	         0,  0, 'h' },
      { "vdir",	         1,  0, 'v' },
      { "ydir",	         1,  0, 'y' },
      { "outfile",       1,  0, 'o' },
      { 0, 0, 0, 0 }
    };
  
  if (argc == 1) {
    print_help("genvch");
    return 0;
  }

  while (1)
    {
      int opt = getopt_long_only (argc, argv, "Vhjne:o:r:v:y:b:D:", longopts, 0);

      if (opt == EOF) {
	break;
      }

      //printf("OPT %c %s\n", opt, optarg);
      switch (opt)
	{
	case 'h':
	  print_help("genvch");
          break;
        case 'o':
          if (!isalpha(optarg[0]) && optarg[0] != '/' && optarg[0] != '.') {
            snprintf(buf, 320, "genvch::decode_options(): invalid filename %s for option (-o, -outfile).\n", argv[optind-1]);
            errstring += buf;
          } else {
            ofstream outfile (optarg);
            if (!outfile) {
              snprintf(buf, 320, "genvch::decode_options(): cannot open file %s for option (-o, -outfile).\n", argv[optind-1]);
              errstring += buf;
            } else {
              outfile.close();
              outfilename = optarg;
            }
          }
          break;
	default:
	  /* unreachable */
	  break;
	}
    }

  // Decode the options and read in the verilog files 
  HdlUtils *hdlUtils = HdlUtils::init();
  if (hdlUtils == NULL) {
    fprintf(stderr, "Error: cannot initialize HdlUtils.\n");
    return 1;
  }

  stat = hdlUtils->decode_options(argc, argv, errstring);

  if (stat) {
    stat &= hdlUtils->analyze(errstring);
  }

  if (!stat) {
    fprintf(stderr, "Error: %s\n", errstring.c_str());
    return 1;
  }

  Array *all_top_modules = veri_file::GetTopModules() ;
  // Get a handle of the first top level module, if any.
  VeriModule *theRootModule = (all_top_modules && all_top_modules->Size()) ?
    (VeriModule *)all_top_modules->GetFirst() : 0 ;

  if (theRootModule == NULL) {
    fprintf(stderr, "Error: cannot find a top module.\n");
    return 1;
  }
    
  //cout << "before Create Top Cell" << endl;
  OccCell *topcell = OccCell::createTopCell(theRootModule);
  buildNetAliases(topcell);

  if (outfilename == "")
    outfilename = "dump.vch";

  writeHierarchicalVcdDefinitionWithAliases(topcell, outfilename.c_str(), NULL);

  printf(" Done\n");
}
