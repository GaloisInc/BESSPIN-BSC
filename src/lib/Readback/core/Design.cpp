
#include "Design.hpp"
#include "Utils.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <algorithm>
#include <string.h>
#include <cassert>
#include <bitset>
#include <vector>

Design::Design()
{
  m_has_top     = false;
  m_handler     = new DataHandler();
  m_netlister    = new Netlister();
  m_family       = VIRTEX6;


}

Design::~Design()
{
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

unsigned int Design::parse_rtl(std::string file)
{
  m_netlister->parse_rtl(file);

 if (m_netlister->getTop(RTL) == NULL) {
    std::cerr << "ERROR: Top module not found" << std::endl;
    exit(1);
  } else {
   m_top = m_netlister->getTop(RTL);
   m_has_top = true;
   m_family = m_netlister->getFamily();
   m_handler->setFamily(m_netlister->getFamily());
  }

  return 0;
}

unsigned int Design::parse_synth(std::string file)
{
  m_netlister->parse_synth(file);

 if (m_netlister->getTop(SYNTH) == NULL) {
    std::cerr << "ERROR: Top module not found" << std::endl;
    exit(1);
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

unsigned int Design::parse_ll(std::string file)
{

  std::ifstream llFile;
  llFile.open (file.c_str());
  std::string one_line;
  unsigned int mapped      = 0;
  unsigned int unmapped    = 0;
  unsigned int used        = 0;
  unsigned int unavailable = 0;

  Part part = m_netlister->getPart();
  unsigned int slr_size = utils::getSLRSize(part);
  unsigned int slr      = 0;

  if (llFile.is_open()) {
    if (!m_has_top) {
      std::cerr << "ERROR: Top module not found. The .rtl file must be loaded before the .ll file." << std::endl;
      llFile.close();
      exit(1);
    } else {
      printf("Parsing .ll file: %s ...\n", file.c_str());
      while (!llFile.eof())
	{
	  int matches;
	  std::getline(llFile, one_line);
	  std::replace(one_line.begin(), one_line.end(), '=', ' ');
	  char kind[1024], net[1024], block[1024];
	  unsigned int abs, addr, offset;
	  slr = 0;
	  
	  matches = sscanf (one_line.c_str(), "Bit %d 0x%x %d Block %s Latch %s Net %s", &abs, &addr, &offset, (char *) block, (char *) kind, (char *) net);

	  if (matches == 6) {
	    if (block[0] == 'A') {
	      fprintf(stderr, "Warning Skipping: %s\n", one_line.c_str());
	      unavailable++;
	      continue;
	    }
	    //	    if (kind[1] == 'M') { // a mux
	      //	      fprintf(stderr, "Warning Skipping MUX: %s\n", one_line.c_str());
	      //	      unavailable++;
	      //	      continue;
	    //	    }
	    if (block[0] == 'S') {
	      std::string tmp_0 = (char *) block;
	      tmp_0 = std::string(tmp_0.begin() + 6, tmp_0.end());
	      std::replace(tmp_0.begin(), tmp_0.end(), 'X', ' ');
	      std::replace(tmp_0.begin(), tmp_0.end(), 'Y', ' ');
	      unsigned int x, y, matches;
	      matches = sscanf (tmp_0.c_str(), " %d %d", &x, &y);
	      if (matches == 2) {
		if (slr_size != 0) {
		  slr = y/slr_size;
		}
		// fprintf(stderr, "X: %d Y: %d SIZE: %d R: %d\n", x, y, slr_size, y/125);
	      } else {
	       	fprintf(stderr, "ERROR: Unable to parse: %s\n", (char*) block);
	       	exit(1);
	      }
	    } else {
	      fprintf(stderr, "ERROR: Unknown block type: %s\n", (char*) block);
	      exit(1);
	    }
	    std::string key = "/";
	    key.append(m_top->getName());
	    key.append("/");
	    key.append((char *) net);
	    std::replace(key.begin(), key.end(), '[', '_');
	    std::replace(key.begin(), key.end(), ']', '_');
	    std::replace(key.begin(), key.end(), '.', '_');
	    std::replace(key.begin(), key.end(), '$', '_');
	    bool in_rtl   = false;
	    bool in_synth = false;
	    Signal* signal_rtl   = m_netlister->findSignal(RTL, key);
	    Signal* signal_synth = m_netlister->findSignal(SYNTH, key);

	    if (signal_synth != NULL) {
	      in_synth = true;
	    }
	    if (signal_rtl != NULL) {
	      in_rtl       = true;

	      // Net* net_rtl = signal_rtl->getNet();
	      // std::string code = net_rtl->getSignature();
	      // fprintf(stderr, "SIG: %d %d %s %s\n", in_rtl, in_synth, key.c_str(), code.c_str());
	    }

	    if (!in_rtl && !in_synth) {

	      size_t pos = key.rfind('/');
	      while (pos != std::string::npos) {
		key[pos] = '_';
		signal_synth = m_netlister->findSignal(SYNTH, key);
		if (signal_synth != NULL) {
		  in_synth = true;
		  break;
		}
		pos = key.rfind('/');
	      }
	    }

	    if (in_rtl || in_synth) {

	       unsigned int addr_c = utils::compressAddress(addr, getFamily());
	       unsigned int addr_o = utils::decompressAddress(addr_c, getFamily());
	       if (addr_o !=  addr) {
		 fprintf(stderr, " utils::compressAddress: Something is messed up %x %x %x\n", addr, addr_c, addr_o);
		 exit(1);
	       }

	      unsigned int offset_c = utils::compressOffset(offset, getFamily());
	      unsigned int offset_o = utils::decompressOffset(offset_c, getFamily());
	      if (offset_o !=  offset) {
		fprintf(stderr, " utils::compressOffset: Something is messed up %x %x %x\n",offset, offset_c, offset_o);
		exit(1);
	      }

	      bool is_used = false;
	      if (in_rtl) {
		BitRef* br_rtl = signal_rtl->getBitRef(0, true);
		if (br_rtl != NULL) {
		  Bit* b_rtl = br_rtl->getBit();
		  if(kind[1] == 'M' && !b_rtl->isSrcState()) {
		    // fprintf(stderr, "Warning RTL skipping MUX: %s\n", one_line.c_str());
		    unavailable++;
		    //		    continue;
		  }
		  if(kind[1] == 'M' && b_rtl->isSrcState()) {
		    signal_rtl->addState(0, addr, offset, slr);
		    is_used = true;
		    // fprintf(stderr, "Warning RTL keeping MUX: %s\n", one_line.c_str());
		  }
		  if(kind[1] != 'M') {
		    signal_rtl->addState(0, addr, offset, slr);
		    is_used = true;
		  }
		}
	      }

	      if (in_synth) {
		BitRef* br_synth = signal_synth->getBitRef(0, true);
		if (br_synth != NULL) {
		  Bit* b_synth = br_synth->getBit();
		  if(kind[1] == 'M' && !b_synth->isSrcState()) {
		    // fprintf(stderr, "Warning SYNTH skipping MUX: %s\n", one_line.c_str());
		    unavailable++;
		    //		    continue;
		  }
		  if(kind[1] == 'M' && b_synth->isSrcState()) {
		    signal_synth->addState(0, addr, offset, slr);
		    is_used = true;
		    // fprintf(stderr, "Warning SYNTH keeping MUX: %s\n", one_line.c_str());
		  }
		  if(kind[1] != 'M') {
		    signal_synth->addState(0, addr, offset, slr);
		    is_used = true;
		  }
		}
	      }
	      if (is_used) {
		used++;
	      }
	    }

	    if (!in_rtl && in_synth) {
	      //	      fprintf(stderr, "SYNTH ONLY! %s\n", key.c_str());
	    }

	    if (!in_rtl && !in_synth) {
	      //	      fprintf(stderr, "NONE! %s\n", (char*) net);
	      unmapped++;
	    } else {
	      mapped++;
	    }
	  }
	}
    }
    
    printf(" NOTE: %d configuration memory locations mapped.\n", mapped);
    //    printf(" NOTE: %d configuration memory locations unavailable.\n", unavailable);
    printf(" NOTE: %d configuration memory locations unmapped.\n", unmapped);
    printf(" NOTE: %d configuration memory locations used.\n", used);
    printf("Parsing .ll file: %s complete.\n\n", file.c_str());

  } else {
    std::cerr << "Can't open file " << file << std::endl;
    exit(1);
  }

  llFile.close();

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

BitRef * Design::findBitRef (EdifView view, std::string cell_def, std::string cell_inst,  unsigned int n)
{

  unsigned int count = m_netlister->m_synth_def_map.count(cell_def);


  if (count != 0) {
    std::pair<std::multimap<std::string, std::string>::iterator, std::multimap<std::string, std::string>::iterator> pp;
    pp = m_netlister->m_synth_def_map.equal_range(cell_def);

    for (std::multimap<std::string, std::string>::iterator it2 = pp.first;
	 it2 != pp.second;  ++it2)
      {
	std::string path = it2->second;
	path.append("/");
	path.append(cell_inst);
	Signal * signal = findSignal(view, path);

	if (signal != NULL) {
	  unsigned int lsb = signal->getLsb();
	  unsigned int msb = signal->getMsb();
	  if (n >= lsb && n <= msb) {
	    BitRef* br = signal->getBitRef(n);
	    if (br != NULL) {
	      return br;
	    }
	  }
	}
      }
  }
  
  return NULL;

}

unsigned int Design::parse_log(std::string file, bool skip_vcd)
{
  if (getFamily() == VIRTEX6) {
    parse_log_6(file, skip_vcd);
  }
  if (getFamily() == KINTEX7) {
    parse_log_7(file, skip_vcd);
  }

  return 0;
}

unsigned int Design::parse_log_6(std::string file, bool skip_vcd)
{

  std::ifstream logFile;
  logFile.open (file.c_str());
  std::string one_line;
  //  EdifView view = RTL;

  if (logFile.is_open()) {

    char cell_def[1024];
    char cell_inst[1024];
    // char cell_removed[1024];
    // char file_name[1024];
    //    char signal_name[1024];
    //    char* p;

    //    unsigned int i;
    unsigned int msb;
    unsigned int lsb;
    int msb_signed, lsb_signed;
      unsigned int value;
    //    unsigned int index;
    //    unsigned int ignore;

    printf("Parsing log file: %s ...\n", file.c_str());
    while (!logFile.eof()) {
      int matches;
      std::getline(logFile, one_line);
      std::replace(one_line.begin(), one_line.end(), '<', ' ');
      std::replace(one_line.begin(), one_line.end(), '>', ' ');
      std::replace(one_line.begin(), one_line.end(), ':', ' ');

      matches = sscanf (one_line.c_str(), "WARNING Xst 2404 -  FFs/Latches  %s %d %d   (without init value) have a constant value of %d in block  %s .",
			(char *) cell_inst, &msb_signed, &lsb_signed, &value, (char *) cell_def);
      if (matches == 5) {
	if ((lsb_signed < 0) || (msb_signed < 0)) {
	  fprintf(stderr, "WARNING: negative index: %s [%d:%d]\n", cell_inst, lsb_signed, msb_signed);
	}
	lsb = (lsb_signed < 0) ? 0 : lsb_signed;
	msb = (msb_signed < 0) ? 0 : msb_signed;

	std::string cd = (char*) cell_def;
	std::string ci = (char*) cell_inst;
	for (unsigned int n = lsb; n <= msb; n++) {

 	  BitRef * br = findBitRef(RTL, cd, ci, n);
	  if (br == NULL) {
	    fprintf(stdout, "WARNING: Unable to locate signal /%s/%s\n", cd.c_str(), ci.c_str());
	  }
	  if (br != NULL) {
	    Bit * b = br->getBit();
	    Bit::BitKind k = b->getKind();
	    //	  printf("(%d) IS CONSTANT (value = %d)\n", 2404, value);
	    if (k == Bit::CNST) {
	      printf("CNST TO CONSTANT\n");
	      if (value != b->getValue()) {
		printf("ERROR! change in value\n");
	      }
	    }
	    if (k == Bit::STATE) {
	      //	      printf("(INDEX: %d) LINE: %s\n", br->getIndex(), one_line.c_str());
	      printf("STATE TO CONSTANT (%s) %s\n", cd.c_str(), ci.c_str());
	    }
	    b->setKind(Bit::CNST);
	    b->setValue(value);
	    b->setZ();
	  }
	}
	continue;
      }

      matches = sscanf (one_line.c_str(), "WARNING Xst 1710 - FF/Latch %s (without init value) has a constant value of %d in block %s. This FF/Latch will be trimmed during the optimization process.", (char *) cell_inst, &value, (char *) cell_def);

      if (matches == 0) {
	matches = sscanf (one_line.c_str(), "WARNING Xst 1895 - Due to other FF/Latch trimming, FF/Latch %s (without init value) has a constant value of %d in block %s. This FF/Latch will be trimmed during the optimization process.", (char *) cell_inst, &value, (char *) cell_def);
      }

      if (matches == 0) {
	matches = sscanf (one_line.c_str(), "WARNING Xst 2677 - Node %s of sequential type is unconnected in block %s.", (char *) cell_inst, (char *) cell_def);
	matches++;
	value = 0;
      }

      if (matches == 3) {

 	std::string cd = (char*) cell_def;
	std::string ci = (char*) cell_inst;
	BitRef * br = findBitRef(RTL, cd, ci, 0);

	if (br == NULL) {
	  ci.append("_");
	  br = findBitRef(RTL, cd, ci, 0);
	}

	if (br == NULL) {
	    fprintf(stdout, "WARNING: Unable to locate signal /%s/%s\n", cd.c_str(), ci.c_str());
	}

	if (br != NULL) {
	  Bit * b = br->getBit();
	  Bit::BitKind k = b->getKind();
	  //	  printf("(%d) IS CONSTANT (value = %d)\n", 1710, value);
	  if (k == Bit::CNST) {
	    printf("CNST TO CONSTANT\n");
	    if (value != b->getValue()) {
	      printf("EEEEEEEEE! change in value\n");
	    }
	  }
	  if (k == Bit::STATE) {
	    //	    printf("(INDEX: %d) LINE: %s\n", br->getIndex(), one_line.c_str());
	    printf("STATE TO CONSTANT (%s) %s\n", cd.c_str(), ci.c_str());
	  }
	  b->setKind(Bit::CNST);
	  b->setValue(value);
	  b->setZ();
	}
	continue;
      }

//       matches = sscanf (one_line.c_str(), "WARNING Xst 2972 - %s line %d. All outputs of instance %s of block %s are unconnected in block %s. Underlying logic will be removed.", (char *) file_name, &value, (char *) cell_inst, (char *) cell_removed, (char *) cell_def);
//       if (matches == 5) {
// 	//	fprintf(stderr, "K4 %s %s \n", one_line.c_str(), cell_inst);
// 	continue;
//       }
    }
    printf("Parsing log file: %s complete.\n", file.c_str());

  } else {
    std::cerr << "Can't open file " << file << std::endl;
    exit(1);
  }

  logFile.close();

  m_netlister->finalPostProcess();

  if (!skip_vcd) {
    m_top->addToVCD(m_handler->getVCDWriter());
    m_handler->getVCDWriter()->startVCDFile();
  }
  initializeBits();

  return 0;

}

std::string Design::getValidModulePath(EdifView view, std::string& path)
{

  std::string current = "";
  std::string saved = "";
  for (std::string::iterator it = path.begin();
       it != path.end(); it++) 
    {
      char c = *it;
      if (c == '/') {
	Module* mod = m_netlister->findModule(view, current);
	if (mod == NULL || mod->getKind() != opUNKNOWN) {
	  //	  fprintf(stderr, "MISSED PATH: |%s|\n", current.c_str());
	  current = saved;
	  current.push_back(c);
	} else {
	  //	  fprintf(stderr, "PATH: |%s|\n", current.c_str());
	  saved = current;
	  current.push_back(c);
	}
      } else {
	current.push_back(c);
      }
    }
  Module* mod = m_netlister->findModule(view, current);
  if (mod == NULL || mod->getKind() != opUNKNOWN) {
    //    fprintf(stderr, "MISSED PATH: |%s|\n", current.c_str());
    current = saved;
  } else {
    //    fprintf(stderr, "PATH: |%s|\n", current.c_str());
    saved = current;
  }
  return saved;
}


unsigned int Design::parse_log_7(std::string file, bool skip_vcd)
{

  std::ifstream logFile;
  logFile.open (file.c_str());
  std::string one_line;
  std::string tt;

  if (logFile.is_open()) {

    //    char cell_def[1024];
    //    char cell_inst[1024];
    char net_0[1024];
    char net_1[1024];
    //    char ignore[1024];

    //    unsigned int i;
    //    unsigned int lsb;
    unsigned int value;
    //    unsigned int index;
    //    unsigned int ignore;

    printf("Parsing log file: %s ...\n", file.c_str());
    while (!logFile.eof()) {
      register int c;
      c = logFile.get();
      if (c != '\n') {
	if (c == '\\') { // remove escape characters
	  continue;
	}
	tt.push_back(c);
	continue;
      } else {
	one_line = tt;
	tt.clear();
      }
      //      fprintf(stderr, "LINE: %s\n", one_line.c_str());
      int matches;
      std::replace(one_line.begin(), one_line.end(), '[', '_');
      std::replace(one_line.begin(), one_line.end(), ']', '_');
      std::replace(one_line.begin(), one_line.end(), '.', '_');
      std::replace(one_line.begin(), one_line.end(), ':', ' ');
      std::replace(one_line.begin(), one_line.end(), '(', ' ');
      std::replace(one_line.begin(), one_line.end(), ')', ' ');


      matches = sscanf (one_line.c_str(), "INFO   _Synth 8-3333_  propagating constant %d across sequential element %s %s", &value, (char *) net_0, (char*) net_1);

      if (matches == 2 || matches == 3) {
      	std::string key = "/";
      	key.append(m_top->getName());
      	key.append("/");
      	key.append((char*) net_0);
      	if (matches == 3) {
      	  key.append((char*) net_1);
      	}

      	Signal* signal_rtl   = m_netlister->findSignal(RTL, key);
      	if(signal_rtl != NULL) {
      	  BitRef* br = signal_rtl->getBitRef(0);
      	  if (br != NULL) {
      	    Bit* b = br->getBit();
      	    b->setKind(Bit::CNST);
      	    b->setValue(value);
	    b->setZ();
      	  }
      	} else {
      	  size_t pos = key.rfind('/');
      	  if (pos != std::string::npos) {
      	    std::string path = std::string(key.begin(), key.begin() + pos);
      	    std::string name = std::string(key.begin() + pos, key.end());
      	    //	    fprintf(stderr, "SPLIT: %s | %s \n", path.c_str(), name.c_str());
      	    std::string c_path = getValidModulePath(RTL, path);
      	    c_path.append(name);
      	    signal_rtl = m_netlister->findSignal(RTL, c_path);
      	    if (signal_rtl != NULL) {
      	      BitRef* br = signal_rtl->getBitRef(0);
      	      if (br != NULL) {
      		Bit* b = br->getBit();
      		b->setKind(Bit::CNST);
      		b->setValue(value);
		b->setZ();
      	      }
      	      continue;
      	    }
      	  }
	  fprintf(stdout, "WARNING: Unable to locate signal %s\n", key.c_str());
      	}
      }
    }

    printf("Parsing log file: %s complete.\n", file.c_str());

    m_netlister->finalPostProcess();

    if (!skip_vcd) {
      m_top->addToVCD(m_handler->getVCDWriter());
      m_handler->getVCDWriter()->startVCDFile();
    }
    initializeBits();

  } else {
    std::cerr << "Can't open file " << file << std::endl;
    exit(1);
  }

  logFile.close();

  return 0;

}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

// unsigned int Design::addModule(Module * module)
// {
//   m_has_top = true;
//   m_top     = module;
//   m_module_map[module->getId()] = module;
//   std::string * key = new std::string("/");
//   key->append(module->getName());
//   m_module_name_map[(*key)] = module;
//   return 0;
// }


// Signal* Design::addSignal(Module * module, char* name, unsigned int id, 
// 			   unsigned int lsb, unsigned int msb, unsigned int source_id, char* value)
// {
//   //  printf("ADD SIGNAL: %x %d\n", source_id, kind);

//   std::string signal_name = module->getPath();
//   signal_name.append("/");
//   signal_name.append(module->getName());
//   signal_name.append("/");
//   signal_name.append(name);
//   //  printf("NAME: %s\n", signal_name.c_str());

//   if (m_net_map.count(source_id) == 0) {
//     Net * net = new Net(source_id, m_vcd_id, lsb, msb);
//     m_vcd_id++;
//     m_net_map[source_id] = net;
//     m_net_map[id] = net;
//     //    m_net_set.insert(net);
//     Signal * signal = new Signal(module, name, id);
//     net->addSignal(signal, value);
//     module->addSignal(signal);
//     m_signal_map.insert(std::pair<std::string, Signal*>(signal_name, signal));
//     //    printf("INSERTING: %s\n", signal_name.c_str());
//     return signal;
//   } else {
//     // printf("Already exists! %x %x \n", id, source_id);
//     Net * net     = m_net_map.find(source_id)-> second;
//     m_net_map[id] = net;
//     Signal * signal = new Signal(module, name, id);
//     net->addSignal(signal, value);
//     module->addSignal(signal);
//     m_signal_map.insert(std::pair<std::string, Signal*>(signal_name, signal));
//     //    printf("INSERTING: %s\n", signal_name.c_str());
//     return signal;
//   }
// }

// Signal* Design::addSignal(Module * module, char* name, unsigned int id, 
// 			       unsigned int lsb, unsigned int msb)
// {
//   return addSignal(module, name, id, lsb, msb, id, NULL);
// }



Net * Design::findNet(unsigned int num)
{
  if (m_net_map.count(num) > 0)
    {
      return m_net_map[num];
    }
    else
    {
      return NULL;
    }
}

Module * Design::findModule(unsigned int num)
{
  if (m_module_map.count(num) > 0)
    {
      return m_module_map[num];
    }
    else
    {
      return NULL;
    }
}

Module * Design::findModule(EdifView view, const std::string path)
{

  return m_netlister->findModule(view, path);

}


Signal * Design::findSignal(EdifView view, const std::string path)
{

  return m_netlister->findSignal(view, path);

}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

unsigned int Design::enableSignal(Signal * signal)
{
  m_handler->addSignal(signal);
  return 0;
}

unsigned int Design::disableSignal(Signal * signal)
{

  m_handler->removeSignal(signal);
  return 0;
}

unsigned int Design::addTerm(Signal* signal, unsigned int value, std::string & mask_str, unsigned int track)
{
  return m_handler->addTerm(signal, value, mask_str, track);
}

unsigned int Design::addTerm(Signal* signal, std::string* value_str, unsigned int track)
{
  return m_handler->addTerm(signal, value_str, track);
}

unsigned int Design::removeTerm(unsigned int id)
{
  return m_handler->removeTerm(id);
}

unsigned int Design::addBreakCode(unsigned int code)
{
  return m_handler->addBreakCode(code);
}

unsigned int Design::refreshHW()
{
  return m_handler->refreshHW();
}

unsigned int Design::getInUseBitCount() 
{
  return 0;
}

unsigned int Design::getInUseFrameCount() 
{
  return m_handler->getInUseFrameCount();
}

Family Design::getFamily()
{
  return m_netlister->getFamily();
}

unsigned int Design::setControl(RdBackControl * control)
{
  unsigned int ret = m_handler->setControl(control);
  return ret;
}

unsigned int Design::setVCDWriter(RdBack::VCDWriter * vcdwriter)
{
  unsigned int ret = m_handler->setVCDWriter(vcdwriter);
  return ret;
}

unsigned int Design::flushVCD()
{
  unsigned int ret = m_handler->flushVCD();
  return ret;
}

unsigned int Design::doReset()
{
  unsigned int ret = m_handler->doReset();
  initializeBits();
  return ret;
}

unsigned int Design::initializeBits()
{
  std::set<Bit*> & all = Bit::getAllBits();
  for (std::set<Bit*>::iterator it = all.begin();
	 it != all.end(); it++)
    {
	Bit* b = (*it);
	b->setZ();
      }
  return 0;
}

unsigned int Design::exportDesign(std::string file, bool include_hidden)
{
  return Export::exportDesign(getFamily(), m_netlister->getTop(RTL), m_netlister->getTop(SYNTH), file, include_hidden);
}

unsigned int Design::parse_xrf(std::string file, bool skip_vcd)
{
  m_netlister->parse_xrf(file);
  m_top = m_netlister->getTop(RTL);
  m_has_top = true;
  m_family = m_netlister->getFamily();
  m_handler->setFamily(m_netlister->getFamily());
  if (!skip_vcd) {
    m_top->addToVCD(m_handler->getVCDWriter());
    m_handler->getVCDWriter()->startVCDFile();
  }
  initializeBits();
  return 0;
}
