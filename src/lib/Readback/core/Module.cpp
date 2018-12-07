#include "Module.hpp"
#include "Utils.hpp"
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <algorithm>

unsigned int Module::m_depth = 0;
bool         Module::m_show_hidden = true;
unsigned int Net::m_next_id = 0;
std::set<Net*> Net::m_all_nets;
std::set<Signal*> Signal::m_all_signals;
std::set<Bit*> Bit::m_all_bits;
unsigned int Bit::m_next_id = 0;
unsigned int Bit::m_max_id  = 0;

Module::Module(char* name, char* defname, unsigned int id)
{
  m_name        = name;
  m_defname     = defname;
  m_id          = id;
  m_path        = "";
  m_hidden      = false;
  m_kind        = opUNKNOWN;
  m_init        = 0;
  m_dinverted   = false;
  m_rstinverted = false;
  m_parent      = NULL;

}

Module::~Module()
{
}

unsigned int Module::addChild(Module* child)
{
  std::string path = m_path;
  if (!(m_path.length() == 0 && m_name.length() == 0)) {
    path.append("/");
  }
  path.append(m_name);
  m_children.insert(child);
  child->setPath(path.c_str());
  child->setParent(this);
  unsigned int id = child->getId();
  if (m_child_map.count(id) == 0) {
    m_child_map.insert(std::pair<unsigned int, Module*>(id, child));
  } else {
    fprintf(stderr, "ERROR: (addChild) A child with the id '%d' already exists.\n", id);
    exit(1);
  }
  return 0;
}

unsigned int Module::addSignal(Signal* signal)
{
  m_signals.insert(signal);
  unsigned int id = signal->getId();
  if (m_signal_map.count(id) == 0) {
    m_signal_map.insert(std::pair<unsigned int, Signal*>(id, signal));
  } else {
    fprintf(stderr, "ERROR: (addSignal) A signal with the id '%d' already exists.\n", id);
    exit(1);
  }
  return 0;
}

unsigned int Module::getId()
{
  return m_id;
}

const char * Module::getName()
{
  return m_name.c_str();
}

const char * Module::getDefName()
{
  return m_defname.c_str();
}

unsigned int Module::setDefName(const char* name)
{
  m_defname = name;
  return 0;
}

const char * Module::getPath()
{
  return m_path.c_str();
}

unsigned int Module::setPath(const char* path)
{
  m_path = path;
  return 0;
}

bool Module::isHidden()
{
  return m_hidden;
}

unsigned int Module::setHidden()
{
  m_hidden = true;
  return 0;
}

opKind  Module::getKind()
{
  return m_kind;
}

unsigned int Module::setKind(opKind kind)
{
  m_kind = kind;
  return 0;
}

tSignalSet & Module::getSignals(bool exclude_hidden)
{
  if (exclude_hidden) {
    return m_signals_visible;
  } else {
    return m_signals;
  }
}

tModuleSet & Module::getChildren(bool exclude_hidden)
{
  if (exclude_hidden) {
    return m_children_visible;
  } else {
    return m_children;
  }
}

unsigned int Module::setParent(Module* parent)
{
  m_parent = parent;
  return 0;
}

Module* Module::getParent()
{
  return m_parent;
}

Module* Module::getChild(unsigned int id, bool no_error)
{
  std::map<unsigned int, Module*>::iterator it = m_child_map.find(id);
  if (it == m_child_map.end()) {
    if (no_error) {
      return NULL;
    } else {
      fprintf(stderr, "ERROR: (getChild) A child with the id '%d' does not exist.\n", id);
      exit(1);
    }
  } else {
    return it->second;
  }
}

Signal* Module::getSignal(unsigned int id, bool no_error)
{
  std::map<unsigned int, Signal*>::iterator it = m_signal_map.find(id);
  if (it == m_signal_map.end()) {
    if (no_error) {
      return NULL;
    } else {
      fprintf(stderr, "ERROR: (getSignal) A signal with the id '%d' does not exist.\n", id);
      exit(1);
    }
  } else {
    return it->second;
  }
}

// unsigned int Module::dump()
// {

//   printf("$scope module %s $end\n", m_name.c_str());

//   for (tSignalSet::iterator it = m_signals.begin();
//        it != m_signals.end(); it++)
//     {
//       (*it)->dump();
//     }

//   for (tModuleSet::iterator it = m_children.begin();
//        it != m_children.end(); it++)
//     {
//       (*it)->dump();
//     }

//   printf("$upscope $end\n");
//   return 0;

// }

unsigned int Module::addToVCD(RdBack::VCDWriter* vcdwriter)
{

  tSignalSet & signals = getSignals(true);

  for (tSignalSet::iterator it = signals.begin();
       it != signals.end(); it++)
    {
      Signal* s = *it;
      if (s != NULL) {
	s->addToVCD(vcdwriter);
      }
    }

  for (tModuleSet::iterator it = m_children_visible.begin();
       it != m_children_visible.end(); it++)
    {
      (*it)->addToVCD(vcdwriter);
    }

  return 0;

}

unsigned int Module::updateVisibleChildren(bool recursive)
{

  m_children_visible.clear();

  if (recursive) {
    for (tModuleSet::iterator it = m_children.begin();
	 it != m_children.end(); it++)
      {
	Module* mod = *it;
	if (mod != NULL) {
	  mod->updateVisibleChildren(recursive);
	}
      }
  }

  for (tModuleSet::iterator it = m_children.begin();
       it != m_children.end(); it++)
    {
      Module* mod = *it;
      if (!mod->isHidden()) {
	m_children_visible.insert(mod);
      }
    }


  return 0;
}

unsigned int Module::updateVisibleSignals(bool recursive)
{

  m_signals_visible.clear();

  if (recursive) {
    for (tModuleSet::iterator it = m_children.begin();
	 it != m_children.end(); it++)
      {
	Module* mod = *it;
	if (mod != NULL) {
	  mod->updateVisibleSignals(recursive);
	}
      }
  }

  for (tSignalSet::iterator it = m_signals.begin();
       it != m_signals.end(); it++)
    {
      Signal* signal = *it;
      if (!signal->isHidden()) {
	m_signals_visible.insert(signal);
      }
    }

  return 0;
}

unsigned int Module::getDepth()
{
  return m_depth;
}
 
unsigned int Module::incrDepth()
{
  m_depth++;
  return m_depth;
}
  
unsigned int Module::decrDepth()
{
  m_depth--;
  return m_depth;
}

unsigned int Module::setInit(uint64_t init)
{
  m_init = init;
  return 0;
}

uint64_t Module::getInit()
{
  return m_init;
}

unsigned int Module::setDInverted(bool inverted)
{
  m_dinverted = inverted;
  return 0;
}

bool Module::getDInverted()
{
  return m_dinverted;
}

unsigned int Module::setRSTInverted(bool inverted)
{
  m_rstinverted = inverted;
  return 0;
}

bool Module::getRSTInverted()
{
  return m_rstinverted;
}

Net::Net()
{
  m_enabled  = false;
  m_id       = m_next_id;
  m_all_nets.insert(this);

  m_next_id++;
}

Net::~Net()
{
}

unsigned int Net::addSignal(Signal * signal)
{
  signal->setNet(this);
  m_signals.insert(signal);
  return 0;
}

unsigned int Net::getId()
{
  return m_id;
}

tSignalSet & Net::getSignals()
{
  return m_signals;
}

// const char* Net::getVCDStr(bool all_z)
// {

//   std::vector<BitRef*> v = m_bits;
//   m_str = "";

//   std::vector<BitRef*>::iterator it = v.end();
//   while (it != v.begin()) {
//     it--;
//     BitRef* br = (*it);
//     Bit* b = br->getBit();
//     if (br != NULL) {
//       if (all_z) {
// 	m_str += "z";
//       } else {
// 	m_str += b->getValueStr();
//       }
//     }
//   }
//   return m_str.c_str();
// }

// const char* Net::getReadStr()
// {

//   std::vector<BitRef*> v = m_bits;
//   m_str = "";

//   std::vector<BitRef*>::iterator it = v.end();
//   while (it != v.begin()) {
//     it--;
//     BitRef* br = (*it);
//     Bit* b = br->getBit();
//     if (b->getKind() == Bit::UNREAD) {
//       m_str += "u";
//     } else {
//       m_str += "r";
//     }
//   }
//   return m_str.c_str();
// }

bool Net::isEnabled() {
  return m_enabled;
}

unsigned int Net::enable() {
  m_enabled = true;
  return 0;
}

unsigned int Net::disable() {
  m_enabled = false;
  return 0;
}

Signal::Signal(Module* module, char* name, unsigned int id, unsigned int lsb, unsigned int msb, bool no_bits)
{
  std::string full_name = module->getPath();
  full_name.append("/");
  full_name.append(module->getName());
  full_name.append("/");
  full_name.append(name);

  m_name     = name;
  m_fullname = full_name;
  m_id       = id;
  m_module   = module;
  m_kind     = dWire;
  m_hidden       = false;
  m_enabled      = false;
  m_is_src_state = false;
  m_lsb      = lsb;
  m_msb      = msb;
  m_bits     = std::vector<BitRef*>(msb+1);

  if (!no_bits) {
    for (unsigned int i = lsb; i <= msb; i++)
      {
	BitRef* bitref = new BitRef(this, i);
	Bit*    bit    = new Bit();
	bit->setKind(Bit::UNAVAILABLE);
	bitref->setBit(bit);
	m_bits[i]      = bitref;
      }
  }

  m_all_signals.insert(this);
}

Signal::~Signal()
{
}

unsigned int Signal::expand(unsigned int lsb, unsigned int msb)
{
  unsigned int lsb_current = getLsb();
  unsigned int msb_current = getMsb();

  if (msb > msb_current) {
    m_bits.resize(msb + 1);
    for (unsigned int i = msb_current + 1; i <= msb; i++)
      {
	BitRef* bitref = new BitRef(this, i);
	Bit*    bit    = new Bit();
	bit->setKind(Bit::UNAVAILABLE);
	bitref->setBit(bit);
	m_bits[i]      = bitref;
      }
    m_msb = msb;
  }

  if (lsb < lsb_current) {
    for (unsigned int i = lsb; i < lsb_current; i++)
      {
	BitRef* bitref = new BitRef(this, i);
	Bit*    bit    = new Bit();
	bit->setKind(Bit::UNAVAILABLE);
	bitref->setBit(bit);
	m_bits[i]      = bitref;
      }
    m_lsb = lsb;
  }

  return 0;

}

unsigned int Signal::getId()
{
  return m_id;
}

const char * Signal::getName()
{
  return m_name.c_str();
}

const char * Signal::getFullName()
{
  return m_fullname.c_str();
}

Net *   Signal::getNet()
{
  return m_net;
}

unsigned int Signal::setNet(Net * net)
{
  m_net = net;
  tSignalSet & signals = net->getSignals();
  signals.insert(this);
  return 0;
}


BitRef* Signal::getBitRef(unsigned int index)
{
  return getBitRef(index, false);
}

BitRef* Signal::getBitRef(unsigned int index, bool just_warn)
{
  if (index < m_lsb || index > m_msb) {
    if (just_warn) {
      fprintf(stderr, "WARNING: Illegal index (%d) for Signal %s.\n", index, getFullName());
      return NULL;
    } else {
      fprintf(stderr, "ERROR: Illegal index (%d) for Signal %s.\n", index, getFullName());
      exit(1);
    }
  }

  return m_bits[index];
}

unsigned int Signal::setBitRef(unsigned int index, BitRef* br)
{
  m_bits[index] = br;
  return 0;
}


unsigned int Signal::addState(unsigned int index, unsigned int addr, unsigned int offset)
{
  return addState(index, addr, offset, 0);
}

unsigned int Signal::addState(unsigned int index, unsigned int addr, unsigned int offset, unsigned int slr)
{

  BitRef * br = getBitRef(index, true);
  if (br != NULL) {
    Bit * b = br->getBit();
    b->setKind(Bit::STATE);
    b->setAddr(addr);
    b->setOffset(offset);
    b->setSlr(slr);
  }

  return 0;
}

unsigned int Signal::addToVCD(RdBack::VCDWriter* vcdwriter)
{

  Module* mod = getModule();
  //  Net*    net = getNet();

  if (!isHidden()) {
    std::string full_name =  mod->getPath();
    full_name.append("/");
    full_name.append(mod->getName());
    full_name.append("/");
    full_name.append(getName());

    full_name = full_name.substr(1);

    unsigned int lsb = getLsb();
    unsigned int msb = getMsb();


    for (unsigned int i = lsb; i <= msb; i++)
      {
	BitRef* br = getBitRef(i);
	if (br != NULL) {
	  Bit* b = br->getBit();
	  unsigned int id = b->getId();
	  std::string label = full_name;
	  if (lsb != msb) {
	    ReadBackProbe* probe = new ReadBackProbe(&label, 1, (int)i, id);
	    vcdwriter->registerReadBackProbe(probe);
	  } else {
	    ReadBackProbe* probe = new ReadBackProbe(&label, 1, id);
	    vcdwriter->registerReadBackProbe(probe);
	  }
	}
      }
  }

  return 0;
}

unsigned int Signal::setSrcState() {

  m_is_src_state = true;

  unsigned int lsb = getLsb();
  unsigned int msb = getMsb();
  for (unsigned int i = lsb; i <= msb; i++)
    {
	BitRef* br = getBitRef(i);
	if (br != NULL) {
	  Bit* b = br->getBit();
	  b->setSrcState();
	}
    }
  return 0;
}

signalKind  Signal::getKind()
{
  return m_kind;
}

unsigned int Signal::setKind(signalKind kind)
{
  m_kind = kind;
  return 0;
}

unsigned int Signal::getLsb()
{
  return m_lsb;
}

unsigned int Signal::getMsb()
{
  return m_msb;
}

unsigned int Signal::getWidth()
{
  return (1 + m_msb - m_lsb);
}

unsigned int Signal::getAvail()
{
  unsigned int count = 0;
  for (std::vector<BitRef*>::iterator it = m_bits.begin();
       it != m_bits.end(); it++)
    {
      BitRef* br = (*it);
      if (br != NULL) {
	Bit* b = br->getBit();
	if (b->getKind() != Bit::UNAVAILABLE) {
	  count++;
	}
      }
    }

  return count;
}

unsigned int Signal::getUnread()
{
  unsigned int count = 0;
  for (std::vector<BitRef*>::iterator it = m_bits.begin();
       it != m_bits.end(); it++)
    {
      BitRef* br = (*it);
      if (br != NULL) {
	Bit* b = br->getBit();
	if (b->getKind() == Bit::UNREAD) {
	  count++;
	}
      }
    }

  return count;
}

unsigned int Signal::getInferred()
{
  unsigned int count = 0;
  for (std::vector<BitRef*>::iterator it = m_bits.begin();
       it != m_bits.end(); it++)
    {
      BitRef* br = (*it);
      if (br != NULL) {
	Bit* b = br->getBit();
	if (b->getInferred()) {
	  count++;
	}
      }
    }

  return count;
}

unsigned int Signal::merge(Signal* signal, unsigned int offset)
{

  unsigned int result = 0;
  for (unsigned int i = m_lsb; i <= m_msb; i++)
    {
      BitRef* br0 = getBitRef(i);
      if (br0 != NULL) {
	Bit*    b0  = br0->getBit();
	BitRef* br1 = signal->getBitRef(i+offset, true);
	if (br1 != NULL) {
	  Bit* b1 = br1->getBit();
	  if (b0 == b1) {
	    //	printf("Bits are identical!\n");
	  } else {
	    result = result || b0->merge(b1);
	    //	printf("MERGE: %s %s\n", b0->getStr(), b1->getStr());
	  }
	}
      }
    }

  return result;

}

unsigned int Signal::merge(Signal* signal, unsigned int index, unsigned int signal_index)
{
  BitRef* br0 = getBitRef(index);
  Bit* b0     = br0->getBit();

  BitRef* br1 = signal->getBitRef(signal_index, true);
  if (br1 != NULL) {
    Bit* b1     = br1->getBit();
    if (b0 == b1) {
      //	printf("Bits are identical!\n");
    } else {
      b0->merge(b1);
      //	printf("MERGE: %s %s\n", b0->getStr(), b1->getStr());
    }
  }
  return 0;
}

std::string  Signal::getSignature()
{

  std::ostringstream convert;
  convert << getMsb();
  convert << "_";

  for (std::vector<BitRef*>::iterator it = m_bits.begin();
       it != m_bits.end(); it++)
    {
      BitRef* br = (*it);
      if (br != NULL) {
	Bit* b = br->getBit();
	convert << b->getId();
	convert << "_";
      }
    }

  convert << getLsb();
  return convert.str();

}

bool Signal::allZeros()
{
  bool all = true;
  for (std::vector<BitRef*>::iterator it = m_bits.begin();
       it != m_bits.end(); it++)
    {
      BitRef* br = (*it);
      if (br != NULL) {
	Bit* b = br->getBit();
	if (b->getKind() != Bit::CNST || b->getValue() != 0) {
	  all = false;
	  break;
	}
      }
    }
  return all;
}

bool Signal::allOnes()
{
  bool all = true;
  for (std::vector<BitRef*>::iterator it = m_bits.begin();
       it != m_bits.end(); it++)
    {
      BitRef* br = (*it);
      if (br != NULL) {
	Bit* b = br->getBit();
	if (b->getKind() != Bit::CNST || b->getValue() != 1) {
	  all = false;
	  break;
	}
      }
    }
  return all;
}

bool Signal::isEnabled() {
  Net* net = getNet();
  if (net != NULL) {
    return net->isEnabled();
  } else {
    return false;
  }
}

unsigned int Signal::enable() {
  Net* net = getNet();
  if (net != NULL) {
    net->enable();
  }
  return 0;
}

unsigned int Signal::disable() {
  Net* net = getNet();
  if (net != NULL) {
    net->disable();
  }
  return 0;
}

std::string Signal::getValueStr(bool isStatic)
{

  std::string* p = new std::string("");
  std::string str = (*p);

  std::vector<BitRef*>::iterator it = m_bits.end();
  while (it != m_bits.begin()) {
    it--;
    BitRef* br = (*it);
    if (br != NULL) {
      Bit* b = br->getBit();
      str += b->getValueStr(isStatic);
    }
  }
  return str;
}

BitRef::BitRef(Signal* signal, unsigned int index)
{
  m_signal   = signal;
  m_index = index;
}

Bit * BitRef::getBit()
{
  return m_bit;
}

unsigned int BitRef::getIndex()
{
  return m_index;
}

Signal* BitRef::getSignal()
{
  return m_signal;
}

unsigned int BitRef::setSignal(Signal* signal)
{
  m_signal = signal;
  return 0;
}

unsigned int BitRef::setBit(Bit * bit)
{
  m_bit = bit;
  bit->addBitRef(this);
  return 0;
}

unsigned int BitRef::merge(BitRef* br)
{

  Bit* b0 = this->getBit();
  Bit* b  = br->getBit();
  return (b0->merge(b));

}

Bit::Bit()
{
  m_kind         = UNAVAILABLE;
  m_value        = false;
  m_str          = "z";
  m_id           = m_next_id;
  m_is_src_state = false;
  m_changed      = false;
  m_addr         = 0;
  m_offset       = 0;

  m_slr          = 0;
  m_inferred     = true;

  m_next_id++;

  m_all_bits.insert(this);
}

Bit::Bit(unsigned int id)
{
  m_kind         = UNAVAILABLE;
  m_value        = false;
  m_str          = "z";
  m_id           = id;
  m_is_src_state = false;
  m_changed      = false;
  m_addr         = 0;
  m_offset       = 0;

  m_slr          = 0;
  m_inferred     = true;

  m_next_id = std::max(id + 1, m_next_id);

  m_all_bits.insert(this);
}

Bit::BitKind  Bit::getKind()
{
  return m_kind;
}

bool     Bit::getValue()
{
  return m_value;
}

unsigned int Bit::setValue(bool value)
{
  if (value) { m_str = "1"; } else { m_str = "0"; }
  m_value = value;
  return 0;
}

unsigned int Bit::setKind(BitKind kind)
{
  m_kind = kind;
  if (m_kind == STATE) {
    //    m_str = "s";
  }
  return 0;
}

unsigned int Bit::setChanged(bool value)
{
  m_changed = value;
  return 0;
}

bool Bit::isChanged() {
  return m_changed;
}

const char* Bit::getValueStr(bool isStatic)
{
  if (isStatic) {
    if (getKind() == Bit::STATE) {
      return (const char*) "s";
    }
    if (getKind() == Bit::UNREAD) {
      return (const char*) "u";
    }
  }
  return m_str.c_str();
}


unsigned int Bit::addBitRef(BitRef* bitref)
{
  m_bitrefs.insert(bitref);
  return 0;
}

unsigned int Bit::merge(Bit* bit)
{

  Bit* bit_0     = this;
  Bit* bit_1     = bit;
  if (bit_0 == bit_1) {
    return 0;
  }

  BitKind kind_0 = bit_0->getKind();
  BitKind kind_1 = bit_1->getKind();
  unsigned int value_0 = bit_0->getValue();
  unsigned int value_1 = bit_1->getValue();
  std::set<BitRef *> * bitrefs = bit_1->getBitRefs();

  //  unsigned int id = std::max(bit_0->getId(), bit_1->getId());

  if (kind_0 == CNST && kind_1 == CNST && value_0 != value_1) {
    printf("ERROR: Bit conflict! 0: %d 1: %d\n", value_0, value_1);
    return 1;
  }

  if (kind_1 == CNST) {
    bit_0->setKind(CNST);
    bit_0->setValue(value_1);
  }

  if (kind_1 == STATE && kind_0 != CNST) {
    bit_0->setKind(STATE);
  }

  for (std::set<BitRef *>::iterator it = bitrefs->begin();
       it != bitrefs->end(); it++)
    {
      BitRef* bitref_1 = (*it);
      bitref_1->setBit(bit_0);
    }
  bit_0->setId(std::max(bit_0->getId(), bit_1->getId()));
  if (bit_1->isSrcState()) {
    bit_0->setSrcState();
  }
  bit_0->setInferred(bit_0->getInferred() && bit_1->getInferred());

  m_all_bits.erase(bit_1);
  delete bit_1;
  return 0;
}

std::set<Bit*> & Bit::getAllBits()
{
  return m_all_bits;
}

unsigned int Bit::setMaxBitId(unsigned int id)
{
  m_max_id = id;
  return 0;
}

unsigned int Bit::getMaxBitId()
{
  return m_max_id;
}

int sigcmp (const char *p1, const char *p2)
{

  if (strrchr(p1,'>') == NULL && strrchr(p2,'>') == NULL) {
    return strcmp(p1, p2);
  }

  unsigned int matches, index1, index2;
  int index1_signed, index2_signed;
  char token1[1024];
  char token2[1024];

  std::string p_1(p1);

  std::replace(p_1.begin(), p_1.end(), '<', ' ');
  std::replace(p_1.begin(), p_1.end(), '>', ' ');

  matches = sscanf(p_1.c_str(), " %s %d %s", (char*) token1, &index1_signed, (char*) token1);

  if (matches == 2) {
    if (index1_signed < 0) {
      fprintf(stderr, "WARNING: negative index: %s <%d>\n", token1, index1_signed);
      index1 = 0;
    } else {
      index1 = index1_signed;
    }

    std::string p_2(p2);

    std::replace(p_2.begin(), p_2.end(), '<', ' ');
    std::replace(p_2.begin(), p_2.end(), '>', ' ');

    matches = sscanf(p_2.c_str(), " %s %d %s", (char*) token2, &index2_signed, (char*) token2);
    
    if (matches == 2 && strcmp((char*) token1, (char*) token2) == 0) {
      if (index2_signed < 0) {
	fprintf(stderr, "WARNING: negative index: %s <%d>\n", token2, index2_signed);
	index2 = 0;
      } else {
	index2 = index2_signed;
      }

      return index1 - index2;
    }
  }

  return strcmp(p1, p2);
}

bool CompareModules::operator() (const Module* l, const Module* r) const {
  Module* ll = (Module*) l;
  Module* rr = (Module*) r;
  return (strcmp(ll->getName(), rr->getName()) < 0);
}

bool CompareSignals::operator() (const Signal* l, const Signal* r) const {
  Signal* ll = (Signal*) l;
  Signal* rr = (Signal*) r;
  return (sigcmp(ll->getName(), rr->getName()) < 0);
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

std::ostream& addIndent(std::ostream& ost)
{

  for (unsigned int i = 0; i < Module::getDepth(); i++)
    {
      ost << "   ";
    }
  return ost;
}

std::ostream& operator<<(std::ostream& ost, Bit::BitKind& bk)
{
  switch (bk) {
  case Bit::STATE :
    ost << "STATE";
    break;
  case Bit::UNAVAILABLE :
    ost << "?";
    break;
  case Bit::CNST :
    ost << "CNST";
    break;
  case Bit::UNREAD :
    ost << "UNREAD";
    break;
  default:
    fprintf(stderr, "Unknown BitKind!\n");
    exit(1);
  }
  return ost;
}

std::ostream& operator<<(std::ostream& ost, signalKind& sk)
{
  switch (sk) {
  case dIn :
    ost << "IN";
    break;
  case dOut :
    ost << "OUT";
    break;
  case dWire :
    ost << "WIRE";
    break;
  default:
    fprintf(stderr, "Unknown SignalKind!\n");
    exit(1);
  }
  return ost;
}

std::ostream& operator<<(std::ostream& ost, Bit* b)
{
  ost << "(BIT ";
  ost << b->getId();
  ost << " ";
  Bit::BitKind bk = b->getKind();
  ost << bk;
  ost << " ";
  if (bk == Bit::CNST) {
    ost << b->getValue();
  } else {
    ost << b->getValueStr(false);
  }
  if (bk == Bit::STATE) {
    ost << " " << b->getAddr() << " " << b->getOffset() << " " << b->getSlr();
  }
  ost << ")";
  return ost;
}

std::ostream& operator<<(std::ostream& ost, Signal* signal)
{
  addIndent(ost);
  ost << "(SIG ";
  if (signal->isHidden()) {
    ost << "HIDDEN ";
  } else {
    ost << "VISIBLE ";
  }
  signalKind sk        = signal->getKind();
  unsigned int lsb     = signal->getLsb();
  unsigned int msb     = signal->getMsb();
  unsigned int id      = signal->getId();
  unsigned int net_id  = signal->getNet()->getId();
  ost << sk;  
  ost << " ";
  ost << signal->getName();
  ost << " ";
  ost << id;
  ost << " ";
  ost << net_id;
  ost << " ";
  ost << lsb;
  ost << " ";
  ost << msb;
  for (unsigned int i = lsb; i <= msb; i++)
    {
      BitRef* br = signal->getBitRef(i, true);
      if (br != NULL) {
	Bit* b = br->getBit();
	ost << " " << b->getId();
      } else {
	ost << " " << "NULL";
      }
    }
  ost << ")";
  return ost;

}

std::ostream& operator<<(std::ostream& ost, Module* mod)
{
  addIndent(ost);
  ost << "(MOD ";
  if (mod->isHidden()) {
    ost << "HIDDEN ";
  } else {
    ost << "VISIBLE ";
  }
  ost << mod->getDefName();
  ost << " ";
  ost << mod->getName();
  ost << " ";
  ost << mod->getId();
  tSignalSet & ss = mod->getSignals(!Module::m_show_hidden);
  if (ss.size() != 0) { ost << std::endl; }
  Module::incrDepth();
  for (tSignalSet::iterator it = ss.begin();
       it != ss.end(); it++)
    {
      ost << std::endl;
      ost << (*it);
    }
  Module::decrDepth();
  tModuleSet & ms = mod->getChildren(!Module::m_show_hidden);
  if (ms.size() != 0) { ost << std::endl; }
  Module::incrDepth();
  for (tModuleSet::iterator it = ms.begin();
       it != ms.end(); it++)
    {
      ost << std::endl;
      ost << (*it);
    }
  Module::decrDepth();
  ost << ")";
  return ost;
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

