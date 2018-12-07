#include <cstdlib>

#include "DataHandler.hpp"
#include "Utils.hpp"

#define ADDR_C_SZ 23

template <size_t N>
inline std::string bitsetToString(const std::bitset<N>& b){
  // Not entirely sure when to_string no longer required all the
  // template parameters to be explicitly given, but it is definitely
  // not present for gcc 3.4
#if __GNUC__ > 3 ||                             \
  (__GNUC__ == 3 && __GNUC_MINOR > 4)
  return b.to_string();
#else
  // http://stackoverflow.com/questions/1840253/c-template-member-function-of-template-class-called-from-template-function/1840318#1840318
  // C++'03 Standard 14.2/4
  // http://publib.boulder.ibm.com/infocenter/comphelp/v8v101/topic/com.ibm.xlcpp8a.doc/language/ref/keyword_template_qualifier.htm
  return b.template to_string<char,std::char_traits<char>,std::allocator<char> >();
#endif
}

BitInfo::BitInfo(Bit* bit)
{

  m_bit        = bit;
  m_values     = tConstraintSet(0);
  m_send       = false;
  m_constraint = false;

}

unsigned int BitInfo::addConstraint(char c, unsigned int track)
{
  if (c == '1') {
    unsigned int index = (track * 2) + 1;
    m_values[index] = 1;
    m_constraint = true;
  }
  if (c == '0') {
    unsigned int index = (track * 2) + 0;
    m_values[index] = 1;
    m_constraint = true;
  }
  return 0;
}

unsigned int BitInfo::merge(BitInfo* bit_info)
{
  if (m_bit != bit_info->getBit()) {
    //    fprintf(stderr, "ERROR: Illegal BitInfo merge.\n");
    //    exit(1);
  }
  m_send       = m_send       || bit_info->isSent();
  m_constraint = m_constraint || bit_info->hasConstraints();
  m_values     = m_values     |  bit_info->getConstraints();
  return 0;
}

BitTerm::BitTerm(unsigned int id)
{
  m_id  = id;
  m_signal = NULL;
  m_bitinfo_set = tBitInfoSet();
}

BitTerm::BitTerm(unsigned int id, Signal* signal)
{
  m_id  = id;
  m_signal = signal;
  m_bitinfo_set = tBitInfoSet();
}

ConfigStore::ConfigStore() {
  InitAll();
}

void ConfigStore::InitConfigIn() {
  m_config_in  = 0;
}

void ConfigStore::InitAll() {
  m_config_in  = 0;
  m_config_out = 0;
  popAll();
  m_current       = new tBitSet();
  m_current_const = new tBitList();
}

unsigned int ConfigStore::insert (Bit* b) {
  m_current->insert(b);
  return 0;
}

unsigned int ConfigStore::insertConst (Bit* b) {
  m_current_const->push_front(b);
  return 0;
}

unsigned int ConfigStore::push() {
  m_deque_config.push_back(m_config_in);
  m_deque_current.push_back(m_current);
  m_deque_current_const.push_back(m_current_const);
  m_config_in++;
  m_current       = new tBitSet();
  m_current_const = new tBitList();
  return 0;
}

unsigned int ConfigStore::popTo(unsigned int config) {
  while (true) {
    if (m_deque_config.empty()) {
      fprintf(stderr, "ERROR: configuration store is empty.\n");
      exit(1);
    }
    if (m_deque_config.front() == config) {
      break;
    }
    tBitSet*  current = m_deque_current.front();
    tBitList* current_const = m_deque_current_const.front();
    m_deque_config.pop_front();
    m_deque_current.pop_front();
    m_deque_current_const.pop_front();
    delete current;
    delete current_const;
  }
  return 0;
}

unsigned int ConfigStore::popAll() {
  while (true) {
    if (m_deque_config.empty()) {
      break;
    }
    tBitSet*  current = m_deque_current.front();
    tBitList* current_const = m_deque_current_const.front();
    m_deque_config.pop_front();
    m_deque_current.pop_front();
    m_deque_current_const.pop_front();
    delete current;
    delete current_const;
  }
  return 0;
}

void DataHandler::InitAll() {

  m_id_map         = std::map<unsigned int, BitTerm*>();
  m_signal_map     = std::map<Signal*, BitTerm*>();        
  m_bit_map        = tBitMap();
  m_frame_set      = std::set<unsigned int>();
  m_signal_set     = std::set<Signal*>();
  m_term_set       = std::set<unsigned int>();
  m_code           = 0;
  m_code_active    = 0;
  m_sent_bits      = tBitList();
  m_sent_bits_prev = tBitList();
  m_unit_mode      = false;

  m_vcd_prev = 0;
  m_vcd_prev_prev  = 0;

  m_config_new     = false;
  m_config_store.InitAll();
  m_unit.InitAll();
  m_time            = 0;

  // m_bit_set_current       = NULL;
  // m_bit_set_current_const = NULL;

  // for (unsigned int i = 0; i < m_config_count; i++) {
  //   tBitSet*  ps = new tBitSet();
  //   tBitList* pl = new tBitList();
  //   m_bit_sets[i]       = (*ps);
  //   m_bit_sets_const[i] = (*pl);
  // }

  // m_bit_set_current       = &m_bit_sets[m_config];
  // m_bit_set_current_const = &m_bit_sets_const[m_config];
  // m_bit_it_current        =  m_bit_set_current->begin();

  m_remaining       = 0;

  m_next_term        = 0;
  m_change           = false;
  m_change_prev      = false;
  m_change_prev_prev = false;

  m_sync_config      = false;

}

DataHandler::DataHandler() {

  InitAll();
  m_control       = NULL;
  m_vcdwriter     = NULL;
  m_unit_mode     = false;

  std::ifstream test;
  test.open("__rdbklog");
  if (test) {
    test.close();
    m_dbg_mode = true;
    m_dbg_stream.open("__rdbklog");
  } else {
    m_dbg_mode = false;
    m_dbg_stream.open("/dev/null");
  }
}

unsigned int DataHandler::addSignal(Signal* signal) {
  addTerm(signal, NULL, 0);
  return 0;
}

unsigned int DataHandler::removeSignal(Signal* signal) {
  m_signal_map.erase(signal);
  updateActive();
  updateMode();
  return 0;
}

unsigned int DataHandler::addTerm(Signal* signal, unsigned int value, std::string & mask_str, unsigned int track)
{

  unsigned int width          = signal->getWidth();
  std::bitset<1024> value_set = std::bitset<1024>(value);
  std::string       bin_str   = bitsetToString(value_set);
  bin_str = bin_str.substr(1024 - width, 1024 - 1);

  std::string       m_str     = utils::hexToBinary(mask_str);
  std::string ext_str = "";
  ext_str.resize(1024, '0');
  
  m_str = ext_str + m_str;
  unsigned int k = m_str.length();
  m_str = m_str.substr(k - width, k - 1);

  std::string value_str = "";

  for (unsigned int i = 0; i < signal->getWidth(); i++)
    {
      if (m_str[i] == '1') {
	value_str = value_str + "X";
      } else {
	value_str = value_str + bin_str[i];
      }
    }
  return addTerm(signal, &value_str, track);
}

unsigned int DataHandler::addTerm(Signal* signal, std::string* value_str, unsigned int track)
{

  if (value_str == NULL) { // enabling a signal
    BitTerm* term = new BitTerm(0, signal);
    addTerm(term);
    return 0;
  } else {
    if (value_str->length() != signal->getWidth()) {
      fprintf(stderr, "ERROR: length of value string does not match width of signal.\n");
      exit(1);
    }
    std::string vv = (*value_str);
    m_next_term++;
    unsigned int id = m_next_term;
    BitTerm* term = new BitTerm(id, signal);
    for (unsigned int i = signal->getLsb(); i <= signal->getMsb(); i++)
      {
	BitRef* bitref = signal->getBitRef(i);
	Bit*    b      = bitref->getBit();
	unsigned int j = signal->getWidth() - 1 - i + signal->getLsb();
	char v  = vv[j];
	if (b->getKind() == Bit::CNST) {
	  if ((v == '0' && b->getValue()) || (v == '1' && !b->getValue())) {
	    fprintf(stderr, "WARNING: Constraint that signal[%d] == %c can never be satisfied.\n", i, v);
	  }
 	}
	if (b->getKind() == Bit::STATE) {
 	  BitInfo* bi = new BitInfo(b);
 	  bi->addConstraint(v, track);
	  term->insert(bi);
 	}
      }
    addTerm(term);
    return id;
  }
}


unsigned int DataHandler::addTerm(BitTerm* bit_term)
{

  unsigned int id = bit_term->getId();
  if (id == 0) { // a signal
    Signal* signal = bit_term->getSignal();
    if (m_signal_map.count(signal) == 0) {
      for (unsigned int i = signal->getLsb(); i <= signal->getMsb(); i++)
	{
	  BitRef* bitref = signal->getBitRef(i);
	  Bit*    b      = bitref->getBit();
	  if (b->getKind() == Bit::STATE) {
	    BitInfo* bi = new BitInfo(b);
	    bi->setAsSent();
	    bit_term->insert(bi);
	  } else if (b->getKind() == Bit::CNST) {
	    BitInfo* bi = new BitInfo(b);
	    bit_term->insert(bi);
	  } else {
	    BitInfo* bi = new BitInfo(b);
	    bit_term->insert(bi);
	  }
	}
      if (false) { fprintf(stderr, "ADDING SIGNAL\n"); }
      m_signal_map.insert(std::pair<Signal*, BitTerm*>(signal, bit_term));
      updateActive();
      updateMode();
    }
  } else {
    if (m_id_map.count(id) == 0) {
      m_id_map.insert(std::pair<unsigned int, BitTerm*>(id, bit_term));
    } else {
      fprintf(stderr, "ERROR: a constraint term with id '%d' already exists\n", id);
      exit(1);
    }
  }
  return 0;
}

unsigned int DataHandler::removeTerm(unsigned int id) {
  if (id == 0) {
    m_id_map.clear();
    refreshHW();
  } else {
    m_id_map.erase(id);
  }
  return 0;
}

unsigned int DataHandler::addBreakCode(unsigned int code) {
  m_code = code;
  if (false) { fprintf(stderr, "ADD CODEX %4x\n", code); }
  return 0;
}

unsigned int DataHandler::refreshHW() {
  updateActive();
  updateMode();
  return 0;
}

unsigned int DataHandler::absorbTerm(BitTerm* bit_term)
{
  Signal* signal = bit_term->getSignal();
  unsigned int id = bit_term->getId();
  if (id == 0) {
    if (false) { fprintf(stderr, "SIGNAL INSERT\n"); }
    signal->enable();
    m_signal_set.insert(signal);
  } else {
    m_term_set.insert(id);
  }
  tBitInfoSet infos = bit_term->getBitInfoSet();
  for (tBitInfoSet::iterator it = infos.begin();
       it != infos.end(); it++)
    {
      BitInfo* bi = (*it);
      Bit* b  = bi->getBit();
      unsigned int addr = b->getAddr();
      if (bi->isSent() || bi->hasConstraints()) {
	m_frame_set.insert(addr);
      }
      if (m_bit_map.count(b) == 0) {
	BitInfo* cp = new BitInfo(*bi);
	m_bit_map.insert(std::pair<Bit*, BitInfo*>(b, cp));
      } else {
	BitInfo* cp = m_bit_map.find(b)->second;
	cp->merge(bi);
      }
    }
  return 0;
}


unsigned int DataHandler::updateActive()
{
  clearActive();
  m_code_active = m_code;
  for (std::map<Signal*, BitTerm*>::iterator it = m_signal_map.begin();
       it != m_signal_map.end(); it++)
    {
      BitTerm* bt = it->second;
      absorbTerm(bt);
    }
  for (std::map<unsigned int, BitTerm*>::iterator it = m_id_map.begin();
        it != m_id_map.end(); it++)
    {
      BitTerm* bt = it->second;
      absorbTerm(bt);
    }

  std::bitset<4> last_set = std::bitset<4>(0);
  for (tBitMap::iterator iz = m_bit_map.end();
       iz != m_bit_map.begin(); iz--)
    {
      tBitMap::iterator it = iz;
      it--;
      BitInfo* bi = it->second;
      if (bi->isSent() || bi->hasConstraints()) {
	tConstraintSet & cs = bi->getConstraints();
	for (unsigned int i = 0; i <= 3; i++) {
	  unsigned int j = i + i;

	  if (last_set[i] == 0 && (cs[j] == 1 || cs[j+1] == 1)) {
	    last_set[i] = 1;
	  }
	  if (last_set[i] == 0 && cs[j] == 0 && cs[j+1] == 0) {
	    cs[j] = 1;
	    cs[j+1] = 1;
	  }
	}
      }
    }
  return 0;
}

unsigned int DataHandler::clearActive()
{

  // for (std::map<Bit*, BitInfo*>::iterator it = m_bit_map.begin();
  //      it != m_bit_map.end(); it++)
  //   {
  //     BitInfo* bi  = it->second;
  //     delete(bi);
  //   }

  for (std::set<Signal*>::iterator it = m_signal_set.begin();
       it != m_signal_set.end(); it++)
    {
      Signal* signal = (*it);
      signal->disable();
    }

  m_bit_map.clear();
  m_frame_set.clear();
  m_signal_set.clear();
  m_term_set.clear();
  m_code_active = 0;

  return 0;

}

unsigned int DataHandler::getInUseFrameCount() 
{
  return m_frame_set.size();
}

unsigned int DataHandler::updateMode()
{
  #if defined(RDBACKCONTROL)
  assert(m_control != NULL);
  #endif

  bool good = true;
  unsigned int store_count = 0;
#if defined(RDBACKCONTROL)
  if (false) { fprintf(stderr, "SENDING CLEAR\n"); }
  good = good && m_control->sendRdBackClear();
#endif

  unsigned int addr_prev = 0;
  unsigned int slr_prev  = 12345;

  for (tBitMap::iterator it = m_bit_map.begin();
       it != m_bit_map.end(); it++)
    {
      BitInfo* bi = it->second;
      Bit*     b  = bi->getBit();

      if (bi->isSent()) {
	m_config_store.insert(b);
      } else {
	if (b->getKind() == Bit::CNST) {
	  m_config_store.insertConst(b);
	}
	if (b->getKind() == Bit::UNREAD) {
	  m_config_store.insertConst(b);
	}
	if (!bi->hasConstraints()) {
	  continue;
	}
      }

      unsigned int addr_c = utils::compressAddress(b->getAddr(), getFamily());
      unsigned int addr_o = utils::decompressAddress(addr_c, getFamily());
      if (addr_o !=  b->getAddr()) {
	if (false) { fprintf(stderr, " utils::compressAddress: Something is messed up\n"); }
	exit(1);
      }

      unsigned int offset_c = utils::compressOffset(b->getOffset(), getFamily());
      unsigned int offset_o = utils::decompressOffset(offset_c, getFamily());
      if (offset_o !=  b->getOffset()) {
	if (false) { fprintf(stderr, " utils::compressOffset: Something is messed up %x %x %x\n", b->getOffset(), offset_c, offset_o); }
	exit(1);
      }

      if (addr_c != addr_prev) {
	addr_prev = addr_c;
	unsigned int slr = b->getSlr();
	if (slr != slr_prev) {
	  slr_prev = slr;
	  std::bitset<ADDR_C_SZ+2>  tmp0   = std::bitset<ADDR_C_SZ+2>(slr);
	  tmp0[ADDR_C_SZ]   = 0; // its an slr
	  tmp0[ADDR_C_SZ+1] = 1; // its an slr
#if defined(RDBACKCONTROL)
	  if (false) { fprintf(stderr, "NEW SLR: %d\n", slr); }
	  good = good && m_control->sendRdBackStore(tmp0.to_ulong());
	  store_count++;
#endif
	}
	  //	std::bitset<3>  xx   = std::bitset<3>(slr);
	std::bitset<ADDR_C_SZ+2>  tmp0   = std::bitset<ADDR_C_SZ+2>(addr_c);
	tmp0[ADDR_C_SZ]   = 1; // its an address
	tmp0[ADDR_C_SZ+1] = 0; // its an address
#if defined(RDBACKCONTROL)
	if (false) { fprintf(stderr, "SENDING ADDR: %x %x\n", addr_c, b->getAddr()); }
        good = good && m_control->sendRdBackStore(tmp0.to_ulong());
	store_count++;
#endif

      }
#if defined(RDBACKCONTROL)
      if (bi->hasConstraints()) {
	std::bitset<ADDR_C_SZ+2>  tmp0   = std::bitset<ADDR_C_SZ+2>(0);
	tmp0[ADDR_C_SZ-1] = 1;  // not an offset
	tmp0[ADDR_C_SZ]   = 0; // not an address
	tmp0[ADDR_C_SZ+1] = 0; // not an address
	if (bi->isSent()) {
	  tmp0[8]  = 1; 
	}
	tConstraintSet s = bi->getConstraints();
	
	for (unsigned int i = 0; i <= 7; i++)
	  {
	    tmp0[i] = s[i];
	  }

	// std::set<BitRef*> * bitrefs = b->getBitRefs();
	// for (std::set<BitRef*>::iterator it = bitrefs->begin();
	//      it != bitrefs->end(); it++)
	//   {
	//     BitRef* br = (*it);
	//     fprintf(stderr, "INDEX: %d\n", br->getIndex());
	//   }
	       
	if (false) { fprintf(stderr, "HAS CONSTRAINTS %s\n", bitsetToString(tmp0).c_str()); }
	good = good && m_control->sendRdBackStore(tmp0.to_ulong());
	store_count++;
	if (false) { fprintf(stderr, "SENDING OFFSET: %d %d \n", offset_c, b->getOffset()); }
	good = good && m_control->sendRdBackStore(offset_c);
	store_count++;
      } else {
	if (false) { fprintf(stderr, "NO  CONSTRAINTS\n"); }
	if (false) { fprintf(stderr, "SENDING OFFSET: %d %d \n", offset_c, b->getOffset()); }
	good = good && m_control->sendRdBackStore(offset_c);
	store_count++;
      }

#endif
   }

#if defined(RDBACKCONTROL)

  if (false) { fprintf(stderr, "SENDING BREAK CODE %4x\n", m_code_active); }
  good = good && m_control->sendRdBackBreakCode(m_code_active);

  if (false) { fprintf(stderr, "SENDING END (%d)\n", m_config_store.configIn()); }
  good = good && m_control->sendRdBackFinish(m_config_store.configIn());
  m_config_store.push();
  // if (current->size() != 0) {
  //   fprintf(stderr, "SENDING CONFIG (%d)\n", m_config); }
  // }
#endif

  fprintf(stderr, "%d commands stored.\n", store_count);
  fflush(stderr);

  if (good) {
    return 0;
  }

  return 1;
}

#if defined(RDBACKCONTROL)

// unsigned int addVCDChanges (uint64_t time, std::set<Net*> * nets,  VCDWriter* vcdwriter, bool all_z)
// {

//   bool first_change = true;
//   bool verbose = false;
//   for (std::set<Net*>::iterator it = nets->begin();
//        it != nets->end(); it++)
//     {
//       Net* net = (*it);
//       if (net->isChanged() && !net->isHidden()) {
// 	if (first_change && verbose) {
// 	  fprintf(stderr, "VCD TIME: %ld\n",  time);
// 	}
// 	first_change = false;
// 	net->setChanged(false);
// 	// std::cerr << "TO VCD: " << time << " " << net->getVCDStr(all_z) << std::endl;
// 	vcdwriter->addChangeReadBack(time, net->getVcdId(), net->getVCDStr(all_z));
// 	if (verbose) {
// 	  std::string symbol = utils::createVcdId(net->getVcdId());
// 	}
//       }
//     }
//   return 0;
// }

unsigned int addVCDChanges (uint64_t time, tBitSet * bits,  DataHandler* handler, bool all_z, uint64_t incr)

{
  RdBack::VCDWriter* vcdwriter = handler->m_vcdwriter;
  uint64_t   t_mod     = handler->getVCDTime(time, incr);
  for (tBitSet::iterator it = bits->begin();
       it != bits->end(); it++)
    {
      Bit* b = (*it);
      if (all_z && b->getValueStr()[0] != 'z') {
	b->setChanged(true);
	b->setZ();
      }
      if (b->isChanged()) {
	vcdwriter->addChangeReadBack(t_mod, b->getId(), b->getValueStr());
	b->setChanged(false);
      }
    }
  return 0;
}

unsigned int addVCDChanges (uint64_t time, tBitList * bits,  DataHandler* handler, bool all_z, uint64_t incr)
{
  RdBack::VCDWriter* vcdwriter = handler->m_vcdwriter;
  uint64_t   t_mod     = handler->getVCDTime(time, incr);
  for (tBitList::iterator it = bits->begin();
       it != bits->end(); it++)
    {
      Bit* b = (*it);
      if (all_z) {
	b->setZ();
      }
      vcdwriter->addChangeReadBack(t_mod, b->getId(), b->getValueStr());
      b->setChanged(false);
    }
  return 0;
}

/// callback for readback data processing
static unsigned int rdbackCallback(void * p, uint32_t data)
{

  DataHandler* handler = (DataHandler*)p;
  bool verbose  = handler->dbgMode();
  bool verbose2 = handler->dbgMode();
  bool verbose3 = handler->dbgMode();
  unsigned int return_value = 0;

  bool is_hdr = (data >> 30) & 1;

  if (verbose2) {
      handler->m_dbg_stream << std::endl << "UNIT MODE: " << handler->m_unit_mode << std::endl;
      handler->m_dbg_stream << std::endl << "     DATA: " << std::hex << data << std::endl;
      handler->m_dbg_stream << std::endl << "   IS_HDR: " << std::hex << is_hdr << std::endl;
  }
  
  if (is_hdr) {
    unsigned int val = (data << 2) >> 2; // 30 bits
    handler->m_header_words.push(val);
  } else {

  switch (handler->m_header_words.size()) {
  case 3: { // CONFIG
    unsigned int config = handler->m_header_words.front();
    handler->m_header_words.pop();

    if (handler->m_sync_config) {
      unsigned int most_recent = handler->m_config_store.configIn() - 1;
      if (verbose2) {
	handler->m_dbg_stream << std::endl << "   MOST RECENT SW CONFIG: " <<  most_recent << std::endl;
	handler->m_dbg_stream << std::endl << "  SYNCING WITH HW CONFIG: " << config << std::endl;
      }

      if (most_recent != config) {

	handler->m_config_store.popTo(most_recent);
	tBitSet*  current       = handler->m_config_store.current();
	tBitList* current_const = handler->m_config_store.currentConst();
	handler->m_config_store.InitConfigIn();
	while (handler->m_config_store.configIn() != config) {
	  handler->m_config_store.push();
	}
	for (tBitSet::iterator it = current->begin();
	     it != current->end(); it++)
	  {
	    Bit* b = (*it);
	    handler->m_config_store.insert(b);
	  }
	for (tBitList::iterator it = current_const->begin();
	     it != current_const->end(); it++)
	  {
	    Bit* b = (*it);
	    handler->m_config_store.insertConst(b);
	  }
	handler->m_config_store.push();
      }
      
      handler->m_sync_config = false;
    }

    handler->m_config_new = true;
    handler->m_config_store.popTo(config);
  }

  case 2: { // MSB
    unsigned int msb = handler->m_header_words.front();
    handler->m_header_words.pop();

    handler->m_time  = (uint64_t)msb;
    handler->m_time = handler->m_time << 30;

    if (verbose2) {
      handler->m_dbg_stream << std::endl << "   MSB: " << std::hex << msb << std::endl;
    }
  }

  case 1: { // LSB
    unsigned int lsb = handler->m_header_words.front();
    handler->m_header_words.pop();

    handler->m_time = (handler->m_time >> 30) << 30;
    handler->m_time = handler->m_time | (uint64_t)lsb;

    if (verbose2) {
      handler->m_dbg_stream << std::endl << "    LSB: (" << std::dec << lsb << ") " << std::hex << lsb << std::endl;
      //      handler->m_dbg_stream << std::endl << "    LSB: " << std::hex << lsb << std::endl;
    }
    if (verbose2) {
      handler->m_dbg_stream << std::endl << "   TIME: " << std::hex << handler->m_time << std::endl;
    }

    bool time_change   = handler->m_vcd_prev != handler->m_time;
    bool gap           = time_change && ((handler->m_vcd_prev + 1) != handler->m_time);
    bool config_change = handler->m_config_new;
    handler->m_config_new = false;
      
    if (time_change) {
      handler->m_change_prev_prev    = handler->m_change_prev;
      handler->m_change_prev         = handler->m_change;
      handler->m_change              = config_change;
    } else {
      handler->m_change  = handler->m_change || config_change;
    }

    if (verbose3) {
      handler->m_dbg_stream << "TIME: " << handler->m_time << std::endl;
      handler->m_dbg_stream << "GAP: " << gap << " TIME CHANGE: " << time_change << std::endl;
      handler->m_dbg_stream << "CHANGE: " << handler->m_change << " " << handler->m_change_prev << " " <<  handler->m_change_prev_prev << std::endl;
    }

    if (config_change) {
      if (verbose2 || verbose3) {
	handler->m_dbg_stream << std::endl << "CONFIG: " << std::hex << handler->m_config_store.configOut() << std::endl;
      }
      //      handler->m_bit_set_current       = &handler->m_config_store.current();
      //      handler->m_bit_set_current_const = &handler->m_config_store.currentConst();
      handler->m_bit_it_current = handler->m_config_store.current()->begin();
    }

    if (verbose3) {
      handler->m_dbg_stream << "SENT SIZE: " << handler->m_sent_bits.size() << " " << handler->m_sent_bits_prev.size() << std::endl;
    }
      
    if (time_change && handler->m_change_prev_prev) {
      // a new time .... deal with nets no longer being captured

      tBitList  s_diff;
      tBitSet   current;
      for (tBitList::iterator it = handler->m_sent_bits.begin();
	   it != handler->m_sent_bits.end(); it++)
	{
	  Bit* b = (*it);
	  current.insert(b);
	}

      for (tBitList::iterator it = handler->m_sent_bits_prev.begin();
	   it != handler->m_sent_bits_prev.end(); it++)
	{
	  Bit* b = (*it);

	  if (current.find(b) == current.end()) {
	    s_diff.push_front(b);
	  }
	}

      addVCDChanges (handler->m_vcd_prev_prev + 1, &s_diff, handler, true, 0);
    }
	
    if (gap) {
      addVCDChanges (handler->m_vcd_prev + 1, &handler->m_sent_bits, handler, true, 0);
      addVCDChanges (handler->m_vcd_prev + 1, handler->m_config_store.currentConst(), handler, true, 0);
    }

    if (time_change) {
      handler->m_sent_bits_prev = handler->m_sent_bits;
      if (handler->m_change_prev) {
	handler->m_sent_bits.clear();
      }
      handler->m_vcd_prev_prev = handler->m_vcd_prev;
      handler->m_vcd_prev      = handler->m_time;
    }

    if (verbose2) {
      handler->m_dbg_stream << std::endl << "GG  TIME: " << std::hex << handler->m_time << std::endl;
    }
    handler->m_remaining =  handler->m_config_store.current()->size();
    if (verbose2) {
      handler->m_dbg_stream << std::endl << "GG  COUNT: " << handler->m_remaining << std::endl;
    }
  }

  case 0:
    break;

  default:
    fprintf(stderr, "ERROR: too many header words: %ld\n", handler->m_header_words.size());
    exit(1);
  }

  //std::cout << "DATA: " << std::hex << data << std::endl;
  
    if (verbose) {
      handler->m_dbg_stream << std::endl << "GG  DATA: " << std::hex << data << std::endl;
    }
    std::bitset<32> dd  = std::bitset<32>(data);
    unsigned int max = 30;
    if (handler->m_remaining < 30) {
      max = handler->m_remaining;
    } else {
      handler->m_remaining = handler->m_remaining - 30;
    }
    for (unsigned int j = 0; j < max; j++) {
      unsigned int i = max - j - 1;
      Bit* b = *(handler->m_bit_it_current);
      bool prev = b->getValue();
      bool current = false;
      if (dd[i] == 0) { // dd value is inverted
	current = true;
      }
      if (handler->m_change_prev) {
	handler->m_sent_bits.push_front(b);
      }
      if (verbose) {
	handler->m_dbg_stream << "[" << i << "] ADDR: " << std::hex << b->getAddr() << " OFFSET: " <<  b->getOffset() << "(" << current << ")" << std::endl;
      }

      //std::cout << "BIT: " << prev << ", " << current << ", " << b->getValueStr() << std::endl;

      if (prev != current) { 
	if (verbose) {
	  handler->m_dbg_stream << "CHANGE FROM " << prev << " to " << current << std::endl;
	}
	b->setValue(current);
	b->setChanged(true);
	handler->m_changed_bits.push_front(b);
	//std::cout << "  prev != current" << std::endl;
      } else if (b->getValueStr()[0] == 'z') {
	b->setValue(current);
	b->setChanged(true);
	handler->m_changed_bits.push_front(b);
	//std::cout << "   prev == z" << std::endl;
      }
      handler->m_bit_it_current++;
      if (handler->m_bit_it_current == handler->m_config_store.current()->end()) {
	if (verbose) {
	  handler->m_dbg_stream << "[" << i << "] ADDR RESTART!" << std::endl;
	}
	handler->m_bit_it_current = handler->m_config_store.current()->begin();

	for (tBitList::iterator it = handler->m_config_store.currentConst()->begin();
	     it != handler->m_config_store.currentConst()->end(); it++)
	  {
	    Bit* b = (*it);
	    if (handler->m_change_prev) {
	      handler->m_sent_bits.push_front(b);
	    }
	    const char* v = b->getValueStr();
	    if (v[0] == 'z') {
	      b->setValue(b->getValue());
	      handler->m_changed_bits.push_front(b);
	    }
	  }

	addVCDChanges (handler->m_time, &handler->m_changed_bits, handler, false, 5);
	if (handler->m_unit_mode) {
	  addVCDChanges (handler->m_time + 1, &handler->m_changed_bits, handler, true, 5);
	}
	handler->m_changed_bits.clear();
	return_value = 1;
	break;
      }
    }
  }

  handler->m_dbg_stream << "EXIT " << return_value << std::endl;
  return return_value;
}

#endif

unsigned int DataHandler::setControl(RdBackControl * control)
{
  assert(control != NULL);
  m_control = control;
#if defined(RDBACKCONTROL)
  m_control->registerRdbackCallback(rdbackCallback, (void*)this);
#endif
  return 0;
}

unsigned int DataHandler::setVCDWriter(RdBack::VCDWriter * vcdwriter)
{
  assert(vcdwriter != NULL);
  m_vcdwriter = vcdwriter;
  return 0;
}

unsigned int DataHandler::flushVCD()
{

  assert(m_vcdwriter != NULL);
  uint64_t t_mod = 0;
  t_mod = getVCDTime(m_vcd_prev_prev);
  // if (m_change_prev_prev) {
  //   t_mod = getVCDTime(m_vcd_prev_prev);
  // } else {
  //   t_mod = getVCDTime(m_vcd_prev);
  // }
  if (m_unit_mode) {
    t_mod = getVCDTime(m_vcd_prev);
  }
  m_vcdwriter->updateLatestTime(t_mod, true);
  m_vcdwriter->flushVCDFile();
  return 0;

}

unsigned int DataHandler::doReset()
{

  assert(m_vcdwriter != NULL);
  InitAll();
  m_vcdwriter->reset("dump.backup");
  return 0;

}

RdBack::VCDWriter* DataHandler::getVCDWriter()
{
  assert(m_vcdwriter != NULL);
  return m_vcdwriter;
}

bool DataHandler::dbgMode() {
  return m_dbg_mode;
}
