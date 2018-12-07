#pragma once

#include <string>
#include <set>
#include <vector>
#include <map>
#include <iostream>
#include <fstream>
#include <string.h>
#include <stdint.h>

#include "RdBackVCDWriter.hpp"

class Module;
class Signal;
class Bit;
class BitRef;
class Net;

enum opKind {
  opBUF, 
  opEQ, 
  opGND, 
  opINV, 
  opNEQ, 
  opPRIM,
  opPWR, 
  opREG,
  opREG_WIDE,
  opUNKNOWN,
  opXOR
};

enum signalKind {
  dIn,
  dOut,
  dWire
};

class CompareModules {
public:
  bool operator() (const Module* l, const Module* r) const;
};

class CompareSignals {
public:
  bool operator() (const Signal* l, const Signal* r) const;
};

typedef std::set<Module*, CompareModules> tModuleSet;
typedef std::set<Signal*, CompareSignals> tSignalSet;
typedef std::set<Net*>                    tNetSet;


class Module {
private:
  std::string         m_name;
  std::string         m_defname;
  std::string         m_path;
  unsigned int        m_id;
  tSignalSet          m_signals;
  tSignalSet          m_signals_visible;
  tModuleSet          m_children;
  tModuleSet          m_children_visible;
  std::map<unsigned   int, Module*> m_child_map;
  std::map<unsigned   int, Signal*> m_signal_map;
  bool                m_hidden;
  opKind              m_kind;
  static unsigned int m_depth; 
  uint64_t            m_init;
  bool                m_dinverted;
  bool                m_rstinverted;
  Module*             m_parent;

public:
  static bool         m_show_hidden;  
  // Constructors
  explicit Module(char* name, char* defname, unsigned int id);
  // Destructor
  ~Module();

  unsigned int addChild(Module* child);
  unsigned int addSignal(Signal* signal);
  unsigned int getId();
  const char * getName();
  const char * getDefName();
  unsigned int setDefName(const char* path);
  const char * getPath();
  unsigned int setPath(const char* path);
  bool         isHidden();
  unsigned int setHidden();
  opKind       getKind();
  unsigned int setKind(opKind kind);
  tSignalSet   & getSignals(bool exclude_hidden=false);
  tModuleSet   & getChildren(bool exclude_hidden=false);
  unsigned int setParent(Module* parent);
  Module*      getParent();
  Module*      getChild(unsigned int num, bool no_error=false);
  Signal*      getSignal(unsigned int num, bool no_error=false);
  unsigned int addToVCD(RdBack::VCDWriter* vcdwriter);
  unsigned int updateVisibleChildren(bool recursive=false);
  unsigned int updateVisibleSignals(bool recursive=false);

  static unsigned int getDepth();
  static unsigned int incrDepth();
  static unsigned int decrDepth();

  unsigned int setInit(uint64_t init);
  uint64_t getInit();

  unsigned int setDInverted(bool inverted);
  bool         getDInverted();

  unsigned int setRSTInverted(bool inverted);
  bool         getRSTInverted();

};

class Net {
private:
  tSignalSet          m_signals;
  bool                m_enabled;
  unsigned int        m_id;
  static unsigned int m_next_id; 

public:
  static std::set<Net*> m_all_nets;
  // Constructors
  explicit Net();
  // Destructor
  ~Net();

  tSignalSet  &   getSignals();
  unsigned int addSignal(Signal * signal);
  unsigned int getId();
  bool         isEnabled();
  unsigned int enable();
  unsigned int disable();

};

class Signal {
private:
  signalKind         m_kind;
  std::string        m_name;
  std::string        m_fullname;
  unsigned int       m_id;
  Net *              m_net;
  Module *           m_module;
  bool               m_hidden;
  bool               m_enabled;
  bool               m_is_src_state;
  unsigned int         m_lsb;
  unsigned int         m_msb;

  std::vector<BitRef*> m_bits;

public:
  static std::set<Signal*> m_all_signals;
  // Constructors
  explicit Signal(Module* module, char* name, unsigned int id, unsigned int lsb, unsigned int msb, bool no_bits=false);
  // Destructor
  ~Signal();

  unsigned int getId();
  const char * getName();
  const char * getFullName();
  Net *        getNet();
  unsigned int setNet(Net* net);
  unsigned int addState(unsigned int index, unsigned int addr, unsigned int offset);
  unsigned int addState(unsigned int index, unsigned int addr, unsigned int offset, unsigned int slr);
  BitRef*      getBitRef(unsigned int index);
  unsigned int setBitRef(unsigned int index, BitRef* br);
  BitRef*      getBitRef(unsigned int index, bool just_warn);
  signalKind   getKind();
  unsigned int setKind(signalKind kind);
  Module*      getModule() 
  {
    return m_module;
  }

  unsigned int    getLsb();
  unsigned int    getMsb();
  unsigned int    getWidth();
  unsigned int    getAvail();
  unsigned int    getUnread();
  unsigned int    getInferred();

  unsigned int isHidden ()
  {
    return m_hidden;
  }

  unsigned int setHidden()
  {
    m_hidden = true;
    return 0;
  }
  unsigned int merge(Signal* signal, unsigned int offset);
  unsigned int merge(Signal* signal, unsigned int index, unsigned int signal_index);

  bool         isEnabled();
  unsigned int enable();
  unsigned int disable();
  unsigned int expand(unsigned int lsb, unsigned int msb);
  bool         allZeros();
  bool         allOnes();
  std::string  getSignature();

  unsigned int addToVCD(RdBack::VCDWriter* vcdwriter);
  unsigned int setSrcState() ;
  bool         isSrcState() {
    return m_is_src_state;
  }
  std::string getValueStr(bool isStatic);
};

class BitRef {
private:
  unsigned int       m_index;
  Signal *           m_signal;
  Bit *              m_bit;

public:
  // Constructors
  explicit BitRef(Signal* signal, unsigned int index);
  // Destructor
  ~BitRef();

  Signal *     getSignal();
  unsigned int setSignal(Signal* signal);
  Bit *        getBit();
  unsigned int setBit(Bit* bit);
  unsigned int getIndex();
  unsigned int merge(BitRef* br);
};


class Bit {
public:
  enum BitKind {STATE, UNAVAILABLE, CNST, UNREAD};

  BitKind             m_kind;
  std::set<BitRef*>   m_bitrefs;
  bool                m_value;
  std::string         m_str;
  bool                m_is_src_state;
  unsigned int        m_addr;
  unsigned int        m_offset;
  unsigned int        m_id;
  bool                m_changed;
  static unsigned int m_next_id; 
  static std::set<Bit*> m_all_bits;
  static unsigned int m_max_id;

  unsigned int        m_slr;
  bool                m_inferred;

public:
  // Constructors
  explicit Bit();
  explicit Bit(unsigned int id);
  // Destructor
  //  ~Bit();

  BitKind getKind();
  bool         getValue();
  unsigned int setKind(BitKind kind);
  unsigned int setValue(bool value);
  unsigned int setZ()
  {
    m_str = "z";
    return 0;
  }
  unsigned int setAddr(unsigned int addr)
  {
    m_addr = addr;
    return 0;
  }
  bool         isChanged();
  unsigned int setChanged(bool value);
  unsigned int setOffset(unsigned int offset)
  {
    m_offset = offset;
    return 0;
  }
  unsigned int getAddr() const
  {
    return m_addr;
  }
  unsigned int getOffset() const
  {
    return m_offset;
  }
  unsigned int remove();
  const char*  getValueStr(bool isStatic = false);
  unsigned int addBitRef(BitRef* bitref);
  std::set<BitRef*> * getBitRefs() {
    return &m_bitrefs;
  }
  unsigned int merge(Bit* bit);
  unsigned int getId() const
  {
    return m_id;
  }
  unsigned int setId(unsigned int id) {
    m_id = id;
    return 0;
  }
  static std::set<Bit*> & getAllBits();

  unsigned int setSrcState() {
    m_is_src_state = true;
    return 0;
  }

  bool         isSrcState() {
    return m_is_src_state;
  }

  unsigned int setSlr(unsigned int slr)
  {
    m_slr = slr;
    return 0;
  }
  unsigned int getSlr() const
  {
    return m_slr;
  }

  unsigned int setInferred(bool value)
  {
    m_inferred = value;
    return 0;
  }
  bool getInferred() const
  {
    return m_inferred;
  }

  static unsigned int setMaxBitId(unsigned int id);
  static unsigned int getMaxBitId();

};

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

std::ostream& addIndent(std::ostream& ost);

std::ostream& operator<<(std::ostream& ost, Bit::BitKind& bk);
std::ostream& operator<<(std::ostream& ost, signalKind& sk);

std::ostream& operator<<(std::ostream& ost, Bit* b);

std::ostream& operator<<(std::ostream& ost, Signal* signal);

std::ostream& operator<<(std::ostream& ost, Module* mod);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

