#pragma once

#include <string>
#include <set>
#include <bitset>
#include <deque>
#include <queue>
#include <fstream>
#include <stdint.h>
#include <stdlib.h>

#include "Module.hpp"
#include "Utils.hpp"
#include "RdBackControl.hpp"
#include "RdBackVCDWriter.hpp"

typedef std::bitset<8> tConstraintSet;

class BitInfo {
private:
  Bit*           m_bit;
  tConstraintSet m_values;
  bool           m_send;
  bool           m_constraint;

public:

  BitInfo(Bit* bit);

  ~BitInfo();

  unsigned int addConstraint(char c, unsigned int track);

  Bit* getBit() {
    return m_bit;
  }
  bool hasConstraints() {
    return m_constraint;
  }
  bool isSent() {
    return m_send;
  }
  unsigned int setAsSent() {
    m_send = true;
    return 0;
  }

  tConstraintSet & getConstraints() {
    return m_values;
  }
  
  unsigned int merge(BitInfo* bit_info);

};

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

class CompareBitFrameAddr {
 public:
  bool operator() (const Bit *l, const Bit *r) const {
    if (l->getAddr() == 0 && r->getAddr() == 0) {
      return (l->getId() < r->getId());
    } else if (l->getSlr() == r->getSlr()) {
      if (l->getAddr() == r->getAddr()) {
	return (l->getOffset() < r->getOffset());
      } else {
	return (l->getAddr() < r->getAddr());
      }
    } else {
      return (l->getSlr() < r->getSlr());
    }
  }
};

typedef std::set<Bit*,  CompareBitFrameAddr>          tBitSet;
typedef std::list<Bit*>                               tBitList;
typedef std::map<Bit*, BitInfo*, CompareBitFrameAddr> tBitMap;

class CompareBitInfoFrameAddr {
 public:
  bool operator() (BitInfo *ll, BitInfo *rr) {
    Bit* l = ll->getBit();
    Bit* r = rr->getBit();
    if (l->getAddr() == 0 && r->getAddr() == 0) {
      return (l->getId() < r->getId());
    } else if (l->getAddr() == r->getAddr()) {
      return (l->getOffset() < r->getOffset());
    } else {
      return (l->getAddr() < r->getAddr());
    }
  }
};

typedef std::set<BitInfo*,  CompareBitInfoFrameAddr> tBitInfoSet;

class BitTerm {
private:
  unsigned int        m_id;
  Signal*             m_signal;
  tBitInfoSet         m_bitinfo_set;
public:
  BitTerm(unsigned int id);
  BitTerm(unsigned int id, Signal* signal);
  ~BitTerm();
  unsigned int insert (BitInfo* bit_info) {
    m_bitinfo_set.insert(bit_info);
    return 0;
  }
  tBitInfoSet & getBitInfoSet() {
    return m_bitinfo_set;
  }
  Signal* getSignal() {
    return m_signal;
  }
  unsigned int getId() {
    return m_id;
  }
};

class ConfigStore {
private:
  unsigned int m_config_in;
  unsigned int m_config_out;
  tBitSet*     m_current;
  tBitList*    m_current_const;

  std::deque<unsigned int> m_deque_config;
  std::deque<tBitSet*>     m_deque_current;
  std::deque<tBitList*>    m_deque_current_const;
public:
  ConfigStore();
  ~ ConfigStore() { }

  void InitAll();
  void InitConfigIn();
  unsigned int insert (Bit* b);
  unsigned int insertConst (Bit* b);
  unsigned int configIn() {
    return m_config_in;
  }
  unsigned int configOut() {
    if (m_deque_config.empty()) {
      fprintf(stderr, "ERROR: configuration store is empty.\n");
      exit(1);
    }
    return m_deque_config.front();
  }
  unsigned int push();
  unsigned int popTo(unsigned int config);
  unsigned int popAll();
  tBitSet*  current() {
    if (m_deque_current.empty()) {
      fprintf(stderr, "ERROR: configuration store is empty.\n");
      exit(1);
    }
    return m_deque_current.front();
  }
  tBitList* currentConst() {
    if (m_deque_current_const.empty()) {
      fprintf(stderr, "ERROR: configuration store is empty.\n");
      exit(1);
    }
    return m_deque_current_const.front();
  }
};

class UnitTime {
private:
  uint64_t m_time_prev;
  uint64_t m_time_unit;

public:
  UnitTime() {
    InitAll();
  }
  ~ UnitTime() { }
  void InitAll() {
    m_time_prev = 0;
    m_time_unit = 0;
  }
  uint64_t getUnitTime(uint64_t time_actual, uint64_t incr) {
    if (time_actual == 0) {
      return 0;
    }
    if (time_actual == m_time_prev) {
      return m_time_unit;
    }
    if (incr != 0) {
      m_time_prev = time_actual;
      m_time_unit = m_time_unit + incr;
    }
    return m_time_unit;
  }
};

class DataHandler {
private:
  std::map<unsigned int, BitTerm*> m_id_map;  // map from id number to BitTerm (break constraints)
  std::map<Signal*, BitTerm*>         m_signal_map; // map from Signal to BitTerm (signals)

  tBitMap                          m_bit_map;
  std::set<unsigned int>           m_frame_set;
  std::set<unsigned int>           m_term_set;
  unsigned int                     m_code;
  unsigned int                     m_code_active;

  RdBackControl *                  m_control;
  unsigned int                     m_next_term;
  UnitTime                         m_unit;

public:
  bool                             m_unit_mode;
  uint64_t                         m_time;

  // temp space for receiving the variable length header
  std::queue<unsigned int>         m_header_words;

  // std::map<unsigned int, tBitSet>  m_bit_sets;
  // std::map<unsigned int, tBitList> m_bit_sets_const;
  // tBitSet*                         m_bit_set_current;
  // tBitList*                        m_bit_set_current_const;

  bool                             m_config_new;
  ConfigStore                      m_config_store;

  tBitSet::iterator                m_bit_it_current;
  unsigned int                     m_remaining;
  std::set<Signal*>                m_signal_set;

  RdBack::VCDWriter *              m_vcdwriter;
  uint64_t                         m_vcd_prev;
  uint64_t                         m_vcd_prev_prev;
  tBitList                         m_sent_bits;
  tBitList                         m_sent_bits_prev;
  tBitList                         m_changed_bits;
  bool                             m_config_changed;

  bool                             m_change;
  bool                             m_change_prev;
  bool                             m_change_prev_prev;
  Family                           m_family;
  bool                             m_dbg_mode;

public:

  bool                             m_sync_config;
  std::ofstream                    m_dbg_stream;

  DataHandler();
  ~ DataHandler() { }

  void InitAll();
  unsigned int clear();
  unsigned int clearActive();
  unsigned int updateActive();
  unsigned int addSignal(Signal* signal);
  unsigned int removeSignal(Signal* signal);
  //  unsigned int addTerm(Net* net, unsigned int value, unsigned int mask_value, unsigned int track);
  //  unsigned int addTerm(Net* net, unsigned int value, unsigned int lsb, unsigned int msb, unsigned int track);
  unsigned int addTerm(Signal* signal, unsigned int value, std::string & mask_str, unsigned int track);
  unsigned int addTerm(Signal* signal, std::string* value_str, unsigned int track);
  unsigned int addTerm(BitTerm* bit_term);
  unsigned int removeTerm(unsigned int id);
  unsigned int addBreakCode(unsigned int code);
  unsigned int refreshHW();

  unsigned int absorbTerm(BitTerm* bit_term);
  unsigned int getInUseFrameCount();

  unsigned int setControl(RdBackControl * control);
  unsigned int setVCDWriter(RdBack::VCDWriter * vcdwriter);
  unsigned int flushVCD();
  unsigned int doReset();

  unsigned int setFamily(Family family)
  {
    m_family = family;
    return 0;
  }
  Family getFamily()
  {
    return m_family ;
  }

  RdBack::VCDWriter* getVCDWriter();
  bool         dbgMode();
  uint64_t     getVCDTime(uint64_t time_actual) {
    return getVCDTime(time_actual, 0);
  }
  uint64_t     getVCDTime(uint64_t time_actual, uint64_t incr) {
    if (m_unit_mode) {
      return m_unit.getUnitTime(time_actual, incr);
    }
    return time_actual;
  }
  unsigned int setUnitTime() {
    m_unit_mode = true;
    return 0;
  }

  unsigned int syncConfig() {
    m_sync_config = true;
    return 0;
  }

private:
  unsigned int updateMode();

};

