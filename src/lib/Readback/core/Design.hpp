#pragma once

#include "Module.hpp"
#include "Utils.hpp"
#include <string>
#include <map>
#include <set>
#include <vector>

#include "RdBackControl.hpp"
#include "DataHandler.hpp"
#include "RdBackVCDWriter.hpp"
#include "Netlister.hpp"
#include "Export.hpp"

class Design {
private:
  Module *                         m_root;
  Module *                         m_top;
  //  std::string                      m_topname;
  bool                             m_has_top;
  std::map<unsigned int, Module *> m_module_map;
  std::map<std::string, Module *>  m_module_name_map;
  std::map<unsigned int, Net    *> m_net_map;
  std::map<std::string,  Signal *> m_signal_map;
  DataHandler *                    m_handler;
  Family                           m_family;
  Netlister*                       m_netlister;
 

public:

    Design();

    ~Design();

  /// Parse .rtl file
  unsigned int parse_rtl(std::string file);

  /// Parse synth file
  unsigned int parse_synth(std::string file);

  /// Parse .ll file
  unsigned int parse_ll(std::string file);

  /// Parse xilinx synthesis file
  unsigned int parse_log(std::string file, bool skip_vcd=false);
  unsigned int parse_log_6(std::string file, bool skip_vcd);
  unsigned int parse_log_7(std::string file, bool skip_vcd);

  Module * findModule(unsigned int id);
  Module * findModule(EdifView view, const std::string path);
  Net    * findNet(unsigned int num);
  Signal * findSignal(EdifView view, const std::string path);

  unsigned int enableSignal(Signal * signal);
  unsigned int disableSignal(Signal * signal);
  unsigned int addTerm(Signal* signal, unsigned int value, std::string & mask_str, unsigned int track);
  unsigned int addTerm(Signal* signal, std::string* value_str, unsigned int track);
  unsigned int removeTerm(unsigned int id);
  unsigned int addBreakCode(unsigned int code);
  unsigned int refreshHW();

  unsigned int getInUseBitCount();
  unsigned int getInUseFrameCount();
  Family       getFamily();

  BitRef *     findBitRef(EdifView view, std::string cell_def, std::string cell_inst, unsigned int n);

  unsigned int setControl(RdBackControl * control);

  unsigned int setVCDWriter(RdBack::VCDWriter * vcdwriter);
  unsigned int flushVCD();
  unsigned int doReset();
  unsigned int initializeBits();

  std::string getValidModulePath(EdifView view, std::string& path);

  unsigned int exportDesign(std::string file, bool include_hidden=false);
  unsigned int parse_xrf(std::string file, bool skip_vcd=false);
  unsigned int setUnitTime() {
    return m_handler->setUnitTime();
  }
  unsigned int syncConfig() {
    return m_handler->syncConfig();
  }

};

