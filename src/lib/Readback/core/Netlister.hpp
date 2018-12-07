#pragma once

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <map>
#include <vector>
#include <deque>
#include "TokenMap.hpp"
#include "Module.hpp"
#include "Utils.hpp"

enum resultKind {ERR, MOD, SIG, BIT, LBL, NUM, EMPTY};

struct tPair {
  unsigned int lsb;
  unsigned int msb;
};

struct tResult {
  resultKind     kind;
  union {
    unsigned int name;
    unsigned int value;
  };
  unsigned int   alias;
  void*          p;
};

class Netlister  
{
public:
  std::multimap<std::string, std::string> m_rtl_def_map;
  std::multimap<std::string, std::string> m_synth_def_map;
private:
  typedef std::vector<unsigned int>           tCodeVector;
  typedef std::vector<unsigned int>::iterator tCodeIter;
  typedef std::map<unsigned int, tCodeIter>   tCellMap;
  typedef std::map<unsigned int, tCodeIter>::iterator tCellIter;
  tCodeIter    m_current;
  tCodeVector  m_codes;
  TokenMap*    m_map;
  std::map<std::string,  std::set<std::string>*> * m_ci;
  unsigned int m_libCurrent;
  tCellMap     m_cellMap;
  unsigned int m_reg_count;
  Family       m_family;
  Part         m_part;

  std::vector<Module *>            m_top;
  std::deque<Module *>             m_parents;

  std::map<std::string, std::vector<Signal*>* > m_signal_map;
  std::map<std::string, std::vector<Module*>* > m_module_map;

  std::map<unsigned int, opKind>   m_kind_map;

public:

  Netlister();

  ~Netlister();

  unsigned int parse_rtl(std::string file);
  unsigned int parse_synth(std::string file);
  unsigned int parse_edf(std::ifstream & edfFile);
  tResult      parse_number();
  tResult      parse_sexpr(EdifView view);
  tResult      skip_sexpr();
  unsigned int parse_body(EdifView view);
  unsigned int skip_body();
  unsigned int backup(unsigned int n = 1);

  unsigned int record_cell (unsigned int cell_name, unsigned int lib_name);
  unsigned int elaborate_cell (EdifView view, unsigned int cell_name, unsigned int lib_name);

  Signal*      addSignal(unsigned int num, EdifView view, Module * module, char* name, unsigned int lsb, unsigned int msb, unsigned int id, char* value);
  Signal*      addSignal(EdifView view, Module * module, char* name, unsigned int lsb, unsigned int msb, unsigned int id, char* value);
  unsigned int initializeMap();
  Module*      getTop(EdifView view);
  Signal *     findSignal(const EdifView view, const std::string path);
  unsigned int registerSignal(const EdifView view, Signal* signal); 
  Module *     findModule(const EdifView view, const std::string path);
  unsigned int registerModule(const EdifView view, Module* module); 
  unsigned int getCode(char* label) {
    return m_map->getCode(label);
  }
  tResult      createRegBit(EdifView view, unsigned int name_num);
  bool         isPrimitive(unsigned int cell_name, unsigned int lib_name, unsigned int lut_init);
  opKind       getKind(unsigned int cell_name, unsigned int lib_name, unsigned int lut_init);
  unsigned int setFamily(Family family);
  Family       getFamily();
  unsigned int setPart(Part part);
  Part         getPart();
  unsigned int postProcess(EdifView view);
  unsigned int finalPostProcess();

  unsigned int parse_xrf(std::string file);

};
