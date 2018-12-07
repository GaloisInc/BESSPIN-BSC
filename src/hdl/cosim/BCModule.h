#pragma once

#include <vector>
using namespace std;
#include "BCPort.h"

// forward reference?
class VHandle;
class BCPort;

class BCModule {
private:
  VHandle  m_module;
  unsigned int   m_idx;
  vector<BCPort> m_ports ;

public:
  BCModule(VHandle mh)
    :m_module(mh)
    ,m_idx(0)
  {};

  char * getName() const {return m_module.getStr(vpiName); }

  VHandle &    getModule();
  BCPort &     addPort (VHandle &port, VHandle &net) ;
  unsigned int getIdx () const ;
  void         setIdx (unsigned int i) ;

  BCPort & getPort (unsigned int idx) ;
  BCPort & lookupPort (const char *pn);

public:
  static BCModule nullModule;
};
