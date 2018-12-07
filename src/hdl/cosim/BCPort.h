#pragma once

#include <vector>
using namespace std;

class VHandle;


class BCPort {
private:
  VHandle m_port;
  VHandle m_net;
  unsigned int m_idx;

public:
  BCPort ( VHandle port, VHandle net)
    : m_port(port), m_net(net)
    , m_idx(0)
  {};

  void setIdx (unsigned int i)
  {
    m_idx = i;
  }
  unsigned int getIdx () const {
    return m_idx;
  };

  PLI_BYTE8 *getName ()  const {
    return m_port.getStr (vpiName);
  };

  PLI_INT32 getSize () const {
    return m_port.getProp (vpiSize);
  }
  PLI_INT32 getDir () const {
    return m_port.getProp (vpiDirection);
  }
  // A valid port's net handle is not null
  bool isValid () const {
    return (NULL != m_port.get());
  }
  const char * getDirection() const ;
  bool setValue (char val[]);

  static BCPort nullPort ;
};

