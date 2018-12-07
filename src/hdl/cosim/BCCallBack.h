#pragma once


class BCosim;
class BCModule;
class BCPort;

class BCCallBack {
 private:
  BCosim   & m_cosim;
  unsigned int  m_midx;
  unsigned int  m_pidx;
 public:
  BCCallBack ( BCosim & bcs, unsigned int mod, unsigned int port)
    : m_cosim(bcs)
    , m_midx(mod)
    , m_pidx(port)
  {}

  BCosim * getCosim() const { return & m_cosim; }
  unsigned int getModIdx() const { return m_midx; }
  unsigned int getPortIdx() const { return m_pidx; }

};
