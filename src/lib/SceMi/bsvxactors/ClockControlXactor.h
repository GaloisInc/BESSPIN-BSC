// -*- c++ -*-

#ifndef __CLOCK_CONTROL_XACTOR_H__
#define __CLOCK_CONTROL_XACTOR_H__

#include <cstdio>
#include <cstdlib>
#include "OutportQueueT.h"
#include "InportProxyT.h"
#include "BSVType.h"
#include "BitT.h"

#define MIN(x, y)   ((x) < (y) ? (x) : (y))
#define MAX(x, y)   ((x) > (y) ? (x) : (y))
#define DIV_ROUND_UP(x, y)  (((x) + (y) - 1) / (y))
#define DIV_ROUND_CLOSEST(x, y) (unsigned long)(((double)(x) / (double)(y)) + 0.5)
#define CLAMP(val, min, max) ((val) < (min) ? (min) : ((val) > (max) ? (max) : (val)))
#define ABS(x)    ((x) < 0 ? -(x) : (x))

const unsigned long clkgen_reg_id            = 0x00;
const unsigned long clkgen_reg_update_enable = 0x01;
const unsigned long clkgen_reg_clkout0_1     = 0x02;
const unsigned long clkgen_reg_clkout0_2     = 0x03;
const unsigned long clkgen_reg_clkout1_1     = 0x04;
const unsigned long clkgen_reg_clkout1_2     = 0x05;
const unsigned long clkgen_reg_clkout2_1     = 0x06;
const unsigned long clkgen_reg_clkout2_2     = 0x07;
const unsigned long clkgen_reg_clkout3_1     = 0x08;
const unsigned long clkgen_reg_clkout3_2     = 0x09;
const unsigned long clkgen_reg_clkout4_1     = 0x0a;
const unsigned long clkgen_reg_clkout4_2     = 0x0b;
const unsigned long clkgen_reg_clkout5_1     = 0x0c;
const unsigned long clkgen_reg_clkout5_2     = 0x0d;
const unsigned long clkgen_reg_clkout6_1     = 0x0e;
const unsigned long clkgen_reg_clkout6_2     = 0x0f;
const unsigned long clkgen_reg_clk_div       = 0x10;
const unsigned long clkgen_reg_clk_fb_1      = 0x11;
const unsigned long clkgen_reg_clk_fb_2      = 0x12;
const unsigned long clkgen_reg_lock_1        = 0x13;
const unsigned long clkgen_reg_lock_2        = 0x14;
const unsigned long clkgen_reg_lock_3        = 0x15;
const unsigned long clkgen_reg_filter_1      = 0x16;
const unsigned long clkgen_reg_filter_2      = 0x17;
const unsigned long clkgen_reg_status        = 0x1f;

static const unsigned long clkgen_filter_table[] = {
  0x01001990, 0x01001190, 0x01009890, 0x01001890,
  0x01008890, 0x01009090, 0x01009090, 0x01009090,
  0x01009090, 0x01000890, 0x01000890, 0x01000890,
  0x08009090, 0x01001090, 0x01001090, 0x01001090,
  0x01001090, 0x01001090, 0x01001090, 0x01001090,
  0x01001090, 0x01001090, 0x01001090, 0x01008090,
  0x01008090, 0x01008090, 0x01008090, 0x01008090,
  0x01008090, 0x01008090, 0x01008090, 0x01008090,
  0x01008090, 0x01008090, 0x01008090, 0x01008090,
  0x01008090, 0x08001090, 0x08001090, 0x08001090,
  0x08001090, 0x08001090, 0x08001090, 0x08001090,
  0x08001090, 0x08001090, 0x08001090
};

static const unsigned long clkgen_lock_table[] = {
  0x060603e8, 0x060603e8, 0x080803e8, 0x0b0b03e8,
  0x0e0e03e8, 0x111103e8, 0x131303e8, 0x161603e8,
  0x191903e8, 0x1c1c03e8, 0x1f1f0384, 0x1f1f0339,
  0x1f1f02ee, 0x1f1f02bc, 0x1f1f028a, 0x1f1f0271,
  0x1f1f023f, 0x1f1f0226, 0x1f1f020d, 0x1f1f01f4,
  0x1f1f01db, 0x1f1f01c2, 0x1f1f01a9, 0x1f1f0190,
  0x1f1f0190, 0x1f1f0177, 0x1f1f015e, 0x1f1f015e,
  0x1f1f0145, 0x1f1f0145, 0x1f1f012c, 0x1f1f012c,
  0x1f1f012c, 0x1f1f0113, 0x1f1f0113, 0x1f1f0113
};

class ClockControlReq : public BSVType {
public:
  BitT<16> m_data;
  BitT<5>  m_addr;
  BitT<1>  m_rnw;

  ClockControlReq()
    : m_data()
    , m_addr()
    , m_rnw()
  {
  }

  ClockControlReq(const SceMiMessageDataInterface *msg, unsigned int &off)
    : m_data(msg, off)
    , m_addr(msg, off)
    , m_rnw(msg, off)
  {
  }

  unsigned int setMessageData(SceMiMessageDataInterface &msg, const unsigned int off=0) const 
  {
    unsigned int running = off;
    running = m_data.setMessageData(msg, running);
    running = m_addr.setMessageData(msg, running);
    running = m_rnw.setMessageData(msg, running);
    if (running != off + 22) {
      std::cerr << "Mismatch in sizes: " << std::dec << running << " vs " << (off+22) << std::endl;
    }
    return running;
  }

  friend std::ostream & operator<< (std::ostream &os, const ClockControlReq &obj) 
  {
    BSVType::PutTo * override = lookupPutToOverride(obj.getClassName());
    if (override != 0) {
      return override(os, obj);
    }

    os << "{" ;
    os << "rnw " << obj.m_rnw; os << " ";
    os << "addr " << obj.m_addr; os << " ";
    os << "data " << obj.m_data; os << "}";
    return os;
  }

  virtual std::ostream & getBitString(std::ostream &os) const
  {
    m_rnw.getBitString(os);
    m_addr.getBitString(os);
    m_data.getBitString(os);
    return os;
  }

  virtual std::ostream & getBSVType(std::ostream &os) const 
  {
    os << "SceMiXilinx::ClockControlReq";
    return os;
  }

  virtual unsigned int getBitSize () const
  {
    return 22;
  }

  virtual const char * getClassName() const 
  {
    return "ClockControlReq";
  }

  virtual BSVKind getKind() const 
  {
    return BSV_Struct;
  }

  virtual unsigned int getMemberCount() const 
  {
    return 3;
  }

  virtual BSVType * getMember (unsigned int idx)
  {
    switch(idx) {
      case 0: return &m_rnw;
      case 1: return &m_addr;
      case 2: return &m_data;
      default: std::cerr << "Index error in getMember for class ClockControlReq" << std::endl;
    }
    return 0;
  }

  virtual const char * getMemberName (unsigned int idx) const
  {
    switch(idx) {
      case 0: return "rnw";
      case 1: return "addr";
      case 2: return "data";
      default: std::cerr << "Index error in getMemberName for class ClockControlReq" << std::endl;
    }
    return 0;
  }
};

class ClockControlXactor
{
private:
  InportProxyT<ClockControlReq>       m_request;
  OutportQueueT<BitT<16> >            m_response;

private:
  unsigned long lookup_filter(unsigned long m)
  {
    if (m < 47)
      return clkgen_filter_table[m];
    return 0x08008090;
  }

  unsigned long lookup_lock(unsigned long m)
  {
    if (m < 36) 
      return clkgen_lock_table[m];
    return 0x1f1f00fa;
  }

  void calc_params(unsigned long fin, unsigned long fout, 
		   unsigned long *best_d, unsigned long *best_m,
		   unsigned long *best_dout)
  {
    const unsigned long fpfd_min = 10000;
    const unsigned long fpfd_max = 300000;
    const unsigned long fvco_min = 600000;
    const unsigned long fvco_max = 1200000;

    unsigned long d = 0;
    unsigned long d_min = 0;
    unsigned long d_max = 0;
    unsigned long _d_min = 0;
    unsigned long _d_max = 0;
    unsigned long m = 0;
    unsigned long m_min = 0;
    unsigned long m_max = 0;
    unsigned long dout = 0;
    unsigned long fvco = 0;
    long f = 0;
    long best_f = 0;

    fin /= 1000;
    fout /= 1000;

    best_f = 0x7FFFFFFF;
    *best_d = 0;
    *best_m = 0;
    *best_dout = 0;

    d_min = MAX(DIV_ROUND_UP(fin, fpfd_max), 1);
    d_max = MIN(fin / fpfd_min, 80);
    
    m_min = MAX(DIV_ROUND_UP(fvco_min, fin) * d_min, 1);
    m_max = MIN(fvco_max * d_max / fin, 64);
    
    for(m = m_min; m <= m_max; m++) {
      _d_min = MAX(d_min, DIV_ROUND_UP(fin * m, fvco_max));
      _d_max = MIN(d_max, fin * m / fvco_min);

      for(d = _d_min; d <= _d_max; d++) {
	fvco = fin * m / d;
	
	dout = DIV_ROUND_CLOSEST(fvco, fout);
	dout = CLAMP(dout, 1, 128);
	f = fvco / dout;
	if (ABS(f - fout) < ABS(best_f - fout)) {
	  best_f = f;
	  *best_d = d;
	  *best_m = m;
	  *best_dout = dout;
	  if (best_f == (long)fout) {
	    return;
	  }
	}
      }
    }
  }

  void calc_clk_params(unsigned long divider, unsigned long *low,
		       unsigned long *high, unsigned long *edge,
		       unsigned long *nocount)
  {
    if (divider == 1)
      *nocount = 1;
    else
      *nocount = 0;
    
    *high = divider / 2;
    *edge = divider % 2;
    *low = divider - *high;
  }

  void write(unsigned long reg, unsigned long val)
  {
    ClockControlReq request;
    request.m_rnw  = 0;
    request.m_addr = reg;
    request.m_data = val & 0xFFFF;
    m_request.sendMessage(request);
  }

  void read(unsigned long reg, unsigned long *val)
  {
    ClockControlReq request;
    BitT<16> response;
    request.m_rnw  = 1;
    request.m_addr = reg;
    m_request.sendMessage(request);
    response = m_response.getMessage();
    *val = response.get();
  }

  ClockControlXactor();
  ClockControlXactor( const ClockControlXactor &);

public:
  ClockControlXactor(const std::string &hier, const std::string &inst, SceMi *scemi)
    : m_request(hier, inst + "_request", scemi)
    , m_response(hier, inst + "_response", scemi)
  {
  }

  ~ClockControlXactor()
  {
  }

  bool InReset()
  {
    unsigned long reg;
    read(clkgen_reg_status, &reg);
    return (bool)((reg >> 1) & 0x1);
  }

  bool IsLocked()
  {
    unsigned long reg;
    read(clkgen_reg_status, &reg);
    return (bool)((reg >> 0) & 0x1);
  }

  int SetRate(unsigned long clknum, unsigned long rate, unsigned long parent_rate = 200000000)
  {
    unsigned long d = 0;
    unsigned long m = 0;
    unsigned long dout = 0;
    unsigned long nocount = 0;
    unsigned long high = 0;
    unsigned long edge = 0;
    unsigned long low = 0;
    unsigned long filter = 0;
    unsigned long lock = 0;

    if (parent_rate == 0 || rate == 0)
      return 0;

    calc_params(parent_rate, rate, &d, &m, &dout);

    if (d == 0 || dout == 0 || m == 0) 
      return 0;

    filter = lookup_filter(m - 1);
    lock = lookup_lock(m - 1);

    write(clkgen_reg_update_enable, 0);

    calc_clk_params(dout, &low, &high, &edge, &nocount);
    write((clknum*2)+clkgen_reg_clkout0_1, (high << 6) | low);
    write((clknum*2)+clkgen_reg_clkout0_2, (edge << 7) | (nocount << 6));
    
    calc_clk_params(d, &low, &high, &edge, &nocount);
    write(clkgen_reg_clk_div, (edge << 13) | (nocount << 12) | (high << 6) | low);
    
    calc_clk_params(m, &low, &high, &edge, &nocount);
    write(clkgen_reg_clk_fb_1, (high << 6) | low);
    write(clkgen_reg_clk_fb_2, (edge << 7) | (nocount << 6));

    write(clkgen_reg_lock_1, lock & 0x3FF);
    write(clkgen_reg_lock_2, (((lock >> 16) & 0x1f) << 10) | 0x1);
    write(clkgen_reg_lock_3, (((lock >> 24) & 0x1f) << 10) | 0x3e9);
    write(clkgen_reg_filter_1, filter >> 16);
    write(clkgen_reg_filter_2, filter);
    write(clkgen_reg_update_enable, 1);

    return 0;
  }

  unsigned long GetRate(unsigned int clknum = 0, unsigned long parent_rate = 200000000)
  {
    unsigned long d, m, dout;
    unsigned long reg;
    unsigned long long tmp;

    read((clknum*2)+clkgen_reg_clkout0_1, &reg);
    dout = (reg & 0x3f) + ((reg >> 6) & 0x3f);
    read(clkgen_reg_clk_div, &reg);
    d = (reg & 0x3f) + ((reg >> 6) & 0x3f);
    read(clkgen_reg_clk_fb_1, &reg);
    m = (reg & 0x3f) + ((reg >> 6) & 0x3f);

    if (d == 0 || dout == 0) 
      return 0;

    tmp = (unsigned long long)(parent_rate / d) * m;
    tmp = tmp / dout;

    if (tmp > 0xFFFFFFFF)
      return 0xFFFFFFFF;
    return (unsigned long)tmp;
  }
};

#endif //__CLOCK_CONTROL_XACTOR_H__
