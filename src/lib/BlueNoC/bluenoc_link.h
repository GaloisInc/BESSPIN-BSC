//
// Copyright (c) 2014
//
#ifndef __BLUENOC_LINK_H__
#define __BLUENOC_LINK_H__

#include <cstdio>
#include <stdint.h>

struct Packet 
{
  uint32_t  nodeid;
  uint32_t  num_bits;
  uint32_t *data;
};

class bluenoc_link
{
public:
  virtual ~bluenoc_link();
  virtual int  recv_pkt(uint64_t* cycle_ptr, Packet **) = 0;

  virtual void debug(const char *msg) {};

  virtual void set_data_handler( uint32_t node,
				 void (*callback)(void *, const char *, uint32_t, bool, bool, bool),
				 void *context) {};

  virtual void set_credit_handler ( uint32_t node,
				    void (*callback)(void *, uint32_t, bool),
				    void *context) {};

  virtual void add_message_data( uint32_t node, uint32_t elem_bytes, uint32_t len, 
				 const uint8_t *data, uint32_t byte_offset, bool eom) {};

  virtual void send_data( uint32_t node, bool overflow, bool flush) {};
  virtual void send_credits( uint32_t node, uint16_t amount, bool underflow) {};
  virtual void send_autoflush(uint32_t node, bool enable) {};

protected:
  bluenoc_link(const char *logEnvir);
  bool m_logTraffic;
  FILE *m_logFile;

  void reportin (const Packet *pkt); // HOST->HW
  void reportout (const Packet *pkt); // HW->HOST
};

#endif // __BLUENOC_LINK_H__
