#ifndef __LINK_H__
#define __LINK_H__

#include <stdio.h>
#include "SceMiTypes.h"

struct Packet
{
  unsigned int channel;
  unsigned int num_bits;
  SceMiU32*    data;
};

class Link
{
 public:
  // SCE-MI 1.1 support
  virtual void queue(Packet* pkt) = 0;
  virtual bool ready_to_send(unsigned int channel) = 0;
  virtual bool send_pkt(unsigned int* channel_ptr) = 0;
  virtual int  recv_pkt(SceMiU64* cycle_ptr, Packet **) = 0;
  virtual Packet* packet(unsigned int len) = 0;
  virtual void release(Packet* pkt) = 0;
  virtual void loop() = 0;
  virtual ~Link() ;

  // SCE-MI 2.1 pipes support
  virtual void set_data_handler( unsigned int pipe_num
                                 , void (*callback)(void*,const char*,unsigned int,bool,bool,bool)
                               , void* context
                               ) {};
  virtual void set_credit_handler( unsigned int pipe_num
                                 , void (*callback)(void*,unsigned int,bool)
                                 , void* context
                                 ) {};
  virtual void send_credits(unsigned int pipe_num, unsigned int amount, bool underflow) {};
  virtual void send_autoflush(unsigned int pipe_num, bool enable) {};
  virtual void add_message_data( unsigned int pipe_num, unsigned int elem_bytes
                                 , unsigned int len, const unsigned char* data
                                 , unsigned int byte_offset, bool eom
                               ) {};
  virtual void send_data(unsigned int pipe_num, bool overflow, bool flush) {};

  // Return vendor specific data object for accessing the platform
  virtual void *scemi_extension_access_vendor_platform() { return NULL; }

  virtual void debug (const char *msg) {};

 protected:
  // Contructor
  Link(const char *logEnvir);
  // protected data for logging data traffic
  bool   _logTraffic;
  FILE * _logFile;

  void report (const SceMiU64& ts, const Packet *pkt);
  void report (const Packet *pkt);
};

#endif /* __LINK_H__ */
