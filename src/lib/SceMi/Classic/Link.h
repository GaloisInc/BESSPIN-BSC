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
  virtual void queue(Packet* pkt) = 0;
  virtual bool ready_to_send(unsigned int channel) = 0;
  virtual bool send_pkt(unsigned int* channel_ptr) = 0;
  virtual Packet* recv_pkt(SceMiU64* cycle_ptr) = 0;
  virtual Packet* packet(unsigned int len) = 0;
  virtual void release(Packet* pkt) = 0;
  virtual void loop() = 0;
  virtual ~Link() ;

  // Return vendor specific data object for accessing the platform
  virtual void *scemi_extension_access_vendor_platform() { return NULL; }

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
