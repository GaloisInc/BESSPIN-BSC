#ifndef __EVE_LINK_H__
#define __EVE_LINK_H__

#include <vector>

#include "SceMiParameters.h"
#include "Link.h"

#include "libZebu.hh"

using namespace ZEBU;

typedef struct
{
  TxPort* tx;
  RxPort* rx;
  unsigned int size;
} tPortInfo;

class EVELink : public Link
{
 private:
  Board* board;
  size_t last_tx;
  std::vector<Packet*> pending;
  unsigned int num_ports;
  tPortInfo* ports;
  unsigned int first_rx_channel;
  unsigned int current_rx_channel;
  unsigned int board_initialized;
  const char *init_mem_file;
  const char *logic_state_file;
  unsigned int severity;
 public:
  EVELink(SceMiParameters* parameters);
  virtual void queue(Packet* pkt);
  virtual bool ready_to_send(unsigned int channel);
  virtual bool send_pkt(unsigned int* channel_ptr);
  virtual Packet* recv_pkt(SceMiU64* cycle_ptr);
  virtual Packet* packet(unsigned int len);
  virtual void release(Packet* pkt);
  virtual void loop();
  virtual ~EVELink();

  virtual void *scemi_extension_access_vendor_platform();
};

#endif /* __EVE_LINK_H__ */
