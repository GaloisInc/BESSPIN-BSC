#ifndef __TCP_LINK_H__
#define __TCP_LINK_H__

#include <vector>
#include <set>

#include "SceMiParameters.h"
#include "Link.h"
#include "TCPLinkCommon.h"

class TCPLink : public Link
{
 private:
  int socket_fd;
  size_t last_tx;
  std::vector<Packet*> pending;
  std::set<tChannelId> request_rx_slots;
  std::set<tChannelId> pending_acks;
  int closed;
 public:
  TCPLink(const SceMiParameters* parameters);
  virtual void queue(Packet* pkt);
  virtual bool ready_to_send(unsigned int channel);
  virtual bool send_pkt(unsigned int* channel_ptr);
  virtual Packet* recv_pkt(SceMiU64* cycle_ptr);
  virtual Packet* packet(unsigned int len);
  virtual void release(Packet* pkt);
  virtual void loop();
  virtual ~TCPLink();
 private:
  Packet* recv_data_packet(SceMiU64* cycle_ptr);
  void recv_req_packet();
  void recv_quit_packet();
  void send_ack_packet();
  void send_shutdown_packet();
};

#endif /* __TCP_LINK_H__ */

