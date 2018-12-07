#pragma once

#include <string>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <netdb.h>

#include "log.h"
#include "cable.h"
#include "interface.h"

using namespace std;

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL (0)
#endif

#ifndef MSG_DONTWAIT
#define MSG_DONTWAIT (0)
#endif


class sim : public cable
{
private:
  int      m_fd;
  int      m_port;
  string   m_addr;

public:
  sim();
  ~sim();

  static cable* create() { return new sim(); }

  int open();
  int close();
  bool is_present();
  void txrx_block(const uint8_t *tdi, uint8_t *tdo, int length, bool last);
  void tx_tms(uint8_t *in, int length, int force);

protected:
  void send_msg(const uint8_t *buf, int len);
  int recv_msg(uint8_t *buf, int len);
};

REGISTER(sim, cable)
