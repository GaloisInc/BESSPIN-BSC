/* Copyright (c) 2008 Bluespec, Inc.  All rights reserved. */

#include  <stdio.h>
#include  <stdlib.h>
#include  <unistd.h>
#include  <string.h>
#include  <assert.h>
#include  <errno.h>

#include  <sys/types.h>
#include  <sys/socket.h>
#include  <netinet/in.h>
#include  <netinet/tcp.h>
#include  <netdb.h>
#include  <poll.h>

#include "sized_types.h"
#include "TCPLink.h"
#include "LinkPlugin.h"

#define DEBUG_LINK 0

// Plugin C-linkage functions

Link* create_link(SceMiParameters* parameters,
                  SceMiErrorHandler hdlr, void* ctx)
{
  if (parameters == NULL) return NULL;

  ec_register_handler(hdlr, ctx);

  return new TCPLink(parameters);
}

void destroy_link(Link* link)
{
  delete link;
}

// Implementation of SW-side of TCP link layer

TCPLink::TCPLink(const SceMiParameters* parameters)
  : Link ("BSC_TRACE_SCEMI_TCP")
{
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    int  s;
    char service[12];

    /* Obtain address(es) matching host/port */

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;  /* TCP/IP socket */
    hints.ai_flags = 0;
    hints.ai_protocol = 0;          /* Any protocol */

    const char* addr  = parameters->AttributeStringValue("Link",0,"TCPAddress");
    if (addr == NULL)
      addr = "127.0.0.1";
    unsigned int port = parameters->AttributeIntegerValue("Link",0,"TCPPort");

    // TODO: use symbolic host name instead of IP address? gethostbyname()?
    sprintf(service, "%d", port);
    s = getaddrinfo(addr, service, &hints, &result);
    if (s != 0) {
	fprintf (stderr, "Error: c_socket_comms_init_client/getaddrinfo() failed: %s\n", gai_strerror(s));
	exit(EXIT_FAILURE);
    }

    /* getaddrinfo() returns a list of address structures.
       Try each address until we successfully connect(2).
       If socket(2) (or connect(2)) fails, we (close the socket
       and) try the next address. */

    for (rp = result; rp != NULL; rp = rp->ai_next) {
	socket_fd = socket(rp->ai_family, rp->ai_socktype,
		     rp->ai_protocol);
	if (socket_fd == -1)
	    continue;

	if (connect(socket_fd, rp->ai_addr, rp->ai_addrlen) != -1)
	    break;                  /* Success */

	close(socket_fd);
    }

    if (rp == NULL) {               /* No address succeeded */
	fprintf (stderr, "Error: c_socket_comms_init_client/connect() failed\n");
	exit(EXIT_FAILURE);
    }

    freeaddrinfo(result);           /* No longer needed */

    // Set NODELAY socket option to disable Nagle algorithm
    int one = 1;
    s = setsockopt(socket_fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

    last_tx = 0;

    // Set closed flag
    closed = false;

    // determine the maximun channel id to setup the pending vector
    unsigned int inportCount = parameters->NumberOfObjects ("MessageInPort");
    int maxInportChannelId = 0;
    for (unsigned int i = 0; i < inportCount ; ++i) {
      int chId = parameters->AttributeIntegerValue("MessageInPort", i, "ChannelId");
      maxInportChannelId = std::max (maxInportChannelId, chId);
    }
    pending.resize(1+maxInportChannelId);


#if DEBUG_LINK
      printf("TCP connection established\n");
    fflush(NULL);
#endif
    if (_logTraffic) {fprintf(_logFile, "TCP connection established\n" ); fflush(_logFile);}
}

void TCPLink::queue(Packet* pkt)
{
  if (closed)
  {
    release(pkt);
    return;
  }
  if (pkt->channel >= pending.size()) {
    fprintf(stderr, "incorrect pending size %u > %d\n", pkt->channel, (int) pending.size());
    exit( EXIT_FAILURE);
  }
  if (pending[pkt->channel] == NULL)
  {
    pending[pkt->channel] = pkt;
#if DEBUG_LINK
    printf("Queued packet on channel %d\n", pkt->channel);
    fflush(NULL);
#endif
    if (_logTraffic) { fprintf (_logFile, "Q"); report (pkt);}
  }
  else
  {
    fprintf(stderr, "No space to queue packet on channel %d\n", pkt->channel);
    exit(EXIT_FAILURE);
  }

}

void TCPLink::send_ack_packet()
{
  unsigned int num_channels;
  tChannelId channel;
  tMsgType msg_type = PKT_ACK;

  // This is a message of type PKT_ACK
  if (send (socket_fd, &msg_type, 1, 0) != 1) {
    fprintf (stderr, "send_ack_packet() failure to send ack packet\n");
    exit(EXIT_FAILURE);
  }

  // Send number of channels to ack
  num_channels = static_cast<unsigned int>(pending_acks.size());
  if (send (socket_fd, &num_channels, 4, 0) != 4) {
    fprintf (stderr, "send_ack_packet() failure to send ack packet\n");
    exit(EXIT_FAILURE);
  }

  // Send each channel
#if DEBUG_LINK
  printf("Sending ack to HW for channels:");
#endif
  for (std::set<tChannelId>::const_iterator iter = pending_acks.begin();
       iter != pending_acks.end();
       ++iter)
  {
    channel = *iter;
    if (_logTraffic) {fprintf (_logFile, "SAP%d\n", channel );fflush(_logFile);}

#if  DEBUG_LINK
    printf(" %d", channel);
#endif
    if (send (socket_fd, &channel, 4, 0) != 4) {
      fprintf (stderr, "send_ack_packet() failure to send ack packet\n");
      exit(EXIT_FAILURE);
    }
  }
#if DEBUG_LINK
  printf("\n");
  fflush(NULL);
#endif

  fsync (socket_fd);

  pending_acks.clear();
}


void TCPLink::send_shutdown_packet()
{
  tMsgType msg_type = PKT_SHUTDOWN;

  // This is a message of type PKT_SHUTDOWN
  if (send (socket_fd, &msg_type, 1, 0) != 1) {
    fprintf (stderr, "send_shutdown_packet() failure to send packet\n");
    exit(EXIT_FAILURE);
  }

  fsync (socket_fd);
  if (_logTraffic) {fprintf (_logFile, "shut down packet sent\n" );fflush(_logFile);}
}

bool TCPLink::ready_to_send(unsigned int channel)
{
  if (closed) return false;
  return  (request_rx_slots.find(channel) != request_rx_slots.end());
}

bool TCPLink::send_pkt(unsigned int* channel_ptr)
{
  if (closed) return false;

  // Select the next packet to send
  Packet* pkt = NULL;
  for (unsigned int n = 1; n <= pending.size(); ++n)
  {
    size_t ch = (last_tx + n) % pending.size();
    if (pending[ch] == NULL) continue;

    pkt = pending[ch];
    pending[ch] = NULL;
    last_tx = ch;
    break;
  }


  // If we have a packet to send, send it
  if (pkt != NULL)
  {
    // Mark the request rx slot as empty
    request_rx_slots.erase(pkt->channel);

    if (channel_ptr != NULL)
      *channel_ptr = pkt->channel;

    // This is a PKT_DATA packet
    tMsgType msg_type = PKT_DATA;
    if (send (socket_fd, &msg_type, 1, 0) != 1) {
      fprintf (stderr, "TCPLink::send() failure on channel number %0d\n", pkt->channel);
      exit (EXIT_FAILURE);
    }

    // Send channel number
    if (send (socket_fd, &(pkt->channel), 4, 0) != 4) {
      fprintf (stderr, "TCPLink::send() failure on channel number %0d\n", pkt->channel);
      exit (EXIT_FAILURE);
    }

    // Send record length
    if (send (socket_fd, &(pkt->num_bits), 4, 0) != 4) {
      fprintf (stderr, "TCPLink::send() failure on record length %0d\n", pkt->num_bits);
      exit (EXIT_FAILURE);
    }

    // Send record data
    int nB = bits_to_bytes(pkt->num_bits);
    if (send (socket_fd, pkt->data, nB, 0) != nB) {
      fprintf (stderr, "TCPLink::send() failure on record data\n");
      exit (EXIT_FAILURE);
    }

    if (_logTraffic) report(pkt);

    release(pkt);
  }

  fsync (socket_fd);

  return (pkt != NULL);
}

Packet* TCPLink::recv_data_packet(SceMiU64* cycle_ptr)
{
  ssize_t nread, n;
  UInt8* p;
  int nB;
  Packet* pkt = new Packet;

  // ---------------- Read channel number (next 4 bytes)
  nread = 0;
  p = (UInt8 *) &(pkt->channel);
  while (nread < 4) {
    n = recv (socket_fd, & (p[nread]), (4 - nread), 0);
    if (n < 0) perror("recv");
    if (n == 0 && nread == 0)
      return NULL; // connection has been shutdown
    if (n <= 0) continue;
    nread += n;
  }

  // ---------------- Read record size (next 4 bytes)
  nread = 0;
  p = (UInt8 *) &(pkt->num_bits);
  while (nread < 4) {
    n = recv (socket_fd, & (p[nread]), (4 - nread), 0);
    if (n < 0) perror("recv");
    if (n <= 0) continue;
    nread += n;
  }

  // Allocate array to hold data
  pkt->data = new SceMiU32[bits_to_words(pkt->num_bits)];

  // ---------------- Read record contents
  nB    = bits_to_bytes(pkt->num_bits);
  nread = 0;
  p = (UInt8 *) pkt->data;
  while (nread < nB) {
    n = recv (socket_fd, & (p[nread]), (nB - nread), 0);
    if (n < 0) perror("recv");
    if (n <= 0) continue;
    nread += n;
  }

  unsigned int lastWord = bit_offset_to_word_index(pkt->num_bits);
  unsigned int bitsToKeep = bit_offset_to_word_offset(pkt->num_bits);
  unsigned int mask = (bitsToKeep == 32) ? (~0) : ((1 << bitsToKeep) - 1);

  if (bitsToKeep)
    pkt->data[lastWord] &= mask;

  // ---------------- Read cycle stamp
  nread = 0;
  SceMiU64 cycle;
  p = (UInt8 *) (&cycle);
  while (nread < 8) {
    n = recv (socket_fd, & (p[nread]), (8 - nread), 0);
    if (n < 0) perror("recv");
    if (n <= 0) continue;
    nread += n;
  }
  if (cycle_ptr != NULL)
    *cycle_ptr = cycle;

  // record that we need to ack this channel
  pending_acks.insert(pkt->channel);

#if DEBUG_LINK
  printf("Received pkt on channel %d\n", pkt->channel);
  fflush(NULL);
#endif
  if(_logTraffic) report (cycle,pkt);
  return pkt;
}

void TCPLink::recv_req_packet()
{
  ssize_t nread, n;
  UInt8* p;
  tChannelId channel;

  // ---------------- Read req channel numbers (4 bytes each)
  nread = 0;
  p = (UInt8 *) &channel;
  while (nread < 4) {
    n = recv (socket_fd, & (p[nread]), (4 - nread), 0);
    if (n < 0) perror("recv");
    if (n <= 0) continue;
    nread += n;
  }
#if DEBUG_LINK
  printf("Revc Req Packet %d\n", channel);
  fflush(NULL);
#endif
  if (_logTraffic) {fprintf (_logFile, "RRP %d\n", channel);fflush(_logFile);}

  // Mark the rx slot as got a request from HW
  if (request_rx_slots.find(channel) == request_rx_slots.end())
    request_rx_slots.insert(channel);
}

void TCPLink::recv_quit_packet()
{
  closed = true;
}

Packet* TCPLink::recv_pkt(SceMiU64* cycle_ptr)
{
  ssize_t nread, n;
  UInt8* p;
  struct pollfd pfd;

  if (closed) return NULL;

  pfd.fd = socket_fd;
  pfd.events = POLLIN;
  pfd.revents = 0;

  n = poll(&pfd, 1, 0);
  if (n < 0)
  {
    if (errno == EINTR) return NULL;
#ifdef __APPLE__
    char err[1024];
    if (strerror_r(errno, err, 1024) != 0)
      strcpy(err, "Unknown error");
#else
    char buf[1024];
    char *err = strerror_r(errno, buf, 1024);
#endif
    fprintf (stderr, "Error: poll failed on socket %0d\n%s\n", socket_fd, err );
    exit(EXIT_FAILURE);
  }

  if (n == 0) // no incoming data
  {
    // if we have unacked received packets, ack them now
    if (!pending_acks.empty())
      send_ack_packet();
    return NULL;
  }

  // ---------------- Read message type code (first byte)
  nread = 0;
  tMsgType msg_type = PKT_DATA;
  p = (UInt8 *) &msg_type;
  while (nread < 1) {
    n = recv (socket_fd, & (p[nread]), (1 - nread), 0);
    if (n < 0) perror("recv");
    if (n == 0 && nread == 0)
      return NULL; // connection has been shutdown
    if (n <= 0) continue;
    nread += n;
  }

  switch (msg_type)
  {
    case PKT_DATA:     { return recv_data_packet(cycle_ptr); }
    case PKT_REQ:      { recv_req_packet(); break; }
    case PKT_SHUTDOWN: { recv_quit_packet(); break; }
    default: fprintf(stderr, "TCPLink::recv_pkt(): unknown message type: %x\n", msg_type);
  }

  return NULL;
}

Packet* TCPLink::packet(unsigned int len)
{
  Packet* pkt = new Packet;
  pkt->num_bits = len;
  pkt->data = new SceMiU32[bits_to_words(len)];
  return pkt;
}

void TCPLink::release(Packet* pkt)
{
  delete[] pkt->data;
  delete pkt;
}

void TCPLink::loop()
{
  // nothing needed here
}

TCPLink::~TCPLink()
{
  if (!closed) {
    bool is_shutdown;
    ssize_t n;
    char buf[32];

    closed = true;
    send_shutdown_packet();
    shutdown(socket_fd, SHUT_WR);

    is_shutdown = false;
    while (!is_shutdown)
    {
      n = recv(socket_fd, &buf, 32, 0);
      if (n < 0) perror("recv");
      if (n == 0)
      {
	// connection is shut down
	is_shutdown = true;
	break;
      }
    }

    close(socket_fd);
  }
}
