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

#include <vector>
#include <set>

#include "TCPLinkCommon.h"

#define DEBUG_LINK 0

// These are functions that implement the HW-side of the TCP link.
// sce* functions are called directly by the user, bscemi_* functions
// are used internally by the BSV library on the HW side.

static int setup = 0;
static int forced_shutdown = 0;
static int sfd; // server socket file descriptor
static std::vector<tPayload> packets_from_sw;
static std::set<tChannelId> full_rx_slots;
static std::set<tChannelId> full_tx_slots;

extern "C"
{

// forward declaration
void bscemi_close_socket(void);

// ================================================================
// Open a server socket
//
// This routine will listend on the given port for a connection.
// Once the connection is established, it will set up to exchange
// data.
//
// If the autoclose argument is non-zero, it will use at_exit()
// to close the socket when the process exits.  Otherwise,
// bscemi_close_socket() must be called explicitly.

void bscemi_open_socket(unsigned int port, unsigned int autoclose)
{
    struct addrinfo   hints,  *result,  *rp;
    int  s;

    struct sockaddr  from_addr;
    int sfd2;
    socklen_t from_addr_len;

    char port_str[12];

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;    /* For wildcard IP address */
    hints.ai_protocol = 0;
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;

    sprintf(port_str, "%d", port);
    s = getaddrinfo(NULL, port_str, &hints, &result);
    if (s != 0) {
        fprintf (stderr, "Error: sceMiTCPListen()/getaddrinfo(): %s\n", gai_strerror(s));
        exit(EXIT_FAILURE);
    }

    /* getaddrinfo() returns a list of address structures.
       Try each address until we successfully bind(2).
       If socket(2) (or bind(2)) fails, we (close the socket
       and) try the next address. */

    for (rp = result; rp != NULL; rp = rp->ai_next) {
        sfd = socket(rp->ai_family, rp->ai_socktype,
                     rp->ai_protocol);
        if (sfd == -1)
            continue;

        int one = 1;
        if (setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one)) == -1)
            perror("setsockopt");

        if (setsockopt(sfd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one)) == -1)
            perror("setsockopt");

        if (bind(sfd, rp->ai_addr, rp->ai_addrlen) == 0)
            break;                  /* Success */
        perror("bind");

        close(sfd);
    }

    if (rp == NULL) {               /* No address succeeded */
        fprintf (stderr, "Error: sceMiTCPListen()/bind() failed at port %d\n", port);
        exit(EXIT_FAILURE);
    }

    freeaddrinfo(result);           /* No longer needed */

    // Start listening for the client's connection
    // The second arg is 1 because we're only expecting a single client
    s = listen (sfd, 1);
    if (s < 0) {
        fprintf (stderr, "Error: sceMiTCPListen()/listen() failed on socket %0d at port %d\n",
                 sfd, port);
        exit(EXIT_FAILURE);
    }

    // Accept a connection from the client

    from_addr_len = sizeof (from_addr);
    sfd2 = accept (sfd, & from_addr, & from_addr_len);
    if (sfd2 < 0) {
        fprintf (stderr, "Error: sceMiTCPListen()/accept() failed on socket %0d at port %d\n",
                 sfd, port);
        exit(EXIT_FAILURE);
    }

    // Typical general-purpose servers do a fork() at this point to have the
    // child process handle this connection while the parent listens for
    // more connections, but here we are not expecting any more connections
    // so we don't fork a new process

    close(sfd);  // not needed any more

    setup = 1;
    forced_shutdown = 0;
    sfd = sfd2;  // Leave socket file descriptor in sfd2

    // setup to close the socket on exit
    if (autoclose)
      atexit(bscemi_close_socket);

    // clear the packet tracking data
    packets_from_sw.clear();
    full_rx_slots.clear();
    full_tx_slots.clear();

    // XXX resize vector to avoid calling resize during runtime, which may not work
    // if resize (in handle_data_packet) is called in seperate threads
    if (packets_from_sw.size() < 2048) {
      packets_from_sw.resize(2048);
    }
}

// ================================================================
// Close the server socket

void bscemi_close_socket(void)
{
  if (setup)
  {
    // Read from socket until recv returns 0, signifying EOF.
    // This indicates that the other side of the connection has
    // shut down.

    bool is_shutdown;
    ssize_t n;
    char buf[32];

    if (forced_shutdown == 0)
    {
      // Send a message of type PKT_SHUTDOWN
      tMsgType msg_type = PKT_SHUTDOWN;
      if (send (sfd, &msg_type, 1, 0) != 1) {
        fprintf (stderr, "bscemi_close_socket() failure to send shutdown packet\n");
        exit(EXIT_FAILURE);
      }

      fsync (sfd);
    }

    shutdown(sfd, SHUT_WR); // shutdown for writing

    is_shutdown = false;
    while (!is_shutdown)
    {
      n = recv(sfd, &buf, 32, 0);
      if (n < 0) perror("recv1");
      if (n == 0)
      {
        // connection is shut down
        is_shutdown = true;
        break;
      }
    }

    close (sfd);
    setup = 0;
  }
}

// ================================================================
// Handle received packets and send ack responses.


// helper routine to handle a PKT_DATA message
static void handle_data_packet()
{
  unsigned int nread;
  char* p;
  ssize_t n;
  unsigned int channel;
  unsigned int bit_len;
  unsigned int* pkt;
  unsigned int nB;

  // ---------------- Read channel number (4 bytes)
  nread = 0;
  p = (char *) &channel;
  while (nread < 4) {
    n = recv (sfd, & (p[nread]), (4 - nread), 0);
    if (n < 0) perror("recv2");
    if (n <= 0) continue;
    nread += static_cast<unsigned int>(n);
  }

  // ---------------- Read record size (next 4 bytes)
  nread = 0;
  p = (char *) &bit_len;
  while (nread < 4) {
    n = recv (sfd, & (p[nread]), (4 - nread), 0);
    if (n < 0) perror("recv3");
    if (n <= 0) continue;
    nread += static_cast<unsigned int>(n);
  }

  // Allocate array to hold data
  pkt = new unsigned int[bits_to_words(bit_len)];

  // ---------------- Read record contents
  nB    = bits_to_bytes(bit_len);
  nread = 0;
  p = (char *) pkt;
  while (nread < nB) {
    n = recv (sfd, & (p[nread]), (nB - nread), 0);
    if (n < 0) perror("recv4");
    if (n <= 0) continue;
    nread += static_cast<unsigned int>(n);
  }

  unsigned int lastWord = bit_offset_to_word_index(bit_len);
  unsigned int bitsToKeep = bit_offset_to_word_offset(bit_len);
  unsigned int mask = (bitsToKeep == 32) ? (~0) : ((1 << bitsToKeep) - 1);

  // Do masking only if the bit_len is not on word boundary
  if (bitsToKeep)
    pkt[lastWord] &= mask;

  // Store received data
  if (channel >= packets_from_sw.size())
    packets_from_sw.resize(channel+1);
  if (packets_from_sw[channel] == NULL)
    packets_from_sw[channel] = pkt;

#if DEBUG_LINK
  printf("Received data packet from SW on channel %d, Bit len: %d pkt: %08x\n", channel, bit_len, *pkt);
  fflush(NULL);
#endif
}

// helper routine to handle a PKT_ACK message
// and send back a PKT_ACK reply.
void handle_ack_packet()
{
  unsigned int nread;
  char* p;
  ssize_t n;
  unsigned int channel;
  unsigned int num_channels;

  // ---------------- Read number of channels (4 bytes)
  nread = 0;
  p = (char *) &num_channels;
  while (nread < 4) {
    n = recv (sfd, & (p[nread]), (4 - nread), 0);
    if (n < 0) perror("recv5");
    if (n <= 0) continue;
    nread += static_cast<unsigned int>(n);
  }

  // ---------------- Read acked channel numbers (4 bytes each)
  while (num_channels-- > 0)
  {
    nread = 0;
    p = (char *) &channel;
    while (nread < 4) {
      n = recv (sfd, & (p[nread]), (4 - nread), 0);
      if (n < 0) perror("recv6");
      if (n <= 0) continue;
      nread += static_cast<unsigned int>(n);
    }

    // Mark the ack'ed tx slot as empty now
    full_tx_slots.erase(channel);
  }

#if DEBUG_LINK
  printf("Received ACK packet from SW\n");
  fflush(NULL);
#endif

  //ack_empty_rx_slots();
}

// helper routine to handle a PKT_SHUTDOWN message
static void handle_quit_packet()
{
  forced_shutdown = 1;
  bscemi_close_socket();
}

// Called to receive packets, and dispatch them appropriately
static void dispatch_packets()
{
  unsigned int nread;
  char* p;
  ssize_t n;
  struct pollfd pfd;
  tMsgType msg_type = PKT_DATA;

  if (!setup) return;

  pfd.fd = sfd;
  pfd.events = POLLIN;
  pfd.revents = 0;

  while (setup)
  {
    n = poll(&pfd, 1, 0);
    if (n < 0)
    {
      if (errno == EINTR) continue;
#ifdef __APPLE__
      char err[1024];
      if (strerror_r(errno, err, 1024) != 0)
        strcpy(err, "Unknown error");
#else
      char buf[1024];
      char *err = strerror_r(errno, buf, 1024);
#endif
      fprintf (stderr, "Error: poll failed on socket %0d\n%s\n", sfd, err);
      exit(EXIT_FAILURE);
    }
    if (n == 0) break;

    /* if we got here, there is data to read */

    // ---------------- Read message type (1 byte)
    nread = 0;
    p = (char *) &msg_type;
    while (nread < 1) {
      n = recv (sfd, & (p[nread]), (1 - nread), 0);
      if (n < 0) perror("recv7");
      if (n <= 0) continue;
      nread += static_cast<unsigned int>(n);
    }

    // dispatch to handler based on msg_type
    switch (msg_type)
    {
      case PKT_DATA:     { handle_data_packet(); break; }
      case PKT_ACK:      { handle_ack_packet(); break; }
      case PKT_SHUTDOWN: { handle_quit_packet(); break; }
      default: fprintf(stderr, "Unknown message type: %x\n", msg_type);
    }
  }
}

// ================================================================
// Test if data has been received on a channel.

char bscemi_is_data_available(unsigned int channel)
{
  dispatch_packets();
  if (channel >= packets_from_sw.size()) return 0;
  if (packets_from_sw[channel] != NULL)
    return 1;
  else
    return 0;
}


// ================================================================
// Get a copy of the data available on a channel.
//
// This should be called when bscemi_is_data_available() has
// returned 1 for this channel.  It is assumes that the len value
// is the correct number of bits for the channel and the data buffer.

void bscemi_recv(unsigned int* data, unsigned int channel, unsigned int len)
{
  if (channel >= packets_from_sw.size()) return;
  if (packets_from_sw[channel] == NULL) return;
  unsigned int* pkt = packets_from_sw[channel];
  memcpy(data, pkt, bits_to_words(len) * 4);
  full_rx_slots.erase(channel);
  packets_from_sw[channel] = NULL;
  delete[] pkt;
#if DEBUG_LINK
  printf("HW read packet from channel %d bit len: %d  Data: %08x\n", channel, len, *data);
  fflush(NULL);
#endif
}

// ================================================================
// Request data on a channel.
//

void bscemi_request(unsigned int channel)
{
  // Don't send request if the link is not open
  if (!setup)
    return;

  // If this channel is already in request mode then return
  if (full_rx_slots.find(channel) != full_rx_slots.end())
    return;

  // This is a message of type PKT_REQ
  tMsgType msg_type = PKT_REQ;
  if (send (sfd, &msg_type, 1, 0) != 1) {
    fprintf (stderr, "bscemi_request() failure to send req message type\n");
    exit (EXIT_FAILURE);
  }

  // Send each channel
#if DEBUG_LINK
  printf("Sending REQ to SW for channel: %d", channel);
#endif

  if (send (sfd, &channel, 4, 0) != 4) {
    fprintf (stderr, "bscemi_request() failure to send req channel number\n");
    exit (EXIT_FAILURE);
  }

  full_rx_slots.insert(channel);

#if DEBUG_LINK
  printf("\n");
  fflush(NULL);
#endif

  fsync (sfd);
}

// ================================================================
// Check if it is ok to send on a channel.
//
// This is done by testing the full_tx_slots set, which
// contains all channels which have had data sent
// but not acknowledged.

char bscemi_is_buffer_available(unsigned int channel)
{
  if (full_tx_slots.find(channel) != full_tx_slots.end())
  {
    dispatch_packets(); // to check for ACK packets
    return 0;
  }
  else
  {
    return 1;
  }
}

// ================================================================
// Send one data packet on the given channel.
//
// It assumes that it is being called correctly, after
// bscemi_is_buffer_available has returned 1 for
// the channel.

void bscemi_send(tChannelId channel,
                 UInt32 len,
                 tPayload data,
                 UInt64 stamp)
{
  if (!setup) return;

  // This is a data packet
  tMsgType msg_type = PKT_DATA;
  if (send (sfd, &msg_type, 1, 0) != 1) {
    fprintf (stderr, "bscemi_send() failure on channel number %0d\n", channel);
    exit (EXIT_FAILURE);
  }

  // Send channel number
  if (send (sfd, &channel, 4, 0) != 4) {
    fprintf (stderr, "bscemi_send() failure on channel number %0d\n", channel);
    exit (EXIT_FAILURE);
  }

  // Send record length
  if (send (sfd, &len, 4, 0) != 4) {
    fprintf (stderr, "bscemi_send() failure on record length %0d\n", len);
    exit (EXIT_FAILURE);
  }

  // Send record data
  int nB = bits_to_bytes(len);
  if (send (sfd, data, nB, 0) != nB) {
    fprintf (stderr, "bscemi_send() failure on record data\n");
    exit (EXIT_FAILURE);
  }

  // Send cycle stamp
  if (send (sfd, &stamp, 8, 0) != 8) {
    fprintf (stderr, "bscemi_send() failure on cycle stamp %0llu\n", stamp);
    exit (EXIT_FAILURE);
  }

  fsync (sfd);

  // Mark the tx slot as full
  full_tx_slots.insert(channel);

#if DEBUG_LINK
  printf("Sent data packet to SW on channel %d, Bit len: %d pkt: %08x\n", channel, len, *data);
  fflush(NULL);
#endif
}

// ================================================================
// External shutdown trigger detection

char bscemi_shutdown_triggered()
{
  if (forced_shutdown == 0)
    dispatch_packets();
  return ((forced_shutdown == 0) ? 0 : 1);
}

} // extern "C"
