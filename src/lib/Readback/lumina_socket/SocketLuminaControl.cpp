// Copyright (c) 2013-2016 Bluespec, Inc.  All Rights Reserved

/* Acknowledgement: portions adapted from example ECHOSERV
     ECHOSERV
     (c) Paul Griffiths, 1999
     http://www.paulgriffiths.net/program/c/echoserv.php
*/

// ================================================================
// C lib includes

#include <sys/socket.h>       /*  socket definitions        */
#include <sys/types.h>        /*  socket types              */
#include <arpa/inet.h>        /*  inet (3) funtions         */
#include <poll.h>
#include <fcntl.h>            /* To set non-blocking mode   */

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <iostream>
#include <sstream>
#include <cstdio>

// ================================================================
// Local includes

#include "SocketLuminaControl.hpp"

// ================================================================

enum RdBackControlReqType { Reserved0, Reserved1, Reserved2, Reserved3,
			    Reserved4, Reserved5, RdBackCmd, RdBackStore };

class RdBackControlReq
{
private:
  RdBackControlReqType  m_type;
  unsigned int          m_data;
public:
  RdBackControlReq (RdBackControlReqType type, unsigned int data)
    : m_type(type)
    , m_data(data)
  {
    unsigned int maxdata = 1 << 29;
    if (data >= maxdata) {
      std::cerr << "Error: RdBackControlReq data too large: " << std::dec << data
		<< ". Must be less than " << maxdata << std::endl;
      m_data = maxdata - 1 ;
    }
  }

  RdBackControlReq ()
    : m_type(Reserved0)
    , m_data(0)
  {
  }

  unsigned int getBits() const
  {
    unsigned int data = m_data & 0x1FFFFFFF;
    data = data | ((m_type & 0x7) << 29);
    return data;
  }

  static const char * toString (const RdBackControlReqType & c)
  {
    switch (c) {
    case Reserved0:   return "Reserved0: "  ; break ;
    case Reserved1:   return "Reserved1: "  ; break ;
    case Reserved2:   return "Reserved2: "  ; break ;
    case Reserved3:   return "Reserved3: "  ; break ;
    case Reserved4:   return "Reserved4:"   ; break ;
    case Reserved5:   return "Reserved5:"   ; break ;
    case RdBackCmd:   return "RdBackCmd:"   ; break ;
    case RdBackStore: return "RdBackStore:" ; break ;
    }
    return "ERROR: RdBackControlReqType enum" ;
  }

  friend std::ostream & operator<< (std::ostream &os, const RdBackControlReq &req)
  {
    os << "{RdBackControlReq " << toString(req.m_type) << " cnt " << std::dec << req.m_data << "}" ;
    return os ;
  }
};

// ================================================================

SocketLuminaControl::SocketLuminaControl(const unsigned int port)
  : LuminaControl()
{
  // ---------------
  // Connect to the server socket

  // Create the socket
  if ( (m_sockfd = socket (AF_INET, SOCK_STREAM, 0)) < 0 ) {
    throw std::string("socket() failed");
  }

  struct sockaddr_in servaddr;  // socket address structure

  // Initialize socket address structure
  memset (& servaddr, 0, sizeof (servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port   = htons (port);

  /*
  // Set the remote IP address
  if (inet_aton (server_host, & servaddr.sin_addr) <= 0 ) {
  fprintf (stderr, "ERROR: _open (): Invalid remote IP address.\n");
  return -1;
  }
  */
  servaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

  // connect() to the remote server
  if (connect (m_sockfd, (struct sockaddr *) &servaddr, sizeof(servaddr) ) < 0 ) {
    throw std::string("connect() failed");
  }
}
  
SocketLuminaControl::~SocketLuminaControl()
{
  if (m_sockfd > 0)
    close(m_sockfd);
}

void SocketLuminaControl::write_word(uint32_t w)
{
  int pkt_size = 4;
  uint8_t write_buf[pkt_size];
  int n_write = 0;

  write_buf[3] = (uint8_t)(  w        & 0xFF );
  write_buf[2] = (uint8_t)( (w >>  8) & 0xFF );
  write_buf[1] = (uint8_t)( (w >> 16) & 0xFF );
  write_buf[0] = (uint8_t)( (w >> 24) & 0xFF );

  while (n_write < pkt_size) {
    ssize_t n = write(m_sockfd, & (write_buf [n_write]), (pkt_size - n_write));
    if (n < 0) {
      if ((errno != EAGAIN) && (errno != EWOULDBLOCK)) {
	std::cout << "write() failed: " << strerror(errno) << std::endl;
	exit(1);
	//throw std::string ("write() failed");
      }
    } else {
      n_write += n;
    }
  }
  //std::cout << "==> WRITE: " << std::hex << w << std::endl;
}

uint32_t SocketLuminaControl::read_word(void)
{
  int pkt_size = 4;
  uint8_t read_buf[pkt_size];
  int n_read = 0;

  struct pollfd fds[1];
  fds[0].fd = m_sockfd;
  fds[0].events = POLLIN;
    
  while (n_read < pkt_size) {
    int p = poll(fds, 1, -1);
    if (p > 0) {

      // Data from hardware
      if (fds[0].revents & POLLIN) {
	int n = read (m_sockfd, & (read_buf [n_read]), (pkt_size - n_read));
	//std::cout << "READ: " << std::dec << n << std::endl;
	if (n < 0) {
	  if ((errno != EAGAIN) && (errno != EWOULDBLOCK)) {
	     std::cout << "read() failed: " << strerror(errno) << std::endl;
	     exit(1);
	     //throw std::string ("read() failed");
	  }
	} else if (n == 0) {
	  // XXX indicates closed connection?
	  std::cout << "read() connection closed" << std::endl;
	  exit(1);
	  //throw std::string ("read() connection closed");
	} else {
	  n_read += n;
	}
      }
      
    } else if (p < 0) {
      std::cout << "poll() failed: " << strerror(errno) << std::endl;
      exit(1);
      //throw (std::string("poll failed: " + std::string(strerror(errno))));
    }
  } // while (true)

  uint32_t w =
    ( ((uint32_t)(read_buf[3]) & 0xFF)       ) |
    ( ((uint32_t)(read_buf[2]) & 0xFF) <<  8 ) |
    ( ((uint32_t)(read_buf[1]) & 0xFF) << 16 ) |
    ( ((uint32_t)(read_buf[0]) & 0xFF) << 24 );
  //std::cout << "==> REQ: " << std::hex << w << std::endl;

  return w;
}

bool SocketLuminaControl::readState()
{
  if (m_sockfd < 0) return false;

  bool debug = false;

  unsigned int end_of_data;

  std::cout << "readState" << std::endl;

  // Send a request for a Readback response packet
  write_word( RdBackControlReq(RdBackCmd,8).getBits() );

  if (debug) printf("Request sent\n");
  do {
    uint32_t data = read_word();
    if (debug) printf("Data received: %x\n", (unsigned int)data);
    // give the word to the Readback core
    end_of_data = m_rdbackCallback(m_rdbackCallbackParm, (unsigned int)data);
    if (debug) if (end_of_data) printf("LAST\n");
  } while (! end_of_data);
    
  return true;
}

bool SocketLuminaControl::sendRdBackClear ()
{
  if (m_sockfd < 0) return false;

  unsigned int code = 0;
  RdBackControlReq req(RdBackCmd, code);
  write_word( req.getBits() );
  return true;
}
  
bool SocketLuminaControl::sendRdBackStore (unsigned int code)
{
  if (m_sockfd < 0) return false;

  RdBackControlReq req(RdBackStore, code);
  write_word( req.getBits() );
  return true;
}

bool SocketLuminaControl::sendRdBackFinish (unsigned int config)
{
  if (m_sockfd < 0) return false;

  unsigned int code = (config & 0x7FFFFFF) | (1<<27);
  RdBackControlReq req(RdBackCmd, code);
  write_word( req.getBits() );
  return true;
}
  
bool SocketLuminaControl::sendRdBackBreakCode (unsigned int config)
{
  if (m_sockfd < 0) return false;

  unsigned int code = (config & 0xFFFF) | (1<<28);
  RdBackControlReq req(RdBackCmd, code);
  write_word( req.getBits() );
  return true;
}

// ================================================================
