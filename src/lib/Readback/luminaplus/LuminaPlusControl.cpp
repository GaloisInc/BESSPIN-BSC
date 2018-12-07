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
#include <stdexcept>

#ifdef __APPLE__
#include <sys/time.h>
#endif

// ================================================================
// Local includes

#include "LuminaPlusControl.hpp"

// ================================================================

enum RdBackControlReqType { Edges,    Query,     Stop,      Resume,
			    RdBackOn, RdBackOff, RdBackCmd, RdBackStore };

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
    : m_type(Query)
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
    case Edges:       return "Edges: "      ; break ;
    case Query:       return "Query: "      ; break ;
    case Stop:        return "Stop: "       ; break ;
    case Resume:      return "Resume: "     ; break ;
    case RdBackOn:    return "RdBackOn:"    ; break ;
    case RdBackOff:   return "RdBackOff:"   ; break ;
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

// Utility function to convert delta time to absolute
static void  setTimeout(struct timespec &ts,
			const time_t delta_seconds, const long delta_microseconds) {
#ifdef __APPLE__
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ts.tv_sec = tv.tv_sec;
  ts.tv_nsec = tv.tv_usec*1000;
#else
  clock_gettime(CLOCK_REALTIME, &ts);
#endif
  time_t overflow_s  = delta_microseconds / 1000000 ;
  long corrected_us  = delta_microseconds % 1000000 ;

  ts.tv_nsec = ts.tv_nsec + (corrected_us * 1000);
  if (ts.tv_nsec >= 1000000000) {
    ts.tv_nsec = ts.tv_nsec - 1000000000;
    overflow_s ++;
  }
  ts.tv_sec = ts.tv_sec + delta_seconds + overflow_s;
}

// ================================================================

LuminaPlusControl::LuminaPlusControl(const unsigned int port)
  : RdBackControl()
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

  // ---------------
  // initialize the status queue signals

  pthread_mutex_init(&m_status_queue_lock, NULL);
  pthread_cond_init(&m_status_queue_cond, NULL);

  // ---------------
  // don't start a service loop until the Readback object has been registered
  m_service_thread_initialized = false;
}
  
LuminaPlusControl::~LuminaPlusControl()
{
  if (m_service_thread_initialized)
    stopServiceLoop();

  // close the socket
  if (m_sockfd > 0)
    close(m_sockfd);
}

void LuminaPlusControl::startServiceLoop()
{
  // create a communication channel between the main and loop threads
  int ret = socketpair(AF_UNIX, SOCK_STREAM, 0, m_service_sockets);
  if (ret < 0) {
    throw (std::string("socketpair failed: ") + std::string(strerror(errno)));
  }

  if ( pthread_create( &m_service_threadid, 0, LuminaPlusControl::startServiceThread, (void*)this ) ) {
    throw std::runtime_error("Failed to create thread.");
  }

  m_service_thread_initialized = true;
}

void LuminaPlusControl::stopServiceLoop()
{
  // Signal the service thread to stop
  char buf[1];
  buf[0] = 0; // value doesn't matter
  write(m_service_sockets[0], buf, 1);
  // Wait for the service thread to finish
  pthread_join(m_service_threadid, 0);
  
  // Destroy the status queue signals
  pthread_cond_destroy(&m_status_queue_cond);
  pthread_mutex_destroy(&m_status_queue_lock);
}

void LuminaPlusControl::serviceLoop()
{
  int pkt_size = 4;
  uint8_t read_buf[pkt_size];
  int n_read = 0;

  struct pollfd fds[2];
  fds[0].fd = m_service_sockets[1];
  fds[0].events = POLLIN;
  fds[1].fd = m_sockfd;
  fds[1].events = POLLIN;
    
  while (true) {
    int p = poll(fds, 2, -1);
    if (p > 0) {

      // Signal from the main thread to stop
      if (fds[0].revents & POLLIN) {
	break;
      }

      // Data from hardware
      if (fds[1].revents & POLLIN) {
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
	  break;
	} else {
	  n_read += n;
	  if (n_read == 4) {
	    n_read = 0;
	    uint32_t w =
	      ( ((uint32_t)(read_buf[3]) & 0xFF)       ) |
	      ( ((uint32_t)(read_buf[2]) & 0xFF) <<  8 ) |
	      ( ((uint32_t)(read_buf[1]) & 0xFF) << 16 ) |
	      ( ((uint32_t)(read_buf[0]) & 0xFF) << 24 );
	    //std::cout << "==> REQ: " << std::hex << w << std::endl;
	    dispatchWord(w);
	  }
	}
      }
      
    } else if (p < 0) {
      std::cout << "poll() failed: " << strerror(errno) << std::endl;
      exit(1);
      //throw (std::string("poll failed: " + std::string(strerror(errno))));
    }
  } // while (true)
}

void LuminaPlusControl::write_word(uint32_t w)
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

void LuminaPlusControl::dispatchWord(uint32_t w)
{
  uint32_t tag = (w >> 31);
  uint32_t msg = (w & 0x7FFFFFFF);

  if (tag) {
    // Status
    //std::cout << "Received status: " << std::hex << tag << ", " << msg << std::endl;
    if (m_status_words.size() < 2) {
      m_status_words.push_back(msg);
    } else {
      RdBackStatus s;
      // word 0
      uint32_t tmp = m_status_words.front();
      m_status_words.pop_front();
      s.rdback_on = (tmp >> 30);
      s.cycle = ((uint64_t)(tmp & 0x3FFFFFFF)) << 30;
      // word 1
      tmp = m_status_words.front();
      m_status_words.pop_front();
      s.cycle |= (uint64_t)(tmp & 0x3FFFFFFF);
      // word 2
      s.running = (msg >> 30);
      s.free_running = (msg >> 29) & 0x1;
      s.edges = (msg & 0x1FFFFFFF);
      // queue the result
      putStatus(s);
    }
  } else {
    // Data
    //std::cout << "Received data: " << std::hex << tag << ", " << msg << std::endl;
    m_rdbackCallback(m_rdbackCallbackParm, msg);
  }
}

void LuminaPlusControl::putStatus(RdBackStatus s)
{
  pthread_mutex_lock(&m_status_queue_lock);

  m_status_queue.push_back(s);

  pthread_cond_signal(&m_status_queue_cond);
  pthread_mutex_unlock(&m_status_queue_lock);
}

bool LuminaPlusControl::sendEmuEdges (unsigned int count)
{
  if (m_sockfd < 0) return false;

  unsigned int code = count & 0x1FFFFFFF;
  RdBackControlReq req(Edges, code);
  write_word( req.getBits() );
  return true;
}

bool LuminaPlusControl::sendEmuQuery ()
{
  if (m_sockfd < 0) return false;

  unsigned int code = 0;
  RdBackControlReq req(Query, code);
  write_word( req.getBits() );
  return true;
}

bool LuminaPlusControl::sendEmuStop ()
{
  if (m_sockfd < 0) return false;

  unsigned int code = 0;
  RdBackControlReq req(Stop, code);
  write_word( req.getBits() );
  return true;
}

bool LuminaPlusControl::sendEmuResume ()
{
  if (m_sockfd < 0) return false;

  unsigned int code = 0;
  RdBackControlReq req(Resume, code);
  write_word( req.getBits() );
  return true;
}

bool LuminaPlusControl::sendRdBackOn ()
{
  if (m_sockfd < 0) return false;

  unsigned int code = 0;
  RdBackControlReq req(RdBackOn, code);
  write_word( req.getBits() );
  return true;
}

bool LuminaPlusControl::sendRdBackOff ()
{
  if (m_sockfd < 0) return false;

  unsigned int code = 0;
  RdBackControlReq req(RdBackOff, code);
  write_word( req.getBits() );
  return true;
}

RdBackStatus LuminaPlusControl::getStatusBlocking()
{
  // grab the lock
  pthread_mutex_lock(&m_status_queue_lock);

  // is there data?
  while (m_status_queue.empty()) {
    // this wait will release the lock atomically with the wait
    pthread_cond_wait(&m_status_queue_cond, &m_status_queue_lock);
    // and will get back the lock when the wait is over
  }

  RdBackStatus res = m_status_queue.front();
  m_status_queue.pop_front();

  // release the lock
  pthread_mutex_unlock(&m_status_queue_lock);

  return res;
}

bool LuminaPlusControl::getStatusNonBlocking(RdBackStatus &s)
{
  bool res = false;

  // grab the lock
  pthread_mutex_lock(&m_status_queue_lock);

  // is data available
  if (!m_status_queue.empty()) {
    s = m_status_queue.front();
    m_status_queue.pop_front();
    res = true;
  }

  // release the lock
  pthread_mutex_unlock(&m_status_queue_lock);

  return res;
}

// Time is specified as a delta-time relative to current time
//   delta_seconds -- timeout delay in seconds
//   delta_microseconds -- delay in micro-seconds
//
bool LuminaPlusControl::getStatusTimed(RdBackStatus &s, const time_t & delta_seconds, const long & delta_microseconds)
{
  struct timespec ts;
  setTimeout(ts, delta_seconds, delta_microseconds);
  return getStatusTimed (s, &ts);
}

// Time is specified as an absolute time
//
bool LuminaPlusControl::getStatusTimed(RdBackStatus &s, struct timespec *expiration)
{
  bool res = false;
  int status;

  // grab the lock
  pthread_mutex_lock(&m_status_queue_lock);

  // is data available
  while (m_status_queue.empty()) {
    // this wait will release the lock atomically with the wait
    status = pthread_cond_timedwait(&m_status_queue_cond, &m_status_queue_lock, expiration);
    // and will get back the lock when the wait is over
    if (status == ETIMEDOUT) {
      break;
    }
  }

  // Even if the timer timed out, it's still possible
  // that data arrived at the last moment
  //
  if (!m_status_queue.empty()) {
    s = m_status_queue.front();
    m_status_queue.pop_front();
    res = true;
  }

  // release the lock
  pthread_mutex_unlock(&m_status_queue_lock);

  return res;
}

bool LuminaPlusControl::sendRdBackClear ()
{
  if (m_sockfd < 0) return false;

  unsigned int code = 0;
  RdBackControlReq req(RdBackCmd, code);
  write_word( req.getBits() );
  return true;
}

bool LuminaPlusControl::sendRdBackStore (unsigned int code)
{
  if (m_sockfd < 0) return false;

  RdBackControlReq req(RdBackStore, code);
  write_word( req.getBits() );
  return true;
}

bool LuminaPlusControl::sendRdBackFinish (unsigned int config)
{
  if (m_sockfd < 0) return false;

  unsigned int code = (config & 0x7FFFFFF) | (1<<27);
  RdBackControlReq req(RdBackCmd, code);
  write_word( req.getBits() );
  return true;
}

bool LuminaPlusControl::sendRdBackBreakCode (unsigned int config)
{
  if (m_sockfd < 0) return false;

  unsigned int code = (config & 0xFFFF) | (1<<28);
  RdBackControlReq req(RdBackCmd, code);
  write_word( req.getBits() );
  return true;
}

// ================================================================
