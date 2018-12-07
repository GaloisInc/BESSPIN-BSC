//-*- C++ -*-x
// Copyright (c) 2011 - 2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <cassert>
#include <string>
#include <sstream>
#include <stdexcept>

#include "FastQueue.h"
#include "XactorAdapter.h"

/// SceMi Adapter
/// This is a full duplex transactors there are channels for both send and recevied
class SceMiAdapter : public XactorAdapter
{
public:
  typedef void (*SceMiAdapterCallBack) (void *context);

private:
 /// SceMi handle for the put side, i.e., data to the client hardware
  void      * m_putHandle;
  /// SceMi handle for the get side, i.e., data from the client hardware
  void      * m_getHandle;

  /// SceMi Handle for the notify callback
  void         *m_get_callback_handle;
  void         *m_put_callback_handle;

  /// A queue for send data
  FastQueue     m_sendDataQ;
  uint32_t      m_sendDataQLimit;
  /// A queue for recv data
  FastQueue     m_recvDataQ;
  mutable uint32_t      m_recvDataQLimit;

  /// Pipe parameters
  uint32_t      m_putPipeDepth;
  uint32_t      m_putPipeWidth;
  uint32_t      m_getPipeDepth;
  uint32_t      m_getPipeWidth;

  /// Lock on scemi commands.
  static pthread_mutex_t s_cmd_mutex;

public:
  /// Constructor for SceMi based client.
  /// \param name -- symbolic name
  /// \param path -- scemi path, in params file
  SceMiAdapter(const std::string &name, const std::string &path);
  SceMiAdapter(const std::string &name, const std::string &path, SInpipe type);
  SceMiAdapter(const std::string &name, const std::string &path, SOutpipe type);
  SceMiAdapter(const std::string &name, const std::string &path, SInport type);
  SceMiAdapter(const std::string &name, const std::string &path, SOutport type);

  /// Destructor
  ~SceMiAdapter();

  /// Print data on the state of the adapter
  /// Specifically,
  /// - name
  /// - canSend -- canSend status
  /// - SendQ   -- number of words in the software buffer
  /// - canRecv -- number of words available from client
  /// - RecvQ   -- number of words in software receive queue
  virtual void debug() const;

private:
  // Disable copy and assignment
  SceMiAdapter(const SceMiAdapter &);
  SceMiAdapter & operator=(const SceMiAdapter &);

  /// Utility function to lock access to the SceMi commands
  void lockCmds(const char *);
  /// Utility function to unlock access to the SceMi commands
  void unlockCmds(const char *);

public:
  /// indicator that the adapter can accept more data, i.e. software queue size is less than
  /// limit
  /// \return true if the adapter can send a packet
  virtual bool canSend() const;

  /// indicator that all data is sent, and the all credits are in place
  virtual bool sendAcknowledged();

  /// Accepts pack for sending to client hardware.
  /// \param p -- the packet to send
  /// \return true if the packet was sent.
  virtual bool doSend(const MsgPacket &p);

  /// Indicates if data is available
  /// \param req -- the number of 8-bit words requested
  /// \return the number of 8-bit words available from client
  virtual unsigned canReceive(const unsigned req) const;

  /// Take maxread words from the receive Q returning them in Packet p.
  /// This operation is atomic either all maxread words are transferred or none
  /// \param p -- the packet where the data is placed
  /// \param maxread -- the number of bytes to take from the receive Q
  /// \return true if the packet has been populated
  virtual bool doRecv (MsgPacket &p, unsigned maxread);

  /// Peek at maxread words from the receive Q returning them in Packet p.
  /// This operation is atomic either all maxread words are copied or none
  /// \param p -- the packet where the data is placed
  /// \param maxread -- the number of bytes to take from the receive Q
  /// \return true if the packet has been populated
  virtual bool doPeek (MsgPacket &p, unsigned maxread) const;

  virtual void flushSend();


private:
  /// Class call-back function; this is the call-back function set as
  /// the scemi can send notify call back
  static void canSendCB (void *pt) ;
  /// Class call-back function; this is the call-back function set as
  /// the scemi can send notify call back
  void canSendCB();

  /// Class call-back function; this is the call-back function set as
  /// the scemi can receive notify call back
  static void canRecvCB (void *pt) ;
  /// Class call-back function; this is the call-back function set as
  /// the scemi can receive notify call back
  void canRecvCB();
  /// worker function for pulling data from pipe
  bool receivePull();
};
