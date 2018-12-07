//-*- C++ -*-x
// Copyright (c) 2011 - 2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <stdexcept>
#include <cassert>
#include <ctime>
#include <cerrno>
#include <iostream>

#include <stdint.h>
#include <stdbool.h>
#include <sys/time.h>
#include <pthread.h>

#include "XactorAdapter.h"

/// Base class for all transactors.
/// This is the base class for the transactor.  It provides a common
/// inteface between SceMi transport layers, as well as several common methods
/// for any derived classes.  Data transfers are accomplished in generic data
/// of the MsgPacket class.
/// This is a full duplex transactors there are channels for both send and recevied
class XactorCore {
public:
  // type definition for call back function
  typedef void (*XactorCallBack) (void *context);

  ///
  ///  Constructor for SceMi based client.
  /// \param name -- symbolic name
  /// \param path -- scemi path, in params file
  XactorCore (const std::string &name, const std::string &path);
  XactorCore (const std::string &name, const std::string &path, XactorAdapter::SInpipe type);
  XactorCore (const std::string &name, const std::string &path, XactorAdapter::SOutpipe type);
  XactorCore (const std::string &name, const std::string &path, XactorAdapter::SInport type);
  XactorCore (const std::string &name, const std::string &path, XactorAdapter::SOutport type);
  XactorCore (const std::string &name, const std::string &path, XactorAdapter::SPipes type);
  XactorCore (const std::string &name, const std::string &path, XactorAdapter::SPorts type);

private:
  // Disallow copy and assignment
  XactorCore (const XactorCore &);
  XactorCore& operator= (const XactorCore &);

public:
  ///
  ///  Destructor
  ///
  virtual ~XactorCore ();

  // Status indicators.
  ///  Indicates if a message can be sent.
  /// \return true if the xactor can send a message
  virtual bool canSend () const;

  // these are all thread-safe and can be used intermixed

  ///  Blocking send method.
  /// This method blocks until the MsgPacket has been 
  /// successfully passed to the xactor
  /// \param p -- the MsgPacket to be sent
  void send (const MsgPacket &p);

  ///  Non-blocking send.
  /// This method sends the MsgPacket if the xactor can accept it.
  /// \param p -- the MsgPacket to be sent
  /// \return  returns true if the MsgPacket has been sent
  bool sendNB (const MsgPacket &p);

  ///  Timed-blocking send.
  /// This method tries to send the MsgPacket but will timeout if the
  /// packet has not been sent within the timeout period.
  /// \param p -- the MsgPacket to be sent
  /// \param seconds -- timeout delay in seconds
  /// \param microseconds -- delay in micro-seconds
  /// \return  returns true if the MsgPacket has been sent, false if
  /// a timeout occurred.
  bool sendT (const MsgPacket &p, const time_t seconds, const long microseconds = 0);

  ///  Timed-blocking send.
  /// This method tries to send the MsgPacket but will timeout if the
  /// packet has not been sent within delta time.
  /// \param p -- the MsgPacket to be sent
  /// \param expiration -- absolute time for the timeout
  /// \return  returns true if the MsgPacket has been sent, false if
  /// a timeout occurred.
  bool sendT (const MsgPacket &p, struct timespec &expiration);
  bool sendT (const MsgPacket &p, struct timespec *expiration);

  /// Wait for send Acknowledge
  /// Blocks until all send data has been received at the dut.
  void waitSendAck();

  ///  Blocking receive.
  /// This method blocks until a MsgPacket is received from the xactor.
  /// Exactly psize 32-bit words of data will be available in the packet
  /// \param p -- the MsgPacket to be received
  /// \param psize -- the desired size of the packet.
  void receive (MsgPacket &p, unsigned psize);

  ///  Non-Blocking receive.
  /// This method checks if a MsgPacket is available from the xactor
  /// and returns it if it is.
  /// Exactly psize 32-bit words of data will be available in the packet
  /// \param p -- the MsgPacket to be received
  /// \param psize -- the desired size of the packet.
  bool receiveNB (MsgPacket &p, unsigned psize);

  ///  Timed-blocking receive.
  /// This method attempts to retrieve a message from the xactor,
  /// blocking until the message is received or the timeout period 
  /// elapses.  Time is specified as an absolute.
  /// Exactly psize 32-bit words of data will be available in the packet
  ///
  /// \param p -- the MsgPacket to be received
  /// \param psize -- the desired size of the packet.
  /// \param expiration -- absolute time for the timeout
  /// \return   returns true if the MsgPacket has been received, false if
  /// a timeout occurred.
  bool receiveT (MsgPacket &p, unsigned psize, struct timespec &expiration);
  bool receiveT (MsgPacket &p, unsigned psize, struct timespec *expiration);

  ///  Timed-blocking receive.
  /// This method attempts to retrieve a message from the xactor,
  /// blocking until the message is received or the timeout period 
  /// elapses.  Time is specified as a delta-time relative to current time.
  /// Exactly psize 32-bit words of data will be available in the packet
  ///
  /// \param p -- the MsgPacket to be received
  /// \param psize -- the desired size of the packet.
  /// \param seconds -- timeout delay in seconds
  /// \param microseconds -- timeout delay in microseconds
  /// \return   returns true if the MsgPacket has been received, false if
  /// a timeout occurred.
  bool receiveT (MsgPacket &p, unsigned psize, uint32_t seconds, uint32_t microseconds = 0);

  // Peeking at data without consumption

  ///  Blocking peek method.
  /// Peek at the data available from the transactor without consumption.
  /// This is blocking until all words are availablee.
  /// Exactly psize 32-bit words of data will be available in the packet
  /// \param p -- the MsgPacket to be received
  /// \param psize -- the desired size of the packet.
  bool peek (MsgPacket &p, unsigned psize) const;

  /// Non-Blocking peek method
  /// Peek at the data available from the transactor without consumption.
  /// This returns immediately.
  /// Exactly 32-bit words of data will be available in the packet.
  /// \param p -- the MsgPacket to be received
  /// \param psize -- the desired size of the packet.
  /// \return true if data is available
  bool peekNB (MsgPacket &p, unsigned psize) const;

  ///  Timed peek function.
  /// This method attempts to retrieve a message from the xactor,
  /// blocking until the message is received or the timeout expires.
  /// Time is specified as an absolute
  /// Exactly psize 32-bit words of data will be available in the
  /// packet
  ///
  /// \param p -- the MsgPacket to be received
  /// \param psize -- the desired size of the packet.
  /// \param expiration -- absolute time for the timeout
  /// \return  returns true if the MsgPacket has been received,  false if
  /// a timeout occurred.
  bool peekT (MsgPacket &p, unsigned psize, struct timespec &expiration) const;
  bool peekT (MsgPacket &p, unsigned psize, struct timespec *expiration) const;

  ///  Timed peek function.
  /// This method attempts to retrieve a message from the xactor,
  /// blocking until the message is received or the timeout expires.
  /// Time is specified as a delta-time relative to current time
  /// Exactly psize 32-bit words of data will be available in the
  /// packet
  ///
  /// \param p -- the MsgPacket to be received
  /// \param psize -- the desired size of the packet.
  /// \param seconds -- timeout delay in seconds.
  /// \param microseconds -- delay in micro-seconds
  /// \return  returns true if the MsgPacket has been received,  false if
  /// a timeout occurred.
  bool peekT (MsgPacket &p, unsigned psize, uint32_t seconds, uint32_t microseconds = 0) const;

  // Call Back Interface

  /// Set call-back function when data can be sent by the xactor
  ///
  /// Set the call-back notify function to be called when data can
  /// be sent by the xactor.  This is not required to be set and may
  /// be set to NULL.
  ///
  /// \param cbfunc -- a pointer to the call-back function
  /// \param context -- user context used by cbfunc when called
  void setCanSendNotifyCallBack (XactorCallBack cbfunc, void *context);

  /// Set call-back function twhen data has been received by xactor.
  ///
  /// Set the call-back notify function to be called when data has
  /// been received by the xactor.  Note the call-back function is called
  /// whenever one ore more new words are available.  Multiple calls may
  /// occur if a large packet is transmitted.
  /// This is not required to be set and may be set to NULL.
  ///
  /// \param cbfunc -- a pointer to the call-back function
  /// \param context -- user context used by cbfunc when called
  void setCanRecvNotifyCallBack(XactorCallBack cbfunc, void *context);

  // Debug And Trace

  /// Enable trace data for the interface
  /// \param mask -- a bit mask to enable various modes,
  ///                1 -- trace data flow to xactors
  ///                2 -- trace data mutex locks
  ///                4 -- trace interrupt behavior
  ///                8 -- trace command mutex locks
  /// \return previous mask status
  uint32_t setAdapterDebug(uint32_t mask);

  /// \brief return the name as assigned by the user
  const char *getNameStr() const;
  
  /// \brief return the name as assigned by the user
  const std::string &getName() const;

  /// Displays internal state for this xactor
  void debug() const;

  /// Displays internal state for all xactors
  static void xdebug();

  /// True if the transactor was opened successfully
  bool isOpen() { return m_padapter->isOpen(); }

protected:

  ///  Indicates how much data is able to be received.
  /// Indicates how many 8-bit data words are available within
  /// the host side of this transactor.
  /// \param req -- the number of 8-bit words requested
  /// \return the number of 8-bit words which can be received
  unsigned canReceive (const unsigned req) const;

  /// wrapper function to execute the can send call back
  void executeSendCallBack();

  /// wrapper function to execute the can receive call back
  void executeRecvCallBack();

  /// Accepts pack for sending to client hardware
  /// \param p -- the packet to send
  /// \return true if the packet was sent.
  bool doSend (const MsgPacket &p);

  /// Take psize words from the receive Q returning them in MsgPacket p.
  /// This operation is atomic either all maxread words are transferred or none
  /// \param p -- the packet where the data is placed
  /// \param psize - the number of words to take from the receive Q
  /// \return true if the packet has been populated
  bool doReceive (MsgPacket &p, unsigned psize);

  /// Peek at psize words from the receive Q returning them in MsgPacket p.
  /// This operation is atomic either all maxread words are copied or none
  /// \param p -- the packet where the data is placed
  /// \param psize -- the number of words to take from the receive Q
  /// \return true if the packet has been populated
  bool doPeek (MsgPacket &p, unsigned psize) const;

  /// Utility function useful as a callback
  /// \param pv -- the context really the class object
  static void signalCanSend (void *pv);
  /// member function to call indicating that data can be sent to the xactor
  void signalCanSend ();

  /// Utility function useful as a callback
  /// \param pv -- the context really the class object
  static void signalCanReceive (void *pv);
  /// member function to call indicating that new data is available from the client
  void signalCanReceive ();

  /// Utility function to convert a delta time to absolute time
  /// The delta time is given 2 part, seconds and microseconds, where the total time is 
  /// the sum of the two parts.
  /// \param ts -- timespec update to absolute time
  /// \param delta_seconds --
  /// \param delta_microseconds --
  static void setTimeout (struct timespec &ts, const time_t delta_seconds, const long delta_microseconds);

private:
  /// A holder for the adapter class
  XactorAdapter *m_padapter;

  /// Callback function for canSend
  XactorCallBack m_sendCallBack;
  /// Callback function for canReceive
  XactorCallBack m_recvCallBack;
  /// Callback context for canSend
  void *m_sendCBContext;
  /// Callback context for canReceive
  void *m_recvCBContext;  
};
