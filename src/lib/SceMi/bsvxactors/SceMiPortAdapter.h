//-*- C++ -*-x
// Copyright (c) 2011 - 2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include "XactorAdapter.h"
#include "InportProxyT.h"
#include "OutportProxyT.h"
#include "BitT.h"

///  SceMi Interface -- internal use.
///  This class is not intended for use outside of the XactorCore class.
/// This is a wrapper/adapter class over a pair of SceMi handle which
///  make up a SceMi-based transactor.
class SceMiPortAdapter : public XactorAdapter {
public:
  typedef void (*SceMiPortAdapterCallBack) (void *context);

private:
  /// SceMi Inport Port Proxy for the put side, i.e., data to the client hardware
  SceMiMessageInPortProxy * m_pinport;
  /// SceMi handle for the get side, i.e., data from the client hardware
  SceMiMessageOutPortProxy * m_poutport;

  /// A queue for send data
  FastQueue     m_sendDataQ;
  /// A queue for recv data
  FastQueue     m_recvDataQ;


  /// Use for local error checking, delete after testing complted.
  bool m_inRCB;                   // in receive call back  for DEBUG
  bool m_readyToSend;

  /// Lock on scemi commands, this may not be needed!
  static pthread_mutex_t s_cmd_mutex ;

public:
  ///  Constructor for SceMi based client.
  /// \param name -- symbolic name
  /// \param path -- scemi path, in params file
  SceMiPortAdapter (const std::string & name, const std::string & path);
  ///  Constructor for SceMi based client.
  /// \param name -- symbolic name
  /// \param path -- scemi path, in params file
  SceMiPortAdapter (const std::string & name, const std::string & path, XactorAdapter::SInport);
  ///  Constructor for SceMi based client.
  /// \param name -- symbolic name
  /// \param path -- scemi path, in params file
  SceMiPortAdapter (const std::string & name, const std::string & path, XactorAdapter::SOutport);

  /// Destructor
  ~SceMiPortAdapter();

  /// Print data on the state of the adapter.
  /// Specially,
  /// - name
  /// - canSend -- canSend status
  /// - SendQ -- number of words in the software buffer
  /// - canRecv --number of words available from client
  /// - RecvQ -- number of words in software receive queue.
  virtual void debug() const;

private:
  /// Disable copy constructor
  SceMiPortAdapter (const SceMiPortAdapter &);
  SceMiPortAdapter & operator=(const SceMiPortAdapter &);
public:

  /// indicator that the adapater can accept more data, IE software queue size is less than limit
  /// \return true if the adapter can send a packet
  virtual bool   canSend() const;

  virtual bool sendAcknowledged();

  /// Accepts pack for sending to client hardware.
  /// \param p -- the packet to send
  /// \return true if the packet was sent.
  virtual bool doSend (const class MsgPacket & p);

  /// Indicates if data is available
  /// \param req -- the number of 8-bit words requested
  /// \return the number of 32-bit words available from client
  virtual unsigned canReceive(const unsigned req) const;

  /// Take maxread words from the receive Q returning them in Packet p.
  /// This operation is atomic either all maxread words are transferred or none
  /// \param p -- the packet where the data is placed
  /// \param maxread -- the number of word to take from the receive Q
  /// \return true if the packet has been populated
  virtual bool doRecv (MsgPacket &p, unsigned maxread);

  /// Peek at maxread words from the receive Q returning them in Packet p.
  /// This operation is atomic either all maxread words are copied or none
  /// \param p -- the packet where the data is placed
  /// \param maxread -- the number of word to take from the receive Q
  /// \return true if the packet has been populated
  virtual bool doPeek (MsgPacket &p, unsigned maxread) const;

private:

  /// Initialization help function
  void initInPort (const std::string &name, const std::string &inPipeName, SceMi *scemi );
  /// Initialization help function
  void initOutPort(const std::string &name, const std::string &outPipeName, SceMi *scemi );

  /// Close call back function for inport
  static void closeInport (void *pt);
  /// Close call back function for inport
  void closeInport();

  /// Close call back function for outport
  static void closeOutport (void *pt);
  /// Close call back function for outport
  void closeOutport();
 
  /// Class call-back function; this is the call-back function set as
  /// the scemi can send notify call back
  static void canSendCB (void *pt) ;
  /// Class call-back function; this is the call-back function set as
  /// the scemi can send notify call back
  void canSendCB();

  /// Class call-back function; this is the call-back function set as
  /// the scemi can receive notify call back
  static void canRecvCB (void *pt,  const SceMiMessageData *msg) ;
  /// Class call-back function; this is the call-back function set as
  /// the scemi can receive notify call back
  void canRecvCB( const SceMiMessageData *msg);

};
