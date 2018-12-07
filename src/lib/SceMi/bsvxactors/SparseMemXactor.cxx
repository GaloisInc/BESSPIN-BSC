// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED

#include "SparseMemXactor.h"

// The static member pointer to the SparseMemXactor:
static SparseMemXactor *theSparseMemXactor = NULL;

// init()
SparseMemXactor *SparseMemXactor::init(bool debug,
				       const std::string & hier, const std::string & instname,
				       SceMi *scemi)
{
  SceMi *sceMi;

  if (theSparseMemXactor)
    return theSparseMemXactor;

  if (scemi == NULL) {
    sceMi = SceMi::Pointer();
    if (sceMi == NULL) {
      std::cerr << "SparseMemXactor Error: SceMi must be intialized before calling init()." << std::endl;
      return NULL;
    }
  } else
    sceMi = scemi;

  theSparseMemXactor = new SparseMemXactor(debug, hier, instname, sceMi);

  return theSparseMemXactor;
}

SparseMemXactor *SparseMemXactor::init(const std::string & hier, const std::string & instname,
				       SceMi *scemi)
{
  return init(false, hier, instname, scemi);
}

// Shutdown the transactor
void SparseMemXactor::shutdown()
{
  delete theSparseMemXactor;

  theSparseMemXactor = NULL;
}


// Constructor
SparseMemXactor::SparseMemXactor(bool debug,
				 const std::string & hier, const std::string & instname,
				 SceMi *scemi)
  : m_request(hier, instname + "_req", scemi)
  , m_response(hier, instname + "_resp", scemi)
  , m_debug(debug)
{
  hash_init();

  // Set callback for watching probe message
  m_request.setCallBack(SparseMemXactor::SreceiveCallBackT, (void*)this);
}

// Destructor (private)
SparseMemXactor::~SparseMemXactor()
{
}

// This call back is executed when the scemi transactor has data.
void SparseMemXactor::receiveCallBack(const local_RAM_Request_Bit_31_Bit_64 &req) {
  if (m_debug) { std::cout << "DEBUG: req received: " << req; }
  if (req.m_read.get()) {
    MemData xull = hash_read(req.m_address.get64());
    SceMiMemData x = xull;
    x.setWord(1, static_cast<SceMiU32>(xull >> 32));
    if (m_debug) { std::cout << "; sent: " << x.get64() << std::endl; }
    m_response.sendMessage(x);
      /*
      if (!m_response.sendMessageNonBlocking(x)) {
	std::cout << "SCEMI ERROR: sparse memory response blocked for "
		  << req << std::endl;
      }
      */
    }
    else {
      if (m_debug) { std::cout << std::endl; }
      hash_write(req.m_address.get64(), req.m_data.get64(), 0);
    }
  }
