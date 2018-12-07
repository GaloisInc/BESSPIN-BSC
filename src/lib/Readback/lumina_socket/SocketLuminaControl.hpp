// Copyright (c) 2013-2016, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// C++ transactor side for a Lumina control module accessed via socket

#include "LuminaControl.hpp"

class SocketLuminaControl : public LuminaControl
{
private:
  int m_sockfd;

public:
  SocketLuminaControl(const unsigned int port);
  ~SocketLuminaControl();

private:
  // Disallow default and copy constructors
  SocketLuminaControl & operator= (const SocketLuminaControl &);
  SocketLuminaControl( const SocketLuminaControl &);

  void write_word(uint32_t w);
  uint32_t read_word(void);

public:
  bool readState();

  bool sendRdBackClear();
  bool sendRdBackStore (unsigned int code);
  bool sendRdBackFinish(unsigned int config);
  bool sendRdBackBreakCode(unsigned int code);
};
