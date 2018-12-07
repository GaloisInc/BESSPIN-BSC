/* Copyright (c) 2011 Bluespec, Inc.  All rights reserved. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

#include "SceMiParameters.h"
#include "SceMiEC.h"
#include "LinkPlugin.h"
#include "BlueNoCLink.h"
#include "bluenoc_tcp.h"

// Plugin C-linkage functions

Link* create_link(SceMiParameters* parameters,
                  SceMiErrorHandler hdlr, void* ctx)
{
  if (parameters == NULL) return NULL;

  ec_register_handler(hdlr, ctx);

  // get the address and port information from the SCE-MI parameters

  const char* addr  = parameters->AttributeStringValue("Link",0,"TCPAddress");
  if (addr == NULL)
    addr = "127.0.0.1";
  unsigned int port = parameters->AttributeIntegerValue("Link",0,"TCPPort");

  // create the TCP connection
  int sock = connect_to_bluenoc_tcp_bridge(addr,port);
  if (sock == -1) {
    fprintf(stderr, "Error: connect_to_bluenoc_tcp_bridge() failed\n");
    exit(EXIT_FAILURE);
        return NULL;
  }

  return new BlueNoCLink(sock, parameters, false, disconnect_from_bluenoc_tcp_bridge);
}

void destroy_link(Link* link)
{
  delete link;
}
