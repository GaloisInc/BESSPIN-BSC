#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <cerrno>

#include "bluenoc_parameters.h"
#include "bluenoc_linkplugin.h"
#include "core_link.h"
#include "bluenoc_tcp.h"

bluenoc_link *create_link(bluenoc_parameters *parameters, void *ctx)
{
  if (parameters == NULL) return NULL;

  const char *addr = parameters->AttributeStringValue("Link", 0, "TCPAddress");
  if (addr == NULL) addr = "127.0.0.1";
  unsigned int port = parameters->AttributeIntegerValue("Link", 0, "TCPPort");

  int sock = connect_to_bluenoc_tcp_bridge(addr, port);
  if (sock == -1) {
    fprintf(stderr, "Error: connect_to_bluenoc_tcp_bridge() failed!\n");
    exit(EXIT_FAILURE);
    return NULL;
  }

  return new core_link(sock, parameters, false, disconnect_from_bluenoc_tcp_bridge);
}

void destroy_link(bluenoc_link *link)
{
  delete link;
}
