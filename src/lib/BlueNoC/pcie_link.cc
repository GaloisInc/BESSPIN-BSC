#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <cerrno>
#include <sys/stat.h>
#include <fcntl.h>
#include <cstring>

#include "bluenoc_parameters.h"
#include "bluenoc_linkplugin.h"
#include "core_link.h"

bluenoc_link *create_link(bluenoc_parameters *parameters, void *ctx)
{
  if (parameters == NULL) return NULL;

  const char *dev_file = parameters->AttributeStringValue("Link", 0, "DeviceName");
  if (dev_file == NULL) {
    fprintf(stderr, "Error: create_link() device file name is not provided in the parameter file.\n");
    exit(EXIT_FAILURE);
    return NULL;
  }

  int dev_fd = open(dev_file, O_RDWR);
  if (dev_fd < 0) {
    fprintf(stderr, "Error: Failed to open BlueNoC device file %s: %s\n", dev_file, strerror(errno));
    exit(EXIT_FAILURE);
  }

  return new core_link(dev_fd, parameters, true);
}

void destroy_link(bluenoc_link *link)
{
  delete link;
}
