/* Copyright (c) 2011 Bluespec, Inc.  All rights reserved. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

#include "SceMiParameters.h"
#include "SceMiEC.h"
#include "LinkPlugin.h"
#include "BlueNoCLink.h"

// Plugin C-linkage functions

Link* create_link(SceMiParameters* parameters,
                  SceMiErrorHandler hdlr, void* ctx)
{
  if (parameters == NULL) return NULL;

  ec_register_handler(hdlr, ctx);

  const char* dev_file  = parameters->AttributeStringValue("Link",0,"DeviceName");
  if (dev_file == NULL) {
    fprintf (stderr, "Error: create_link() device file name is not provided in the parameter file.\n");
    exit(EXIT_FAILURE);
    return NULL;
  }

  // Open the device
  int dev_fd = open(dev_file, O_RDWR);
  if (dev_fd < 0) {
        fprintf(stderr, "Error: Failed to open BlueNoC device file %s: %s\n", dev_file, strerror(errno));
        exit(EXIT_FAILURE);
  }

  return new BlueNoCLink(dev_fd, parameters, true);
}

void destroy_link(Link* link)
{
  delete link;
}
