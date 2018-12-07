/* Copyright (c) 2009 Bluespec, Inc.  All rights reserved. */

#include <sys/ioctl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/mman.h>

#include "sized_types.h"
#include "Link.h"

Link::Link(const char *logEnvir)
  : _logTraffic(false)
  , _logFile(0)
{
  // Open trace file for logging all pci traffic.   based on environment variable
  char * outfile = getenv (logEnvir);
  if (outfile) {
    _logFile = fopen (outfile, "w");
    if (_logFile == NULL) {
      fprintf(stderr, "Could not open LINK trace file %s\n", outfile);
    }
    else {
      fprintf(stderr, "Opened LINK trace file %s\n", outfile);
      _logTraffic = true;
      fprintf (_logFile, "Opened LINK trace file %s\n", outfile);
      fflush(_logFile);
    }
  }
}

Link::~Link() {
  if (_logTraffic) {
    fprintf (_logFile, "Closing LINK trace file\n");
    fclose (_logFile);
    _logTraffic = false;
  }
}

void Link::report (const Packet *pkt)
{
  fprintf (_logFile,"I%d", pkt->channel);
  for (unsigned int i=bits_to_words(pkt->num_bits); i != 0 ; --i) {
    fprintf (_logFile, " %08x", pkt->data[i-1]);
  }
  fprintf (_logFile, "\n");
  fflush(_logFile);
}

void Link::report (const SceMiU64& ts, const Packet *pkt)
{
  fprintf (_logFile,"O%d %llu", pkt->channel, ts);
  for (unsigned int i=bits_to_words(pkt->num_bits); i != 0 ; --i) {
    fprintf (_logFile, " %08x", pkt->data[i-1]);
  }
  fprintf (_logFile, "\n");
  fflush(_logFile);
}
