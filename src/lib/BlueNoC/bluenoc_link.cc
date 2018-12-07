#include <sys/ioctl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/mman.h>

#include "sized_types.h"
#include "bluenoc_link.h"


bluenoc_link::bluenoc_link(const char *logEnvir)
  : m_logTraffic(false)
  , m_logFile(0)
{
  // Open trace file for logging all traffic
  char *outfile = getenv(logEnvir);
  if (outfile) {
    m_logFile = fopen(outfile, "w");
    if (!m_logFile) {
      fprintf(stderr, "Could not open LINK trace file %s\n", outfile);
    }
    else {
      fprintf(stderr, "Opened LINK trace file %s\n", outfile);
      m_logTraffic = true;
      fprintf (m_logFile, "Opened LINK trace file %s\n", outfile);
      fflush(m_logFile);
    }
  }
}

bluenoc_link::~bluenoc_link()
{
  if (m_logTraffic) {
    fprintf (m_logFile, "Closing LINK trace file\n");
    fclose(m_logFile);
    m_logTraffic = false;
  }
}

void bluenoc_link::reportin ( const Packet *pkt)
{
  fprintf (m_logFile,"I%d", pkt->nodeid);
  for (uint32_t i=bits_to_words(pkt->num_bits); i != 0 ; --i) {
    fprintf (m_logFile, " %08x", pkt->data[i-1]);
  }
  fprintf (m_logFile, "\n");
  fflush(m_logFile);
}

void bluenoc_link::reportout (const Packet *pkt)
{
  fprintf (m_logFile,"O%d", pkt->nodeid);
  for (uint32_t i=bits_to_words(pkt->num_bits); i != 0 ; --i) {
    fprintf (m_logFile, " %08x", pkt->data[i-1]);
  }
  fprintf (m_logFile, "\n");
  fflush(m_logFile);
}
