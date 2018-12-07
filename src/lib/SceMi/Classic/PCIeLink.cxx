/* Copyright (c) 2009 Bluespec, Inc.  All rights reserved. */

#include <sys/ioctl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/mman.h>
#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>

#include "PCIeLink.h"
#include "LinkPlugin.h"
#include "scemi.h"

#define DEBUG_LINK 0

#define BLUESPEC_ID_COOKIE 0x426c756573706563llu


// Plugin C-linkage functions
Link* create_link(SceMiParameters* parameters,
		  SceMiErrorHandler hdlr, void* ctx)
{
  if (parameters == NULL) return NULL;

  ec_register_handler(hdlr, ctx);

  const char* lt = parameters->AttributeStringValue("Link",0,"LinkType");
  if (lt == NULL) {
    fprintf (stderr, "Error: create_link() link type is not provided in the parameter file.\n");
    exit(EXIT_FAILURE);
    return NULL;
  }

  return new PCIeLink(lt, parameters);
}

void destroy_link(Link* link)
{
  delete link;
}

struct device_id
{
  uint16_t vendor;
  uint16_t device;
};

const struct device_id bsemu_id_table[] =
  {
    // Beta boards supporting SceMi 1.1 and no DMA
    { 0xB100, 0xB5CE, },
    { 0xB100, 0xB5C3, },

    // Dini based boards
    { 0x17DF, 0x1900, },

    // Bluespec Emulation boards
    { 0x1BE7, 0xB5CE, },
    { 0x1BE7, 0xB5C3, },

    {0},
  };

static int sysfs_readint(const char * path)
{
  int fd;
  char str[16];
  ssize_t ret;

  fd = open(path, O_RDONLY);
  if (fd < 0)
    return fd;

  ret = read(fd, str, sizeof(str));
  if (ret < 0)
    return static_cast<int>(ret);

  close(fd);

  return static_cast<int>(strtol(str, NULL, 0));
}

static int sysfs_find_devices(const struct device_id * id_table, char *** devices)
{
  char path_argv0[] = "/sys/devices";
  char * const path_argv[] = {path_argv0, NULL};

  FTS * fts = fts_open(path_argv, FTS_PHYSICAL | FTS_XDEV, NULL);
  if (fts == NULL)
    {
      // error
      return -1;
    }

  FTSENT * ftsent;
  int found = 0;

  *devices = NULL;

  while (true)
    {
      ftsent = fts_read(fts);
      if (ftsent == NULL)
	{
	  // todo done, or error
	  break;
	}

      if (ftsent->fts_info == FTS_D)
	{
	  char path[NAME_MAX];

	  strncpy(path, ftsent->fts_path, sizeof(path));
	  strncat(path, "/vendor", sizeof(path) - ftsent->fts_pathlen);
	  int vendor = sysfs_readint(path);

	  strncpy(path, ftsent->fts_path, sizeof(path));
	  strncat(path, "/device", sizeof(path) - ftsent->fts_pathlen);
	  int device = sysfs_readint(path);

	  if ((vendor > 0) && (device > 0))
	    {
	      for (const struct device_id *id = id_table; (id->vendor != 0) && (id->device != 0); id++)
		{
		  if (id->vendor == vendor && id->device == device)
		    {
		      found++;

		      void * ptr = realloc(*devices, sizeof(char*) * (found + 1));
		      if (ptr == NULL)
			{
			  // todo alloc failed, return
			}

		      *devices = (char**)ptr;

		      (*devices)[found-1] = (char*)malloc(ftsent->fts_pathlen+1);
		      strncpy((*devices)[found-1], ftsent->fts_path, ftsent->fts_pathlen+1);

		      (*devices)[found] = NULL;

		      break;
		    }
		}
	    }
	}
    }

  fts_close(fts);

  return found;
}

static int sysfs_find_free(char ** devices)
{
  if (devices != NULL)
    {
      for (char ** device = devices; *device != NULL; device++)
	{
	  free(*device);
	}
    }

  free(devices);

  return 0;
}

// Constructor
PCIeLink::PCIeLink(const char* lt,
		   SceMiParameters* parameters)
  :Link ("BSC_TRACE_SCEMI_PCIE")
  ,link_type(lt)
{
  int num_bits;
  int count;
  char ** devices;
  char devpath[NAME_MAX];

  count = sysfs_find_devices(bsemu_id_table, &devices);
  if (count <= 0)
    {
      fprintf(stderr, "Error: could not find PCIe device\n");
      exit(EXIT_FAILURE);
    }

  strncpy(devpath, devices[0], sizeof(devpath));
  sysfs_find_free(devices);

  // Setup mmap to PCIe regions provided by the device
  if (!strcmp(link_type, "PCIE_DINI") ||
      !strcmp(link_type, "PCIE_VIRTEX5") ||
      !strcmp(link_type, "PCIE_VIRTEX6") ||
      !strcmp(link_type, "PCIE_KINTEX7") ||
      !strcmp(link_type, "PCIE_VIRTEX7") ||
      !strcmp(link_type, "PCIE_VIRTEXU"))
  {
    int fd;
    char resourcepath[NAME_MAX];

    strncpy(resourcepath, devpath, sizeof(resourcepath));
    strncat(resourcepath, "/resource1", sizeof(resourcepath) - strlen(devpath));

    // Open the device
    fd = open(resourcepath, O_RDWR);
    if (fd < 0) {
      fprintf (stderr, "Error: Failed to open BAR1 resource %s: %s\n", resourcepath, strerror(errno));
      exit(EXIT_FAILURE);
    }

    pBar1 = (tBar1*) mmap(NULL, sizeof(tBar1), PROT_READ | PROT_WRITE,
			  MAP_SHARED, fd, 0);
    if (pBar1 == MAP_FAILED)
    {
      fprintf (stderr, "Error: mmap of BAR1: %s\n", strerror(errno));
      close(fd);
      exit(EXIT_FAILURE);
    }

    close(fd);


    strncpy(resourcepath, devpath, sizeof(resourcepath));
    strncat(resourcepath, "/resource2", sizeof(resourcepath) - strlen(devpath));

    // Open the device
    fd = open(resourcepath, O_RDWR);
    if (fd < 0) {
      fprintf (stderr, "Error: Failed to open BAR2 resource %s: %s\n", resourcepath, strerror(errno));
      exit(EXIT_FAILURE);
    }

    pBar2 = (tBar2*) mmap(NULL, sizeof(tBar2), PROT_READ | PROT_WRITE,
			  MAP_SHARED, fd, 0);
    if (pBar2 == MAP_FAILED)
    {
      fprintf (stderr, "Error: mmap of BAR2: %s\n", strerror(errno));
      close(fd);
      exit(EXIT_FAILURE);
    }

    close(fd);

    pInPorts = (tInPort*)&pBar2->InPorts;
    pOutPorts = (tOutPort*)&pBar2->OutPorts;

#if DEBUG_LINK
    printf("pBar1 = %p   pInPorts = %p   pOutPorts = %p\n", pBar1, pInPorts, pOutPorts);
    fflush(NULL);
#endif
    if (_logTraffic) fprintf(_logFile, "pBar1 = %p   pInPorts = %p   pOutPorts = %p\n", pBar1, pInPorts, pOutPorts);


    /* check for ID register on PCIE Bar1 */
    UInt64 id = (((UInt64) pBar1->bluespec_id_hi) << 32) | ((UInt64) pBar1->bluespec_id_lo);
    if (id != BLUESPEC_ID_COOKIE)
    {
      fprintf (stderr, "Error: ID register read returned %llx (expected %llx)\n", id, BLUESPEC_ID_COOKIE);
      exit(EXIT_FAILURE);
    }
  }
  else
  {
    fprintf (stderr, "Error: Unsupported PCIE link type: %s\n", link_type);
    exit(EXIT_FAILURE);
  }

  // Initialize last transmit channel for round robin
  last_tx = 0;

  // Number of output message ports
  number_outports = parameters->NumberOfObjects("MessageOutPort");

#if DEBUG_LINK
  printf("Initializing %d output ports.\n", number_outports);
#endif

  // port_width for each channel
  port_width = new unsigned int[number_outports];
  for (unsigned int i=0; i<number_outports; i++) {

    num_bits = parameters->AttributeIntegerValue("MessageOutPort", i, "PortWidth");
    unsigned int channel = parameters->AttributeIntegerValue("MessageOutPort", i, "ChannelId");
    port_width[channel] = num_bits;
    if (_logTraffic) {
      fprintf (_logFile,"Output port %d, channel %u is %d bits wide.\n", i, channel, num_bits);
    }
  }

  // determine the maximun channel id to setup the pending vector
  unsigned int inportCount = parameters->NumberOfObjects ("MessageInPort");
  int maxInportChannelId = 0;
  for (unsigned int i = 0; i < inportCount ; ++i) {
    int chId = parameters->AttributeIntegerValue("MessageInPort", i, "ChannelId");
    maxInportChannelId = std::max (maxInportChannelId, chId);
  }
  pending.resize(1+maxInportChannelId);

#if DEBUG_LINK
  fflush(NULL);
#endif
}

Packet* PCIeLink::packet(unsigned int len)
{
  Packet* pkt = new Packet;
  pkt->num_bits = len;
  pkt->data = new SceMiU32[bits_to_words(len)];
  return pkt;
}

void PCIeLink::loop()
{
}

void PCIeLink::queue(Packet* pkt)
{
  if (pkt->channel >= pending.size()) {
    fprintf(stderr, "incorrect pending size %d > %d\n", pkt->channel, (int) pending.size());
    exit( EXIT_FAILURE);
  }
  if (pending[pkt->channel] == NULL)
  {
    pending[pkt->channel] = pkt;
#if DEBUG_LINK
    printf("Queued packet on channel %d\n", pkt->channel);
    fflush(NULL);
#endif
    if (_logTraffic) { fprintf (_logFile, "Q"); report (pkt);}
  }
  else
  {
    fprintf(stderr, "No space to queue packet on channel %d\n", pkt->channel);
    exit(EXIT_FAILURE);
  }

}

Packet* PCIeLink::recv_pkt(SceMiU64* cycle_ptr)
{
  UInt32 ch = 0;
  Packet *pkt = NULL;

  // read next output channel number
  do {
    ch = pBar1->next_output_chan;
    if (ch != 0xffffffff) break;
    if (_logTraffic) fprintf (_logFile,"Channel read timeout %llu\n", *cycle_ptr);

#if DEBUG_LINK
    printf("Channel read timeout %llu\n", *cycle_ptr);
    fflush(NULL);
#endif
    usleep(1);
  } while (true);

  // if data is ready, decode port number and read the data
  if (ch & OUTPUT_DATA_READY)
  {
    SceMiU64 cs;

    ch &= OUTPUT_CHANNEL_MASK;

    if (ch >= number_outports)
    {
      printf("Invalid output channel %d!\n", ch);
      return NULL;
    }

#if DEBUG_LINK
    printf("Reading from output channel %d.\n", ch);
    fflush(NULL);
#endif

    // read cycle stamp
    cs = (SceMiU64) (pBar1->cycle_stamp_lo);
    cs |= ((SceMiU64) (pBar1->cycle_stamp_hi)) << 32;
    if (cycle_ptr)
      *cycle_ptr = cs;

#if DEBUG_LINK
    printf("  *  cycle stamp = %llu\n", cs);
    fflush(NULL);
#endif

    // create a packet
    pkt = new Packet;
    pkt->channel = ch;
    pkt->num_bits = port_width[ch];

    // allocate array to hold data
    pkt->data = new SceMiU32[bits_to_words(pkt->num_bits)];

    // read data from port
    for (unsigned int i=0; i < bits_to_words(pkt->num_bits); i++) {
      pkt->data[i] = pOutPorts[ch].data;
#if DEBUG_LINK
      printf("  *  data[%d] = %x\n", i, pkt->data[i]);
      fflush(NULL);
#endif
    }
    if (_logTraffic) report(cs, pkt);
  }

  return pkt;
}

bool PCIeLink::ready_to_send(unsigned int channel)
{
  // Check if the channel is ready to receive
  if ((pInPorts[channel].status & 0x1) == 0)
    return false;

  return true;
}

bool PCIeLink::send_pkt(unsigned int* channel_ptr)
{
  // Select the next packet to send
  Packet* pkt = NULL;
  int num_words;
  size_t ch = 0;

  for (unsigned int n = 1; n <= pending.size(); ++n)
  {
    ch = (last_tx + n) % pending.size();
    if (pending[ch] == NULL) continue;

    // Check if the channel is ready to receive
    if ((pInPorts[ch].status & 0x1) == 0) continue;

    pkt = pending[ch];
    pending[ch] = NULL;
    last_tx = ch;

    break;
  }

  // If we have a packet to send, send it
  if (pkt != NULL)
  {
    if (channel_ptr != NULL)
      *channel_ptr = pkt->channel;

    if (_logTraffic) report(pkt);

#if DEBUG_LINK
    printf("PCIeLink: Sending pkt on channel %d\n", pkt->channel);
    fflush(NULL);
#endif

    num_words = bits_to_words(pkt->num_bits);
    for (int i=0; i<num_words; i++) {
      pInPorts[ch].data = pkt->data[i];
    }

    release(pkt);

    return true;
  }

  return false;
}

void PCIeLink::release(Packet* pkt)
{
  delete[] pkt->data;
  delete pkt;
}

PCIeLink::~PCIeLink()
{
  if (pBar1)
    munmap(pBar1, sizeof(*pBar1));

  if (pBar2)
    munmap(pBar2, sizeof(*pBar2));

  delete[] port_width;
}

