/* Copyright (c) 2008 Bluespec, Inc.  All rights reserved. */

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "sized_types.h"
#include "EVELink.h"
#include "LinkPlugin.h"

// Plugin C-linkage functions

Link* create_link(SceMiParameters* parameters,
                  SceMiErrorHandler hdlr, void* ctx)

{
  if (parameters == NULL) return NULL;

  ec_register_handler(hdlr, ctx);

  return new EVELink(parameters);
}

void destroy_link(Link* link)
{
  delete link;
}

// Implementation of SW-side connection to EVE

EVELink::EVELink(SceMiParameters* parameters)
  : Link ("BSC_TRACE_SCEMI_EVE")
{
  if (parameters == NULL)
  {
    fprintf(stderr, "EVELink given a NULL parameters pointer\n");
    exit(EXIT_FAILURE);
  }

  /* Lookup EVE link parameters */
  const char* work_dir = parameters->AttributeStringValue("Link", 0, "EveWorkDir");
  if (work_dir == NULL)
  {
    fprintf(stderr, "EVELink parameters are missing Link 0 EveWorkDir\n");
    exit(EXIT_FAILURE);
  }
  const char* des_feat = parameters->AttributeStringValue("Link", 0, "EveDesignFeaturesFile");
  if (des_feat == NULL)
  {
    fprintf(stderr, "EVELink parameters are missing Link 0 EveDesignFeaturesFile\n");
    exit(EXIT_FAILURE);
  }

  /* Open the board */
  board = Board::open(work_dir, des_feat, "default_process");
  if (board == NULL)
  {
    fprintf(stderr, "EVELink failed to open board\n");
    exit(EXIT_FAILURE);
  }

  /* Create the ports */
  num_ports = parameters->NumberOfObjects("MessageInPort") +
              parameters->NumberOfObjects("MessageOutPort");
  first_rx_channel = 0;
  current_rx_channel = 0;
  board_initialized = 0;
  if (num_ports > 0)
  {
    ports = new tPortInfo[num_ports+1];
    ports[0].tx   = NULL;
    ports[0].rx   = NULL;
    ports[0].size = 0;
    for (unsigned int i = 0;
	 i < parameters->NumberOfObjects("MessageInPort");
	 ++i)
    {
      const char* xact = parameters->AttributeStringValue("MessageInPort",i,"TransactorName");
      const char* name = parameters->AttributeStringValue("MessageInPort",i,"PortName");
      unsigned int channel = parameters->AttributeIntegerValue("MessageInPort",i,"ChannelId");
      unsigned int bits = parameters->AttributeIntegerValue("MessageInPort",i,"PortWidth");
      ports[channel].tx = new TxPort(name,bits_to_words(bits));
      ports[channel].rx = NULL;
      ports[channel].size = bits;
      ports[channel].tx->connect(board,xact);
    }
    for (unsigned int i = 0;
	 i < parameters->NumberOfObjects("MessageOutPort");
	 ++i)
    {
      const char* xact = parameters->AttributeStringValue("MessageOutPort",i,"TransactorName");
      const char* name = parameters->AttributeStringValue("MessageOutPort",i,"PortName");
      unsigned int channel = parameters->AttributeIntegerValue("MessageOutPort",i,"ChannelId");
      unsigned int bits = parameters->AttributeIntegerValue("MessageOutPort",i,"PortWidth");
      if (i == 0) first_rx_channel = channel;
      ports[channel].tx = NULL;
      ports[channel].rx = new RxPort(name,bits_to_words(bits));
      ports[channel].size = bits;
      ports[channel].rx->connect(board,xact);
    }
  }
  else
  {
    ports = NULL;
  }

  // determine the maximun channel id to setup the pending vector
  unsigned int inportCount = parameters->NumberOfObjects ("MessageInPort");
  int maxInportChannelId = 0;
  for (unsigned int i = 0; i < inportCount ; ++i) {
    int chId = parameters->AttributeIntegerValue("MessageInPort", i, "ChannelId");
    maxInportChannelId = std::max (maxInportChannelId, chId);
  }
  pending.resize(1+maxInportChannelId);

  current_rx_channel = first_rx_channel;
  last_tx = 0;

  // get the parameters for board init call
  init_mem_file = parameters->AttributeStringValue("Link", 0, "EveInitMemFile");  
  logic_state_file = parameters->AttributeStringValue("Link", 0, "EveLogicStateFile");
  severity = parameters->AttributeIntegerValue("Link", 0, "EveSeverity");

  /* Initialize the board (no longer call init() here, moved to send_pkt())*/
  /* board->init(); */
}

void EVELink::queue(Packet* pkt)
{
  if (pkt->channel >= pending.size()) {
    fprintf(stderr, "incorrect pending size %d > %d\n", pkt->channel, (int) pending.size());
    exit( EXIT_FAILURE);
  }

  if (pending[pkt->channel] == NULL) {
    pending[pkt->channel] = pkt;
  }
  else
  {
    fprintf(stderr, "No space to queue packet on channel %d\n", pkt->channel);
    exit(EXIT_FAILURE);
  }

}

bool EVELink::ready_to_send(unsigned int channel)
{
  if (board == NULL) return false;

  TxPort* port = ports[channel].tx;
  if (port == NULL || !port->isPossibleToSend())
    return false;

  return true;
}

bool EVELink::send_pkt(unsigned int* channel_ptr)
{
  if (board == NULL) return false;

  if (board_initialized == 0) {
    board->init(init_mem_file, logic_state_file, severity);
    board_initialized = 1;
  }

  // Select the next packet to send
  Packet* pkt = NULL;
  TxPort* port = NULL;
  for (unsigned int n = 1; n <= pending.size(); ++n)
  {
    size_t ch = (last_tx + n) % pending.size();
    pkt = pending[ch];
    if (pkt == NULL) continue;
    if (pkt->channel >= num_ports) continue;
    port = ports[pkt->channel].tx;
    if (port == NULL || !port->isPossibleToSend()) continue;

    // if we reach this point we are ready to send on this channel
    pending[ch] = NULL;
    last_tx = ch;

    if (channel_ptr != NULL)
      *channel_ptr = pkt->channel;

    // Send data across port
    unsigned int* message = port->message();
    memcpy(message, pkt->data, 4*bits_to_words(pkt->num_bits));
    port->sendMessage();

    return true;
  }

  return false;
}

Packet* EVELink::recv_pkt(SceMiU64* cycle_ptr)
{
  if ((board == NULL) || (first_rx_channel == 0))
    return NULL;

  // Scan ports (resuming at current_rx_channel)
  RxPort* port = NULL;
  Packet* pkt  = NULL;
  unsigned int stop_channel = current_rx_channel;
  do
  {
    unsigned int channel = current_rx_channel;
    current_rx_channel += 1;
    if (current_rx_channel > num_ports) current_rx_channel = first_rx_channel;
    port = (RxPort*) ports[channel].rx;
    if (port == NULL) continue;
    // Check if we can receive data
    if (port->isPossibleToReceive())
    {
      unsigned int* message = port->receiveMessage();
      pkt = new Packet;
      pkt->channel = channel;
      pkt->num_bits = ports[channel].size;
      pkt->data = new SceMiU32[bits_to_words(pkt->num_bits)];
      memcpy(pkt->data, message, 4*bits_to_words(pkt->num_bits));
      return pkt;
    }
  } while (current_rx_channel != stop_channel);

  // If we got here, there was no data to receive
  return NULL;
}

Packet* EVELink::packet(unsigned int len)
{
  Packet* pkt = new Packet;
  pkt->num_bits = len;
  pkt->data = new SceMiU32[bits_to_words(len)];
  return pkt;
}

void EVELink::release(Packet* pkt)
{
  delete[] pkt->data;
  delete pkt;
}

void EVELink::loop()
{
  if (board != NULL)
    board->loop();
}

EVELink::~EVELink()
{
  if (board != NULL)
    board->close();
}

void *EVELink::scemi_extension_access_vendor_platform()
{
  return (void*)board;
}
