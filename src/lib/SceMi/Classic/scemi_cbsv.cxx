/*
  Parts Copyright © 2003-2007 by Accellera
  Parts Copyright (c) 2008 Bluespec, Inc; all rights reserved
*/

/* include header files for the various Sce-Mi classes */

#include "scemi.h"
#include <list>
#include <vector>
#include <cstring>
#include <cstdio>

/* Packet with cycle stamp */
struct SPacket
{
  unsigned int   channel;
  unsigned int   num_bits;
  unsigned char* data;
  SceMiU64       cycle;
};

/*
  List of packets for buffering packets between
  BSV proxies and SceMi message ports
*/
typedef std::list<SPacket*> SPacketList;
static std::vector<SPacketList*> bsvscemi_packets_from_hw;
static std::vector<bool> bsvscemi_link_ready_for_send;
static std::vector<SceMiMessageInPortProxy*> bsvscemi_in_proxies;

extern "C" {

  /* -------------------------------------------------------------------- */
  /* Scemi support for interfacing with BSV SceMiMessage(In/Out)PortProxy */
  /* -------------------------------------------------------------------- */

  /* Declarations of callback functions used when HW is ready to send/receive */
  void bsvscemi_message_inport_ready_CB(void* context);
  void bsvscemi_message_outport_ready_CB(void* context, const SceMiMessageData* data);


  /* Message input port proxy binding */
  /* returns Maybe#(UInt#(32)) */
  unsigned long long
  bsvscemi_bind_message_inport(const char *paramFile,
                               const char *transactorName,
                               const char *portName)
  {
    SceMiMessageInPortProxy *inProxy;
    SceMiMessageInPortBinding msgInPortBinding;
    unsigned int index = 0;

    // Default result is "tagged Invalid"
    unsigned long long invalid = 0;

    SceMiEC* ec = NULL;
    SceMi *scemi = SceMi::Pointer(ec);

    if (scemi == NULL) {
      int sceMiVersion = SceMi::Version( SCEMI_VERSION_STRING );
      SceMiParameters parameters(paramFile);
      if (!parameters.loaded()) return invalid;
      scemi = SceMi::Init(sceMiVersion, &parameters);
      if (scemi == NULL) return invalid;
    }

    inProxy = scemi->BindMessageInPort(transactorName, portName);

    /* Create a list for storing the ready flags */
    if (inProxy) {

      index = inProxy->ChannelId();
      if (index >= bsvscemi_in_proxies.size())
	{
	  bsvscemi_link_ready_for_send.resize(index + 1);
	  bsvscemi_in_proxies.resize(index + 1);
	}
      bsvscemi_link_ready_for_send[index] = false;
      bsvscemi_in_proxies[index] = inProxy;
    }

    msgInPortBinding.Context = inProxy;
    msgInPortBinding.IsReady = bsvscemi_message_inport_ready_CB;
    msgInPortBinding.Close   = NULL;
    inProxy->ReplaceBinding(&msgInPortBinding);

    return (0x100000000ull | (unsigned long long)index);
  }

  /* Message output port proxy binding */
  /* returns Maybe#(UInt#(32)) */
  unsigned long long
  bsvscemi_bind_message_outport(const char *paramFile,
                                const char *transactorName,
                                const char *portName)
  {
    SceMiMessageOutPortProxy *outProxy;
    SceMiMessageOutPortBinding msgOutPortBinding;
    unsigned int index = 0;

    // Default result is "tagged Invalid"
    unsigned long long invalid = 0;

    SceMiEC* ec = NULL;
    SceMi *scemi = SceMi::Pointer(ec);

    if (scemi == NULL) {
      int sceMiVersion = SceMi::Version( SCEMI_VERSION_STRING );
      SceMiParameters parameters(paramFile);
      if (!parameters.loaded()) return invalid;
      scemi = SceMi::Init(sceMiVersion, &parameters);
      if (scemi == NULL) return invalid;
    }

    outProxy = scemi->BindMessageOutPort(transactorName, portName);

    msgOutPortBinding.Context = outProxy;
    msgOutPortBinding.Receive = bsvscemi_message_outport_ready_CB;
    msgOutPortBinding.Close   = NULL;
    outProxy->ReplaceBinding(&msgOutPortBinding);

    /* Create a list for storing the packets */
    if (outProxy) {

      index = outProxy->ChannelId();
      if (index >= bsvscemi_packets_from_hw.size())
	bsvscemi_packets_from_hw.resize(index + 1);
      if (bsvscemi_packets_from_hw[index] == NULL)
	bsvscemi_packets_from_hw[index] = new SPacketList;
    }

    return (0x100000000ull | (unsigned long long)index);
  }

  /* Returns whether the buffer for output port to the DUT/Transactor is ready to recieve */
  unsigned int bsvscemi_message_inport_proxy_ready(unsigned int index)
  {
    SceMiEC* ec = NULL;
    SceMi *scemi = SceMi::Pointer(ec);

    if (scemi)
      scemi->ServiceLoop(NULL, NULL, NULL);

    return  bsvscemi_link_ready_for_send[index];
  }

  /* Message input port send, put the data into a packet and then the to_hw queue */
  void bsvscemi_message_inport_proxy_send(unsigned int *data, unsigned int len, unsigned int index)
  {
    SceMiMessageInPortProxy *proxy;
    int nwords;

    if (bsvscemi_link_ready_for_send[index]) {

      proxy = bsvscemi_in_proxies[index];
      SceMiMessageData msg(*proxy);

      nwords = bits_to_words(len);
      for (int i=0; i<nwords; i++) {
	msg.Set(i, data[i]);
      }

      proxy->Send(msg);

      bsvscemi_link_ready_for_send[index] = false;

    } else {

      SceMiEC* ec = NULL;

      raiseError ("bsvscemi_message_inport_proxy_send",
		  "something wrong, the port proxy is not ready for send", SceMiError, ec);
    }
  }

  /* Returns whether the buffer for output port from the DUT/Transactor has data */
  unsigned int bsvscemi_message_outport_proxy_ready(unsigned int index)
  {
    if (bsvscemi_packets_from_hw[index] == NULL)
      return false;

    if (bsvscemi_packets_from_hw[index]->size() > 0)
      return true;

    return false;
  }

  /* Message output port receive */
  void bsvscemi_message_outport_proxy_data_get(unsigned int *data, unsigned int len,
					       unsigned int index)
  {
    SPacket *pkt;
    unsigned int nbytes;
    SceMiEC* ec = NULL;
    unsigned int i;
    unsigned char *ptr;

    if (bsvscemi_packets_from_hw[index] == NULL)
      return;

    pkt = bsvscemi_packets_from_hw[index]->front();
    if (pkt) {
      bsvscemi_packets_from_hw[index]->pop_front();

      if (pkt->num_bits != len)
	raiseError ("bsvscemi_message_outport_proxy_data_get",
		    "the length of data packet is not the same as port data width", SceMiError, ec);

      ptr = (unsigned char*)data;
      memcpy(ptr, &pkt->cycle, 8);
      nbytes = bits_to_bytes(len);
      for (i=0; i<nbytes; i++) {
	ptr[i+8] = pkt->data[i];
      }

      delete [] pkt->data;
      delete pkt;
    }
  }

  /* Callback for sending data to HW */
  void bsvscemi_message_inport_ready_CB(void* context)
  {
    SceMiMessageInPortProxy *proxy;
    unsigned int index;

    proxy = (SceMiMessageInPortProxy*)context;
    index = proxy->ChannelId();

    bsvscemi_link_ready_for_send[index] = true; /* Record ready for send */
  }

  /* Callback for receiving data from HW */
  void bsvscemi_message_outport_ready_CB(void* context, const SceMiMessageData* data)
  {
    SceMiMessageInPortProxy *proxy;
    SPacket *pkt;
    int nwords, nbytes;
    SceMiEC* ec = NULL;
    SceMiU32 word_data;

    proxy = (SceMiMessageInPortProxy*)context;

    if (data->WidthInBits() != proxy->PortWidth())
      {
	char m[128];
	sprintf(m, "the length of data packet from SceMiMessageOutPort %d is not the same as port data width",
		proxy->ChannelId());
	raiseError ("bsvscemi_message_outport_ready_CB",
		    m, SceMiError, ec);
      }

    /* Get data */
    pkt = new SPacket;
    pkt->num_bits = data->WidthInBits();
    nwords = bits_to_words(pkt->num_bits);
    pkt->channel = proxy->ChannelId();
    pkt->data = new unsigned char[nwords*4];
    for (int i=0; i<nwords; i++) {
      word_data = data->Get(i);
      nbytes = i*4;
      memcpy(pkt->data+nbytes, &word_data, 4);
    }

    /* Get CycleStamp */
    pkt->cycle = data->CycleStamp();

    bsvscemi_packets_from_hw[pkt->channel]->push_back(pkt);
  }

  /* Deallocate and shutdown SceMi infrastructure */
  void bsvscemi_shutdown()
  {
    SceMiEC* ec = NULL;
    SceMi *scemi = SceMi::Pointer(ec);
    SPacket *spkt;

    for (unsigned int i=0; i<bsvscemi_packets_from_hw.size(); i++) {
      /* Delete any left over from HW packets */
      SPacketList *slist = bsvscemi_packets_from_hw[i];
      while (slist && !slist->empty()) {
	spkt = slist->front();
	if (spkt) {
	  delete [] spkt->data;
	  delete spkt;
	  slist->pop_front();
	}
      }
      delete slist;
      bsvscemi_packets_from_hw[i] = NULL;
    }

    /* Shutdown and deallocate the scemi infrastructure */
    scemi->Shutdown(scemi, ec);
  }
};
