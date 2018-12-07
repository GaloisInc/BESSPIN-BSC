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
#include "SceMiPipeInfo.h"
#include <sched.h>

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
static std::vector<SceMiPipeInfo*> bsvscemi_inpipes;
static std::vector<SceMiPipeInfo*> bsvscemi_outpipes;

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

    scemi->ServiceThreadStop(true);

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

      char m[128];
      sprintf(m, "something wrong, the port proxy (%d) is not ready for send", index);

      raiseError ("bsvscemi_message_inport_proxy_send",
		  m, SceMiError, ec);
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

      if (pkt->num_bits != len) {
	char m[128];
	sprintf(m, "the length of data packet %0d is not the same as port data width %0d\n", pkt->num_bits, len);
	raiseError ("bsvscemi_message_outport_proxy_data_get",
		    m, SceMiError, ec);
      }
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

  ////////////////////////////////////////////////////////////////////////////////
  ///
  ////////////////////////////////////////////////////////////////////////////////

  /* Output pipe proxy binding */
  /* returns Maybe#(UInt#(32)) */
  unsigned long long
  bsvscemi_bind_outpipe(const char *paramFile,
			const char *transactorName,
			const char *pipeName)

  {
    // fprintf(stderr, "BINDING OUTPIPE\n");
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
      if (scemi == NULL) {
     	fprintf(stderr, "Unable to initialize SceMi.\n");
     	return invalid;
      }
    }

    std::string path;

    if (strcmp(transactorName, "") == 0) {
      path = pipeName;
    } else {
      path = transactorName;
      path.append(".");
      path.append(pipeName);
    }

    SceMiPipe* pipe = scemi->get_scemi_pipe(path.c_str());
    if (pipe == NULL) {
      fprintf(stderr, "SceMi Pipe named %s does not exist.\n", path.c_str());
      return invalid;
    } else {
      index = pipe->num();
      if (index >= bsvscemi_outpipes.size())
	bsvscemi_outpipes.resize(index+1);
      SceMiPipeInfo* info = new SceMiPipeInfo(pipe);
      bsvscemi_outpipes[index] = info;
    }

    return (0x100000000ull | (unsigned long long)index);

  }

  unsigned int bsvscemi_outpipe_proxy_ready(unsigned int index) {
    SceMiPipeInfo* info = bsvscemi_outpipes[index];
    SceMiPipe* pipe = info->getPipe();
    if (pipe == NULL) {
      fprintf(stderr, "SceMi Pipe %d does not exist.\n", index);
      return false;
    } else {
      int32_t received = 0;

      int depth  = info->getDepth();
      svBit eom;
      svBitVecVal *ptr = info->getPtr();

      received = scemi_pipe_c_try_receive(pipe, 0, depth+1, ptr, &eom);

      if (received) {
      	//	fprintf(stderr, "RECV %d\n", received);
      	info->recv_push(ptr, received);
      }

      return (int) (!info->recv_empty());
    }
  }

  void bsvscemi_outpipe_proxy_data_get(unsigned int *data, unsigned int len, unsigned int index) {
    (*data) = 0;
    SceMiPipeInfo* info = bsvscemi_outpipes[index];

    if(!info->recv_empty()) {
      info->copyFromFront(data, 1);
      info->recv_pop(1);
    } else {

      SceMiEC* ec = NULL;

      char m[128];
      sprintf(m, "something wrong, the outpipe proxy (%d) does not have data available", index);

      raiseError ("bsvscemi_outpipe_proxy_get",
    		  m, SceMiError, ec);

    }
  }


  void bsvscemi_outpipe_proxy_data_get_immediate(unsigned int *data, unsigned int len, unsigned int index) {
    SceMiPipeInfo* info = bsvscemi_outpipes[index];
    SceMiEC* ec = NULL;
    char m[128];
    if (info == NULL) {
      sprintf(m, "SceMi Pipe %d does not exist.\n", index);
      raiseError ("bsvscemi_outpipe_proxy_data_get_immediate",
		  m, SceMiError, ec);
    }
    SceMiPipe* pipe = info->getPipe();
    if (pipe == NULL) {
      sprintf(m, "SceMi Pipe %d does not exist.\n", index);
      raiseError ("bsvscemi_outpipe_proxy_data_get_immediate",
		  m, SceMiError, ec);
    }
    (*data) = 0;

    int depth  = info->getDepth();
    svBit eom;
    svBitVecVal *ptr = info->getPtr();

    int32_t received = 0;
    received = scemi_pipe_c_try_receive(pipe, 0, depth+1, ptr, &eom);
    if (received) {
      info->recv_push(ptr, received);
    }
    while (true) {
      if (info->recv_count() != 0) {
	info->copyFromFront(data, 1);
	info->recv_pop(1);
	break;
      }
      sched_yield();
      received = scemi_pipe_c_try_receive(pipe, 0, depth+1, ptr, &eom);
      if (received) {
      	info->recv_push(ptr, received);
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  ///
  ////////////////////////////////////////////////////////////////////////////////

  /* Input pipe proxy binding */
  /* returns Maybe#(UInt#(32)) */
  unsigned long long
  bsvscemi_bind_inpipe(const char *paramFile,
		       const char *transactorName,
		       const char *pipeName)

  {
    // fprintf(stderr, "BINDING INPIPE\n");
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
       if (scemi == NULL) {
     	fprintf(stderr, "Unable to initialize SceMi.\n");
     	return invalid;
       }
     }

     // Is this needed? Why was it added to bind_inport?
     scemi->ServiceThreadStop(true);

    std::string path;

    if (strcmp(transactorName, "") == 0) {
      path = pipeName;
    } else {
      path = transactorName;
      path.append(".");
      path.append(pipeName);
    }

    SceMiPipe* pipe = scemi->get_scemi_pipe(path.c_str());
    if (pipe == NULL) {
      fprintf(stderr, "C SceMi Pipe named %s does not exist.\n", path.c_str());
      return invalid;
    } else {
      index = pipe->num();
      if (index >= bsvscemi_inpipes.size())
	bsvscemi_inpipes.resize(index+1);
      SceMiPipeInfo* info = new SceMiPipeInfo(pipe);
      bsvscemi_inpipes[index] = info;
    }

    return (0x100000000ull | (unsigned long long)index);

  }

  unsigned int bsvscemi_inpipe_proxy_ready(unsigned int index) {
    SceMiEC* ec = NULL;
    SceMi *scemi = SceMi::Pointer(ec);

    if (scemi)
      scemi->ServiceLoop(NULL, NULL, NULL);

    SceMiPipeInfo* info = bsvscemi_inpipes[index];
    SceMiPipe* pipe = info->getPipe();

    if (pipe == NULL) {
      fprintf(stderr, "SceMi Pipe %d does not exist.\n", index);
      return false;
    } else {
      info->send_data();
      return (info->send_count() < info->getDepth());
    }
  }

  void bsvscemi_inpipe_proxy_send(unsigned int *data, unsigned int len, unsigned int index) {
    SceMiPipeInfo* info = bsvscemi_inpipes[index];
    if (info != NULL) {
       info->send_push(data, 1);
    } else {
      fprintf(stderr, "SceMi Pipe %d does not exist.\n", index);
    }
  }

  void bsvscemi_inpipe_proxy_send_immediate(unsigned int *data, unsigned int len, unsigned int index) {
    SceMiPipeInfo* info = bsvscemi_inpipes[index];
    SceMiEC* ec = NULL;
    char m[128];
    if (info == NULL) {
      sprintf(m, "SceMi Pipe %d does not exist.\n", index);
      raiseError ("bsvscemi_inpipe_proxy_send_immediate",
		  m, SceMiError, ec);
    }
    SceMiPipe* pipe = info->getPipe();
    if (pipe == NULL) {
      sprintf(m, "SceMi Pipe %d does not exist.\n", index);
      raiseError ("bsvscemi_inpipe_proxy_send_immediate",
		  m, SceMiError, ec);
    }

    //    fprintf(stderr, "SEND IMMEDIATE %x\n", (*data));
    info->send_data_immediate();
    info->send_push(data, 1);
    info->send_data_immediate();
  }

};
