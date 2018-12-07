/*
  Parts Copyright © 2003-2007 by Accellera
  Parts Copyright (c) 2008-2009 Bluespec, Inc; all rights reserved
  scemi_capi.h - SCE-MI C Interface
*/

#ifndef __SCEMI_CBSV_H__
#define __SCEMI_CBSV_H__


/* include header files for the various Sce-Mi classes */


#ifdef __cplusplus
extern "C" {
#endif

  /* -------------------------------------------------------------------- */
  /* Scemi support for interfacing with BSV SceMiMessage(In/Out)PortProxy */
  /* -------------------------------------------------------------------- */

  /* Message input port proxy binding */
  unsigned long long bsvscemi_bind_message_inport(const char *transactorName, const char *portName);

  /* Message output port proxy binding */
  unsigned long long bsvscemi_bind_message_outport(const char *transactorName, const char *portName);

 
  /* Returns whether the buffer for output port to the DUT/Transactor is ready to recieved */
  unsigned int bsvscemi_message_inport_proxy_ready(unsigned int index);

  /* Message input port send */
  void bsvscemi_message_inport_proxy_send(unsigned int *data, unsigned int len,
					  unsigned int index);

  /* Returns whether the buffer for output port from the DUT/Transactor has data */
  unsigned int bsvscemi_message_outport_proxy_ready(unsigned int index);

  /* Message output port receive */
  void bsvscemi_message_outport_proxy_data_get(unsigned int *data, unsigned int *len, 
					     unsigned int index);

  /* Declarations of callback functions used when HW is ready to send/receive */
  void bsvscemi_message_inport_ready_CB(void* context);
  void bsvscemi_message_outport_ready_CB(void* context, const SceMiMessageData* data);

  /* Deallocate and shutdown SceMi infrastructure */
  void bsvscemi_shutdown();

#ifdef __cplusplus
};
#endif

#endif /* __SCEMI_CBSV_H__ */
