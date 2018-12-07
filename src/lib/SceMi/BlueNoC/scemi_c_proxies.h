/*
  Parts Copyright © 2003-2007 by Accellera
  Parts Copyright (c) 2008-2009 Bluespec, Inc; all rights reserved
*/
#ifndef __SCEMI_C_PROXIES_H__
#define __SCEMI_C_PROXIES_H__

#include "scemi_capi.h"

#ifdef __cplusplus
extern "C" {
#endif

  /* Message input port proxy binding */
  SceMiMessageInPortProxy *SceMiBindMessageInPort(SceMi *sceMiHandle,
						  const char *transactorName,
						  const char *portName,
						  const SceMiMessageInPortBinding *binding,
						  SceMiEC *ec);

  /* Message output port proxy binding */
  SceMiMessageOutPortProxy *SceMiBindMessageOutPort(SceMi *sceMiHandle,
						    const char *transactorName,
						    const char *portName,
						    const SceMiMessageOutPortBinding *binding,
						    SceMiEC *ec);

  /* SceMiMessageData - message data object */
  /* Constructor */
  SceMiMessageData *SceMiMessageDataNew(const SceMiMessageInPortProxy *messageInPortProxyHandle,
					SceMiEC *ec );

  /* Destructor */
  void SceMiMessageDataDelete(SceMiMessageData *messageDataHandle);

  /* Accessors */
  unsigned int SceMiMessageDataWidthInBits(const SceMiMessageData *messageDataHandle);

  unsigned int SceMiMessageDataWidthInWords(const SceMiMessageData *messageDataHandle);

  void SceMiMessageDataSet(SceMiMessageData *messageDataHandle,
			   unsigned int i,
			   SceMiU32 word,
			   SceMiEC *ec);

  void SceMiMessageDataSetBit(SceMiMessageData *messageDataHandle,
			      unsigned int i,
			      int bit,
			      SceMiEC *ec);

  void SceMiMessageDataSetBitRange(SceMiMessageData *messageDataHandle,
				   unsigned int i,
				   unsigned int range,
				   SceMiU32 bits,
				   SceMiEC *ec);

  SceMiU32 SceMiMessageDataGet(const SceMiMessageData *messageDataHandle,
			       unsigned int i,
			       SceMiEC *ec);

  int SceMiMessageDataGetBit(const SceMiMessageData *messageDataHandle,
			     unsigned int i,
			     SceMiEC *ec);

  SceMiU32 SceMiMessageDataGetBitRange(const SceMiMessageData *messageDataHandle,
				       unsigned int i,
				       unsigned int range,
				       SceMiEC *ec);

  SceMiU64 SceMiMessageDataCycleStamp(const SceMiMessageData *messageDataHandle);


  /* SceMiMessageInPortProxy - message input port proxy */
  /* Sending input messages */
  void SceMiMessageInPortProxySend(SceMiMessageInPortProxy *messageInPortProxyHandle,
				   const SceMiMessageData *messageDataHandle,
				   SceMiEC *ec);

  /* Replacing port binding */
  void SceMiMessageInPortProxyReplaceBinding(SceMiMessageInPortProxy *messageInPortProxyHandle,
					     const SceMiMessageInPortBinding* binding,
					     SceMiEC* ec);
  /* Accessors */
  const char *
  SceMiMessageInPortProxyTransactorName(const SceMiMessageInPortProxy *messageInPortProxyHandle);

  const char *
  SceMiMessageInPortProxyPortName(const SceMiMessageInPortProxy *messageInPortProxyHandle);

  unsigned
  SceMiMessageInPortProxyPortWidth(const SceMiMessageInPortProxy *messageInPortProxyHandle);


#ifdef __cplusplus
};
#endif

#endif /* __SCEMI_C_PROXIES_H__ */
