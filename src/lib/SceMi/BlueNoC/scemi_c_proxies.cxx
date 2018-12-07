/*
  Parts Copyright © 2003-2007 by Accellera
  Parts Copyright (c) 2008 Bluespec, Inc; all rights reserved
*/

#include "scemi.h"

extern "C" {

  /* Message input port proxy binding */
  SceMiMessageInPortProxy *SceMiBindMessageInPort(SceMi *sceMiHandle,
						  const char *transactorName,
						  const char *portName,
						  const SceMiMessageInPortBinding *binding,
						  SceMiEC *ec)
  {
    return sceMiHandle->BindMessageInPort(transactorName, portName, binding, ec);
  }

  /* Message output port proxy binding */
  SceMiMessageOutPortProxy *SceMiBindMessageOutPort(
						    SceMi *sceMiHandle,
						    const char *transactorName,
						    const char *portName,
						    const SceMiMessageOutPortBinding *binding,
						    SceMiEC *ec)
  {
    return sceMiHandle->BindMessageOutPort(transactorName, portName, binding, ec);
  }

  /* SceMiMessageData - message data object */
  /* Constructor */
  SceMiMessageData *SceMiMessageDataNew(const SceMiMessageInPortProxy *messageInPortProxyHandle,
					SceMiEC *ec )
  {
    return new SceMiMessageData(*messageInPortProxyHandle, ec);
  }

  /* Destructor */
  void SceMiMessageDataDelete(SceMiMessageData *messageDataHandle)
  {
    delete messageDataHandle;
  }

  /* Accessors */
  unsigned int SceMiMessageDataWidthInBits(const SceMiMessageData *messageDataHandle)
  {
    return messageDataHandle->WidthInBits();
  }

  unsigned int SceMiMessageDataWidthInWords(const SceMiMessageData *messageDataHandle)
  {
    return messageDataHandle->WidthInWords();
  }

  void SceMiMessageDataSet(SceMiMessageData *messageDataHandle,
			   unsigned int i,
			   SceMiU32 word,
			   SceMiEC *ec)
  {
    messageDataHandle->Set(i, word, ec);
  }

  void SceMiMessageDataSetBit(SceMiMessageData *messageDataHandle,
			      unsigned int i,
			      int bit,
			      SceMiEC *ec)
  {
    messageDataHandle->SetBit(i, bit, ec);
  }

  void SceMiMessageDataSetBitRange(SceMiMessageData *messageDataHandle,
				   unsigned int i,
				   unsigned int range,
				   SceMiU32 bits,
				   SceMiEC *ec)
  {
    messageDataHandle->SetBitRange(i, range, bits, ec);
  }

  SceMiU32 SceMiMessageDataGet(const SceMiMessageData *messageDataHandle,
			       unsigned int i,
			       SceMiEC *ec)
  {
    return messageDataHandle->Get(i, ec);
  }

  int SceMiMessageDataGetBit(const SceMiMessageData *messageDataHandle,
			     unsigned int i,
			     SceMiEC *ec)
  {
    return messageDataHandle->GetBit(i, ec);
  }

  SceMiU32 SceMiMessageDataGetBitRange(const SceMiMessageData *messageDataHandle,
				       unsigned int i,
				       unsigned int range,
				       SceMiEC *ec)
  {
    return messageDataHandle->GetBitRange(i, range, ec);
  }

  SceMiU64 SceMiMessageDataCycleStamp(const SceMiMessageData *messageDataHandle);


  /* SceMiMessageInPortProxy - message input port proxy */
  /* Sending input messages */
  void SceMiMessageInPortProxySend(SceMiMessageInPortProxy *messageInPortProxyHandle,
				   const SceMiMessageData *messageDataHandle,
				   SceMiEC *ec)
  {
    messageInPortProxyHandle->Send(*messageDataHandle, ec);
  }

  /* Replacing port binding */
  void SceMiMessageInPortProxyReplaceBinding(SceMiMessageInPortProxy *messageInPortProxyHandle,
					     const SceMiMessageInPortBinding* binding,
					     SceMiEC* ec)
  {
    messageInPortProxyHandle->ReplaceBinding(binding, ec);
  }

  /* Accessors */
  const char *
  SceMiMessageInPortProxyTransactorName(const SceMiMessageInPortProxy *messageInPortProxyHandle)
  {
    return messageInPortProxyHandle->TransactorName();
  }

  const char *
  SceMiMessageInPortProxyPortName(const SceMiMessageInPortProxy *messageInPortProxyHandle)
  {
    return messageInPortProxyHandle->PortName();
  }

  unsigned
  SceMiMessageInPortProxyPortWidth(const SceMiMessageInPortProxy *messageInPortProxyHandle)
  {
    return messageInPortProxyHandle->PortWidth();
  }

};
