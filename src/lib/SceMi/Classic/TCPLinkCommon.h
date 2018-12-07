// Copyright 2008-2009 Bluespec, Inc.  All rights reserved.

#ifndef __TCP_LINK_COMMON_H__
#define __TCP_LINK_COMMON_H__

#include "sized_types.h"

// Each port/proxy pair shares a virtual channel number
typedef UInt32 tChannelId;

// Message data is an array of 32-bit words
typedef UInt32* tPayload;

// Each packet has a 1-byte header describing the message type
typedef enum { PKT_DATA = 0, PKT_ACK, PKT_REQ, PKT_SHUTDOWN } tMsgType;

#endif /* __TCP_LINK_COMMON_H__ */

