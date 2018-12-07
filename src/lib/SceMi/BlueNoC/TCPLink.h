#ifndef __TCP_LINK_H__
#define __TCP_LINK_H__

#include "SceMiParameters.h"
#include "Link.h"

typedef struct {
  char* buffer_start;
  char* current_msg_header;
  unsigned int buffered_bytes;
} tPipeMsgBuffer;

typedef struct {
  tPipeMsgBuffer* partial_msg_buf;
  std::list<tPipeMsgBuffer*> full_msg_bufs;
} tPipeBufferState;

#endif /* __TCP_LINK_H__ */
