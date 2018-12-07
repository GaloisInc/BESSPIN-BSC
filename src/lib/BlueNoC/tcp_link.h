#ifndef __TCP_LINK_H__
#define __TCP_LINK_H__

#include "bluenoc_parameters.h"
#include "bluenoc_link.h"
#include "core_link.h"

typedef struct {
  char *buffer_start;
  char *current_msg_header;
  uint32_t buffered_bytes;
} pipe_msg_buffer_t;

typedef struct {
  pipe_msg_buffer_t *partial_msg_buf;
  std::list<pipe_msg_buffer_t*> full_msg_bufs;
} pipe_buffer_state_t;

#endif // __TCP_LINK_H__
