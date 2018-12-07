#ifndef __CORE_LINK_H__
#define __CORE_LINK_H__

#include <vector>
#include <set>
#include <list>

#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <cstring>
#include <cassert>
#include <cerrno>
#include <poll.h>
#include "sized_types.h"

#include "bluenoc_link.h"
#include "bluenoc_parameters.h"

#define BYTES_PER_BEAT (4)
#define DEBUG_LINK     (0)
#define NUM_BUFFER_PAGES (16)
#define BN_DATA_OPCODE (0x10)
#define BN_CREDIT_OPCODE (0x1F)

typedef struct {
  void*        buffer_ptr;     // start of buffer memory
  unsigned int buffer_size;    // length of contiguous buffer area, in bytes
  char*        data_start;     // pointer to first byte of data
  unsigned int buffered_bytes; // total number of data bytes
} msg_buffer_t;

typedef struct {
  msg_buffer_t *           partial_msg_buf;      // tx buffer currently being filled
  char *                   current_msg_header;   // start of partial message in tx buffer
  std::list<msg_buffer_t*> full_msg_bufs;        // filled tx buffers
} tx_buffers_t;

class core_link : public bluenoc_link
{
private:
  int          fd;
  bool         align;
  uint32_t     bytes_per_beat;
  uint32_t     max_msg_size;  // including padding to beat boundary
  void       (*shutdown_action)(int fd);

  size_t       last_tx;
  std::vector<Packet*> pending;
  int closed;

  msg_buffer_t *tx_buffer;
  msg_buffer_t *rx_buffer;

  std::list<msg_buffer_t*> free_buffer_pool;
  std::vector<tx_buffers_t> inpipe_output_buffers;

  std::vector<msg_buffer_t*> outpipe_output_buffers;
  std::vector<bool> outpipe_autoflush_state;

  std::vector<void(*)(void*,const char*,unsigned int,bool,bool,bool)> data_callbacks;
  std::vector<void*>                                             data_contexts;
  std::vector<void(*)(void*,unsigned int,bool)>                  credit_callbacks;
  std::vector<void*>                                             credit_contexts;
  pthread_mutex_t  m_write_mutex;

public:
  core_link(int link_fd, const bluenoc_parameters *parameters, bool align_buffers, void (*shutdown)(int) = NULL);
  virtual ~core_link();

  virtual int  recv_pkt(uint64_t* cycle_ptr, Packet **pptr);

  virtual void debug(const char *msg);

  virtual void set_data_handler( uint32_t node,
				 void (*callback)(void *, const char *, uint32_t, bool, bool, bool),
				 void *context);

  virtual void set_credit_handler ( uint32_t node,
				    void (*callback)(void *, uint32_t, bool),
				    void *context);

  virtual void add_message_data( uint32_t node, uint32_t elem_bytes, uint32_t len, 
				 const uint8_t *data, uint32_t byte_offset, bool eom);

  virtual void send_data( uint32_t node, bool overflow, bool flush);
  virtual void send_credits( uint32_t node, uint16_t amount, bool underflow);
  virtual void send_autoflush(uint32_t node, bool enable);

private:
  void recv_pipe_data(const char* msg);
  void recv_pipe_credits(const char* msg);
  void send_buffer(msg_buffer_t* buf);
  int is_data_pending();
  const char* recv_msg();
  void print_msg(FILE *dfp, const char* msg);
  msg_buffer_t* get_inpipe_output_buffer(tx_buffers_t& tx_bufs);
  void inpipe_output_buffer_is_full(tx_buffers_t& tx_bufs);
  void release_inpipe_output_buffer(msg_buffer_t* buf);
  void reportNoC (const msg_buffer_t *);
};

#endif //__CORE_LINK_H__
