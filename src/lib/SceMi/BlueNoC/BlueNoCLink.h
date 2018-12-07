#ifndef __BLUENOC_LINK_H__
#define __BLUENOC_LINK_H__

#include <vector>
#include <set>
#include <list>
#include <signal.h>

#include "Link.h"
#include "SceMiParameters.h"

/* Buffer data structure */
typedef struct {
  void*        buffer_ptr;     // start of buffer memory
  unsigned int buffer_size;    // length of contiguous buffer area, in bytes
  char*        data_start;     // pointer to first byte of data
  unsigned int buffered_bytes; // total number of data bytes
} tMsgBuffer;

/* Multiple-buffer collection (used for TX channel) */
typedef struct {
  tMsgBuffer*            partial_msg_buf;    // tx buffer currently being filled
  char*                  current_msg_header; // start of partial message in tx buffer
  std::list<tMsgBuffer*> full_msg_bufs;      // filled tx buffers
} tTxBuffers;

class BlueNoCLink : public Link
{
 private:
  int          fd;
  bool         align;
  unsigned int bytes_per_beat;
  unsigned int max_msg_size; // including padding to beat boundary
  void       (*shutdown_action)(int fd);

  size_t       last_tx;
  std::vector<Packet*> pending;
  std::set<unsigned int> request_rx_slots;
  std::set<unsigned int> pending_acks;
  int closed;

  Packet*      recv_packet_in_progress;
  unsigned int recv_channel_in_progress;
  char*        recv_data_ptr;
  SceMiU64     recv_cyclestamp;

  tMsgBuffer* tx_buffer;
  tMsgBuffer* rx_buffer;

  std::list<tMsgBuffer*> free_buffer_pool;
  std::vector<tTxBuffers> inpipe_output_buffers;

  std::vector<tMsgBuffer*> outpipe_output_buffers;
  std::vector<bool> outpipe_autoflush_state;

  std::vector<void(*)(void*,const char*,unsigned int,bool,bool,bool)> data_callbacks;
  std::vector<void*>                                             data_contexts;
  std::vector<void(*)(void*,unsigned int,bool)>                  credit_callbacks;
  std::vector<void*>                                             credit_contexts;
  pthread_mutex_t  m_write_mutex;

  sigset_t block_set;

 public:
  BlueNoCLink(int link_fd, const SceMiParameters* parameters, bool align_buffers, void (*shutdown)(int) = NULL);

  // SCE-MI 1.1 support
  virtual void queue(Packet* pkt);
  virtual bool ready_to_send(unsigned int channel);
  virtual bool send_pkt(unsigned int* channel_ptr);
  virtual int  recv_pkt(SceMiU64* cycle_ptr, Packet **pptr);
  virtual Packet* packet(unsigned int len);
  virtual void release(Packet* pkt);
  virtual void loop();

  // SCE-MI 2.1 pipes support
  virtual void set_data_handler( unsigned int pipe_num,
                                 void (*callback)(void*,const char*,unsigned int,bool,bool,bool),
                                 void* context
                               );
  virtual void set_credit_handler( unsigned int pipe_num,
                                   void (*callback)(void*,unsigned int,bool),
                                   void* context
                                 );
  virtual void send_credits(unsigned int pipe_num, unsigned int amount, bool underflow);
  virtual void send_autoflush(unsigned int pipe_num, bool enable);
  virtual void add_message_data( unsigned int pipe_num, unsigned int elem_bytes
                                 , unsigned int len, const unsigned char* data
                                 , unsigned int byte_offset, bool eom
                               );
  virtual void send_data(unsigned int pipe_num, bool overflow, bool flush);
  virtual void debug (const char *msg) ;

  virtual ~BlueNoCLink();

 private:
  Packet* recv_data_packet(const char* msg, SceMiU64* cycle_ptr);
  void recv_req_packet(const char* msg);
  void recv_quit_packet(const char* msg);
  void send_ack_packet();
  void recv_pipe_data(const char* msg);
  void recv_pipe_credits(const char* msg);
  void send_buffer(tMsgBuffer* buf);
  int is_data_pending();
  const char* recv_msg();
  void print_msg(FILE *dfp, const char* msg);
  tMsgBuffer* get_inpipe_output_buffer(tTxBuffers& tx_bufs);
  void inpipe_output_buffer_is_full(tTxBuffers& tx_bufs);
  void release_inpipe_output_buffer(tMsgBuffer* buf);

  void reportNoC (const tMsgBuffer *);
};

#endif /* __BLUENOC_LINK_H__ */
