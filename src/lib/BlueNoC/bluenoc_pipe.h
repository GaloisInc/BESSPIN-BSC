// Copyright (c) 2014  Bluespec, Inc.  ALL RIGHTS RESERVED

#ifndef __BLUENOC_PIPE_H__
#define __BLUENOC_PIPE_H__

#include <cstdlib>
#include <list>
#include <vector>
#include <pthread.h>
#include <cstdio>
#include <cassert>
#include <list>
#include <algorithm>
#include <cstring>


#include "bluenoc_link.h"
#include "DataBufferQ.h"

typedef struct {
  void *key;
  void *value;
} user_data_t;

extern "C" {
  typedef void c_callback_fn(void *);
}

struct notification_t;
class bluenoc_pipe;

void *get_notification_context(notification_t *);
bluenoc_pipe *get_notification_pipe(notification_t *);

class bluenoc_pipe
{
protected:
  // transactor hierarchy
  const char *m_xactor_name;
  const char *m_pipe_name;
  char *m_endpoint_path;

  // node id
  const uint32_t m_nodeid;

  // number of bits in an element
  const uint32_t m_elem_bits;
  // number of bytes in an element
  const uint32_t m_elem_bytes;
  // number of elements deep
  const uint32_t m_depth;

  // link used to transmit and receive messages
  bluenoc_link *m_link;

  notification_t *m_notify_hd;
  notification_t *m_notify_tl;

  bool m_pending_notify;
  bool m_flush_recvd;

  // mutex which protects the pipe state
  pthread_mutex_t m_lock;

private:
  // user data
  std::list<user_data_t> m_user_data;

private:
  bluenoc_pipe();
  bluenoc_pipe(const bluenoc_pipe &);

protected:
  bluenoc_pipe(const char *   xactor_name,
	       const char *   pipe_name,
	       uint32_t       nodeid,
	       uint32_t       elem_bits,
	       uint32_t       pipe_depth,
	       bluenoc_link * link);

public:
  virtual ~bluenoc_pipe();

  virtual bool is_input() const = 0;

  const char *endpoint_path() const { return m_endpoint_path; }

  uint32_t get_nodeid() const { return m_nodeid; }

  uint32_t bits_per_element() const { return m_elem_bits; }

  uint32_t bytes_per_element() const { return m_elem_bytes; }

  uint32_t buffer_depth() const { return m_depth; }
 
  virtual bool in_flush_state() = 0;

  virtual bool set_auto_flush(bool enable) = 0;

  virtual void debug();

  notification_t *request_notification(c_callback_fn *fn, void *ctx, uint32_t threshold);
  void cancel_notification(notification_t *hdl);

  void set_user_data(void *key, void *value);
  void *get_user_data(void *key);

  virtual void process_notification() = 0;

protected:
  void do_notification(uint32_t trigger_level, bool do_all);
  void set_pending_notify(bool flush);
};

class bluenoc_outpipe: public bluenoc_pipe
{
private:
  // pipe state
  uint32_t m_elem_count;
  uint32_t m_credits;
  bool m_underflow;
  bool m_active;
  bool m_pending_send;
  bool m_pending_flush;
  bool m_autoflush;

  // buffered element state
  DataBufferQ m_dataQ;
  EomQ        m_eomQ;

  uint32_t m_current_elem_bytes;

 public:
  bluenoc_outpipe(const char*  xactor_name,
                  const char*  pipe_name,
                  uint32_t     nodeid,
                  uint32_t     elem_bits,
                  uint32_t     pipe_depth,
                  bluenoc_link* link
                );

  virtual ~bluenoc_outpipe();

  virtual bool is_input() const { return false; }

  // -------------------------
  // Application-side API

  unsigned int can_receive();

  unsigned int try_receive(uint32_t      byte_offset,
                           uint32_t      num_elems,
                           unsigned char* data,
			   bool*         eom,
                           bool*         flush,
                           void        (*callback)(void*),
                           void*         callback_arg
                           );

  virtual bool in_flush_state();

  virtual bool set_auto_flush(bool enable);
  virtual void debug();

  // -------------------------
  // Link-side API

  void put_data(const char* buf, uint32_t len, bool overflow, bool flush, bool eom);
  virtual void process_notification();
};

class bluenoc_inpipe : public bluenoc_pipe
{
 private:
  // pipe state
  uint32_t m_elem_count;
  uint32_t m_credits;
  bool m_overflow;
  bool m_flushing;
  bool m_active;
  bool m_pending_recv;
  bool m_autoflush;

 public:
  bluenoc_inpipe(const char*  xactor_name,
                 const char*  pipe_name,
                 uint32_t     nodeid,
                 uint32_t     elem_bits,
                 uint32_t     pipe_depth,
                 bluenoc_link *link
                );

  virtual ~bluenoc_inpipe();

  virtual bool is_input() const { return true; }

  // -------------------------
  // Application-side API

  unsigned int can_send() const;

  unsigned int try_send(uint32_t            byte_offset,
                        uint32_t            num_elems,
                        const unsigned char* data,
			bool                eom,
                        void              (*callback)(void*),
                        void*               callback_arg
                        );

  unsigned int try_flush(void (*callback)(void*), void* callback_arg);

  virtual bool in_flush_state();

  virtual bool set_auto_flush(bool enable);
  virtual void debug();

  // -------------------------
  // Link-side API

  void put_credits(unsigned int amount, bool underflow);
  virtual void process_notification();
};

#endif // __BLUENOC_PIPE_H__
