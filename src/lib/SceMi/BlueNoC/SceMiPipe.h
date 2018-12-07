#ifndef __SCEMI_PIPE_H__
#define __SCEMI_PIPE_H__

#include <cstdlib>
#include <list>
#include <vector>
#include <pthread.h>

#include "Link.h"

#include "svdpi.h"
#include "DataBufferQ.h"

typedef enum { DEFERRED, IMMEDIATE, FIFO, INVALID } Visibility;

typedef struct {
  void* key;
  void* value;
} tUserData;

extern "C" {
  typedef void c_callback_fn(void*);
}

struct tNotification;

class SceMiPipe;

void* get_notification_context(struct tNotification*);
SceMiPipe* get_notification_pipe(struct tNotification*);

class SceMiPipe {
 protected:
  // transactor hierarchy
  const char *m_xactor_name;
  const char *m_pipe_name;
  char *m_endpoint_path;

  // pipe number
  const unsigned int m_pipe_num;

  // number of bits in an element
  const unsigned int m_elem_bits;
  // number of bytes in an element
  const unsigned int m_elem_bytes;
  // number of elements deep
  const unsigned int m_depth;
  // visibility mode (0 = Deferred, 1 = Immediate, 2 = Fifo)
  const Visibility m_visibility;

  // link used to transmit and receive messages
  Link* m_link;

  // notification requests
  tNotification* m_notify_hd;
  tNotification* m_notify_tl;

  bool m_pending_notify;
  bool m_flush_recvd;

  // mutex which protects the pipe state
  pthread_mutex_t m_lock;

 private:
  // user data
  std::list<tUserData> m_user_data;

 private:
  // prevent initialization without arguments by making this private
  SceMiPipe();
  SceMiPipe(const SceMiPipe & );
 protected:
  SceMiPipe(const char*  xactor_name,
            const char*  pipe_name,
            unsigned int pipe_num,
            unsigned int elem_bits,
            unsigned int pipe_depth,
            Visibility   visibility,
            Link*        link);
 public:
  virtual ~SceMiPipe();

  virtual bool is_input() const = 0;

  const char* endpoint_path() const { return m_endpoint_path; }

  unsigned int num() const { return m_pipe_num; }

  unsigned int bits_per_element() const { return m_elem_bits; }

  unsigned int bytes_per_element() const { return m_elem_bytes; }

  unsigned int buffer_depth() const { return m_depth; }

  Visibility visibility() const { return m_visibility; }

  virtual bool in_flush_state() = 0;

  virtual bool set_auto_flush(bool enable) = 0;

  virtual void debug();

  tNotification* request_notification(c_callback_fn* fn, void* ctx, unsigned int threshold);
  void cancel_notification(tNotification* hdl);

  void set_user_data(void* key, void* value);
  void* get_user_data(void* key);

  virtual void process_notification() = 0;

 protected:
  void do_notification(unsigned int trigger_level, bool do_all);

  // Notification related to link side
  void set_pending_notify( bool flush );

}; // class SceMiPipe


class SceMiOutputPipe: public SceMiPipe {
 private:
  // pipe state
  unsigned int m_elem_count;
  unsigned int m_credits;
  bool m_underflow;
  bool m_active;
  bool m_pending_send;
  bool m_pending_flush;
  bool m_autoflush;

  // buffered element state
  DataBufferQ m_dataQ;
  EomQ        m_eomQ;

  unsigned int m_current_elem_bytes;

 public:
  SceMiOutputPipe(const char*  xactor_name,
                  const char*  pipe_name,
                  unsigned int pipe_num,
                  unsigned int elem_bits,
                  unsigned int pipe_depth,
                  Visibility   visibility,
                  Link*        link
                );

  virtual ~SceMiOutputPipe();

  virtual bool is_input() const { return false; }

  // -------------------------
  // Application-side API

  unsigned int can_receive();

  unsigned int try_receive(unsigned int  byte_offset,
                           unsigned int  num_elems,
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

  void put_data(const char* buf, unsigned int len, bool overflow, bool flush, bool eom);
  virtual void process_notification();

}; // class SceMiOutputPipe

class SceMiInputPipe: public SceMiPipe {
 private:
  // pipe state
  unsigned int m_elem_count;
  unsigned int m_credits;
  bool m_overflow;
  bool m_flushing;
  bool m_active;
  bool m_pending_recv;
  bool m_autoflush;

 public:
  SceMiInputPipe(const char*  xactor_name,
                 const char*  pipe_name,
                 unsigned int pipe_num,
                 unsigned int elem_bits,
                 unsigned int pipe_depth,
                 Visibility   visibility,
                 Link*        link
                );

  virtual ~SceMiInputPipe();

  virtual bool is_input() const { return true; }

  // -------------------------
  // Application-side API

  unsigned int can_send() const;

  unsigned int try_send(unsigned int        byte_offset,
                        unsigned int        num_elems,
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

}; // class SceMiInputPipe


#endif // __SCEMI_PIPE_H__
