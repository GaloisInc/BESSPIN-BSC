#include <stdio.h>
#include <stdlib.h>
#include <list>
#include <algorithm>

#include "SceMiPipe.h"

#define DEBUG false

// ------------------------------------------------------------
// implementation structures

struct tNotification {
  tNotification* prev;
  tNotification* next;
  SceMiPipe*     pipe_hdl;
  void*          context;
  unsigned int   threshold;
  c_callback_fn* fn;
};
typedef struct tNotification tNotification;

void* get_notification_context(struct tNotification* ptr)
{
  return ptr->context;
}

SceMiPipe* get_notification_pipe(struct tNotification* ptr)
{
  return ptr->pipe_hdl;
}

// ------------------------------------------------------------
// class SceMiPipe

SceMiPipe::SceMiPipe(const char*  xactor_name,
                     const char*  pipe_name,
                     unsigned int pipe_num,
                     unsigned int elem_bits,
                     unsigned int pipe_depth,
                     Visibility   vis,
                     Link*        link)
  : m_xactor_name(xactor_name),
    m_pipe_name(pipe_name),
    m_pipe_num(pipe_num),
    m_elem_bits(elem_bits),
    m_elem_bytes((elem_bits + 7) / 8),
    m_depth(pipe_depth),
    m_visibility(vis),
    m_link(link),
    m_notify_hd(NULL),
    m_notify_tl(NULL),
    m_pending_notify(false),
    m_flush_recvd(false)
{
  // construct endpoint path
  int res;
  if (strcmp(xactor_name, "") == 0)
    res = asprintf(&m_endpoint_path, "%s", pipe_name);
  else
    res = asprintf(&m_endpoint_path, "%s.%s", xactor_name, pipe_name);
  if (res == -1) {
    perror("allocating string for endpoint_path");
    exit(EXIT_FAILURE);
  }

  pthread_mutex_init(&m_lock, NULL);
}

SceMiPipe::~SceMiPipe()
{
  // this string should always be non-NULL
  if (m_endpoint_path != NULL)
    free(m_endpoint_path);

  pthread_mutex_destroy(&m_lock);
}

tNotification* SceMiPipe::request_notification(c_callback_fn* fn, void* ctx, unsigned int threshold)
{
  if (fn == NULL) return NULL;

  pthread_mutex_lock(&m_lock);

  tNotification* hdl = (tNotification*) malloc(sizeof(tNotification));
  if (hdl != NULL) {
    hdl->prev = m_notify_tl;
    hdl->next = NULL;
    if (m_notify_hd == NULL)
      m_notify_hd = hdl;
    if (m_notify_tl != NULL)
      m_notify_tl->next = hdl;
    m_notify_tl = hdl;
    hdl->pipe_hdl  = this;
    hdl->context   = ctx;
    hdl->threshold = threshold;
    hdl->fn        = fn;
  }

  pthread_mutex_unlock(&m_lock);

  return hdl;
}

void SceMiPipe::cancel_notification(tNotification* hdl)
{
  if (hdl == NULL) return;

  pthread_mutex_lock(&m_lock);

  if (hdl->prev != NULL)
    hdl->prev->next = hdl->next;
  else
    m_notify_hd = hdl->next;
  if (hdl->next != NULL)
    hdl->next->prev = hdl->prev;
  else
    m_notify_tl = hdl->prev;

  // instead of freeing the memory, we set the callback function to NULL.
  // then we can free the memory in one place to avoid race conditions that
  // would lead to a double-free.
  hdl->fn = NULL;

  pthread_mutex_unlock(&m_lock);
}

void SceMiPipe::do_notification(unsigned int trigger_level, bool do_all)
{
  tNotification* ptr  = NULL;
  tNotification* next = NULL;
  c_callback_fn* fn        = NULL;
  void*          ctx       = NULL;
  unsigned int   threshold = 0;

  pthread_mutex_lock(&m_lock);
  ptr = m_notify_hd;
  while (ptr != NULL) {
    next      = ptr->next;
    fn        = ptr->fn;
    ctx       = ptr->context;
    threshold = ptr->threshold;
    if (fn == NULL)
      free(ptr); // this is a notification that has been cancelled
    else if (do_all || (trigger_level >= ptr->threshold)) {
      if (threshold != 0) // this is a dynamic notification, so remove it
        cancel_notification(ptr);
      pthread_mutex_unlock(&m_lock);
      fn(ctx);
      pthread_mutex_lock(&m_lock);
    }
    ptr = next;
  }
  pthread_mutex_unlock(&m_lock);
}

void SceMiPipe::set_user_data(void* key, void* value)
{
  tUserData d;
  d.key = key;
  d.value = value;
  m_user_data.push_front(d);
}

void* SceMiPipe::get_user_data(void* key)
{
  std::list<tUserData>::iterator p = m_user_data.begin();
  while (p != m_user_data.end()) {
    tUserData d = *p;
    if (d.key == key)
      return d.value;
    ++p;
  }
  return NULL;
}

void SceMiPipe::set_pending_notify( bool flush )
{
  m_pending_notify = true;
  m_flush_recvd = flush;
}

void SceMiPipe::debug()
{
  fprintf(stderr, "SceMiPipe %s %s num=%d, depth=%d",
          m_xactor_name, m_pipe_name, m_pipe_num, m_depth);
}


// forward declarations of callback helper functions
static void output_pipe_handle_data(void* pipe, const char* buf, unsigned int len, bool overflow, bool flush, bool eom);
static void input_pipe_handle_credits(void* pipe, unsigned int amount, bool underflow);

// ------------------------------------------------------------
// class SceMiOutputPipe
//
// This is the consumer side of an output pipe

SceMiOutputPipe::SceMiOutputPipe(const char*  xactor_name,
                                 const char*  pipe_name,
                                 unsigned int pipe_num,
                                 unsigned int elem_bits,
                                 unsigned int pipe_depth,
                                 Visibility   vis,
                                 Link*        link)
  : SceMiPipe(xactor_name, pipe_name, pipe_num, elem_bits, pipe_depth, vis, link)
  , m_elem_count   (0)
  , m_credits       (0)
  , m_underflow     (false)
  , m_active        (false)
  , m_pending_send  (false)
  , m_pending_flush (false)
  , m_autoflush     (false)
  , m_dataQ         ()
  , m_eomQ          ()
  , m_current_elem_bytes (0)
{
  // register the pipe with the link, to receive data messages
  link->set_data_handler(pipe_num,output_pipe_handle_data,(void*)this);
}

SceMiOutputPipe::~SceMiOutputPipe()
{
}

// -------------------------
// Application-side API

unsigned int SceMiOutputPipe::can_receive()
{
  // This is safe without acquiring the mutex, since a concurrent
  // operation from the link can only increase the number of
  // elements available.
  if  (m_visibility == DEFERRED && !m_active) return 0;
  return m_elem_count;
}

unsigned int SceMiOutputPipe::try_receive(unsigned int  byte_offset,
                                          unsigned int  num_elems,
                                          unsigned char* data,
                                          bool*         eom,
                                          bool*         flush,
                                          void        (*callback)(void*),
                                          void*         callback_arg
                                          )
{
  if (num_elems < 1)
    return 0;

  pthread_mutex_lock(&m_lock);

  unsigned int yielded           = 0;
  bool         return_credits    = false;
  bool         fails             = false;
  unsigned int credits_to_return = 0;
  bool         is_flush          = m_pending_flush;

  unsigned int elems_available = m_eomQ.next_eom();
  bool empties = false;

  if (m_visibility == DEFERRED) {
    yielded        = !m_active ? 0 : std::min(num_elems,elems_available);
    empties        = (m_elem_count != 0) && (yielded == m_elem_count);
    fails          = (yielded == m_elem_count) && (num_elems > yielded);
    return_credits = empties || fails ;

    if (return_credits || fails) {
      credits_to_return = m_credits + yielded;
      m_credits = 0;
    } else {
      m_credits += yielded;
    }

    m_active        &= !empties;
    m_pending_send  &= !return_credits;
    m_pending_flush &= !return_credits;


    // fprintf(stderr, "%s yielded= %d (%d), RC=%d, RF=%d, m_act=%d, PF=%d empty=%d fails=%d\n",
    //         __func__, yielded, m_elem_count, return_credits, fails, m_active, m_pending_flush,
    //         empties, fails
    //         );

  } else {
    yielded           = std::min(num_elems,elems_available);
    return_credits    = (yielded != 0);
    credits_to_return = yielded;
  }

  m_elem_count -= yielded;
  if ((yielded != num_elems) && (m_elem_count == 0))
    m_underflow = true;

  // copy data to user buffer
  bool is_eom = false;
  if (yielded != 0) {
    m_dataQ.copyFromFront( data + byte_offset, yielded * m_elem_bytes);
    m_dataQ.pop ( yielded * m_elem_bytes );
    is_eom = m_eomQ.take_eom(yielded);
  }
  *eom = is_eom;

  is_flush &= (m_elem_count == 0);
  if (flush)
    *flush = is_flush;

  if (callback && (yielded != num_elems) && !is_eom && !is_flush)
    callback(callback_arg);

  pthread_mutex_unlock(&m_lock);

  if (return_credits || fails) {
    // send msg via link
    m_link->send_credits(m_pipe_num,credits_to_return,fails);
  }

  return yielded;
} // try_receive()

bool SceMiOutputPipe::in_flush_state()
{
  // It is OK to read this value without acquiring the lock, since we
  // are not updating it atomically.
  return m_pending_flush;
}

bool SceMiOutputPipe::set_auto_flush(bool enable)
{
  // no mutex needed to read/modify autoflush because
  // it is not modified anywhere else
  bool res = m_autoflush;
  if (enable != m_autoflush) {
    m_autoflush = enable;
    // send msg via link
    m_link->send_autoflush(m_pipe_num,enable);
  }
  return res;
}

// -------------------------
// Link-side API

static void output_pipe_handle_data(void* pipe, const char* buf, unsigned int len, bool overflow, bool flush, bool eom)
{
  SceMiOutputPipe* outpipe = (SceMiOutputPipe*)pipe;
  outpipe->put_data(buf,len,overflow,flush, eom);
}

void SceMiOutputPipe::put_data(const char* buf, unsigned int len, bool overflow, bool flush, bool eom)
{
  pthread_mutex_lock(&m_lock);

  //fprintf(stderr, "%s %s %d ", __func__, m_pipe_name, len);
  bool         do_callback     = false;

  // copy data from message into pipe buffer
  m_dataQ.push (buf, len);
  // Figure out how many elements we now have
  m_current_elem_bytes += len;
  unsigned int completed_elems =  m_current_elem_bytes/m_elem_bytes ;
  m_current_elem_bytes = m_current_elem_bytes % m_elem_bytes ;

  assert (!(eom) || (completed_elems != 0));
  if (eom) {
    m_eomQ.add_eom (false, completed_elems - 1);
    m_eomQ.add_eom (true, 1);
  } else {
    m_eomQ.add_eom (false, completed_elems);
  }

  // update pipe state
  bool end_underflow =  m_underflow && (completed_elems != 0);

  do_callback = ((completed_elems != 0) && (m_underflow)) || flush ;
  m_elem_count += completed_elems;
  m_underflow  &= (completed_elems == 0); // underflow is over if we have at least 1 new elem

  if (m_visibility == DEFERRED) {
    m_active        |= overflow || flush || end_underflow ;
    m_pending_send  |= overflow;
    m_pending_flush |= flush;

    do_callback = flush || overflow || end_underflow ;

    // fprintf(stderr, "%s %s %d ov=%d flush=%d eom=%d",
    //         __func__, m_pipe_name, len, overflow, flush, eom);
    // fprintf(stderr, "  credits=%d  elem_cnt=%d doCB=%d PF=%d\n",
    //         m_credits, m_elem_count, do_callback, m_pending_flush
    //         );
  }

  pthread_mutex_unlock(&m_lock);

  //fprintf(stderr, "%s %s %d %d %d\n", __func__, m_pipe_name, len, do_callback, m_visibility);
  // call notification callback if required
  if (do_callback) {
    set_pending_notify(flush);
  }
}

void SceMiOutputPipe::process_notification()
{
  if (m_pending_notify) {
    do_notification (m_elem_count, m_flush_recvd);
    m_pending_notify = false;
    m_flush_recvd = false;
  }
}



void SceMiOutputPipe::debug()
{
  SceMiPipe::debug();
  fprintf(stderr, "-> Output act=%d, psnd=%d elem_cnt=%d, credits=%d\n",
          m_active, m_pending_send, m_elem_count, m_credits);
}


// ------------------------------------------------------------
// class SceMiInputPipe
//
// This is the producer side of an input pipe

SceMiInputPipe::SceMiInputPipe(const char*  xactor_name,
                               const char*  pipe_name,
                               unsigned int pipe_num,
                               unsigned int elem_bits,
                               unsigned int pipe_depth,
                               Visibility   vis,
                               Link*        link)
  : SceMiPipe(xactor_name, pipe_name, pipe_num, elem_bits, pipe_depth, vis, link)
{
  m_elem_count   = 0;
  m_credits      = pipe_depth;
  m_overflow     = false;
  m_flushing     = false;
  m_active       = true;
  m_pending_recv = false;
  m_autoflush    = false;

  // register the pipe with the link, to receive credit messages
  link->set_credit_handler(pipe_num,input_pipe_handle_credits,(void*)this);
}

SceMiInputPipe::~SceMiInputPipe()
{
}

// -------------------------
// Application-side API

unsigned int SceMiInputPipe::can_send() const
{
  // This is safe without acquiring the mutex, since a concurrent
  // operation from the link can only increase the number of credits.
  return (m_flushing || ((m_visibility == DEFERRED) && !m_active)) ? 0 : m_credits;
}

unsigned int SceMiInputPipe::try_send(unsigned int        byte_offset,
                                      unsigned int        num_elems,
                                      const unsigned char* data,
                                      bool                eom,
                                      void              (*callback)(void*),
                                      void*               callback_arg
                                      )
{
  unsigned int res = 0;

  if (num_elems < 1)
    return 0;

  if (!m_flushing) {
    unsigned int num_accepted = 0;
    bool         send_data    = false;
    bool         send_fail    = false;
    bool         do_flush     = false;

    pthread_mutex_lock(&m_lock);

    // calculate accepted elements and whether to send to consumer
    if (m_visibility == DEFERRED) {
      num_accepted = (!m_active) ? 0 : std::min(m_credits,num_elems);
      send_data    =  ((m_elem_count + num_accepted) == m_depth)
                   && ((num_accepted != num_elems) || m_pending_recv)
                   ;
      send_fail    = (num_accepted != num_elems) && !m_overflow;
    } else {
      num_accepted = std::min(m_credits,num_elems);
      send_data    = num_accepted != 0;
    }

    // add the accepted data to the link's message buffer
    if (num_accepted != 0) {
      m_link->add_message_data( m_pipe_num, m_elem_bytes
                              , num_accepted, data, byte_offset
                              , eom && (num_accepted == num_elems)
                              );
    }

    if (  eom
       && (num_accepted == num_elems)
       && m_autoflush
       )
      do_flush = true;


    // if we want to send at this point, call the link to send the message buffer contents
    if (send_data || send_fail || do_flush) {
      m_link->send_data(m_pipe_num, send_fail, do_flush);
    }

    // update producer state
    m_elem_count    = (send_data || send_fail || do_flush) ? 0 : (m_elem_count + num_accepted);
    m_credits      -= num_accepted;
    m_overflow      = (m_overflow && !do_flush) || (num_accepted != num_elems);
    m_active       &= !send_data;
    m_flushing      = do_flush;

    res = num_accepted;

    if (callback && (num_accepted != num_elems)) {
      callback(callback_arg);
    }

    pthread_mutex_unlock(&m_lock);
  }

  return res;
} // try_send()


unsigned int SceMiInputPipe::try_flush(void (*callback)(void*), void* callback_arg)
{
  int res = 0;
  bool do_flush = false;

  pthread_mutex_lock(&m_lock);

  if (m_credits == m_depth)
    res = 1;
  else if (m_flushing)
    res = 0;
  else {
    do_flush = true;
    // update producer state
    m_elem_count = 0;
    m_overflow   = false;
    m_flushing   = true;

    res = 0;
  }

  if (callback && (res == 0))
    callback(callback_arg);

  if (do_flush) {
    // send msg via link
    m_link->send_data(m_pipe_num,false,true);
  }

  pthread_mutex_unlock(&m_lock);

  return res;
} // try_flush()

bool SceMiInputPipe::in_flush_state()
{
  // It is OK to read this value without acquiring the lock, since we
  // are not updating it atomically.
  return m_flushing;
}

bool SceMiInputPipe::set_auto_flush(bool enable)
{
  // no mutex needed to read/modify autoflush because
  // it is not modified anywhere else
  bool res = m_autoflush;
  if (enable != m_autoflush) {
    m_autoflush = enable;
  }
  return res;
}

// -------------------------
// Link-side API

static void input_pipe_handle_credits(void* pipe, unsigned int amount, bool underflow)
{
  SceMiInputPipe* inpipe = (SceMiInputPipe*)pipe;
  inpipe->put_credits(amount,underflow);
}

void SceMiInputPipe::put_credits(unsigned int amount, bool underflow)
{
  pthread_mutex_lock(&m_lock);


  unsigned int new_credits  = m_credits + amount;
  bool         send_data    = false;
  bool         became_empty = (new_credits == m_depth) && (amount != 0);
  bool         end_overflow = false;
  bool         do_callback  = false;

  if (m_visibility == DEFERRED) {
    send_data    = (m_elem_count == m_depth) && underflow;
    end_overflow = m_overflow && (amount != 0);

    // if we want to send at this point, call the link to send the message buffer contents
    if (send_data) {
      m_link->send_data(m_pipe_num, false, false);
      m_elem_count = 0;
    }
  } else {
    unsigned int threshold = (m_visibility == FIFO) ? 1 : m_depth;
    end_overflow = m_overflow && (new_credits >= threshold);
  }

  do_callback = end_overflow || (((m_visibility == DEFERRED) || m_flushing) && became_empty);

  m_credits       = new_credits;
  m_overflow     &= !end_overflow;
  m_flushing     &= !became_empty;
  m_active       |= (amount != 0);


  m_pending_recv  = (m_pending_recv || underflow) && !send_data;

  pthread_mutex_unlock(&m_lock);

  // call notification callback if required
  if (do_callback)
    set_pending_notify(false);
}

void SceMiInputPipe::process_notification()
{
  if (m_pending_notify) {
    // credit message
    do_notification (m_credits, false);
    m_pending_notify = false;
    m_flush_recvd = false;
  }
}

void SceMiInputPipe::debug()
{
  SceMiPipe::debug();
  fprintf(stderr, "-> Input act=%d, ovrflw=%d pendr=%d, elem_cnt=%d, credits=%d\n",
          m_active, m_overflow, m_pending_recv, m_elem_count, m_credits);
}
