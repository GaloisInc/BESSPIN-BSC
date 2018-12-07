#include <iostream>
#include "bluenoc_pipe.h"

#define DEBUG (false)

struct notification_t{
  notification_t *prev;
  notification_t *next;
  bluenoc_pipe   *pipe_hdl;
  void           *context;
  uint32_t        threshold;
  c_callback_fn  *fn;
};

typedef struct notification_t notification_t;

void *get_notification_context(notification_t *ptr) 
{
  return ptr->context;
}

bluenoc_pipe* get_notification_pipe(notification_t *ptr)
{
  return ptr->pipe_hdl;
}


bluenoc_pipe::bluenoc_pipe(const char *   xactor_name,
			   const char *   pipe_name,
			   uint32_t       nodeid,
			   uint32_t       elem_bits,
			   uint32_t       pipe_depth,
			   bluenoc_link * link)
  : m_xactor_name(xactor_name)
  , m_pipe_name(pipe_name)
  , m_nodeid(nodeid)
  , m_elem_bits(elem_bits)
  , m_elem_bytes((elem_bits+7)/8)
  , m_depth(pipe_depth)
  , m_link(link)
  , m_notify_hd(NULL)
  , m_notify_tl(NULL)
  , m_pending_notify(false)
  , m_flush_recvd(false)
{
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

bluenoc_pipe::~bluenoc_pipe()
{
  if (m_endpoint_path != NULL) {
    free(m_endpoint_path);
  }

  pthread_mutex_destroy(&m_lock);
}

notification_t *bluenoc_pipe::request_notification(c_callback_fn *fn, void *ctx, uint32_t threshold)
{
  if (fn == NULL) return NULL;

  pthread_mutex_lock(&m_lock);

  notification_t *hdl = (notification_t *)malloc(sizeof(notification_t));
  if (hdl != NULL) {
    hdl->prev = m_notify_tl;
    hdl->next = NULL;
    if (m_notify_hd == NULL) 
      m_notify_hd = hdl;
    if (m_notify_tl != NULL)
      m_notify_tl->next = hdl;
    m_notify_tl = hdl;
    hdl->pipe_hdl = this;
    hdl->context  = ctx;
    hdl->threshold = threshold;
    hdl->fn = fn;
  }

  pthread_mutex_unlock(&m_lock);

  return hdl;
}

void bluenoc_pipe::cancel_notification(notification_t *hdl)
{
  if (hdl == NULL) return;

  pthread_mutex_lock(&m_lock);

  if (hdl->prev != NULL) {
    hdl->prev->next = hdl->next;
  } else {
    m_notify_hd = hdl->next;
  }

  if (hdl->next != NULL) {
    hdl->next->prev = hdl->prev;
  } else {
    m_notify_tl = hdl->prev;
  }

  // instead of freeing the memory, we set the callback function to NULL.
  // then we can free the memory in one place to avoid race conditions that
  // would lead to a double-free.
  hdl->fn = NULL;

  pthread_mutex_unlock(&m_lock);
}

void bluenoc_pipe::do_notification(uint32_t trigger_level, bool do_all)
{
  notification_t *ptr = NULL;
  notification_t *next = NULL;
  c_callback_fn *fn = NULL;
  void *ctx = NULL;
  uint32_t threshold = 0;

  pthread_mutex_lock(&m_lock);

  ptr = m_notify_hd;
  while(ptr != NULL) {
    next = ptr->next;
    fn   = ptr->fn;
    ctx  = ptr->context;
    threshold = ptr->threshold;
    if (fn == NULL) {
      free(ptr); // cancelled notification
    } else if (do_all || (trigger_level >= ptr->threshold)) {
      if (threshold != 0) { // this is a dynamic notification, so remove it
	cancel_notification(ptr);
      }
      pthread_mutex_unlock(&m_lock);
      fn(ctx);
      pthread_mutex_lock(&m_lock);
    }
    ptr = next;    
  }
  pthread_mutex_unlock(&m_lock);
}

void bluenoc_pipe::set_user_data(void *key, void *value)
{
  user_data_t d;
  d.key = key;
  d.value = value;
  m_user_data.push_front(d);
}

void *bluenoc_pipe::get_user_data(void *key)
{
  std::list<user_data_t>::iterator p = m_user_data.begin();
  while(p != m_user_data.end()) {
    user_data_t d = *p;
    if (d.key == key) {
      return d.value;
    }
    ++p;
  }
  return NULL;
}

void bluenoc_pipe::set_pending_notify(bool flush)
{
  m_pending_notify = true;
  m_flush_recvd = flush;
}

void bluenoc_pipe::debug()
{
  fprintf(stderr, "bluenoc_pipe %s %s nodeid=%d depth=%d", m_xactor_name, m_pipe_name, m_nodeid, m_depth);
}

// forward declarations of callback helper functions
static void out_pipe_handle_data(void *pipe, const char *buf, uint32_t len, bool overflow, bool flush, bool eom);
static void in_pipe_handle_credits(void *pipe, uint32_t amount, bool underflow);

bluenoc_outpipe::bluenoc_outpipe(const char*  xactor_name,
				 const char*  pipe_name,
				 uint32_t     nodeid,
				 uint32_t     elem_bits,
				 uint32_t     pipe_depth,
				 bluenoc_link* link
				 )
  : bluenoc_pipe(xactor_name, pipe_name, nodeid, elem_bits, pipe_depth, link)
  , m_elem_count(0)
  , m_credits(0)
  , m_underflow(false)
  , m_active(false)
  , m_pending_send(false)
  , m_pending_flush(false)
  , m_autoflush(false)
  , m_dataQ()
  , m_eomQ()
  , m_current_elem_bytes(0)
{
  link->set_data_handler(nodeid, out_pipe_handle_data, (void*)this);
}

bluenoc_outpipe::~bluenoc_outpipe()
{
}

uint32_t bluenoc_outpipe::can_receive()
{
  // This is safe without acquiring the mutex since a concurrent operation
  // from the link can only increase the number of elements available
  return m_elem_count;
}

uint32_t bluenoc_outpipe::try_receive(uint32_t      byte_offset,
				      uint32_t      num_elems,
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

  uint32_t  yielded           = 0;
  bool      return_credits    = false;
  bool      fails             = false;
  uint32_t  credits_to_return = 0;
  bool      is_flush          = m_pending_flush;

  uint32_t  elems_available   = m_eomQ.next_eom();

  yielded = std::min(num_elems, elems_available);
  return_credits = (yielded != 0);
  credits_to_return = yielded;

  m_elem_count -= yielded;
  if ((yielded != num_elems) && (m_elem_count == 0)) {
    m_underflow = true;
  }

  // copy data to user buffer
  bool is_eom = false;
  if (yielded != 0) {
    m_dataQ.copyFromFront(data + byte_offset, yielded * m_elem_bytes);
    m_dataQ.pop(yielded * m_elem_bytes);
    is_eom = m_eomQ.take_eom(yielded);
  }
  *eom = is_eom;

  is_flush &= (m_elem_count == 0);
  if (flush) {
    *flush = is_flush;
  }

  if (callback && (yielded != num_elems) && !is_eom && !is_flush) {
    callback(callback_arg);
  }

  pthread_mutex_unlock(&m_lock);

  if (return_credits || fails) {
    m_link->send_credits(m_nodeid, credits_to_return, fails);
  }

  return yielded;
}

bool bluenoc_outpipe::in_flush_state()
{
  // It is okay to read this value without acquiring the lock since we 
  // are not updating it atomically.
  return m_pending_flush;
}

bool bluenoc_outpipe::set_auto_flush(bool enable)
{
  // no mutex needed to read/modify autoflush because
  // it is not modified anywhere else
  bool res = m_autoflush;
  if (enable != m_autoflush) {
    m_link->send_autoflush(m_nodeid, enable);
  }
  return res;
}

static void out_pipe_handle_data(void *pipe, const char *buf, uint32_t len, bool overflow, bool flush, bool eom)
{
  bluenoc_outpipe *outpipe = (bluenoc_outpipe*)pipe;
  outpipe->put_data(buf, len, overflow, flush, eom);
}

void bluenoc_outpipe::put_data(const char *buf, uint32_t len, bool overflow, bool flush, bool eom)
{
  pthread_mutex_lock(&m_lock);

  bool do_callback = false;

  // copy data from message into pipe buffer
  m_dataQ.push(buf, len);

  // Figure out how many elements we now have
  m_current_elem_bytes += len;
  uint32_t completed_elems = m_current_elem_bytes/m_elem_bytes;
  m_current_elem_bytes = m_current_elem_bytes % m_elem_bytes;

  assert(!(eom) || (completed_elems != 0));
  if (eom) {
    m_eomQ.add_eom(false, completed_elems - 1);
    m_eomQ.add_eom(true, 1);
  } else {
    m_eomQ.add_eom(false, completed_elems);
  }

  // update pipe state
  do_callback = ((completed_elems != 0) && (m_underflow)) || flush;
  m_elem_count += completed_elems;
  m_underflow &= (completed_elems == 0);

  pthread_mutex_unlock(&m_lock);

  if (do_callback) {
    set_pending_notify(flush);
  }
}

void bluenoc_outpipe::process_notification()
{
  if (m_pending_notify) {
    do_notification(m_elem_count, m_flush_recvd);
    m_pending_notify = false;
    m_flush_recvd = false;
  }
}

void bluenoc_outpipe::debug()
{
  bluenoc_pipe::debug();
  fprintf(stderr, "-> Out act=%d psnd=%d elem_cnt=%d credits=%d\n", m_active, m_pending_send, m_elem_count, m_credits);
}

bluenoc_inpipe::bluenoc_inpipe(const char*  xactor_name,
			       const char*  pipe_name,
			       uint32_t     nodeid,
			       uint32_t     elem_bits,
			       uint32_t     pipe_depth,
			       bluenoc_link *link
			       )
  : bluenoc_pipe(xactor_name, pipe_name, nodeid, elem_bits, pipe_depth, link)
  , m_elem_count(0)
  , m_credits(pipe_depth)
  , m_overflow(false)
  , m_flushing(false)
  , m_active(true)
  , m_pending_recv(false)
  , m_autoflush(false)
{
  link->set_credit_handler(nodeid, in_pipe_handle_credits, (void*)this);
}

bluenoc_inpipe::~bluenoc_inpipe()
{
}

uint32_t bluenoc_inpipe::can_send() const
{
  // This is safe without acquiring the mutex, since a concurrent
  // operation from the link can only increase the number of credits.
  return (m_flushing) ? 0 : m_credits;
}

uint32_t bluenoc_inpipe::try_send(uint32_t            byte_offset,
				  uint32_t            num_elems,
				  const unsigned char* data,
				  bool                eom,
				  void              (*callback)(void*),
				  void*               callback_arg
				  )
{
  uint32_t res = 0;

  if (num_elems < 1)
    return 0;

  if (!m_flushing) {
    uint32_t num_accepted = 0;
    bool send_data = false;
    bool send_fail = false;
    bool do_flush  = false;

    pthread_mutex_lock(&m_lock);

    num_accepted = std::min(m_credits, num_elems);
    send_data = num_accepted != 0;

    if (num_accepted != 0) {
      m_link->add_message_data(m_nodeid, m_elem_bytes, num_accepted, data, byte_offset, eom && (num_accepted == num_elems));
    }

    if (eom && (num_accepted == num_elems) && m_autoflush) {
      do_flush = true;
    }

    if (send_data || send_fail || do_flush) {
      m_link->send_data(m_nodeid, send_fail, do_flush);
    }

    m_elem_count = (send_data || send_fail || do_flush) ? 0 : (m_elem_count + num_accepted);
    m_credits   -= num_accepted;
    m_overflow   = (m_overflow && !do_flush) || (num_accepted != num_elems);
    m_active    &= !send_data;
    m_flushing   = do_flush;

    res = num_accepted;

    if (callback && (num_accepted != num_elems)) {
      callback(callback_arg);
    }

    pthread_mutex_unlock(&m_lock);
  }
  return res;
}
	
uint32_t bluenoc_inpipe::try_flush(void (*callback)(void*), void *callback_arg)
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
    m_elem_count = 0;
    m_overflow = false;
    m_flushing = true;

    res = 0;
  }

  if (callback && (res == 0))
    callback(callback_arg);

  if (do_flush) {
    m_link->send_data(m_nodeid, false, true);
  }

  pthread_mutex_unlock(&m_lock);

  return res;
}	

bool bluenoc_inpipe::in_flush_state()
{
  // It is OK to read this value without acquiring the lock, since we
  // are not updating it atomically.
  return m_flushing;
}

bool bluenoc_inpipe::set_auto_flush(bool enable)
{
  // no mutex needed to read/modify autoflush because
  // it is not modified anywhere else
  bool res = m_autoflush;
  if (enable != m_autoflush) {
    m_autoflush = enable;
  }
  return res;
}

static void in_pipe_handle_credits(void *pipe, uint32_t amount, bool underflow)
{
  bluenoc_inpipe *inpipe = (bluenoc_inpipe*)pipe;
  inpipe->put_credits(amount, underflow);
}

void bluenoc_inpipe::put_credits(uint32_t amount, bool underflow)
{
  pthread_mutex_lock(&m_lock);

  uint32_t new_credits = m_credits + amount;
  bool send_data = false;
  bool became_empty = (new_credits == m_depth) && (amount != 0);
  bool end_overflow = false;
  bool do_callback = false;

  uint32_t threshold = 1; // ? why is this not m_depth?
  end_overflow = m_overflow && (new_credits >= threshold);

  do_callback = end_overflow || (m_flushing && became_empty);
  
  m_credits = new_credits;
  m_overflow &= !end_overflow;
  m_flushing &= !became_empty;
  m_active |= (amount != 0);

  m_pending_recv = (m_pending_recv || underflow) && !send_data;

  pthread_mutex_unlock(&m_lock);

  if (do_callback) {
    set_pending_notify(false);
  }
}

void bluenoc_inpipe::process_notification()
{
  if (m_pending_notify) {
    do_notification(m_credits, false);
    m_pending_notify = false;
    m_flush_recvd = false;
  }
}

void bluenoc_inpipe::debug()
{
  bluenoc_pipe::debug();
  fprintf(stderr, "-> In  act=%d ovrflw=%d pendr=%d elem_cnt=%d credits=%d\n", m_active, m_overflow, m_pending_recv, m_elem_count, m_credits);
}
