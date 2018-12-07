#include <cstdio>
#include <cstdlib>
#include <pthread.h>
#include <iostream>

#include "bluenoc_pipes.h"
#include "bluenoc.h"

static bluenoc_inpipe *requireInputPipe(const char *location, void *pipe_handle)
{
  if (pipe_handle != NULL) {
    bluenoc_pipe *pipe = (bluenoc_pipe*)pipe_handle;
    if (pipe->is_input()) {
      return (bluenoc_inpipe*)pipe;
    }
  }
  std::cerr << "ERROR: " << location << " argument is not an input pipe!" << std::endl;
  return NULL;
}

static bluenoc_outpipe *requireOutputPipe(const char *location, void *pipe_handle)
{
  if (pipe_handle != NULL) {
    bluenoc_pipe *pipe = (bluenoc_pipe*)pipe_handle;
    if (!pipe->is_input()) {
      return (bluenoc_outpipe*)pipe;
    }
  }
  std::cerr << "ERROR: " << location << " argument is not an output pipe!" << std::endl;
  return NULL;
}

typedef struct {
  pthread_mutex_t ev_mutex;
  pthread_cond_t  ev_cond_var;
  bool            triggered;
} event_t;

static event_t *create_event()
{
  event_t *ev = (event_t*)malloc(sizeof(event_t));
  pthread_mutex_init(&(ev->ev_mutex), NULL);
  pthread_cond_init(&(ev->ev_cond_var), NULL);
  ev->triggered = false;
  return ev;
}

static void wait_for_event(event_t *ev)
{
  if (ev == NULL) return;
  pthread_mutex_lock(&(ev->ev_mutex));
  while (!ev->triggered) {
    pthread_cond_wait(&(ev->ev_cond_var), &(ev->ev_mutex));
  }
  ev->triggered = false;
  pthread_mutex_unlock(&(ev->ev_mutex));
}

static void signal_event(event_t *ev)
{
  if (ev == NULL) return;
  pthread_mutex_lock(&(ev->ev_mutex));
  ev->triggered = true;
  pthread_cond_broadcast(&(ev->ev_cond_var));
  pthread_mutex_unlock(&(ev->ev_mutex));
}

static void signal_event_void_ptr(void *ptr)
{
  event_t *ev = (event_t*)ptr;
  signal_event(ev);
}

static void clear_event(event_t *ev)
{
  if (ev == NULL) return;
  pthread_mutex_lock(&(ev->ev_mutex));
  ev->triggered = false;
  pthread_mutex_unlock(&(ev->ev_mutex));
}

static void clear_event_void_ptr(void *ptr)
{
  event_t *ev = (event_t*)ptr;
  clear_event(ev);
}

extern "C" {

  void *bluenoc_pipe_c_handle(const char *endpoint_path)
  {
    bluenoc *bluenoc = bluenoc::Pointer();
    if (!bluenoc)
      return NULL;

    bluenoc_pipe *pipe = bluenoc->get_bluenoc_pipe(endpoint_path);
    return (void*)(pipe);
  }

  bool bluenoc_pipe_get_direction(void *pipe_handle)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    if (pipe->is_input())
      return true;
    else
      return false;
  }

  int bluenoc_pipe_get_depth(void *pipe_handle)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    return (int)(pipe->buffer_depth());
  }

  int bluenoc_pipe_get_bytes_per_element(void *pipe_handle)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    return (int)(pipe->bytes_per_element());
  }

  int bluenoc_pipe_get_bits_per_element(void *pipe_handle)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    return (int)(pipe->bits_per_element());
  }

  void bluenoc_pipe_c_send(void *pipe_handle, int num_elements, const uint8_t *data, bool eom)
  {
    int byte_offset = 0;
    int elements_sent;

    bluenoc_inpipe *inpipe = requireInputPipe(__func__, pipe_handle);
    event_t *ev = (event_t*)bluenoc_pipe_get_user_data(pipe_handle, NULL);

    // if event has not been set up, do that now
    if (ev == NULL) {
      ev = create_event();
      // Call direct to pipe function...
      bluenoc_pipe_put_user_data(pipe_handle, NULL, (void*)ev);
      bluenoc_pipe_set_notify_callback( pipe_handle, signal_event_void_ptr, (void*)ev, 0 );
    }

    while (num_elements != 0) {
      elements_sent = inpipe->try_send( byte_offset, num_elements, (const uint8_t*)data, eom, clear_event_void_ptr, ev);

      // if pipe is full, wait until OK to send more
      if (elements_sent == 0) {
	wait_for_event(ev);
      } else {
	byte_offset += elements_sent * inpipe->bytes_per_element();
	num_elements -= elements_sent;
      }
    }
  }

  void bluenoc_pipe_c_receive(void *pipe_handle, int num_elements, int *num_elements_valid, uint8_t *data, bool *eom)
  {
    int elements_received;
    int byte_offset = 0;
    bool is_flushing = false;
    bool is_eom = false;
    
    bluenoc_outpipe *outpipe = requireOutputPipe(__func__, pipe_handle);
    event_t *ev = (event_t*)bluenoc_pipe_get_user_data(pipe_handle, NULL);

    // if event has not been setup, do that now
    if (ev == NULL) {
      ev = create_event();
      bluenoc_pipe_put_user_data(pipe_handle, NULL, (void*)ev);
      bluenoc_pipe_set_notify_callback(pipe_handle, signal_event_void_ptr, (void*)ev, 0);
    }

    *num_elements_valid = 0;
    while(num_elements != 0) {
      elements_received = outpipe->try_receive((uint32_t) byte_offset, (uint32_t)num_elements, (uint8_t*)data, &is_eom, &is_flushing, clear_event_void_ptr, ev);
      *num_elements_valid += elements_received;
      *eom = is_eom ? true : false;

      if (*eom || is_flushing) return;

      num_elements -= elements_received;
      if (num_elements > 0) {
	byte_offset += elements_received * bluenoc_pipe_get_bytes_per_element(pipe_handle);
	wait_for_event(ev);
      }
    }
  }

  void bluenoc_pipe_c_flush(void *pipe_handle)
  {
    event_t *ev = (event_t*) bluenoc_pipe_get_user_data(pipe_handle, NULL);
    if (ev == NULL) {
      ev = create_event();
      bluenoc_pipe_put_user_data(pipe_handle, NULL, (void*)ev);
      bluenoc_pipe_set_notify_callback(pipe_handle, signal_event_void_ptr, (void*)ev, 0);
    }

    while(bluenoc_pipe_c_try_flush(pipe_handle, clear_event_void_ptr, ev) == 0)
      wait_for_event(ev);
  }

  int bluenoc_pipe_c_try_send(void *pipe_handle, int byte_offset, int num_elements, const uint8_t *data, bool eom)
  {
    bluenoc_inpipe *inpipe = requireInputPipe(__func__, pipe_handle);
    if (inpipe) {
      return (int)inpipe->try_send( (uint32_t)byte_offset, (uint32_t)num_elements, (const uint8_t *)data, eom, NULL, NULL);
    } else {
      return 0;
    }
  }

  int bluenoc_pipe_c_try_receive(void *pipe_handle, int byte_offset, int num_elements, uint8_t *data, bool *eom)
  {
    bluenoc_outpipe *outpipe = requireOutputPipe(__func__, pipe_handle);
    if (outpipe) {
      bool eom_indicator;
      uint32_t res = outpipe->try_receive((uint32_t)byte_offset, (uint32_t)num_elements, (uint8_t *)data, &eom_indicator, NULL, NULL, NULL);
      *eom = eom_indicator == 1;
      return (int)res;
    } else {
      return 0;
    }
  }

  int bluenoc_pipe_c_try_flush(void *pipe_handle, void (*noflush_callback)(void*), void *noflush_data)
  {
    bluenoc_inpipe *inpipe = requireInputPipe(__func__, pipe_handle);
    if (inpipe) 
      return (int)inpipe->try_flush(noflush_callback, noflush_data);
    else
      return 0;
  }

  bool bluenoc_pipe_c_in_flush_state(void *pipe_handle)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)pipe_handle;
    return pipe->in_flush_state();
    
  }
  
  int bluenoc_pipe_c_can_send(void *pipe_handle)
  {
    bluenoc_inpipe *inpipe = requireInputPipe(__func__, pipe_handle);
    if (inpipe)
      return inpipe->can_send();
    else 
      return 0;
  }

  int bluenoc_pipe_c_can_receive(void *pipe_handle)
  {
    bluenoc_outpipe *outpipe = requireOutputPipe(__func__, pipe_handle);
    if (outpipe)
      return outpipe->can_receive();
    else
      return 0;
  }
  
  bluenoc_pipe_notify_callback_handle bluenoc_pipe_set_notify_callback(void *pipe_handle,
								       bluenoc_pipe_notify_callback notify_callback,
								       void *notify_context,
								       int callback_threshold)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    return pipe->request_notification(notify_callback, notify_context, callback_threshold);
  }

  void bluenoc_pipe_clear_notify_callback(bluenoc_pipe_notify_callback_handle notify_callback_handle)
  {
    notification_t *hdl = (notification_t*)notify_callback_handle;
    bluenoc_pipe *pipe = get_notification_pipe(hdl);
    pipe->cancel_notification(hdl);
  }

  void *bluenoc_pipe_get_notify_context(bluenoc_pipe_notify_callback_handle notify_callback_handle)
  {
    notification_t *hdl = (notification_t*)notify_callback_handle;
    return get_notification_context(hdl);
  }

  void bluenoc_pipe_put_user_data(void *pipe_handle, void *user_key, void *user_data)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    pipe->set_user_data(user_key, user_data);
  }

  void *bluenoc_pipe_get_user_data(void *pipe_handle, void *user_key)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    return pipe->get_user_data(user_key);
  }

  bool bluenoc_pipe_set_eom_auto_flush(void *pipe_handle, bool enabled)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    return pipe->set_auto_flush(enabled);
  }

  void bluenoc_pipe_debug(void *pipe_handle)
  {
    bluenoc_pipe *pipe = (bluenoc_pipe*)(pipe_handle);
    pipe->debug();
  }
};

