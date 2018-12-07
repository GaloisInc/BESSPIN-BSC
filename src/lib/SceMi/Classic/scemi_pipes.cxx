#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "scemi_pipes.h"
#include "scemi.h"

/* helper functions for checking pipe direction */

static SceMiInputPipe* requireInputPipe(const char* location, void* pipe_handle)
{
  if (pipe_handle != NULL) {
    SceMiPipe* pipe = (SceMiPipe*) pipe_handle;
    if (pipe->is_input())
      return (SceMiInputPipe*) pipe;
  }
  fprintf(stderr,"ERROR: %s argument is not an input pipe\n", location);
  return NULL;
}

static SceMiOutputPipe* requireOutputPipe(const char* location, void* pipe_handle)
{
  if (pipe_handle != NULL) {
    SceMiPipe* pipe = (SceMiPipe*) pipe_handle;
    if (!pipe->is_input())
      return (SceMiOutputPipe*) pipe;
  }
  fprintf(stderr,"ERROR: %s argument is not an output pipe\n", location);
  return NULL;
}

/* simple single-waiter event facility based on pthread condition
 * variables
 */

typedef struct {
  pthread_mutex_t ev_mutex;
  pthread_cond_t  ev_cond_var;
  bool            triggered;
} tEvent;

static tEvent* create_event()
{
  tEvent* ev = (tEvent*) malloc(sizeof(tEvent));
  pthread_mutex_init(&(ev->ev_mutex),NULL);
  pthread_cond_init(&(ev->ev_cond_var),NULL);
  ev->triggered = false;
  return ev;
}

/*
static void destroy_event(tEvent* ev)
{
  if (ev == NULL) return;
  pthread_mutex_unlock(&(ev->ev_mutex));
  pthread_mutex_destroy(&(ev->ev_mutex));
  pthread_cond_destroy(&(ev->ev_cond_var));
  free(ev);
}
*/

static void wait_for_event(tEvent* ev)
{
  if (ev == NULL) return;
  pthread_mutex_lock(&(ev->ev_mutex));
  while (!ev->triggered) {
    pthread_cond_wait(&(ev->ev_cond_var),&(ev->ev_mutex));
  }
  ev->triggered = false;
  pthread_mutex_unlock(&(ev->ev_mutex));
}

static void signal_event(tEvent* ev)
{
  if (ev == NULL) return;
  pthread_mutex_lock(&(ev->ev_mutex));
  ev->triggered = true;
  pthread_cond_broadcast(&(ev->ev_cond_var));
  pthread_mutex_unlock(&(ev->ev_mutex));
}

static void signal_event_void_ptr(void* ptr)
{
  tEvent* ev = (tEvent*) ptr;
  signal_event(ev);
}

static void clear_event(tEvent* ev)
{
  if (ev == NULL) return;
  pthread_mutex_lock(&(ev->ev_mutex));
  ev->triggered = false;
  pthread_mutex_unlock(&(ev->ev_mutex));
}

static void clear_event_void_ptr(void* ptr)
{
  tEvent* ev = (tEvent*) ptr;
  clear_event(ev);
}

extern "C" {

//---------------------------------------------------------------------------
// scemi_pipe_c_handle()
//
// This function retrieves an opaque handle representing a transaction
// input or output pipe given an HDL scope and a pipe ID.
//---------------------------------------------------------------------------

  void *scemi_pipe_c_handle(const char *endpoint_path)
  {
    SceMi* scemi = SceMi::Pointer();
    if (!scemi)
      return NULL;

    SceMiPipe *pipe = scemi->get_scemi_pipe(endpoint_path);
    return (void*)(pipe);
  }

//---------------------------------------------------------------------------
// scemi_pipe_get_direction()
// scemi_pipe_get_depth()
// scemi_pipe_get_bytes_per_element()
//
// These functions return the direction, depth, and bytes per element
// of a previously defined pipe, given the pipe handle
// ----------------------------------------------------------------------------

  svBit scemi_pipe_get_direction(void *pipe_handle)
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    if (pipe->is_input())
      return sv_1;
    else
      return sv_0;
  }

  int scemi_pipe_get_depth(void *pipe_handle)
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    return (int)(pipe->buffer_depth());
  }

  int scemi_pipe_get_bytes_per_element(void *pipe_handle)
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    return (int)(pipe->bytes_per_element());
  }

//---------------------------------------------------------------------------
// scemi_pipe_get_bits_per_element()
//
// This function extends the SCE-MI API to support elements whose size are
// not a whole byte multiple.
// ----------------------------------------------------------------------------

  int scemi_pipe_get_bits_per_element(void *pipe_handle)
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    return (int)(pipe->bits_per_element());
  }

//---------------------------------------------------------------------------
// scemi_pipe_get_visibility_mode()
// scemi_pipe_get_notification_threshold()
//
// These functions are not in the SCE-MI 2.1 spec, but seem like
// reasonable additions to the API
//----------------------------------------------------------------------------

  int scemi_pipe_get_visibility_mode(void *pipe_handle)
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    return (pipe->visibility() == DEFERRED) ? 2 : 1;
  }

  int scemi_pipe_get_notification_threshold(void *pipe_handle)
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    return (pipe->visibility() == FIFO) ? 1 : (int)(pipe->buffer_depth());
  }


//---------------------------------------------------------------------------
// scemi_pipe_c_send()
//
// This is the basic blocking send function for a transaction input pipe.
// The passed in data is sent to the pipe. If necessary the calling thread
// is suspended until there is room in the pipe.
//
// The eom arg is a flag which is used for user specified end-of-message (eom)
// indication. It can be used for example to mark the end of a frame containing
// a sequence of transactions.
//
// scemi_pipe_c_receive()
//
// This is the basic blocking receive function for a transaction output pipe.
//
// The eom argument for this call is an output argument. It is set to the
// same settings of the flag passed on the send end of the pipe as described
// above. Thus is can be used by the caller to query whether the current
// read is one for which an eom was specified when the data was written on
// the send end.
//
// Both the send() and receive() calls are thread-aware. They can be
// easily implemented using a simple reference implementation that makes
// use of the non-blocking thread-neutral interface decribed below
// in conjunction with a selected threading system.
//---------------------------------------------------------------------------

  void scemi_pipe_c_send(
      void *pipe_handle,       // input: pipe handle
      int num_elements,        // input: #elements to be written
      const svBitVecVal *data, // input: data
      svBit eom )              // input: end-of-message marker flag (and flush)
  {
    int byte_offset = 0;
    int elements_sent;
    SceMiInputPipe* inpipe = requireInputPipe(__func__,pipe_handle);

    tEvent* ev = (tEvent*) scemi_pipe_get_user_data(pipe_handle,NULL);

    // if event has not been set up, do that now
    if (ev == NULL) {
      ev = create_event();
      // Call direct to Pipe function...  XXXXXXXXXXX
      scemi_pipe_put_user_data(pipe_handle,NULL,(void*)ev);
      scemi_pipe_set_notify_callback( pipe_handle
                                    , signal_event_void_ptr
                                    , (void*)ev
                                    , 0 /* persistent callback */
                                    );
    }

    while (num_elements != 0) {
      elements_sent = inpipe->try_send( byte_offset
                                        , num_elements
                                        , (const unsigned char*) data
                                        , eom
                                        , clear_event_void_ptr
                                        , ev
                                        );

      // if pipe is full, wait until OK to send more
      if (elements_sent == 0) {
        wait_for_event(ev);
      } else {
        byte_offset += elements_sent * inpipe->bytes_per_element();
        num_elements -= elements_sent;
      }
    }
  }

  void scemi_pipe_c_receive(
      void *pipe_handle,       //  input: pipe handle
      int num_elements,        //  input: #elements to be read
      int *num_elements_valid, // output: #elements that are valid
      svBitVecVal *data,       // output: data
      svBit *eom )             // output: end-of-message marker flag (and flush)
  {
    int elements_received;
    int byte_offset = 0;
    bool is_flushing = false;
    bool is_eom = false;
    SceMiOutputPipe* outpipe = requireOutputPipe(__func__,pipe_handle);
    tEvent* ev = (tEvent*) scemi_pipe_get_user_data(pipe_handle,NULL);

    // if event has not been set up, do that now
    if (ev == NULL) {
      ev = create_event();
      scemi_pipe_put_user_data(pipe_handle,NULL,(void*)ev);
      scemi_pipe_set_notify_callback( pipe_handle
                                    , signal_event_void_ptr
                                    , (void*)ev
                                    , 0 /* persistent callback */
                                    );
    }

    *num_elements_valid = 0;
    while (num_elements != 0) {
      elements_received = outpipe->try_receive( (unsigned int) byte_offset
                                                , (unsigned int) num_elements
                                                , (unsigned char *) data
                                                , &is_eom
                                                , &is_flushing
                                                , clear_event_void_ptr
                                                , ev
                                                );
      *num_elements_valid += elements_received;
      *eom = is_eom ? sv_1 : sv_0;

      if (*eom || is_flushing) return;

      num_elements -= elements_received;
      if (num_elements > 0) { /* Wait until OK to receive more */
        byte_offset += elements_received * scemi_pipe_get_bytes_per_element(pipe_handle);
        wait_for_event(ev);
      }


    }
  }

//---------------------------------------------------------------------------
// scemi_pipe_c_flush()
//
// Flush pipelined data.
//---------------------------------------------------------------------------

  void scemi_pipe_c_flush(void *pipe_handle)
  {
    tEvent* ev = (tEvent*) scemi_pipe_get_user_data(pipe_handle,NULL);

    // if event has not been set up, do that now
    if (ev == NULL) {
      ev = create_event();
      scemi_pipe_put_user_data(pipe_handle,NULL,(void*)ev);
      scemi_pipe_set_notify_callback( pipe_handle
                                    , signal_event_void_ptr
                                    , (void*)ev
                                    , 0 /* persistent callback */
                                    );
    }

    while (scemi_pipe_c_try_flush(pipe_handle, clear_event_void_ptr, ev) == 0)
      wait_for_event(ev);
  }

//---------------------------------------------------------------------------
// scemi_pipe_c_try_send()
//
// This is the basic non-blocking send function for a transaction input pipe.
// If there is room in the pipe for any elements, up to the indicated number
// of elements is transferred to the pipe and the number of transferred
// elements is returned. Otherwise, nothing is done with the data and a
// status of 0 is returned.
//
// This function is thread-neutral can can be used to create a reference
// implementation of the blocking send function (scemi_pipe_c_send)
// over a selected C-based threading environment.
//
// scemi_pipe_c_try_receive()
//
// This is the basic non-blocking receive function for a transaction output
// pipe. If any elements exist in the pipe, up to the indicated number of
// elements are transferred out of the pipe and the number of transferred
// elements is returned. Otherwise, the pipe is left alone and a status of
// 0 is returned.
//
// This function is thread-neutral can can be used to create a reference
// implementation of the blocking receive function (scemi_pipe_c_receive)
// over a selected C-based threading environment.
//
//---------------------------------------------------------------------------

  int scemi_pipe_c_try_send(
      void *pipe_handle,       // input: pipe handle
      int byte_offset,         // input: byte offset within data array
      int num_elements,        // input: #elements to be written
      const svBitVecVal *data, // input: data
      svBit eom )              // input: end-of-message marker flag
  {
    SceMiInputPipe* inpipe = requireInputPipe(__func__,pipe_handle);
    if (inpipe)
      return (int)inpipe->try_send( (unsigned int) byte_offset
                                  , (unsigned int)num_elements
                                  , (const unsigned char*)data
                                  , eom == sv_1
                                  , NULL
                                  , NULL
                                  );
    else
      return 0;
  }

  int scemi_pipe_c_try_receive(
      void *pipe_handle,       //  input: pipe handle
      int byte_offset,         //  input: byte offset within data array
      int num_elements,        //  input: #elements to be read
      svBitVecVal *data,       // output: data
      svBit *eom )             // output: end-of-message marker flag
  {
    SceMiOutputPipe* outpipe = requireOutputPipe(__func__,pipe_handle);
    if (outpipe) {
      bool eom_indicator;
      unsigned int res = outpipe->try_receive( (unsigned int) byte_offset
                                             , (unsigned int) num_elements
                                             , (unsigned char*) data
                                             , &eom_indicator
                                             , NULL
                                             , NULL
                                             , NULL
                                             );
      *eom = eom_indicator ? sv_1 : sv_0;
      return (int) res;
    } else {
      return 0;
    }
  }


//---------------------------------------------------------------------------
// scemi_pipe_c_try_flush()
//---------------------------------------------------------------------------

  int scemi_pipe_c_try_flush( void *pipe_handle               // input: pipe handle
                            , void (*noflush_callback)(void*) // input: optional callback if not flushed
                            , void* noflush_data              // input: optional callback argument
                            )
  {
    SceMiInputPipe* inpipe = requireInputPipe(__func__,pipe_handle);
    if (inpipe)
      return (int) inpipe->try_flush(noflush_callback, noflush_data);
    else
      return 0;
  }

//---------------------------------------------------------------------------
// scemi_pipe_c_in_flush_state()
//
// This function is used to test if an input pipe is in the process of
// flushing.
//
// ---------------------------------------------------------------------------

  svBit scemi_pipe_c_in_flush_state(void *pipe_handle)
  {
    SceMiPipe* pipe = (SceMiPipe*)pipe_handle;
    return (pipe->in_flush_state()) ? sv_1 : sv_0;
  }

//---------------------------------------------------------------------------
// scemi_pipe_c_can_send()
//
// This is function returns the number of elements for which there is
// currently space in the pipe, meaning that the next call to
// scemi_pipe_c_send() will be successfully accept that many elements
// without requiring a block.
//
// scemi_pipe_c_can_receive()
//
// This is function returns the number of elements currently in the pipe,
// meaning that the next call to scemi_pipe_c_receive() will successfully
// return that many elements without requiring a block.
//
//---------------------------------------------------------------------------

  int scemi_pipe_c_can_send(void *pipe_handle)
  {
    SceMiInputPipe* inpipe = requireInputPipe(__func__,pipe_handle);
    if (inpipe)
      return inpipe->can_send();
    else
      return 0;
  }

  int scemi_pipe_c_can_receive(void *pipe_handle)
  {
    SceMiOutputPipe* outpipe = requireOutputPipe(__func__,pipe_handle);
    if (outpipe)
      return outpipe->can_receive();
    else
      return 0;
  }

//---------------------------------------------------------------------------
// Notify callback support
//

  scemi_pipe_notify_callback_handle scemi_pipe_set_notify_callback(
      void *pipe_handle,       // input: pipe handle
      scemi_pipe_notify_callback notify_callback,
                               // input: notify callback function
      void *notify_context,    // input: notify context
      int callback_threshold ) // input: threshold for notify callback function
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    return pipe->request_notification(notify_callback, notify_context, callback_threshold);
  }

  void scemi_pipe_clear_notify_callback(
      scemi_pipe_notify_callback_handle notify_callback_handle )
       // input: notify callback handle
  {
    tNotification* hdl = (tNotification*)notify_callback_handle;
    SceMiPipe* pipe = get_notification_pipe(hdl);
    pipe->cancel_notification(hdl);
  }

  void *scemi_pipe_get_notify_context( //return: notify context object pointer
      scemi_pipe_notify_callback_handle notify_callback_handle )
       // input: notify callback handle
  {
    tNotification* hdl = (tNotification*)notify_callback_handle;
    return get_notification_context(hdl);
  }

//---------------------------------------------------------------------------
// Per-pipe user data storage support
//

  void scemi_pipe_put_user_data(
      void *pipe_handle,      // input: pipe handle
      void *user_key,         // input: user key
      void *user_data)        // input: user data
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    pipe->set_user_data(user_key,user_data);
  }

  void *scemi_pipe_get_user_data(
      void *pipe_handle,      // input: pipe handle
      void *user_key)         // input: user key
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    return pipe->get_user_data(user_key);
  }

//---------------------------------------------------------------------------
// Autoflush support
//

  svBit scemi_pipe_set_eom_auto_flush(
       void *pipe_handle,  // input: pipe handle
       svBit enabled )     // input: enable/disable
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    return pipe->set_auto_flush(enabled == sv_1) ? sv_1 : sv_0;
  }


  void scemi_pipe_debug(void *pipe_handle) 
  {
    SceMiPipe *pipe = (SceMiPipe*)(pipe_handle);
    pipe->debug();
  }
}; // extern "C"
