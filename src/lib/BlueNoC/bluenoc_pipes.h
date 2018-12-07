#ifndef __BLUENOC_PIPES_H__
#define __BLUENOC_PIPES_H__

#include <stdint.h>
#include <cstdlib>


#ifdef __cplusplus
extern "C" {
#endif

  void *bluenoc_pipe_c_handle(const char *endpoint_path );

  bool bluenoc_pipe_get_direction(void *pipe_handle);
  int bluenoc_pipe_get_depth(void *pipe_handle);
  int bluenoc_pipe_get_bytes_per_element(void *pipe_handle);

  int bluenoc_pipe_get_bits_per_element(void *pipe_handle);

  void bluenoc_pipe_c_send(void *pipe_handle, int num_elements, const uint8_t *data, bool eom);
  void bluenoc_pipe_c_receive(void *pipe_handle, int num_elements, int *num_elements_valid, uint8_t *data, bool *eom);
  void bluenoc_pipe_c_flush(void *pipe_handle);

  int bluenoc_pipe_c_try_send(void *pipe_handle, int byte_offset, int num_elements, const uint8_t *data, bool eom);
  int bluenoc_pipe_c_try_receive(void *pipe_handle, int byte_offset, int num_elements, uint8_t *data, bool *eom);
  int bluenoc_pipe_c_try_flush(void *pipe_handle, void (*noflush_callback)(void*) = NULL, void *noflush_data = NULL);

  bool bluenoc_pipe_c_in_flush_state(void *pipe_handle);
  
  int bluenoc_pipe_c_can_send(void *pipe_handle);
  int bluenoc_pipe_c_can_receive(void *pipe_handle);

  typedef void (*bluenoc_pipe_notify_callback)(void *context);
  typedef void *bluenoc_pipe_notify_callback_handle;

#ifdef __cplusplus
  bluenoc_pipe_notify_callback_handle bluenoc_pipe_set_notify_callback(void *pipe_handle,
								       bluenoc_pipe_notify_callback notify_callback,
								       void *notify_context,
								       int callback_threshold = 0);
#else
  bluenoc_pipe_notify_callback_handle bluenoc_pipe_set_notify_callback(void *pipe_handle,
								       bluenoc_pipe_notify_callback notify_callback,
								       void *notify_context,
								       int callback_threshold);
#endif // __cplusplus

  void bluenoc_pipe_clear_notify_callback(bluenoc_pipe_notify_callback_handle notify_callback_handle);
  void *bluenoc_pipe_get_notify_context(bluenoc_pipe_notify_callback_handle notify_callback_handle);

  void bluenoc_pipe_put_user_data(void *pipe_handle, void *user_key, void *user_data);
  void *bluenoc_pipe_get_user_data(void *pipe_handle, void *user_key);

  bool bluenoc_pipe_set_eom_auto_flush(void *pipe_handle, bool enabled);
  void bluenoc_pipe_debug(void *pipe_handle);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif //__BLUENOC_PIPES_H__
