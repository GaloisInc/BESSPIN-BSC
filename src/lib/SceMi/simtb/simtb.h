// Copyright Bluespec Inc. 2012-2013

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

  unsigned int
  bsvsimtb_interp_start(unsigned int port);

  unsigned int
  bsvsimtb_interp_message_ready(unsigned int channel);

  unsigned long long
  bsvsimtb_interp_message_get(unsigned int channel);
  unsigned long long
  bsvsimtb_interp_message_send(unsigned int channel, unsigned int value)

  void emu_stop();

  void timer_clear();
  void timer_show();

#ifdef __cplusplus
};
#endif

