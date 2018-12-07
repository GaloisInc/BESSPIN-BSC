#pragma once

#include <usb.h>
#include <ftdi.h>

#include <string.h>
#include <stdint.h>
#include <errno.h>


#include "log.h"
#include "cable.h"
#include "interface.h"

#define FT2232_VENDOR_ID    (0x0403)
#define FT2232_DEVICE_ID    (0x6010)

#define TX_BUF            (4096)

using namespace std;

class ft2232 : public cable
{
protected:
  struct ftdi_context *ftdi_handle;
  uint8_t usbuf[TX_BUF];
  int buflen;
  uint32_t bptr;
  int calls_rd, calls_wr, subtype, retries;
  bool device_has_fast_clock;
  uint32_t tck_freq;
  
public:
  ft2232();
  ~ft2232();

  static cable* create() { return new ft2232(); }

  int open();
  int close();
  bool is_present();
  void txrx_block(const uint8_t *tdi, uint8_t *tdo, int length, bool last);
  void tx_tms(uint8_t *in, int length, int force);

protected:
  void settype(int sub_type);
  void mpsse_add_cmd(uint8_t const *buf, int len);
  void mpsse_send(void);
  uint32_t readusb(uint8_t *rbuf, unsigned long len);
  void flush();
};

REGISTER(ft2232, cable)
