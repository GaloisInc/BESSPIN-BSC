#pragma once

#include <usb.h>
#include <string.h>
#include <stdint.h>

#include "log.h"
#include "cable.h"
#include "interface.h"

#define XPC_VENDOR_ID     (0x03fd)
#define XPC_DEVICE_ID     (0x0008)

#define XPC_INTERNAL      (1)

#define XPC_PROG          (1<<3)
#define XPC_TCK           (1<<2)
#define XPC_TMS           (1<<1)
#define XPC_TDI           (1<<0)
#define XPC_TDO           (1<<0)

// send max 4096 bytes to CPLD
// 8192 TDI plus 8192 TDO bits
#define CPLD_MAX_BYTES    (1<<12)

using namespace std;

typedef struct {
  int        	 in_bits;
  int        	 out_bits;
  int        	 out_done;
  unsigned char *out;
  unsigned char  buf[CPLD_MAX_BYTES];
} xpc_ext_transfer_state_t;


class xpc : public cable
{
private:
  struct usb_dev_handle *xpc_dev;

public:
  xpc();
  ~xpc();

  static cable* create() { return new xpc(); }

  int open();
  int close();
  bool is_present();
  void txrx_block(const uint8_t *tdi, uint8_t *tdo, int length, bool last);
  void tx_tms(uint8_t *in, int length, int force);

protected:
  int xpc_output_enable(uint32_t enable);
  int xpc_bit_reverse(uint8_t bits_in, uint8_t *bits_out);
  int xpc_request_28(int value);
  int xpc_write_gpio(uint8_t bits);
  int xpc_read_gpio(uint8_t *bits);
  int xpc_read_cpld_version(uint16_t *buf);
  int xpc_read_firmware_version(uint16_t *buf);
  int xpc_select_gpio(int int_or_ext);
  int xpc_shift(uint32_t reqnum, uint32_t bits, uint32_t in_len, uint8_t *in, uint32_t out_len, uint8_t *out);
  void xpc_add_bit_for_ext_transfer(xpc_ext_transfer_state_t *xts, bool in, bool tms, bool is_real);
  int xpc_do_ext_transfer(xpc_ext_transfer_state_t *xts);
};

REGISTER(xpc, cable)
