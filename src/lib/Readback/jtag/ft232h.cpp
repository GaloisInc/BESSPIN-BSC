#include "ft232h.h"

ft232h::ft232h()
  : ftdi_handle(0)
  , bptr(0)
  , calls_rd(0)
  , calls_wr(0)
  , retries(0)
{
  description = string("Digilent USB Device");
}

ft232h::~ft232h()
{
  if (ftdi_handle) {
    close();
    free(ftdi_handle);
  }
}

int 
ft232h::open()
{
  uint8_t buf1[5];
  uint8_t buf[9] = { SET_BITS_LOW,  0x00, 0x0b,
		     TCK_DIVISOR,   0x03, 0x00,
		     SET_BITS_HIGH, 0x00, 0x00 };

  char *description = NULL;
  char descstring[256] = "Digilent USB Device";
  uint32_t vendor = FT232H_VENDOR_ID, product = FT232H_DEVICE_ID;
  uint32_t channel = 0;
  uint32_t dbus_data = 0, dbus_en = 0x0b, cbus_data = 0, cbus_en = 0;
  uint32_t divisor;
  int result;
  uint32_t freq = 1500000;

  divisor = 6000000/freq - ((6000000 & freq) ? 0 : 1);
  if (divisor > 0xFFFF)
    divisor = 0xFFFF;

  buf[4] = divisor & 0xFF;
  buf[5] = (divisor >> 8) & 0xFF;
  
  description = descstring;

  channel = 0;
  dbus_data = 0x80;
  dbus_en |= 0x80;
  cbus_data = 0x00;
  cbus_en = 0x0; 

  ftdi_handle = ftdi_new();
  if (ftdi_handle == 0) {
    throw string("failed to allocate and initialize FTDI structure!");
  }

  if (channel > 2) {
    throw string ("Invalid MPSSE channel");
  }

  result = ftdi_set_interface(ftdi_handle, (ftdi_interface)channel);
  if (result < 0) {
    throw string(string("ftdi_set_interface: ") + string(ftdi_get_error_string(ftdi_handle)));
  }

  result = ftdi_usb_open_desc(ftdi_handle, vendor, product, description, 0);
  if (result < 0) {
    throw string(string("Could not open FTDI device (using libftdi): ") + string(ftdi_get_error_string(ftdi_handle)));
  }

  result = ftdi_set_bitmode(ftdi_handle, 0x00, BITMODE_RESET);
  if (result < 0) {
    throw string(string("ftdi_set_bitmode: ") + string(ftdi_get_error_string(ftdi_handle)));
  }
  
  result = ftdi_usb_purge_buffers(ftdi_handle);
  if (result < 0) {
    throw string(string("ftdi_usb_purge_buffers: ") + string(ftdi_get_error_string(ftdi_handle)));
  }
  
  // set latency time to a low value
  result = ftdi_set_latency_timer(ftdi_handle, 1);
  if (result < 0) {
    throw string(string("ftdi_set_latency_timer: ") + string(ftdi_get_error_string(ftdi_handle)));
  }
  
  // set mode to mpsse
  result = ftdi_set_bitmode(ftdi_handle, 0xFB, BITMODE_MPSSE);
  if (result < 0) {
    throw string(string("ftdi_set_bitmode: ") + string(ftdi_get_error_string(ftdi_handle)));
  }
  
  // workaround for consecutive runs
  ftdi_read_data(ftdi_handle, buf1, 5);
  
  // check if we have a fast clock capable device
  switch(ftdi_handle->type) {
    case TYPE_2232H:
    case TYPE_4232H:
      device_has_fast_clock = true;
      break;
    default:
      device_has_fast_clock = false;
      break;
  }

  buf[1] |= dbus_data;
  buf[2] |= dbus_en;
  buf[7] = cbus_data;
  buf[8] = cbus_en;

  mpsse_add_cmd(buf, 9);
  mpsse_send();

  if (device_has_fast_clock && ((freq == 0) || (freq > 458))) {
    if ((freq == 0) || (freq >= 30000000)) {
      divisor = 0;
    } else {
      divisor = 30000000/freq - ((30000000%freq) ? 0 : 1);
    }

    if (divisor > 0xFFFF) 
      divisor = 0xFFFF;
#ifndef DIS_DIV_5
#define DIS_DIV_5 (0x8a)
#endif
    buf[0] = DIS_DIV_5;
    buf[1] = TCK_DIVISOR;
    buf[2] = divisor & 0xFF;
    buf[3] = (divisor >> 8) & 0xFF;
    mpsse_add_cmd(buf, 4);
    mpsse_send();
    tck_freq = 30000000 / (1+divisor);
  } else {
    tck_freq = 6000000 / (1+divisor);
  }

  return 0;
}

int 
ft232h::close()
{
  int read;

  static unsigned char tbuf[16] = { SET_BITS_LOW, 0xFF, 0x00,
				    SET_BITS_HIGH, 0xFF, 0x00,
				    LOOPBACK_START,
				    MPSSE_DO_READ|MPSSE_READ_NEG|MPSSE_DO_WRITE|MPSSE_WRITE_NEG|MPSSE_LSB,
				    0x04, 0x00,
				    0xAA, 0x55, 0x00, 0xFF, 0xAA,
				    LOOPBACK_END };

  mpsse_add_cmd(tbuf, 16);
  read = readusb(tbuf, 5);
  if (read != 5) {
    fprintf(stderr, "Loopback failed, expect problems on later runs!\n");
  }

  ftdi_set_bitmode(ftdi_handle, 0, BITMODE_RESET);
  ftdi_usb_reset(ftdi_handle);
  ftdi_usb_close(ftdi_handle);
  ftdi_deinit(ftdi_handle);

  return 0;
}

bool
ft232h::is_present()
{
  struct usb_bus *bus;
  struct usb_device *dev;

  // initialize libusb
  usb_init();

  if (usb_find_busses() < 0)
    throw string("usb_find_busses() failed");
  if (usb_find_devices() < 0)
    throw string("usb_find_devices() failed");

  for(bus = usb_get_busses(); bus; bus = bus->next) {
    for(dev = bus->devices; dev; dev = dev->next) {
      if (dev->descriptor.idVendor == FT232H_VENDOR_ID && dev->descriptor.idProduct == FT232H_DEVICE_ID) {
	return true;
      }
    }
  }
  return false;
}

void
ft232h::txrx_block(const uint8_t *tdi, uint8_t *tdo, int length, bool last)
{
  unsigned char rbuf[TX_BUF];
  unsigned const char *tmpsbuf = tdi;
  unsigned char *tmprbuf = tdo;
  unsigned int rem = (last) ? length - 1 : length;
  unsigned char buf[TX_BUF];
  unsigned int buflen = TX_BUF - 3;
  unsigned int rembits;

  /*out on -ve edge, in on +ve edge */
  if (rem/8 > buflen) {
    while (rem/8 > buflen) {
      /* full chunks*/
      buf[0] = ((tdo)?(MPSSE_DO_READ |MPSSE_READ_NEG):0)
	|((tdi)?MPSSE_DO_WRITE:0)|MPSSE_LSB|MPSSE_WRITE_NEG;
      buf[1] = (buflen-1) & 0xff;        /* low lenbth byte */
      buf[2] = ((buflen-1) >> 8) & 0xff; /* high lenbth byte */
      mpsse_add_cmd (buf, 3);
      if(tdi) {
	mpsse_add_cmd (tmpsbuf, buflen);
	tmpsbuf+=buflen;
      }
      rem -= buflen * 8;
      if (tdo) {
	if (readusb(tmprbuf,buflen) != buflen) {
	  fprintf(stderr,"IO_JTAG_MPSSE::shiftTDITDO:"
		  "Failed to read block 0x%x bytes\n", buflen );
	}
	tmprbuf+=buflen;
      }
    }
  }
  rembits = rem % 8;
  rem  = rem - rembits;
  if (rem %8 != 0 ) {
    fprintf(stderr,"IO_JTAG_MPSSE::shiftTDITDO: Programmer error\n");
  }
  buflen = rem/8;
  if(rem) {
    buf[0] = ((tdo)?(MPSSE_DO_READ|MPSSE_READ_NEG):0)
      |((tdi)?MPSSE_DO_WRITE:0)|MPSSE_LSB|MPSSE_WRITE_NEG;
    buf[1] =  (buflen - 1)       & 0xff; /* low length byte */
    buf[2] = ((buflen - 1) >> 8) & 0xff; /* high length byte */
    mpsse_add_cmd (buf, 3);
    if(tdi) {
      mpsse_add_cmd (tmpsbuf, buflen );
      tmpsbuf  += buflen;
    }
  }
  
  if (buflen >=(TX_BUF - 4)) {
    /* No space for the last data. Send and evenually read 
       As we handle whole bytes, we can use the receiv buffer direct*/
    if(tdo) {
      readusb(tmprbuf, buflen);
      tmprbuf+=buflen;
    }
    buflen = 0;
  }
  if( rembits) {
    /* Clock Data Bits Out on -ve Clock Edge LSB First (no Read)
       (use if TCK/SK starts at 0) */
    buf[0] = ((tdo)?(MPSSE_DO_READ|MPSSE_READ_NEG):0)
      |((tdi)?MPSSE_DO_WRITE:0)|MPSSE_LSB|MPSSE_BITMODE|MPSSE_WRITE_NEG;
    buf[1] = rembits-1; /* length: only one byte left*/
    mpsse_add_cmd (buf, 2);
    if(tdi)
      mpsse_add_cmd (tmpsbuf,1) ;
    buflen ++;
  }
  if(last) {
    bool lastbit = false;
    if(tdi) 
      lastbit = (*tmpsbuf & (1<< rembits));
    /* TMS/CS with LSB first on -ve TCK/SK edge, read on +ve edge 
       - use if TCK/SK is set to 0*/
    buf[0] = MPSSE_WRITE_TMS|((tdo)?(MPSSE_DO_READ|MPSSE_READ_NEG):0)|
      MPSSE_LSB|MPSSE_BITMODE|MPSSE_WRITE_NEG;
    buf[1] = 0;     /* only one bit */
    buf[2] = (lastbit) ? 0x81 : 1 ;     /* TMS set */
    mpsse_add_cmd (buf, 3);
    buflen ++;
  }
  if(tdo) {
    if (!last) {
      readusb(tmprbuf, buflen);
      if (rembits) /* last bits for incomplete byte must get shifted down*/
	tmprbuf[buflen-1] = tmprbuf[buflen-1]>>(8-rembits);
    } else {
      /* we need to handle the last bit. It's much faster to
	 read into an extra buffer than to issue two USB reads */
      readusb(rbuf, buflen); 
      if(!rembits) {
	rbuf[buflen-1] = (rbuf[buflen - 1]& 0x80)?1:0;
      } else {
	/* TDO Bits are shifted downwards, so align them 
	   We only shift TMS once, so the relevant bit is bit 7 (0x80) */
	rbuf[buflen-2] = rbuf[buflen-2]>>(8-rembits) |
	  ((rbuf[buflen - 1]&0x80) >> (7 - rembits));
	buflen--;
      }
      memcpy(tmprbuf,rbuf,buflen);
    }
  }
}

void
ft232h::tx_tms(uint8_t *in, int length, int force)
{
  unsigned char buf[3] = {MPSSE_WRITE_TMS|MPSSE_LSB|MPSSE_BITMODE|
			  MPSSE_WRITE_NEG, 0, in[0]};
  int len = length, i, j=0;
  if (!len) return;
  while (len>0) {
    /* Attention: Bug in FT2232L(D?, H not!). 
       With 7 bits TMS shift, static TDO 
       value gets set to TMS on last TCK edge*/ 
    buf[1] = (len >6)? 5: (len-1);
    buf[2] = 0x80;
    for (i=0; i < (buf[1]+1); i++) {
      buf[2] |= (((in[j>>3] & (1<< (j &0x7)))?1:0)<<i);
      j++;
    }
    len -=(buf[1]+1);
    mpsse_add_cmd (buf, 3);
  }
  if(force)
    mpsse_send();
}

void
ft232h::settype(int sub_type)
{
  subtype = sub_type;
}

uint32_t 
ft232h::readusb(uint8_t *rbuf, unsigned long len)
{
  unsigned char buf[1] = { SEND_IMMEDIATE };
  mpsse_add_cmd(buf, 1);
  mpsse_send();

  unsigned int read = 0;
  int length = (int)len;
  int timeout = 0, last_errno, last_read;
  calls_rd++;
  last_read = ftdi_read_data(ftdi_handle, rbuf, length);
  if (last_read > 0) {
    read += last_read;
  }
  while(((int) read < length ) && (timeout < 1000)) {
    last_errno = 0;
    retries++;
    last_read = ftdi_read_data(ftdi_handle, rbuf+read, length-read);
    if (last_read > 0) {
      read += last_read;
    } else {
      last_errno = errno;
    }
    timeout++;
  }
  if (timeout >= 1000) {
    fprintf(stderr,"readusb waiting too long for %ld bytes, only %d read\n", len, last_read);
    if (last_errno) {
      throw string(strerror(-last_read));
    }
  }
  return read;
}

void
ft232h::mpsse_add_cmd(uint8_t const *buf, int len)
{
 /* The TX FIFO has 128 Byte. It can easily be overrun
    So send only chunks of the TX Buffersize and hope
    that the OS USB scheduler gives the MPSSE machine 
    enough time empty the buffer
 */
  if (bptr + len + 1 >= TX_BUF) {
    mpsse_send();
  }
  memcpy(usbuf + bptr, buf, len);
  bptr += len;
}

void 
ft232h::mpsse_send(void)
{
  if (bptr == 0) return;

  calls_wr++;
  int written = ftdi_write_data(ftdi_handle, usbuf, bptr);
  if (written != (int)bptr) {
    fprintf(stderr,"mpsse_send: Short write %d vs %d at run %d, Err: %s\n", 
	    written, bptr, calls_wr, ftdi_get_error_string(ftdi_handle));
    throw runtime_error(string(""));
  }
  bptr = 0;
}

void
ft232h::flush()
{
  mpsse_send();
}
