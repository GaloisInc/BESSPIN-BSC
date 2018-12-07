#include "xpc.h"

xpc::xpc()
  : xpc_dev(0)
{
  description = string("Xilinx Platform Cable USB (DLC9/DLC10)");
}

xpc::~xpc()
{
  close();
}

int
xpc::open()
{
  struct usb_bus *bus;
  struct usb_device *dev;
  unsigned char buf[256];

  // initialize libusb
  usb_init();

  if (usb_find_busses() < 0)
    throw string("usb_find_busses() failed");
  if (usb_find_devices() < 0)
    throw string("usb_find_devices() failed");

  for(bus = usb_get_busses(); bus; bus = bus->next) {
    for(dev = bus->devices; dev; dev = dev->next) {
      // If the current usb device matches the one we are looking for (Xilinx XPC), initialize it!
      if (dev->descriptor.idVendor == XPC_VENDOR_ID && dev->descriptor.idProduct == XPC_DEVICE_ID) {
	logfile.Debug(MSG_DEBUG, "open usb");
	if (!(xpc_dev = usb_open(dev))) {
	  throw string("usb_open() failed");
	}
	logfile.Debug(MSG_DEBUG10, "vendor:device    = %04x:%04x", dev->descriptor.idVendor, dev->descriptor.idProduct);
	if (usb_set_configuration(xpc_dev, dev->config[0].bConfigurationValue) < 0) {
	  usb_close(xpc_dev);
	  throw string(string(usb_strerror()) + string(" : unable to set configuration"));
	}
	logfile.Debug(MSG_DEBUG10, "claiming interface 0");
	if (usb_claim_interface(xpc_dev, 0) < 0) {
	  usb_close(xpc_dev);
	  throw string(string(usb_strerror()) + string(" : unable to claim interface"));
	}
	if (xpc_request_28(0x11) < 0) {
	  usb_close(xpc_dev);
	  throw string("Failed to initialize the Xilinx XPC cable");
	}
	if (xpc_write_gpio(XPC_PROG) < 0) {
	  usb_close(xpc_dev);
	  throw string("Failed to initialize the Xilinx XPC cable");
	}

	// read cpld version
	if (usb_control_msg(xpc_dev, 0xC0, 0xB0, 0x0050, 0x0003, (char*)buf, 2, 1000) < 0) {
	  usb_close(xpc_dev);
	  throw string(string(usb_strerror()) + string(" : failed to read cpld version"));
	}
	logfile.Debug(MSG_DEBUG10, "cpld version     = 0x%02x%02x (%u)\n", buf[1], buf[0], buf[1]<<8 | buf[0]);

	// read firmware version
	if (usb_control_msg(xpc_dev, 0xC0, 0xB0, 0x0050, 0x0000, (char*)buf, 2, 1000) < 0) {
	  usb_close(xpc_dev);
	  throw string(string(usb_strerror()) + string(" : failed to read firmware version"));
	}
	logfile.Debug(MSG_DEBUG10, "firmware version = 0x%02x%02x (%u)\n", buf[1], buf[0], buf[1]<<8 | buf[0]);

	// fill out the bit reverse table
	logfile.Debug(MSG_DEBUG10, "filling out bit reverse table");
	uint8_t j;
	for(int i = 0; i < 256; i++) {
	  if (xpc_bit_reverse((uint8_t)i, &j) < 0) {
	    throw string("Failed to initialize the Xilinx XPC cable");
	  }
	  bitrev_table[i] = j;
	}

	// initialize the cpld
	uint8_t zero[2] = { 0, 0 };
	if (xpc_request_28(0x11) < 0) {
	  throw string("first xpc_request_28 failed");
	}
	if (xpc_output_enable(1) < 0) {
	  throw string("xpc_output_enable failed");
	}
	if (xpc_shift(0xA6, 2, 2, zero, 0, NULL) < 0) {
	  throw string("xpc_shift failed");
	}
	if (xpc_request_28(0x12) < 0) {
	  throw string("second xpc_request failed");
	}
	return 0;
      }
    }
  }
  return -1;
}

int
xpc::close()
{
  if (xpc_dev) {
    logfile.Debug(MSG_DEBUG, "close usb");
    xpc_output_enable(0);
    return (usb_close(xpc_dev) == 0);
  }
  return true;
}

bool
xpc::is_present()
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
      if (dev->descriptor.idVendor == XPC_VENDOR_ID && dev->descriptor.idProduct == XPC_DEVICE_ID) {
	return true;
      }
    }
  }
  return false;
}

void 
xpc::txrx_block(const uint8_t *in, uint8_t *out, int len, bool last)
{
  int i, j;
  xpc_ext_transfer_state_t xts;

  char buffer[1024], *ptr = buffer;
  logfile.Debug(MSG_DEBUG10, "---");
  logfile.Debug(MSG_DEBUG10, "transfer size %d, %s output", len, (out!=NULL) ? "with" : "without");
  for(i = 0; i < len; i++)
    sprintf(ptr++, "%c", (in)?((in[i>>3] & (1<<i%8))?'1':'0'):'0');
  sprintf(ptr, "%s", (last) ? "last" : "");
  logfile.Debug(MSG_DEBUG10, "tdi: %s", buffer);
  
  xts.out = (out) ? out : NULL;
  xts.in_bits = 0;
  xts.out_bits = 0;
  xts.out_done = 0;
  
  int tdi = 0;
  for(i = 0, j = 0; i < len && j >= 0; i++) {
    if (in) {
      if ((i & 0x7) == 0) {
	tdi = in[i>>3];
      }
    } else {
      tdi = 0;
    }
    
    xpc_add_bit_for_ext_transfer(&xts, (tdi & 1), (i == len-1) ? last : 0, 1);
    tdi >>= 1;
    if (xts.in_bits == (2 * CPLD_MAX_BYTES - 1)) {
      j = xpc_do_ext_transfer(&xts);
    }
  }
  
  if (xts.in_bits > 0 && j >= 0) {
    if ((xts.in_bits & 3) == 0) {
      xpc_add_bit_for_ext_transfer(&xts, 0, 0, 0);
    }
    j = xpc_do_ext_transfer(&xts);
  }	
}

void 
xpc::tx_tms(uint8_t *in, int len, int force)
{
  int i, j;
  xpc_ext_transfer_state_t xts;

  char buffer[1024], *ptr = buffer;
  logfile.Debug(MSG_DEBUG10, "---");
  logfile.Debug(MSG_DEBUG10, "transfer size %d", len);
  for(i = 0; i < len; i++)
    sprintf(ptr++, "%c", (in[i>>3] & 1<<(i%8))?'1':'0');
  logfile.Debug(MSG_DEBUG10, "TMS: %s", buffer);

  xts.out = NULL;
  xts.in_bits = 0;
  xts.out_bits = 0;
  xts.out_done = 0;
      
  for(i=0,j=0; i<len && j>=0; i++) {
    xpc_add_bit_for_ext_transfer( &xts, 1, (in[i>>3] & (1<<(i%8))), 1 );
    if(xts.in_bits == (2*CPLD_MAX_BYTES - 1)) {
      j = xpc_do_ext_transfer( &xts );
    }
  }
      
  if(xts.in_bits > 0 && j>=0) {
    // CPLD doesn't like multiples of 4; add one dummy bit 
    if((xts.in_bits & 3) == 0) {
      xpc_add_bit_for_ext_transfer( &xts, 0, 0, 0 );
    }
    j = xpc_do_ext_transfer( &xts );
  }
}

int 
xpc::xpc_output_enable(uint32_t enable)
{
  logfile.Debug(MSG_DEBUG10, "xpc_output_enable(0x%02x)", enable);
  if (usb_control_msg(xpc_dev, 0x40, 0xB0, enable ? 0x18 : 0x10, 0, NULL, 0, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_output_enable failed"));
    return -1;
  }
  return 0;
}

int 
xpc::xpc_bit_reverse(uint8_t bits_in, uint8_t *bits_out)
{
  if (usb_control_msg(xpc_dev, 0xC0, 0xB0, 0x0020, bits_in, (char*)bits_out, 1, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_bit_reverse failed"));
    return -1;
  }
  logfile.Debug(MSG_DEBUG10, "xpc_bit_reverse(0x%02x) = 0x%02x", bits_in, bits_out[0]);
  return 0;
}

int 
xpc::xpc_request_28(int value)
{
  logfile.Debug(MSG_DEBUG10, "xpc_request_28(0x%02x)", value);
  if (usb_control_msg(xpc_dev, 0x40, 0xB0, 0x0028, value, NULL, 0, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_request_28 failed"));
    return -1;
  }
  return 0;
}

int 
xpc::xpc_write_gpio(uint8_t bits)
{
  logfile.Debug(MSG_DEBUG10, "xpc_write_gpio(0x%02x)", bits);
  if (usb_control_msg(xpc_dev, 0x40, 0xB0, 0x0030, bits, NULL, 0, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_write_gpio failed"));
    return -1;
  }
  return 0;    
}

int 
xpc::xpc_read_gpio(uint8_t *bits)
{
  if (usb_control_msg(xpc_dev, 0xC0, 0xB0, 0x0038, 0, (char*)bits, 1, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_read_gpio failed"));
    return -1;
  }
  logfile.Debug(MSG_DEBUG10, "xpc_read_gpio() = 0x%02x", bits[0]);
  return 0;
}

int 
xpc::xpc_read_cpld_version(uint16_t *buf)
{
  if (usb_control_msg(xpc_dev, 0xC0, 0xB0, 0x0050, 0x0001, (char*)buf, 2, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_read_cpld_version failed"));
    return -1;
  }
  logfile.Debug(MSG_DEBUG10, "xpc_read_cpld_verison() = 0x%02x%02x", buf[1], buf[0]);
  return 0;
}

int 
xpc::xpc_read_firmware_version(uint16_t *buf)
{
  if (usb_control_msg(xpc_dev, 0xC0, 0xB0, 0x0050, 0x0000, (char*)buf, 2, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_read_firmware_version failed"));
    return -1;
  }
  logfile.Debug(MSG_DEBUG10, "xpc_read_firmware_version() = 0x%02x%02x", buf[1], buf[0]);
  return 0;
}

int 
xpc::xpc_select_gpio(int int_or_ext)
{
  logfile.Debug(MSG_DEBUG10, "xpc_select_gpio(0x%02x)", int_or_ext);
  if (usb_control_msg(xpc_dev, 0x40, 0xB0, 0x0052, int_or_ext, NULL, 0, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_select_gpio failed"));
    return -1;
  }
  return 0;
}

int 
xpc::xpc_shift(uint32_t reqnum, uint32_t bits, uint32_t in_len, uint8_t *in, uint32_t out_len, uint8_t *out)
{
  if (usb_control_msg(xpc_dev, 0x40, 0xB0, reqnum, bits, NULL, 0, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : xpc_shift failed"));
    return -1;
  }

  char buffer[1024], *ptr = buffer;
  logfile.Debug(MSG_DEBUG10, "####");
  logfile.Debug(MSG_DEBUG10, "reqno  = %02X", reqnum);
  logfile.Debug(MSG_DEBUG10, "bits   = %d", bits);
  logfile.Debug(MSG_DEBUG10, "inlen  = %d, inlen*2  = %d", in_len, in_len*2);
  logfile.Debug(MSG_DEBUG10, "outlen = %d, outlen*8 = %d", out_len, out_len*8);
  sprintf(ptr++, "(");
  for(uint32_t i = 0; i < in_len; i++) {
    if (i+1<in_len) {
      sprintf(ptr, "%02X%s", in[i], ",");
      ptr += 3;
    } else {
      sprintf(ptr, "%02X", in[i]);
      ptr += 2;
    }
  }
  sprintf(ptr, "\", ");
  ptr += 3;

  if (usb_bulk_write(xpc_dev, 0x02, (char*)in, in_len, 1000) < 0) {
    throw string(string(usb_strerror()) + string(" : usb_bulk_write error(xpc_shift) failed"));
    return -1;
  }

  if (out_len > 0 && out != NULL) {
    if (usb_bulk_read(xpc_dev, 0x86, (char*)out, out_len, 1000) < 0) {
      throw string(string(usb_strerror()) + string(" : usb_bulk_read error(xpc_shift) failed"));
      return -1;
    }
  }

  sprintf(ptr, "\"");
  ptr += 1;

  for(uint32_t i = 0; i < out_len; i++) {
    if (i+1 < out_len) {
      sprintf(ptr, "%02X%s", out[i], ",");
      ptr += 3;
    } else {
      sprintf(ptr, "%02X", out[i]);
      ptr += 2;
    }
  }

  logfile.Debug(MSG_DEBUG10, "%s\")", buffer);
  return 0;
}

void 
xpc::xpc_add_bit_for_ext_transfer(xpc_ext_transfer_state_t *xts, bool in, bool tms, bool is_real)
{
  int bit_idx = (xts->in_bits & 3);
  int buf_idx = (xts->in_bits - bit_idx) >> 1;
  
  if(bit_idx == 0) {
    xts->buf[buf_idx] = 0;
    xts->buf[buf_idx+1] = 0;
  }
  
  xts->in_bits++;
  
  if(is_real) {
    if(in) xts->buf[buf_idx] |= (0x01<<bit_idx);
    if(tms) xts->buf[buf_idx] |= (0x10<<bit_idx);
      
    if(xts->out) {
      xts->buf[buf_idx+1] |= (0x11<<bit_idx);
      xts->out_bits++;
    } else {
      xts->buf[buf_idx+1] |= (0x01<<bit_idx);
    }
  }  
}

int 
xpc::xpc_do_ext_transfer(xpc_ext_transfer_state_t *xts)
{
  int r;
  int in_len, out_len;

  // cpld expects data (tdi) to be in 16-bit words
  in_len = 2 * (xts->in_bits >> 2);
  if ((xts->in_bits & 3) != 0) in_len += 2;

  // cpld returns the read data (tdo) in 32-bit words
  out_len = 2 * (xts->out_bits >> 4);
  if ((xts->out_bits & 15) != 0) out_len += 2;

  if (xts->out != NULL) {
    r = xpc_shift(0xA6, xts->in_bits, in_len, xts->buf, out_len, xts->buf);
  } else {
    r = xpc_shift(0xA6, xts->in_bits, in_len, xts->buf, 0, NULL);
  }

  if (r >= 0 && xts->out_bits > 0) {
    int i;
    uint32_t aligned_32bitwords, aligned_bytes;
    uint32_t shift, bit_num, bit_val;

    aligned_32bitwords = xts->out_bits/32;
    aligned_bytes = aligned_32bitwords * 4;
    memcpy(xts->out, xts->buf, aligned_bytes);
    xts->out_done = aligned_bytes * 8;

    // This data is not aligned
    if (xts->out_bits % 32) {
      shift = xts->out_bits % 16;
      if (shift) 
	shift = 16 - shift;
      //  logfile.Debug(MSG_DEBUG10, "out_done %d  shift %d\n", xts->out_done, shift);
      for(i = aligned_bytes * 8; i < xts->out_bits; i++) {
	bit_num = i + shift;
	bit_val = xts->buf[bit_num/8] & (1 << (bit_num%8));
	if (!(xts->out_done % 8))
	  xts->out[xts->out_done/8] = 0;
	if (bit_val)
	  xts->out[xts->out_done/8] |= (1<<(xts->out_done%8));
	xts->out_done++;
      }
    }
    char buffer[1024], *ptr = buffer;
    for(int i = 0; i < out_len; i++) {
      sprintf(ptr, " %02X", xts->out[i]);
      ptr += 3;
    }
    logfile.Debug(MSG_DEBUG10, "Shifted data %s", buffer);
  }

  xts->in_bits = 0;
  xts->out_bits = 0;

  return r;
}

