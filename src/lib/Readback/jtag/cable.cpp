#include "cable.h"

cable::cable()
{
  memset(ones, 0xFF, CHUNKSIZE);
  memset(zeroes, 0,  CHUNKSIZE);
  memset(tms_buf, 0, CHUNKSIZE);
  tms_len = 0;
}

cable::~cable()
{
}

int
cable::open()
{
  return -1;
}

int
cable::close()
{
  return -1;
}

void 
cable::shift_tdi_tdo(const uint8_t* tdi, uint8_t *tdo, int length, bool last)
{
  if (length == 0) {
    return;
  }

  flush_tms(false);
  txrx_block(tdi, tdo, length, last);
}

void 
cable::shift_tdi(const uint8_t* tdi, int length, bool last)
{
  shift_tdi_tdo(tdi, NULL, length, last);
}

void 
cable::shift_tdo(uint8_t* tdo, int length, bool last)
{
  shift_tdi_tdo(NULL, tdo, length, last);
}

void 
cable::shift(bool tdi, int length, bool last)
{
  int len = length;
  uint8_t *block = (tdi) ? ones : zeroes;
  flush_tms(false);
  while(len > CHUNKSIZE * 8) {
    txrx_block(block, NULL, CHUNKSIZE*8, false);
    len -= (CHUNKSIZE*8);
  }
  shift_tdi_tdo(block, NULL, len, last);
}

void 
cable::set_tms(bool value)
{
  if (tms_len + 1 > CHUNKSIZE * 8) {
    flush_tms(false);
  }
  if (value) {
    tms_buf[tms_len/8] |= (1 << (tms_len & 0x7) );
  }
  tms_len++;
}

void 
cable::flush_tms(int force)
{
  if (tms_len) {
    tx_tms(tms_buf, tms_len, force);
  }
  memset(tms_buf, 0, CHUNKSIZE);
  tms_len = 0;
}

void
cable::usleep(uint32_t usec)
{
  flush_tms(false);
  usleep(usec);
}
