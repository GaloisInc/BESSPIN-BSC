#pragma once

#include <cstdio>
#include <vector>
#include <string>
#include <cstring>
#include <sys/types.h>
#include <stdint.h>

#include "log.h"

#define BLOCKSIZE     (65536)
#define CHUNKSIZE     (128)
#define TICKCOUNT     (2048)

using namespace std;

class cable
{
protected:
  Log      logfile;

public:
  uint8_t  bitrev_table[256];
  uint8_t  tms_buf[CHUNKSIZE];
  uint8_t  ones[CHUNKSIZE];
  uint8_t  zeroes[CHUNKSIZE];
  uint32_t tms_len;
  string   description;

public:
  cable();
  virtual ~cable();

  virtual int open();
  virtual int close();

  virtual bool is_present() = 0;
  virtual void txrx_block(const uint8_t *tdi, uint8_t *tdo, int length, bool last) = 0;
  virtual void tx_tms(uint8_t *in, int length, int force) = 0;

  virtual void shift_tdi_tdo(const uint8_t* tdi, uint8_t *tdo, int length, bool last = true);
  virtual void shift_tdi(const uint8_t* tdi, int length, bool last = true);
  virtual void shift_tdo(uint8_t* tdo, int length, bool last = true);
  virtual void shift(bool tdi, int length, bool last = true);
  virtual void set_tms(bool value);
  virtual void flush_tms(int force);
  virtual void usleep(uint32_t usec);
};
