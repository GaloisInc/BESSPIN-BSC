#pragma once

#include <cstdio>
#include <vector>
#include <stdint.h>
#include <unistd.h>

#include "cable.h"
#include "fpga.h"
#include "log.h"

#define MAX_DEVICES (1000)

using namespace std;

typedef enum {
  TAPSTATE_RESET      = 0x00,
  TAPSTATE_IDLE       = 0x01,
  TAPSTATE_SELECTDR   = 0x02,
  TAPSTATE_CAPTUREDR  = 0x03,
  TAPSTATE_SHIFTDR    = 0x04,
  TAPSTATE_EXIT1DR    = 0x05,
  TAPSTATE_PAUSEDR    = 0x06,
  TAPSTATE_EXIT2DR    = 0x07,
  TAPSTATE_UPDATEDR   = 0x08,
  TAPSTATE_SELECTIR   = 0x09,
  TAPSTATE_CAPTUREIR  = 0x0A,
  TAPSTATE_SHIFTIR    = 0x0B,
  TAPSTATE_EXIT1IR    = 0x0C,
  TAPSTATE_PAUSEIR    = 0x0D,
  TAPSTATE_EXIT2IR    = 0x0E,
  TAPSTATE_UPDATEIR   = 0x0F,
  TAPSTATE_UNKNOWN    = 0xFF
} tap_state_t;

class fpga;

class jtag
{
  tap_state_t            state;
  vector<fpga*>          devices;
  cable                 *io;
  int                    num_devices;
  tap_state_t            post_dr_state;
  tap_state_t            post_ir_state;
  int                    device_index;
  bool                   shift_dr_incomplete;
  Log                    logfile;

public:
  jtag(cable *io);
  ~jtag();

  int get_chain(bool detect = false);
  void detect_chain();
  fpga *get_device(int index);
  void add_device(const uint32_t &id);
  void set_post_dr_state(tap_state_t s) { post_dr_state = s; }
  void set_post_ir_state(tap_state_t s) { post_ir_state = s; }
  void set_tap_state(tap_state_t state, int pre = 0);
  void tap_test_logic_reset(void);
  void next_tap_state(bool tms);
  void cycle_tck(int n, bool tdi = 1);
  tap_state_t get_tap_state(void);
  uint32_t get_device_id(uint32_t dev);
  void usleep(uint32_t usec);
  int select_device(int dev);
  void shift_dr(const uint8_t *tdi, uint8_t *tdo, int length, int align = 0, bool exit = true);
  void shift_ir(const uint8_t *tdi, uint8_t *tdo = 0);

  unsigned long long read_dna();

  void send_instruction(const string &inst_code);
  void send_user1_data(const uint32_t &data);
  unsigned long long recv_user1_data();
  void write_user1_register(const uint32_t &data);
  unsigned long long read_user1_register(const uint32_t &data);

  void long_to_byte_array(uint32_t i, uint8_t *b);
  void long_to_byte_array_reverse(uint32_t i, uint8_t *b);
  void short_to_byte_array(const uint16_t l, uint8_t *b);
  uint32_t byte_array_to_long(const uint8_t *b);
};
