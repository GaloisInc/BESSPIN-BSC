#include "jtag.h"

jtag::jtag(cable *io)
  : state(TAPSTATE_UNKNOWN)
  , io(io)
  , num_devices(-1)
  , post_dr_state(TAPSTATE_IDLE)
  , post_ir_state(TAPSTATE_IDLE)
  , device_index(-1)
  , shift_dr_incomplete(false)
{
}

jtag::~jtag()
{
  vector<fpga*>::iterator it = devices.begin();
  while(it != devices.end()) {
    delete *it;
    it++;
  }
  devices.clear();
}

int
jtag::get_chain(bool detect)
{
  if (num_devices == -1 || detect) {
    tap_test_logic_reset();
    set_tap_state(TAPSTATE_SHIFTDR);
    uint8_t idx[4];
    uint8_t zero[4];
    num_devices = 0;
    for (int i = 0; i<4; i++) zero[i] = 0;
    do {
      io->shift_tdi_tdo(zero, idx, 32, false);
      uint32_t id = byte_array_to_long(idx);
      if (id != 0 && id != 0xFFFFFFFF) {
	num_devices++;
	fpga *f = fpga::find_by_idcode(id);
	devices.insert(devices.begin(), f);
      } else {
	if (id == 0xFFFFFFFF && num_devices > 0) {
	  logfile.Error("Probably a broken Atmel device in your chain!");
	  logfile.Error("No succeeding device can be identified");
	}
	break;
      }
    } while(num_devices < MAX_DEVICES);
    set_tap_state(TAPSTATE_RESET);
  }

  return num_devices;
}

fpga *
jtag::get_device(int index)
{
  // XXX what if size is zero?
  if ((size_t)index > (devices.size()-1)) {
    logfile.Error("No device exists at index %d", index);
    return 0;
  }

  select_device(index);

  return devices[index];
}

void
jtag::detect_chain()
{
  int num = this->get_chain(true);
  logfile.Debug("%d jtag device(s) found in chain", num);
  for (int i = 0; i < num; i++) {
    fpga *f = this->get_device(i);
    logfile.Debug("JTAG loc:%3d   IDCODE: 0x%08lx    %s", i, this->get_device_id(i), (f) ? f->description.c_str() : "Unknown or Undetermined device");
  }
}

void
jtag::add_device(const uint32_t &id)
{
  fpga *f = fpga::find_by_idcode(id);
  devices.insert(devices.begin(), f);
}

void 
jtag::set_tap_state(tap_state_t tostate, int pre)
{
  bool tms;
  while(state != tostate) {
    switch(state) {
      case TAPSTATE_RESET:
	switch(tostate) {
	  case TAPSTATE_RESET:
	    tms = true;
	    break;
	  default:
	    tms = false;
	    state = TAPSTATE_IDLE;
	    break;
	};
	break;

      case TAPSTATE_IDLE:
	switch(tostate) {
	  case TAPSTATE_IDLE:
	    tms = false;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_SELECTDR;
	    break;
	};
	break;

      case TAPSTATE_SELECTDR:
	switch(tostate) {
	  case TAPSTATE_CAPTUREDR:
	  case TAPSTATE_SHIFTDR:
	  case TAPSTATE_EXIT1DR:
	  case TAPSTATE_PAUSEDR:
	  case TAPSTATE_EXIT2DR:
	  case TAPSTATE_UPDATEDR:
	    tms = false;
	    state = TAPSTATE_CAPTUREDR;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_SELECTIR;
	    break;
	};
	break;

      case TAPSTATE_CAPTUREDR:
	switch(tostate) {
	  case TAPSTATE_SHIFTDR:
	    tms = false;
	    state = TAPSTATE_SHIFTDR;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_EXIT1DR;
	    break;
	};
	break;

      case TAPSTATE_SHIFTDR:
	switch(tostate) {
	  case TAPSTATE_SHIFTDR:
	    tms = false;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_EXIT1DR;
	    break;
	};
	break;

      case TAPSTATE_EXIT1DR:
	switch(tostate) {
	  case TAPSTATE_PAUSEDR:
	  case TAPSTATE_EXIT2DR:
	  case TAPSTATE_SHIFTDR:
	  case TAPSTATE_EXIT1DR:
	    tms = false;
	    state = TAPSTATE_PAUSEDR;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_UPDATEDR;
	    break;
	};
	break;

      case TAPSTATE_PAUSEDR:
	switch(tostate) {
	  case TAPSTATE_PAUSEDR:
	    tms = false;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_EXIT2DR;
	    break;
	};
	break;

      case TAPSTATE_EXIT2DR:
	switch(tostate) {
	  case TAPSTATE_SHIFTDR:
	  case TAPSTATE_EXIT1DR:
	  case TAPSTATE_PAUSEDR:
	    tms = false;
	    state = TAPSTATE_SHIFTDR;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_UPDATEDR;
	    break;
	};
	break;

      case TAPSTATE_UPDATEDR:
	switch(tostate) {
	  case TAPSTATE_IDLE:
	    tms = false;
	    state = TAPSTATE_IDLE;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_SELECTDR;
	    break;
	};
	break;

      case TAPSTATE_SELECTIR:
	switch(tostate) {
	  case TAPSTATE_CAPTUREIR:
	  case TAPSTATE_SHIFTIR:
	  case TAPSTATE_EXIT1IR:
	  case TAPSTATE_PAUSEIR:
	  case TAPSTATE_EXIT2IR:
	  case TAPSTATE_UPDATEIR:
	    tms = false;
	    state = TAPSTATE_CAPTUREIR;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_RESET;
	    break;
	};
	break;

      case TAPSTATE_CAPTUREIR:
	switch(tostate) {
	  case TAPSTATE_SHIFTIR:
	    tms = false;
	    state = TAPSTATE_SHIFTIR;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_EXIT1IR;
	    break;
	};
	break;

      case TAPSTATE_SHIFTIR:
	switch(tostate) {
	  case TAPSTATE_SHIFTIR:
	    tms = false;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_EXIT1IR;
	    break;
	};
	break;

      case TAPSTATE_EXIT1IR:
	switch(tostate) {
	  case TAPSTATE_PAUSEIR:
	  case TAPSTATE_EXIT2IR:
	  case TAPSTATE_SHIFTIR:
	  case TAPSTATE_EXIT1IR:
	    tms = false;
	    state = TAPSTATE_PAUSEIR;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_UPDATEIR;
	    break;
	};
	break;

      case TAPSTATE_PAUSEIR:
	switch(tostate) {
	  case TAPSTATE_PAUSEIR:
	    tms = false;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_EXIT2IR;
	    break;
	};
	break;

      case TAPSTATE_EXIT2IR:
	switch(tostate) {
	  case TAPSTATE_SHIFTIR:
	  case TAPSTATE_EXIT1IR:
	  case TAPSTATE_PAUSEIR:
	    tms = false;
	    state = TAPSTATE_SHIFTIR;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_UPDATEIR;
	    break;
	};
	break;

      case TAPSTATE_UPDATEIR:
	switch(tostate) {
	  case TAPSTATE_IDLE:
	    tms = false;
	    state = TAPSTATE_IDLE;
	    break;
	  default:
	    tms = true;
	    state = TAPSTATE_SELECTDR;
	    break;
	};
	break;

      default:
	tap_test_logic_reset();
	tms = true;
    };
    io->set_tms(tms);
  }
  for(int i = 0; i < pre; i++) {
    io->set_tms(false);
  }
}

void 
jtag::tap_test_logic_reset(void)
{
  int i;
  for(i = 0; i < 5; i++) io->set_tms(true);
  state = TAPSTATE_RESET;
  io->flush_tms(true);
}

void 
jtag::next_tap_state(bool tms)
{
  if (state == TAPSTATE_SHIFTDR) {
    if (tms) state = TAPSTATE_EXIT1DR;
  } else if (state == TAPSTATE_SHIFTIR) {
    if (tms) state = TAPSTATE_EXIT1IR;
  } else {
    logfile.Error("next_tap_state Unexpected state %d", state);
    tap_test_logic_reset();
  }
}

void 
jtag::cycle_tck(int n, bool tdi)
{
  if (state == TAPSTATE_RESET) {
    logfile.Error("cycle_tck in TEST_LOGIC_RESET");
  }
  io->shift(tdi, n, false);
}

tap_state_t 
jtag::get_tap_state(void)
{
  return state;
}

uint32_t 
jtag::get_device_id(uint32_t dev)
{
  if (dev >= devices.size())
    return 0;
  // XXX unless we store a pair of the id and the fpga pointer,
  // XXX there's no way to get the id from a NULL pointer
  if (devices[dev] == NULL)
    return 0;
  return devices[dev]->idcode;
}

void 
jtag::usleep(uint32_t usec)
{
  io->usleep(usec);
}

int 
jtag::select_device(int dev)
{
  if (dev >= num_devices) 
    device_index = -1;
  else
    device_index = dev;

  return device_index;
}

void 
jtag::shift_dr(const uint8_t *tdi, uint8_t *tdo, int length, int align, bool exit)
{
  if (device_index < 0)
    return;

  int post = device_index;

  if (!shift_dr_incomplete) {
    int pre = num_devices - device_index - 1;
    if (align) {
      pre = -post;
      while(pre <= 0)
	pre += align;
    }
    set_tap_state(TAPSTATE_SHIFTDR, pre);
  }

  if (tdi != 0 && tdo != 0) 
    io->shift_tdi_tdo(tdi, tdo, length, post == 0 && exit);
  else if (tdi != 0 && tdo == 0)
    io->shift_tdi(tdi, length, post == 0 && exit);
  else if (tdi == 0 && tdo != 0)
    io->shift_tdo(tdo, length, post == 0 && exit);
  else 
    io->shift(false, length, post == 0 && exit);

  next_tap_state(post == 0 && exit);
  if (exit) {
    io->shift(false, post);
    if (!(post == 0 && exit))
      next_tap_state(true);
    set_tap_state(post_dr_state);
    shift_dr_incomplete = false;
  } else {
    shift_dr_incomplete = true;
  }
}

void 
jtag::shift_ir(const uint8_t *tdi, uint8_t *tdo)
{
  if (device_index < 0)
    return;

  set_tap_state(TAPSTATE_SHIFTIR);

  int pre = 0;
  for(int dev = device_index + 1; dev < num_devices; dev++) {
    pre += devices[dev]->irlength;
  }
  int post = 0;
  for(int dev = 0; dev < device_index; dev++) {
    post += devices[dev]->irlength;
  }
  io->shift(true, pre, false);
  if (tdo != 0) io->shift_tdi_tdo(tdi, tdo, devices[device_index]->irlength, post == 0);
  else if (tdo == 0) io->shift_tdi(tdi, devices[device_index]->irlength, post == 0);
  io->shift(true, post);
  next_tap_state(true);
  set_tap_state(post_ir_state);
}

unsigned long long
jtag::read_dna()
{
  unsigned long long bufferi = 0LL, buffero = 0LL;
  uint8_t *ptri = reinterpret_cast<uint8_t*>(&bufferi);
  uint8_t *ptro = reinterpret_cast<uint8_t*>(&buffero);

  // disable fpga
  this->send_instruction("ISC_ENABLE");
  // read out ddna
  this->send_instruction("XSC_DNA");
  this->shift_dr(ptri, ptro, 57);
  // enable fpga
  this->send_instruction("ISC_DISABLE");

  // buffero contains the dna value
  return buffero;
}

void
jtag::send_instruction(const string &inst_code)
{
  fpga *f = devices[device_index];
  f->set_instruction(this, inst_code);
}

void
jtag::send_user1_data(const uint32_t &data)
{
  uint8_t *ptr = reinterpret_cast<uint8_t*>(const_cast<uint32_t*>(&data));
  uint8_t *buffer = new uint8_t[8];
  memcpy(buffer, ptr, 4);
  buffer[4] = buffer[5] = buffer[6] = buffer[7] = 0;
  
  this->shift_dr(buffer, 0, 64);
}

unsigned long long
jtag::recv_user1_data()
{
  unsigned long long data, dontcare = 0;
  uint8_t *ptr = reinterpret_cast<uint8_t*>(&data);
  uint8_t *dkptr = reinterpret_cast<uint8_t*>(&dontcare);
  this->shift_dr(dkptr, ptr, 64);
  return data;
}

void
jtag::write_user1_register(const uint32_t &data)
{
  logfile.Debug("Write: %08X", data);
  send_user1_data(data);
  recv_user1_data();
}

unsigned long long
jtag::read_user1_register(const uint32_t &data)
{
  send_user1_data(data);
  unsigned long long readdata = recv_user1_data();
  logfile.Debug("Read: %08X -> %0llx", data, readdata);
  return readdata;
}

void 
jtag::long_to_byte_array(uint32_t l, uint8_t *b)
{
  b[0] = (uint8_t)( l      & 0xFF);
  b[1] = (uint8_t)((l>> 8) & 0xFF);
  b[2] = (uint8_t)((l>>16) & 0xFF);
  b[3] = (uint8_t)((l>>24) & 0xFF);
}

void 
jtag::long_to_byte_array_reverse(uint32_t l, uint8_t *b)
{
  b[0] = io->bitrev_table[ l      & 0xFF];
  b[1] = io->bitrev_table[(l>> 8) & 0xFF];
  b[2] = io->bitrev_table[(l>>16) & 0xFF];
  b[3] = io->bitrev_table[(l>>24) & 0xFF];
}

void 
jtag::short_to_byte_array(const uint16_t l, uint8_t *b)
{
  b[0] = (uint8_t)(l & 0xFF);
  b[1] = (uint8_t)((l>>8)&0xFF);
}

uint32_t 
jtag::byte_array_to_long(const uint8_t *b)
{
  return ((uint32_t)b[3]<<24)+((uint32_t)b[2]<<16)+((uint32_t)b[1]<<8)+((uint32_t)b[0]);
}

