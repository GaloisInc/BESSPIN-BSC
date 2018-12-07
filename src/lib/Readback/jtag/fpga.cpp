#include "fpga.h"

fpga::fpga(const char *name, const char *description, const uint32_t &idcode, const uint32_t &idmask, const uint32_t &irlen)
  : name(name)
  , description(description)
  , idcode(idcode)
  , idmask(idmask)
  , irlength(irlen)
{
}

fpga::~fpga()
{
}

fpga *
fpga::find_by_idcode(const uint32_t &idcode)
{
  set<string> fpgas = interface_registry<fpga>::shared_instance().get_entries();
  set<string>::iterator it;
  for(it = fpgas.begin(); it != fpgas.end(); it++) {
    fpga *f = interface_registry<fpga>::shared_instance().create_object_of(*it);
    if ((idcode & f->idmask) == (f->idcode & f->idmask)) {
      return f;
    }
  }

  fprintf(stdout, "Warning: unsupported FPGA, idcode not found (0x%x)\n", idcode);
  return NULL;
}

void
fpga::set_instruction(jtag *j, const string &idcode)
{
  instruction::iterator it = ircodes.find(idcode);
  if (it != ircodes.end()) {
    uint32_t code = ircodes[idcode];
    logfile.Debug(MSG_DEBUG10, "Sending 0x%04x corresponding to %s", code, idcode.c_str());
    j->shift_ir(reinterpret_cast<uint8_t*>(&code));
  }
}
