#ifndef __PCIE_LINK_H__
#define __PCIE_LINK_H__

#include <list>
#include <set>
#include <vector>

#include "SceMiParameters.h"
#include "Link.h"
#include "sized_types.h"

#define OUTPUT_DATA_READY   (1 << 10)
#define OUTPUT_CHANNEL_MASK (0x3ff)

typedef struct {
  /* Identification and Version Info */
  /* 0x0000 */ UInt32 bluespec_id_lo;
               UInt32 bluespec_id_hi;
  /* 0x0008 */ UInt32 address_map_version;
               UInt32 _padding0;
  /* 0x0010 */ UInt32 scemi_version;
               UInt32 _padding1;
  /* 0x0018 */ UInt32 build_revision;
               UInt32 _padding2;
  /* 0x0020 */ UInt32 build_timestamp;
               UInt32 _padding3[55];
  /* SCE-MI Configuration */
  /* 0x0100 */ UInt32 num_input_channels;
               UInt32 _padding4;
  /* 0x0108 */ UInt32 num_output_channels;
               UInt32 _padding5[61];
  /* Control */
  /* 0x0200 */ volatile UInt32 command_lo;
               volatile UInt32 command_hi;
               UInt32 _padding6[62];
  /* Status */
  /* 0x0300 */ volatile UInt32 status_lo;
               volatile UInt32 status_hi;
  /* 0x0308 */ volatile UInt32 bar1_pkt_count;
               UInt32 _padding7;
  /* 0x0310 */ volatile UInt32 bar2_pkt_count;
               UInt32 _padding8;
  /* 0x0318 */ volatile UInt32 error_pkt_count;
               UInt32 _padding9;
  /* 0x0320 */ volatile UInt32 cycle_stamp_lo;
               volatile UInt32 cycle_stamp_hi;
  /* 0x0328 */ volatile UInt32 next_output_chan;
               UInt32 _padding10;
} tBar1;

typedef struct {
  volatile UInt32 status;
  UInt32 _padding0;
  volatile UInt32 data;
  UInt32 _padding1;
} tInPort;

typedef struct {
  volatile UInt32 data;
  UInt32 _padding0;
} tOutPort;

typedef struct {
  tInPort InPorts[1024];
  tOutPort OutPorts[1024];
} tBar2;

class PCIeLink : public Link
{
 private:

  const char* link_type; /* Specific PCIE link type */

  size_t last_tx;
  std::vector<Packet*> pending;
  unsigned int number_outports;
  unsigned int *port_width;

  tBar1* pBar1;
  tBar2* pBar2;
  tInPort*  pInPorts;
  tOutPort* pOutPorts;
 public:

  PCIeLink(const char* lt, SceMiParameters* parameters);

  virtual void queue(Packet* pkt);
  virtual bool ready_to_send(unsigned int channel);
  virtual bool send_pkt(unsigned int* channel_ptr);
  virtual Packet* recv_pkt(SceMiU64* cycle_ptr);
  virtual Packet* packet(unsigned int len);
  virtual void release(Packet* pkt);
  virtual void loop();
  virtual ~PCIeLink();

};

#endif /* __PCIE_LINK_H__ */
