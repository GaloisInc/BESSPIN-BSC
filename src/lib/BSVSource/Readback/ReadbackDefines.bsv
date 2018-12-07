////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2016  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision: 34856 $
// $Date: 2016-09-14 17:14:11 -0400 (Wed, 14 Sep 2016) $
////////////////////////////////////////////////////////////////////////////////
//  Filename      : ReadbackDefines.bsv
//  Description   : Common definitions exposed outside of the Readback core
////////////////////////////////////////////////////////////////////////////////
package ReadbackDefines;

////////////////////////////////////////////////////////////////////////////////
// Some hardware is parameterized on the FPGA family

typedef enum {VIRTEX6, KINTEX7} XilinxFamily deriving(Bounded, Bits, Eq);

////////////////////////////////////////////////////////////////////////////////
// Readback samples are tagged with a "configuration number" that starts at
// zero and is incremented each time the table is configured (that is, each
// time the final "Finish" entry is written).

typedef UInt#(30) ConfigNum;

////////////////////////////////////////////////////////////////////////////////
// Readback samples are tagged with a timestamp
// (which is increments on each controlled clock edge)

typedef Bit#(60) RdBackCycleStamp;

function Bit#(30) getCycleStampMSB (RdBackCycleStamp cycle);
   return truncateLSB(cycle);
endfunction

function Bit#(30) getCycleStampLSB (RdBackCycleStamp cycle);
   return truncate(cycle);
endfunction

////////////////////////////////////////////////////////////////////////////////
// Format of the "store" entries

// Only 16 bits of each 32-bit word in a frame contain the bits we care about.
// So rather than index into all 32 bits, we can compress the address by 1 bit
// and only index into the 16 positions that contain data.
//
typedef 23 CompAddrSize;

typedef UInt#(CompAddrSize) CompAddrType;

typedef union tagged {a Offset; // msb == 00
		      a Addr;   // msb == 01
   		      a Slr;    // msb == 10
                      } StoreCmd#(type a) deriving(Eq, Bits, Bounded, FShow);

// XXX Since the Offset and Slr tags are also using the CompAddrType,
// XXX the type name shows up places where its name is misleading.
//
typedef StoreCmd#(CompAddrType) RdBackStoreCmd;

////////////////////////////////////////////////////////////////////////////////

endpackage: ReadbackDefines
