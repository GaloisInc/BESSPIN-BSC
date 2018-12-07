// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiVirtex6PCIE;

// This is an implementation of SceMi over PCI-Express.

import Clocks::*;
import Vector::*;
import List::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;
import Connectable::*;
import TieOff::*;
import ModuleContext::*;
import DefaultValue::*;
import XilinxPCIE::*;
import XilinxVirtex6PCIE::*;
import XilinxCells::*;
import DiniPCIE::*;
import DReg::*;
import BUtils::*;
import Counter::*;
import OInt::*;

import SceMiDefines::*;
import SceMiInternals::*;
import SceMiClocks::*;

// Interface wrapper for PCIE
interface SceMiV6PCIEIfc#(type i, numeric type lanes);
   interface i                orig_ifc;
   interface PCIE_EXP#(lanes) pcie;
   (* always_ready *)
   method Bool isLinkUp();
   (* always_ready *)
   method Bool isOutOfReset();
   (* always_ready *)
   method Bool isClockAdvancing();
endinterface

// Interface from the port logic facing the PCIE bus
interface SCEMI_to_PCIE_Ifc;
   interface Get#(TLPData#(16)) getTLPWord;
   interface Put#(TLPData#(16)) putTLPWord;
   interface PCIE_PIO_Ifc       pio;
endinterface

// Argument structure used for passing in PCIE clocks
typedef struct {
   Clock         pci_sys_clk_p;
   Clock         pci_sys_clk_n;
   Reset         pci_sys_reset;
   Clock         ref_clk;
   SceMiLinkType link_type;
} SceMiV6PCIEArgs;

typedef enum {
   IDLE,
   WRITE_4DW
} State deriving (Bits, Eq);   

// This module builds the transactor hierarchy, the clock
// generation logic and the PCIE-to-port logic.
module [Module] buildSceMiPCIEV6#( SceMiModule#(i) mod
                                 , Clock pci_sys_clk_p
                                 , Clock pci_sys_clk_n
				 , Reset pci_sys_reset
				 , Clock ref_clk
				 , SceMiLinkType link_type
                                 )
                                 (SceMiV6PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex6PCIE#(lanes));

   // Buffer clocks and reset before they are used
   Clock sys_clk_buf   <- mkClockIBUFDS_GTXE1(defaultValue, True, pci_sys_clk_p, pci_sys_clk_n);
   
   // Instantiate the PCIE endpoint
   PCIExpressV6#(lanes) _ep <- mkPCIExpressEndpointV6( defaultValue
						     , clocked_by sys_clk_buf
                                                     , reset_by pci_sys_reset
                                                     );
   mkTieOff(_ep.cfg);
   mkTieOff(_ep.cfg_interrupt);
   mkTieOff(_ep.cfg_err);
   mkTieOff(_ep.pl);

   // The reference clock is the base SCE-MI clock
   Clock scemiClock = ref_clk;
   Reset scemiReset <- mkAsyncReset(6, _ep.trn.reset_n, scemiClock);

   // We create a clock for use as the uncontrolled clock
   MakeClockIfc#(Bit#(1)) uclkgen <- mkUngatedClock(0, clocked_by scemiClock, reset_by scemiReset);
   Clock uClock = uclkgen.new_clk;

   PulseWire rising_uclock_pw <- mkPulseWire(clocked_by scemiClock, reset_by scemiReset);
   ReadOnly#(Bool) rising_uclock = pulseWireToReadOnly(rising_uclock_pw);

   rule toggle_uclock;
      let new_value = ~uclkgen.getClockValue();
      uclkgen.setClockValue(new_value);
      if (new_value == 1)
	 rising_uclock_pw.send();
   endrule

   // We create a reset for use as the uncontrolled reset
   MakeResetIfc rstgen <- genSceMiReset(uClock, clocked_by scemiClock, reset_by scemiReset);
   Reset uReset = rstgen.new_rst;

   // Setup initial state for the SceMiModule monad
   SceMiModuleState init_state <- get_initial_state(uClock, uReset, scemiClock, scemiReset, rising_uclock, link_type);

   // Execute the SceMi module with the initial state
   let {state, _m} <- runWithContext(init_state, mod, clocked_by uClock, reset_by uReset);

   // Create the clock generation logic
   let clockGenerators <- build_clock_generator(state, rstgen);

   PCIE_PIO_Ifc pio = ?;
   if (valueOf(lanes) == 1) begin
      // The PCIE endpoint is processing TLPWord#(8)s at 62.5MHz.  The
      // PCIE_to_PIO converter works with TLPWord#(16)s at 62.5MHz
      // on one side and presents a PIO interface at uClock speeds on
      // the other.  The connection between the endpoint and the
      // converter is simple in this case and does not require a
      // GearBox, only a TLPData#(16) <--> TLPData#(8) conversion.

      // The PCIe endpoint exports the full (62.5MHz) clock, and clk2 is unconnected.
      Clock epClock62 = _ep.trn.clk;
      Reset epReset62 <- mkAsyncReset(4, _ep.trn.reset_n, epClock62);

      // Convert the PCIE interface to PIO
      SCEMI_to_PCIE_Ifc _pio_conv <- convert_PCIE_to_PIO( state
							, rstgen.isAsserted()
							, _ep.cfg.bus_number
							, _ep.cfg.device_number
							, _ep.cfg.function_number
							, epClock62
							, epReset62
							, clocked_by scemiClock
							, reset_by scemiReset
							);

      // Connect port connections to the endpoint
      mkConnection(_pio_conv.putTLPWord, _ep.trn_rx, clocked_by epClock62, reset_by epReset62);
      mkConnection(_ep.trn_tx, _pio_conv.getTLPWord, clocked_by epClock62, reset_by epReset62);

      pio = _pio_conv.pio;
   end
   else begin
      // The PCIE endpoint is processing TLPWord#(8)s at 250MHz.  The
      // PCIE_to_PIO converter works with TLPWord#(16)s at 125MHz on
      // one side and presents a PIO interface at uClock speeds on the
      // other.  The connection between the endpoint and the converter
      // contains GearBox instances for the TLPWord#(8)@250 <-->
      // TLPWord#(16)@125 conversion.

      // The PCIe endpoint exports full (250MHz) and half-speed (125MHz) clocks
      Clock epClock250 = _ep.trn.clk;
      Reset epReset250 <- mkAsyncReset(4, _ep.trn.reset_n, epClock250);
      Clock epClock125 = _ep.trn.clk2;
      Reset epReset125 <- mkAsyncReset(4, _ep.trn.reset_n, epClock125);

      // Convert the PCIE interface to PIO
      SCEMI_to_PCIE_Ifc _pio_conv <- convert_PCIE_to_PIO( state
							, rstgen.isAsserted()
							, _ep.cfg.bus_number
							, _ep.cfg.device_number
							, _ep.cfg.function_number
							, epClock125
							, epReset125
							, clocked_by scemiClock
							, reset_by scemiReset
							);

      // Connect port connections to the endpoint
      mkConnectionWithClocks(_pio_conv.putTLPWord, _ep.trn_rx, epClock250, epReset250, epClock125, epReset125);
      mkConnectionWithClocks(_ep.trn_tx, _pio_conv.getTLPWord, epClock250, epReset250, epClock125, epReset125);

      pio = _pio_conv.pio;
   end

   // Create the PCIE-to-ports logic
   Empty _pc <- build_PCIE_port_connections( state
                                           , rstgen.isAsserted()
                                           , pio
                                           , clocked_by scemiClock
                                           , reset_by scemiReset
                                           );

   // Pass along the required interface elements
   interface orig_ifc = _m;
   interface pcie     = _ep.pcie;
   method Bool isLinkUp         = _ep.trn.link_up;
   method Bool isOutOfReset     = clockGenerators.outOfReset;
   method Bool isClockAdvancing = clockGenerators.isClockAdvancing;

endmodule: buildSceMiPCIEV6

typedef struct {
   PciId           reqID;
   DWAddress       addr;
   TLPFirstDWBE    firstBE;
   TLPLastDWBE     lastBE;
   TLPTag          tag;
   TLPTrafficClass tc;
   TLPPoison       poisoned;
   TLPAttrNoSnoop  snoop;
   TLPAttrRelaxedOrdering relaxed;
   Bit#(7)         bar;
   Bool            read;
   Bool            dw3;
   Bool            sof;
   Bool            eof;
} PIOInfo deriving (Bits);

module convert_PCIE_to_PIO#(  SceMiModuleState   state
			    , Bool               in_reset
			    , BusNumber          bus_number
			    , DevNumber          device_number
			    , FuncNumber         function_number
			    , Clock              ep_clk
			    , Reset              ep_rstn
			    )
                            (SCEMI_to_PCIE_Ifc);
   
   PciId myReqId = PciId { bus: bus_number, dev: device_number, func: function_number };

   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Functions
   ////////////////////////////////////////////////////////////////////////////////
   function Bit#(32) byteSwap(Bit#(32) w);
      Vector#(4, Bit#(8)) bytes = unpack(w);
      return pack(reverse(bytes));
   endfunction
   
   function TLPData#(16) completeRead(PIOInfo rinfo, Maybe#(Bit#(32)) mdata);
      Bool isPoisoned  = (rinfo.poisoned == POISONED) || !isValid(mdata);
      TLPCompletionHeader hdr = defaultValue;
      hdr.reqid     = rinfo.reqID;
      hdr.cmplid    = myReqId;
      hdr.tag       = rinfo.tag;
      hdr.tclass    = rinfo.tc;
      hdr.length    = 1;
      hdr.poison    = (isPoisoned) ? POISONED : NOT_POISONED;
      hdr.nosnoop   = rinfo.snoop;
      hdr.relaxed   = rinfo.relaxed;
      hdr.loweraddr = getLowerAddr(rinfo.addr, rinfo.firstBE);
      hdr.bytecount = computeByteCount(1, rinfo.firstBE, rinfo.lastBE);
      hdr.data      = byteSwap(validValue(mdata));
      Bit#(128) pkt = pack(hdr);
      TLPData#(16) pw = TLPData { data: pkt
                                , be:   '1
                                , hit:  ?
                                , sof:  True
                                , eof:  True
                                };
      return pw;
   endfunction

   function PIOInfo saveInfo(TLPData#(16) data);
      TLPMemoryIO3DWHeader header3 = unpack(data.data);
      TLPMemory4DWHeader   header4 = unpack(data.data);
      PIOInfo rinfo = ?;

      rinfo.bar  = data.hit;
      rinfo.read = (header3.format == MEM_READ_3DW_NO_DATA || header4.format == MEM_READ_4DW_NO_DATA);
      rinfo.dw3  = (header3.format == MEM_READ_3DW_NO_DATA || header3.format == MEM_WRITE_3DW_DATA);
      rinfo.sof  = data.sof;
      rinfo.eof  = data.eof;

      case(header3.format)
         MEM_WRITE_3DW_DATA,
         MEM_READ_3DW_NO_DATA:
         begin
            rinfo.reqID    = header3.reqid;
            rinfo.addr     = header3.addr;
            rinfo.firstBE  = header3.firstbe;
            rinfo.lastBE   = header3.lastbe;
            rinfo.tag      = header3.tag;
            rinfo.tc       = header3.tclass;
            rinfo.snoop    = header3.nosnoop;
            rinfo.relaxed  = header3.relaxed;
            rinfo.poisoned = header3.poison;
         end
         MEM_WRITE_4DW_DATA,
         MEM_READ_4DW_NO_DATA:
         begin
            rinfo.reqID    = header4.reqid;
            rinfo.addr     = truncate(header4.addr);
            rinfo.firstBE  = header4.firstbe;
            rinfo.lastBE   = header4.lastbe;
            rinfo.tag      = header4.tag;
            rinfo.tc       = header4.tclass;
            rinfo.snoop    = header4.nosnoop;
            rinfo.relaxed  = header4.relaxed;
            rinfo.poisoned = header4.poison;
         end
      endcase

      return rinfo;
   endfunction
   
   function Vector#(16, Bool) allocateTag(Vector#(16, Bool) bs, UInt#(4) lp);
      function f(bspg, b);
	 match {.bs, .p, .going} = bspg;
	 if (going) begin
	    if (b) return tuple3(1 << p, ?, False);
	    else   return tuple3(0, ((p == 15) ? 0 : p+1), True);
	 end
	 else return tuple3(bs, ?, False);
      endfunction
      
      match { .bits, .*, .* } = foldl(f, tuple3(?, lp, True), reverse(rotateBy(reverse(bs), lp)));
      
      return unpack(bits);
   endfunction
   
   function Bool invert(Bool in);
      return !in;
   endfunction

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   SyncFIFOIfc#(TLPData#(16))      	     fRx       		 <- mkSyncFIFOToCC( 16, ep_clk, ep_rstn );
   SyncFIFOIfc#(TLPData#(16))      	     fTx       		 <- mkSyncFIFOFromCC( 16, ep_clk );
   
   Reg#(State)                               rState    		 <- mkReg(IDLE);
   Wire#(Bool)                               wReadAccept      	 <- mkBypassWire;
   Wire#(Bool)                               wWriteAccept     	 <- mkBypassWire;
   
   Counter#(5)                               rReadsInFlight   	 <- mkCounter(0);
   Vector#(16, Reg#(Maybe#(PIOInfo)))        vScoreBoard      	 <- replicateM(mkReg(Invalid));

   PulseWire                                 pwRead              <- mkPulseWire;
   PulseWire                                 pwWrite             <- mkPulseWire;

   Reg#(UInt#(32))                           wAddress            <- mkDWire(0);
   Reg#(Bit#(64))                            wWriteData          <- mkWire;
   Reg#(Bit#(8))                             wWriteBe            <- mkWire;
   Reg#(Bit#(3))                             wBar                <- mkDWire(0);
   Reg#(Bit#(4))                             wTag                <- mkWire;
   
   Reg#(UInt#(32))                           r4DWAddress         <- mkRegU;
   Reg#(Bit#(8))                             r4DWBe              <- mkRegU;
   Reg#(Bit#(3))                             r4DWBar             <- mkRegU;
   
   RWire#(Tuple2#(Bit#(64), Bit#(4)))        rwReadResponse      <- mkRWire;
   
   
   PIOInfo rinfo = saveInfo(fRx.first);
   TLPMemoryIO3DWHeader hdr3 = unpack(fRx.first.data);
   TLPMemory4DWHeader   hdr4 = unpack(fRx.first.data);
   
   Bool    idle      = rState == IDLE;
   Bool    write4dw  = rState == WRITE_4DW;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule process_read(idle && wReadAccept && rinfo.sof && rinfo.read && rReadsInFlight.value < 16);
      let packet  = fRx.first; fRx.deq;
      let scoreb  = readVReg(vScoreBoard);
      let avail   = map(isValid, scoreb);
      
      let granted = allocateTag( avail, 15 );
      avail       = zipWith(\&& , avail, map(invert, granted));
      
      let index   = fromOInt(unpack(pack(granted)));
      
      scoreb[index] = tagged Valid rinfo;
      writeVReg(vScoreBoard, scoreb);
      
      rReadsInFlight.up;
   
      wAddress    <= unpack({ rinfo.addr, 2'b00 });
      wBar        <= { rinfo.bar[4], rinfo.bar[2], rinfo.bar[1] };
      wTag        <= pack(index);
      pwRead.send;
   endrule
   
   rule process_3dw_write(idle && wWriteAccept && rinfo.sof && !rinfo.read && rinfo.dw3);
      fRx.deq;
      
      wAddress   <= unpack({ rinfo.addr, 2'b00 });
      wBar       <= { rinfo.bar[4], rinfo.bar[2], rinfo.bar[1] };
      wWriteData <= rinfo.addr[0] == 1 ? { byteSwap(hdr3.data), 32'd0 } : { 32'd0, byteSwap(hdr3.data) };
      wWriteBe   <= rinfo.addr[0] == 1 ? { rinfo.firstBE, 4'd0 } : { 4'd0, rinfo.firstBE };
      pwWrite.send;
   endrule
   
   rule process_4dw_write(idle && wWriteAccept && rinfo.sof && !rinfo.read && !rinfo.dw3);
      fRx.deq;
      
      r4DWAddress <= unpack({ rinfo.addr, 2'b00 });
      r4DWBar    <= { rinfo.bar[4], rinfo.bar[2], rinfo.bar[1] };
      r4DWBe     <= rinfo.addr[0] == 1 ? { rinfo.firstBE, 4'd0 } : { 4'd0, rinfo.firstBE };
      rState     <= WRITE_4DW;
   endrule
   
   rule process_4dw_write_data(write4dw && wWriteAccept);
      let packet = fRx.first; fRx.deq;
      
      wAddress   <= r4DWAddress;
      wBar       <= r4DWBar;
      wWriteData <= pack(r4DWAddress)[2] == 1 ? { byteSwap(packet.data[127:96]), 32'd0 } : { 32'd0, byteSwap(packet.data[127:96]) };
      wWriteBe   <= r4DWBe;
      rState     <= IDLE;
      pwWrite.send;
   endrule
   
   rule process_read_response(rwReadResponse.wget matches tagged Valid .response 
			      &&& vScoreBoard[tpl_2(response)] matches tagged Valid .info
			      );
      Bit#(32) dhi;
      Bit#(32) dlo;

      match { .data, .tag } = response;
      { dhi, dlo } = split(data);
      let  scoreb = readVReg(vScoreBoard);
      let  rdata  = (info.addr[0] == 1) ? dhi : dlo;
      let  packet = completeRead( info, Valid(rdata) );
      
      fTx.enq(packet);
      
      rReadsInFlight.down;
      scoreb[tag] = tagged Invalid;
      writeVReg(vScoreBoard, scoreb);
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface getTLPWord = toGet(fTx);
   interface putTLPWord = toPut(fRx);
   interface PCIE_PIO_Ifc pio;
      method Action    accept_read();
	 wReadAccept <= True;
      endmethod
      
      method Action    accept_write();
	 wWriteAccept <= True;
      endmethod
      
      method Action    fulfill_read(data, tag);
	 rwReadResponse.wset(tuple2(data, tag));
      endmethod
      
      method Bool      address_valid();
	 return (pwRead || pwWrite);
      endmethod
      
      method Bit#(3)   bar();
	 return wBar;
      endmethod
      
      method UInt#(32) addr();
	 return wAddress;
      endmethod
      
      method Bit#(64)  write_data() if (pwWrite);
	 return wWriteData;
      endmethod
      
      method Bit#(8)   byte_enables() if (pwWrite);
	 return wWriteBe;
      endmethod
      
      method Bit#(4)   tag() if (pwRead);
	 return wTag;
      endmethod
      
   endinterface

endmodule

// Address map (for 64-bit words)
// This should match the src/lib/SceMi/PCIE_address_map.txt file.

typedef UInt#(12) Addr;

function Addr addr64(UInt#(32) n);
   return truncate(n >> 3);
endfunction: addr64

Addr idAndVersionBaseAddr = addr64('h0000);  // BAR1
Addr scemiConfigBaseAddr  = addr64('h0100);  // BAR1
Addr controlBaseAddr      = addr64('h0200);  // BAR1
Addr statusBaseAddr       = addr64('h0300);  // BAR1

// BAR1 fixed addresses

Addr bluespecIdRegAddr         = idAndVersionBaseAddr + addr64('h0000);
Addr addressMapVersionRegAddr  = idAndVersionBaseAddr + addr64('h0008);
Addr scemiVersionRegAddr       = idAndVersionBaseAddr + addr64('h0010);
Addr buildRevisionRegAddr      = idAndVersionBaseAddr + addr64('h0018);
Addr buildTimeStampRegAddr     = idAndVersionBaseAddr + addr64('h0020);

Addr inputChannelCountRegAddr  = scemiConfigBaseAddr  + addr64('h0000);
Addr outputChannelCountRegAddr = scemiConfigBaseAddr  + addr64('h0008);

Addr systemCommandRegAddr      = controlBaseAddr      + addr64('h0000);

Addr systemStatusRegAddr       = statusBaseAddr       + addr64('h0000);
Addr bar1PacketCountRegAddr    = statusBaseAddr       + addr64('h0008);
Addr bar2PacketCountRegAddr    = statusBaseAddr       + addr64('h0010);
Addr errorPacketCountRegAddr   = statusBaseAddr       + addr64('h0018);
Addr cycleStampRegAddr         = statusBaseAddr       + addr64('h0020);
Addr nextOutputChannelRegAddr  = statusBaseAddr       + addr64('h0028);

// BAR2 port addresses

function Bool isOutputChannelAddr(Addr addr) = (pack(addr)[11] == 1);
function Bool isChannelDataAddr(Addr addr);
   if (isOutputChannelAddr(addr))
      return True;
   else
      return (pack(addr)[0] == 1);
endfunction
function UInt#(10) channelNumber(Addr addr);
   if (isOutputChannelAddr(addr))
      return unpack(pack(addr)[9:0]);
   else
      return unpack(pack(addr)[10:1]);
endfunction

// Build the logic to connect PCIE read/write traffic
// to the correct SCE-MI in and out ports.

module build_PCIE_port_connections#( SceMiModuleState state
                                   , Bool             in_reset
                                   , PCIE_PIO_Ifc     pio
                                   )
                                   ();

   // Config & Status registers

   UInt#(8) addr_map_version = 2;

   UInt#(8) scemi_major_version = 1;
   UInt#(8) scemi_minor_version = 0;

   Reg#(UInt#(32)) bar1PacketCount  <- mkReg(0);
   Reg#(UInt#(32)) bar2PacketCount  <- mkReg(0);
   Reg#(UInt#(32)) errorPacketCount <- mkReg(0);

   Reg#(Bool) ready_for_xfer <- mkReg(False);

   // PCIE read/write pipeline state

   Reg#(Maybe#(Addr)) stored_addr <- mkReg(tagged Invalid);
   RWire#(Addr)       addr_w <- mkRWire();

   Reg#(Bool)            rd_is_valid_stage1 <- mkReg(False);
   Reg#(Vector#(3,Bool)) rd_bar_stage1      <- mkRegU();
   Reg#(Addr)            rd_addr_stage1     <- mkRegU();
   Reg#(Bit#(4))         rd_tag_stage1      <- mkRegU();

   Reg#(Bool)            rd_is_valid_stage2 <- mkReg(False);
   Reg#(Bit#(4))         rd_tag_stage2      <- mkRegU();
   Reg#(Bit#(64))        rd_data_stage2     <- mkRegU();

   Reg#(Bit#(8))         wr_bes_stage1  <- mkReg('0);
   Reg#(Vector#(3,Bool)) wr_bar_stage1  <- mkRegU();
   Reg#(Addr)            wr_addr_stage1 <- mkRegU();
   Reg#(Bit#(64))        wr_data_stage1 <- mkRegU();

   // Address handling rules

   (* fire_when_enabled, no_implicit_conditions *)
   rule capture_new_address if (pio.address_valid());
      let addr = truncate(pio.addr() >> 3);
      stored_addr <= tagged Valid addr;
      addr_w.wset(addr);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule use_stored_address if (!pio.address_valid() &&& stored_addr matches tagged Valid .addr);
      addr_w.wset(addr);
   endrule

   // Always accept both reads and writes

   (* fire_when_enabled, no_implicit_conditions *)
   rule always_accept;
      pio.accept_read();
      pio.accept_write();
   endrule

   // Write pipeline implementation

   (* fire_when_enabled *)
   rule initiate_write if (addr_w.wget() matches tagged Valid .addr);
      wr_bes_stage1  <= pio.byte_enables();
      wr_bar_stage1  <= unpack(pio.bar());
      wr_addr_stage1 <= addr;
      wr_data_stage1 <= truncate(pio.write_data()); // implicit_condition = write_enable
   endrule

   (* preempts="initiate_write, write_bubble" *)
   rule write_bubble;
      wr_bes_stage1 <= '0;
   endrule

   // ignore sub-word byte enables
   Bool write_low_word  = wr_bes_stage1[3:0] != 0;
   Bool write_high_word = wr_bes_stage1[7:4] != 0;
   Bit#(64) write_mask  = { (write_high_word ? 32'hffffffff : 32'h00000000)
                          , (write_low_word ? 32'hffffffff : 32'h00000000)
                          };

   // sort ports by number
   List#(InPortInfo)  input_ports  = List::sortBy(cmp_inport_num,  state.inports);
   List#(OutPortInfo) output_ports = List::sortBy(cmp_outport_num, state.outports);

   (* fire_when_enabled, no_implicit_conditions *)
   rule do_write if (write_low_word || write_high_word);
      if (wr_bar_stage1[0]) begin // bar1 write
         if (wr_addr_stage1 == systemCommandRegAddr) begin
            if ((wr_data_stage1 & write_mask) == 64'h00000000_ffffffff) begin
               // perform soft reset
               bar1PacketCount <= bar1PacketCount + 1;
            end
         end
         else begin
            errorPacketCount <= errorPacketCount + 1;
         end
      end
      else if (wr_bar_stage1[1]) begin // bar2 write
         Bool isInPort = !isOutputChannelAddr(wr_addr_stage1);
         Bool isData   = isChannelDataAddr(wr_addr_stage1);
         UInt#(10) portNum = channelNumber(wr_addr_stage1);
         if (isInPort && isData && (portNum < fromInteger(state.inPortCount))) begin
            // write data to input channel
            input_ports[portNum].wordIfc.putWord(wr_data_stage1[31:0]);
            bar2PacketCount <= bar2PacketCount + 1;
         end
         else begin
            errorPacketCount <= errorPacketCount + 1;
         end
      end
      else begin
         // invalid bar
         errorPacketCount <= errorPacketCount + 1;
      end
   endrule

   // Read pipeline implementation

   (* fire_when_enabled *)
   rule initiate_read if (addr_w.wget() matches tagged Valid .addr);
      rd_is_valid_stage1 <= True;
      rd_tag_stage1      <= pio.tag(); // implicit condition = read_enable
      rd_bar_stage1      <= unpack(pio.bar());
      rd_addr_stage1     <= addr;
   endrule

   (* preempts="initiate_read, read_bubble" *)
   rule read_bubble;
      rd_is_valid_stage1 <= False;
   endrule

   (* fire_when_enabled, no_implicit_conditions, mutually_exclusive="do_read,do_write" *)
   rule do_read if (rd_is_valid_stage1);
      if (rd_bar_stage1[0]) begin // bar1 read
         Bool is_ok = True;
         case (rd_addr_stage1)
            bluespecIdRegAddr:         rd_data_stage2 <= 64'h42_6c_75_65_73_70_65_63; // Bluespec
            addressMapVersionRegAddr:  rd_data_stage2 <= zeroExtend(pack(addr_map_version));
            scemiVersionRegAddr:       rd_data_stage2 <= zeroExtend({ pack(scemi_major_version)
                                                                    , pack(scemi_minor_version)
                                                                    });
            buildRevisionRegAddr:      rd_data_stage2 <= zeroExtend(pack(buildVersion));
            buildTimeStampRegAddr:     rd_data_stage2 <= zeroExtend(pack(epochTime));
            inputChannelCountRegAddr:  rd_data_stage2 <= fromInteger(state.inPortCount);
            outputChannelCountRegAddr: rd_data_stage2 <= fromInteger(state.outPortCount);
            systemCommandRegAddr:      rd_data_stage2 <= '0;
            systemStatusRegAddr:       rd_data_stage2 <= zeroExtend(pack(in_reset));
            bar1PacketCountRegAddr:    rd_data_stage2 <= zeroExtend(pack(bar1PacketCount));
            bar2PacketCountRegAddr:    rd_data_stage2 <= zeroExtend(pack(bar2PacketCount));
            errorPacketCountRegAddr:   rd_data_stage2 <= zeroExtend(pack(errorPacketCount));
            cycleStampRegAddr:         rd_data_stage2 <= pack(nextMsgCycle(state));
	    nextOutputChannelRegAddr:  begin
					  if (hasOutputMsg(state)) begin
					     rd_data_stage2 <= zeroExtend({1'b1,pack(nextMsgChannel(state))});
					     ready_for_xfer <= True;
					  end
					  else
					     rd_data_stage2 <= '0;
				       end
            default:                   begin
                                          rd_data_stage2 <= 64'h0bad_0bad_0bad_0bad;
                                          is_ok = False;
                                       end
         endcase
         if (is_ok) bar1PacketCount <= bar1PacketCount + 1;
         else       errorPacketCount <= errorPacketCount + 1;
      end
      else if (rd_bar_stage1[1]) begin // bar2 read
	 Bool isInPort = !isOutputChannelAddr(rd_addr_stage1);
	 Bool isData   = isChannelDataAddr(rd_addr_stage1);
	 UInt#(10) portNum = channelNumber(rd_addr_stage1);
	 Bool is_ok = True;
	 if (isInPort) begin
	    if (portNum >= fromInteger(state.inPortCount)) begin
	       // port number is out of range
	       rd_data_stage2 <= 64'h1bad_1bad_1bad_1bad;
	       is_ok = False;
            end
	    else if (isData) begin
	       // input data port is write-only
	       rd_data_stage2 <= 64'h2bad_2bad_2bad_2bad;
	       is_ok = False;
	    end
            else begin
	       // read buffer available indicator
	       rd_data_stage2 <= zeroExtend(pack(input_ports[portNum].wordIfc.hasRequest()));
	    end
	 end
	 else begin
	    if (portNum >= fromInteger(state.outPortCount)) begin
	       // port number is out of range
	       rd_data_stage2 <= 64'h3bad_3bad_3bad_3bad;
	       is_ok = False;
            end
	    else begin
	       // read output data
	       Bit#(32) data <- output_ports[portNum].wordIfc.takeWord();
	       rd_data_stage2 <= zeroExtend(data);
	    end
	    if (ready_for_xfer) begin
	       advanceOutputMsg(state);
	       ready_for_xfer <= False;
	    end
	 end
	 if (is_ok) bar2PacketCount <= bar2PacketCount + 1;
	 else       errorPacketCount <= errorPacketCount + 1;
      end
      else
      begin
         // invalid bar
         rd_data_stage2 <= 64'hdead_dead_dead_dead;
         errorPacketCount <= errorPacketCount + 1;
      end
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule advance_read_pipeline;
      rd_is_valid_stage2 <= rd_is_valid_stage1;
      rd_tag_stage2      <= rd_tag_stage1;
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule finish_read if (rd_is_valid_stage2);
      pio.fulfill_read(rd_data_stage2, rd_tag_stage2);
   endrule

   // output port ordering infrastructure

   if (state.outPortCount > 0) begin
      rule scan_output_ports;
	 if (state.currentOutPort < fromInteger(state.outPortCount-1))
	    state.currentOutPort <= state.currentOutPort + 1;
	 else
	    state.currentOutPort <= 0;
      endrule
   end

   rule drop_output if (state.is_in_reset);
      clearOutputMsgs(state);
   endrule

endmodule: build_PCIE_port_connections

endpackage: SceMiVirtex6PCIE
