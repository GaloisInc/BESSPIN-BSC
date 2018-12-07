// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiDiniPCIE;

// This is an implementation of SceMi over PCI-Express using the Dini endpoint.

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

import SceMiDefines::*;
import SceMiInternals::*;
import SceMiClocks::*;

import DiniPCIE::*;

// Interface wrapper for PCIE
interface SceMiDiniPCIEIfc#(type i);
   interface i orig_ifc;
   interface PCIE_Pins pcie;
   (* always_ready *)
   method Bool isOutOfReset();
   (* always_ready *)
   method Bool isClockAdvancing();
endinterface

// Argument structure used for passing in PCIE clocks
typedef struct {
   Clock         pci_sample_clk;
   Reset         pci_sample_rstn;
   SceMiLinkType link_type;
} SceMiDiniPCIEArgs;

// This module builds the transactor hierarchy, the clock
// generation logic and the PCIE-to-port logic.
module [Module] buildSceMiPCIEDini#( SceMiModule#(i) mod
				   , Clock pcie_sample_clk
				   , Reset pcie_sample_rstn
			           , SceMiLinkType link_type
			           )
			           (SceMiDiniPCIEIfc#(i));

   // Expose clock and reset
   Clock sys_clk  <- exposeCurrentClock();
   Reset sys_rstn <- exposeCurrentReset();

   // Instantiate the PCIE endpoint
   PCIE_Ifc dini_pcie <- mkPCIE(pcie_sample_clk, pcie_sample_rstn, sys_clk, sys_rstn);

   // The sys_clk used with the PCIE endpoint is also the base SCE-MI clock
   Clock scemiClock = sys_clk;
   Reset scemiReset = sys_rstn;

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

   // Create the PCIE-to-ports logic
   (*hide*)
   Empty _pc <- build_PCIE_port_connections( state
					   , rstgen.isAsserted()
					   , dini_pcie.pio
					   , clocked_by scemiClock
					   , reset_by scemiReset
					   );

   // Pass along the required interface elements
   interface orig_ifc = _m;
   interface pcie     = dini_pcie.pins;
   method Bool isOutOfReset     = clockGenerators.outOfReset;
   method Bool isClockAdvancing = clockGenerators.isClockAdvancing;

endmodule: buildSceMiPCIEDini

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
	    cycleStampRegAddr:         begin
                                          if (hasOutputMsg(state))
					     rd_data_stage2 <= pack(nextMsgCycle(state));
					  else
					     rd_data_stage2 <= '0;
				       end
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
	       // read request status
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

endpackage: SceMiDiniPCIE
