// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package DiniPCIE;

import Clocks::*;

export PCIE_Pins(..), PCIE_PIO_Ifc(..), PCIE_DMA_Ifc(..), PCIE_Ifc(..);
export mkPCIE;

// This is the set of signals which routes to the FPGA boundary
// and out to the PCIE connector or MEG array.
interface PCIE_Pins;
   // inputs
   (* always_enabled, prefix="" *)
   method Action in_chan((* port="IN_CHAN" *) Bit#(3) x);
   (* always_enabled, prefix="" *)
   method Action in_d((* port="IN_D" *) Bit#(64) x);
   (* always_enabled, prefix="" *)
   method Action in_sof((* port="IN_SOF" *) Bool x);
   (* always_enabled, prefix="" *)
   method Action in_eof((* port="IN_EOF" *) Bool x);
   (* always_enabled, prefix="" *)
   method Action in_info((* port="IN_INFO" *) Bit#(2) x);
   (* always_enabled, prefix="" *)
   method Action in_tc((* port="IN_TC" *) Bit#(2) x);
   (* always_enabled, prefix="" *)
   method Action in_extra((* port="IN_EXTRA" *) Bit#(6) x);
   (* always_enabled, prefix="" *)
   method Action in_valid((* port="IN_VALID" *) Bool x);
   (* always_enabled, prefix="" *)
   method Action in_all_valid((* port="IN_ALL_VALID" *) Bool x);
   (* always_enabled, prefix="" *)
   method Action in_almost_full((* port="IN_ALMOST_FULL" *) Bool x);
   // outputs
   (* always_ready, result="OUT_CHAN" *)
   method Bit#(3)  out_chan();
   (* always_ready, result="OUT_D" *)
   method Bit#(64) out_d();
   (* always_ready, result="OUT_SOF" *)
   method Bool     out_sof();
   (* always_ready, result="OUT_EOF" *)
   method Bool     out_eof();
   (* always_ready, result="OUT_INFO" *)
   method Bit#(2)  out_info();
   (* always_ready, result="OUT_TC" *)
   method Bit#(2)  out_tc();
   (* always_ready, result="OUT_EXTRA" *)
   method Bit#(7)  out_extra();
   (* always_ready, result="OUT_VALID" *)
   method Bool     out_valid();
   (* always_ready, result="OUT_ALL_VALID" *)
   method Bool     out_all_valid();
   (* always_ready, result="OUT_PRESENT" *)
   method Bool     out_present();
   (* always_ready, result="OUT_ALMOST_FULL" *)
   method Bool     out_almost_full();
endinterface: PCIE_Pins

// This is the interface for programmed IO through the PCIE block
interface PCIE_PIO_Ifc;
   (* always_ready *)
   method Action    accept_read();
   (* always_ready *)
   method Action    accept_write();
   (* always_ready *)
   method Action    fulfill_read(Bit#(64) data, Bit#(4) tag);
   (* always_ready *)
   method Bool      address_valid();
   (* always_ready *)
   method Bit#(3)   bar();
   (* always_ready *)
   method UInt#(32) addr();
   method Bit#(64)  write_data();
   method Bit#(8)   byte_enables();
   method Bit#(4)   tag();
endinterface: PCIE_PIO_Ifc

// This is the interface for DMA through the PCIE block
interface PCIE_DMA_Ifc;
   method Bit#(64) data();
   method Bit#(8)  ctrl();
   (* always_ready *)
   method Bool     almost_full();
   (* always_ready *)
   method Action   advance();
   (* always_ready *)
   method Action   to_host(Bit#(64) data, Bit#(8) ctrl);
endinterface: PCIE_DMA_Ifc

// This is the full interface of the Dini PCIE user-fpga-side block
interface PCIE_Ifc;
   interface PCIE_Pins pins;
   interface PCIE_PIO_Ifc pio;
   interface PCIE_DMA_Ifc dma0;
   interface PCIE_DMA_Ifc dma1;
endinterface: PCIE_Ifc

// Import wrapper around Dini's pcie_x8_user_interface module

import "BVI" pcie_x8_user_interface =
   module rawPCIE(Clock sample_clock, Reset sample_resetn,
                  Clock pcie_clock, Reset pcie_resetn, Reset pcie_reset,
                  PCIE_Ifc ifc);
      default_clock no_clock;
      no_reset;

      input_clock sample_clk(pcie8t_qclk_in) = sample_clock;
      input_reset sample_rstn() clocked_by(sample_clk) = sample_resetn;

      input_clock pcie_clk(pcie8t_qclk_out)  = pcie_clock;
      input_reset pcie_rstn() clocked_by(pcie_clk) = pcie_resetn;
      input_reset pcie_rst(pcie_reset) clocked_by(pcie_clk) = pcie_reset;

      interface PCIE_Pins pins;
	 method in_chan (pcie_in_chan_reg)
	    enable((*inhigh*)in_chan_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_d (pcie_in_d_reg)
	    enable((*inhigh*)in_d_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_sof (pcie_in_sof_reg )
	    enable((*inhigh*)in_sof_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_eof (pcie_in_eof_reg )
	    enable((*inhigh*)in_eof_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_info (pcie_in_info_reg)
	    enable((*inhigh*)in_info_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_tc (pcie_in_tc_reg)
	    enable((*inhigh*)in_tc_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_extra (pcie_in_extra_reg)
	    enable((*inhigh*)in_extra_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_valid (pcie_in_valid_reg )
	    enable((*inhigh*)in_valid_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_all_valid (pcie_in_all_valid_reg )
	    enable((*inhigh*)in_all_valid_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method in_almost_full (pcie_in_almost_full_reg )
	    enable((*inhigh*)in_almost_full_enable) clocked_by(sample_clk) reset_by(sample_rstn);
	 method pcie_out_chan_preff out_chan ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_d_preff out_d ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_sof_preff out_sof ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_eof_preff out_eof ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_info_preff out_info ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_tc_preff out_tc ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_extra_preff out_extra ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_valid_preff out_valid ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_all_valid_preff out_all_valid ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_present_preff out_present ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method pcie_out_almost_full_preff out_almost_full ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
      endinterface

      interface PCIE_PIO_Ifc pio;
	 method accept_read ()
	    enable(target_read_accept) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method accept_write ()
	    enable(target_write_accept) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method fulfill_read (target_read_data, target_read_data_tag)
	    enable(target_read_data_valid) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method target_bar bar ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method target_address addr ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method target_address_valid address_valid()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method target_write_data write_data ()
	    ready(target_write_enable) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method target_write_be byte_enables ()
	    ready(target_write_enable) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method target_request_tag tag ()
	    ready(target_read_enable) clocked_by(pcie_clk) reset_by(pcie_rstn);
      endinterface

      interface PCIE_DMA_Ifc dma0;
	 method dma0_from_host_data data ()
	    ready(dma0_from_host_valid) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method dma0_from_host_ctrl ctrl ()
	    ready(dma0_from_host_valid) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method dma0_to_host_almost_full almost_full ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method advance ()
	    enable(dma0_from_host_advance) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method to_host (dma0_to_host_data, dma0_to_host_ctrl)
	    enable(dma0_to_host_valid) clocked_by(pcie_clk) reset_by(pcie_rstn);
      endinterface

      interface PCIE_DMA_Ifc dma1;
	 method dma1_from_host_data data ()
	    ready(dma1_from_host_valid) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method dma1_from_host_ctrl ctrl ()
	    ready(dma1_from_host_valid) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method dma1_to_host_almost_full almost_full ()
	    clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method advance ()
	    enable(dma1_from_host_advance) clocked_by(pcie_clk) reset_by(pcie_rstn);
	 method to_host (dma1_to_host_data, dma1_to_host_ctrl)
	    enable(dma1_to_host_valid) clocked_by(pcie_clk) reset_by(pcie_rstn);
      endinterface

      schedule (pins_in_chan, pins_in_d, pins_in_sof, pins_in_eof, pins_in_info,
                pins_in_tc, pins_in_extra, pins_in_valid,
                pins_in_all_valid, pins_in_almost_full)
               CF
               (pins_in_chan, pins_in_d, pins_in_sof, pins_in_eof, pins_in_info,
                pins_in_tc, pins_in_extra, pins_in_valid,
                pins_in_all_valid, pins_in_almost_full);
      schedule (pio_accept_read, pio_accept_write, pio_fulfill_read, pio_tag,
                pio_bar, pio_addr, pio_address_valid, pio_write_data, pio_byte_enables,
                dma0_advance, dma0_to_host, dma0_data, dma0_ctrl, dma0_almost_full,
                dma1_advance, dma1_to_host, dma1_data, dma1_ctrl, dma1_almost_full)
               CF
               (pio_accept_read, pio_accept_write, pio_fulfill_read, pio_tag,
                pio_bar, pio_addr, pio_address_valid, pio_write_data, pio_byte_enables,
                dma0_advance, dma0_to_host, dma0_data, dma0_ctrl, dma0_almost_full,
                dma1_advance, dma1_to_host, dma1_data, dma1_ctrl, dma1_almost_full);
   endmodule


// Wrapper module around rawPCIE that adds the registers required on the input
// and output pins
(* synthesize *)
(* clock_prefix="", reset_prefix="", no_default_clock, no_default_reset *)
module mkPCIE(Clock sample_clock, Reset sample_resetn, Clock pcie_clock, Reset pcie_resetn, PCIE_Ifc ifc);

   // input registers
   Reg#(Bit#(64)) i_d_reg           <- mkReg('0, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bool)     i_sof_reg         <- mkReg(False, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bool)     i_eof_reg         <- mkReg(False, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bit#(2))  i_tc_reg          <- mkReg('0, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bit#(3))  i_chan_reg        <- mkReg('0, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bool)     i_valid_reg       <- mkReg(False, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bool)     i_all_valid_reg   <- mkReg(False, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bit#(2))  i_info_reg        <- mkReg('0, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bool)     i_almost_full_reg <- mkReg(False, clocked_by sample_clock, reset_by sample_resetn);
   Reg#(Bit#(6))  i_extra_reg       <- mkReg('0, clocked_by sample_clock, reset_by sample_resetn);

   // raw PCIE endpoint
   Reset pcie_reset <- mkResetInverter(pcie_resetn, clocked_by pcie_clock);
   PCIE_Ifc raw <- rawPCIE(sample_clock, sample_resetn, pcie_clock, pcie_resetn, pcie_reset);

   // output registers
   Reg#(Bit#(64)) o_d_reg           <- mkReg('0, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bool)     o_sof_reg         <- mkReg(False, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bool)     o_eof_reg         <- mkReg(False, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bit#(2))  o_tc_reg          <- mkReg('0, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bit#(3))  o_chan_reg        <- mkReg('0, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bool)     o_valid_reg       <- mkReg(False, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bool)     o_all_valid_reg   <- mkReg(False, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bit#(2))  o_info_reg        <- mkReg('0, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bool)     o_almost_full_reg <- mkReg(False, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bit#(7))  o_extra_reg       <- mkReg('0, clocked_by pcie_clock, reset_by pcie_resetn);
   Reg#(Bool)     o_present_reg     <- mkReg(False, clocked_by pcie_clock, reset_by pcie_resetn);

   // input register connections
   (* fire_when_enabled, no_implicit_conditions *)
   rule input_registers;
      raw.pins.in_chan(i_chan_reg);
      raw.pins.in_d(i_d_reg);
      raw.pins.in_sof(i_sof_reg);
      raw.pins.in_eof(i_eof_reg);
      raw.pins.in_info(i_info_reg);
      raw.pins.in_tc(i_tc_reg);
      raw.pins.in_extra(i_extra_reg);
      raw.pins.in_valid(i_valid_reg);
      raw.pins.in_all_valid(i_all_valid_reg);
      raw.pins.in_almost_full(i_almost_full_reg);
   endrule

   // output register connections
   (* fire_when_enabled, no_implicit_conditions *)
   rule output_registers;
      o_chan_reg        <= raw.pins.out_chan();
      o_d_reg           <= raw.pins.out_d();
      o_sof_reg         <= raw.pins.out_sof();
      o_eof_reg         <= raw.pins.out_eof();
      o_info_reg        <= raw.pins.out_info();
      o_tc_reg          <= raw.pins.out_tc();
      o_valid_reg       <= raw.pins.out_valid();
      o_all_valid_reg   <= raw.pins.out_all_valid();
      o_almost_full_reg <= raw.pins.out_almost_full();
      o_extra_reg       <= raw.pins.out_extra();
      o_present_reg     <= raw.pins.out_present();
   endrule

   interface PCIE_Pins pins;
      // inputs
      method in_chan        = i_chan_reg._write;
      method in_d           = i_d_reg._write;
      method in_sof         = i_sof_reg._write;
      method in_eof         = i_eof_reg._write;
      method in_info        = i_info_reg._write;
      method in_tc          = i_tc_reg._write;
      method in_extra       = i_extra_reg._write;
      method in_valid       = i_valid_reg._write;
      method in_all_valid   = i_all_valid_reg._write;
      method in_almost_full = i_almost_full_reg._write;
      // outputs
      method Bit#(3) out_chan         = o_chan_reg;
      method Bit#(64) out_d           = o_d_reg;
      method Bool     out_sof         = o_sof_reg;
      method Bool     out_eof         = o_eof_reg;
      method Bit#(2)  out_info        = o_info_reg;
      method Bit#(2)  out_tc          = o_tc_reg;
      method Bit#(7)  out_extra       = o_extra_reg;
      method Bool     out_valid       = o_valid_reg;
      method Bool     out_all_valid   = o_all_valid_reg;
      method Bool     out_present     = o_present_reg;
      method Bool     out_almost_full = o_almost_full_reg;
   endinterface

   interface PCIE_PIO_Ifc pio  = raw.pio;
   interface PCIE_DMA_Ifc dma0 = raw.dma0;
   interface PCIE_DMA_Ifc dma1 = raw.dma1;

endmodule

endpackage: DiniPCIE
