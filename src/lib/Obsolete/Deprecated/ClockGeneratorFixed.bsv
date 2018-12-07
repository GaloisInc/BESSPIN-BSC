// Copyright 2009-2010 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

import Xilinx::*;

import Clocks            ::*;
import DefaultValue      ::*;
import TieOff            ::*;
import Vector            ::*;
import Real              ::*;

// Bluespec's mkClockGenerator implementation is broken, because it doesn't
// translate the PLL params correctly. Here's my fixed version.

interface VPLL_DRP;
   method    Action   request(Bool write, Bit#(5) addr, Bit#(16) datain);
   method    Bit#(16) response;
endinterface: VPLL_DRP

interface VPLL;
   interface Clock       clkout0;
   interface Clock       clkout1;
   interface Clock       clkout2;
   interface Clock       clkout3;
   interface Clock       clkout4;
   interface Clock       clkout5;
   interface Clock       clkoutdcm0;
   interface Clock       clkoutdcm1;
   interface Clock       clkoutdcm2;
   interface Clock       clkoutdcm3;
   interface Clock       clkoutdcm4;
   interface Clock       clkoutdcm5;
   interface Clock       clkfbout;
   interface Clock       clkfbdcm;
   interface VPLL_DRP    recfg;
   (* always_ready *)
   method    Bool        locked;
   (* always_ready, always_enabled *)
   method    Action      rel(Bool i);
   (* always_ready, always_enabled *)
   method    Action      clkin1sel(Bool select);
   (* always_ready, always_enabled *)
   method    Action      fbin(Bool i);
endinterface: VPLL

import "BVI" PLL_ADV =
module vMkPLL#(PLLParams params, Clock clkin2/*, Clock fbin*/, Clock dclk)(VPLL);
   default_clock clk1(CLKIN1);
   default_reset rst(RST);

   parameter BANDWIDTH              = params.bandwidth;
   parameter CLKFBOUT_DESKEW_ADJUST = params.clkfbout_deskew_adjust;
   parameter CLKOUT0_DESKEW_ADJUST  = params.clkout0_deskew_adjust;
   parameter CLKOUT1_DESKEW_ADJUST  = params.clkout1_deskew_adjust;
   parameter CLKOUT2_DESKEW_ADJUST  = params.clkout2_deskew_adjust;
   parameter CLKOUT3_DESKEW_ADJUST  = params.clkout3_deskew_adjust;
   parameter CLKOUT4_DESKEW_ADJUST  = params.clkout4_deskew_adjust;
   parameter CLKOUT5_DESKEW_ADJUST  = params.clkout5_deskew_adjust;
   parameter CLKFBOUT_MULT          = params.clkfbout_mult;
   parameter CLKFBOUT_PHASE         = params.clkfbout_phase;
   parameter CLKIN1_PERIOD          = params.clkin1_period;
   parameter CLKIN2_PERIOD          = params.clkin2_period;
   parameter CLKOUT0_DIVIDE         = params.clkout0_divide;
   parameter CLKOUT0_DUTY_CYCLE     = params.clkout0_duty_cycle;
   parameter CLKOUT0_PHASE          = params.clkout0_phase;
   parameter CLKOUT1_DIVIDE         = params.clkout1_divide;
   parameter CLKOUT1_DUTY_CYCLE     = params.clkout1_duty_cycle;
   parameter CLKOUT1_PHASE          = params.clkout1_phase;
   parameter CLKOUT2_DIVIDE         = params.clkout2_divide;
   parameter CLKOUT2_DUTY_CYCLE     = params.clkout2_duty_cycle;
   parameter CLKOUT2_PHASE          = params.clkout2_phase;
   parameter CLKOUT3_DIVIDE         = params.clkout3_divide;
   parameter CLKOUT3_DUTY_CYCLE     = params.clkout3_duty_cycle;
   parameter CLKOUT3_PHASE          = params.clkout3_phase;
   parameter CLKOUT4_DIVIDE         = params.clkout4_divide;
   parameter CLKOUT4_DUTY_CYCLE     = params.clkout4_duty_cycle;
   parameter CLKOUT4_PHASE          = params.clkout4_phase;
   parameter CLKOUT5_DIVIDE         = params.clkout5_divide;
   parameter CLKOUT5_DUTY_CYCLE     = params.clkout5_duty_cycle;
   parameter CLKOUT5_PHASE          = params.clkout5_phase;
   parameter COMPENSATION           = params.compensation;
   parameter DIVCLK_DIVIDE          = params.divclk_divide;
   parameter EN_REL                 = params.en_rel;
   parameter PLL_PMCD_MODE          = params.pll_pmcd_mode;
   parameter REF_JITTER             = params.ref_jitter;
   parameter RESET_ON_LOSS_OF_LOCK  = params.reset_on_loss_of_lock;
   parameter RST_DEASSERT_CLK       = params.rst_deassert_clk;

   /// XXX Potentially hazardous as these clocks are intended to be ungated
   /// XXX and if a gated clock is connected (with the directives below this
   /// XXX is now allowed) then the gate signal will not properly disable the
   /// XXX clock.
   input_clock clk2(CLKIN2, (*unused*)CLKIN2_GATE)   = clkin2;
   input_clock dclk(DCLK, (*unused*)DCLK_GATE)       = dclk;
   //put_clock fbclk(CLKFBIN) = fbin;

   output_clock clkfbout(CLKFBOUT);
   output_clock clkfbdcm(CLKFBDCM);
   output_clock clkout0(CLKOUT0);
   output_clock clkout1(CLKOUT1);
   output_clock clkout2(CLKOUT2);
   output_clock clkout3(CLKOUT3);
   output_clock clkout4(CLKOUT4);
   output_clock clkout5(CLKOUT5);

   output_clock clkoutdcm0(CLKOUTDCM0);
   output_clock clkoutdcm1(CLKOUTDCM1);
   output_clock clkoutdcm2(CLKOUTDCM2);
   output_clock clkoutdcm3(CLKOUTDCM3);
   output_clock clkoutdcm4(CLKOUTDCM4);
   output_clock clkoutdcm5(CLKOUTDCM5);

   method LOCKED locked()            clocked_by(no_clock) reset_by(no_reset);
   method        rel(REL)            enable((*inhigh*)en0) clocked_by(clk1) reset_by(rst);
   method        clkin1sel(CLKINSEL) enable((*inhigh*)en1) clocked_by(clk1) reset_by(rst);

   method        fbin(CLKFBIN)       enable((*inhigh*)en2) clocked_by(clkfbout) reset_by(no_reset);

   interface VPLL_DRP recfg;
      method        request(DWE, DADDR, DI) enable(DEN) clocked_by(dclk) reset_by(no_reset);
      method DO     response() ready(DRDY) clocked_by(dclk) reset_by(no_reset);
   endinterface

   schedule (locked, rel, clkin1sel, fbin) CF (locked, rel, clkin1sel, fbin);
   schedule recfg_response SB recfg_request;
   schedule recfg_request C recfg_request;
   schedule recfg_response CF recfg_response;

endmodule: vMkPLL

module mkClockGeneratorFixed#(ClockGeneratorParams params)(ClockGenerator);

   let refclkMHz = trunc(1000 / params.clkin1_period);
   let vcoclkMHz = refclkMHz * params.feedback_mul / params.feedback_div;

   if (vcoclkMHz < 400 || vcoclkMHz > 1400) error("The internal feedback frequency must be between 400 MHz and 1400 MHz.");

   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                                     clk                 <- exposeCurrentClock;
   Clock                                     clk_buffered         = ?;

   if (params.clkin_buffer) begin
      Clock inbuffer <- mkClockIBUFG;
      clk_buffered  = inbuffer;
   end
   else begin
      clk_buffered  = clk;
   end

   Reset                                     rst_n               <- mkAsyncResetFromCR(3, clk_buffered);
   Reset                                     rst                 <- mkResetInverter(rst_n);

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   PLLParams                                 pll_params           = defaultValue;
   pll_params.clkin1_period      = params.clkin1_period;
   pll_params.clkfbout_mult      = params.feedback_mul;
   pll_params.divclk_divide      = params.feedback_div;
   pll_params.clkout0_divide     = params.clk0_div;
   pll_params.clkout0_duty_cycle = params.clk0_duty_cycle;
   pll_params.clkout0_phase      = params.clk0_phase;
   pll_params.clkout1_divide     = params.clk1_div;
   pll_params.clkout1_duty_cycle = params.clk1_duty_cycle;
   pll_params.clkout1_phase      = params.clk1_phase;
   pll_params.clkout2_divide     = params.clk2_div;
   pll_params.clkout2_duty_cycle = params.clk2_duty_cycle;
   pll_params.clkout2_phase      = params.clk2_phase;
   pll_params.clkout3_divide     = params.clk3_div;
   pll_params.clkout3_duty_cycle = params.clk3_duty_cycle;
   pll_params.clkout3_phase      = params.clk3_phase;
   pll_params.clkout4_divide     = params.clk4_div;
   pll_params.clkout4_duty_cycle = params.clk4_duty_cycle;
   pll_params.clkout4_phase      = params.clk4_phase;
   pll_params.clkout5_divide     = params.clk5_div;
   pll_params.clkout5_duty_cycle = params.clk5_duty_cycle;
   pll_params.clkout5_phase      = params.clk5_phase;

   VPLL                                      pll                 <- vMkPLL(pll_params, noClock, noClock, clocked_by clk_buffered, reset_by rst);

   ReadOnly#(Bool)                           clkfbbuf            <- mkClockBitBUFG(clocked_by pll.clkfbout);
   Clock                                     clkout0buf          <- mkClockBUFG(clocked_by pll.clkout0);
   Clock                                     clkout1buf          <- mkClockBUFG(clocked_by pll.clkout1);
   Clock                                     clkout2buf          <- mkClockBUFG(clocked_by pll.clkout2);
   Clock                                     clkout3buf          <- mkClockBUFG(clocked_by pll.clkout3);
   Clock                                     clkout4buf          <- mkClockBUFG(clocked_by pll.clkout4);
   Clock                                     clkout5buf          <- mkClockBUFG(clocked_by pll.clkout5);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled, no_implicit_conditions *)
   rule connect_clkin1sel;
      pll.clkin1sel(True);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule connect_rel;
      pll.rel(False);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule connect_feedback;
      pll.fbin(clkfbbuf);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface clkout0 = clkout0buf;
   interface clkout1 = clkout1buf;
   interface clkout2 = clkout2buf;
   interface clkout3 = clkout3buf;
   interface clkout4 = clkout4buf;
   interface clkout5 = clkout5buf;
   method    locked  = pll.locked;

endmodule

