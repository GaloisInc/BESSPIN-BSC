// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Dini7002;

import NoCUtils :: *;

import DiniRS232::*;
import DiniSODIMM::*;

import PCIE    :: *;
import LEDController :: *;

export DN7002_FPGA_A(..),      DN7002_FPGA_B(..);
export DN7002_FPGA_A_DDR2(..), DN7002_FPGA_B_DDR2(..);
export DN7002_FPGA_A_SRAM(..), DN7002_FPGA_B_SRAM(..);

export DN7002_FPGA_Q(..);

// Non-SODIMM variants

interface DN7002_FPGA_A;
   (* prefix="TRAIN" *)
   interface TrainingSignals training;
   (* prefix="NOC_SOURCE" *)
   interface FPGASource#(4) source;
   (* prefix="NOC_SINK" *)
   interface FPGASink#(4) sink;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(6) leds();
endinterface

interface DN7002_FPGA_B;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

// DDR2 variants

interface DN7002_FPGA_A_DDR2;
   interface Clock noc_clk;
   (* prefix="TRAIN" *)
   interface TrainingSignals training;
   (* prefix="NOC_SOURCE" *)
   interface FPGASource#(4) source;
   (* prefix="NOC_SINK" *)
   interface FPGASink#(4) sink;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(6) leds();
endinterface

interface DN7002_FPGA_B_DDR2;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

// SRAM variants

interface DN7002_FPGA_A_SRAM;
   interface Clock noc_clk;
   (* prefix="TRAIN" *)
   interface TrainingSignals training;
   (* prefix="NOC_SOURCE" *)
   interface FPGASource#(4) source;
   (* prefix="NOC_SINK" *)
   interface FPGASink#(4) sink;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(6) leds();
endinterface

interface DN7002_FPGA_B_SRAM;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

// FPGA Q

interface DN7002_FPGA_Q;
   interface Clock           refclk125;
   interface Reset           refrst125;
   interface LED#(4)         leds;
   interface PCIE_EXP#(8)    pcie;
   interface TrainingSignals training;
   interface FPGASource#(4)  source;
   interface FPGASink#(4)    sink;
endinterface

endpackage: Dini7002
