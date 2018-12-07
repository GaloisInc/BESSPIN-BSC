// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Dini7406;

import NoCUtils :: *;

import DiniPCIE   :: *;
import DiniRS232  :: *;
import DiniSODIMM :: *;

import PCIE          :: *;
import LEDController :: *;

export DN7406_FPGA_A(..);
export DN7406_FPGA_B(..);
export DN7406_FPGA_C(..);
export DN7406_FPGA_D(..);
export DN7406_FPGA_E(..);
export DN7406_FPGA_F(..);
export DN7406_FPGA_A_DDR2(..);
export DN7406_FPGA_C_DDR2(..);
export DN7406_FPGA_D_DDR2(..);
export DN7406_FPGA_F_DDR2(..);
export DN7406_FPGA_A_SRAM(..);
export DN7406_FPGA_C_SRAM(..);
export DN7406_FPGA_D_SRAM(..);
export DN7406_FPGA_F_SRAM(..);

export DN7406_FPGA_Q(..);

// non-SODIMM variants

interface DN7406_FPGA_A;
   (* prefix="TRAIN" *)
   interface TrainingSignals training;
   (* prefix="NOC_SOURCE" *)
   interface FPGASource#(4) source;
   (* prefix="NOC_SINK" *)
   interface FPGASink#(4) sink;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(3) leds();
   (* always_ready *)
   method Bool isDDRReady();
   (* always_ready *)
   method Bool isOutOfReset();
endinterface

interface DN7406_FPGA_B;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(13) leds();
endinterface

interface DN7406_FPGA_C;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(11) leds();
endinterface

interface DN7406_FPGA_D;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

interface DN7406_FPGA_E;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(3) leds();
endinterface

interface DN7406_FPGA_F;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

// DDR2 variants

interface DN7406_FPGA_A_DDR2;
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
   method Bit#(3) leds();
   (* always_ready *)
   method Bool isDDRReady();
   (* always_ready *)
   method Bool isOutOfReset();
endinterface

interface DN7406_FPGA_C_DDR2;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(11) leds();
endinterface

interface DN7406_FPGA_D_DDR2;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

interface DN7406_FPGA_F_DDR2;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

// SRAM variants

interface DN7406_FPGA_A_SRAM;
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
   method Bit#(3) leds();
endinterface

interface DN7406_FPGA_C_SRAM;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(11) leds();
endinterface

interface DN7406_FPGA_D_SRAM;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

interface DN7406_FPGA_F_SRAM;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

// FPGA Q

interface DN7406_FPGA_Q;
   interface Clock           refclk125;
   interface Reset           refrst125;
   interface LED#(4)         leds;
   interface PCIE_EXP#(8)    pcie;
   interface TrainingSignals training;
   interface FPGASource#(4)  source;
   interface FPGASink#(4)    sink;
   (* always_ready, always_enabled *)
   method    Action isDDRReady(Bool i);
   (* always_ready, always_enabled *)
   method    Action isOutOfReset(Bool i);
endinterface

endpackage: Dini7406
