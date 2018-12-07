// Copyright (c) 2000-2009 Bluespec, Inc.

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// $Revision$
// $Date$

`ifdef BSV_ASSIGNMENT_DELAY
`else
 `define BSV_ASSIGNMENT_DELAY
`endif

module altpll_wrapper
  ( 
      CLKIN
    , RESET_N
    , LOCKED
    , CLK0
    , CLK1
    , CLK2
    , CLK3
    , CLK4
    , CLK5
    , CLK6
    , CLK7
    , CLK8
    , CLK9
    );

   parameter bandwidth_type          = "AUTO";
   parameter clk0_divide_by          = 1;
   parameter clk0_duty_cycle         = 50;
   parameter clk0_multiply_by        = 1;
   parameter clk0_phase_shift        = "0";
   parameter clk1_divide_by          = 1;  
   parameter clk1_duty_cycle         = 50; 
   parameter clk1_multiply_by        = 1;  
   parameter clk1_phase_shift        = "0";
   parameter clk2_divide_by          = 1;  
   parameter clk2_duty_cycle         = 50; 
   parameter clk2_multiply_by        = 1;  
   parameter clk2_phase_shift        = "0";
   parameter clk3_divide_by          = 1;  
   parameter clk3_duty_cycle         = 50; 
   parameter clk3_multiply_by        = 1;  
   parameter clk3_phase_shift        = "0";
   parameter clk4_divide_by          = 1;  
   parameter clk4_duty_cycle         = 50; 
   parameter clk4_multiply_by        = 1;  
   parameter clk4_phase_shift        = "0";
   parameter clk5_divide_by          = 1;  
   parameter clk5_duty_cycle         = 50; 
   parameter clk5_multiply_by        = 1;  
   parameter clk5_phase_shift        = "0";
   parameter clk6_divide_by          = 1;  
   parameter clk6_duty_cycle         = 50; 
   parameter clk6_multiply_by        = 1;  
   parameter clk6_phase_shift        = "0";
   parameter clk7_divide_by          = 1;  
   parameter clk7_duty_cycle         = 50; 
   parameter clk7_multiply_by        = 1;  
   parameter clk7_phase_shift        = "0";
   parameter clk8_divide_by          = 1;  
   parameter clk8_duty_cycle         = 50; 
   parameter clk8_multiply_by        = 1;  
   parameter clk8_phase_shift        = "0";
   parameter clk9_divide_by          = 1;  
   parameter clk9_duty_cycle         = 50; 
   parameter clk9_multiply_by        = 1;  
   parameter clk9_phase_shift        = "0";
   parameter compensate_clock        = "CLK0";
   parameter inclk0_input_frequency  = 10000;
   parameter intended_device_family  = "Stratix III";
   parameter lpm_type                = "altpll";
   parameter operation_mode          = "NORMAL";
   parameter pll_type                = "AUTO";
   parameter port_activeclock        = "PORT_UNUSED";
   parameter port_areset             = "PORT_USED";
   parameter port_clkbad0            = "PORT_UNUSED";
   parameter port_clkbad1            = "PORT_UNUSED";
   parameter port_clkloss            = "PORT_UNUSED";
   parameter port_clkswitch          = "PORT_UNUSED";
   parameter port_configupdate       = "PORT_UNUSED";
   parameter port_fbin               = "PORT_UNUSED";
   parameter port_fbout              = "PORT_UNUSED";
   parameter port_inclk0             = "PORT_USED";
   parameter port_inclk1             = "PORT_UNUSED";
   parameter port_locked             = "PORT_USED";
   parameter port_pfdena             = "PORT_UNUSED";
   parameter port_phasecounterselect = "PORT_UNUSED";
   parameter port_phasedone          = "PORT_UNUSED";
   parameter port_phasestep          = "PORT_UNUSED";
   parameter port_phaseupdown        = "PORT_UNUSED";
   parameter port_pllena             = "PORT_UNUSED";
   parameter port_scanaclr           = "PORT_UNUSED";
   parameter port_scanclk            = "PORT_UNUSED";
   parameter port_scanclkena         = "PORT_UNUSED";
   parameter port_scandata           = "PORT_UNUSED";
   parameter port_scandataout        = "PORT_UNUSED";
   parameter port_scandone           = "PORT_UNUSED";
   parameter port_scanread           = "PORT_UNUSED";
   parameter port_scanwrite          = "PORT_UNUSED";
   parameter port_clk0               = "PORT_USED";
   parameter port_clk1               = "PORT_USED";
   parameter port_clk2               = "PORT_USED";
   parameter port_clk3               = "PORT_USED";
   parameter port_clk4               = "PORT_USED";
   parameter port_clk5               = "PORT_USED";
   parameter port_clk6               = "PORT_USED";
   parameter port_clk7               = "PORT_USED";
   parameter port_clk8               = "PORT_USED";
   parameter port_clk9               = "PORT_USED";
   parameter port_clkena0            = "PORT_UNUSED";
   parameter port_clkena1            = "PORT_UNUSED";
   parameter port_clkena2            = "PORT_UNUSED";
   parameter port_clkena3            = "PORT_UNUSED";
   parameter port_clkena4            = "PORT_UNUSED";
   parameter port_clkena5            = "PORT_UNUSED";
   parameter self_reset_on_loss_lock = "OFF";
   parameter using_fbmimicbidir_port = "OFF";
   parameter width_clock             = 10;   
   
   input           CLKIN;
   input           RESET_N;
   
   output          LOCKED;
   output          CLK0;
   output          CLK1;
   output          CLK2;
   output          CLK3;
   output          CLK4;
   output          CLK5;
   output          CLK6;
   output          CLK7;
   output          CLK8;
   output          CLK9;

   wire            areset = !RESET_N;
   wire [9:0]      clks;

   assign 	   CLK9 = clks[9];
   assign 	   CLK8 = clks[8];
   assign 	   CLK7 = clks[7];
   assign 	   CLK6 = clks[6];
   assign 	   CLK5 = clks[5];
   assign 	   CLK4 = clks[4];
   assign 	   CLK3 = clks[3];
   assign 	   CLK2 = clks[2];
   assign 	   CLK1 = clks[1];
   assign 	   CLK0 = clks[0];
   
   altpll
     #(
      .bandwidth_type (bandwidth_type),
      .clk0_divide_by (clk0_divide_by),
      .clk0_duty_cycle (clk0_duty_cycle),
      .clk0_multiply_by (clk0_multiply_by),
      .clk0_phase_shift (clk0_phase_shift),
      .clk1_divide_by (clk1_divide_by),
      .clk1_duty_cycle (clk1_duty_cycle),
      .clk1_multiply_by (clk1_multiply_by),
      .clk1_phase_shift (clk1_phase_shift),
      .clk2_divide_by (clk2_divide_by),
      .clk2_duty_cycle (clk2_duty_cycle),
      .clk2_multiply_by (clk2_multiply_by),
      .clk2_phase_shift (clk2_phase_shift),
      .clk3_divide_by (clk3_divide_by),
      .clk3_duty_cycle (clk3_duty_cycle),
      .clk3_multiply_by (clk3_multiply_by),
      .clk3_phase_shift (clk3_phase_shift),
      .clk4_divide_by (clk4_divide_by),
      .clk4_duty_cycle (clk4_duty_cycle),
      .clk4_multiply_by (clk4_multiply_by),
      .clk4_phase_shift (clk4_phase_shift),
      .clk5_divide_by (clk5_divide_by),
      .clk5_duty_cycle (clk5_duty_cycle),
      .clk5_multiply_by (clk5_multiply_by),
      .clk5_phase_shift (clk5_phase_shift),
      .clk6_divide_by (clk6_divide_by),
      .clk6_duty_cycle (clk6_duty_cycle),
      .clk6_multiply_by (clk6_multiply_by),
      .clk6_phase_shift (clk6_phase_shift),
      .clk7_divide_by (clk7_divide_by),
      .clk7_duty_cycle (clk7_duty_cycle),
      .clk7_multiply_by (clk7_multiply_by),
      .clk7_phase_shift (clk7_phase_shift),
      .clk8_divide_by (clk8_divide_by),
      .clk8_duty_cycle (clk8_duty_cycle),
      .clk8_multiply_by (clk8_multiply_by),
      .clk8_phase_shift (clk8_phase_shift),
      .clk9_divide_by (clk9_divide_by),
      .clk9_duty_cycle (clk9_duty_cycle),
      .clk9_multiply_by (clk9_multiply_by),
      .clk9_phase_shift (clk9_phase_shift),
      .compensate_clock (compensate_clock),
      .inclk0_input_frequency (inclk0_input_frequency),
      .intended_device_family (intended_device_family),
      .lpm_type (lpm_type),
      .operation_mode (operation_mode),
      .pll_type (pll_type),
      .port_activeclock (port_activeclock),
      .port_areset (port_areset),
      .port_clkbad0 (port_clkbad0),
      .port_clkbad1 (port_clkbad1),
      .port_clkloss (port_clkloss),
      .port_clkswitch (port_clkswitch),
      .port_configupdate (port_configupdate),
      .port_fbin (port_fbin),
      .port_fbout (port_fbout),
      .port_inclk0 (port_inclk0),
      .port_inclk1 (port_inclk1),
      .port_locked (port_locked),
      .port_pfdena (port_pfdena),
      .port_phasecounterselect (port_phasecounterselect),
      .port_phasedone (port_phasedone),
      .port_phasestep (port_phasestep),
      .port_phaseupdown (port_phaseupdown),
      .port_pllena (port_pllena),
      .port_scanaclr (port_scanaclr),
      .port_scanclk (port_scanclk),
      .port_scanclkena (port_scanclkena),
      .port_scandata (port_scandata),
      .port_scandataout (port_scandataout),
      .port_scandone (port_scandone),
      .port_scanread (port_scanread),
      .port_scanwrite (port_scanwrite),
      .port_clk0 (port_clk0),
      .port_clk1 (port_clk1),
      .port_clk2 (port_clk2),
      .port_clk3 (port_clk3),
      .port_clk4 (port_clk4),
      .port_clk5 (port_clk5),
      .port_clk6 (port_clk6),
      .port_clk7 (port_clk7),
      .port_clk8 (port_clk8),
      .port_clk9 (port_clk9),
      .port_clkena0 (port_clkena0),
      .port_clkena1 (port_clkena1),
      .port_clkena2 (port_clkena2),
      .port_clkena3 (port_clkena3),
      .port_clkena4 (port_clkena4),
      .port_clkena5 (port_clkena5),
      .self_reset_on_loss_lock (self_reset_on_loss_lock),
      .using_fbmimicbidir_port (using_fbmimicbidir_port),
      .width_clock (width_clock)
      ) pll
   (
    .inclk              ({ 1'b0, CLKIN }),
    .areset             (areset),
    .clk                (clks),
    .locked             (LOCKED),
    .activeclock        (),
    .clkbad             (),
    .clkena             ({6{1'b1}}),
    .clkloss            (),
    .clkswitch          (1'b0),
    .configupdate       (1'b0),
    .enable0            (),
    .enable1            (),
    .extclk             (),
    .extclkena          ({4{1'b1}}),
    .fbin               (1'b1),
    .fbmimicbidir       (),
    .fbout              (),
    .pfdena             (1'b1),
    .phasecounterselect ({4{1'b1}}),
    .phasedone          (),
    .phasestep          (1'b1),
    .phaseupdown        (1'b1),
    .pllena             (1'b1),
    .scanaclr           (1'b0),
    .scanclk            (1'b0),
    .scanclkena         (1'b1),
    .scandata           (1'b0),
    .scandataout        (),
    .scandone           (),
    .scanread           (1'b0),
    .scanwrite          (1'b0),
    .sclkout0           (),
    .sclkout1           (),
    .vcooverrange       (),
    .vcounderrange      ()    
    );
   
endmodule // altpll_wrapper
