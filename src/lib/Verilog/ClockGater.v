
// Copyright (c) 2000-2012 Bluespec, Inc.

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
// $Revision: 29441 $
// $Date: 2012-08-27 17:58:03 -0400 (Mon, 27 Aug 2012) $

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif


// Bluespec primitive module which gates a clock
// To avoid glitches, CLK_GATE_OUT only changes  when CLK_IN is low.
// CLK_GATE_OUT follows CLK_GATE_IN in the same cycle, but COND is first
// registered, thus delaying the gate condition by one cycle.
// In this model, the oscillator CLK_OUT does *not* stop when the CLK_GATE_IN or
// COND are deasserted.
module ClockGater(
		  // ports for the internal register
		  CLK,
		  RST,
                  COND,
		  // ports for the output clock
                  CLK_OUT,
                  CLK_GATE_OUT );

   parameter init = 1 ;

   input  CLK ;
   input  RST ;
   input  COND ;
   output CLK_OUT ;
   output CLK_GATE_OUT ;

   // BUFG buf_gC(.I(CLK), .O(CLK_OUT));
   assign CLK_OUT = CLK;
   //BUFG buf_gG(.I(COND),   .O(CLK_GATE_OUT));
   assign CLK_GATE_OUT = COND;
endmodule // GatedClock
