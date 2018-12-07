// Copyright (c) 2000-2013 Bluespec, Inc.

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

module SceMiInPipeProxyPut(CLK,
			   RST_N,

			   DATA,
			   DATA_EN,
			   DATA_RDY);
   parameter paramFile      = "";
   parameter transactorName = "";
   parameter portName       = "";
   parameter WIDTH          = 0;
   input  CLK;
   input  RST_N;

  // action method put
  input  [WIDTH - 1 : 0] DATA;
  input  DATA_EN;
  output DATA_RDY;

  // signals for module outputs
  wire DATA_RDY;

  // ports of submodule proxy
  wire [WIDTH - 1 : 0] proxy$DATA;
  wire proxy$DATA_EN, proxy$DATA_RDY;

  // rule scheduling signals
  wire CAN_FIRE_put, WILL_FIRE_put;

  // action method put
  assign DATA_RDY = proxy$DATA_RDY ;
  assign CAN_FIRE_put = proxy$DATA_RDY ;
  assign WILL_FIRE_put = DATA_EN ;

  // submodule proxy
  SceMiInPipeProxyF #(.WIDTH(WIDTH),
		      .paramFile(paramFile),
		      .transactorName(transactorName),
		      .portName(portName)) proxy(.CLK(CLK),
						 .RST_N(RST_N),
						 .DATA(proxy$DATA),
						 .DATA_EN(proxy$DATA_EN),
						 .ACCEPT(),
						 .DATA_RDY(proxy$DATA_RDY));

  // submodule proxy
  assign proxy$DATA = DATA ;
  assign proxy$DATA_EN = DATA_EN ;
endmodule

