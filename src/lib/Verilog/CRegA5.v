
// Copyright (c) 2014 Bluespec, Inc.

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


module CRegA5
         (CLK,
          RST,

	  // port 0 read
          Q_OUT_0,
	  // port 0 write
          EN_0, D_IN_0,

          // port 1 read
          Q_OUT_1,
	  // port 1 write
          EN_1, D_IN_1,

          // port 2 read
          Q_OUT_2,
	  // port 2 write
          EN_2, D_IN_2,

          // port 3 read
          Q_OUT_3,
	  // port 3 write
          EN_3, D_IN_3,

          // port 4 read
          Q_OUT_4,
	  // port 4 write
          EN_4, D_IN_4
         );

   parameter width = 1 ;
   parameter init  = { width {1'b0} } ;

   input  CLK ;
   input  RST ;

   output [width - 1 : 0]  Q_OUT_0 ;
   input                   EN_0 ;
   input  [width - 1 : 0]  D_IN_0 ;

   output [width - 1 : 0]  Q_OUT_1 ;
   input                   EN_1 ;
   input  [width - 1 : 0]  D_IN_1 ;

   output [width - 1 : 0]  Q_OUT_2 ;
   input                   EN_2 ;
   input  [width - 1 : 0]  D_IN_2 ;

   output [width - 1 : 0]  Q_OUT_3 ;
   input                   EN_3 ;
   input  [width - 1 : 0]  D_IN_3 ;

   output [width - 1 : 0]  Q_OUT_4 ;
   input                   EN_4 ;
   input  [width - 1 : 0]  D_IN_4 ;

   reg    [width - 1 : 0]  Q_OUT_0 ;
   wire   [width - 1 : 0]  Q_OUT_1 ;
   wire   [width - 1 : 0]  Q_OUT_2 ;
   wire   [width - 1 : 0]  Q_OUT_3 ;
   wire   [width - 1 : 0]  Q_OUT_4 ;
   wire   [width - 1 : 0]  Q_OUT_5 ;
   
   assign Q_OUT_1 = EN_0 ? D_IN_0 : Q_OUT_0 ;
   assign Q_OUT_2 = EN_1 ? D_IN_1 : Q_OUT_1 ;
   assign Q_OUT_3 = EN_2 ? D_IN_2 : Q_OUT_2 ;
   assign Q_OUT_4 = EN_3 ? D_IN_3 : Q_OUT_3 ;
   assign Q_OUT_5 = EN_4 ? D_IN_4 : Q_OUT_4 ;
   
   always@(posedge CLK or `BSV_RESET_EDGE RST)
     begin
        if (RST == `BSV_RESET_VALUE)
          Q_OUT_0 <= `BSV_ASSIGNMENT_DELAY init ;
        else
          Q_OUT_0 <= `BSV_ASSIGNMENT_DELAY Q_OUT_5 ;
     end

`ifdef BSV_NO_INITIAL_BLOCKS
`else // not BSV_NO_INITIAL_BLOCKS
   // synopsys translate_off
   initial begin
      Q_OUT_0 = {((width + 1)/2){2'b10}} ;
   end
   // synopsys translate_on
`endif // BSV_NO_INITIAL_BLOCKS

endmodule

