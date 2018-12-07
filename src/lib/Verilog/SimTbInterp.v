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

module SimTbInterp(CLK,
		   RST_N);
  parameter PORT = 0;
  input  CLK;
  input  RST_N;

  // register started
  reg started;


  // synopsys translate_off
  integer port;
  initial
    begin
       started = 1'h0;
       if (PORT == 0)
	 begin
	    if (!$value$plusargs("tcl_port=%d", port))  
	      port = 1234;
	 end
       else
	 port = PORT;
       started = $imported_bsvsimtb_interp_start(port);
    end

   always@(posedge CLK or negedge CLK)
     begin
	if (!started)
	  begin
	     $display("ERROR: Unable to start interpreter in module %m");
	     $finish(32'd0);
	  end
     end
  // synopsys translate_on
endmodule

