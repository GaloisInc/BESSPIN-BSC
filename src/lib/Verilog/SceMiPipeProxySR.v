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

module SceMiPipeProxySR(CLK,
			RST_N,

			INPUTS,
			OUTPUTS);
   
   parameter paramFile      = "";
   parameter transactorName = "";
   parameter portNameIn     = "";
   parameter portNameOut    = "";
   parameter WIDTH_IN       = 0;
   parameter WIDTH_OUT      = 0;

   input  CLK;
   input  RST_N;

   input [WIDTH_IN - 1 : 0] INPUTS;
   output [WIDTH_OUT - 1 : 0] OUTPUTS;

   // start an interpreter if it doesn't already exist
   SimTbInterp interp(.CLK(CLK), .RST_N(RST_N));

   reg [31 : 0] 	      index_in;
   reg [31 : 0] 	      index_out;
   reg [32 : 0] 	      maybe_index_in;
   reg [32 : 0] 	      maybe_index_out;
   reg 			      initialized;
   reg 			      sent;
   reg [WIDTH_OUT - 1 : 0]    outputs_reg;

   assign OUTPUTS = outputs_reg;

   // synopsys translate_off
   initial
     begin
	maybe_index_in  = 0; // invalid
	maybe_index_out = 0; // invalid
	initialized     = 0;
	#1; // wait for interpreter to start
	maybe_index_in  = $imported_bsvscemi_bind_inpipe(paramFile, transactorName, portNameIn);
	maybe_index_out = $imported_bsvscemi_bind_outpipe(paramFile, transactorName, portNameOut);
	#0;
	if (!maybe_index_in[32])
	  if (transactorName == "")
	    $display("ERROR: Unable to bind pipe %s in module %m", portNameIn);
	  else
	    $display("ERROR: Unable to bind pipe %s/%s in module %m", transactorName, portNameIn);
	if (!maybe_index_out[32])
	   if (transactorName == "")
	    $display("ERROR: Unable to bind pipe %s in module %m", portNameOut);
	  else
	    $display("ERROR: Unable to bind pipe %s/%s in module %m", transactorName, portNameOut);
	if (!maybe_index_in[32] || !maybe_index_out[32])
	  begin
	     $finish(0);
	  end
	index_in  = maybe_index_in  [31:0];
	index_out = maybe_index_out [31:0];
	initialized     = 1;
	sent            = 0;
	outputs_reg     = 0;
     end // initial begin

   always@(posedge CLK)
     begin
	`BSV_ASSIGNMENT_DELAY #1;
	if (RST_N != `BSV_RESET_VALUE)
	  if (initialized)
	    begin
	       sent = 1;
//	       $display("(%0d) SENDING DATA", $time);
	       $imported_bsvscemi_inpipe_proxy_send_immediate(INPUTS,
							      32'd0,
							      index_in);
	    end
     end
   always@(negedge CLK)
     begin
	`BSV_ASSIGNMENT_DELAY #1;
	if (RST_N != `BSV_RESET_VALUE)
	  if (initialized && sent)
	    begin
//	       $display("(%0d) GETTING DATA", $time);
	       $imported_bsvscemi_outpipe_proxy_data_get_immediate(outputs_reg,
								   32'd0,
								   index_out);
	    end
     end
   // synopsys translate_on
endmodule

