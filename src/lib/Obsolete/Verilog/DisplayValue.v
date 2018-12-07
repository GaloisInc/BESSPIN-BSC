// Copyright 2000--2004 Bluespec, Inc.  All rights reserved.

// 
module DisplayValue(CLK, RST_N, valx, DISPX, vald, DISPD, FINI);
  parameter width = 1;
  input CLK;
  input RST_N;
  input DISPX;
  input DISPD;
  input FINI;
  input [width - 1 : 0] valx;
  input [width - 1 : 0] vald;

  // synopsys translate_off
  always@(posedge CLK) begin
    if (DISPX && RST_N)
      $display("0x%h", valx);
    if (DISPD && RST_N)
      $display("%d", vald);
    if (FINI && RST_N)
      $finish;
  end
  // synopsys translate_on
endmodule

