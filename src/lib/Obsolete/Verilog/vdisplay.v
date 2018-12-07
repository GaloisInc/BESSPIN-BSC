
module vdisplay(CLK, RST_N, valx, DISPX, vald, DISPD, DISPC, FINI);
  parameter width = 1;
  input CLK;
  input RST_N;
  input DISPX;
  input DISPD;
  input DISPC;
  input FINI;
  input [width - 1 : 0] valx;
  input [width - 1 : 0] vald;
  reg [31:0] count;
   
  always@(posedge CLK) begin
    if (DISPX && RST_N)
      $display("0x%x", valx);
    if (DISPD && RST_N)
      $display("%0d", vald);
    if (DISPC && RST_N)
      $display("%0d", count);
    if (FINI && RST_N)
      $finish(0);
    count <= RST_N ? count+1 : 1;
  end
endmodule

