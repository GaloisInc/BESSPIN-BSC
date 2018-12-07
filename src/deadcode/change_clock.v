//
//            __    __    __    __    __    __    __    __    __    __    __    __    __    __
//         __|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  
// 
//            ___     ___     ___     ___     ___     ___     ___     ___     ___     ___
//         __|   |___|   |___|   |___|   |___|   |___|   |___|   |___|   |___|   |___|  
//
//     
// in_strobe  _______                 _______ 
//         __|       |_______________|       |______________________________________
//         __________                 _______     
// in_ok             |_______________|       |___________________________
//            _______________________         ____________________
// in_valid__|                       |_______|
//                        ___________________________
// in_stateF  ___________|                          |_______________
//                        _______                _________
// out_valid_____________|       |______________|
//                        ____________________________________________________________________
// out_ok   _____________|
//                        _______                ________           
// out_strobe____________|       |______________|        |_______
//                                    _______________________
// out_stateS  ______________________|                       |________________________
//
//

module clocked_wire( rstn, clk, in ,out );
  parameter size = 1;

  input             rstn;
  input             clk;
  input  [size-1:0] in;
  output [size-1:0] out;

  reg   [size-1:0] inC;
  reg   [size-1:0] inCC;

  assign out = inCC;

  always @ (posedge clk)
  begin
    if (!rstn) begin

      inC <= 0;
      inCC <= 0;

    end else begin

      inC  <= in;
      inCC <= inC;

    end
  end

endmodule
 



module change_clock( 
		in_rstn, out_rstn,
		in_clk, out_clk, 
		in_data, in_strobe, in_ready, 
		out_data, out_strobe, out_ready );

  parameter size = 32;

  input in_rstn;
  input in_clk;
  input out_rstn;
  input out_clk;

  input  [size-1:0] in_data;
  output            in_ready;
  input             in_strobe;

  output [size-1:0] out_data;
  output            out_ready;
  input             out_strobe;

  reg               in_state;
  reg               in_stateD;   // in_state delayed 1/2 clock cycle to compensate for skew
  wire              out_stateS;
  reg               out_stateS2;
  reg               in_valid;
  reg    [size-1:0] in_latch;

  reg               out_state;
  wire              in_stateF;
  reg               in_stateF2;
  wire   [size-1:0] in_latchF;
  reg               out_valid;
  reg    [size-1:0] out_latch;
  reg               change;     // Latch to remember if in_state have changed


  clocked_wire        out_change_wire(.rstn(in_rstn),.clk(in_clk),.in(out_state),.out(out_stateS));
  clocked_wire        in_change_wire( .rstn(out_rstn),.clk(out_clk),.in(in_stateD), .out(in_stateF));
  clocked_wire#(size) data_wire(      .rstn(out_rstn),.clk(out_clk),.in(in_latch), .out(in_latchF));

  reg  in_synced;

  assign out_data = out_latch;
  assign in_ready = in_synced & ~in_valid;

  always @ (negedge in_clk)
  begin
    if (!in_rstn) begin

      in_stateD <= 0;

    end else begin

      in_stateD <= in_state;

    end
  end



  always @ (posedge in_clk)
  begin
    if (!in_rstn) begin

      out_stateS2 <= 0;

      in_state <= 0;
      in_valid <= 0;
      in_synced <= 0;
      in_latch <= 0;

    end else begin

      out_stateS2 <= out_stateS;

      if ( !in_synced ) begin

        in_state <= 1;
	in_synced <= out_stateS != out_stateS2;

      end else begin

	if ( !in_valid && in_strobe ) begin
	  in_valid <= 1;
	  in_latch <= in_data;
	  in_state <= ~in_state;
	end else begin
	  if ( in_valid && (out_stateS != out_stateS2) ) begin
	    in_valid <= 0;
	  end
	end	
      end
    end
  end


  reg    out_synced;
  assign out_ready = out_synced && out_valid;

  always @ (posedge out_clk)
  begin

    if (!out_rstn) begin

      in_stateF2 <= 0;

      out_state <= 0;
      out_valid <= 0;
      out_synced <= 0;
      out_latch <= 0;
      change <= 0;

    end else begin

      in_stateF2 <= in_stateF;

      if ( !out_synced ) begin

        out_state <= 1;
	out_synced <= in_stateF != in_stateF2;

      end else begin

	if ( !out_valid && (change || (in_stateF != in_stateF2)) ) begin
	  out_valid <= 1;
	  out_latch <= in_latchF;
	  out_state <= ~out_state;
	  change <= 0;
	end else begin
	  if ( out_valid && out_strobe ) begin
	    if (change || (in_stateF != in_stateF2)) begin
	      out_latch <= in_latchF;
	      out_state <= ~out_state;
	      change <= 0;
	    end else begin 
	      change <= change || in_stateF != in_stateF2;
	      out_valid <= 0;
	    end
	  end else begin
	    change <= change || in_stateF != in_stateF2;
	  end
	end	
      end
    end
  end


endmodule


