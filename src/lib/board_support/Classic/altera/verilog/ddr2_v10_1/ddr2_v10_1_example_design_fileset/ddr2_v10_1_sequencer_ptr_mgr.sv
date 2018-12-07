//USER ******
//USER ptr_mgr
//USER ******
//USER
//USER PTR Manager
//USER
//USER General Description
//USER -------------------
//USER
//USER This component allows a way for JTAG debug master to figure out where
//USER data transfer arrays are placed in memory. NIOS code will place the
//USER pointer to the array in a register, and the JTAG debug master picks it
//USER up from there.
//USER

module ddr2_v10_1_sequencer_ptr_mgr (
	//USER Avalon Interface
	
	avl_clk,
	avl_reset_n,
	avl_address,
	avl_write,
	avl_writedata,
	avl_read,
	avl_readdata,
	avl_waitrequest
);

	parameter AVL_DATA_WIDTH = 32;
	parameter AVL_ADDR_WIDTH = 16;

	input avl_clk;
	input avl_reset_n;
	input [AVL_ADDR_WIDTH - 1:0] avl_address;
	input avl_write;
	input [AVL_DATA_WIDTH - 1:0] avl_writedata;
	input avl_read;
	output [AVL_DATA_WIDTH - 1:0] avl_readdata;
	output avl_waitrequest;
	
	reg [AVL_DATA_WIDTH - 1:0] avl_readdata;
	reg avl_waitrequest;
	
	reg [AVL_DATA_WIDTH - 1:0] rfile_tcl_rx_io;
	reg [AVL_DATA_WIDTH - 1:0] rfile_tcl_tx_io;
	reg [AVL_DATA_WIDTH - 1:0] rfile_info_step;
	reg [AVL_DATA_WIDTH - 1:0] rfile_info_group;
	reg [AVL_DATA_WIDTH - 1:0] rfile_info_extra;

	//USER register selected
	
	wire sel_rfile, sel_rfile_wr, sel_rfile_rd;
	
	assign sel_rfile = 
		~avl_address[AVL_ADDR_WIDTH - 1] &
		~avl_address[AVL_ADDR_WIDTH - 2] &
		~avl_address[AVL_ADDR_WIDTH - 3];
	assign sel_rfile_wr = sel_rfile & avl_write;
	assign sel_rfile_rd = sel_rfile & avl_read;

	always_ff @ (posedge avl_clk) begin
		if (sel_rfile_wr) begin
			case (avl_address[2:0])
			3'b000: rfile_tcl_rx_io <= avl_writedata;
			3'b001: rfile_info_step <= avl_writedata;
			3'b010: rfile_info_group <= avl_writedata;
			3'b011: rfile_info_extra <= avl_writedata;
			3'b100: rfile_tcl_tx_io <= avl_writedata;
			endcase
		end
	end

	//USER wait request management and read data gating
	
	always_comb
	begin
		avl_waitrequest <= 0;
		
		if (sel_rfile_rd) 
			case (avl_address[2:0])
			3'b000: avl_readdata <= rfile_tcl_rx_io;
			3'b001: avl_readdata <= rfile_info_step;
			3'b010: avl_readdata <= rfile_info_group;
			3'b100: avl_readdata <= rfile_tcl_tx_io;
			default: avl_readdata <= rfile_info_extra;
			endcase
		else 
			avl_readdata <= '0;
	end
	
endmodule
