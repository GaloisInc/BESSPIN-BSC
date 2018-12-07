module ddr2_v10_1_0002_sequencer_bridge (
	// Avalon Interface
	
	avlf_clk,
	avlf_reset_n,
	avlf_address,
	avlf_write,
	avlf_writedata,
	avlf_read,
	avlf_readdata,
	avlf_waitrequest,

	// PHY control

	avlt_clk,
	avlt_reset_n,
	avlt_address,
	avlt_write,
	avlt_writedata,
	avlt_read,
	avlt_readdata,
	avlt_waitrequest
);

	parameter AVL_DATA_WIDTH = 32;
	parameter AVL_ADDR_WIDTH = 16;

	input avlf_clk;
	input avlf_reset_n;
	input [AVL_ADDR_WIDTH - 1:0] avlf_address;
	input avlf_write;
	input [AVL_DATA_WIDTH - 1:0] avlf_writedata;
	input avlf_read;
	output [AVL_DATA_WIDTH - 1:0] avlf_readdata;
	output avlf_waitrequest;

	output avlt_clk;
	output avlt_reset_n;
	output [AVL_ADDR_WIDTH - 1:0] avlt_address;
	output avlt_write;
	output [AVL_DATA_WIDTH - 1:0] avlt_writedata;
	output avlt_read;
	input [AVL_DATA_WIDTH - 1:0] avlt_readdata;
	input avlt_waitrequest;

	assign avlt_clk = avlf_clk;
	assign avlt_reset_n = avlf_reset_n;
	assign avlt_address = avlf_address;
	assign avlt_write = avlf_write;
	assign avlt_writedata = avlf_writedata;
	assign avlt_read = avlf_read;
	assign avlf_readdata = avlt_readdata;
	assign avlf_waitrequest = avlt_waitrequest;

endmodule
