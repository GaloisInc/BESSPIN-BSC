module ddr2_v10_1_0002_rom_bridge (
	write_clock,

	rom_address,
	rom_data,
	rom_rden,
	init,
	rom_data_ready,
	init_busy,

	avlm_address,
	avlm_writedata,
	avlm_wren,
	avlm_waitrequest
	);

parameter AVL_DATA_WIDTH = 32;
parameter AVL_ADDR_WIDTH = 12;

input          write_clock;

output   [AVL_ADDR_WIDTH-1:0] rom_address;
input   [AVL_DATA_WIDTH-1:0] rom_data;
output         rom_rden;
input          init;
input          rom_data_ready;
output         init_busy;

output  [AVL_ADDR_WIDTH-1:0] avlm_address;
output  [AVL_DATA_WIDTH-1:0] avlm_writedata;
output         avlm_wren;
input          avlm_waitrequest;


	assign avlm_wren = 1'b0;
	assign rom_address = 0;
	assign rom_rden = 0;
	assign init_busy = 0;
	assign avlm_address = 0;
	assign avlm_writedata = 0;

endmodule
