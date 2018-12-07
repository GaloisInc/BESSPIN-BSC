//USER ******
//USER scc_mgr
//USER ******
//USER
//USER SCC Manager
//USER
//USER General Description
//USER -------------------
//USER
//USER This component allows the NIOS to control the delay chains in the IOs.
//USER

// altera message_off 10230
module ddr2_v10_1_sequencer_scc_mgr (
	//USER Avalon Interface
	
	avl_clk,
	avl_reset_n,
	avl_address,
	avl_write,
	avl_writedata,
	avl_read,
	avl_readdata,
	avl_waitrequest,

	reset_n_scc_clk,	
	scc_clk,
	scc_data,
	scc_dqs_ena,
	scc_dqs_io_ena,
	scc_dq_ena,
	scc_dm_ena,
	scc_upd
);

	parameter AVL_DATA_WIDTH		= 32;
	parameter AVL_ADDR_WIDTH		= 16;

	parameter MEM_DQS_WIDTH			= "";
	parameter MEM_DQ_WIDTH			= "";
	parameter MEM_DM_WIDTH			= "";

	parameter DLL_DELAY_CHAIN_LENGTH = "";
	parameter DELAY_PER_OPA_TAP   = "";
	parameter DELAY_PER_DCHAIN_TAP   = "";

	localparam DQS_IN_PHASE_MAX		= 3;
	localparam DQS_IN_DELAY_MAX		= 15;
	localparam DQS_EN_DELAY_MAX		= 7;
	localparam IO_OUT1_DELAY_MAX	= 15;
	localparam IO_OUT2_DELAY_MAX	= 7;
	localparam IO_IN_DELAY_MAX		= 15;

	localparam DQS_SDATA_BITS		= 46;
	localparam IO_SDATA_BITS		= 11;
	
	input avl_clk;
	input avl_reset_n;
	input [AVL_ADDR_WIDTH - 1:0] avl_address;
	input avl_write;
	input [AVL_DATA_WIDTH - 1:0] avl_writedata;
	input avl_read;
	output [AVL_DATA_WIDTH - 1:0] avl_readdata;
	output avl_waitrequest;
	
	input scc_clk;
	input reset_n_scc_clk;
	output scc_data;
	output [MEM_DQS_WIDTH - 1:0] scc_dqs_ena;
	output [MEM_DQS_WIDTH - 1:0] scc_dqs_io_ena;
	output [MEM_DQ_WIDTH - 1:0] scc_dq_ena;
	output [MEM_DM_WIDTH - 1:0] scc_dm_ena;
	output scc_upd;

	//USER internal versions of interfacing signals.
	
	reg [AVL_DATA_WIDTH - 1:0] avl_readdata;
	reg avl_waitrequest;

	reg scc_data;
	reg [MEM_DQS_WIDTH - 1:0] scc_dqs_ena;
	reg [MEM_DQS_WIDTH - 1:0] scc_dqs_io_ena;
	reg [MEM_DQ_WIDTH - 1:0] scc_dq_ena;
	reg [MEM_DM_WIDTH - 1:0] scc_dm_ena;
	reg scc_upd;

	reg scc_data_c;
	reg [MEM_DQS_WIDTH - 1:0] scc_dqs_ena_c;
	reg [MEM_DQS_WIDTH - 1:0] scc_dqs_io_ena_c;
	reg [MEM_DQ_WIDTH - 1:0] scc_dq_ena_c;
	reg [MEM_DM_WIDTH - 1:0] scc_dm_ena_c;
	reg scc_upd_c;	

	//USER IO config register
	
	reg [IO_SDATA_BITS - 1:0] avl_io_cfg;
	reg [IO_SDATA_BITS - 1:0] scc_io_cfg_curr;
	reg [IO_SDATA_BITS - 1:0] scc_io_cfg_next;
	
	//USER DQS config register
	
	reg [DQS_SDATA_BITS - 1:0] avl_dqs_cfg;
	reg [DQS_SDATA_BITS - 1:0] scc_dqs_cfg_curr;
	reg [DQS_SDATA_BITS - 1:0] scc_dqs_cfg_next;
	
	//USER is scc manager selected?
	
	reg sel_scc;
	
	//USER parameter addresses

	typedef enum bit [3:0] {
		SCC_ADDR_DQS_IN_DELAY	= 4'b0001,
		SCC_ADDR_DQS_EN_PHASE	= 4'b0010,
		SCC_ADDR_DQS_EN_DELAY	= 4'b0011,
		SCC_ADDR_DQDQS_OUT_PHASE= 4'b0100,
		SCC_ADDR_OCT_OUT1_DELAY	= 4'b0110,
		SCC_ADDR_OCT_OUT2_DELAY	= 4'b0111,
		SCC_ADDR_IO_OUT1_DELAY	= 4'b1000,
		SCC_ADDR_IO_OUT2_DELAY	= 4'b1001,
		SCC_ADDR_IO_IN_DELAY	= 4'b1010
	} sdata_addr_t;

	//USER DQS scan data access
	
	reg sel_sdata, sel_sdata_wr;
	
	//USER parameter addresses

	typedef enum bit [3:0] {
		SCC_PAR_DQS_IN_DELAY_MAX		= 4'b0001,
		SCC_PAR_DQS_EN_PHASE_MAX		= 4'b0010,
		SCC_PAR_DQS_EN_DELAY_MAX		= 4'b0011,
		SCC_PAR_DQDQS_OUT_PHASE_MAX		= 4'b0100,
		SCC_PAR_IO_OUT1_DELAY_MAX		= 4'b0110,
		SCC_PAR_IO_OUT2_DELAY_MAX		= 4'b0111,
		SCC_PAR_IO_IN_DELAY_MAX			= 4'b1000,
		SCC_PAR_DLL_CHAIN_LENGTH		= 4'b1001,
		SCC_PAR_DELAY_PER_OPA_TAP		= 4'b1010,
		SCC_PAR_DQS_IN_RESERVE			= 4'b1011,
		SCC_PAR_DQS_OUT_RESERVE			= 4'b1100,
		SCC_PAR_DELAY_PER_DCHAIN_TAP 	= 4'b1101,
		SCC_PAR_DQ_OUT_RESERVE			= 4'b1110,
		SCC_PAR_DM_OUT_RESERVE			= 4'b1111
	} scc_par_t;

	//USER parameter reading
	
	reg sel_param_rd;

	//USER go signal going to the SCC clock side.

	reg avl_go_dqs_ena;
	reg avl_go_dqs_io_ena;
	reg avl_go_dq_ena;
	reg avl_go_dm_ena;
	reg avl_go_upd;

	//USER go signal coming from AVL side.

	reg scc_go_dqs_ena;
	reg scc_go_dqs_io_ena;
	reg scc_go_dq_ena;
	reg scc_go_dm_ena;
	reg scc_go_upd;
	
	//USER enable pattern.
	
	reg [7:0] scc_ena_addr;
	reg [255:0] scc_ena_addr_decode;

	//USER done signal coming back from the scc side.
	
	reg scc_done;
	
	//USER avalon version of scc done signal
	
	reg avl_done;

	//USER SCAN state machine

	typedef enum int unsigned {
		STATE_SCC_IDLE,
		STATE_SCC_LOAD,
		STATE_SCC_DONE
	} STATE_SCC_RAM_T;
	
	STATE_SCC_RAM_T scc_state_curr, scc_state_next;
	reg [7:0] scc_shift_cnt_curr;
	reg [7:0] scc_shift_cnt_next;
	
	//USER phase decoding.

	reg [2:0] dqsi_phase;
	
	reg [6:0] dqs_phase_reset;
	reg [6:0] dqs_phase;
	
	reg [6:0] dq_phase_reset;
	reg [6:0] dq_phase;
	
	reg [5:0] dqse_phase_reset;
	reg [5:0] dqse_phase;

	//USER Avalon bus interfacing
	
	always_comb begin
		sel_scc = 
			~avl_address[AVL_ADDR_WIDTH - 1] &
			avl_address[AVL_ADDR_WIDTH - 2] &
			avl_address[AVL_ADDR_WIDTH - 3];
		
		sel_sdata_wr = 0;
		avl_go_dqs_ena = 0;
		avl_go_dqs_io_ena = 0;
		avl_go_dq_ena = 0;
		avl_go_dm_ena = 0;
		avl_go_upd = 0;
		
		if (sel_scc & avl_write) begin
			case (avl_address[AVL_ADDR_WIDTH - 4:AVL_ADDR_WIDTH - 6])
			3'b000: sel_sdata_wr = 1;
			3'b001: avl_go_dqs_ena = 1;
			3'b010: avl_go_dqs_io_ena = 1;
			3'b011: avl_go_dq_ena = 1;
			3'b100: avl_go_dm_ena = 1;
			3'b101: avl_go_upd = 1;	
			default : begin end
			endcase
		end

		sel_param_rd = sel_scc & avl_read;

		avl_waitrequest = 
			(avl_go_dqs_ena |
			 avl_go_dqs_io_ena |
			 avl_go_dq_ena |
			 avl_go_dm_ena |
			 avl_go_upd) & ~avl_done;

		avl_readdata = '0;

		if (sel_param_rd) begin
			case (avl_address[3:0])
			SCC_PAR_DQS_IN_DELAY_MAX: avl_readdata = DQS_IN_DELAY_MAX;
			SCC_PAR_DQS_EN_PHASE_MAX: begin
				case (DLL_DELAY_CHAIN_LENGTH)
				6: avl_readdata = 5;
				8: avl_readdata = 7;
				10: avl_readdata = 9;
				12: avl_readdata = 11;
				default : begin end
				endcase
			end
			SCC_PAR_DQS_EN_DELAY_MAX: avl_readdata = DQS_EN_DELAY_MAX;
			SCC_PAR_DQDQS_OUT_PHASE_MAX: begin
				case (DLL_DELAY_CHAIN_LENGTH)
				6: avl_readdata = 10;
				8: avl_readdata = 14;
				10: avl_readdata = 17;
				12: avl_readdata = 21;
				default : begin end
				endcase
			end
			SCC_PAR_IO_OUT1_DELAY_MAX: avl_readdata = IO_OUT1_DELAY_MAX;
			SCC_PAR_IO_OUT2_DELAY_MAX: avl_readdata = IO_OUT2_DELAY_MAX;
			SCC_PAR_IO_IN_DELAY_MAX: avl_readdata = IO_IN_DELAY_MAX;
			SCC_PAR_DLL_CHAIN_LENGTH: avl_readdata = DLL_DELAY_CHAIN_LENGTH;
			SCC_PAR_DELAY_PER_OPA_TAP: avl_readdata = DELAY_PER_OPA_TAP;
			SCC_PAR_DELAY_PER_DCHAIN_TAP: avl_readdata = DELAY_PER_DCHAIN_TAP;
			SCC_PAR_DQS_IN_RESERVE: begin
				case (DLL_DELAY_CHAIN_LENGTH)
				6: avl_readdata = 6;
				8: avl_readdata = 3;
				10: avl_readdata = 3;
				12: avl_readdata = 6;
				default : begin end
				endcase
			end
			SCC_PAR_DQS_OUT_RESERVE: begin
				case (DLL_DELAY_CHAIN_LENGTH)
				6: avl_readdata = 5;
				8: avl_readdata = 3;
				10: avl_readdata = 3;
				12: avl_readdata = 6;
				default : begin end
				endcase
			end
			SCC_PAR_DQ_OUT_RESERVE: begin
				case (DLL_DELAY_CHAIN_LENGTH)
				6: avl_readdata = 0;
				8: avl_readdata = 0;
				10: avl_readdata = 0;
				12: avl_readdata = 0;
				default : begin end
				endcase
			end
			SCC_PAR_DM_OUT_RESERVE: begin
				case (DLL_DELAY_CHAIN_LENGTH)
				6: avl_readdata = 0;
				8: avl_readdata = 0;
				10: avl_readdata = 0;
				12: avl_readdata = 0;
				default : begin end
				endcase
			end
			default : begin end
			endcase
		end
	end

	//USER decode phases
	
	always_comb begin
		dqsi_phase = 0;
		
		dqs_phase_reset = 0;
		dqs_phase = 0;
		
		dq_phase_reset = 0;
		dq_phase = 0;
		
		dqse_phase_reset = 0;
		dqse_phase = 0;

		case (DLL_DELAY_CHAIN_LENGTH)
		6: begin
			//USER DQSin = 60, DQS = 180, DQ = 120, DQSE = 120
			dqsi_phase = 3'b000;
			dqs_phase  = 7'b0010100;
			dq_phase   = 7'b0001100;
			dqse_phase = 6'b001000;

			//USER DQS = 480, DQ = 420, DQSE = 240
			dqs_phase_reset = 7'b0100001;
			dq_phase_reset = 7'b0011010;
			dqse_phase_reset = 6'b000110;
			
			case (avl_writedata[4:0])
			5'b00000: //USER DQS = 180, DQ = 120, DQSE = 120
				begin
					dqs_phase  = 7'b0010100;
					dq_phase   = 7'b0001100;
					dqse_phase = 6'b001000;
				end
			5'b00001: //USER DQS = 240, DQ = 180, DQSE = 180
				begin
					dqs_phase  = 7'b0011100;
					dq_phase   = 7'b0010100;
					dqse_phase = 6'b001100;
				end
			5'b00010: //USER DQS = 300, DQ = 240, DQSE = 240
				begin
					dqs_phase  = 7'b0100110;
					dq_phase   = 7'b0011100;
					dqse_phase = 6'b000110;
				end
			5'b00011: //USER DQS = 360, DQ = 300, DQSE = 300
				begin
					dqs_phase  = 7'b0010010;
					dq_phase   = 7'b0001010;
					dqse_phase = 6'b001011;
				end
			5'b00100: //USER DQS = 420, DQ = 360, DQSE = 360
				begin
					dqs_phase  = 7'b0011010;
					dq_phase   = 7'b0010010;
					dqse_phase = 6'b001111;
				end
			5'b00101: //USER DQS = 480, DQ = 420, DQSE = 420
				begin
					dqs_phase  = 7'b0100001;
					dq_phase   = 7'b0011010;
					dqse_phase = 6'b000101;
				end
			5'b00110: //USER DQS = 540, DQ = 480
				begin
					dqs_phase  = 7'b0010101;
					dq_phase   = 7'b0001101;
				end
			5'b00111: //USER DQS = 600, DQ = 540
				begin
					dqs_phase  = 7'b0011101;
					dq_phase   = 7'b0010101;
				end
			5'b01000: //USER DQS = 660, DQ = 600
				begin
					dqs_phase  = 7'b0100111;
					dq_phase   = 7'b0011101;
				end
			5'b01001: //USER DQS = 720, DQ = 660
				begin
					dqs_phase  = 7'b0010011;
					dq_phase   = 7'b0001011;
				end
			5'b01010: //USER DQS = 780, DQ = 720
				begin
					dqs_phase  = 7'b0011011;
					dq_phase   = 7'b0010011;
				end
			default : begin end
			endcase
		end
		8: begin
			//USER DQSin = 90, DQS = 180, DQ = 90, DQSE = 90
			
			dqsi_phase = 3'b001;
			dqs_phase  = 7'b0010100;
			dq_phase   = 7'b0000100;
			dqse_phase = 6'b001000;

			//USER DQS = 540, DQ = 450, DQSE = 270
			dqs_phase_reset = 7'b0010101;
			dq_phase_reset = 7'b0000110;
			dqse_phase_reset = 6'b001010;

			case (avl_writedata[4:0])
			5'b00000: //USER DQS = 180, DQ = 90, DQSE = 90
				begin
					dqs_phase  = 7'b0010100;
					dq_phase   = 7'b0000100;
					dqse_phase = 6'b001000;
				end
			5'b00001: //USER DQS = 225, DQ = 135, DQSE = 135
				begin
					dqs_phase  = 7'b0011100;
					dq_phase   = 7'b0001100;
					dqse_phase = 6'b001100;
				end
			5'b00010: //USER DQS = 270, DQ = 180, DQSE = 180
				begin
					dqs_phase  = 7'b0100100;
					dq_phase   = 7'b0010100;
					dqse_phase = 6'b010000;
				end
			5'b00011: //USER DQS = 315, DQ = 225, DQSE = 225
				begin
					dqs_phase  = 7'b0101110;
					dq_phase   = 7'b0011100;
					dqse_phase = 6'b000110;
				end
			5'b00100: //USER DQS = 360, DQ = 270, DQSE = 270
				begin
					dqs_phase  = 7'b0010010;
					dq_phase   = 7'b0000000;
					dqse_phase = 6'b001010;
				end
			5'b00101: //USER DQS = 405, DQ = 315, DQSE = 315
				begin
					dqs_phase  = 7'b0011010;
					dq_phase   = 7'b0001010;
					dqse_phase = 6'b001111;
				end
			5'b00110: //USER DQS = 450, DQ = 360, DQSE = 360
				begin
					dqs_phase  = 7'b0100010;
					dq_phase   = 7'b0010010;
					dqse_phase = 6'b010011;
				end
			5'b00111: //USER DQS = 495, DQ = 405, DQSE = 405
				begin
					dqs_phase  = 7'b0101001;
					dq_phase   = 7'b0011010;
					dqse_phase = 6'b000101;
				end
			5'b01000: //USER DQS = 540, DQ = 450
				begin
					dqs_phase  = 7'b0010101;
					dq_phase   = 7'b0000110;
				end
			5'b01001: //USER DQS = 585, DQ = 495
				begin
					dqs_phase  = 7'b0011101;
					dq_phase   = 7'b0001101;
				end
			5'b01010: //USER DQS = 630, DQ = 540
				begin
					dqs_phase  = 7'b0100101;
					dq_phase   = 7'b0010101;
				end
			5'b01011: //USER DQS = 675, DQ = 585
				begin
					dqs_phase  = 7'b0101111;
					dq_phase   = 7'b0011101;
				end
			5'b01100: //USER DQS = 720, DQ = 630
				begin
					dqs_phase  = 7'b0010011;
					dq_phase   = 7'b0000001;
				end
			5'b01101: //USER DQS = 765, DQ = 675
				begin
					dqs_phase  = 7'b0011011;
					dq_phase   = 7'b0001011;
				end
			5'b01110: //USER DQS = 810, DQ = 720
				begin
					dqs_phase  = 7'b0100011;
					dq_phase   = 7'b0010011;
				end
			default : begin end
			endcase
		end
		10: begin
			//USER DQSin = 72, DQS = 180, DQ = 108, DQSE = 108
			dqsi_phase = 3'b001;
			dqs_phase  = 7'b0010100;
			dq_phase   = 7'b0000100;
			dqse_phase = 6'b001100;

			//USER DQS = 540, DQ = 468, DQSE = 252
			dqs_phase_reset = 7'b0010101;
			dq_phase_reset = 7'b0000101;
			dqse_phase_reset = 6'b001010;

			case (avl_writedata[4:0])
			5'b00000: //USER DQS = 180, DQ = 108, DQSE = 108
				begin
					dqs_phase  = 7'b0010100;
					dq_phase   = 7'b0000100;
					dqse_phase = 6'b001100;
				end
			5'b00001: //USER DQS = 216, DQ = 144, DQSE = 144
				begin
					dqs_phase  = 7'b0011100;
					dq_phase   = 7'b0001100;
					dqse_phase = 6'b010000;
				end
			5'b00010: //USER DQS = 252, DQ = 180, DQSE = 180
				begin
					dqs_phase  = 7'b0100100;
					dq_phase   = 7'b0010100;
					dqse_phase = 6'b010100;
				end
			5'b00011: //USER DQS = 288, DQ = 216, DQSE = 216
				begin
					dqs_phase  = 7'b0101110;
					dq_phase   = 7'b0011100;
					dqse_phase = 6'b000110;
				end
			5'b00100: //USER DQS = 324, DQ = 252, DQSE = 252
				begin
					dqs_phase  = 7'b0110110;
					dq_phase   = 7'b0100100;
					dqse_phase = 6'b001010;
				end
			5'b00101: //USER DQS = 360, DQ = 288, DQSE = 288
				begin
					dqs_phase  = 7'b0010010;
					dq_phase   = 7'b0000010;
					dqse_phase = 6'b001111;
				end
			5'b00110: //USER DQS = 396, DQ = 324, DQSE = 324
				begin
					dqs_phase  = 7'b0011010;
					dq_phase   = 7'b0001010;
					dqse_phase = 6'b010011;
				end
			5'b00111: //USER DQS = 432, DQ = 360, DQSE = 360
				begin
					dqs_phase  = 7'b0100010;
					dq_phase   = 7'b0010010;
					dqse_phase = 6'b010111;
				end
			5'b01000: //USER DQS = 468, DQ = 396, DQSE = 396
				begin
					dqs_phase  = 7'b0101001;
					dq_phase   = 7'b0011010;
					dqse_phase = 6'b000101;
				end
			5'b01001: //USER DQS = 504, DQ = 432, DQSE = 432
				begin
					dqs_phase  = 7'b0110001;
					dq_phase   = 7'b0100010;
					dqse_phase = 6'b001001;
				end
			5'b01010: //USER DQS = 540, DQ = 468
				begin
					dqs_phase  = 7'b0010101;
					dq_phase   = 7'b0000101;
				end
			5'b01011: //USER DQS = 576, DQ = 504
				begin
					dqs_phase  = 7'b0011101;
					dq_phase   = 7'b0001101;
				end
			5'b01100: //USER DQS = 612, DQ = 540
				begin
					dqs_phase  = 7'b0100101;
					dq_phase   = 7'b0010101;
				end
			5'b01101: //USER DQS = 648, DQ = 576
				begin
					dqs_phase  = 7'b0101111;
					dq_phase   = 7'b0011101;
				end
			5'b01110: //USER DQS = 684, DQ = 612
				begin
					dqs_phase  = 7'b0110111;
					dq_phase   = 7'b0100101;
				end
			5'b01111: //USER DQS = 720, DQ = 648
				begin
					dqs_phase  = 7'b0010011;
					dq_phase   = 7'b0000011;
				end
			5'b10000: //USER DQS = 756, DQ = 684
				begin
					dqs_phase  = 7'b0011011;
					dq_phase   = 7'b0001011;
				end
			5'b10001: //USER DQS = 792, DQ = 720
				begin
					dqs_phase  = 7'b0100011;
					dq_phase   = 7'b0010011;
				end
			default : begin end
			endcase
		end
		12: begin
			//USER DQSin = 60, DQS = 180, DQ = 120, DQSE = 90
			dqsi_phase = 3'b001;
			dqs_phase  = 7'b0010100;
			dq_phase   = 7'b0000100;
			dqse_phase = 6'b001100;

			//USER DQS = 540, DQ = 480, DQSE = 270
			dqs_phase_reset = 7'b0010101;
			dq_phase_reset = 7'b0000101;
			dqse_phase_reset = 6'b001110;

			case (avl_writedata[4:0])
			5'b00000: //USER DQS = 180, DQ = 120, DQSE = 90
				begin
					dqs_phase  = 7'b0010100;
					dq_phase   = 7'b0000100;
					dqse_phase = 6'b001100;
				end
			5'b00001: //USER DQS = 210, DQ = 150, DQSE = 120
				begin
					dqs_phase  = 7'b0011100;
					dq_phase   = 7'b0001100;
					dqse_phase = 6'b010000;
				end
			5'b00010: //USER DQS = 240, DQ = 180, DQSE = 150
				begin
					dqs_phase  = 7'b0100100;
					dq_phase   = 7'b0010100;
					dqse_phase = 6'b010100;
				end
			5'b00011: //USER DQS = 270, DQ = 210, DQSE = 180
				begin
					dqs_phase  = 7'b0101100;
					dq_phase   = 7'b0011100;
					dqse_phase = 6'b011000;
				end
			5'b00100: //USER DQS = 300, DQ = 240, DQSE = 210
				begin
					dqs_phase  = 7'b0110110;
					dq_phase   = 7'b0100100;
					dqse_phase = 6'b000110;
				end
			5'b00101: //USER DQS = 330, DQ = 270, DQSE = 240
				begin
					dqs_phase  = 7'b0111110;
					dq_phase   = 7'b0101100;
					dqse_phase = 6'b001010;
				end
			5'b00110: //USER DQS = 360, DQ = 300, DQSE = 270
				begin
					dqs_phase  = 7'b0010010;
					dq_phase   = 7'b0000010;
					dqse_phase = 6'b001110;
				end
			5'b00111: //USER DQS = 390, DQ = 330, DQSE = 300
				begin
					dqs_phase  = 7'b0011010;
					dq_phase   = 7'b0001010;
					dqse_phase = 6'b010011;
				end
			5'b01000: //USER DQS = 420, DQ = 360, DQSE = 330
				begin
					dqs_phase  = 7'b0100010;
					dq_phase   = 7'b0010010;
					dqse_phase = 6'b010111;
				end
			5'b01001: //USER DQS = 450, DQ = 390, DQSE = 360
				begin
					dqs_phase  = 7'b0101010;
					dq_phase   = 7'b0011010;
					dqse_phase = 6'b011011;
				end
			5'b01010: //USER DQS = 480, DQ = 420, DQSE = 390
				begin
					dqs_phase  = 7'b0110001;
					dq_phase   = 7'b0100010;
					dqse_phase = 6'b000101;
				end
			5'b01011: //USER DQS = 510, DQ = 450, DQSE = 420
				begin
					dqs_phase  = 7'b0111001;
					dq_phase   = 7'b0101010;
					dqse_phase = 6'b001001;
				end
			5'b01100: //USER DQS = 540, DQ = 480
				begin
					dqs_phase  = 7'b0010101;
					dq_phase   = 7'b0000101;
				end
			5'b01101: //USER DQS = 570, DQ = 510
				begin
					dqs_phase  = 7'b0011101;
					dq_phase   = 7'b0001101;
				end
			5'b01110: //USER DQS = 600, DQ = 540
				begin
					dqs_phase  = 7'b0100101;
					dq_phase   = 7'b0010101;
				end
			5'b01111: //USER DQS = 630, DQ = 570
				begin
					dqs_phase  = 7'b0101101;
					dq_phase   = 7'b0011101;
				end
			5'b10000: //USER DQS = 660, DQ = 600
				begin
					dqs_phase  = 7'b0110111;
					dq_phase   = 7'b0100101;
				end
			5'b10001: //USER DQS = 690, DQ = 630
				begin
					dqs_phase  = 7'b0111111;
					dq_phase   = 7'b0101101;
				end
			5'b10010: //USER DQS = 720, DQ = 660
				begin
					dqs_phase  = 7'b0010011;
					dq_phase   = 7'b0000011;
				end
			5'b10011: //USER DQS = 750, DQ = 690
				begin
					dqs_phase  = 7'b0011011;
					dq_phase   = 7'b0001011;
				end
			5'b10100: //USER DQS = 780, DQ = 720
				begin
					dqs_phase  = 7'b0100011;
					dq_phase   = 7'b0010011;
				end
			5'b10101: //USER DQS = 810, DQ = 750
				begin
					dqs_phase  = 7'b0101011;
					dq_phase   = 7'b0011011;
				end
			default : begin end
			endcase
		end
		default : begin end
		endcase
	end
	
	//USER config data storage
	
	always_ff @ (posedge avl_clk or negedge avl_reset_n) begin
		if (~avl_reset_n) begin
			avl_io_cfg <= '0;
			avl_dqs_cfg <= '0;
			
			avl_dqs_cfg[6:4] <= dqsi_phase;

			avl_dqs_cfg[14:11] <= dqs_phase_reset[6:3];
			avl_dqs_cfg[45] <= dqs_phase_reset[2];
			avl_dqs_cfg[39] <= dqs_phase_reset[1];
			avl_dqs_cfg[24] <= dqs_phase_reset[0];
	
			avl_dqs_cfg[18:15] <= dq_phase_reset[6:3];
			avl_dqs_cfg[44] <= dq_phase_reset[2];
			avl_dqs_cfg[40] <= dq_phase_reset[1];
			avl_dqs_cfg[26] <= dq_phase_reset[0];
	
			avl_dqs_cfg[10:7] <= dqse_phase_reset[5:2];
			avl_dqs_cfg[43] <= dqse_phase_reset[1];
			avl_dqs_cfg[38] <= dqse_phase_reset[0];
		end else if (sel_sdata_wr) begin
			avl_dqs_cfg[23:19] <= '0;
			avl_dqs_cfg[25] <= '0;
			avl_dqs_cfg[37] <= '0;
			avl_dqs_cfg[42:41] <= '0;
			
			avl_dqs_cfg[6:4] <= dqsi_phase;

			case (avl_address[3:0])
			SCC_ADDR_DQS_IN_DELAY: begin
				avl_dqs_cfg[3:0] <= avl_writedata[3:0];
			end
			SCC_ADDR_DQS_EN_PHASE: begin
				avl_dqs_cfg[10:7] <= dqse_phase[5:2];
				avl_dqs_cfg[43] <= dqse_phase[1];
				avl_dqs_cfg[38] <= dqse_phase[0];
			end
			SCC_ADDR_DQS_EN_DELAY: begin
				avl_dqs_cfg[29:27] <= avl_writedata[2:0];
			end
			SCC_ADDR_DQDQS_OUT_PHASE: begin
				avl_dqs_cfg[14:11] <= dqs_phase[6:3];
				avl_dqs_cfg[45] <= dqs_phase[2];
				avl_dqs_cfg[39] <= dqs_phase[1];
				avl_dqs_cfg[24] <= dqs_phase[0];

				avl_dqs_cfg[18:15] <= dq_phase[6:3];
				avl_dqs_cfg[44] <= dq_phase[2];
				avl_dqs_cfg[40] <= dq_phase[1];
				avl_dqs_cfg[26] <= dq_phase[0];
			end
			SCC_ADDR_OCT_OUT1_DELAY: begin
				avl_dqs_cfg[33:30] <= avl_writedata[3:0];
			end
			SCC_ADDR_OCT_OUT2_DELAY: begin
				avl_dqs_cfg[36:34] <= avl_writedata[2:0];
			end
			SCC_ADDR_IO_OUT1_DELAY: begin
				avl_io_cfg[3:0] <= avl_writedata[3:0];
			end
			SCC_ADDR_IO_OUT2_DELAY: begin
				avl_io_cfg[6:4] <= avl_writedata[2:0];
			end
			SCC_ADDR_IO_IN_DELAY: begin
				avl_io_cfg[10:7] <= avl_writedata[3:0];
			end
			default : begin end
			endcase
		end else begin
			avl_dqs_cfg[23:19] <= '0;
			avl_dqs_cfg[25] <= '0;
			avl_dqs_cfg[37] <= '0;
			avl_dqs_cfg[42:41] <= '0;

			avl_dqs_cfg[6:4] <= dqsi_phase;
		end
	end
	
	//USER data transfer from SCC to AVALON
	
	always_ff @ (posedge avl_clk) begin
		avl_done <= scc_done;
	end
	
	//USER scan chain side state update
	
	always_ff @ (posedge scc_clk) begin
		scc_go_dqs_ena <= avl_go_dqs_ena;
		scc_go_dqs_io_ena <= avl_go_dqs_io_ena;
		scc_go_dq_ena <= avl_go_dq_ena;
		scc_go_dm_ena <= avl_go_dm_ena;
		scc_go_upd <= avl_go_upd;
		scc_ena_addr <= avl_writedata[7:0];
		scc_io_cfg_curr <= scc_io_cfg_next;
		scc_dqs_cfg_curr <= scc_dqs_cfg_next;
		scc_shift_cnt_curr <= scc_shift_cnt_next;
	end

	//USER scan chain state machine transitions.
	
	always_ff @ (posedge scc_clk or negedge reset_n_scc_clk) begin
		if (~reset_n_scc_clk) begin
			scc_state_curr <= STATE_SCC_IDLE;
		end else begin
			scc_state_curr <= scc_state_next;
		end
	end
	
	always_ff @ (negedge scc_clk) begin
		scc_data <= scc_data_c;
		scc_dqs_ena <= scc_dqs_ena_c;
		scc_dqs_io_ena <= scc_dqs_io_ena_c;
		scc_dq_ena <= scc_dq_ena_c;
		scc_dm_ena <= scc_dm_ena_c;
		scc_upd <= scc_upd_c;	
	end

	always_comb begin
		scc_ena_addr_decode = '0;

		if (scc_ena_addr == 8'b11111111) 
			scc_ena_addr_decode = '1;
		else
			scc_ena_addr_decode[scc_ena_addr] = 1;
		
		scc_state_next = scc_state_curr;
		scc_shift_cnt_next = '0;
		scc_io_cfg_next = avl_io_cfg;
		scc_dqs_cfg_next = avl_dqs_cfg;
		scc_data_c = 0;
		scc_dqs_ena_c = '0;
		scc_dqs_io_ena_c = '0;
		scc_dq_ena_c = '0;
		scc_dm_ena_c = '0;
		scc_upd_c = 0;
		scc_done = 0;

		case (scc_state_curr)
		STATE_SCC_IDLE: begin
			if (scc_go_dqs_io_ena | scc_go_dq_ena | scc_go_dm_ena) begin
				scc_state_next = STATE_SCC_LOAD;
				scc_shift_cnt_next = IO_SDATA_BITS - 1;
			end else if (scc_go_dqs_ena) begin
				scc_state_next = STATE_SCC_LOAD;
				scc_shift_cnt_next = DQS_SDATA_BITS - 1;
			end else if (scc_go_upd) begin
				scc_state_next = STATE_SCC_DONE;
				scc_upd_c = 1;
			end
		end
		STATE_SCC_LOAD: begin
			scc_shift_cnt_next = scc_shift_cnt_curr - 1;

			if (scc_go_dqs_ena) begin
				scc_dqs_ena_c = scc_ena_addr_decode;
				scc_data_c = scc_dqs_cfg_curr[DQS_SDATA_BITS - 1];
				scc_dqs_cfg_next = scc_dqs_cfg_curr << 1;
			end
			
			if (scc_go_dqs_io_ena) begin
				scc_dqs_io_ena_c = scc_ena_addr_decode;
				scc_data_c = scc_io_cfg_curr[IO_SDATA_BITS - 1];
				scc_io_cfg_next = scc_io_cfg_curr << 1;
			end 
			
			if (scc_go_dq_ena) begin
				scc_dq_ena_c = scc_ena_addr_decode;
				scc_data_c = scc_io_cfg_curr[IO_SDATA_BITS - 1];
				scc_io_cfg_next = scc_io_cfg_curr << 1;
			end
			
			if (scc_go_dm_ena) begin
				scc_dm_ena_c = scc_ena_addr_decode;
				scc_data_c = scc_io_cfg_curr[IO_SDATA_BITS - 1];
				scc_io_cfg_next = scc_io_cfg_curr << 1;
			end
			
			if (scc_shift_cnt_curr == 0) begin
				scc_state_next = STATE_SCC_DONE;
			end
		end
		STATE_SCC_DONE:	begin
			scc_done = 1;

			if (~scc_go_dqs_ena & ~scc_go_dqs_io_ena &
				~scc_go_dq_ena & ~scc_go_dm_ena & ~scc_go_upd) 
				scc_state_next = STATE_SCC_IDLE;
		end
		default : begin end
		endcase
	end
endmodule
