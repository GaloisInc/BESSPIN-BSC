//USER ********************************************************************************************
//USER File name: rw_mgr_core.sv
//USER Defines the innards of the R/W manager portion of the sequencer.
//USER The R/W manager interacts with the PHY through the AFI interface and
//USER is responsible for encapsulating the memory protocol required to talk
//USER to the memory device. It supports initial reset and bring up
//USER (initialization sequence), refresh for those protocols that require
//USER it, read, writes, and other special reads and writes required for
//USER calibration. It does not provide any reordering or bank management
//USER capability and is intended to be used for calibration only!
//USER 
//USER Usage:
//USER 
//USER  - 
//USER 
//USER ********************************************************************************************

// altera message_off 10230 10036
module ddr2_v10_1_0002_sequencer_rw_mgr_core (
	avl_clk,
	avl_reset_n,
 
	//USER AFI interface portion
  
	afi_clk,
	afi_reset_n,
	
	afi_cke,
	afi_odt,
	afi_address,
	afi_bank,
	afi_cs_n,
	afi_ras_n,
	afi_cas_n,
	afi_we_n,
	afi_wdata,
	afi_wdata_valid,
	afi_dm,
	afi_rdata_en,
	afi_rdata,
	afi_rdata_valid,
	afi_dqs_en,

	
	//USER SOPC interface portion
  
	config_reg_wr,	//USER config register write
	config_reg_rd,	//USER config register read
	cmd_exec,		//USER command execute
	cmd_done,		//USER command done?
	param_rd,		//USER parameter read
	cstate_rd,		//USER core state read
	avl_address,	//USER avalon address
	avl_wdata,		//USER avalon write data
	avl_rdata		//USER avalon read data
);
  
//USER ********************************************************************************************
//USER Parameter section

parameter AVL_DATA_WIDTH 			= 32;
parameter AVL_ADDRESS_WIDTH			= 16;

//USER AFI parameters

parameter AFI_ADDRESS_WIDTH			= "";
parameter AFI_CONTROL_WIDTH			= "";
parameter AFI_DATA_WIDTH			= "";
parameter AFI_DATA_MASK_WIDTH		= "";
parameter AFI_DQS_WIDTH				= "";

//USER MEM parameters 

parameter MEM_IF_READ_DQS_WIDTH		= "";
parameter MEM_IF_WRITE_DQS_WIDTH	= "";

parameter MEM_NUMBER_OF_RANKS = "";
parameter MEM_MIRROR_ADDRESSING = "";

//USER Stuff that goes into Mode Registers

	parameter MEM_TINIT_CK = "";
	parameter MEM_TMRD_CK = "";
	parameter AFI_BANK_WIDTH = ""; 
	parameter AFI_CHIP_SELECT_WIDTH = "";
	parameter AFI_CLK_EN_WIDTH = ""; 
	parameter AFI_ODT_WIDTH = ""; 
	parameter INIT_NOP_COUNT_WIDTH = "";
	parameter MRD_COUNT_WIDTH = "";
	parameter MR0_BL = "";
	parameter MR0_BT = "";
	parameter MR0_CAS_LATENCY = "";
	parameter MR0_WR = "";
	parameter MR0_PD = "";
	parameter MR1_DLL = "";
	parameter MR1_ODS = "";
	parameter MR1_RTT = "";
	parameter MR1_AL = "";
	parameter MR1_QOFF = "";
	parameter RDIMM = "";
	parameter RP_COUNT_WIDTH = "";
	parameter RFC_COUNT_WIDTH = "";
	parameter OIT_COUNT_WIDTH = "";
	parameter MR1_DQS = "";
	parameter MR1_RDQS = "";
	parameter MR2_SRT = "";

input avl_clk;
input avl_reset_n;

//USER AFI interface portion

input afi_clk;
input afi_reset_n;
	
output [AFI_CLK_EN_WIDTH - 1:0] afi_cke;
output [AFI_ODT_WIDTH - 1:0] afi_odt;
output [AFI_ADDRESS_WIDTH - 1:0] afi_address;
output [AFI_BANK_WIDTH - 1:0] afi_bank;
output [AFI_CHIP_SELECT_WIDTH - 1:0] afi_cs_n;
output [AFI_CONTROL_WIDTH - 1:0] afi_ras_n;
output [AFI_CONTROL_WIDTH - 1:0] afi_cas_n;
output [AFI_CONTROL_WIDTH - 1:0] afi_we_n;
output [AFI_DATA_WIDTH - 1:0] afi_wdata;
output [AFI_DQS_WIDTH - 1:0] afi_wdata_valid;
output [AFI_DATA_MASK_WIDTH - 1:0] afi_dm;
output afi_rdata_en;
input [AFI_DATA_WIDTH - 1:0] afi_rdata;
input afi_rdata_valid;
output [AFI_DQS_WIDTH - 1:0] afi_dqs_en;

//USER SOPC interface portion

input config_reg_wr;
input config_reg_rd;
input cmd_exec;
output cmd_done;
input param_rd;
input cstate_rd;
input [AVL_ADDRESS_WIDTH - 1:0] avl_address;
input [AVL_DATA_WIDTH - 1:0] avl_wdata;
output [AVL_DATA_WIDTH - 1:0] avl_rdata;

//USER Local parameters

	localparam MEM_ADDRESS_WIDTH = AFI_ADDRESS_WIDTH / 2;
	localparam MEM_BANK_WIDTH = AFI_BANK_WIDTH / 2;
	localparam MEM_CONTROL_WIDTH = AFI_CONTROL_WIDTH / 2;
	localparam MEM_CHIP_SELECT_WIDTH = AFI_CHIP_SELECT_WIDTH / 2;
	localparam MEM_CLK_EN_WIDTH = AFI_CLK_EN_WIDTH / 2;
	localparam MEM_ODT_WIDTH = AFI_ODT_WIDTH / 2;
	localparam MEM_DATA_WIDTH = AFI_DATA_WIDTH / 4;
	localparam MEM_DATA_MASK_WIDTH = AFI_DATA_MASK_WIDTH / 4;

localparam MEM_DQ_PER_DQS = MEM_DATA_WIDTH / MEM_IF_READ_DQS_WIDTH;

localparam MR0	= ((MR0_BL) | ((MR0_BT) << 3) | ((MR0_CAS_LATENCY) << 4) | (1 << 8) | ((MR0_WR) << 9) | ((MR0_PD) << 12));
localparam MR1	= ((MR1_DLL) | (((MR1_ODS) & 1) << 1) | (((MR1_RTT) & 1) << 2) | (((MR1_RTT) & 2) << 5) | ((MR1_AL) << 3) | (0 << 7) | ((MR1_DQS) << 10) | ((MR1_RDQS) << 11) | ((MR1_QOFF) << 12));
localparam MR2	= ((MR2_SRT) << 7);
localparam MR3	= 0;

//USER Commands available

typedef enum bit [4:0] {
	RWC_CMD_WRITE_MREG0 = 5'b00000,
	RWC_CMD_WRITE_MREG1 = 5'b00001,
	RWC_CMD_WRITE_MREG2 = 5'b00010,
	RWC_CMD_WRITE_MREG3 = 5'b00011,
	RWC_CMD_WRITE_B0	= 5'b00100,
	RWC_CMD_WRITE_B1	= 5'b00101,
	RWC_CMD_READ_B0		= 5'b00110,
	RWC_CMD_READ_B1		= 5'b00111,
	RWC_CMD_WRITE_C0	= 5'b01000,
	RWC_CMD_WRITE_C1	= 5'b01001,
	RWC_CMD_READ_B2B	= 5'b01010,
	RWC_CMD_WRITE_B2B	= 5'b01011,
	RWC_CMD_RUN_ZQCS	= 5'b01100,
	RWC_CMD_RUN_ZQCL	= 5'b01101,
	RWC_CMD_ACTIVATE0	= 5'b01110,
	RWC_CMD_ACTIVATE1	= 5'b01111,
	RWC_CMD_PRECHARGE	= 5'b10000,
	RWC_CMD_REFRESH		= 5'b10001,
	RWC_CMD_NOP  		= 5'b10010
} rwc_cmd_t;

//USER configuration registers

typedef enum bit [3:0] {
	RWC_CREG_DATAMASK = 4'b0000,
	RWC_CREG_BURST_WR = 4'b0001,
	RWC_CREG_BURST_RD = 4'b0010,
	RWC_CREG_WDINV_WR = 4'b0011,
	RWC_CREG_WDINV_RD = 4'b0100,
	RWC_CREG_CKE	  = 4'b0101,
	RWC_CREG_ODT	  = 4'b0110,
	RWC_CREG_RESET	  = 4'b0111,
	RWC_CREG_ISSUE_HI = 4'b1000,
	RWC_CREG_MREG0	  = 4'b1001,
	RWC_CREG_MREG1	  = 4'b1010,
	RWC_CREG_MREG2	  = 4'b1011,
	RWC_CREG_MREG3	  = 4'b1100,
	RWC_CREG_RANK	  = 4'b1101
} rwc_creg_t;

//USER State of R/W manager

typedef enum int unsigned { 
	STATE_RWC_IDLE, //USER waiting for commands
	STATE_RWC_LOAD, //USER loading various parameters
	STATE_RWC_WAIT, //USER waiting for command to finish
	STATE_RWC_DONE  //USER waiting for AVALON to acknowledge done signal
} rwc_state_t;

rwc_state_t curr_state;
rwc_state_t next_state;
logic cmd_done_afi;

//USER local versions of AFI outputs

reg [AFI_ADDRESS_WIDTH - 1:0] afi_address;
reg [AFI_BANK_WIDTH - 1:0] afi_bank;
reg [AFI_CHIP_SELECT_WIDTH - 1:0] afi_cs_n;
reg [AFI_CONTROL_WIDTH - 1:0] afi_ras_n;
reg [AFI_CONTROL_WIDTH - 1:0] afi_cas_n;
reg [AFI_CONTROL_WIDTH - 1:0] afi_we_n;
reg [AFI_DATA_WIDTH - 1:0] afi_wdata;
reg [AFI_DQS_WIDTH - 1:0] afi_wdata_valid;
reg [AFI_DATA_MASK_WIDTH - 1:0] afi_dm;
reg afi_rdata_en;
reg [AFI_CLK_EN_WIDTH - 1:0] afi_cke;
reg [AFI_ODT_WIDTH - 1:0] afi_odt;
reg [AFI_DQS_WIDTH - 1:0] afi_dqs_en;

//USER local versions of AVL outputs 

reg cmd_done;
reg [AVL_DATA_WIDTH - 1:0] avl_rdata;

	//USER config registers AVL side.
	
	reg [31:0] creg_datamask_avl;
	reg [31:0] creg_burst_wr_avl;
	reg [31:0] creg_burst_rd_avl;
	reg creg_wdinv_wr_avl;
	reg creg_wdinv_rd_avl;
	reg creg_cke_avl;
	reg creg_odt_avl;
	reg creg_mreset_n_avl;
	reg creg_issue_hi_avl;
	reg [12:0] creg_mreg0_avl;
	reg [12:0] creg_mreg1_avl;
	reg [12:0] creg_mreg2_avl;
	reg [12:0] creg_mreg3_avl;
	reg [3:0] creg_rank_avl;
	
	//USER config registers AFI side
	
	reg [31:0] creg_datamask_afi;
	reg [31:0] creg_burst_wr_afi;
	reg [31:0] creg_burst_rd_afi;
	reg [31:0] burst_rd_afi;
	reg creg_wdinv_wr_afi;
	reg creg_wdinv_rd_afi;
	reg creg_issue_hi_afi;
	reg [12:0] creg_mreg0_afi;
	reg [12:0] creg_mreg1_afi;
	reg [12:0] creg_mreg2_afi;
	reg [12:0] creg_mreg3_afi;
	reg [3:0] creg_rank_afi;
	wire [31:0] creg_rank_afi_mem_mirror_addressing;
	
	//USER timer running on AVL clock
	
	reg [AVL_DATA_WIDTH - 1:0] timer_avl;

	//USER command decoding AVL side
	
	reg [4:0] cmd_op_avl;
	reg [3:0] cmd_op_tm_i0_avl;
	reg [3:0] cmd_op_tm_i1_avl;
	reg [5:0] cmd_op_tm_bb_avl;
	reg [5:0] cmd_op_tm_be_avl;
	reg [5:0] cmd_op_tm_end_avl;
	reg cmd_op_col_avl;
	
	reg [MEM_DATA_WIDTH - 1:0] rpath_burst_chk_avl;
	
	//USER command decoding AFI side
	
	reg [4:0] cmd_op_afi;
	reg [3:0] cmd_op_tm_i0_afi;
	reg [3:0] cmd_op_tm_i1_afi;
	reg [5:0] cmd_op_tm_bb_afi;
	reg [5:0] cmd_op_tm_be_afi;
	reg [5:0] cmd_op_tm_end_afi;
	reg cmd_op_col_afi;
	
	//USER command exec AFI side
	
	reg cmd_exec_afi;

	//USER AVL read bus
	
	logic [AVL_DATA_WIDTH - 1:0] avl_rdata0;
	logic [AVL_DATA_WIDTH - 1:0] avl_rdata1;
	logic [AVL_DATA_WIDTH - 1:0] avl_rdata2;
	
	//USER AFI state timer.

	reg afi_timer_reset;
	reg [5:0] afi_timer;
	
	//USER burst data construction
	
	wire wr_burst_w0e;
	wire wr_burst_w1e;
		wire wr_burst_w2e;
		wire wr_burst_w3e;

	wire rd_burst_w0e;
	wire rd_burst_w1e;
		wire rd_burst_w2e;
		wire rd_burst_w3e;

	wire datamask_w0;
	wire datamask_w1;
		wire datamask_w2;
		wire datamask_w3;
	
	//USER AFI write operations.
	
	reg [5:0] wpath_tm_bgn_dqs_en_b;
	reg [5:0] wpath_tm_bgn_b;
	reg [5:0] wpath_tm_end_b;
		reg [2:0] wpath_output_cnt;
	reg wpath_output_valid;
	reg wpath_output_dqs_en_bgn;
	reg wpath_output_dqs_en_mid;

	wire [AFI_DATA_WIDTH - 1:0] wpath_data;
	wire [AFI_DATA_MASK_WIDTH - 1:0] wpath_datam;
	
	//USER AFI read operations
	
	reg [AFI_DATA_WIDTH - 1:0] afi_rdata_reg;
	reg afi_rdata_valid_reg;
	
	reg [5:0] rpath_tm_bgn_b;
	reg [5:0] rpath_tm_end_b;
	reg [MEM_DATA_WIDTH - 1:0] rpath_burst_chk_afi;
	reg rpath_read_enable;
	
	wire [MEM_DATA_WIDTH - 1:0] rpath_data_chk;

	reg [5:0] addrcmd_tm_issue0;
	reg [5:0] addrcmd_tm_issue1;
	reg [5:0] addrcmd_tm_end;
	reg addrcmd_output_i0;
	reg addrcmd_output_i1;
	reg addrcmd_done;

	//USER address and command issued first
	
	logic [MEM_ADDRESS_WIDTH - 1:0] afi_address_c0;
	logic [MEM_BANK_WIDTH - 1:0] afi_bank_c0;
	logic afi_cs_n_c0_tmp;
	logic [MEM_CHIP_SELECT_WIDTH - 1:0] afi_cs_n_c0;
	logic [MEM_CONTROL_WIDTH - 1:0] afi_ras_n_c0;
	logic [MEM_CONTROL_WIDTH - 1:0] afi_cas_n_c0;
	logic [MEM_CONTROL_WIDTH - 1:0] afi_we_n_c0;

	//USER address and command issued second (if turned on)
	
	logic [MEM_ADDRESS_WIDTH - 1:0] afi_address_c1;
	logic [MEM_BANK_WIDTH - 1:0] afi_bank_c1;
	logic afi_cs_n_c1_tmp;
	logic [MEM_CHIP_SELECT_WIDTH - 1:0] afi_cs_n_c1;
	logic [MEM_CONTROL_WIDTH - 1:0] afi_ras_n_c1;
	logic [MEM_CONTROL_WIDTH - 1:0] afi_cas_n_c1;
	logic [MEM_CONTROL_WIDTH - 1:0] afi_we_n_c1;

	//USER combine several address and command words into
	//USER a single AFI word (accounts for half/full rate)
	
	logic [AFI_ADDRESS_WIDTH - 1:0] afi_address_c0m;
	logic [AFI_BANK_WIDTH - 1:0] afi_bank_c0m;
	logic [AFI_CHIP_SELECT_WIDTH - 1:0] afi_cs_n_c0m;
	logic [AFI_CONTROL_WIDTH - 1:0] afi_ras_n_c0m;
	logic [AFI_CONTROL_WIDTH - 1:0] afi_cas_n_c0m;
	logic [AFI_CONTROL_WIDTH - 1:0] afi_we_n_c0m;

	logic [AFI_ADDRESS_WIDTH - 1:0] afi_address_c1m;
	logic [AFI_BANK_WIDTH - 1:0] afi_bank_c1m;
	logic [AFI_CHIP_SELECT_WIDTH - 1:0] afi_cs_n_c1m;
	logic [AFI_CONTROL_WIDTH - 1:0] afi_ras_n_c1m;
	logic [AFI_CONTROL_WIDTH - 1:0] afi_cas_n_c1m;
	logic [AFI_CONTROL_WIDTH - 1:0] afi_we_n_c1m;

	// Create a 32-bit address for accessing the MEM_MIRROR_ADDRESSING parameter
	// since parameters are 32-bits
	assign creg_rank_afi_mem_mirror_addressing = {28'b0,creg_rank_afi};


	//USER AVL timer. Useful for profiling how long operations take in
	//USER AVALON cycles.
	
	always_ff @ (posedge avl_clk or negedge avl_reset_n)
		if (~avl_reset_n)
			timer_avl <= '0;
		else
			timer_avl <= timer_avl + 1;

	//USER AVL side write operations
	
	always_ff @ (posedge avl_clk or negedge avl_reset_n) begin
		if (~avl_reset_n) begin
			creg_mreg0_avl <= MR0;
			creg_mreg1_avl <= MR1;
			creg_mreg2_avl <= MR2;
			creg_mreg3_avl <= MR3;
			creg_cke_avl <= '0;
			creg_odt_avl <= '0;
			creg_mreset_n_avl <= '0;
		end else if (config_reg_wr) begin
			//USER AVL writing into the config registers
			
			case (avl_address[3:0])
			RWC_CREG_MREG0: creg_mreg0_avl <= avl_wdata[12:0];
			RWC_CREG_MREG1: creg_mreg1_avl <= avl_wdata[12:0];
			RWC_CREG_MREG2: creg_mreg2_avl <= avl_wdata[12:0];
			RWC_CREG_MREG3: creg_mreg3_avl <= avl_wdata[12:0];
			RWC_CREG_CKE: creg_cke_avl <= avl_wdata[0];
			RWC_CREG_ODT: creg_odt_avl <= avl_wdata[0];
			RWC_CREG_RESET: creg_mreset_n_avl <= avl_wdata[0];
			default : begin end
			endcase
		end
	end
	
	always_ff @ (posedge avl_clk) begin
		if (config_reg_wr) begin
			//USER AVL writing into the config registers
			
			case (avl_address[3:0])
			RWC_CREG_DATAMASK: creg_datamask_avl <= avl_wdata;
			RWC_CREG_BURST_WR: creg_burst_wr_avl <= avl_wdata;
			RWC_CREG_BURST_RD: creg_burst_rd_avl <= avl_wdata;
			RWC_CREG_WDINV_WR: creg_wdinv_wr_avl <= avl_wdata[0];
			RWC_CREG_WDINV_RD: creg_wdinv_rd_avl <= avl_wdata[0];
			RWC_CREG_ISSUE_HI: creg_issue_hi_avl <= avl_wdata[0];
			RWC_CREG_RANK: creg_rank_avl <= avl_wdata[3:0];
			default : begin end
			endcase
		end
			
		
		//USER command decode.
		
		cmd_op_avl <= avl_wdata[4:0]; //USER operation type
		cmd_op_tm_i0_avl <= avl_wdata[8:5]; //USER time at which first addr/cmd go out.
		cmd_op_tm_i1_avl <= avl_wdata[12:9]; //USER time at which second addr/cmd go out.
		cmd_op_tm_bb_avl <= avl_wdata[18:13]; //USER time at which burst starts.
		cmd_op_tm_be_avl <= avl_wdata[24:19]; //USER time at which burst ends.
		cmd_op_tm_end_avl <= avl_wdata[30:25]; //USER time at which entire operation ends.
		cmd_op_col_avl <= avl_wdata[31]; //USER column address for reads/writes.
	end
	
	//USER SOPC side read bus 
	
	always_comb begin
		avl_rdata0 = '0;
		
		if (config_reg_rd) begin
			case (avl_address[3:0])
			RWC_CREG_DATAMASK: avl_rdata0 = creg_datamask_avl;
			RWC_CREG_BURST_WR: avl_rdata0 = creg_burst_wr_avl;
			RWC_CREG_BURST_RD: avl_rdata0 = creg_burst_rd_avl;
			RWC_CREG_WDINV_WR: avl_rdata0[0] = creg_wdinv_wr_avl;
			RWC_CREG_WDINV_RD: avl_rdata0[0] = creg_wdinv_rd_avl;
			RWC_CREG_CKE: avl_rdata0[0] = creg_cke_avl;
			RWC_CREG_ODT: avl_rdata0[0] = creg_odt_avl;
			RWC_CREG_RESET: avl_rdata0[0] = creg_mreset_n_avl;
			RWC_CREG_ISSUE_HI: avl_rdata0[0] = creg_issue_hi_avl;
			RWC_CREG_MREG0: avl_rdata0[12:0] = creg_mreg0_avl;
			RWC_CREG_MREG1: avl_rdata0[12:0] = creg_mreg1_avl;
			RWC_CREG_MREG2: avl_rdata0[12:0] = creg_mreg2_avl;
			RWC_CREG_MREG3: avl_rdata0[12:0] = creg_mreg3_avl;
			RWC_CREG_RANK: avl_rdata0[3:0] = creg_rank_avl;
			default : begin end
			endcase
		end

		avl_rdata1 = '0;
		
		if (param_rd) begin
// This could be ifdef-ed in a more efficient way by reusing common
// pieces of encoding but it would be almost impossible to read and 
// keep track of the increment. This problem is made even worse by
// the fact that this mapping must be kept manually in sync with the 
// software one. For now I'll break this in full protocol chuncks.
// Eventually this part of the code should be automatically generated
// for bot SW and HW
			case (avl_address[5:0])
			6'b000000: avl_rdata1 = MEM_ADDRESS_WIDTH;
			6'b000001: avl_rdata1 = MEM_BANK_WIDTH;
			6'b000010: avl_rdata1 = MEM_CONTROL_WIDTH;
			6'b000011: avl_rdata1 = MEM_CHIP_SELECT_WIDTH;
			6'b000100: avl_rdata1 = MEM_CLK_EN_WIDTH;
			6'b000101: avl_rdata1 = MEM_ODT_WIDTH;
			6'b000110: avl_rdata1 = MEM_DATA_WIDTH;
			6'b000111: avl_rdata1 = MEM_DATA_MASK_WIDTH;
			6'b001000: avl_rdata1 = MEM_IF_READ_DQS_WIDTH;
			6'b001001: avl_rdata1 = MEM_IF_WRITE_DQS_WIDTH;
			6'b001010: avl_rdata1 = MR0_BL;
			6'b001011: avl_rdata1 = MR0_BT;
			6'b001100: avl_rdata1 = MR0_CAS_LATENCY;
			6'b001101: avl_rdata1 = MR1_QOFF;
			6'b001110: avl_rdata1 = MR0_WR;
			6'b001111: avl_rdata1 = MR0_PD;
			6'b010000: avl_rdata1 = MR1_DLL;
			6'b010001: avl_rdata1 = MR1_ODS;
			6'b010010: avl_rdata1 = MR1_RTT;
			6'b010011: avl_rdata1 = MR1_AL;
			6'b010100: avl_rdata1 = RP_COUNT_WIDTH;
			6'b010101: avl_rdata1 = RFC_COUNT_WIDTH;
			6'b010110: avl_rdata1 = OIT_COUNT_WIDTH;
			6'b010111: avl_rdata1 = MR1_DQS;
			6'b011000: avl_rdata1 = MR1_RDQS;
			6'b011001: avl_rdata1 = MR2_SRT;
				6'b011010: avl_rdata1 = 1;
			6'b011011: avl_rdata1 = MEM_DQ_PER_DQS;
			6'b011100: avl_rdata1 = MEM_NUMBER_OF_RANKS;
			6'b011101: avl_rdata1 = MEM_TINIT_CK;
			6'b011110: avl_rdata1 = MEM_TMRD_CK;
				6'b011111: avl_rdata1 = 1;
				6'b100000: avl_rdata1 = 0;
			default : begin end
			endcase
		end
		
		avl_rdata2 = '0;
		
		if (cstate_rd) begin
			case (avl_address[9:8])
			2'b00: avl_rdata2 = timer_avl;
			2'b01: avl_rdata2[0] = (rpath_burst_chk_avl[avl_address[7:0]] !== 1'b0); // 4-state compare to avoid passing X into Nios
			default : begin end
			endcase
		end
	end

	//USER SOPC side read bus 
	
	always_ff @ (posedge avl_clk) begin
		avl_rdata <= avl_rdata0 | avl_rdata1 | avl_rdata2;
	end
	
	//USER AVL-to-AFI transfer
	
	always_ff @ (posedge afi_clk) begin
		creg_datamask_afi <= creg_datamask_avl;
		creg_burst_wr_afi <= creg_burst_wr_avl;
		creg_burst_rd_afi <= creg_burst_rd_avl;
		creg_wdinv_wr_afi <= creg_wdinv_wr_avl;
		creg_wdinv_rd_afi <= creg_wdinv_rd_avl;
		creg_issue_hi_afi <= creg_issue_hi_avl;
		creg_mreg0_afi <= creg_mreg0_avl;
		creg_mreg1_afi <= creg_mreg1_avl;
		creg_mreg2_afi <= creg_mreg2_avl;
		creg_mreg3_afi <= creg_mreg3_avl;
		creg_rank_afi <= creg_rank_avl;

		cmd_op_afi <= cmd_op_avl;
		
		cmd_exec_afi <= cmd_exec;
		cmd_op_tm_i0_afi <= cmd_op_tm_i0_avl;
		cmd_op_tm_i1_afi <= cmd_op_tm_i1_avl;
		cmd_op_tm_bb_afi <= cmd_op_tm_bb_avl;
		cmd_op_tm_be_afi <= cmd_op_tm_be_avl;
		cmd_op_tm_end_afi <= cmd_op_tm_end_avl;
		cmd_op_col_afi <= cmd_op_col_avl;
		
		afi_cke <= {(AFI_CLK_EN_WIDTH){creg_cke_avl}};
		afi_odt <= {(AFI_ODT_WIDTH){creg_odt_avl}};
	end

	//USER AFI-to-AVL transfer
	
	always_ff @ (posedge avl_clk) begin
		rpath_burst_chk_avl <= rpath_burst_chk_afi;
		cmd_done <= cmd_done_afi;
	end
	
	//USER AFI global timer, and preload of all timing pointers
	
	always_ff @ (posedge afi_clk) begin
		if (afi_timer_reset) begin
			afi_timer <= '0;
			
			wpath_tm_bgn_dqs_en_b <= '0;
			wpath_tm_bgn_b <= '0;
			wpath_tm_end_b <= '0;
			rpath_tm_bgn_b <= '0;
			rpath_tm_end_b <= '0;
			addrcmd_tm_issue0 <= '0;
			addrcmd_tm_issue1 <= '0;
			addrcmd_tm_end <= '0;

			case (cmd_op_afi)
			RWC_CMD_WRITE_MREG0: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_WRITE_MREG1: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_WRITE_MREG2: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_WRITE_MREG3: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_WRITE_B0: begin
				wpath_tm_bgn_dqs_en_b <= cmd_op_tm_bb_afi - 1;
				wpath_tm_bgn_b <= cmd_op_tm_bb_afi;
				wpath_tm_end_b <= cmd_op_tm_be_afi;
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_WRITE_B1: begin
				wpath_tm_bgn_dqs_en_b <= cmd_op_tm_bb_afi - 1;
				wpath_tm_bgn_b <= cmd_op_tm_bb_afi;
				wpath_tm_end_b <= cmd_op_tm_be_afi;
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_READ_B0: begin
				rpath_tm_bgn_b <= cmd_op_tm_bb_afi;
				rpath_tm_end_b <= cmd_op_tm_be_afi;
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_READ_B1: begin
				rpath_tm_bgn_b <= cmd_op_tm_bb_afi;
				rpath_tm_end_b <= cmd_op_tm_be_afi;
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_WRITE_C0: begin
				wpath_tm_bgn_dqs_en_b <= cmd_op_tm_bb_afi - 1;
				wpath_tm_bgn_b <= cmd_op_tm_bb_afi;
				wpath_tm_end_b <= cmd_op_tm_be_afi;
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i1_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_WRITE_C1: begin
				wpath_tm_bgn_dqs_en_b <= cmd_op_tm_bb_afi - 1;
				wpath_tm_bgn_b <= cmd_op_tm_bb_afi;
				wpath_tm_end_b <= cmd_op_tm_be_afi;
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i1_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_READ_B2B: begin
				rpath_tm_bgn_b <= cmd_op_tm_bb_afi;
				rpath_tm_end_b <= cmd_op_tm_be_afi;
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i1_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_WRITE_B2B: begin
				wpath_tm_bgn_dqs_en_b <= cmd_op_tm_bb_afi - 1;
				wpath_tm_bgn_b <= cmd_op_tm_bb_afi;
				wpath_tm_end_b <= cmd_op_tm_be_afi;
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i1_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_RUN_ZQCS: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_RUN_ZQCL: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_ACTIVATE0: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_ACTIVATE1: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_PRECHARGE: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_REFRESH: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			RWC_CMD_NOP: begin
				addrcmd_tm_issue0[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_issue1[3:0] <= cmd_op_tm_i0_afi;
				addrcmd_tm_end <= cmd_op_tm_end_afi;
			end
			default : begin end
			endcase
		end else if (afi_timer != '1) begin
			afi_timer <= afi_timer + 1;
		end
	end
	
	//USER construct outgoing data
	
	//USER construct outgoing data
	
		assign wr_burst_w0e = creg_burst_wr_afi[{wpath_output_cnt, 2'b00}];
		assign wr_burst_w1e = creg_burst_wr_afi[{wpath_output_cnt, 2'b01}];
		assign wr_burst_w2e = creg_burst_wr_afi[{wpath_output_cnt, 2'b10}];
		assign wr_burst_w3e = creg_burst_wr_afi[{wpath_output_cnt, 2'b11}];
	
		assign rd_burst_w0e = burst_rd_afi[0];
		assign rd_burst_w1e = burst_rd_afi[1];
		assign rd_burst_w2e = burst_rd_afi[2];
		assign rd_burst_w3e = burst_rd_afi[3];

		assign datamask_w0 = creg_datamask_afi[{wpath_output_cnt, 2'b00}];
		assign datamask_w1 = creg_datamask_afi[{wpath_output_cnt, 2'b01}];
		assign datamask_w2 = creg_datamask_afi[{wpath_output_cnt, 2'b10}];
		assign datamask_w3 = creg_datamask_afi[{wpath_output_cnt, 2'b11}];
	
	generate
		genvar i;
		
		for (i = 0; i < MEM_DATA_WIDTH; i = i + 2) begin: gen_data
			assign wpath_data[i                     ] = wr_burst_w0e;
			assign wpath_data[i +     MEM_DATA_WIDTH] = wr_burst_w1e;
				assign wpath_data[i + 2 * MEM_DATA_WIDTH] = wr_burst_w2e;
				assign wpath_data[i + 3 * MEM_DATA_WIDTH] = wr_burst_w3e;

			assign wpath_data[i                      + 1] = wr_burst_w0e ^ creg_wdinv_wr_afi;
			assign wpath_data[i +     MEM_DATA_WIDTH + 1] = wr_burst_w1e ^ creg_wdinv_wr_afi;
				assign wpath_data[i + 2 * MEM_DATA_WIDTH + 1] = wr_burst_w2e ^ creg_wdinv_wr_afi;
				assign wpath_data[i + 3 * MEM_DATA_WIDTH + 1] = wr_burst_w3e ^ creg_wdinv_wr_afi;

				assign rpath_data_chk[i] = 
					(rd_burst_w0e ^ afi_rdata_reg[i                     ]) |
					(rd_burst_w1e ^ afi_rdata_reg[i +     MEM_DATA_WIDTH]) |
					(rd_burst_w2e ^ afi_rdata_reg[i + 2 * MEM_DATA_WIDTH]) |
					(rd_burst_w3e ^ afi_rdata_reg[i + 3 * MEM_DATA_WIDTH]);
				assign rpath_data_chk[i + 1] = 
					(rd_burst_w0e ^ creg_wdinv_rd_afi ^ afi_rdata_reg[i                      + 1]) |
					(rd_burst_w1e ^ creg_wdinv_rd_afi ^ afi_rdata_reg[i +     MEM_DATA_WIDTH + 1]) |
					(rd_burst_w2e ^ creg_wdinv_rd_afi ^ afi_rdata_reg[i + 2 * MEM_DATA_WIDTH + 1]) |
					(rd_burst_w3e ^ creg_wdinv_rd_afi ^ afi_rdata_reg[i + 3 * MEM_DATA_WIDTH + 1]);
		end

		for (i = 0; i < MEM_DATA_MASK_WIDTH; i = i + 1) begin: gen_mask
			assign wpath_datam[i                          ] = datamask_w0;
			assign wpath_datam[i +     MEM_DATA_MASK_WIDTH] = datamask_w1;
				assign wpath_datam[i + 2 * MEM_DATA_MASK_WIDTH] = datamask_w2;
				assign wpath_datam[i + 3 * MEM_DATA_MASK_WIDTH] = datamask_w3;
		end
	endgenerate

	//USER AFI Write operations
	
	always_ff @ (posedge afi_clk) begin
		if (afi_timer_reset) begin
			wpath_output_valid <= 0;
			wpath_output_dqs_en_bgn <= 0;
			wpath_output_dqs_en_mid <= 0;
		end else begin
			if (afi_timer == wpath_tm_bgn_b) begin
				wpath_output_valid <= 1;
				wpath_output_dqs_en_mid <= 1;
				wpath_output_dqs_en_bgn <= 0;
			end

			if (afi_timer == wpath_tm_bgn_dqs_en_b) begin
				wpath_output_dqs_en_bgn <= 1;
			end
			
			if (afi_timer == wpath_tm_end_b) begin
				wpath_output_valid <= 0;
				wpath_output_dqs_en_mid <= 0;
				wpath_output_dqs_en_bgn <= 0;
			end			
		end
	end
	
	//USER AFI Write data
	
	always_ff @ (posedge afi_clk) begin
		if (wpath_output_valid) begin
			afi_dm <= wpath_datam;
			afi_wdata_valid <= {(AFI_DQS_WIDTH){1'b1}};
			afi_wdata <= wpath_data;
			wpath_output_cnt <= wpath_output_cnt + 1;
		end else begin
			afi_dm <= '0;
			afi_wdata <= '0;
			afi_wdata_valid <= {(AFI_DQS_WIDTH){1'b0}};			
			wpath_output_cnt <= 0;
		end
		
		if (wpath_output_dqs_en_bgn) begin
				afi_dqs_en <= {{(MEM_IF_WRITE_DQS_WIDTH){1'b1}}, {(MEM_IF_WRITE_DQS_WIDTH){1'b0}}};
		end else if (wpath_output_dqs_en_mid) begin
			afi_dqs_en <= {(AFI_DQS_WIDTH){1'b1}};
		end else begin
			afi_dqs_en <= '0;
		end
	end
	
	//USER AFI Read data registering 
	
	always_ff @ (posedge afi_clk) begin
		afi_rdata_reg <= afi_rdata;
		afi_rdata_valid_reg <= afi_rdata_valid;
	end
	
	//USER AFI Read data path
	
	always_ff @ (posedge afi_clk) begin
		if (afi_timer_reset) begin
			rpath_read_enable <= 0;
		end else begin
			if (afi_timer == rpath_tm_bgn_b) begin
				rpath_read_enable <= 1;
			end

			if (afi_timer == rpath_tm_end_b) begin
				rpath_read_enable <= 0;
			end
		end
	end
	
	//USER AFI Read data path enable
	
	always_ff @ (posedge afi_clk) begin
		if (afi_timer_reset) begin
			afi_rdata_en <= 0;
		end else begin
			afi_rdata_en <= rpath_read_enable;
		end
	end
	
	//USER AFI Read data path bit checking
	
	always_ff @ (posedge afi_clk) begin
		if (afi_timer_reset) begin
			rpath_burst_chk_afi <= '0;
			burst_rd_afi <= creg_burst_rd_afi;
		end else if (afi_rdata_valid_reg) begin
			rpath_burst_chk_afi <= rpath_burst_chk_afi | rpath_data_chk;
			burst_rd_afi <= {4'b0000, burst_rd_afi[31:4]};
		end		
	end

	//USER AFI Address and Command issue
	
	always_ff @ (posedge afi_clk) begin
		if (afi_timer_reset) begin
			addrcmd_done <= 0;
			addrcmd_output_i0 <= 0;
			addrcmd_output_i1 <= 0;
		end else begin			
			if (afi_timer == addrcmd_tm_issue0) begin
				addrcmd_output_i0 <= 1;
			end else begin
				addrcmd_output_i0 <= 0;
			end

			if (afi_timer == addrcmd_tm_issue1) begin
				addrcmd_output_i1 <= 1;
			end else begin
				addrcmd_output_i1 <= 0;
			end

			if (afi_timer == addrcmd_tm_end) begin
				addrcmd_done <= 1;
			end
		end
	end
	
	//USER AFI Address and Command
	
	always_ff @ (posedge afi_clk) begin
		if (addrcmd_output_i0) begin
			afi_address <= afi_address_c0m;
			afi_bank <= afi_bank_c0m;
			afi_cs_n <= afi_cs_n_c0m;
			afi_ras_n <= afi_ras_n_c0m;
			afi_cas_n <= afi_cas_n_c0m;
			afi_we_n <= afi_we_n_c0m;
		end else if (addrcmd_output_i1) begin
			afi_address <= afi_address_c1m;
			afi_bank <= afi_bank_c1m;
			afi_cs_n <= afi_cs_n_c1m;
			afi_ras_n <= afi_ras_n_c1m;
			afi_cas_n <= afi_cas_n_c1m;
			afi_we_n <= afi_we_n_c1m;
		end else begin
			afi_address <= '0;
			afi_bank <= '0;
			afi_cs_n <= '1;
			afi_ras_n <= '1;
			afi_cas_n <= '1;
			afi_we_n <= '1;
		end
	end

	//USER Address and Command issue logic
	
	always_comb begin
		afi_address_c0 = '0;
		afi_bank_c0 = '0;
		afi_cs_n_c0_tmp = 1;
		afi_ras_n_c0 = '1;
		afi_cas_n_c0 = '1;
		afi_we_n_c0 = '1;

		afi_address_c1 = '0;
		afi_bank_c1 = '0;
		afi_cs_n_c1_tmp = 1;
		afi_ras_n_c1 = '1;
		afi_cas_n_c1 = '1;
		afi_we_n_c1 = '1;

		case (cmd_op_afi)
		RWC_CMD_WRITE_MREG0: begin
			afi_address_c0[12:0] = creg_mreg0_afi;
			afi_bank_c0[0] = 0;
			afi_bank_c0[1] = 0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '0;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_WRITE_MREG1: begin
			afi_address_c0[12:0] = creg_mreg1_afi;
			afi_bank_c0[0] = 1;
			afi_bank_c0[1] = 0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '0;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_WRITE_MREG2: begin
			afi_address_c0[12:0] = creg_mreg2_afi;
			afi_bank_c0[0] = 0;
			afi_bank_c0[1] = 1;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '0;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_WRITE_MREG3: begin
			afi_address_c0[12:0] = creg_mreg3_afi;
			afi_bank_c0[0] = 1;
			afi_bank_c0[1] = 1;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '0;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_WRITE_B0: begin
			afi_address_c0 = '0;
			afi_address_c0[3] = cmd_op_col_afi;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_WRITE_B1: begin
			afi_address_c0 = '0;
			afi_address_c0[3] = cmd_op_col_afi;
			afi_bank_c0 = '1;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_READ_B0: begin
			afi_address_c0 = '0;
			afi_address_c0[3] = cmd_op_col_afi;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '1;
		end
		RWC_CMD_READ_B1: begin
			afi_address_c0 = '0;
			afi_address_c0[3] = cmd_op_col_afi;
			afi_bank_c0 = '1;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '1;
		end
		RWC_CMD_WRITE_C0: begin
			afi_address_c0 = '0;
			afi_address_c0[3] = cmd_op_col_afi;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;

			afi_address_c1 = '0;
			afi_address_c1[3] = cmd_op_col_afi;
			afi_bank_c1 = '0;
			afi_cs_n_c1_tmp = 0;
			afi_ras_n_c1 = '1;
			afi_cas_n_c1 = '0;
			afi_we_n_c1 = '0;
		end
		RWC_CMD_WRITE_C1: begin
			afi_address_c0 = '0;
			afi_address_c0[3] = cmd_op_col_afi;
			afi_bank_c0 = '1;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;

			afi_address_c1 = '0;
			afi_address_c1[3] = cmd_op_col_afi;
			afi_bank_c1 = '1;
			afi_cs_n_c1_tmp = 0;
			afi_ras_n_c1 = '1;
			afi_cas_n_c1 = '0;
			afi_we_n_c1 = '0;
		end
		RWC_CMD_READ_B2B: begin
			afi_address_c0 = '0;
			afi_address_c0[3] = cmd_op_col_afi;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '1;

			afi_address_c1 = '0;
			afi_address_c1[3] = cmd_op_col_afi;
			afi_bank_c1 = '1;
			afi_cs_n_c1_tmp = 0;
			afi_ras_n_c1 = '1;
			afi_cas_n_c1 = '0;
			afi_we_n_c1 = '1;
		end
		RWC_CMD_WRITE_B2B: begin
			afi_address_c0 = '0;
			afi_address_c0[3] = cmd_op_col_afi;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '0;

			afi_address_c1 = '0;
			afi_address_c1[3] = cmd_op_col_afi;
			afi_bank_c1 = '1;
			afi_cs_n_c1_tmp = 0;
			afi_ras_n_c1 = '1;
			afi_cas_n_c1 = '0;
			afi_we_n_c1 = '0;
		end
		RWC_CMD_RUN_ZQCS: begin
			afi_address_c0 = '0;
			afi_address_c0[10] = 0;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '1;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_RUN_ZQCL: begin
			afi_address_c0 = '0;
			afi_address_c0[10] = 1;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '1;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_ACTIVATE0: begin
			afi_address_c0 = '0;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '0;
			afi_cas_n_c0 = '1;
			afi_we_n_c0 = '1;
		end
		RWC_CMD_ACTIVATE1: begin
			afi_address_c0 = '0;
			afi_bank_c0 = '1;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '0;
			afi_cas_n_c0 = '1;
			afi_we_n_c0 = '1;
		end
		RWC_CMD_PRECHARGE: begin
			afi_address_c0 = '0;
			afi_address_c0[10] = 1;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '0;
			afi_cas_n_c0 = '1;
			afi_we_n_c0 = '0;
		end
		RWC_CMD_REFRESH: begin
			afi_address_c0 = '0;
			afi_bank_c0 = '0;
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '0;
			afi_cas_n_c0 = '0;
			afi_we_n_c0 = '1;
		end
		RWC_CMD_NOP: begin
			afi_cs_n_c0_tmp = 0;
			afi_ras_n_c0 = '1;
			afi_cas_n_c0 = '1;
			afi_we_n_c0 = '1;
		end
		default : begin end
		endcase

		//USER mirror addressing on all odd ranks 
		
		if (MEM_MIRROR_ADDRESSING[creg_rank_afi_mem_mirror_addressing]) begin
			afi_address_c0[4:3] = {afi_address_c0[3], afi_address_c0[4]};
			afi_address_c0[6:5] = {afi_address_c0[5], afi_address_c0[6]};
			afi_address_c0[8:7] = {afi_address_c0[7], afi_address_c0[8]};
			afi_bank_c0[1:0] = {afi_bank_c0[0], afi_bank_c0[1]};

			afi_address_c1[4:3] = {afi_address_c1[3], afi_address_c1[4]};
			afi_address_c1[6:5] = {afi_address_c1[5], afi_address_c1[6]};
			afi_address_c1[8:7] = {afi_address_c1[7], afi_address_c1[8]};
			afi_bank_c1[1:0] = {afi_bank_c1[0], afi_bank_c1[1]};
		end
		
		//USER decode rank into chip select
		
		afi_cs_n_c0 = '1;
		afi_cs_n_c1 = '1;
		
		if (afi_cs_n_c0_tmp == 0)
			if (creg_rank_afi == 4'b1111)
				afi_cs_n_c0 = '0;
			else
				afi_cs_n_c0[creg_rank_afi] = 0;
			
		if (afi_cs_n_c1_tmp == 0)
			if (creg_rank_afi == 4'b1111)
				afi_cs_n_c1 = '0;
			else
				afi_cs_n_c1[creg_rank_afi] = 0;

			//USER Using 2T timing for everything but chip-select in half-rate
			
			afi_address_c0m = {afi_address_c0, afi_address_c0};
			afi_bank_c0m = {afi_bank_c0, afi_bank_c0};
			afi_ras_n_c0m = {afi_ras_n_c0, afi_ras_n_c0};
			afi_cas_n_c0m = {afi_cas_n_c0, afi_cas_n_c0};
			afi_we_n_c0m = {afi_we_n_c0, afi_we_n_c0};

			afi_address_c1m = {afi_address_c1, afi_address_c1};
			afi_bank_c1m = {afi_bank_c1, afi_bank_c1};
			afi_ras_n_c1m = {afi_ras_n_c1, afi_ras_n_c1};
			afi_cas_n_c1m = {afi_cas_n_c1, afi_cas_n_c1};
			afi_we_n_c1m = {afi_we_n_c1, afi_we_n_c1};

			if (creg_issue_hi_afi) begin
				afi_cs_n_c0m = {afi_cs_n_c0, {(MEM_CHIP_SELECT_WIDTH){1'b1}}};
				afi_cs_n_c1m = {afi_cs_n_c1, {(MEM_CHIP_SELECT_WIDTH){1'b1}}};
			end else begin
				afi_cs_n_c0m = {{(MEM_CHIP_SELECT_WIDTH){1'b1}}, afi_cs_n_c0};
				afi_cs_n_c1m = {{(MEM_CHIP_SELECT_WIDTH){1'b1}}, afi_cs_n_c1};
			end
	end
	
	//USER State update
	always_ff @ (posedge afi_clk or negedge afi_reset_n) begin
		if (afi_reset_n == 0) begin
			curr_state <= STATE_RWC_IDLE;
		end else begin
			curr_state <= next_state;
		end
	end
	
	//USER State transitions and outputs
	
	always_comb begin
		//USER state defaults;
		
		next_state = curr_state;
		afi_timer_reset = 0;
		cmd_done_afi = 0;
		
		case (curr_state)
		STATE_RWC_IDLE: begin
			if (cmd_exec_afi) begin
				next_state = STATE_RWC_LOAD;
			end
		end
		STATE_RWC_LOAD: begin
			afi_timer_reset = 1;
			next_state = STATE_RWC_WAIT;
		end
		STATE_RWC_WAIT: begin
			if (addrcmd_done) begin
				next_state = STATE_RWC_DONE;
			end
		end
		STATE_RWC_DONE: begin
			cmd_done_afi = 1;
			
			if (cmd_exec_afi == 0) begin
				next_state = STATE_RWC_IDLE;
			end
		end
		default : begin end
		endcase
	end
endmodule
