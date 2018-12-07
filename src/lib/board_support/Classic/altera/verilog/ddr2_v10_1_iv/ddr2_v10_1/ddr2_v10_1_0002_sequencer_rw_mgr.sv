//USER ******
//USER rw_mgr
//USER ******
//USER
//USER Read Write Manager
//USER
//USER General Description
//USER -------------------
//USER
//USER This component implements a Read/Write interface between the Avalon bus
//USER and the AFI. Its main tasks are:
//USER    - Writing calibration patterns to the memory device through 
//USER      the AFI interface
//USER    - Reading calibration patterns from the memory device through
//USER      the AFI interface
//USER
//USER Only simple the read/write operations required by the calibration
//USER algorithm are supported.
//USER
//USER To enable read/write access to the memory device in consecutive clock
//USER cycles, the calibration patterns to and from the Avalon bus are
//USER temporarily stored in local registers.
//USER
//USER Architecture
//USER ------------
//USER
//USER The Read Write Manager is organized as follows
//USER    - Avalon Interface: it's a Memory-Mapped interface to the Avalon
//USER      Bus.
//USER    - Read Write Core: it's the components that implements the simple
//USER      controller to manage read/write operations through the AFI.
//USER      See description in rw_mgr_core.sv for more details
//USER
//USER Address Space
//USER -------------
//USER
//USER The address space is divided in 4 identical portions.
//USER The 2 most signigicant bits select one of the internal component.
//USER The rest of the address word (whose size depend on parameterization)
//USER is used to access a specific location of the selected component.
//USER
//USER 00 - Instruction/Configuration Address.
//USER      Data written to this portion of the address space is interpreted
//USER		as a command and forwarded to the rw_mgr_core for execution.
//USER		Reads sent to this address space return configuration parameters.
//USER
//USER 01 - Register File.
//USER      Register files can be accessed in both read and write mode. The
//USER      number of 32-bit registers depends on parameterization.
//USER      Writes outside the valid range will be ignored. Reads outside
//USER      the valid range will return unpredictable values.
//USER
//USER 10 - Core State Read.
//USER		Read the contents of various internal R/W manager state registers.
//USER

module ddr2_v10_1_0002_sequencer_rw_mgr (
	// Avalon Interface
	avl_clk,
	avl_reset_n,
	avl_address,
	avl_write,
	avl_writedata,
	avl_read,
	avl_readdata,
	avl_waitrequest,

	// AFI Interface
	afi_clk,
	afi_reset_n,
	afi_address,



	afi_bank,
	afi_cs_n,
	afi_cke,
	afi_odt,
	afi_ras_n,
	afi_cas_n,
	afi_we_n,
	afi_dqs_en,


	afi_wdata,
	afi_wdata_valid,
	afi_dm,
	afi_rdata_en,
	afi_rdata,
	afi_rdata_valid
);

	parameter AVL_DATA_WIDTH 			= 32;
	parameter AVL_ADDRESS_WIDTH			= 16;
	
	//USER common memory parameters

	parameter AFI_ADDRESS_WIDTH			= "";
	parameter AFI_CONTROL_WIDTH			= "";
	parameter AFI_DATA_WIDTH			= "";
	parameter AFI_DATA_MASK_WIDTH		= "";
	parameter AFI_DQS_WIDTH				= "";
	parameter MEM_IF_READ_DQS_WIDTH		= "";
	parameter MEM_IF_WRITE_DQS_WIDTH	= "";

	parameter MEM_NUMBER_OF_RANKS = "";
	parameter MEM_MIRROR_ADDRESSING = "";

	//USER memory specific parameters

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
	parameter MEM_CHIP_SELECT_WIDTH = "";
	parameter RDIMM = "";
	parameter RP_COUNT_WIDTH = "";
	parameter RFC_COUNT_WIDTH = "";
	parameter OIT_COUNT_WIDTH = "";
	parameter MR1_DQS = "";
	parameter MR1_RDQS = "";
	parameter MR2_SRT = "";

	input avl_clk;
	input avl_reset_n;
	input [AVL_ADDRESS_WIDTH-1:0] avl_address;
	input avl_write;
	input [AVL_DATA_WIDTH-1:0] avl_writedata;
	input avl_read;
	output [AVL_DATA_WIDTH-1:0] avl_readdata;
	output avl_waitrequest;

	input afi_clk;
	input afi_reset_n;
	output [AFI_ADDRESS_WIDTH - 1:0] afi_address;
	
	
	
	output [AFI_BANK_WIDTH - 1:0] afi_bank;
	output [AFI_CHIP_SELECT_WIDTH - 1:0] afi_cs_n;
	output [AFI_CLK_EN_WIDTH - 1:0] afi_cke;
	output [AFI_ODT_WIDTH - 1:0] afi_odt;
	output [AFI_CONTROL_WIDTH - 1:0] afi_ras_n;
	output [AFI_CONTROL_WIDTH - 1:0] afi_cas_n;
	output [AFI_CONTROL_WIDTH - 1:0] afi_we_n;
	output [AFI_DQS_WIDTH - 1:0] afi_dqs_en;


	output [AFI_DATA_WIDTH - 1:0] afi_wdata;
	output [AFI_DQS_WIDTH - 1:0] afi_wdata_valid;
	output [AFI_DATA_MASK_WIDTH - 1:0] afi_dm;
	output afi_rdata_en;
	input [AFI_DATA_WIDTH - 1:0] afi_rdata;
	input afi_rdata_valid;

	wire [AVL_DATA_WIDTH-1:0] avl_readdata_g;
	reg [AVL_DATA_WIDTH-1:0] avl_readdata;
	reg avl_waitrequest;
	
	logic config_reg_wr;
	logic config_reg_rd;
	
	logic cmd_wr;
	logic cmd_exec;
	wire cmd_done;
	
	logic param_rd;
	
	logic cstate_rd;
	
	logic avl_rd;
	
	typedef enum int unsigned {
		STATE_RW_IDLE,
		STATE_RW_EXEC,
		STATE_RW_DONE
	} STATE_RW_T;

	STATE_RW_T state;

	always_comb begin
		cmd_wr = 
			~avl_address[AVL_ADDRESS_WIDTH - 1] &
			 avl_address[AVL_ADDRESS_WIDTH - 2] &
			~avl_address[AVL_ADDRESS_WIDTH - 3] &
			~avl_address[AVL_ADDRESS_WIDTH - 4] &
			~avl_address[AVL_ADDRESS_WIDTH - 5] &
			avl_write;

		param_rd = 
			~avl_address[AVL_ADDRESS_WIDTH - 1] &
			 avl_address[AVL_ADDRESS_WIDTH - 2] &
			~avl_address[AVL_ADDRESS_WIDTH - 3] &
			~avl_address[AVL_ADDRESS_WIDTH - 4] &
			~avl_address[AVL_ADDRESS_WIDTH - 5] &
			avl_read;
			
		config_reg_wr = 
			~avl_address[AVL_ADDRESS_WIDTH - 1] &
			 avl_address[AVL_ADDRESS_WIDTH - 2] &
			~avl_address[AVL_ADDRESS_WIDTH - 3] &
			~avl_address[AVL_ADDRESS_WIDTH - 4] &
			 avl_address[AVL_ADDRESS_WIDTH - 5] &
			avl_write;
		
		config_reg_rd = 
			~avl_address[AVL_ADDRESS_WIDTH - 1] &
			 avl_address[AVL_ADDRESS_WIDTH - 2] &
			~avl_address[AVL_ADDRESS_WIDTH - 3] &
			~avl_address[AVL_ADDRESS_WIDTH - 4] &
			 avl_address[AVL_ADDRESS_WIDTH - 5] &
			avl_read;
			
		cstate_rd = 
			~avl_address[AVL_ADDRESS_WIDTH - 1] &
			 avl_address[AVL_ADDRESS_WIDTH - 2] &
			~avl_address[AVL_ADDRESS_WIDTH - 3] &
			 avl_address[AVL_ADDRESS_WIDTH - 4] &
			~avl_address[AVL_ADDRESS_WIDTH - 5] &
			avl_read;
			
		avl_rd = param_rd | config_reg_rd | cstate_rd;
	end
	

	//USER CMD State Machine
	
	always_ff @(posedge avl_clk) begin
		if (~avl_reset_n) begin
			state <= STATE_RW_IDLE;
			cmd_exec <= 0;
		end else begin
			case (state)
			STATE_RW_IDLE:
				if (cmd_wr) begin
					state <= STATE_RW_EXEC;
					cmd_exec <= 1;
				end else if (avl_rd) begin
					state <= STATE_RW_DONE;
				end
			STATE_RW_EXEC: 
				if (cmd_done) begin
					state <= STATE_RW_DONE;
					cmd_exec <= 0;
				end
			STATE_RW_DONE: 
				if (~cmd_wr && ~avl_rd) begin
					state <= STATE_RW_IDLE;
				end
			endcase
		end
	end

	ddr2_v10_1_0002_sequencer_rw_mgr_core sequencer_rw_mgr_core_inst (
		.avl_reset_n(avl_reset_n),
		.avl_clk(avl_clk),
		.afi_clk(afi_clk),
		.afi_reset_n(afi_reset_n),
		.afi_address(afi_address),
		


		.afi_bank (afi_bank),
		.afi_cs_n (afi_cs_n),
		.afi_cke (afi_cke),
		.afi_odt (afi_odt),
		.afi_ras_n (afi_ras_n),
		.afi_cas_n (afi_cas_n),
		.afi_we_n (afi_we_n),
		.afi_dqs_en (afi_dqs_en),

		
		.afi_wdata(afi_wdata),
		.afi_wdata_valid(afi_wdata_valid),
		.afi_dm(afi_dm),
		.afi_rdata_en(afi_rdata_en),
		.afi_rdata(afi_rdata),
		.afi_rdata_valid(afi_rdata_valid),
		
		.config_reg_wr(config_reg_wr),
		.config_reg_rd(config_reg_rd),
		.cmd_exec(cmd_exec),
		.cmd_done(cmd_done),
		.param_rd(param_rd),
		.cstate_rd(cstate_rd),
		.avl_address (avl_address),
		.avl_wdata (avl_writedata),
		.avl_rdata (avl_readdata_g)
	);
	defparam sequencer_rw_mgr_core_inst.AFI_ADDRESS_WIDTH = AFI_ADDRESS_WIDTH;
	defparam sequencer_rw_mgr_core_inst.AFI_CONTROL_WIDTH = AFI_CONTROL_WIDTH;
	defparam sequencer_rw_mgr_core_inst.AFI_DATA_WIDTH = AFI_DATA_WIDTH;
	defparam sequencer_rw_mgr_core_inst.AFI_DATA_MASK_WIDTH = AFI_DATA_MASK_WIDTH;
	defparam sequencer_rw_mgr_core_inst.AFI_DQS_WIDTH = AFI_DQS_WIDTH;
	defparam sequencer_rw_mgr_core_inst.MEM_IF_READ_DQS_WIDTH	= MEM_IF_READ_DQS_WIDTH;
	defparam sequencer_rw_mgr_core_inst.MEM_IF_WRITE_DQS_WIDTH = MEM_IF_WRITE_DQS_WIDTH;

	defparam sequencer_rw_mgr_core_inst.MEM_MIRROR_ADDRESSING = MEM_MIRROR_ADDRESSING;
	


	defparam sequencer_rw_mgr_core_inst.MEM_TINIT_CK = MEM_TINIT_CK;
	defparam sequencer_rw_mgr_core_inst.MEM_TMRD_CK = MEM_TMRD_CK;
	defparam sequencer_rw_mgr_core_inst.AFI_BANK_WIDTH = AFI_BANK_WIDTH; 
	defparam sequencer_rw_mgr_core_inst.AFI_CHIP_SELECT_WIDTH = AFI_CHIP_SELECT_WIDTH;
	defparam sequencer_rw_mgr_core_inst.AFI_CLK_EN_WIDTH = AFI_CLK_EN_WIDTH; 
	defparam sequencer_rw_mgr_core_inst.AFI_ODT_WIDTH = AFI_ODT_WIDTH; 
	defparam sequencer_rw_mgr_core_inst.INIT_NOP_COUNT_WIDTH = INIT_NOP_COUNT_WIDTH;
	defparam sequencer_rw_mgr_core_inst.MRD_COUNT_WIDTH = MRD_COUNT_WIDTH;
	defparam sequencer_rw_mgr_core_inst.MR0_BL = MR0_BL;
	defparam sequencer_rw_mgr_core_inst.MR0_BT = MR0_BT;
	defparam sequencer_rw_mgr_core_inst.MR0_CAS_LATENCY = MR0_CAS_LATENCY;
	defparam sequencer_rw_mgr_core_inst.MR0_WR = MR0_WR;
	defparam sequencer_rw_mgr_core_inst.MR0_PD = MR0_PD;
	defparam sequencer_rw_mgr_core_inst.MR1_DLL = MR1_DLL;
	defparam sequencer_rw_mgr_core_inst.MR1_ODS = MR1_ODS;
	defparam sequencer_rw_mgr_core_inst.MR1_RTT = MR1_RTT;
	defparam sequencer_rw_mgr_core_inst.MR1_AL = MR1_AL;
	defparam sequencer_rw_mgr_core_inst.MR1_QOFF = MR1_QOFF;
	defparam sequencer_rw_mgr_core_inst.RDIMM = RDIMM;
	defparam sequencer_rw_mgr_core_inst.RP_COUNT_WIDTH = RP_COUNT_WIDTH;
	defparam sequencer_rw_mgr_core_inst.RFC_COUNT_WIDTH = RFC_COUNT_WIDTH;
	defparam sequencer_rw_mgr_core_inst.OIT_COUNT_WIDTH = OIT_COUNT_WIDTH;
	defparam sequencer_rw_mgr_core_inst.MR1_DQS = MR1_DQS;
	defparam sequencer_rw_mgr_core_inst.MR1_RDQS = MR1_RDQS;
	defparam sequencer_rw_mgr_core_inst.MR2_SRT = MR2_SRT;
	defparam sequencer_rw_mgr_core_inst.MEM_NUMBER_OF_RANKS = MEM_CHIP_SELECT_WIDTH;

	// wait request and execute

	always_comb
	begin
		if ((cmd_wr | avl_rd) && state != STATE_RW_DONE)
			avl_waitrequest = 1;
		else
			avl_waitrequest = 0;
			
		if (avl_rd)
			avl_readdata = avl_readdata_g;
		else
			avl_readdata = '0;
	end

endmodule
