// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

`timescale 1 ps / 1 ps

module mem_model 
    # (parameter	 MEM_CLK_EN_WIDTH = 1,
                     MEM_IF_BA_WIDTH = 2,
                     MEM_IF_ADDR_WIDTH = 12, 
                     MEM_IF_ROW_WIDTH = 12, 
                     MEM_IF_COL_WIDTH = 10, 
                     MEM_IF_CS_WIDTH = 1,
                     MEM_IF_CS_PER_RANK = 1,
                     MEM_DQS_WIDTH = 2,
                     MEM_DQ_WIDTH = 16, 
                     MEM_TRTP = 6,
                     MEM_TRCD = 11
	 )                    
     (
	mem_a,
	mem_ba,
	mem_ck,
	mem_ck_n,
	mem_cke,
	mem_cs_n,
	mem_ras_n,
	mem_cas_n,
	mem_we_n,
	mem_dm,
	mem_dq,
	mem_dqs,
	mem_dqs_n,
	mem_odt
);

input	[MEM_IF_ADDR_WIDTH - 1:0]	mem_a;
input	[MEM_IF_BA_WIDTH - 1:0]	mem_ba;
input	mem_ck;
input	mem_ck_n;
input	[MEM_CLK_EN_WIDTH - 1:0] mem_cke;
input	[MEM_IF_CS_WIDTH - 1:0] mem_cs_n;
input	mem_ras_n;
input	mem_cas_n;
input	mem_we_n;
input	[MEM_DQS_WIDTH - 1:0] mem_dm;
inout   [MEM_DQ_WIDTH - 1:0]	mem_dq;
inout   [MEM_DQS_WIDTH - 1:0]	mem_dqs;
inout   [MEM_DQS_WIDTH - 1:0]	mem_dqs_n;
input 	mem_odt;


wire	[MEM_IF_ADDR_WIDTH-1:0]	a;
wire	[MEM_IF_BA_WIDTH-1:0]	ba;
wire	cke;
wire	[MEM_IF_CS_WIDTH-1:0]	cs_n;
wire	ras_n;
wire	cas_n;
wire	we_n;
wire	odt;
wire 	ac_parity;
assign a = mem_a;
assign ba = mem_ba;
assign cke = mem_cke;
assign cs_n = mem_cs_n;
assign ras_n = mem_ras_n;
assign cas_n = mem_cas_n;
assign we_n = mem_we_n;
assign odt = mem_odt;



generate
genvar rank;
    for (rank = 0; rank < MEM_IF_CS_WIDTH/MEM_IF_CS_PER_RANK; rank = rank + 1)
    begin : rank_gen
        mem_rank_model # (
                            .MEM_IF_BA_WIDTH (MEM_IF_BA_WIDTH),
                            .MEM_IF_ADDR_WIDTH (MEM_IF_ADDR_WIDTH),
                            .MEM_IF_ROW_ADDR_WIDTH (MEM_IF_ROW_WIDTH),
                            .MEM_IF_COL_ADDR_WIDTH (MEM_IF_COL_WIDTH),
                            .MEM_DQS_WIDTH (MEM_DQS_WIDTH),
                            .MEM_DQ_WIDTH (MEM_DQ_WIDTH),
                            .MEM_CS_WIDTH (MEM_IF_CS_PER_RANK),
                            .MEM_TRTP (MEM_TRTP),
                            .MEM_TRCD (MEM_TRCD)
                        ) rank_inst (
                        .mem_a       (a),
                        .mem_ba      (ba),
                        .mem_ck      (mem_ck),
                        .mem_ck_n    (mem_ck_n),   
                        .mem_cke     (cke),
                        .mem_cs_n    (cs_n[(rank+1)*MEM_IF_CS_PER_RANK-1:rank*MEM_IF_CS_PER_RANK]),
                        .mem_ras_n   (ras_n),
                        .mem_cas_n   (cas_n),
                        .mem_we_n    (we_n),
                        .mem_dm      (mem_dm),
                        .mem_dq      (mem_dq),
                        .mem_dqs     (mem_dqs),
                        .mem_dqs_n   (mem_dqs_n),
                        .mem_odt     (odt)
                    );
    end
endgenerate

endmodule


`timescale 1 ps / 1 ps

module mem_rank_model 
    # (parameter	 MEM_IF_BA_WIDTH = 2,
                     MEM_IF_ADDR_WIDTH = 12, 
                     MEM_IF_ROW_ADDR_WIDTH = 12, 
                     MEM_IF_COL_ADDR_WIDTH = 10, 
                     MEM_DQS_WIDTH = 2,
                     MEM_DQ_WIDTH = 16,
                     MEM_CS_WIDTH = 1,
                     MEM_TRTP = 6,
                     MEM_TRCD = 11
	 )                    
     (
	mem_a,
	mem_ba,
	mem_ck,
	mem_ck_n,
	mem_cke,
	mem_cs_n,
	mem_ras_n,
	mem_cas_n,
	mem_we_n,
	mem_dm,
	mem_dq,
	mem_dqs,
	mem_dqs_n,
	mem_odt
);

localparam NUM_BANKS = 2**MEM_IF_BA_WIDTH;
localparam MEM_DQS_GROUP_SIZE = MEM_DQ_WIDTH / MEM_DQS_WIDTH;
localparam VERBOSE_MODE = 1;        
localparam DISABLE_NOP_DISPLAY = 1;
localparam CHECK_VIOLATIONS = 1;
localparam REFRESH_INTERVAL_PS = 36000000;
localparam MAX_LATENCY = 22;
localparam MAX_BURST = 8;

input	[MEM_IF_ADDR_WIDTH - 1:0]	mem_a;
input	[MEM_IF_BA_WIDTH - 1:0]	mem_ba;
input	mem_ck;
input	mem_ck_n;
input	mem_cke;
input	[MEM_CS_WIDTH - 1:0] mem_cs_n;
input	[MEM_DQS_WIDTH - 1:0] mem_dm;
input	mem_ras_n;
input	mem_cas_n;
input	mem_we_n;
inout   [MEM_DQ_WIDTH - 1:0]	mem_dq;
inout   [MEM_DQS_WIDTH - 1:0]	mem_dqs;
inout   [MEM_DQS_WIDTH - 1:0]	mem_dqs_n;
input 	mem_odt;

wire [MEM_DQS_WIDTH - 1:0] mem_dqs_shifted;
wire [MEM_DQS_WIDTH - 1:0] mem_dqs_n_shifted;

typedef enum logic[3:0] {
	OPCODE_PRECHARGE = 4'b0010,
	OPCODE_ACTIVATE = 4'b0011,
	OPCODE_WRITE = 4'b0100,
	OPCODE_READ = 4'b0101,
	OPCODE_MRS = 4'b0000,
	OPCODE_REFRESH = 4'b0001,
	OPCODE_DES = 4'b1xxx,
	OPCODE_NOP = 4'b0111
} OPCODE_TYPE;

typedef enum {
	DDR_BURST_TYPE_BL8,
	DDR_BURST_TYPE_OTF,
	DDR_BURST_TYPE_BL4
} DDR_BURST_TYPE;

typedef enum {
	DDR_AL_TYPE_ZERO,
	DDR_AL_TYPE_CL_MINUS_1,
	DDR_AL_TYPE_CL_MINUS_2
} DDR_AL_TYPE;

DDR_BURST_TYPE burst_type;
int cas_latency;
int cas_additive_latency;
int cas_write_latency;
DDR_AL_TYPE al_type;

int tRTP_cycles = MEM_TRTP;
int tRCD_cycles = MEM_TRCD;

int clock_cycle;

reg clock_stable;

typedef struct {
	bit [MEM_IF_ROW_ADDR_WIDTH - 1:0] opened_row;
	time last_ref_time;
	int last_ref_cycle;
	int last_activate_cycle;
	int last_precharge_cycle;
	int last_write_cmd_cycle;
	int last_write_access_cycle;
	int last_read_cmd_cycle;
	int last_read_access_cycle;
} bank_struct;

bit [MEM_DQ_WIDTH - 1:0] mem_data[*];

bank_struct banks [NUM_BANKS - 1:0];

typedef enum {
	DDR_CMD_TYPE_PRECHARGE,
	DDR_CMD_TYPE_ACTIVATE,
	DDR_CMD_TYPE_WRITE,
	DDR_CMD_TYPE_READ,
	DDR_CMD_TYPE_MRS,
	DDR_CMD_TYPE_REFRESH,
	DDR_CMD_TYPE_NOP,
	DDR_CMD_TYPE_DES,
	DDR_CMD_TYPE_ERROR
} DDR_CMD_TYPE;

typedef struct {
	DDR_CMD_TYPE cmd_type;
	int word_count;
	int burst_length;
	bit [MEM_IF_ADDR_WIDTH - 1:0] address;
	bit [MEM_IF_BA_WIDTH - 1:0] bank;
	bit [3:0] opcode;
} command_struct;



DDR_CMD_TYPE write_command_queue[$];
int write_word_count_queue[$];
int write_burst_length_queue[$];
bit [MEM_IF_ADDR_WIDTH - 1:0] write_address_queue[$];
bit [MEM_IF_BA_WIDTH - 1:0] write_bank_queue[$];

DDR_CMD_TYPE read_command_queue[$];
int read_word_count_queue[$];
int read_burst_length_queue[$];
bit [MEM_IF_ADDR_WIDTH - 1:0] read_address_queue[$];
bit [MEM_IF_BA_WIDTH - 1:0] read_bank_queue[$];

DDR_CMD_TYPE precharge_command_queue[$];
bit [MEM_IF_BA_WIDTH - 1:0] precharge_bank_queue[$];

DDR_CMD_TYPE activate_command_queue[$];
bit [MEM_IF_BA_WIDTH-1:0] activate_bank_queue[$];
bit [MEM_IF_ADDR_WIDTH-1:0] activate_row_queue[$];

command_struct active_command;
command_struct new_command;
command_struct precharge_command;
command_struct activate_command;

bit [2 * MAX_LATENCY + 1:0] read_command_pipeline;
bit [2 * MAX_LATENCY + 1:0] write_command_pipeline;
bit [2 * MAX_LATENCY + 1:0] precharge_command_pipeline;
bit [2 * MAX_LATENCY + 1:0] activate_command_pipeline;

reg [MEM_DQ_WIDTH - 1:0]	mem_dq_int;
reg [MEM_DQ_WIDTH - 1:0]	mem_dq_captured;
reg [MEM_DQS_WIDTH - 1:0]	mem_dm_captured;
bit mem_dq_en;
bit mem_dqs_en;
bit mem_dqs_preamble;
wire [MEM_DQ_WIDTH - 1:0] full_mask;

integer file, r;
reg [MEM_DQ_WIDTH - 1:0] data;
reg [MEM_IF_ADDR_WIDTH - 1:0] addr;
/*
initial
    begin : file_block
    file = $fopen("IPTCL_MODULENAME.dat", "r");
    if (!file) begin
        $display("Can't find IPTCL_MODULENAME.dat.");
        disable file_block;
    end
    
    while (!$feof(file))
        begin
        r = $fscanf(file, "@%h %h \n", addr, data);
        mem_data[addr] = data;
        $display("Initializing the memory. addr = %h, mem_data = %h", addr, mem_data[addr]);
        end // while not EOF

    $fclose(file);
    end // initial
*/     
function automatic int min;
	input int a;
	input int b;
	int result = (a < b) ? a : b;
	return result;
endfunction

task automatic initialize_db;
	while (write_command_queue.size() > 0)
		write_command_queue.delete(0);
	while (write_word_count_queue.size() > 0)
		write_word_count_queue.delete(0);
	while (write_burst_length_queue.size() > 0)
		write_burst_length_queue.delete(0);
	while (write_address_queue.size() > 0)
		write_address_queue.delete(0);
	while (write_bank_queue.size() > 0)
		write_bank_queue.delete(0);

	while (read_command_queue.size() > 0)
		read_command_queue.delete(0);
	while (read_word_count_queue.size() > 0)
		read_word_count_queue.delete(0);
	while (read_burst_length_queue.size() > 0)
		read_burst_length_queue.delete(0);
	while (read_address_queue.size() > 0)
		read_address_queue.delete(0);
	while (read_bank_queue.size() > 0)
		read_bank_queue.delete(0);

	while (precharge_command_queue.size() > 0)
		precharge_command_queue.delete(0);
	while (precharge_bank_queue.size() > 0)
		precharge_bank_queue.delete(0);

    while (activate_command_queue.size() > 0)
        activate_command_queue.delete(0);
    while (activate_bank_queue.size() > 0)
        activate_bank_queue.delete(0);
    while (activate_row_queue.size() > 0)
        activate_row_queue.delete(0);

	mem_data.delete();
endtask

task automatic set_cas_latency (input bit [2:0] code);
    case(code)
            3'b011 : cas_latency = 3;
            3'b100 : cas_latency = 4;
            3'b101 : cas_latency = 5;
            3'b110 : cas_latency = 6;
            3'b111 : cas_latency = 7;
        default: begin
            $display("Error: Use of reserved CAS latency code : %b", code);
            $stop(1);
        end
    endcase
    if (VERBOSE_MODE) begin
        $display("   CAS LATENCY set to : %d", cas_latency);
    end
    
        set_cas_write_latency();
endtask

    task automatic set_additive_latency (input bit [2:0] code);
        case(code)
            3'b000 : cas_additive_latency = 0;
            3'b001 : cas_additive_latency = 1;
            3'b010 : cas_additive_latency = 2;
            3'b011 : cas_additive_latency = 3;
            3'b100 : cas_additive_latency = 4;
            3'b101 : cas_additive_latency = 5;
            3'b110 : cas_additive_latency = 6;
            default: begin
                $display("Error: Use of reserved CAS additive latency code : %b", code);
                $stop(1);
            end
        endcase
        if (VERBOSE_MODE)
            $display("   CAS ADDITIVE LATENCY set to : %d", cas_additive_latency);
    endtask

function automatic int get_read_latency;
	int read_latency = cas_latency + cas_additive_latency; 
	return read_latency;
endfunction

function automatic int get_write_latency;
	int write_latency = cas_write_latency + cas_additive_latency;
	return write_latency;
endfunction

function automatic int get_precharge_latency;

	int precharge_latency = tRTP_cycles + cas_additive_latency;
	return precharge_latency;
endfunction

    task automatic set_cas_write_latency;
        cas_write_latency = cas_latency - 1;
        $display("   CAS WRITE LATENCY set to : %d", cas_write_latency);
    endtask

task automatic reset_dll (input bit code);
	if(code == 1'b1) begin
		if (VERBOSE_MODE)
			$display("   Resetting DLL");
	end
endtask

    task automatic set_burst_type (input bit [2:0] burst_mode);
        case (burst_mode)
            3'b010 : begin
                    if (VERBOSE_MODE)
                        $display("   Setting burst length Fixed BL4");
                    burst_type = DDR_BURST_TYPE_BL4;
                    end
            3'b011 : begin
                    if (VERBOSE_MODE)
                        $display("   Setting burst length Fixed BL8");
                    burst_type = DDR_BURST_TYPE_BL8;
                    end
            default : begin
                $display("ERROR: Invalid burst type mode %0d specified!", burst_mode);
                $finish(1);
                end
        endcase
    endtask

task automatic cmd_nop;
	if (VERBOSE_MODE && !DISABLE_NOP_DISPLAY)
		$display("[%0t] %m: NOP Command", $time);
			
endtask

task automatic cmd_program_rdimm;
	bit [3:0] rdimm_addr = {new_command.bank[2], new_command.address[2], new_command.address[1], new_command.address[0]};
	bit [3:0] rdimm_d = {new_command.bank[1], new_command.bank[0], new_command.address[4], new_command.address[3]};
	$display("[%0t] %m: DDR3 RDIMM RC%0d => %0H", $time, rdimm_addr, rdimm_d);
endtask

task automatic cmd_des;
	if (VERBOSE_MODE && !DISABLE_NOP_DISPLAY)
		$display("[%0t] %m: DES Command", $time);
endtask

task automatic cmd_unknown;
	if (VERBOSE_MODE)
		$display("[%0t] %m: WARNING: Unknown Command (OPCODE %b). Command ignored.", $time, new_command.opcode);
endtask

task automatic cmd_set_activate;
	int activate_latency = min(get_read_latency(), get_write_latency()) + 1;

	if (VERBOSE_MODE)
		$display("[%0t] %m: ACTIVATE (queue) - BANK [ %0h ] - ROW [ %0h ]", $time, new_command.bank, new_command.address);
	activate_command_queue.push_back(DDR_CMD_TYPE_ACTIVATE);
	activate_bank_queue.push_back(new_command.bank);
	activate_row_queue.push_back(new_command.address);

	activate_command_pipeline[ 2 * activate_latency ] = 1;
    banks[new_command.bank].last_activate_cycle = clock_cycle;
endtask

task automatic cmd_activate(bit [MEM_IF_BA_WIDTH-1:0] bank, bit [MEM_IF_ADDR_WIDTH-1:0] address);
	if (VERBOSE_MODE)
		$display("[%0t] %m: ACTIVATE (execute) - BANK [ %0h ] - ROW [ %0h ]", $time, bank, address);


	banks[bank].opened_row = address;
endtask

task automatic cmd_precharge(bit [2:0] bank, bit all_banks);
	if (VERBOSE_MODE)
		if(all_banks)
			$display("[%0t] %m: PRECHARGE - ALL BANKS", $time);
		else
			$display("[%0t] %m: PRECHARGE - BANK [ %d ]", $time, bank);

	banks[mem_ba].last_precharge_cycle = clock_cycle;
endtask

task automatic cmd_mrs;
	if (VERBOSE_MODE)
		$display("[%0t] %m: MRS Command - MRS [ %d ] -> %0h", $time, new_command.bank, new_command.address);

	case(new_command.bank)
		3'b000 : begin
			if (VERBOSE_MODE)
				$display("   MRS - 0");

                set_burst_type(new_command.address[2:0]);
			set_cas_latency(new_command.address[6:4]);

			reset_dll(new_command.address[8]);
		end

		3'b001 : begin
			if (VERBOSE_MODE)
				$display("   MRS - 1");
                set_additive_latency(new_command.address[5:3]);
                case(new_command.address[9:7])
                    3'b000 : begin
                        if(VERBOSE_MODE)
                            $display("   OCD EXIT");
                    end
                    3'b111 : begin
                        if(VERBOSE_MODE)
                            $display("   ENABLING OCD DEFAULTS");
                    end
                    default : begin
                        $display("Error: MR 1 - Invalid OCD setting: %b", new_command.address[5:3]);
                    end
                endcase
		end

		3'b010 : begin
                if (VERBOSE_MODE)
                    $display("   MRS - 2: not supported");
		end

		3'b011 : begin
			if (VERBOSE_MODE)
				$display("   MRS - 3: not supported");
		end

		default : begin
			$display("Error: MRS Invalid Bank Address: %d", new_command.bank);

			$stop(1);
		end
	endcase
endtask

task automatic cmd_refresh;
	if (VERBOSE_MODE)
		$display("[%0t] %m: REFRESH Command", $time);


endtask

task automatic cmd_read;
	int read_latency = get_read_latency();
	int precharge_latency = get_precharge_latency();

	if (VERBOSE_MODE)
		if(mem_a[10])
			$display("[%0t] %m: READ with AP (BL%d) - BANK [ %d ] - COL [ %0h ]", $time, new_command.burst_length, new_command.bank, new_command.address);
		else
			$display("[%0t] %m: READ (BL%d) - BANK [ %d ] - COL [ %0h ]", $time, new_command.burst_length, new_command.bank, new_command.address);
	
	new_command.word_count = 0;
	
	read_command_queue.push_back(new_command.cmd_type);
	read_word_count_queue.push_back(new_command.word_count);
	read_burst_length_queue.push_back(new_command.burst_length);
	read_address_queue.push_back(new_command.address);
	read_bank_queue.push_back(new_command.bank);

	read_command_pipeline[ 2 * read_latency ] = 1;
	
	banks[mem_ba].last_read_cmd_cycle = clock_cycle;

	refresh_bank(mem_ba);
		
	if(mem_a[10]) begin
		precharge_command_queue.push_back(DDR_CMD_TYPE_PRECHARGE);
		precharge_bank_queue.push_back(new_command.bank);

		precharge_command_pipeline[ 2 * precharge_latency ] = 1;
	end
endtask

task automatic cmd_write;
	int write_latency = get_write_latency();

	if (VERBOSE_MODE)
		$display("[%0t] %m: WRITE (BL%d) - BANK [ %d ] - COL [ %0h ]", $time, new_command.burst_length, new_command.bank, new_command.address);
	
	new_command.word_count = 0;
	
	write_command_queue.push_back(new_command.cmd_type);
	write_word_count_queue.push_back(new_command.word_count);
	write_burst_length_queue.push_back(new_command.burst_length);
	write_address_queue.push_back(new_command.address);
	write_bank_queue.push_back(new_command.bank);

	write_command_pipeline[2 * write_latency] = 1'b1;
	
	banks[mem_ba].last_write_cmd_cycle = clock_cycle;
endtask

task automatic refresh_command;
	if (VERBOSE_MODE)
		$display("[%0t] %m: Refresh Command to bank %0h", $time, mem_ba);
	refresh_bank(mem_ba);
endtask

task automatic refresh_bank(input int bank_num);
	if (VERBOSE_MODE)
		$display("[%0t] %m: Refreshing bank %0h", $time, bank_num);
	banks[bank_num].last_ref_time = $time;
	banks[bank_num].last_ref_cycle = clock_cycle;
endtask

task automatic init_banks;
	int i;
	for (i = 0; i < NUM_BANKS; i++) begin
		if (VERBOSE_MODE)
			$display("[%0t] %m: Initializing bank %0d", $time, i);
		banks[i].opened_row = '0;
		banks[i].last_ref_time = 0;
		banks[i].last_ref_cycle = 0;
		banks[i].last_activate_cycle = 0;
		banks[i].last_precharge_cycle = 0;
		banks[i].last_read_cmd_cycle = 0;
		banks[i].last_read_access_cycle = 0;
		banks[i].last_write_cmd_cycle = 0;
		banks[i].last_write_access_cycle = 0;
	end
		
endtask

task automatic check_violations;
	int i;

	/* **** *
	 ddr2_v10_1_aiigx_hr_write_io tRCD *
	 ddr2_v10_1_aiigx_hr_write_io **** */

	if(new_command.cmd_type == DDR_CMD_TYPE_READ) begin
		if(banks[new_command.bank].last_activate_cycle > banks[new_command.bank].last_read_cmd_cycle - tRCD_cycles) begin
			$display("[%0t] %m: ERROR: tRCD violation (READ) on bank %d @ cycle %d", $time, i, clock_cycle);
			$display("tRCD = %d", tRCD_cycles);
			$display("Last ACTIVATE @ %d", banks[new_command.bank].last_activate_cycle);
			$display("Last READ CMD @ %d", banks[new_command.bank].last_read_cmd_cycle);
		end
	end
	if(new_command.cmd_type == DDR_CMD_TYPE_WRITE) begin
		if(banks[new_command.bank].last_activate_cycle > banks[new_command.bank].last_write_cmd_cycle - tRCD_cycles) begin
			$display("[%0t] %m: ERROR: tRCD violation (WRITE) on bank %d @ cycle %d", $time, i, clock_cycle);
			$display("tRCD = %d", tRCD_cycles);
			$display("Last ACTIVATE @ %d", banks[new_command.bank].last_activate_cycle);
			$display("Last WRITE CMD @ %d", banks[new_command.bank].last_write_cmd_cycle);
		end
	end
endtask

task write_memory(
	input command_struct write_command,
	input [MEM_DQ_WIDTH - 1:0] write_data,
	input [MEM_DQ_WIDTH - 1:0] data_mask);

	bit [MEM_IF_BA_WIDTH - 1:0] bank_address;
	bit [MEM_IF_ROW_ADDR_WIDTH - 1:0] row_address;
	bit [MEM_IF_COL_ADDR_WIDTH - 1:0] col_address;
	bit [MEM_IF_BA_WIDTH + MEM_IF_ROW_ADDR_WIDTH + MEM_IF_COL_ADDR_WIDTH - 1 : 0] address;
	bit [MEM_DQ_WIDTH - 1:0] masked_data;

	integer i;

	bank_address = write_command.bank;

	row_address = banks[bank_address].opened_row;
	col_address = write_command.address;
	address = {bank_address, row_address, col_address} + write_command.word_count;

	for(i = 0; i < MEM_DQ_WIDTH; i = i + 1) begin
		if(data_mask[i]) masked_data[i] = mem_data[address][i];
		else masked_data[i] = write_data[i];
	end

	if (VERBOSE_MODE)
		$display("[%0t] %m: Writing data %0h @ %0h (B, R, C) - (%0h , %0h, %0h ) burst %0d", 
			$time, write_data, address, bank_address, row_address, col_address, write_command.word_count);
	if(data_mask > 0 ) 
		if (VERBOSE_MODE)
			$display("[%0t] %m: MASK = %0h - Masked Data: %0h ", $time, data_mask, masked_data);

	mem_data[address] = masked_data;
	banks[bank_address].last_write_access_cycle = clock_cycle;

endtask

task read_memory(
	input command_struct write_command,
	output [MEM_DQ_WIDTH - 1:0] read_data);

	bit [MEM_IF_BA_WIDTH - 1:0] bank_address;
	bit [MEM_IF_ROW_ADDR_WIDTH - 1:0] row_address;
	bit [MEM_IF_COL_ADDR_WIDTH - 1:0] col_address;
	bit [MEM_IF_BA_WIDTH + MEM_IF_ROW_ADDR_WIDTH + MEM_IF_COL_ADDR_WIDTH - 1 : 0] address;

	bank_address = write_command.bank;

	row_address = banks[bank_address].opened_row;
	col_address = write_command.address;
	address = {bank_address, row_address, col_address} + write_command.word_count;

	if (mem_data.exists(address)) begin
		read_data = mem_data[address];
		if (VERBOSE_MODE)
			$display("[%0t] %m: Reading data %0h @ %0h (B, R, C) - (%0h , %0h, %0h ) burst %0d", 
				$time, read_data, address, bank_address, row_address, col_address, write_command.word_count);
	end
	else begin
		$display("[%0t] %m: WARNING: Attempting to read from invalid address @ %0h (B, R, C) - (%0h , %0h, %0h ) burst %0d", 
			$time, address, bank_address, row_address, col_address, write_command.word_count);
		read_data = 'X;
	end

	banks[bank_address].last_read_access_cycle = clock_cycle;
endtask


logic mem_ck_diff;
always @(posedge mem_ck) #8 mem_ck_diff <= mem_ck;
always @(posedge mem_ck_n) #8 mem_ck_diff <= ~mem_ck_n;

initial begin
	int i;
	
	$display("Altera Generic DDR3 Memory Model");
	if (VERBOSE_MODE) begin
		$display("[%0t] %m: Max refresh interval of %0d ps", $time, REFRESH_INTERVAL_PS);
	end
	
	clock_cycle = 0;
	clock_stable = 1'b0;
	
	initialize_db;
    set_burst_type(3'b010);

	init_banks();
	
	mem_data.delete();
	
	active_command.cmd_type <= DDR_CMD_TYPE_NOP;
	
	for (i = 0; i < 2 * MAX_LATENCY; i++) begin
		read_command_pipeline[i] = 0;
		write_command_pipeline[i] = 0;
	end
end

always @ (posedge mem_ck) begin
	clock_cycle <= clock_cycle + 1;
	if (clock_cycle == 4) clock_stable <= 1'b1;
end

wire [MEM_IF_COL_ADDR_WIDTH-1:0] col_addr;
generate
	if(MEM_IF_COL_ADDR_WIDTH <= 10) begin : col_addr_gen1
		assign col_addr = mem_a[9:0];
	end
	else begin : col_addr_gen2
		assign col_addr = {mem_a[MEM_IF_COL_ADDR_WIDTH:11],mem_a[9:0]};
	end
endgenerate

always @ (posedge mem_ck_diff or negedge mem_ck_diff) begin
	
	read_command_pipeline = read_command_pipeline >> 1;
	write_command_pipeline = write_command_pipeline >> 1;
    activate_command_pipeline = activate_command_pipeline >> 1;
    
        if(mem_ck_diff && clock_stable) begin
            new_command.bank = mem_ba;
            new_command.word_count = 0;
            new_command.opcode = {mem_cs_n, mem_ras_n, mem_cas_n, mem_we_n};
    
            case (burst_type)
                DDR_BURST_TYPE_BL8 : new_command.burst_length = 8;
                DDR_BURST_TYPE_BL4 : new_command.burst_length = 4;
            endcase
                
            casex (new_command.opcode)
                OPCODE_PRECHARGE : new_command.cmd_type = DDR_CMD_TYPE_PRECHARGE;
                OPCODE_ACTIVATE : new_command.cmd_type = DDR_CMD_TYPE_ACTIVATE;
                OPCODE_WRITE : new_command.cmd_type = DDR_CMD_TYPE_WRITE;
                OPCODE_READ : new_command.cmd_type = DDR_CMD_TYPE_READ;
                OPCODE_MRS : new_command.cmd_type = DDR_CMD_TYPE_MRS;
                OPCODE_REFRESH : new_command.cmd_type = DDR_CMD_TYPE_REFRESH;
                OPCODE_NOP : new_command.cmd_type = DDR_CMD_TYPE_NOP;
                OPCODE_DES : new_command.cmd_type = DDR_CMD_TYPE_DES;
                default : new_command.cmd_type = DDR_CMD_TYPE_ERROR;
            endcase
    
            new_command.address = mem_a;
            if(new_command.cmd_type == DDR_CMD_TYPE_READ || new_command.cmd_type == DDR_CMD_TYPE_WRITE) begin
		new_command.address = {'0,col_addr};
            end
            
            case (new_command.cmd_type)
                DDR_CMD_TYPE_NOP : cmd_nop();
                DDR_CMD_TYPE_DES : cmd_des();
                DDR_CMD_TYPE_ERROR : cmd_unknown();
                DDR_CMD_TYPE_ACTIVATE : cmd_set_activate();
                DDR_CMD_TYPE_PRECHARGE : cmd_precharge(new_command.bank, mem_a[10]);
                DDR_CMD_TYPE_WRITE : cmd_write();
                DDR_CMD_TYPE_READ : cmd_read();
                DDR_CMD_TYPE_MRS : cmd_mrs();
                DDR_CMD_TYPE_REFRESH : cmd_refresh();
            endcase
    
            if(CHECK_VIOLATIONS)
                check_violations();
        end
        
        if (read_command_pipeline[0]) begin
            if (read_command_queue.size() == 0) begin
                $display("[%0t] %m: Internal Error: READ command queue empty but READ commands expected!", $time);
                $stop(1);
            end
        end
    
        if (write_command_pipeline[0]) begin
            if (write_command_queue.size() == 0) begin
                $display("[%0t] %m: Internal Error: WRITE command queue empty but WRITE commands expected!", $time);
                $stop(1);
            end
        end
    
        if (active_command.cmd_type != DDR_CMD_TYPE_NOP) begin
            if (active_command.word_count == active_command.burst_length) begin
                active_command.cmd_type = DDR_CMD_TYPE_NOP;
            end
        end
        
    
        if (active_command.cmd_type == DDR_CMD_TYPE_NOP) begin
            if (read_command_pipeline[0]) begin
                active_command.cmd_type = read_command_queue.pop_front();
                active_command.word_count = read_word_count_queue.pop_front();
                active_command.burst_length = read_burst_length_queue.pop_front();
                active_command.address = read_address_queue.pop_front();
                active_command.bank = read_bank_queue.pop_front();
    
                if (active_command.cmd_type != DDR_CMD_TYPE_READ) begin
                    $display("[%0t] %m: Internal Error: Expected READ command not in queue!", $time);
                    $stop(1);
                end
                
            end
            else if (write_command_pipeline[0]) begin
                active_command.cmd_type = write_command_queue.pop_front();
                active_command.word_count = write_word_count_queue.pop_front();
                active_command.burst_length = write_burst_length_queue.pop_front();
                active_command.address = write_address_queue.pop_front();
                active_command.bank = write_bank_queue.pop_front();
    
                if (active_command.cmd_type != DDR_CMD_TYPE_WRITE) begin
                    $display("[%0t] %m: Internal Error: Expected WRITE command not in queue!", $time);
                    $stop(1);
                end
                
            end
        end
        else begin
            if (read_command_pipeline[0] || write_command_pipeline[0]) begin
                $display("[%0t] %m: Internal Error: Active command but read/write pipeline also active!", $time);
                $stop(1);
            end
        end
    
        if (precharge_command_pipeline[0]) begin
            precharge_command.cmd_type = precharge_command_queue.pop_front();
            precharge_command.bank = precharge_bank_queue.pop_front();
    
            cmd_precharge(precharge_command.bank, 1'b0);
        end
        
        if (activate_command_pipeline[0]) begin
            activate_command.cmd_type = activate_command_queue.pop_front();
            activate_command.bank = activate_bank_queue.pop_front();
            activate_command.address = activate_row_queue.pop_front();
            
            cmd_activate(activate_command.bank, activate_command.address);
        end
    
        mem_dq_en = 1'b0;
        mem_dqs_en = 1'b0;
        mem_dqs_preamble = 1'b0;
        if (active_command.cmd_type == DDR_CMD_TYPE_WRITE) begin
			#100;
            write_memory(active_command, mem_dq_captured, full_mask);
            active_command.word_count = active_command.word_count+1;
        end
        else if (active_command.cmd_type == DDR_CMD_TYPE_READ) begin
            read_memory(active_command, mem_dq_int);
            mem_dq_en = 1'b1;
            mem_dqs_en = 1'b1;
            active_command.word_count = active_command.word_count+1;
        end
    
        if (!mem_dqs_en & (read_command_pipeline[2] | read_command_pipeline[1])) begin
            mem_dqs_en = 1'b1;
            mem_dqs_preamble = 1'b1;
        end
end

generate
genvar dm_count;
	for (dm_count = 0; dm_count < MEM_DQS_WIDTH; dm_count = dm_count + 1)
	begin: dm_mapping
		assign full_mask [(dm_count + 1) * MEM_DQS_GROUP_SIZE - 1 : dm_count * MEM_DQS_GROUP_SIZE] = {MEM_DQS_GROUP_SIZE{mem_dm_captured[dm_count]}};
	end
endgenerate

assign #1 mem_dqs_shifted = mem_dqs;
assign #1 mem_dqs_n_shifted = mem_dqs_n;
always @(posedge mem_dqs_shifted[0] or posedge mem_dqs_n_shifted[0]) begin
	mem_dq_captured <= mem_dq;
	mem_dm_captured <= mem_dm;
end

assign mem_dq = (mem_dq_en) ? mem_dq_int : 'z;
assign mem_dqs = 
	(mem_dqs_en) 
		?  (mem_dqs_preamble) 
			     ? '0 : {MEM_DQS_WIDTH{mem_ck_diff}} 
		              : 'z;
        
assign mem_dqs_n = 
	(mem_dqs_en) 
		?  (mem_dqs_preamble) 
			? '1 : {MEM_DQS_WIDTH{~mem_ck_diff}} 
		: 'z;

endmodule
