// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

`timescale 1 ns / 1 ns

module ddr2_v10_1_altera_jtag_streaming (
    output tck,
    input  reset_n,
    output [7:0] source_data,
    output       source_valid,
    input [7:0] sink_data,
    input       sink_valid,
    output      sink_ready,
    input clock_to_sample,
    input reset_to_sample,
    output reg resetrequest = 1'b0
);

    function integer flog2;
      input [31:0] Depth;
      integer i;
      begin
        i = Depth;
        if ( i <= 0 ) flog2 = 0;
        else begin
          for(flog2 = -1; i > 0; flog2 = flog2 + 1)
          i = i >> 1;
        end
      end    
    endfunction // flog2

    parameter PURPOSE = 0;
    parameter UPSTREAM_FIFO_SIZE = 0;
    parameter DOWNSTREAM_FIFO_SIZE = 0;
    localparam UPSTREAM_ENCODED_SIZE = flog2(UPSTREAM_FIFO_SIZE);
    localparam DOWNSTREAM_ENCODED_SIZE = flog2(DOWNSTREAM_FIFO_SIZE);

   parameter TCK_TO_SYSCLK_SYNC_DEPTH = 8;
   parameter SYSCLK_TO_TCK_SYNC_DEPTH = 3;

    localparam DATA     = 0;
    localparam LOOPBACK = 1;
    localparam DEBUG    = 2;
    localparam INFO     = 3;
    localparam CONTROL  = 4;


    localparam IRWIDTH = 3;

    wire [IRWIDTH - 1 : 0] ir_out;
    wire [IRWIDTH - 1 : 0] ir_in;
    reg tdo = 0;
    wire tdi;
    wire sdr;
    wire cdr;
    wire udr;

    localparam ST_BYPASS     = 'h0;
    localparam ST_HEADER_1   = 'h1;
    localparam ST_HEADER_2   = 'h2;
    localparam ST_WRITE_DATA = 'h3;

    localparam ST_HEADER    = 'h0;
    localparam ST_PADDED    = 'h1;
    localparam ST_READ_DATA = 'h2;

    reg [1:0] write_state = ST_BYPASS;
    reg [1:0] read_state  = ST_HEADER;

    reg [ 7:0] dr_data_in  = 'b0;
    reg [ 7:0] dr_data_out = 'b0;
    reg        dr_loopback = 'b0;
    reg [ 2:0] dr_debug    = 'b0;
    reg [10:0] dr_info     = 'b0;
    reg [ 8:0] dr_control  = 'b0;

    reg [ 8:0] padded_bit_counter             = 'b0;
    reg [ 7:0] bypass_bit_counter             = 'b0;
    reg [ 2:0] write_data_bit_counter         = 'b0;
    reg [ 2:0] read_data_bit_counter          = 'b0;
    reg [ 3:0] header_in_bit_counter          = 'b0;
    reg [ 3:0] header_out_bit_counter         = 'b0;
    reg [18:0] scan_length_byte_counter       = 'b0;
    reg [18:0] valid_write_data_length_byte_counter  = 'b0;

    reg write_data_valid     = 'b0;
    reg read_data_valid      = 'b0;
    reg read_data_all_valid  = 'b0;

    reg decode_header_1 = 'b0;
    reg decode_header_2 = 'b0;

    wire write_data_byte_aligned;
    wire read_data_byte_aligned;
    wire padded_bit_byte_aligned;
    wire bytestream_end;

    assign write_data_byte_aligned = (write_data_bit_counter == 1);
    assign read_data_byte_aligned  = (read_data_bit_counter == 1);
    assign padded_bit_byte_aligned = (padded_bit_counter[2:0] == 'b0);
    assign bytestream_end          = (scan_length_byte_counter == 'b0);

    reg [ 7:0] offset     = 'b0;
    reg [15:0] header_in  = 'b0;

    reg [9:0] scan_length       = 'b0;
    reg [2:0] read_data_length  = 'b0;
    reg [2:0] write_data_length = 'b0;

    wire [7:0] idle_inserter_sink_data;
    wire       idle_inserter_sink_valid;
    wire       idle_inserter_sink_ready;
    wire [7:0] idle_inserter_source_data;
    reg        idle_inserter_source_ready = 'b0;
    reg  [7:0] idle_remover_sink_data     = 'b0;
    reg        idle_remover_sink_valid    = 'b0;
    wire [7:0] idle_remover_source_data;
    wire       idle_remover_source_valid;

    assign source_data  = idle_remover_source_data;
    assign source_valid = idle_remover_source_valid;
    assign sink_ready   = idle_inserter_sink_ready;
    assign idle_inserter_sink_data  = sink_data;
    assign idle_inserter_sink_valid = sink_valid;

    reg clock_sensor         = 'b0;
    reg clock_to_sample_div2 = 'b0;
    (* altera_attribute = {"-name GLOBAL_SIGNAL OFF"}*) reg clock_sense_reset_n  = 'b1;

    wire data_available;

    assign data_available = sink_valid;

    wire [18:0] decoded_scan_length;
    wire [18:0] decoded_write_data_length;
    wire [18:0] decoded_read_data_length;

    assign decoded_scan_length =  { scan_length, {8{1'b1}} };

    assign decoded_write_data_length = (write_data_length == 0) ? 19'h0 : (19'h00080 << write_data_length);
    assign decoded_read_data_length  = (read_data_length == 0)  ? 19'h0 : (19'h00080 << read_data_length);

    wire clock_sensor_sync;
    wire reset_to_sample_sync;
    wire clock_to_sample_div2_sync;
    wire clock_sense_reset_n_sync;

    altera_std_synchronizer #(.depth(SYSCLK_TO_TCK_SYNC_DEPTH)) clock_sensor_synchronizer (
        .clk(tck),
        .reset_n(1'b1),
        .din(clock_sensor),
        .dout(clock_sensor_sync));

    altera_std_synchronizer #(.depth(SYSCLK_TO_TCK_SYNC_DEPTH)) reset_to_sample_synchronizer (
        .clk(tck),
        .reset_n(1'b1),
        .din(reset_to_sample),
        .dout(reset_to_sample_sync));

    altera_std_synchronizer #(.depth(SYSCLK_TO_TCK_SYNC_DEPTH)) clock_to_sample_div2_synchronizer (
        .clk(tck),
        .reset_n(1'b1),
        .din(clock_to_sample_div2),
        .dout(clock_to_sample_div2_sync));

    altera_std_synchronizer #(.depth(TCK_TO_SYSCLK_SYNC_DEPTH)) clock_sense_reset_n_synchronizer (
        .clk(clock_to_sample),
        .reset_n(clock_sense_reset_n),
        .din(1'b1),
        .dout(clock_sense_reset_n_sync));

    always @ (posedge clock_to_sample or negedge clock_sense_reset_n_sync) begin
        if (~clock_sense_reset_n_sync) begin
            clock_sensor <= 1'b0;
        end else begin
            clock_sensor <= 1'b1;
        end
    end

    always @ (posedge clock_to_sample) begin
        clock_to_sample_div2 <= ~clock_to_sample_div2;
    end

    always @ (posedge tck) begin

        idle_remover_sink_valid <= 1'b0;
        idle_inserter_source_ready <= 1'b0;


        if (ir_in == DATA) begin


            if (cdr) begin
                if (offset == 'b0) begin
                    write_state <= ST_HEADER_1;
                end else begin
                    write_state <= ST_BYPASS;
                end
                bypass_bit_counter <= offset;
                header_in_bit_counter <= 15;
                write_data_bit_counter <= 0;
                decode_header_1 <= 1'b0;
                decode_header_2 <= 1'b0;
                read_data_all_valid  <= 1'b0;
                valid_write_data_length_byte_counter  <= 0;
            end

            if (sdr) begin

                case (write_state)
                    ST_BYPASS: begin
                        bypass_bit_counter <= bypass_bit_counter - 1'b1;
                        if (bypass_bit_counter == 1) begin
                            write_state <= ST_HEADER_1;
                        end
                    end
                    ST_HEADER_1: begin
                        header_in <= {tdi, header_in[15:1]};
                        header_in_bit_counter <= header_in_bit_counter - 1'b1;
                        if (header_in_bit_counter == 3) begin
                            read_data_length  <= {tdi, header_in[15:14]};
                            scan_length       <= header_in[13:4];
                            write_state <= ST_HEADER_2;
                            decode_header_1 <= 1'b1;
                        end
                    end
                    ST_HEADER_2: begin
                        header_in <= {tdi, header_in[15:1]};
                        header_in_bit_counter <= header_in_bit_counter - 1'b1;
                        if (decode_header_1) begin
                            decode_header_1 <= 1'b0;
                            if (read_data_length == 3'b111) begin
                                read_data_all_valid <= 1'b1;
                            end
                            scan_length_byte_counter <= decoded_scan_length;
                        end
                        if (header_in_bit_counter == 0) begin
                            write_data_length <= {tdi, header_in[15:14]};
                            write_state <= ST_WRITE_DATA;
                            decode_header_2 <= 1'b1;
                        end
                    end
                    ST_WRITE_DATA: begin
                        dr_data_in <= {tdi, dr_data_in[7:1]};
                        if (decode_header_2) begin
                            decode_header_2 <= 1'b0;
                            case (write_data_length)
                                3'b111:  valid_write_data_length_byte_counter <= decoded_scan_length + 1'b1;
                                3'b000:  valid_write_data_length_byte_counter <= 'b0;
                                default: valid_write_data_length_byte_counter <= decoded_write_data_length;
                            endcase
                        end
                        write_data_bit_counter <= write_data_bit_counter - 1'b1;
                        write_data_valid <= (valid_write_data_length_byte_counter != 0);
                        if (write_data_byte_aligned && write_data_valid) begin
                            valid_write_data_length_byte_counter <= valid_write_data_length_byte_counter - 1'b1;
                            idle_remover_sink_valid <= 1'b1;
                            idle_remover_sink_data <= {tdi, dr_data_in[7:1]};
                        end
                    end
                endcase
            end

        end


        if (ir_in == DATA) begin

            if (cdr) begin

                read_state <= ST_HEADER;
                if (|offset[2:0]) begin
                    padded_bit_counter[8:3] <= offset[7:3] + 1'b1;
                    padded_bit_counter[2:0] <= 3'b0;
                end else begin
                    padded_bit_counter <= {1'b0, offset};
                end
                header_out_bit_counter <= 0;
                read_data_bit_counter <= 0;
                dr_data_out <= {{7{1'b0}}, data_available};
                read_data_valid <= 0;

            end

            if (sdr) begin

                dr_data_out <= {1'b0, dr_data_out[7:1]};
                case (read_state)
                    ST_HEADER: begin
                        header_out_bit_counter <= header_out_bit_counter - 1'b1;
                        if (header_out_bit_counter == 2) begin
                            if (padded_bit_counter == 0) begin
                                idle_inserter_source_ready <= read_data_all_valid;
                            end
                        end
                        if (header_out_bit_counter == 1) begin
                            if (padded_bit_counter == 0) begin
                                read_state <= ST_READ_DATA;
                                read_data_valid <= read_data_all_valid || (scan_length_byte_counter<=decoded_read_data_length+1);
                                dr_data_out <= read_data_all_valid ? idle_inserter_source_data : 8'h4a;
                            end else begin
                                read_state <= ST_PADDED;
                                padded_bit_counter <= padded_bit_counter - 1'b1;
                                idle_inserter_source_ready <= 1'b0;
                                dr_data_out <= 8'h4a;
                            end
                        end
                    end
                    ST_PADDED: begin
                        padded_bit_counter <= padded_bit_counter - 1'b1;
                        if (padded_bit_byte_aligned) begin
                            dr_data_out <= 8'h4a;
                        end
                        if (padded_bit_counter == 1) begin
                            idle_inserter_source_ready <= read_data_all_valid;
                        end
                        if (padded_bit_counter == 0) begin // TODO: might make use of (padded_bit_counter[8:3]&padded_bit_byte_aligned)
                            read_state <= ST_READ_DATA;
                            read_data_valid <= read_data_all_valid || (scan_length_byte_counter<=decoded_read_data_length+1);
                            dr_data_out <= read_data_all_valid ? idle_inserter_source_data : 8'h4a;
                        end
                    end
                    ST_READ_DATA: begin
                        read_data_bit_counter <= read_data_bit_counter - 1'b1;
                        if (read_data_bit_counter == 2) begin
                            idle_inserter_source_ready <= bytestream_end ? 1'b0 : read_data_valid;
                        end
                        if (read_data_byte_aligned) begin
                            if (~bytestream_end) begin
                                scan_length_byte_counter <= scan_length_byte_counter - 1'b1;
                            end
                            read_data_valid <= read_data_all_valid || (scan_length_byte_counter<=decoded_read_data_length+1);
                            dr_data_out <= (read_data_valid & ~bytestream_end) ? idle_inserter_source_data : 8'h4a;
                        end
                    end
                endcase

            end

        end

        if (ir_in == LOOPBACK) begin
            if (cdr) begin
                dr_loopback <= 1'b0; // capture 0
            end
            if (sdr) begin
                dr_loopback <= tdi;
            end
        end

        if (ir_in == DEBUG) begin
            if (cdr) begin
                dr_debug <= {clock_sensor_sync, clock_to_sample_div2_sync, reset_to_sample_sync};
            end
            if (sdr) begin
                dr_debug <= {1'b0, dr_debug[2:1]}; // tdi is ignored
            end
            if (udr) begin
                clock_sense_reset_n <= 1'b0;
            end else begin
                clock_sense_reset_n <= 1'b1;
            end
        end

        if (ir_in == INFO) begin
            if (cdr) begin
                dr_info <= {PURPOSE[2:0], UPSTREAM_ENCODED_SIZE[3:0], DOWNSTREAM_ENCODED_SIZE[3:0]};
            end
            if (sdr) begin
                dr_info <= {1'b0, dr_info[10:1]}; // tdi is ignored
            end
        end

        if (ir_in == CONTROL) begin
            if (cdr) begin
                dr_control <= 'b0; // capture 0
            end
            if (sdr) begin
                dr_control <= {tdi, dr_control[8:1]};
            end
            if (udr) begin
                {resetrequest, offset} <= dr_control;
            end
        end

    end

    always @ * begin
        if (sdr) begin
            case (ir_in)
                DATA:     tdo <= dr_data_out[0];
                LOOPBACK: tdo <= dr_loopback;
                DEBUG:    tdo <= dr_debug[0];
                INFO:     tdo <= dr_info[0];
                CONTROL:  tdo <= dr_control[0];
                default:  tdo <= 1'b0;
            endcase
        end else begin
            tdo <= 1'b0;
        end
    end

    ddr2_v10_1_altera_jtag_sld_node node (
        .ir_out             (ir_out),
        .tdo                (tdo),
        .ir_in              (ir_in),
        .tck                (tck),
        .tdi                (tdi),
        .virtual_state_cdr  (cdr),
        .virtual_state_cir  (),
        .virtual_state_e1dr (),
        .virtual_state_e2dr (),
        .virtual_state_pdr  (),
        .virtual_state_sdr  (sdr),
        .virtual_state_udr  (udr),
        .virtual_state_uir  ()
    );

    ddr2_v10_1_altera_avalon_st_idle_remover idle_remover (
        .clk     (tck),
        .reset_n (reset_n),

        .in_ready (), // left disconnected
        .in_valid (idle_remover_sink_valid),
        .in_data  (idle_remover_sink_data),

        .out_ready (1'b1), // downstream is expected to be always ready
        .out_valid (idle_remover_source_valid),
        .out_data  (idle_remover_source_data)
    );

    ddr2_v10_1_altera_avalon_st_idle_inserter idle_inserter (
        .clk     (tck),
        .reset_n (reset_n),

        .in_ready (idle_inserter_sink_ready),
        .in_valid (idle_inserter_sink_valid),
        .in_data  (idle_inserter_sink_data),

        .out_ready (idle_inserter_source_ready),
        .out_valid (),
        .out_data  (idle_inserter_source_data)
    );

endmodule
