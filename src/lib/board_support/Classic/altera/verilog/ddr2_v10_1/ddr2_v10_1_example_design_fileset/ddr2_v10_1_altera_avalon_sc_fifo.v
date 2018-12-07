// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.


`timescale 1 ns / 1 ns

module ddr2_v10_1_altera_avalon_sc_fifo
#(
    parameter SYMBOLS_PER_BEAT  = 1,
    parameter BITS_PER_SYMBOL   = 8,
    parameter FIFO_DEPTH        = 16,
    parameter CHANNEL_WIDTH     = 0,
    parameter ERROR_WIDTH       = 0,
    parameter USE_PACKETS       = 0,
    parameter USE_FILL_LEVEL    = 0,
    parameter USE_STORE_FORWARD = 0,
    parameter USE_ALMOST_FULL_IF = 0,
    parameter USE_ALMOST_EMPTY_IF = 0,

    parameter EMPTY_LATENCY     = 3,
    parameter USE_MEMORY_BLOCKS = 1,

    parameter DATA_WIDTH  = SYMBOLS_PER_BEAT * BITS_PER_SYMBOL,
    parameter EMPTY_WIDTH = log2ceil(SYMBOLS_PER_BEAT)
)
(
    input                       clk,
    input                       reset,

    input [DATA_WIDTH-1: 0]     in_data,
    input                       in_valid,
    input                       in_startofpacket,
    input                       in_endofpacket,
    input [((EMPTY_WIDTH>0) ? (EMPTY_WIDTH-1):0) : 0]     in_empty,
    input [((ERROR_WIDTH>0) ? (ERROR_WIDTH-1):0) : 0]     in_error,
    input [((CHANNEL_WIDTH>0) ? (CHANNEL_WIDTH-1):0): 0]  in_channel,
    output                      in_ready,

    output [DATA_WIDTH-1 : 0]   out_data,
    output reg                  out_valid,
    output                      out_startofpacket,
    output                      out_endofpacket,
    output [((EMPTY_WIDTH>0) ? (EMPTY_WIDTH-1):0) : 0]    out_empty,
    output [((ERROR_WIDTH>0) ? (ERROR_WIDTH-1):0) : 0]    out_error,
    output [((CHANNEL_WIDTH>0) ? (CHANNEL_WIDTH-1):0): 0] out_channel,
    input                       out_ready,

    input [(USE_STORE_FORWARD ? 2 : 1) : 0]   csr_address,
    input                       csr_write,
    input                       csr_read,
    input [31 : 0]              csr_writedata,
    output reg [31 : 0]         csr_readdata,

    output  wire                almost_full_data,
    output  wire                almost_empty_data
);

    localparam ADDR_WIDTH   = log2ceil(FIFO_DEPTH);
    localparam DEPTH        = FIFO_DEPTH;
    localparam PKT_SIGNALS_WIDTH = 2 + EMPTY_WIDTH;
    localparam PAYLOAD_WIDTH     = (USE_PACKETS == 1) ? 
                   2 + EMPTY_WIDTH + DATA_WIDTH + ERROR_WIDTH + CHANNEL_WIDTH:
                   DATA_WIDTH + ERROR_WIDTH + CHANNEL_WIDTH;

    genvar i;

    reg [PAYLOAD_WIDTH-1 : 0] mem [DEPTH-1 : 0];
    reg [ADDR_WIDTH-1 : 0]  wr_ptr;
    reg [ADDR_WIDTH-1 : 0]  rd_ptr;
    reg [DEPTH-1      : 0]  mem_used;

    wire [ADDR_WIDTH-1 : 0] next_wr_ptr;
    wire [ADDR_WIDTH-1 : 0] next_rd_ptr;
    wire [ADDR_WIDTH-1 : 0] incremented_wr_ptr;
    wire [ADDR_WIDTH-1 : 0] incremented_rd_ptr;

    wire [ADDR_WIDTH-1 : 0] mem_rd_ptr;

    wire read;
    wire write;

    reg empty;
    reg next_empty;
    reg full;
    reg next_full;

    wire [PKT_SIGNALS_WIDTH-1 : 0] in_packet_signals;
    wire [PKT_SIGNALS_WIDTH-1 : 0] out_packet_signals;
    wire [PAYLOAD_WIDTH-1 : 0] in_payload;
    reg  [PAYLOAD_WIDTH-1 : 0] internal_out_payload;
    reg  [PAYLOAD_WIDTH-1 : 0] out_payload;

    reg  internal_out_valid;
    wire internal_out_ready;

    reg  [ADDR_WIDTH : 0] fifo_fill_level;
    reg  [ADDR_WIDTH : 0] fill_level;

    reg  [ADDR_WIDTH-1 : 0]   sop_ptr = 0;
    reg  [23:0]   almost_full_threshold;
    reg  [23:0]   almost_empty_threshold;
    reg  [23:0]   cut_through_threshold;
    reg  [15:0]   pkt_cnt;
    reg           drop_on_error_en;
    reg           error_in_pkt;
    reg           pkt_has_started;
    reg           sop_has_left_fifo;
    reg           fifo_too_small_r;
    reg           pkt_cnt_eq_zero;
    reg           pkt_cnt_eq_one;

    wire          wait_for_threshold;
    reg           pkt_mode;
    wire          wait_for_pkt;
    wire          ok_to_forward;
    wire          in_pkt_eop_arrive;
    wire          out_pkt_leave;
    wire          in_pkt_start;
    wire          in_pkt_error;
    wire          drop_on_error;
    wire          fifo_too_small;
    wire          out_pkt_sop_leave;
    wire [31:0]   max_fifo_size;

    generate
        if (EMPTY_WIDTH > 0) begin
            assign in_packet_signals = {in_startofpacket, in_endofpacket, in_empty};
            assign {out_startofpacket, out_endofpacket, out_empty} = out_packet_signals;
        end 
        else begin
            assign out_empty = in_error;
            assign in_packet_signals = {in_startofpacket, in_endofpacket};
            assign {out_startofpacket, out_endofpacket} = out_packet_signals;
        end
    endgenerate

    generate
        if (USE_PACKETS) begin
            if (ERROR_WIDTH > 0) begin
                if (CHANNEL_WIDTH > 0) begin
                    assign in_payload = {in_packet_signals, in_data, in_error, in_channel};
                    assign {out_packet_signals, out_data, out_error, out_channel} = out_payload;
                end
                else begin
                    assign out_channel = in_channel;
                    assign in_payload = {in_packet_signals, in_data, in_error};
                    assign {out_packet_signals, out_data, out_error} = out_payload;
                end
            end
            else begin
                assign out_error = in_error;
                if (CHANNEL_WIDTH > 0) begin
                    assign in_payload = {in_packet_signals, in_data, in_channel};
                    assign {out_packet_signals, out_data, out_channel} = out_payload;
                end
                else begin
                    assign out_channel = in_channel;
                    assign in_payload = {in_packet_signals, in_data};
                    assign {out_packet_signals, out_data} = out_payload;
                end
            end
        end
        else begin 
            assign out_packet_signals = 0;
            if (ERROR_WIDTH > 0) begin
                if (CHANNEL_WIDTH > 0) begin
                    assign in_payload = {in_data, in_error, in_channel};
                    assign {out_data, out_error, out_channel} = out_payload;
                end
                else begin
                    assign out_channel = in_channel;
                    assign in_payload = {in_data, in_error};
                    assign {out_data, out_error} = out_payload;
                end
            end
            else begin
                assign out_error = in_error;
                if (CHANNEL_WIDTH > 0) begin
                    assign in_payload = {in_data, in_channel};
                    assign {out_data, out_channel} = out_payload;
                end
                else begin
                    assign out_channel = in_channel;
                    assign in_payload = in_data;
                    assign out_data = out_payload;
                end
            end
        end
    endgenerate

    generate if (USE_MEMORY_BLOCKS == 1) begin

        if (EMPTY_LATENCY == 1) begin

            always @(posedge clk) begin
                if (in_valid && in_ready)
                    mem[wr_ptr] = in_payload;

                internal_out_payload = mem[mem_rd_ptr];
            end

        end else begin

            always @(posedge clk) begin
                if (in_valid && in_ready)
                    mem[wr_ptr] <= in_payload;

                internal_out_payload <= mem[mem_rd_ptr];
            end

        end

        assign mem_rd_ptr = next_rd_ptr;
    
    end else begin 

        for (i = 0; i < DEPTH-1; i = i + 1) begin : shift_reg
            always @(posedge clk or posedge reset) begin
                if (reset) begin
                    mem[i] <= 0;
                end 
                else if (read || !mem_used[i]) begin
                    if (!mem_used[i+1])
                        mem[i] <= in_payload;
                    else
                        mem[i] <= mem[i+1];
                end
            end
        end

        always @(posedge clk, posedge reset) begin
            if (reset) begin
                mem[DEPTH-1] <= 0;
            end 
            else begin
                if (!mem_used[DEPTH-1])
                    mem[DEPTH-1] <= in_payload;

                if (DEPTH == 1) begin
                    if (write)
                        mem[DEPTH-1] <= in_payload;
                end
            end
        end

    end
    endgenerate

    assign read  = internal_out_ready && internal_out_valid  && ok_to_forward;
    assign write = in_ready && in_valid;

    generate if (USE_MEMORY_BLOCKS == 1) begin

        assign incremented_wr_ptr = wr_ptr + 1'b1;
        assign incremented_rd_ptr = rd_ptr + 1'b1;
        assign next_wr_ptr =  drop_on_error ? sop_ptr : write ?  incremented_wr_ptr : wr_ptr;
        assign next_rd_ptr = (read) ? incremented_rd_ptr : rd_ptr;

        always @(posedge clk or posedge reset) begin
            if (reset) begin
                wr_ptr <= 0;
                rd_ptr <= 0;
            end
            else begin
                wr_ptr <= next_wr_ptr;
                rd_ptr <= next_rd_ptr;
            end
        end

    end else begin

        always @(posedge clk or posedge reset) begin
            if (reset) begin
                mem_used[0] <= 0;
            end 
            else begin
                if (write ^ read) begin
                    if (read) begin
                        if (DEPTH > 1) 
                            mem_used[0] <= mem_used[1];
                        else
                            mem_used[0] <= 0;
                    end
                    if (write)
                        mem_used[0] <= 1;
                end
            end
        end

        if (DEPTH > 1) begin
            always @(posedge clk or posedge reset) begin
                if (reset) begin
                    mem_used[DEPTH-1] <= 0;
                end
                else begin 
                    if (write ^ read) begin            
                        mem_used[DEPTH-1] <= 0;
                        if (write)
                            mem_used[DEPTH-1] <= mem_used[DEPTH-2];
                    end
                end
            end
          end
     
        for (i = 1; i < DEPTH-1; i = i + 1) begin : storage_logic
            always @(posedge clk, posedge reset) begin
                if (reset) begin
                    mem_used[i] <= 0;
                end 
                else begin
                    if (write ^ read) begin
                        if (read)
                            mem_used[i] <= mem_used[i+1];
                        if (write) 
                            mem_used[i] <= mem_used[i-1];
                    end
                end
            end
        end
     
    end
    endgenerate


    generate if (USE_MEMORY_BLOCKS == 1) begin

        always @* begin
            next_full = full;
            next_empty = empty;
     
            if (read && !write) begin
                next_full = 1'b0;
     
                if (incremented_rd_ptr == wr_ptr)
                    next_empty = 1'b1;
            end
            
            if (write && !read) begin
                if (!drop_on_error)
                  next_empty = 1'b0;
                else if (sop_ptr == rd_ptr)   // drop on error and only 1 pkt in fifo
                  next_empty = 1'b1;
     
                if (incremented_wr_ptr == rd_ptr && !drop_on_error)
                    next_full = 1'b1;
            end

            if (write && read && drop_on_error) begin
                if (sop_ptr == next_rd_ptr)
                  next_empty = 1'b1;
            end
        end
     
        always @(posedge clk or posedge reset) begin
            if (reset) begin
                empty <= 1;
                full  <= 0;
            end
            else begin 
                empty <= next_empty;
                full  <= next_full;
            end
        end

    end else begin
        always @* begin
            full  = mem_used[DEPTH-1];
            empty = !mem_used[0];

            if (DEPTH == 1)
                full = mem_used[0] && !read;

            internal_out_payload = mem[0];

            if (EMPTY_LATENCY == 0) begin
                empty = !mem_used[0] && !in_valid;

                if (!mem_used[0] && in_valid)
                    internal_out_payload = in_payload;
            end
        end

    end
    endgenerate

    assign in_ready = !full;
    assign internal_out_ready = out_ready || !out_valid;

    generate if (EMPTY_LATENCY > 1) begin
        always @(posedge clk or posedge reset) begin
            if (reset)
                internal_out_valid <= 0;
            else begin
                internal_out_valid <= !empty & ok_to_forward & ~drop_on_error;

                if (read) begin
                    if (incremented_rd_ptr == wr_ptr)
                        internal_out_valid <= 1'b0;
                end
            end
        end
    end else begin
        always @* begin
            internal_out_valid = !empty & ok_to_forward;
        end
    end
    endgenerate

    generate if (EMPTY_LATENCY == 3) begin
        always @(posedge clk or posedge reset) begin
            if (reset) begin
                out_valid   <= 0;
                out_payload <= 0;
            end
            else begin
                if (internal_out_ready) begin
                    out_valid   <= internal_out_valid & ok_to_forward;
                    out_payload <= internal_out_payload;
                end
            end
        end
    end
    else begin
        always @* begin
            out_valid   = internal_out_valid;
            out_payload = internal_out_payload;
        end
    end
    endgenerate

    generate if (USE_FILL_LEVEL) begin
        wire [31:0] depth32;
        assign depth32 = DEPTH;
        always @(posedge clk or posedge reset) begin
            if (reset) 
                fifo_fill_level <= 0;
            else if (next_full & !drop_on_error)
                fifo_fill_level <= depth32[ADDR_WIDTH:0];
            else begin
                fifo_fill_level[ADDR_WIDTH]     <= 1'b0;
                fifo_fill_level[ADDR_WIDTH-1 : 0] <= next_wr_ptr - next_rd_ptr;
            end
        end

        always @* begin
            fill_level = fifo_fill_level;

            if (EMPTY_LATENCY == 3)
                fill_level = fifo_fill_level + {{ADDR_WIDTH{1'b0}}, out_valid};
        end
    end
    else begin
          always @* begin
            fill_level = 0;
          end
    end
    endgenerate

    generate if (USE_ALMOST_FULL_IF) begin
      assign almost_full_data = (fill_level >= almost_full_threshold);
    end
    else
      assign almost_full_data = 0;
    endgenerate

    generate if (USE_ALMOST_EMPTY_IF) begin
      assign almost_empty_data = (fill_level <= almost_empty_threshold);
    end
    else
      assign almost_empty_data = 0;
    endgenerate

    generate if (USE_STORE_FORWARD) begin
    assign max_fifo_size = FIFO_DEPTH - 1;
      always @(posedge clk or posedge reset) begin
          if (reset) begin
              almost_full_threshold  <= max_fifo_size[23 : 0];
              almost_empty_threshold <= 0;
              cut_through_threshold  <= 0;
              drop_on_error_en       <= 0;
              csr_readdata           <= 0;
              pkt_mode               <= 1'b1;
          end
          else begin
             if (csr_write) begin
               if(csr_address == 3'b010)
                  almost_full_threshold  <= csr_writedata[23:0]; 
               if(csr_address == 3'b011)
                  almost_empty_threshold <= csr_writedata[23:0]; 
               if(csr_address == 3'b100) begin
                  cut_through_threshold  <= csr_writedata[23:0]; 
                  pkt_mode <= (csr_writedata[23:0] == 0);
                end
               if(csr_address == 3'b101)
                  drop_on_error_en       <= csr_writedata[0]; 
              end

              if (csr_read) begin
                csr_readdata <= 32'b0;
                if (csr_address == 0)
                    csr_readdata <= {{(31 - ADDR_WIDTH){1'b0}}, fill_level};
                if (csr_address == 2)
                    csr_readdata <= {8'b0, almost_full_threshold};
                if (csr_address == 3)
                    csr_readdata <= {8'b0, almost_empty_threshold};
                if (csr_address == 4)
                    csr_readdata <= {8'b0, cut_through_threshold};
                if (csr_address == 5)
                    csr_readdata <= {31'b0, drop_on_error_en};
              end
          end
      end
    end
    else if (USE_ALMOST_FULL_IF || USE_ALMOST_EMPTY_IF) begin
    assign max_fifo_size = FIFO_DEPTH - 1;
      always @(posedge clk or posedge reset) begin
          if (reset) begin
              almost_full_threshold  <= max_fifo_size[23 : 0];
              almost_empty_threshold <= 0;
              csr_readdata           <= 0;
          end
          else begin
             if (csr_write) begin
               if(csr_address == 3'b010)
                  almost_full_threshold  <= csr_writedata[23:0];
               if(csr_address == 3'b011)
                  almost_empty_threshold <= csr_writedata[23:0];
              end

              if (csr_read) begin
                csr_readdata <= 32'b0;
                if (csr_address == 0)
                    csr_readdata <= {{(31 - ADDR_WIDTH){1'b0}}, fill_level};
                if (csr_address == 2)
                    csr_readdata <= {8'b0, almost_full_threshold};
                if (csr_address == 3)
                    csr_readdata <= {8'b0, almost_empty_threshold};
              end
          end
      end
    end
    else begin
      always @(posedge clk or posedge reset) begin
          if (reset) begin
              csr_readdata <= 0;
          end
          else if (csr_read) begin
              csr_readdata <= 0;

              if (csr_address == 0) 
                  csr_readdata <= fill_level;
          end
      end
    end
    endgenerate


    generate if (USE_STORE_FORWARD) begin
      assign wait_for_threshold   = (fifo_fill_level < cut_through_threshold) & wait_for_pkt ;
      assign wait_for_pkt         = pkt_cnt_eq_zero  | (pkt_cnt_eq_one  & out_pkt_leave);
      assign ok_to_forward        = (pkt_mode ? (~wait_for_pkt | ~pkt_has_started) : 
                                     ~wait_for_threshold) | fifo_too_small_r;
      assign in_pkt_eop_arrive    = in_valid & in_ready & in_endofpacket;
      assign in_pkt_start         = in_valid & in_ready & in_startofpacket;
      assign in_pkt_error         = in_valid & in_ready & |in_error;
      assign out_pkt_sop_leave    = out_valid & out_ready & out_startofpacket;
      assign out_pkt_leave        = out_valid & out_ready & out_endofpacket;
      assign fifo_too_small       = (pkt_mode ? wait_for_pkt : wait_for_threshold) & full & out_ready;

      always @(posedge clk or posedge reset) begin
        if (reset) begin
          pkt_cnt           <= 0;
          pkt_has_started   <= 0;
          sop_has_left_fifo <= 0;
          fifo_too_small_r  <= 0;
          pkt_cnt_eq_zero   <= 1'b1;
          pkt_cnt_eq_one    <= 1'b0;
        end
        else begin
          fifo_too_small_r <= fifo_too_small;

          if( in_pkt_eop_arrive )
            sop_has_left_fifo <= 1'b0;
          else if (out_pkt_sop_leave & pkt_cnt_eq_zero )
            sop_has_left_fifo <= 1'b1;

          if (in_pkt_eop_arrive & ~out_pkt_leave & ~drop_on_error ) begin
            pkt_cnt <= pkt_cnt + 1'b1;
            pkt_cnt_eq_zero <= 0;
            if (pkt_cnt == 0)
              pkt_cnt_eq_one <= 1'b1;
            else
              pkt_cnt_eq_one <= 1'b0;
          end
          else if((~in_pkt_eop_arrive | drop_on_error) & out_pkt_leave) begin
            pkt_cnt <= pkt_cnt - 1'b1;
            if (pkt_cnt == 1) 
              pkt_cnt_eq_zero <= 1'b1;
            else
              pkt_cnt_eq_zero <= 1'b0;
            if (pkt_cnt == 2) 
              pkt_cnt_eq_one <= 1'b1;
            else
              pkt_cnt_eq_one <= 1'b0;
          end

          if (in_pkt_start)
            pkt_has_started <= 1'b1;
          else if (in_pkt_eop_arrive)
            pkt_has_started <= 1'b0;
        end
      end

      always @(posedge clk or posedge reset) begin
        if (reset) begin
          sop_ptr <= 0;
          error_in_pkt <= 0;
        end
        else begin
          if ( in_pkt_start ) 
            sop_ptr <= wr_ptr;

          if (in_pkt_eop_arrive)
            error_in_pkt <= 1'b0;
          else if ( in_pkt_error & (pkt_has_started | in_pkt_start))
            error_in_pkt <= 1'b1;
        end
      end
      assign drop_on_error = drop_on_error_en & (error_in_pkt | in_pkt_error) & in_pkt_eop_arrive & 
                            ~sop_has_left_fifo & ~(out_pkt_sop_leave & pkt_cnt_eq_zero);

    end
    else begin
      assign ok_to_forward = 1'b1;
      assign drop_on_error = 1'b0;
    end
    endgenerate


    function integer log2ceil;
        input integer val;
        integer i;

        begin
            i = 1;
            log2ceil = 0;

            while (i < val) begin
                log2ceil = log2ceil + 1;
                i = i << 1; 
            end
        end
    endfunction

endmodule
