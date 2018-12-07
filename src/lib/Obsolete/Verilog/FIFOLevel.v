// Copyright 2000--2006 Bluespec, Inc.  All rights reserved.

`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif

// Sized fifo.  Model has output register which improves timing
module FIFOLevel(CLK,
                 RST_N,
                 D_IN,
                 ENQ,
                 FULL_N,
                 D_OUT,
                 DEQ,
                 EMPTY_N,
                 CLR,
                 COUNT
                 );
   
   // synopsys template   
   parameter               dataWidth = 1; // data width
   parameter               depth = 3;
   parameter               indxWidth = 1; // log(depth-1)
   // The -1 is allowed since this model has a fast output register
   parameter               init = 0 ;
   
   input                   CLK;
   input                   RST_N;
   input                   CLR;
   input [dataWidth - 1 : 0] D_IN;
   input                   ENQ;
   input                   DEQ;
   
   output                  FULL_N;
   output                  EMPTY_N;
   output [dataWidth - 1 : 0] D_OUT;
   output [indxWidth : 0] COUNT ;
   
   reg                      not_ring_full;
   reg                      ring_empty;
   
   reg [indxWidth-1 : 0] head;
   wire [indxWidth-1 : 0] next_head;

   reg [indxWidth-1 : 0]  tail;
   wire [indxWidth-1 : 0] next_tail;
   
   reg [dataWidth - 1 : 0]     arr[0:depth-2];

   reg [dataWidth - 1 : 0]     D_OUT;
   reg                       hasodata;

   wire [indxWidth-1:0]   depthLess2 = depth - 2 ;

   wire [indxWidth-1 : 0] incr_tail;
   wire [indxWidth-1 : 0] incr_head;
   reg [indxWidth : 0]    countReg ;
   
               
   assign                    incr_tail = tail + 1'b1 ;
   assign                    incr_head = head + 1'b1 ;
   
   
   assign    next_head = (head == depthLess2[indxWidth-1:0] ) ? 'b0 : incr_head ;
   assign    next_tail = (tail == depthLess2[indxWidth-1:0] ) ? 'b0 : incr_tail ;
   
   assign    EMPTY_N = hasodata;   
   assign    FULL_N  = not_ring_full;
   assign    COUNT   = countReg ;
   
`ifdef BSV_NO_INITIAL_BLOCKS
`else // not BSV_NO_INITIAL_BLOCKS
   // synopsys translate_off
   initial 
     begin : initial_block
        integer                   i;
        D_OUT         = {((dataWidth + 1)/2){2'b10}} ;

        ring_empty    = 1'b1;
        not_ring_full = 1'b1;
        hasodata      = 1'b0;
        head          = {indxWidth/2 {2'b10}} ;
        tail          = {indxWidth/2 {2'b10}} ;
        countReg      = {indxWidth/2 {2'b10}} ;

        for (i = 0; i <= depth - 2; i = i + 1) 
          begin
             arr[i]   = D_OUT ;
          end 
     end 
   // synopsys translate_on
`endif // BSV_NO_INITIAL_BLOCKS

   
   always @(posedge CLK) 
     begin
        if (CLR || !RST_N)
          begin
             head          <= `BSV_ASSIGNMENT_DELAY {indxWidth {1'b0}} ;
             tail          <= `BSV_ASSIGNMENT_DELAY {indxWidth {1'b0}} ;
             ring_empty    <= `BSV_ASSIGNMENT_DELAY 1'b1;
             not_ring_full <= `BSV_ASSIGNMENT_DELAY 1'b1;
             hasodata      <= `BSV_ASSIGNMENT_DELAY 1'b0;
             countReg      <= `BSV_ASSIGNMENT_DELAY 0 ;             
          end
        else 
          begin
             if (DEQ)
               if (ENQ)
                 begin
                    if (ring_empty)
                      D_OUT <= `BSV_ASSIGNMENT_DELAY D_IN;
                    else begin
                       arr[tail] <= `BSV_ASSIGNMENT_DELAY D_IN;
                       tail <= `BSV_ASSIGNMENT_DELAY next_tail;
                       D_OUT <= `BSV_ASSIGNMENT_DELAY arr[head];
                       head <= `BSV_ASSIGNMENT_DELAY next_head;
                    end
                 end
               else             // deq and not enq 
                 begin
                    countReg <= `BSV_ASSIGNMENT_DELAY countReg - 1'b1 ;                    
                    if (ring_empty)
                      hasodata <= `BSV_ASSIGNMENT_DELAY 1'b0;
                    else begin
                       D_OUT <= `BSV_ASSIGNMENT_DELAY arr[head];
                       head <= `BSV_ASSIGNMENT_DELAY next_head;
                       not_ring_full <= `BSV_ASSIGNMENT_DELAY 1'b1;
                       if (next_head == tail)
                         ring_empty <= `BSV_ASSIGNMENT_DELAY 1'b1;
                    end
                 end
             else
               if (ENQ)
                 begin
                    countReg <= `BSV_ASSIGNMENT_DELAY countReg + 1 ;                    
                    if (hasodata) begin
                       arr[tail] <= `BSV_ASSIGNMENT_DELAY D_IN;
                       tail <= `BSV_ASSIGNMENT_DELAY next_tail;
                       ring_empty <= `BSV_ASSIGNMENT_DELAY 1'b0;
                       if (next_tail == head)
                         not_ring_full <= `BSV_ASSIGNMENT_DELAY 1'b0;
                    end else begin
                       D_OUT <= `BSV_ASSIGNMENT_DELAY D_IN;
                       hasodata <= `BSV_ASSIGNMENT_DELAY 1'b1;
                    end
                 end
          end
     end // always @ (posedge CLK)

   // synopsys translate_off
   always@(posedge CLK)
     begin: error_checks
        reg deqerror, enqerror ;
        
        deqerror =  0;
        enqerror = 0;        
        if ( ! EMPTY_N && DEQ )
          begin
             deqerror = 1 ;             
             $display( "Warning: FIFOLevel: %m -- Dequeuing from empty fifo" ) ;
          end
        if ( ! FULL_N && ENQ && ! DEQ)
          begin
             enqerror =  1 ;             
             $display( "Warning: FIFOLevel: %m -- Enqueuing to a full fifo" ) ;
          end
     end // block: error_checks
   // synopsys translate_on

`ifdef BSV_NO_INITIAL_BLOCKS
`else // not BSV_NO_INITIAL_BLOCKS
   // synopsys translate_off
   // Some assertions about parameter values
   initial
     begin : parameter_assertions
        integer ok ;
        ok = 1 ;

        if ( depth <= 2 ) 
          begin
             ok = 0;
             $display ( "ERROR FIFOLevel.v: depth parameter must be greater than 2" ) ;
          end         

        if ( indxWidth <= 0 ) 
          begin
             ok = 0;
             $display ( "ERROR FIFOLevel.v: width parameter must be greater than 0" ) ;
          end         

        if ( ok == 0 ) $finish ;
                   
      end // initial begin
   // synopsys translate_on
`endif // BSV_NO_INITIAL_BLOCKS
   
endmodule

