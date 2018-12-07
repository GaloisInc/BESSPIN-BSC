
`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module SceMiOutPipeProxyF(CLK,
			  RST_N,

			  HAS_DATA,

			  DATA,
			  DATA_RDY,

			  SHUTDOWN,
			  SHUTDOWN_RDY,

			  ACK);
  parameter paramFile      = "";
  parameter transactorName = "";
  parameter portName       = "";
  parameter WIDTH          = 0;
  input  CLK;
  input  RST_N;

  // value method HAS_DATA
  output HAS_DATA;

  // value method DATA
  output [WIDTH - 1 : 0] DATA;
  output DATA_RDY;

  // action method shutdown
  input  SHUTDOWN;
  output SHUTDOWN_RDY;

  // action method ack
  input  ACK;

  // signals for module outputs
  wire [WIDTH - 1 : 0] DATA;
  wire DATA_RDY, SHUTDOWN_RDY, HAS_DATA;

  // register initialized
  reg initialized;
  wire initialized$D_IN, initialized$EN;

  // register proxy_index
  reg [31 : 0] proxy_index;
  wire [31 : 0] proxy_index$D_IN;
  wire proxy_index$EN;

  // ports of submodule fifo
  wire [WIDTH - 1 : 0] fifo$D_IN, fifo$D_OUT;
  wire fifo$CLR, fifo$DEQ, fifo$EMPTY_N, fifo$ENQ, fifo$FULL_N;

  // rule scheduling signals
  wire CAN_FIRE_RL_do_deq,
       CAN_FIRE_RL_poll,
       CAN_FIRE_RL_start,
       CAN_FIRE_ack,
       CAN_FIRE_shutdown,
       WILL_FIRE_RL_do_deq,
       WILL_FIRE_RL_poll,
       WILL_FIRE_RL_start,
       WILL_FIRE_ack,
       WILL_FIRE_shutdown;

  // remaining internal signals
  reg [WIDTH - 1 : 0] v__h409;
  reg [32 : 0] TASK_bsvscemi_bind_outpipe___d3;
  reg TASK_bsvscemi_outpipe_proxy_ready___d10;

  // value method HAS_DATA
  assign HAS_DATA = fifo$EMPTY_N ;

  // value method DATA
  assign DATA = fifo$D_OUT ;
  assign DATA_RDY = fifo$EMPTY_N ;

  // action method shutdown
  assign SHUTDOWN_RDY = 1'd1 ;
  assign CAN_FIRE_shutdown = 1'd1 ;
  assign WILL_FIRE_shutdown = SHUTDOWN ;

  // action method ack
  assign CAN_FIRE_ack = 1'd1 ;
  assign WILL_FIRE_ack = ACK ;

  // submodule fifo
  FIFO2 #(.width(WIDTH), .guarded(32'd1)) fifo(.RST(RST_N),
						.CLK(CLK),
						.D_IN(fifo$D_IN),
						.ENQ(fifo$ENQ),
						.DEQ(fifo$DEQ),
						.CLR(fifo$CLR),
						.D_OUT(fifo$D_OUT),
						.FULL_N(fifo$FULL_N),
						.EMPTY_N(fifo$EMPTY_N));

  // rule RL_start
  assign CAN_FIRE_RL_start = !initialized ;
  assign WILL_FIRE_RL_start = CAN_FIRE_RL_start ;

  // rule RL_poll
  assign CAN_FIRE_RL_poll = fifo$FULL_N && initialized ;
  assign WILL_FIRE_RL_poll = CAN_FIRE_RL_poll ;

  // rule RL_do_deq
  assign CAN_FIRE_RL_do_deq = fifo$EMPTY_N && ACK ;
  assign WILL_FIRE_RL_do_deq = CAN_FIRE_RL_do_deq ;

  // register initialized
  assign initialized$D_IN = 1'd1 ;
  assign initialized$EN =
	     WILL_FIRE_RL_start && TASK_bsvscemi_bind_outpipe___d3[32] ;

  // register proxy_index
  assign proxy_index$D_IN = TASK_bsvscemi_bind_outpipe___d3[31:0] ;
  assign proxy_index$EN =
	     WILL_FIRE_RL_start && TASK_bsvscemi_bind_outpipe___d3[32] ;

  // submodule fifo
  assign fifo$D_IN = v__h409 ;
  assign fifo$ENQ =
	     WILL_FIRE_RL_poll && TASK_bsvscemi_outpipe_proxy_ready___d10 ;
  assign fifo$DEQ = fifo$EMPTY_N && ACK ;
  assign fifo$CLR = 1'b0 ;

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        initialized <= `BSV_ASSIGNMENT_DELAY 1'd0;
	proxy_index <= `BSV_ASSIGNMENT_DELAY 32'd0;
      end
    else
      begin
        if (initialized$EN)
	  initialized <= `BSV_ASSIGNMENT_DELAY initialized$D_IN;
	if (proxy_index$EN)
	  proxy_index <= `BSV_ASSIGNMENT_DELAY proxy_index$D_IN;
      end
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    initialized = 1'h0;
    proxy_index = 32'hAAAAAAAA;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on

  // handling of system tasks

  // synopsys translate_off
  always@(negedge CLK)
  begin
    #0;
    if (SHUTDOWN) $imported_bsvscemi_shutdown;
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_start)
	begin
	  TASK_bsvscemi_bind_outpipe___d3 =
	      $imported_bsvscemi_bind_outpipe(paramFile,
					      transactorName,
					      portName);
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_start && !TASK_bsvscemi_bind_outpipe___d3[32])
	$finish(32'd0);
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_poll)
	begin
	  TASK_bsvscemi_outpipe_proxy_ready___d10 =
	      $imported_bsvscemi_outpipe_proxy_ready(proxy_index);
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_poll && TASK_bsvscemi_outpipe_proxy_ready___d10)
	$imported_bsvscemi_outpipe_proxy_data_get(v__h409,
						  WIDTH,
						  proxy_index);
  end
  // synopsys translate_on
endmodule

