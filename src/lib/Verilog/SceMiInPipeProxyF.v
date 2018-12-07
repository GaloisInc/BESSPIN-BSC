
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

module SceMiInPipeProxyF(CLK,
			RST_N,

			ACCEPT,

			DATA,
			DATA_EN,
			DATA_RDY);
  parameter paramFile      = "";
  parameter transactorName = "";
  parameter portName       = "";
  parameter WIDTH          = 0;
  input  CLK;
  input  RST_N;

  // value method ACCEPT
  output ACCEPT;

  // action method send
  input  [WIDTH - 1 : 0] DATA;
  input  DATA_EN;
  output DATA_RDY;

  // signals for module outputs
  wire DATA_RDY, ACCEPT;

  // inlined wires
  reg space_available$whas;

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
  reg CAN_FIRE_RL_send_data, WILL_FIRE_RL_send_data;
  wire CAN_FIRE_RL_check_buffer,
       CAN_FIRE_RL_start,
       CAN_FIRE_send,
       WILL_FIRE_RL_check_buffer,
       WILL_FIRE_RL_start,
       WILL_FIRE_send;

  // remaining internal signals
  reg [63 : 0] v__h350;
  reg [32 : 0] TASK_bsvscemi_bind_inpipe___d3;
  reg TASK_bsvscemi_inpipe_proxy_ready___d10;

  // value method ACCEPT
  assign ACCEPT = fifo$FULL_N ;

  // action method send
  assign DATA_RDY = fifo$FULL_N ;
  assign CAN_FIRE_send = fifo$FULL_N ;
  assign WILL_FIRE_send = DATA_EN ;

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

  // rule RL_check_buffer
  assign CAN_FIRE_RL_check_buffer = initialized ;
  assign WILL_FIRE_RL_check_buffer = initialized ;

  // rule RL_start
  assign CAN_FIRE_RL_start = !initialized ;
  assign WILL_FIRE_RL_start = CAN_FIRE_RL_start ;

  // register initialized
  assign initialized$D_IN = 1'd1 ;
  assign initialized$EN =
	     WILL_FIRE_RL_start && TASK_bsvscemi_bind_inpipe___d3[32] ;

  // register proxy_index
  assign proxy_index$D_IN = TASK_bsvscemi_bind_inpipe___d3[31:0] ;
  assign proxy_index$EN =
	     WILL_FIRE_RL_start && TASK_bsvscemi_bind_inpipe___d3[32] ;

  // submodule fifo
  assign fifo$D_IN = DATA ;
  assign fifo$ENQ = DATA_EN ;
  assign fifo$DEQ = CAN_FIRE_RL_send_data ;
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
    if (RST_N != `BSV_RESET_VALUE)
      if (initialized)
	begin
	  TASK_bsvscemi_inpipe_proxy_ready___d10 =
	      $imported_bsvscemi_inpipe_proxy_ready(proxy_index);
	  #0;
	end
    space_available$whas =
	initialized && TASK_bsvscemi_inpipe_proxy_ready___d10;
    CAN_FIRE_RL_send_data = fifo$EMPTY_N && space_available$whas;
    WILL_FIRE_RL_send_data = CAN_FIRE_RL_send_data;
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_send_data)
	$imported_bsvscemi_inpipe_proxy_send(fifo$D_OUT, 32'd53, proxy_index);
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_start)
	begin
	  TASK_bsvscemi_bind_inpipe___d3 =
	      $imported_bsvscemi_bind_inpipe(paramFile,
					     transactorName,
					     portName);
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_start && TASK_bsvscemi_bind_inpipe___d3[32])
	begin
	  v__h350 = $time;
	  #0;
	end
    // if (RST_N != `BSV_RESET_VALUE)
    //   if (WILL_FIRE_RL_start && TASK_bsvscemi_bind_inpipe___d3[32])
    // 	$display("(%0d) VALID (%0d)",
    // 		 v__h350,
    // 		 $unsigned(TASK_bsvscemi_bind_inpipe___d3[31:0]));
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_start && !TASK_bsvscemi_bind_inpipe___d3[32])
	$finish(32'd0);
  end
  // synopsys translate_on
endmodule  // inPipeSynth

