
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

module SceMiOutPipeProxyGet(CLK,
			    RST_N,

			    DATA_EN,
			    DATA,
			    DATA_RDY);
  parameter paramFile      = "";
  parameter transactorName = "";
  parameter portName       = "";
  parameter WIDTH          = 0;
  input  CLK;
  input  RST_N;

  // actionvalue method get
  input  DATA_EN;
  output [WIDTH - 1 : 0] DATA;
  output DATA_RDY;

  // signals for module outputs
  wire [WIDTH - 1 : 0] DATA;
  wire DATA_RDY;

  // ports of submodule proxy
  wire [WIDTH - 1 : 0] proxy$DATA;
  wire proxy$ACK, proxy$DATA_RDY, proxy$SHUTDOWN;

  // rule scheduling signals
  wire CAN_FIRE_get, WILL_FIRE_get;

  // actionvalue method get
  assign DATA = proxy$DATA ;
  assign DATA_RDY = proxy$DATA_RDY ;
  assign CAN_FIRE_get = proxy$DATA_RDY ;
  assign WILL_FIRE_get = DATA_EN ;

  // submodule proxy
  SceMiOutPipeProxyF #(.WIDTH(WIDTH),
		       .paramFile(paramFile),
		       .transactorName(transactorName),
		       .portName(portName)) proxy(.CLK(CLK),
						  .RST_N(RST_N),
						  .SHUTDOWN(proxy$SHUTDOWN),
						  .ACK(proxy$ACK),
						  .HAS_DATA(),
						  .DATA(proxy$DATA),
						  .DATA_RDY(proxy$DATA_RDY),
						  .SHUTDOWN_RDY());

  // submodule proxy
  assign proxy$SHUTDOWN = 1'b0 ;
  assign proxy$ACK = DATA_EN ;
endmodule

