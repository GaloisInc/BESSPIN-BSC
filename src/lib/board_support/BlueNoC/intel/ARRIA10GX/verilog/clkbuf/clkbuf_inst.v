	clkbuf u0 (
		.inclk  (_connected_to_inclk_),  //   input,  width = 1,  altclkctrl_input.inclk
		.ena    (_connected_to_ena_),    //   input,  width = 1,                  .ena
		.outclk (_connected_to_outclk_)  //  output,  width = 1, altclkctrl_output.outclk
	);

