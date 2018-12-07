
#**************************************************************
# Set False Path
#**************************************************************

set_false_path -from [get_clocks {scemi_iopll|_unnamed_|twentynm_pll|iopll_inst|outclk}] -to   [get_clocks {scemi_ep|pcie_a10_hip_0|wys~CORE_CLK_OUT}]
set_false_path -to   [get_clocks {scemi_iopll|_unnamed_|twentynm_pll|iopll_inst|outclk}] -from [get_clocks {scemi_ep|pcie_a10_hip_0|wys~CORE_CLK_OUT}]
