	component p1b is
		port (
			hpg_ctrler          : in  std_logic_vector(4 downto 0)   := (others => 'X'); -- hpg_ctrler
			tl_cfg_add          : out std_logic_vector(3 downto 0);                      -- tl_cfg_add
			tl_cfg_ctl          : out std_logic_vector(31 downto 0);                     -- tl_cfg_ctl
			tl_cfg_sts          : out std_logic_vector(52 downto 0);                     -- tl_cfg_sts
			cpl_err             : in  std_logic_vector(6 downto 0)   := (others => 'X'); -- cpl_err
			cpl_pending         : in  std_logic                      := 'X';             -- cpl_pending
			coreclkout_hip      : out std_logic;                                         -- clk
			currentspeed        : out std_logic_vector(1 downto 0);                      -- currentspeed
			test_in             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- test_in
			simu_mode_pipe      : in  std_logic                      := 'X';             -- simu_mode_pipe
			sim_pipe_pclk_in    : in  std_logic                      := 'X';             -- sim_pipe_pclk_in
			sim_pipe_rate       : out std_logic_vector(1 downto 0);                      -- sim_pipe_rate
			sim_ltssmstate      : out std_logic_vector(4 downto 0);                      -- sim_ltssmstate
			eidleinfersel0      : out std_logic_vector(2 downto 0);                      -- eidleinfersel0
			eidleinfersel1      : out std_logic_vector(2 downto 0);                      -- eidleinfersel1
			eidleinfersel2      : out std_logic_vector(2 downto 0);                      -- eidleinfersel2
			eidleinfersel3      : out std_logic_vector(2 downto 0);                      -- eidleinfersel3
			eidleinfersel4      : out std_logic_vector(2 downto 0);                      -- eidleinfersel4
			eidleinfersel5      : out std_logic_vector(2 downto 0);                      -- eidleinfersel5
			eidleinfersel6      : out std_logic_vector(2 downto 0);                      -- eidleinfersel6
			eidleinfersel7      : out std_logic_vector(2 downto 0);                      -- eidleinfersel7
			powerdown0          : out std_logic_vector(1 downto 0);                      -- powerdown0
			powerdown1          : out std_logic_vector(1 downto 0);                      -- powerdown1
			powerdown2          : out std_logic_vector(1 downto 0);                      -- powerdown2
			powerdown3          : out std_logic_vector(1 downto 0);                      -- powerdown3
			powerdown4          : out std_logic_vector(1 downto 0);                      -- powerdown4
			powerdown5          : out std_logic_vector(1 downto 0);                      -- powerdown5
			powerdown6          : out std_logic_vector(1 downto 0);                      -- powerdown6
			powerdown7          : out std_logic_vector(1 downto 0);                      -- powerdown7
			rxpolarity0         : out std_logic;                                         -- rxpolarity0
			rxpolarity1         : out std_logic;                                         -- rxpolarity1
			rxpolarity2         : out std_logic;                                         -- rxpolarity2
			rxpolarity3         : out std_logic;                                         -- rxpolarity3
			rxpolarity4         : out std_logic;                                         -- rxpolarity4
			rxpolarity5         : out std_logic;                                         -- rxpolarity5
			rxpolarity6         : out std_logic;                                         -- rxpolarity6
			rxpolarity7         : out std_logic;                                         -- rxpolarity7
			txcompl0            : out std_logic;                                         -- txcompl0
			txcompl1            : out std_logic;                                         -- txcompl1
			txcompl2            : out std_logic;                                         -- txcompl2
			txcompl3            : out std_logic;                                         -- txcompl3
			txcompl4            : out std_logic;                                         -- txcompl4
			txcompl5            : out std_logic;                                         -- txcompl5
			txcompl6            : out std_logic;                                         -- txcompl6
			txcompl7            : out std_logic;                                         -- txcompl7
			txdata0             : out std_logic_vector(31 downto 0);                     -- txdata0
			txdata1             : out std_logic_vector(31 downto 0);                     -- txdata1
			txdata2             : out std_logic_vector(31 downto 0);                     -- txdata2
			txdata3             : out std_logic_vector(31 downto 0);                     -- txdata3
			txdata4             : out std_logic_vector(31 downto 0);                     -- txdata4
			txdata5             : out std_logic_vector(31 downto 0);                     -- txdata5
			txdata6             : out std_logic_vector(31 downto 0);                     -- txdata6
			txdata7             : out std_logic_vector(31 downto 0);                     -- txdata7
			txdatak0            : out std_logic_vector(3 downto 0);                      -- txdatak0
			txdatak1            : out std_logic_vector(3 downto 0);                      -- txdatak1
			txdatak2            : out std_logic_vector(3 downto 0);                      -- txdatak2
			txdatak3            : out std_logic_vector(3 downto 0);                      -- txdatak3
			txdatak4            : out std_logic_vector(3 downto 0);                      -- txdatak4
			txdatak5            : out std_logic_vector(3 downto 0);                      -- txdatak5
			txdatak6            : out std_logic_vector(3 downto 0);                      -- txdatak6
			txdatak7            : out std_logic_vector(3 downto 0);                      -- txdatak7
			txdetectrx0         : out std_logic;                                         -- txdetectrx0
			txdetectrx1         : out std_logic;                                         -- txdetectrx1
			txdetectrx2         : out std_logic;                                         -- txdetectrx2
			txdetectrx3         : out std_logic;                                         -- txdetectrx3
			txdetectrx4         : out std_logic;                                         -- txdetectrx4
			txdetectrx5         : out std_logic;                                         -- txdetectrx5
			txdetectrx6         : out std_logic;                                         -- txdetectrx6
			txdetectrx7         : out std_logic;                                         -- txdetectrx7
			txelecidle0         : out std_logic;                                         -- txelecidle0
			txelecidle1         : out std_logic;                                         -- txelecidle1
			txelecidle2         : out std_logic;                                         -- txelecidle2
			txelecidle3         : out std_logic;                                         -- txelecidle3
			txelecidle4         : out std_logic;                                         -- txelecidle4
			txelecidle5         : out std_logic;                                         -- txelecidle5
			txelecidle6         : out std_logic;                                         -- txelecidle6
			txelecidle7         : out std_logic;                                         -- txelecidle7
			txdeemph0           : out std_logic;                                         -- txdeemph0
			txdeemph1           : out std_logic;                                         -- txdeemph1
			txdeemph2           : out std_logic;                                         -- txdeemph2
			txdeemph3           : out std_logic;                                         -- txdeemph3
			txdeemph4           : out std_logic;                                         -- txdeemph4
			txdeemph5           : out std_logic;                                         -- txdeemph5
			txdeemph6           : out std_logic;                                         -- txdeemph6
			txdeemph7           : out std_logic;                                         -- txdeemph7
			txmargin0           : out std_logic_vector(2 downto 0);                      -- txmargin0
			txmargin1           : out std_logic_vector(2 downto 0);                      -- txmargin1
			txmargin2           : out std_logic_vector(2 downto 0);                      -- txmargin2
			txmargin3           : out std_logic_vector(2 downto 0);                      -- txmargin3
			txmargin4           : out std_logic_vector(2 downto 0);                      -- txmargin4
			txmargin5           : out std_logic_vector(2 downto 0);                      -- txmargin5
			txmargin6           : out std_logic_vector(2 downto 0);                      -- txmargin6
			txmargin7           : out std_logic_vector(2 downto 0);                      -- txmargin7
			txswing0            : out std_logic;                                         -- txswing0
			txswing1            : out std_logic;                                         -- txswing1
			txswing2            : out std_logic;                                         -- txswing2
			txswing3            : out std_logic;                                         -- txswing3
			txswing4            : out std_logic;                                         -- txswing4
			txswing5            : out std_logic;                                         -- txswing5
			txswing6            : out std_logic;                                         -- txswing6
			txswing7            : out std_logic;                                         -- txswing7
			phystatus0          : in  std_logic                      := 'X';             -- phystatus0
			phystatus1          : in  std_logic                      := 'X';             -- phystatus1
			phystatus2          : in  std_logic                      := 'X';             -- phystatus2
			phystatus3          : in  std_logic                      := 'X';             -- phystatus3
			phystatus4          : in  std_logic                      := 'X';             -- phystatus4
			phystatus5          : in  std_logic                      := 'X';             -- phystatus5
			phystatus6          : in  std_logic                      := 'X';             -- phystatus6
			phystatus7          : in  std_logic                      := 'X';             -- phystatus7
			rxdata0             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- rxdata0
			rxdata1             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- rxdata1
			rxdata2             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- rxdata2
			rxdata3             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- rxdata3
			rxdata4             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- rxdata4
			rxdata5             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- rxdata5
			rxdata6             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- rxdata6
			rxdata7             : in  std_logic_vector(31 downto 0)  := (others => 'X'); -- rxdata7
			rxdatak0            : in  std_logic_vector(3 downto 0)   := (others => 'X'); -- rxdatak0
			rxdatak1            : in  std_logic_vector(3 downto 0)   := (others => 'X'); -- rxdatak1
			rxdatak2            : in  std_logic_vector(3 downto 0)   := (others => 'X'); -- rxdatak2
			rxdatak3            : in  std_logic_vector(3 downto 0)   := (others => 'X'); -- rxdatak3
			rxdatak4            : in  std_logic_vector(3 downto 0)   := (others => 'X'); -- rxdatak4
			rxdatak5            : in  std_logic_vector(3 downto 0)   := (others => 'X'); -- rxdatak5
			rxdatak6            : in  std_logic_vector(3 downto 0)   := (others => 'X'); -- rxdatak6
			rxdatak7            : in  std_logic_vector(3 downto 0)   := (others => 'X'); -- rxdatak7
			rxelecidle0         : in  std_logic                      := 'X';             -- rxelecidle0
			rxelecidle1         : in  std_logic                      := 'X';             -- rxelecidle1
			rxelecidle2         : in  std_logic                      := 'X';             -- rxelecidle2
			rxelecidle3         : in  std_logic                      := 'X';             -- rxelecidle3
			rxelecidle4         : in  std_logic                      := 'X';             -- rxelecidle4
			rxelecidle5         : in  std_logic                      := 'X';             -- rxelecidle5
			rxelecidle6         : in  std_logic                      := 'X';             -- rxelecidle6
			rxelecidle7         : in  std_logic                      := 'X';             -- rxelecidle7
			rxstatus0           : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- rxstatus0
			rxstatus1           : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- rxstatus1
			rxstatus2           : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- rxstatus2
			rxstatus3           : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- rxstatus3
			rxstatus4           : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- rxstatus4
			rxstatus5           : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- rxstatus5
			rxstatus6           : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- rxstatus6
			rxstatus7           : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- rxstatus7
			rxvalid0            : in  std_logic                      := 'X';             -- rxvalid0
			rxvalid1            : in  std_logic                      := 'X';             -- rxvalid1
			rxvalid2            : in  std_logic                      := 'X';             -- rxvalid2
			rxvalid3            : in  std_logic                      := 'X';             -- rxvalid3
			rxvalid4            : in  std_logic                      := 'X';             -- rxvalid4
			rxvalid5            : in  std_logic                      := 'X';             -- rxvalid5
			rxvalid6            : in  std_logic                      := 'X';             -- rxvalid6
			rxvalid7            : in  std_logic                      := 'X';             -- rxvalid7
			rxdataskip0         : in  std_logic                      := 'X';             -- rxdataskip0
			rxdataskip1         : in  std_logic                      := 'X';             -- rxdataskip1
			rxdataskip2         : in  std_logic                      := 'X';             -- rxdataskip2
			rxdataskip3         : in  std_logic                      := 'X';             -- rxdataskip3
			rxdataskip4         : in  std_logic                      := 'X';             -- rxdataskip4
			rxdataskip5         : in  std_logic                      := 'X';             -- rxdataskip5
			rxdataskip6         : in  std_logic                      := 'X';             -- rxdataskip6
			rxdataskip7         : in  std_logic                      := 'X';             -- rxdataskip7
			rxblkst0            : in  std_logic                      := 'X';             -- rxblkst0
			rxblkst1            : in  std_logic                      := 'X';             -- rxblkst1
			rxblkst2            : in  std_logic                      := 'X';             -- rxblkst2
			rxblkst3            : in  std_logic                      := 'X';             -- rxblkst3
			rxblkst4            : in  std_logic                      := 'X';             -- rxblkst4
			rxblkst5            : in  std_logic                      := 'X';             -- rxblkst5
			rxblkst6            : in  std_logic                      := 'X';             -- rxblkst6
			rxblkst7            : in  std_logic                      := 'X';             -- rxblkst7
			rxsynchd0           : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- rxsynchd0
			rxsynchd1           : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- rxsynchd1
			rxsynchd2           : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- rxsynchd2
			rxsynchd3           : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- rxsynchd3
			rxsynchd4           : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- rxsynchd4
			rxsynchd5           : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- rxsynchd5
			rxsynchd6           : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- rxsynchd6
			rxsynchd7           : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- rxsynchd7
			currentcoeff0       : out std_logic_vector(17 downto 0);                     -- currentcoeff0
			currentcoeff1       : out std_logic_vector(17 downto 0);                     -- currentcoeff1
			currentcoeff2       : out std_logic_vector(17 downto 0);                     -- currentcoeff2
			currentcoeff3       : out std_logic_vector(17 downto 0);                     -- currentcoeff3
			currentcoeff4       : out std_logic_vector(17 downto 0);                     -- currentcoeff4
			currentcoeff5       : out std_logic_vector(17 downto 0);                     -- currentcoeff5
			currentcoeff6       : out std_logic_vector(17 downto 0);                     -- currentcoeff6
			currentcoeff7       : out std_logic_vector(17 downto 0);                     -- currentcoeff7
			currentrxpreset0    : out std_logic_vector(2 downto 0);                      -- currentrxpreset0
			currentrxpreset1    : out std_logic_vector(2 downto 0);                      -- currentrxpreset1
			currentrxpreset2    : out std_logic_vector(2 downto 0);                      -- currentrxpreset2
			currentrxpreset3    : out std_logic_vector(2 downto 0);                      -- currentrxpreset3
			currentrxpreset4    : out std_logic_vector(2 downto 0);                      -- currentrxpreset4
			currentrxpreset5    : out std_logic_vector(2 downto 0);                      -- currentrxpreset5
			currentrxpreset6    : out std_logic_vector(2 downto 0);                      -- currentrxpreset6
			currentrxpreset7    : out std_logic_vector(2 downto 0);                      -- currentrxpreset7
			txsynchd0           : out std_logic_vector(1 downto 0);                      -- txsynchd0
			txsynchd1           : out std_logic_vector(1 downto 0);                      -- txsynchd1
			txsynchd2           : out std_logic_vector(1 downto 0);                      -- txsynchd2
			txsynchd3           : out std_logic_vector(1 downto 0);                      -- txsynchd3
			txsynchd4           : out std_logic_vector(1 downto 0);                      -- txsynchd4
			txsynchd5           : out std_logic_vector(1 downto 0);                      -- txsynchd5
			txsynchd6           : out std_logic_vector(1 downto 0);                      -- txsynchd6
			txsynchd7           : out std_logic_vector(1 downto 0);                      -- txsynchd7
			txblkst0            : out std_logic;                                         -- txblkst0
			txblkst1            : out std_logic;                                         -- txblkst1
			txblkst2            : out std_logic;                                         -- txblkst2
			txblkst3            : out std_logic;                                         -- txblkst3
			txblkst4            : out std_logic;                                         -- txblkst4
			txblkst5            : out std_logic;                                         -- txblkst5
			txblkst6            : out std_logic;                                         -- txblkst6
			txblkst7            : out std_logic;                                         -- txblkst7
			txdataskip0         : out std_logic;                                         -- txdataskip0
			txdataskip1         : out std_logic;                                         -- txdataskip1
			txdataskip2         : out std_logic;                                         -- txdataskip2
			txdataskip3         : out std_logic;                                         -- txdataskip3
			txdataskip4         : out std_logic;                                         -- txdataskip4
			txdataskip5         : out std_logic;                                         -- txdataskip5
			txdataskip6         : out std_logic;                                         -- txdataskip6
			txdataskip7         : out std_logic;                                         -- txdataskip7
			rate0               : out std_logic_vector(1 downto 0);                      -- rate0
			rate1               : out std_logic_vector(1 downto 0);                      -- rate1
			rate2               : out std_logic_vector(1 downto 0);                      -- rate2
			rate3               : out std_logic_vector(1 downto 0);                      -- rate3
			rate4               : out std_logic_vector(1 downto 0);                      -- rate4
			rate5               : out std_logic_vector(1 downto 0);                      -- rate5
			rate6               : out std_logic_vector(1 downto 0);                      -- rate6
			rate7               : out std_logic_vector(1 downto 0);                      -- rate7
			pld_core_ready      : in  std_logic                      := 'X';             -- pld_core_ready
			pld_clk_inuse       : out std_logic;                                         -- pld_clk_inuse
			serdes_pll_locked   : out std_logic;                                         -- serdes_pll_locked
			reset_status        : out std_logic;                                         -- reset_status
			testin_zero         : out std_logic;                                         -- testin_zero
			rx_in0              : in  std_logic                      := 'X';             -- rx_in0
			rx_in1              : in  std_logic                      := 'X';             -- rx_in1
			rx_in2              : in  std_logic                      := 'X';             -- rx_in2
			rx_in3              : in  std_logic                      := 'X';             -- rx_in3
			rx_in4              : in  std_logic                      := 'X';             -- rx_in4
			rx_in5              : in  std_logic                      := 'X';             -- rx_in5
			rx_in6              : in  std_logic                      := 'X';             -- rx_in6
			rx_in7              : in  std_logic                      := 'X';             -- rx_in7
			tx_out0             : out std_logic;                                         -- tx_out0
			tx_out1             : out std_logic;                                         -- tx_out1
			tx_out2             : out std_logic;                                         -- tx_out2
			tx_out3             : out std_logic;                                         -- tx_out3
			tx_out4             : out std_logic;                                         -- tx_out4
			tx_out5             : out std_logic;                                         -- tx_out5
			tx_out6             : out std_logic;                                         -- tx_out6
			tx_out7             : out std_logic;                                         -- tx_out7
			derr_cor_ext_rcv    : out std_logic;                                         -- derr_cor_ext_rcv
			derr_cor_ext_rpl    : out std_logic;                                         -- derr_cor_ext_rpl
			derr_rpl            : out std_logic;                                         -- derr_rpl
			dlup                : out std_logic;                                         -- dlup
			dlup_exit           : out std_logic;                                         -- dlup_exit
			ev128ns             : out std_logic;                                         -- ev128ns
			ev1us               : out std_logic;                                         -- ev1us
			hotrst_exit         : out std_logic;                                         -- hotrst_exit
			int_status          : out std_logic_vector(3 downto 0);                      -- int_status
			l2_exit             : out std_logic;                                         -- l2_exit
			lane_act            : out std_logic_vector(3 downto 0);                      -- lane_act
			ltssmstate          : out std_logic_vector(4 downto 0);                      -- ltssmstate
			rx_par_err          : out std_logic;                                         -- rx_par_err
			tx_par_err          : out std_logic_vector(1 downto 0);                      -- tx_par_err
			cfg_par_err         : out std_logic;                                         -- cfg_par_err
			ko_cpl_spc_header   : out std_logic_vector(7 downto 0);                      -- ko_cpl_spc_header
			ko_cpl_spc_data     : out std_logic_vector(11 downto 0);                     -- ko_cpl_spc_data
			app_int_sts         : in  std_logic                      := 'X';             -- app_int_sts
			app_int_ack         : out std_logic;                                         -- app_int_ack
			app_msi_num         : in  std_logic_vector(4 downto 0)   := (others => 'X'); -- app_msi_num
			app_msi_req         : in  std_logic                      := 'X';             -- app_msi_req
			app_msi_tc          : in  std_logic_vector(2 downto 0)   := (others => 'X'); -- app_msi_tc
			app_msi_ack         : out std_logic;                                         -- app_msi_ack
			npor                : in  std_logic                      := 'X';             -- npor
			pin_perst           : in  std_logic                      := 'X';             -- pin_perst
			pld_clk             : in  std_logic                      := 'X';             -- clk
			pm_auxpwr           : in  std_logic                      := 'X';             -- pm_auxpwr
			pm_data             : in  std_logic_vector(9 downto 0)   := (others => 'X'); -- pm_data
			pme_to_cr           : in  std_logic                      := 'X';             -- pme_to_cr
			pm_event            : in  std_logic                      := 'X';             -- pm_event
			pme_to_sr           : out std_logic;                                         -- pme_to_sr
			refclk              : in  std_logic                      := 'X';             -- clk
			rx_st_bar           : out std_logic_vector(7 downto 0);                      -- rx_st_bar
			rx_st_mask          : in  std_logic                      := 'X';             -- rx_st_mask
			rx_st_sop           : out std_logic_vector(0 downto 0);                      -- startofpacket
			rx_st_eop           : out std_logic_vector(0 downto 0);                      -- endofpacket
			rx_st_err           : out std_logic_vector(0 downto 0);                      -- error
			rx_st_valid         : out std_logic_vector(0 downto 0);                      -- valid
			rx_st_ready         : in  std_logic                      := 'X';             -- ready
			rx_st_data          : out std_logic_vector(127 downto 0);                    -- data
			rx_st_empty         : out std_logic_vector(0 downto 0);                      -- empty
			tx_cred_data_fc     : out std_logic_vector(11 downto 0);                     -- tx_cred_data_fc
			tx_cred_fc_hip_cons : out std_logic_vector(5 downto 0);                      -- tx_cred_fc_hip_cons
			tx_cred_fc_infinite : out std_logic_vector(5 downto 0);                      -- tx_cred_fc_infinite
			tx_cred_hdr_fc      : out std_logic_vector(7 downto 0);                      -- tx_cred_hdr_fc
			tx_cred_fc_sel      : in  std_logic_vector(1 downto 0)   := (others => 'X'); -- tx_cred_fc_sel
			tx_st_sop           : in  std_logic_vector(0 downto 0)   := (others => 'X'); -- startofpacket
			tx_st_eop           : in  std_logic_vector(0 downto 0)   := (others => 'X'); -- endofpacket
			tx_st_err           : in  std_logic_vector(0 downto 0)   := (others => 'X'); -- error
			tx_st_valid         : in  std_logic_vector(0 downto 0)   := (others => 'X'); -- valid
			tx_st_ready         : out std_logic;                                         -- ready
			tx_st_data          : in  std_logic_vector(127 downto 0) := (others => 'X'); -- data
			tx_st_empty         : in  std_logic_vector(0 downto 0)   := (others => 'X')  -- empty
		);
	end component p1b;

	u0 : component p1b
		port map (
			hpg_ctrler          => CONNECTED_TO_hpg_ctrler,          --      config_tl.hpg_ctrler
			tl_cfg_add          => CONNECTED_TO_tl_cfg_add,          --               .tl_cfg_add
			tl_cfg_ctl          => CONNECTED_TO_tl_cfg_ctl,          --               .tl_cfg_ctl
			tl_cfg_sts          => CONNECTED_TO_tl_cfg_sts,          --               .tl_cfg_sts
			cpl_err             => CONNECTED_TO_cpl_err,             --               .cpl_err
			cpl_pending         => CONNECTED_TO_cpl_pending,         --               .cpl_pending
			coreclkout_hip      => CONNECTED_TO_coreclkout_hip,      -- coreclkout_hip.clk
			currentspeed        => CONNECTED_TO_currentspeed,        --   currentspeed.currentspeed
			test_in             => CONNECTED_TO_test_in,             --       hip_ctrl.test_in
			simu_mode_pipe      => CONNECTED_TO_simu_mode_pipe,      --               .simu_mode_pipe
			sim_pipe_pclk_in    => CONNECTED_TO_sim_pipe_pclk_in,    --       hip_pipe.sim_pipe_pclk_in
			sim_pipe_rate       => CONNECTED_TO_sim_pipe_rate,       --               .sim_pipe_rate
			sim_ltssmstate      => CONNECTED_TO_sim_ltssmstate,      --               .sim_ltssmstate
			eidleinfersel0      => CONNECTED_TO_eidleinfersel0,      --               .eidleinfersel0
			eidleinfersel1      => CONNECTED_TO_eidleinfersel1,      --               .eidleinfersel1
			eidleinfersel2      => CONNECTED_TO_eidleinfersel2,      --               .eidleinfersel2
			eidleinfersel3      => CONNECTED_TO_eidleinfersel3,      --               .eidleinfersel3
			eidleinfersel4      => CONNECTED_TO_eidleinfersel4,      --               .eidleinfersel4
			eidleinfersel5      => CONNECTED_TO_eidleinfersel5,      --               .eidleinfersel5
			eidleinfersel6      => CONNECTED_TO_eidleinfersel6,      --               .eidleinfersel6
			eidleinfersel7      => CONNECTED_TO_eidleinfersel7,      --               .eidleinfersel7
			powerdown0          => CONNECTED_TO_powerdown0,          --               .powerdown0
			powerdown1          => CONNECTED_TO_powerdown1,          --               .powerdown1
			powerdown2          => CONNECTED_TO_powerdown2,          --               .powerdown2
			powerdown3          => CONNECTED_TO_powerdown3,          --               .powerdown3
			powerdown4          => CONNECTED_TO_powerdown4,          --               .powerdown4
			powerdown5          => CONNECTED_TO_powerdown5,          --               .powerdown5
			powerdown6          => CONNECTED_TO_powerdown6,          --               .powerdown6
			powerdown7          => CONNECTED_TO_powerdown7,          --               .powerdown7
			rxpolarity0         => CONNECTED_TO_rxpolarity0,         --               .rxpolarity0
			rxpolarity1         => CONNECTED_TO_rxpolarity1,         --               .rxpolarity1
			rxpolarity2         => CONNECTED_TO_rxpolarity2,         --               .rxpolarity2
			rxpolarity3         => CONNECTED_TO_rxpolarity3,         --               .rxpolarity3
			rxpolarity4         => CONNECTED_TO_rxpolarity4,         --               .rxpolarity4
			rxpolarity5         => CONNECTED_TO_rxpolarity5,         --               .rxpolarity5
			rxpolarity6         => CONNECTED_TO_rxpolarity6,         --               .rxpolarity6
			rxpolarity7         => CONNECTED_TO_rxpolarity7,         --               .rxpolarity7
			txcompl0            => CONNECTED_TO_txcompl0,            --               .txcompl0
			txcompl1            => CONNECTED_TO_txcompl1,            --               .txcompl1
			txcompl2            => CONNECTED_TO_txcompl2,            --               .txcompl2
			txcompl3            => CONNECTED_TO_txcompl3,            --               .txcompl3
			txcompl4            => CONNECTED_TO_txcompl4,            --               .txcompl4
			txcompl5            => CONNECTED_TO_txcompl5,            --               .txcompl5
			txcompl6            => CONNECTED_TO_txcompl6,            --               .txcompl6
			txcompl7            => CONNECTED_TO_txcompl7,            --               .txcompl7
			txdata0             => CONNECTED_TO_txdata0,             --               .txdata0
			txdata1             => CONNECTED_TO_txdata1,             --               .txdata1
			txdata2             => CONNECTED_TO_txdata2,             --               .txdata2
			txdata3             => CONNECTED_TO_txdata3,             --               .txdata3
			txdata4             => CONNECTED_TO_txdata4,             --               .txdata4
			txdata5             => CONNECTED_TO_txdata5,             --               .txdata5
			txdata6             => CONNECTED_TO_txdata6,             --               .txdata6
			txdata7             => CONNECTED_TO_txdata7,             --               .txdata7
			txdatak0            => CONNECTED_TO_txdatak0,            --               .txdatak0
			txdatak1            => CONNECTED_TO_txdatak1,            --               .txdatak1
			txdatak2            => CONNECTED_TO_txdatak2,            --               .txdatak2
			txdatak3            => CONNECTED_TO_txdatak3,            --               .txdatak3
			txdatak4            => CONNECTED_TO_txdatak4,            --               .txdatak4
			txdatak5            => CONNECTED_TO_txdatak5,            --               .txdatak5
			txdatak6            => CONNECTED_TO_txdatak6,            --               .txdatak6
			txdatak7            => CONNECTED_TO_txdatak7,            --               .txdatak7
			txdetectrx0         => CONNECTED_TO_txdetectrx0,         --               .txdetectrx0
			txdetectrx1         => CONNECTED_TO_txdetectrx1,         --               .txdetectrx1
			txdetectrx2         => CONNECTED_TO_txdetectrx2,         --               .txdetectrx2
			txdetectrx3         => CONNECTED_TO_txdetectrx3,         --               .txdetectrx3
			txdetectrx4         => CONNECTED_TO_txdetectrx4,         --               .txdetectrx4
			txdetectrx5         => CONNECTED_TO_txdetectrx5,         --               .txdetectrx5
			txdetectrx6         => CONNECTED_TO_txdetectrx6,         --               .txdetectrx6
			txdetectrx7         => CONNECTED_TO_txdetectrx7,         --               .txdetectrx7
			txelecidle0         => CONNECTED_TO_txelecidle0,         --               .txelecidle0
			txelecidle1         => CONNECTED_TO_txelecidle1,         --               .txelecidle1
			txelecidle2         => CONNECTED_TO_txelecidle2,         --               .txelecidle2
			txelecidle3         => CONNECTED_TO_txelecidle3,         --               .txelecidle3
			txelecidle4         => CONNECTED_TO_txelecidle4,         --               .txelecidle4
			txelecidle5         => CONNECTED_TO_txelecidle5,         --               .txelecidle5
			txelecidle6         => CONNECTED_TO_txelecidle6,         --               .txelecidle6
			txelecidle7         => CONNECTED_TO_txelecidle7,         --               .txelecidle7
			txdeemph0           => CONNECTED_TO_txdeemph0,           --               .txdeemph0
			txdeemph1           => CONNECTED_TO_txdeemph1,           --               .txdeemph1
			txdeemph2           => CONNECTED_TO_txdeemph2,           --               .txdeemph2
			txdeemph3           => CONNECTED_TO_txdeemph3,           --               .txdeemph3
			txdeemph4           => CONNECTED_TO_txdeemph4,           --               .txdeemph4
			txdeemph5           => CONNECTED_TO_txdeemph5,           --               .txdeemph5
			txdeemph6           => CONNECTED_TO_txdeemph6,           --               .txdeemph6
			txdeemph7           => CONNECTED_TO_txdeemph7,           --               .txdeemph7
			txmargin0           => CONNECTED_TO_txmargin0,           --               .txmargin0
			txmargin1           => CONNECTED_TO_txmargin1,           --               .txmargin1
			txmargin2           => CONNECTED_TO_txmargin2,           --               .txmargin2
			txmargin3           => CONNECTED_TO_txmargin3,           --               .txmargin3
			txmargin4           => CONNECTED_TO_txmargin4,           --               .txmargin4
			txmargin5           => CONNECTED_TO_txmargin5,           --               .txmargin5
			txmargin6           => CONNECTED_TO_txmargin6,           --               .txmargin6
			txmargin7           => CONNECTED_TO_txmargin7,           --               .txmargin7
			txswing0            => CONNECTED_TO_txswing0,            --               .txswing0
			txswing1            => CONNECTED_TO_txswing1,            --               .txswing1
			txswing2            => CONNECTED_TO_txswing2,            --               .txswing2
			txswing3            => CONNECTED_TO_txswing3,            --               .txswing3
			txswing4            => CONNECTED_TO_txswing4,            --               .txswing4
			txswing5            => CONNECTED_TO_txswing5,            --               .txswing5
			txswing6            => CONNECTED_TO_txswing6,            --               .txswing6
			txswing7            => CONNECTED_TO_txswing7,            --               .txswing7
			phystatus0          => CONNECTED_TO_phystatus0,          --               .phystatus0
			phystatus1          => CONNECTED_TO_phystatus1,          --               .phystatus1
			phystatus2          => CONNECTED_TO_phystatus2,          --               .phystatus2
			phystatus3          => CONNECTED_TO_phystatus3,          --               .phystatus3
			phystatus4          => CONNECTED_TO_phystatus4,          --               .phystatus4
			phystatus5          => CONNECTED_TO_phystatus5,          --               .phystatus5
			phystatus6          => CONNECTED_TO_phystatus6,          --               .phystatus6
			phystatus7          => CONNECTED_TO_phystatus7,          --               .phystatus7
			rxdata0             => CONNECTED_TO_rxdata0,             --               .rxdata0
			rxdata1             => CONNECTED_TO_rxdata1,             --               .rxdata1
			rxdata2             => CONNECTED_TO_rxdata2,             --               .rxdata2
			rxdata3             => CONNECTED_TO_rxdata3,             --               .rxdata3
			rxdata4             => CONNECTED_TO_rxdata4,             --               .rxdata4
			rxdata5             => CONNECTED_TO_rxdata5,             --               .rxdata5
			rxdata6             => CONNECTED_TO_rxdata6,             --               .rxdata6
			rxdata7             => CONNECTED_TO_rxdata7,             --               .rxdata7
			rxdatak0            => CONNECTED_TO_rxdatak0,            --               .rxdatak0
			rxdatak1            => CONNECTED_TO_rxdatak1,            --               .rxdatak1
			rxdatak2            => CONNECTED_TO_rxdatak2,            --               .rxdatak2
			rxdatak3            => CONNECTED_TO_rxdatak3,            --               .rxdatak3
			rxdatak4            => CONNECTED_TO_rxdatak4,            --               .rxdatak4
			rxdatak5            => CONNECTED_TO_rxdatak5,            --               .rxdatak5
			rxdatak6            => CONNECTED_TO_rxdatak6,            --               .rxdatak6
			rxdatak7            => CONNECTED_TO_rxdatak7,            --               .rxdatak7
			rxelecidle0         => CONNECTED_TO_rxelecidle0,         --               .rxelecidle0
			rxelecidle1         => CONNECTED_TO_rxelecidle1,         --               .rxelecidle1
			rxelecidle2         => CONNECTED_TO_rxelecidle2,         --               .rxelecidle2
			rxelecidle3         => CONNECTED_TO_rxelecidle3,         --               .rxelecidle3
			rxelecidle4         => CONNECTED_TO_rxelecidle4,         --               .rxelecidle4
			rxelecidle5         => CONNECTED_TO_rxelecidle5,         --               .rxelecidle5
			rxelecidle6         => CONNECTED_TO_rxelecidle6,         --               .rxelecidle6
			rxelecidle7         => CONNECTED_TO_rxelecidle7,         --               .rxelecidle7
			rxstatus0           => CONNECTED_TO_rxstatus0,           --               .rxstatus0
			rxstatus1           => CONNECTED_TO_rxstatus1,           --               .rxstatus1
			rxstatus2           => CONNECTED_TO_rxstatus2,           --               .rxstatus2
			rxstatus3           => CONNECTED_TO_rxstatus3,           --               .rxstatus3
			rxstatus4           => CONNECTED_TO_rxstatus4,           --               .rxstatus4
			rxstatus5           => CONNECTED_TO_rxstatus5,           --               .rxstatus5
			rxstatus6           => CONNECTED_TO_rxstatus6,           --               .rxstatus6
			rxstatus7           => CONNECTED_TO_rxstatus7,           --               .rxstatus7
			rxvalid0            => CONNECTED_TO_rxvalid0,            --               .rxvalid0
			rxvalid1            => CONNECTED_TO_rxvalid1,            --               .rxvalid1
			rxvalid2            => CONNECTED_TO_rxvalid2,            --               .rxvalid2
			rxvalid3            => CONNECTED_TO_rxvalid3,            --               .rxvalid3
			rxvalid4            => CONNECTED_TO_rxvalid4,            --               .rxvalid4
			rxvalid5            => CONNECTED_TO_rxvalid5,            --               .rxvalid5
			rxvalid6            => CONNECTED_TO_rxvalid6,            --               .rxvalid6
			rxvalid7            => CONNECTED_TO_rxvalid7,            --               .rxvalid7
			rxdataskip0         => CONNECTED_TO_rxdataskip0,         --               .rxdataskip0
			rxdataskip1         => CONNECTED_TO_rxdataskip1,         --               .rxdataskip1
			rxdataskip2         => CONNECTED_TO_rxdataskip2,         --               .rxdataskip2
			rxdataskip3         => CONNECTED_TO_rxdataskip3,         --               .rxdataskip3
			rxdataskip4         => CONNECTED_TO_rxdataskip4,         --               .rxdataskip4
			rxdataskip5         => CONNECTED_TO_rxdataskip5,         --               .rxdataskip5
			rxdataskip6         => CONNECTED_TO_rxdataskip6,         --               .rxdataskip6
			rxdataskip7         => CONNECTED_TO_rxdataskip7,         --               .rxdataskip7
			rxblkst0            => CONNECTED_TO_rxblkst0,            --               .rxblkst0
			rxblkst1            => CONNECTED_TO_rxblkst1,            --               .rxblkst1
			rxblkst2            => CONNECTED_TO_rxblkst2,            --               .rxblkst2
			rxblkst3            => CONNECTED_TO_rxblkst3,            --               .rxblkst3
			rxblkst4            => CONNECTED_TO_rxblkst4,            --               .rxblkst4
			rxblkst5            => CONNECTED_TO_rxblkst5,            --               .rxblkst5
			rxblkst6            => CONNECTED_TO_rxblkst6,            --               .rxblkst6
			rxblkst7            => CONNECTED_TO_rxblkst7,            --               .rxblkst7
			rxsynchd0           => CONNECTED_TO_rxsynchd0,           --               .rxsynchd0
			rxsynchd1           => CONNECTED_TO_rxsynchd1,           --               .rxsynchd1
			rxsynchd2           => CONNECTED_TO_rxsynchd2,           --               .rxsynchd2
			rxsynchd3           => CONNECTED_TO_rxsynchd3,           --               .rxsynchd3
			rxsynchd4           => CONNECTED_TO_rxsynchd4,           --               .rxsynchd4
			rxsynchd5           => CONNECTED_TO_rxsynchd5,           --               .rxsynchd5
			rxsynchd6           => CONNECTED_TO_rxsynchd6,           --               .rxsynchd6
			rxsynchd7           => CONNECTED_TO_rxsynchd7,           --               .rxsynchd7
			currentcoeff0       => CONNECTED_TO_currentcoeff0,       --               .currentcoeff0
			currentcoeff1       => CONNECTED_TO_currentcoeff1,       --               .currentcoeff1
			currentcoeff2       => CONNECTED_TO_currentcoeff2,       --               .currentcoeff2
			currentcoeff3       => CONNECTED_TO_currentcoeff3,       --               .currentcoeff3
			currentcoeff4       => CONNECTED_TO_currentcoeff4,       --               .currentcoeff4
			currentcoeff5       => CONNECTED_TO_currentcoeff5,       --               .currentcoeff5
			currentcoeff6       => CONNECTED_TO_currentcoeff6,       --               .currentcoeff6
			currentcoeff7       => CONNECTED_TO_currentcoeff7,       --               .currentcoeff7
			currentrxpreset0    => CONNECTED_TO_currentrxpreset0,    --               .currentrxpreset0
			currentrxpreset1    => CONNECTED_TO_currentrxpreset1,    --               .currentrxpreset1
			currentrxpreset2    => CONNECTED_TO_currentrxpreset2,    --               .currentrxpreset2
			currentrxpreset3    => CONNECTED_TO_currentrxpreset3,    --               .currentrxpreset3
			currentrxpreset4    => CONNECTED_TO_currentrxpreset4,    --               .currentrxpreset4
			currentrxpreset5    => CONNECTED_TO_currentrxpreset5,    --               .currentrxpreset5
			currentrxpreset6    => CONNECTED_TO_currentrxpreset6,    --               .currentrxpreset6
			currentrxpreset7    => CONNECTED_TO_currentrxpreset7,    --               .currentrxpreset7
			txsynchd0           => CONNECTED_TO_txsynchd0,           --               .txsynchd0
			txsynchd1           => CONNECTED_TO_txsynchd1,           --               .txsynchd1
			txsynchd2           => CONNECTED_TO_txsynchd2,           --               .txsynchd2
			txsynchd3           => CONNECTED_TO_txsynchd3,           --               .txsynchd3
			txsynchd4           => CONNECTED_TO_txsynchd4,           --               .txsynchd4
			txsynchd5           => CONNECTED_TO_txsynchd5,           --               .txsynchd5
			txsynchd6           => CONNECTED_TO_txsynchd6,           --               .txsynchd6
			txsynchd7           => CONNECTED_TO_txsynchd7,           --               .txsynchd7
			txblkst0            => CONNECTED_TO_txblkst0,            --               .txblkst0
			txblkst1            => CONNECTED_TO_txblkst1,            --               .txblkst1
			txblkst2            => CONNECTED_TO_txblkst2,            --               .txblkst2
			txblkst3            => CONNECTED_TO_txblkst3,            --               .txblkst3
			txblkst4            => CONNECTED_TO_txblkst4,            --               .txblkst4
			txblkst5            => CONNECTED_TO_txblkst5,            --               .txblkst5
			txblkst6            => CONNECTED_TO_txblkst6,            --               .txblkst6
			txblkst7            => CONNECTED_TO_txblkst7,            --               .txblkst7
			txdataskip0         => CONNECTED_TO_txdataskip0,         --               .txdataskip0
			txdataskip1         => CONNECTED_TO_txdataskip1,         --               .txdataskip1
			txdataskip2         => CONNECTED_TO_txdataskip2,         --               .txdataskip2
			txdataskip3         => CONNECTED_TO_txdataskip3,         --               .txdataskip3
			txdataskip4         => CONNECTED_TO_txdataskip4,         --               .txdataskip4
			txdataskip5         => CONNECTED_TO_txdataskip5,         --               .txdataskip5
			txdataskip6         => CONNECTED_TO_txdataskip6,         --               .txdataskip6
			txdataskip7         => CONNECTED_TO_txdataskip7,         --               .txdataskip7
			rate0               => CONNECTED_TO_rate0,               --               .rate0
			rate1               => CONNECTED_TO_rate1,               --               .rate1
			rate2               => CONNECTED_TO_rate2,               --               .rate2
			rate3               => CONNECTED_TO_rate3,               --               .rate3
			rate4               => CONNECTED_TO_rate4,               --               .rate4
			rate5               => CONNECTED_TO_rate5,               --               .rate5
			rate6               => CONNECTED_TO_rate6,               --               .rate6
			rate7               => CONNECTED_TO_rate7,               --               .rate7
			pld_core_ready      => CONNECTED_TO_pld_core_ready,      --        hip_rst.pld_core_ready
			pld_clk_inuse       => CONNECTED_TO_pld_clk_inuse,       --               .pld_clk_inuse
			serdes_pll_locked   => CONNECTED_TO_serdes_pll_locked,   --               .serdes_pll_locked
			reset_status        => CONNECTED_TO_reset_status,        --               .reset_status
			testin_zero         => CONNECTED_TO_testin_zero,         --               .testin_zero
			rx_in0              => CONNECTED_TO_rx_in0,              --     hip_serial.rx_in0
			rx_in1              => CONNECTED_TO_rx_in1,              --               .rx_in1
			rx_in2              => CONNECTED_TO_rx_in2,              --               .rx_in2
			rx_in3              => CONNECTED_TO_rx_in3,              --               .rx_in3
			rx_in4              => CONNECTED_TO_rx_in4,              --               .rx_in4
			rx_in5              => CONNECTED_TO_rx_in5,              --               .rx_in5
			rx_in6              => CONNECTED_TO_rx_in6,              --               .rx_in6
			rx_in7              => CONNECTED_TO_rx_in7,              --               .rx_in7
			tx_out0             => CONNECTED_TO_tx_out0,             --               .tx_out0
			tx_out1             => CONNECTED_TO_tx_out1,             --               .tx_out1
			tx_out2             => CONNECTED_TO_tx_out2,             --               .tx_out2
			tx_out3             => CONNECTED_TO_tx_out3,             --               .tx_out3
			tx_out4             => CONNECTED_TO_tx_out4,             --               .tx_out4
			tx_out5             => CONNECTED_TO_tx_out5,             --               .tx_out5
			tx_out6             => CONNECTED_TO_tx_out6,             --               .tx_out6
			tx_out7             => CONNECTED_TO_tx_out7,             --               .tx_out7
			derr_cor_ext_rcv    => CONNECTED_TO_derr_cor_ext_rcv,    --     hip_status.derr_cor_ext_rcv
			derr_cor_ext_rpl    => CONNECTED_TO_derr_cor_ext_rpl,    --               .derr_cor_ext_rpl
			derr_rpl            => CONNECTED_TO_derr_rpl,            --               .derr_rpl
			dlup                => CONNECTED_TO_dlup,                --               .dlup
			dlup_exit           => CONNECTED_TO_dlup_exit,           --               .dlup_exit
			ev128ns             => CONNECTED_TO_ev128ns,             --               .ev128ns
			ev1us               => CONNECTED_TO_ev1us,               --               .ev1us
			hotrst_exit         => CONNECTED_TO_hotrst_exit,         --               .hotrst_exit
			int_status          => CONNECTED_TO_int_status,          --               .int_status
			l2_exit             => CONNECTED_TO_l2_exit,             --               .l2_exit
			lane_act            => CONNECTED_TO_lane_act,            --               .lane_act
			ltssmstate          => CONNECTED_TO_ltssmstate,          --               .ltssmstate
			rx_par_err          => CONNECTED_TO_rx_par_err,          --               .rx_par_err
			tx_par_err          => CONNECTED_TO_tx_par_err,          --               .tx_par_err
			cfg_par_err         => CONNECTED_TO_cfg_par_err,         --               .cfg_par_err
			ko_cpl_spc_header   => CONNECTED_TO_ko_cpl_spc_header,   --               .ko_cpl_spc_header
			ko_cpl_spc_data     => CONNECTED_TO_ko_cpl_spc_data,     --               .ko_cpl_spc_data
			app_int_sts         => CONNECTED_TO_app_int_sts,         --        int_msi.app_int_sts
			app_int_ack         => CONNECTED_TO_app_int_ack,         --               .app_int_ack
			app_msi_num         => CONNECTED_TO_app_msi_num,         --               .app_msi_num
			app_msi_req         => CONNECTED_TO_app_msi_req,         --               .app_msi_req
			app_msi_tc          => CONNECTED_TO_app_msi_tc,          --               .app_msi_tc
			app_msi_ack         => CONNECTED_TO_app_msi_ack,         --               .app_msi_ack
			npor                => CONNECTED_TO_npor,                --           npor.npor
			pin_perst           => CONNECTED_TO_pin_perst,           --               .pin_perst
			pld_clk             => CONNECTED_TO_pld_clk,             --        pld_clk.clk
			pm_auxpwr           => CONNECTED_TO_pm_auxpwr,           --     power_mgnt.pm_auxpwr
			pm_data             => CONNECTED_TO_pm_data,             --               .pm_data
			pme_to_cr           => CONNECTED_TO_pme_to_cr,           --               .pme_to_cr
			pm_event            => CONNECTED_TO_pm_event,            --               .pm_event
			pme_to_sr           => CONNECTED_TO_pme_to_sr,           --               .pme_to_sr
			refclk              => CONNECTED_TO_refclk,              --         refclk.clk
			rx_st_bar           => CONNECTED_TO_rx_st_bar,           --         rx_bar.rx_st_bar
			rx_st_mask          => CONNECTED_TO_rx_st_mask,          --               .rx_st_mask
			rx_st_sop           => CONNECTED_TO_rx_st_sop,           --          rx_st.startofpacket
			rx_st_eop           => CONNECTED_TO_rx_st_eop,           --               .endofpacket
			rx_st_err           => CONNECTED_TO_rx_st_err,           --               .error
			rx_st_valid         => CONNECTED_TO_rx_st_valid,         --               .valid
			rx_st_ready         => CONNECTED_TO_rx_st_ready,         --               .ready
			rx_st_data          => CONNECTED_TO_rx_st_data,          --               .data
			rx_st_empty         => CONNECTED_TO_rx_st_empty,         --               .empty
			tx_cred_data_fc     => CONNECTED_TO_tx_cred_data_fc,     --        tx_cred.tx_cred_data_fc
			tx_cred_fc_hip_cons => CONNECTED_TO_tx_cred_fc_hip_cons, --               .tx_cred_fc_hip_cons
			tx_cred_fc_infinite => CONNECTED_TO_tx_cred_fc_infinite, --               .tx_cred_fc_infinite
			tx_cred_hdr_fc      => CONNECTED_TO_tx_cred_hdr_fc,      --               .tx_cred_hdr_fc
			tx_cred_fc_sel      => CONNECTED_TO_tx_cred_fc_sel,      --               .tx_cred_fc_sel
			tx_st_sop           => CONNECTED_TO_tx_st_sop,           --          tx_st.startofpacket
			tx_st_eop           => CONNECTED_TO_tx_st_eop,           --               .endofpacket
			tx_st_err           => CONNECTED_TO_tx_st_err,           --               .error
			tx_st_valid         => CONNECTED_TO_tx_st_valid,         --               .valid
			tx_st_ready         => CONNECTED_TO_tx_st_ready,         --               .ready
			tx_st_data          => CONNECTED_TO_tx_st_data,          --               .data
			tx_st_empty         => CONNECTED_TO_tx_st_empty          --               .empty
		);

