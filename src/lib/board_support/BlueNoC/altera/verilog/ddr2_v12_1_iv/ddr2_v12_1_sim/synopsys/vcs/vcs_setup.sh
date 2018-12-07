
# (C) 2001-2013 Altera Corporation. All rights reserved.
# Your use of Altera Corporation's design tools, logic functions and 
# other software and tools, and its AMPP partner logic functions, and 
# any output files any of the foregoing (including device programming 
# or simulation files), and any associated documentation or information 
# are expressly subject to the terms and conditions of the Altera 
# Program License Subscription Agreement, Altera MegaCore Function 
# License Agreement, or other applicable license agreement, including, 
# without limitation, that your use is for the sole purpose of 
# programming logic devices manufactured by Altera and sold by Altera 
# or its authorized distributors. Please refer to the applicable 
# agreement for further details.

# ACDS 12.1 177 linux 2013.04.19.15:11:29

# ----------------------------------------
# vcs - auto-generated simulation script

# ----------------------------------------
# initialize variables
TOP_LEVEL_NAME="ddr2_v12_1"
SKIP_FILE_COPY=0
QSYS_SIMDIR="./../../"
SKIP_ELAB=0
SKIP_SIM=0
USER_DEFINED_ELAB_OPTIONS=""
USER_DEFINED_SIM_OPTIONS="+vcs+finish+100"
# ----------------------------------------
# overwrite variables - DO NOT MODIFY!
# This block evaluates each command line argument, typically used for 
# overwriting variables. An example usage:
#   sh <simulator>_setup.sh SKIP_ELAB=1 SKIP_SIM=1
for expression in "$@"; do
  eval $expression
  if [ $? -ne 0 ]; then
    echo "Error: This command line argument, \"$expression\", is/has an invalid expression." >&2
    exit $?
  fi
done

# ----------------------------------------
# copy RAM/ROM files to simulation directory
if [ $SKIP_FILE_COPY -eq 0 ]; then
  cp -f $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_sequencer_mem.hex ./
  cp -f $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_AC_ROM.hex ./
  cp -f $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_inst_ROM.hex ./
fi

vcs -lca -timescale=1ps/1ps -sverilog +verilog2001ext+.v -ntb_opts dtm $USER_DEFINED_ELAB_OPTIONS \
  -v /raid/tools/altera/12.1/quartus/eda/sim_lib/altera_primitives.v \
  -v /raid/tools/altera/12.1/quartus/eda/sim_lib/220model.v \
  -v /raid/tools/altera/12.1/quartus/eda/sim_lib/sgate.v \
  -v /raid/tools/altera/12.1/quartus/eda/sim_lib/altera_mf.v \
  /raid/tools/altera/12.1/quartus/eda/sim_lib/altera_lnsim.sv \
  -v /raid/tools/altera/12.1/quartus/eda/sim_lib/stratixiv_hssi_atoms.v \
  -v /raid/tools/altera/12.1/quartus/eda/sim_lib/stratixiv_pcie_hip_atoms.v \
  -v /raid/tools/altera/12.1/quartus/eda/sim_lib/stratixiv_atoms.v \
  $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_mm_st_converter.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_addr_cmd.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_addr_cmd_wrap.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ddr2_odt_gen.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ddr3_odt_gen.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_lpddr2_addr_cmd.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_odt_gen.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_rdwr_data_tmg.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_arbiter.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_burst_gen.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_cmd_gen.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_csr.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_buffer.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_buffer_manager.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_burst_tracking.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_dataid_manager.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_fifo.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_list.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_rdata_path.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_wdata_path.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_decoder.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_decoder_32_syn.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_decoder_64_syn.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_encoder.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_encoder_32_syn.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_encoder_64_syn.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_encoder_decoder_wrapper.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_axi_st_converter.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_input_if.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_rank_timer.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_sideband.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_tbp.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_timing_param.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_controller.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_controller_st_top.v \
  +incdir+$QSYS_SIMDIR/ddr2_v12_1/ $QSYS_SIMDIR/ddr2_v12_1/alt_mem_if_nextgen_ddr2_controller_core.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_avalon_st_handshake_clock_crosser.v \
  $QSYS_SIMDIR/ddr2_v12_1/altera_avalon_st_clock_crosser.v \
  $QSYS_SIMDIR/ddr2_v12_1/altera_avalon_st_pipeline_base.v \
  $QSYS_SIMDIR/ddr2_v12_1/altera_merlin_arbitrator.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_rsp_xbar_mux.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_rsp_xbar_demux.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_cmd_xbar_demux.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_reset_controller.v \
  $QSYS_SIMDIR/ddr2_v12_1/altera_reset_synchronizer.v \
  $QSYS_SIMDIR/ddr2_v12_1/altera_merlin_traffic_limiter.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_id_router.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_addr_router.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_avalon_sc_fifo.v \
  $QSYS_SIMDIR/ddr2_v12_1/altera_merlin_slave_agent.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_merlin_burst_uncompressor.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_merlin_master_agent.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_merlin_slave_translator.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_merlin_master_translator.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_simple_avalon_mm_bridge.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_dll_stratixiv.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_oct_stratixiv.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_c0.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0.v \
  $QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_sequencer_cpu_no_ifdef_params_sim_cpu_inst_test_bench.v \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_sv_wrapper.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_ram.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_cmd_xbar_mux_003.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_irq_mapper.sv \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_siii_wrapper.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_sequencer_cpu_no_ifdef_params_sim_cpu_inst.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_inst_ROM_reg.v \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_reg_file.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_ram_csr.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_lfsr36.v \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_data_mgr.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_lfsr72.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_generic.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_pattern_fifo.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_id_router.sv \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_mgr.sv \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_phy_mgr.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_addr_router.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_dm_decoder.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_id_router_003.sv \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_acv_phase_decode.v \
  $QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_sequencer_mem_no_ifdef_params.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_di_buffer_wrap.v \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_acv_wrapper.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_bitcheck.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_inst_ROM_no_ifdef_params.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_data_decoder.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_ddr2.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_lfsr12.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_cmd_xbar_demux_001.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_di_buffer.v \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_sv_phase_decode.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_cmd_xbar_demux.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_rsp_xbar_demux_003.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_read_datapath.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_core.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_addr_router_001.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_datamux.v \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_reg_file.sv \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_ac_ROM_reg.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_write_decoder.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_ac_ROM_no_ifdef_params.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_jumplogic.v \
  $QSYS_SIMDIR/ddr2_v12_1/rw_manager_data_broadcast.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_rsp_xbar_mux.sv \
  $QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_siii_phase_decode.v \
  $QSYS_SIMDIR/ddr2_v12_1/afi_mux_ddrx.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_clock_pair_generator.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_read_valid_selector.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_addr_cmd_datapath.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_reset.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_acv_ldc.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_memphy.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_reset_sync.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_new_io_pads.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_fr_cycle_shifter.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_fr_cycle_extender.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_read_datapath.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_write_datapath.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_simple_ddio_out.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_phy_csr.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_iss_probe.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_addr_cmd_pads.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_flop_mem.v \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_altdqdqs.v \
  $QSYS_SIMDIR/ddr2_v12_1/altdq_dqs2_ddio_3reg_stratixiv.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altdq_dqs2_abstract.sv \
  $QSYS_SIMDIR/ddr2_v12_1/altdq_dqs2_cal_delays.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_pll0.sv \
  $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_0002.v \
  $QSYS_SIMDIR/ddr2_v12_1.v \
  -top $TOP_LEVEL_NAME
# ----------------------------------------
# simulate
if [ $SKIP_SIM -eq 0 ]; then
  ./simv $USER_DEFINED_SIM_OPTIONS
fi
