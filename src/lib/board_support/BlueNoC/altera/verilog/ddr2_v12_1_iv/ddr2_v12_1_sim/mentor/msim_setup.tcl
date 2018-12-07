
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

# ACDS 12.1 177 linux 2013.04.19.15:11:28

# ----------------------------------------
# Auto-generated simulation script

# ----------------------------------------
# Initialize the variable
if ![info exists SYSTEM_INSTANCE_NAME] { 
  set SYSTEM_INSTANCE_NAME ""
} elseif { ![ string match "" $SYSTEM_INSTANCE_NAME ] } { 
  set SYSTEM_INSTANCE_NAME "/$SYSTEM_INSTANCE_NAME"
} 

if ![info exists TOP_LEVEL_NAME] { 
  set TOP_LEVEL_NAME "ddr2_v12_1"
} elseif { ![ string match "" $TOP_LEVEL_NAME ] } { 
  set TOP_LEVEL_NAME "$TOP_LEVEL_NAME"
} 

if ![info exists QSYS_SIMDIR] { 
  set QSYS_SIMDIR "./../"
} elseif { ![ string match "" $QSYS_SIMDIR ] } { 
  set QSYS_SIMDIR "$QSYS_SIMDIR"
} 


# ----------------------------------------
# Copy ROM/RAM files to simulation directory
file copy -force $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_sequencer_mem.hex ./
file copy -force $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_AC_ROM.hex ./
file copy -force $QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_inst_ROM.hex ./

# ----------------------------------------
# Create compilation libraries
proc ensure_lib { lib } { if ![file isdirectory $lib] { vlib $lib } }
ensure_lib          ./libraries/     
ensure_lib          ./libraries/work/
vmap       work     ./libraries/work/
vmap       work_lib ./libraries/work/
if { ![ string match "*ModelSim ALTERA*" [ vsim -version ] ] } {
  ensure_lib                        ./libraries/altera_ver/            
  vmap       altera_ver             ./libraries/altera_ver/            
  ensure_lib                        ./libraries/lpm_ver/               
  vmap       lpm_ver                ./libraries/lpm_ver/               
  ensure_lib                        ./libraries/sgate_ver/             
  vmap       sgate_ver              ./libraries/sgate_ver/             
  ensure_lib                        ./libraries/altera_mf_ver/         
  vmap       altera_mf_ver          ./libraries/altera_mf_ver/         
  ensure_lib                        ./libraries/altera_lnsim_ver/      
  vmap       altera_lnsim_ver       ./libraries/altera_lnsim_ver/      
  ensure_lib                        ./libraries/stratixiv_hssi_ver/    
  vmap       stratixiv_hssi_ver     ./libraries/stratixiv_hssi_ver/    
  ensure_lib                        ./libraries/stratixiv_pcie_hip_ver/
  vmap       stratixiv_pcie_hip_ver ./libraries/stratixiv_pcie_hip_ver/
  ensure_lib                        ./libraries/stratixiv_ver/         
  vmap       stratixiv_ver          ./libraries/stratixiv_ver/         
}
ensure_lib                                                           ./libraries/a0/                                                       
vmap       a0                                                        ./libraries/a0/                                                       
ensure_lib                                                           ./libraries/ng0/                                                      
vmap       ng0                                                       ./libraries/ng0/                                                      
ensure_lib                                                           ./libraries/crosser/                                                  
vmap       crosser                                                   ./libraries/crosser/                                                  
ensure_lib                                                           ./libraries/rsp_xbar_mux/                                             
vmap       rsp_xbar_mux                                              ./libraries/rsp_xbar_mux/                                             
ensure_lib                                                           ./libraries/rsp_xbar_demux/                                           
vmap       rsp_xbar_demux                                            ./libraries/rsp_xbar_demux/                                           
ensure_lib                                                           ./libraries/cmd_xbar_demux/                                           
vmap       cmd_xbar_demux                                            ./libraries/cmd_xbar_demux/                                           
ensure_lib                                                           ./libraries/rst_controller/                                           
vmap       rst_controller                                            ./libraries/rst_controller/                                           
ensure_lib                                                           ./libraries/limiter/                                                  
vmap       limiter                                                   ./libraries/limiter/                                                  
ensure_lib                                                           ./libraries/id_router/                                                
vmap       id_router                                                 ./libraries/id_router/                                                
ensure_lib                                                           ./libraries/addr_router/                                              
vmap       addr_router                                               ./libraries/addr_router/                                              
ensure_lib                                                           ./libraries/p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo/
vmap       p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo ./libraries/p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo/
ensure_lib                                                           ./libraries/p0_csr_translator_avalon_universal_slave_0_agent/         
vmap       p0_csr_translator_avalon_universal_slave_0_agent          ./libraries/p0_csr_translator_avalon_universal_slave_0_agent/         
ensure_lib                                                           ./libraries/csr_bridge_m0_translator_avalon_universal_master_0_agent/ 
vmap       csr_bridge_m0_translator_avalon_universal_master_0_agent  ./libraries/csr_bridge_m0_translator_avalon_universal_master_0_agent/ 
ensure_lib                                                           ./libraries/p0_csr_translator/                                        
vmap       p0_csr_translator                                         ./libraries/p0_csr_translator/                                        
ensure_lib                                                           ./libraries/csr_bridge_m0_translator/                                 
vmap       csr_bridge_m0_translator                                  ./libraries/csr_bridge_m0_translator/                                 
ensure_lib                                                           ./libraries/csr_bridge/                                               
vmap       csr_bridge                                                ./libraries/csr_bridge/                                               
ensure_lib                                                           ./libraries/dll0/                                                     
vmap       dll0                                                      ./libraries/dll0/                                                     
ensure_lib                                                           ./libraries/oct0/                                                     
vmap       oct0                                                      ./libraries/oct0/                                                     
ensure_lib                                                           ./libraries/c0/                                                       
vmap       c0                                                        ./libraries/c0/                                                       
ensure_lib                                                           ./libraries/s0/                                                       
vmap       s0                                                        ./libraries/s0/                                                       
ensure_lib                                                           ./libraries/m0/                                                       
vmap       m0                                                        ./libraries/m0/                                                       
ensure_lib                                                           ./libraries/p0/                                                       
vmap       p0                                                        ./libraries/p0/                                                       
ensure_lib                                                           ./libraries/pll0/                                                     
vmap       pll0                                                      ./libraries/pll0/                                                     
ensure_lib                                                           ./libraries/ddr2_v12_1/                                               
vmap       ddr2_v12_1                                                ./libraries/ddr2_v12_1/                                               

# ----------------------------------------
# Compile device library files
alias dev_com {
  echo "\[exec\] dev_com"
  if { ![ string match "*ModelSim ALTERA*" [ vsim -version ] ] } {
    vlog     "/raid/tools/altera/12.1/quartus/eda/sim_lib/altera_primitives.v"        -work altera_ver            
    vlog     "/raid/tools/altera/12.1/quartus/eda/sim_lib/220model.v"                 -work lpm_ver               
    vlog     "/raid/tools/altera/12.1/quartus/eda/sim_lib/sgate.v"                    -work sgate_ver             
    vlog     "/raid/tools/altera/12.1/quartus/eda/sim_lib/altera_mf.v"                -work altera_mf_ver         
    vlog -sv "/raid/tools/altera/12.1/quartus/eda/sim_lib/altera_lnsim.sv"            -work altera_lnsim_ver      
    vlog     "/raid/tools/altera/12.1/quartus/eda/sim_lib/stratixiv_hssi_atoms.v"     -work stratixiv_hssi_ver    
    vlog     "/raid/tools/altera/12.1/quartus/eda/sim_lib/stratixiv_pcie_hip_atoms.v" -work stratixiv_pcie_hip_ver
    vlog     "/raid/tools/altera/12.1/quartus/eda/sim_lib/stratixiv_atoms.v"          -work stratixiv_ver         
  }
}

# ----------------------------------------
# Compile the design files in correct order
alias com {
  echo "\[exec\] com"
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_mm_st_converter.v"                                        -work a0                                                       
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_addr_cmd.v"                                               -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_addr_cmd_wrap.v"                                          -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ddr2_odt_gen.v"                                           -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ddr3_odt_gen.v"                                           -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_lpddr2_addr_cmd.v"                                        -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_odt_gen.v"                                                -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_rdwr_data_tmg.v"                                          -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_arbiter.v"                                                -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_burst_gen.v"                                              -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_cmd_gen.v"                                                -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_csr.v"                                                    -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_buffer.v"                                                 -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_buffer_manager.v"                                         -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_burst_tracking.v"                                         -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_dataid_manager.v"                                         -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_fifo.v"                                                   -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_list.v"                                                   -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_rdata_path.v"                                             -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_wdata_path.v"                                             -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_decoder.v"                                            -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_decoder_32_syn.v"                                     -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_decoder_64_syn.v"                                     -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_encoder.v"                                            -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_encoder_32_syn.v"                                     -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_encoder_64_syn.v"                                     -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_ecc_encoder_decoder_wrapper.v"                            -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_axi_st_converter.v"                                       -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_input_if.v"                                               -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_rank_timer.v"                                             -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_sideband.v"                                               -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_tbp.v"                                                    -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_timing_param.v"                                           -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_controller.v"                                             -work ng0                                                      
  vlog     +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_ddrx_controller_st_top.v"                                      -work ng0                                                      
  vlog -sv +incdir+$QSYS_SIMDIR/ddr2_v12_1/ "$QSYS_SIMDIR/ddr2_v12_1/alt_mem_if_nextgen_ddr2_controller_core.sv"                            -work ng0                                                      
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_avalon_st_handshake_clock_crosser.v"                            -work crosser                                                  
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_avalon_st_clock_crosser.v"                                      -work crosser                                                  
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_avalon_st_pipeline_base.v"                                      -work crosser                                                  
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_arbitrator.sv"                                           -work rsp_xbar_mux                                             
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_rsp_xbar_mux.sv"                                            -work rsp_xbar_mux                                             
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_rsp_xbar_demux.sv"                                          -work rsp_xbar_demux                                           
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_cmd_xbar_demux.sv"                                          -work cmd_xbar_demux                                           
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_reset_controller.v"                                             -work rst_controller                                           
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_reset_synchronizer.v"                                           -work rst_controller                                           
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_traffic_limiter.sv"                                      -work limiter                                                  
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_avalon_st_pipeline_base.v"                                      -work limiter                                                  
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_id_router.sv"                                               -work id_router                                                
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_addr_router.sv"                                             -work addr_router                                              
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_avalon_sc_fifo.v"                                               -work p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_slave_agent.sv"                                          -work p0_csr_translator_avalon_universal_slave_0_agent         
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_burst_uncompressor.sv"                                   -work p0_csr_translator_avalon_universal_slave_0_agent         
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_master_agent.sv"                                         -work csr_bridge_m0_translator_avalon_universal_master_0_agent 
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_slave_translator.sv"                                     -work p0_csr_translator                                        
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_master_translator.sv"                                    -work csr_bridge_m0_translator                                 
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_simple_avalon_mm_bridge.sv"                              -work csr_bridge                                               
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_dll_stratixiv.sv"                                        -work dll0                                                     
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_oct_stratixiv.sv"                                        -work oct0                                                     
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_c0.v"                                                       -work c0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0.v"                                                       -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_sequencer_cpu_no_ifdef_params_sim_cpu_inst_test_bench.v" -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_sv_wrapper.sv"                                           -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_burst_uncompressor.sv"                                   -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_avalon_sc_fifo.v"                                               -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_ram.v"                                                      -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_cmd_xbar_mux_003.sv"                                     -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_slave_translator.sv"                                     -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_irq_mapper.sv"                                           -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_siii_wrapper.sv"                                         -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_sequencer_cpu_no_ifdef_params_sim_cpu_inst.v"            -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_inst_ROM_reg.v"                                             -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_reg_file.v"                                              -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_master_translator.sv"                                    -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_reset_controller.v"                                             -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_ram_csr.v"                                                  -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_lfsr36.v"                                                   -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/sequencer_data_mgr.sv"                                                 -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_lfsr72.v"                                                   -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_generic.sv"                                                 -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_pattern_fifo.v"                                             -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_id_router.sv"                                            -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_mgr.sv"                                                  -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/sequencer_phy_mgr.sv"                                                  -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_addr_router.sv"                                          -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_dm_decoder.v"                                               -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_id_router_003.sv"                                        -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_acv_phase_decode.v"                                      -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_arbitrator.sv"                                           -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_mem_if_sequencer_mem_no_ifdef_params.sv"                        -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_di_buffer_wrap.v"                                           -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_acv_wrapper.sv"                                          -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_bitcheck.v"                                                 -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_inst_ROM_no_ifdef_params.v"                                 -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_data_decoder.v"                                             -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_ddr2.v"                                                     -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_lfsr12.v"                                                   -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_cmd_xbar_demux_001.sv"                                   -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_di_buffer.v"                                                -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_sv_phase_decode.v"                                       -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_cmd_xbar_demux.sv"                                       -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_rsp_xbar_demux_003.sv"                                   -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_read_datapath.v"                                            -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_master_agent.sv"                                         -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_core.sv"                                                    -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_addr_router_001.sv"                                      -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_datamux.v"                                                  -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/sequencer_reg_file.sv"                                                 -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_ac_ROM_reg.v"                                               -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altera_merlin_slave_agent.sv"                                          -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_write_decoder.v"                                            -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_ac_ROM_no_ifdef_params.v"                                   -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_jumplogic.v"                                                -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/rw_manager_data_broadcast.v"                                           -work s0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_s0_rsp_xbar_mux.sv"                                         -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/sequencer_scc_siii_phase_decode.v"                                     -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/altera_reset_synchronizer.v"                                           -work s0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/afi_mux_ddrx.v"                                                        -work m0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_clock_pair_generator.v"                                  -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_read_valid_selector.v"                                   -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_addr_cmd_datapath.v"                                     -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_reset.v"                                                 -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_acv_ldc.v"                                               -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_memphy.sv"                                               -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_reset_sync.v"                                            -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_new_io_pads.v"                                           -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_fr_cycle_shifter.v"                                      -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_fr_cycle_extender.v"                                     -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_read_datapath.sv"                                        -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_write_datapath.v"                                        -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_simple_ddio_out.sv"                                      -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_phy_csr.sv"                                              -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_iss_probe.v"                                             -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_addr_cmd_pads.v"                                         -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_flop_mem.v"                                              -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0.sv"                                                      -work p0                                                       
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_p0_altdqdqs.v"                                              -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altdq_dqs2_ddio_3reg_stratixiv.sv"                                     -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altdq_dqs2_abstract.sv"                                                -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/altdq_dqs2_cal_delays.sv"                                              -work p0                                                       
  vlog -sv                                  "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_pll0.sv"                                                    -work pll0                                                     
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1/ddr2_v12_1_0002.v"                                                     -work ddr2_v12_1                                               
  vlog                                      "$QSYS_SIMDIR/ddr2_v12_1.v"                                                                                                                                    
}

# ----------------------------------------
# Elaborate top level design
alias elab {
  echo "\[exec\] elab"
  vsim -t ps -L work -L work_lib -L a0 -L ng0 -L crosser -L rsp_xbar_mux -L rsp_xbar_demux -L cmd_xbar_demux -L rst_controller -L limiter -L id_router -L addr_router -L p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo -L p0_csr_translator_avalon_universal_slave_0_agent -L csr_bridge_m0_translator_avalon_universal_master_0_agent -L p0_csr_translator -L csr_bridge_m0_translator -L csr_bridge -L dll0 -L oct0 -L c0 -L s0 -L m0 -L p0 -L pll0 -L ddr2_v12_1 -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L stratixiv_hssi_ver -L stratixiv_pcie_hip_ver -L stratixiv_ver $TOP_LEVEL_NAME
}

# ----------------------------------------
# Elaborate the top level design with novopt option
alias elab_debug {
  echo "\[exec\] elab_debug"
  vsim -novopt -t ps -L work -L work_lib -L a0 -L ng0 -L crosser -L rsp_xbar_mux -L rsp_xbar_demux -L cmd_xbar_demux -L rst_controller -L limiter -L id_router -L addr_router -L p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo -L p0_csr_translator_avalon_universal_slave_0_agent -L csr_bridge_m0_translator_avalon_universal_master_0_agent -L p0_csr_translator -L csr_bridge_m0_translator -L csr_bridge -L dll0 -L oct0 -L c0 -L s0 -L m0 -L p0 -L pll0 -L ddr2_v12_1 -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L stratixiv_hssi_ver -L stratixiv_pcie_hip_ver -L stratixiv_ver $TOP_LEVEL_NAME
}

# ----------------------------------------
# Compile all the design files and elaborate the top level design
alias ld "
  dev_com
  com
  elab
"

# ----------------------------------------
# Compile all the design files and elaborate the top level design with -novopt
alias ld_debug "
  dev_com
  com
  elab_debug
"

# ----------------------------------------
# Print out user commmand line aliases
alias h {
  echo "List Of Command Line Aliases"
  echo
  echo "dev_com                       -- Compile device library files"
  echo
  echo "com                           -- Compile the design files in correct order"
  echo
  echo "elab                          -- Elaborate top level design"
  echo
  echo "elab_debug                    -- Elaborate the top level design with novopt option"
  echo
  echo "ld                            -- Compile all the design files and elaborate the top level design"
  echo
  echo "ld_debug                      -- Compile all the design files and elaborate the top level design with -novopt"
  echo
  echo 
  echo
  echo "List Of Variables"
  echo
  echo "TOP_LEVEL_NAME                -- Top level module name."
  echo
  echo "SYSTEM_INSTANCE_NAME          -- Instantiated system module name inside top level module."
  echo
  echo "QSYS_SIMDIR                   -- Qsys base simulation directory."
}
h
