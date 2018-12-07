
namespace eval p1b {
  proc get_memory_files {QSYS_SIMDIR} {
    set memory_files [list]
    return $memory_files
  }
  
  proc get_common_design_files {QSYS_SIMDIR} {
    set design_files [dict create]
    dict set design_files "altera_common_sv_packages::altera_xcvr_native_a10_functions_h" "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/altera_xcvr_native_a10_functions_h.sv"
    return $design_files
  }
  
  proc get_design_files {QSYS_SIMDIR} {
    set design_files [dict create]
    dict set design_files "alt_xcvr_resync.sv"                        "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_resync.sv"                       
    dict set design_files "alt_xcvr_arbiter.sv"                       "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_arbiter.sv"                      
    dict set design_files "twentynm_pcs.sv"                           "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/twentynm_pcs.sv"                          
    dict set design_files "twentynm_pma.sv"                           "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/twentynm_pma.sv"                          
    dict set design_files "twentynm_xcvr_avmm.sv"                     "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/twentynm_xcvr_avmm.sv"                    
    dict set design_files "twentynm_xcvr_native.sv"                   "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/twentynm_xcvr_native.sv"                  
    dict set design_files "a10_avmm_h.sv"                             "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/a10_avmm_h.sv"                            
    dict set design_files "alt_xcvr_native_pipe_retry.sv"             "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_pipe_retry.sv"            
    dict set design_files "alt_xcvr_native_avmm_csr.sv"               "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_avmm_csr.sv"              
    dict set design_files "alt_xcvr_native_prbs_accum.sv"             "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_prbs_accum.sv"            
    dict set design_files "alt_xcvr_native_odi_accel.sv"              "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_odi_accel.sv"             
    dict set design_files "alt_xcvr_native_rcfg_arb.sv"               "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_rcfg_arb.sv"              
    dict set design_files "altera_xcvr_native_pcie_dfe_params_h.sv"   "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/altera_xcvr_native_pcie_dfe_params_h.sv"  
    dict set design_files "pcie_mgmt_commands_h.sv"                   "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_commands_h.sv"                  
    dict set design_files "pcie_mgmt_functions_h.sv"                  "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_functions_h.sv"                 
    dict set design_files "pcie_mgmt_program.sv"                      "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_program.sv"                     
    dict set design_files "pcie_mgmt_cpu.sv"                          "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_cpu.sv"                         
    dict set design_files "pcie_mgmt_master.sv"                       "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_master.sv"                      
    dict set design_files "altera_xcvr_native_pcie_dfe_ip.sv"         "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/altera_xcvr_native_pcie_dfe_ip.sv"        
    dict set design_files "p1b_altera_xcvr_native_a10_180_ei7wkpy.sv" "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/p1b_altera_xcvr_native_a10_180_ei7wkpy.sv"
    dict set design_files "alt_xcvr_native_rcfg_opt_logic_ei7wkpy.sv" "$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_rcfg_opt_logic_ei7wkpy.sv"
    dict set design_files "phy_g1x8.v"                                "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/phy_g1x8.v"                                  
    dict set design_files "twentynm_xcvr_avmm.sv"                     "$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/twentynm_xcvr_avmm.sv"                      
    dict set design_files "alt_xcvr_resync.sv"                        "$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/alt_xcvr_resync.sv"                         
    dict set design_files "altera_xcvr_fpll_a10.sv"                   "$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/altera_xcvr_fpll_a10.sv"                    
    dict set design_files "a10_avmm_h.sv"                             "$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/a10_avmm_h.sv"                              
    dict set design_files "alt_xcvr_native_avmm_nf.sv"                "$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/alt_xcvr_native_avmm_nf.sv"                 
    dict set design_files "alt_xcvr_pll_embedded_debug.sv"            "$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/alt_xcvr_pll_embedded_debug.sv"             
    dict set design_files "alt_xcvr_pll_avmm_csr.sv"                  "$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/alt_xcvr_pll_avmm_csr.sv"                   
    dict set design_files "fpll_g1g2xn.v"                             "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/fpll_g1g2xn.v"                               
    dict set design_files "p1b_altera_pcie_a10_hip_180_d4vpmgy.v"     "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/p1b_altera_pcie_a10_hip_180_d4vpmgy.v"       
    dict set design_files "altpcie_a10_hip_pipen1b.v"                 "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_a10_hip_pipen1b.v"                   
    dict set design_files "altpcie_sc_bitsync.v"                      "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_sc_bitsync.v"                        
    dict set design_files "altpcie_reset_delay_sync.v"                "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_reset_delay_sync.v"                  
    dict set design_files "altpcie_rs_a10_hip.v"                      "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_rs_a10_hip.v"                        
    dict set design_files "altpcie_a10_hip_pllnphy.v"                 "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_a10_hip_pllnphy.v"                   
    dict set design_files "skp_det_g3.v"                              "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/skp_det_g3.v"                                
    dict set design_files "altera_xcvr_functions.sv"                  "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altera_xcvr_functions.sv"                    
    dict set design_files "altpcie_monitor_a10_dlhip_sim.sv"          "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_monitor_a10_dlhip_sim.sv"            
    dict set design_files "altpcie_tlp_inspector_a10.v"               "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_a10.v"                 
    dict set design_files "altpcie_tlp_inspector_cseb_a10.sv"         "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_cseb_a10.sv"           
    dict set design_files "altpcie_tlp_inspector_monitor_a10.sv"      "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_monitor_a10.sv"        
    dict set design_files "altpcie_tlp_inspector_trigger_a10.v"       "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_trigger_a10.v"         
    dict set design_files "altpcie_tlp_inspector_pcsig_drive_a10.v"   "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_pcsig_drive_a10.v"     
    dict set design_files "altpcie_a10_gbfifo.v"                      "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_a10_gbfifo.v"                        
    dict set design_files "altpcie_scfifo_a10.v"                      "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_scfifo_a10.v"                        
    dict set design_files "altpcie_a10_scfifo_ext.v"                  "$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_a10_scfifo_ext.v"                    
    dict set design_files "p1b.v"                                     "$QSYS_SIMDIR/p1b.v"                                                                      
    return $design_files
  }
  
  proc get_elab_options {SIMULATOR_TOOL_BITNESS} {
    set ELAB_OPTIONS ""
    if ![ string match "bit_64" $SIMULATOR_TOOL_BITNESS ] {
    } else {
    }
    return $ELAB_OPTIONS
  }
  
  
  proc get_sim_options {SIMULATOR_TOOL_BITNESS} {
    set SIM_OPTIONS ""
    if ![ string match "bit_64" $SIMULATOR_TOOL_BITNESS ] {
    } else {
    }
    return $SIM_OPTIONS
  }
  
  
  proc get_env_variables {SIMULATOR_TOOL_BITNESS} {
    set ENV_VARIABLES [dict create]
    set LD_LIBRARY_PATH [dict create]
    dict set ENV_VARIABLES "LD_LIBRARY_PATH" $LD_LIBRARY_PATH
    if ![ string match "bit_64" $SIMULATOR_TOOL_BITNESS ] {
    } else {
    }
    return $ENV_VARIABLES
  }
  
  
}
