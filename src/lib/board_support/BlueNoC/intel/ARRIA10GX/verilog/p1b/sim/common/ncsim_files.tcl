
namespace eval p1b {
  proc get_design_libraries {} {
    set libraries [dict create]
    dict set libraries altera_common_sv_packages  1
    dict set libraries altera_xcvr_native_a10_180 1
    dict set libraries altera_pcie_a10_hip_180    1
    dict set libraries altera_xcvr_fpll_a10_180   1
    dict set libraries p1b                        1
    return $libraries
  }
  
  proc get_memory_files {QSYS_SIMDIR} {
    set memory_files [list]
    return $memory_files
  }
  
  proc get_common_design_files {USER_DEFINED_COMPILE_OPTIONS USER_DEFINED_VERILOG_COMPILE_OPTIONS USER_DEFINED_VHDL_COMPILE_OPTIONS QSYS_SIMDIR} {
    set design_files [dict create]
    dict set design_files "altera_common_sv_packages::altera_xcvr_native_a10_functions_h" "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/altera_xcvr_native_a10_functions_h.sv\"  -work altera_common_sv_packages -cdslib  ./cds_libs/altera_common_sv_packages.cds.lib"
    return $design_files
  }
  
  proc get_design_files {USER_DEFINED_COMPILE_OPTIONS USER_DEFINED_VERILOG_COMPILE_OPTIONS USER_DEFINED_VHDL_COMPILE_OPTIONS QSYS_SIMDIR} {
    set design_files [list]
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_resync.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                       
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_arbiter.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                      
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/twentynm_pcs.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                          
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/twentynm_pma.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                          
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/twentynm_xcvr_avmm.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                    
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/twentynm_xcvr_native.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                  
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/a10_avmm_h.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                            
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_pipe_retry.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"            
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_avmm_csr.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"              
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_prbs_accum.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"            
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_odi_accel.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"             
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_rcfg_arb.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"              
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/altera_xcvr_native_pcie_dfe_params_h.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"  
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_commands_h.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                  
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_functions_h.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                 
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_program.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                     
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_cpu.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                         
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/pcie_mgmt_master.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"                      
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/altera_xcvr_native_pcie_dfe_ip.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"        
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/p1b_altera_xcvr_native_a10_180_ei7wkpy.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_native_a10_180/sim/alt_xcvr_native_rcfg_opt_logic_ei7wkpy.sv\"  -work altera_xcvr_native_a10_180 -cdslib  ./cds_libs/altera_xcvr_native_a10_180.cds.lib"
    lappend design_files "ncvlog -compcnfg $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/phy_g1x8.v\"  -work altera_pcie_a10_hip_180"                                                                                      
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/twentynm_xcvr_avmm.sv\"  -work altera_xcvr_fpll_a10_180 -cdslib  ./cds_libs/altera_xcvr_fpll_a10_180.cds.lib"                          
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/alt_xcvr_resync.sv\"  -work altera_xcvr_fpll_a10_180 -cdslib  ./cds_libs/altera_xcvr_fpll_a10_180.cds.lib"                             
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/altera_xcvr_fpll_a10.sv\"  -work altera_xcvr_fpll_a10_180 -cdslib  ./cds_libs/altera_xcvr_fpll_a10_180.cds.lib"                        
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/a10_avmm_h.sv\"  -work altera_xcvr_fpll_a10_180 -cdslib  ./cds_libs/altera_xcvr_fpll_a10_180.cds.lib"                                  
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/alt_xcvr_native_avmm_nf.sv\"  -work altera_xcvr_fpll_a10_180 -cdslib  ./cds_libs/altera_xcvr_fpll_a10_180.cds.lib"                     
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/alt_xcvr_pll_embedded_debug.sv\"  -work altera_xcvr_fpll_a10_180 -cdslib  ./cds_libs/altera_xcvr_fpll_a10_180.cds.lib"                 
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_xcvr_fpll_a10_180/sim/alt_xcvr_pll_avmm_csr.sv\"  -work altera_xcvr_fpll_a10_180 -cdslib  ./cds_libs/altera_xcvr_fpll_a10_180.cds.lib"                       
    lappend design_files "ncvlog -compcnfg $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/fpll_g1g2xn.v\"  -work altera_pcie_a10_hip_180"                                                                                   
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/p1b_altera_pcie_a10_hip_180_d4vpmgy.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                 
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_a10_hip_pipen1b.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                             
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_sc_bitsync.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                                  
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_reset_delay_sync.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                            
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_rs_a10_hip.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                                  
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_a10_hip_pllnphy.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                             
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/skp_det_g3.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                                          
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altera_xcvr_functions.sv\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                          
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_monitor_a10_dlhip_sim.sv\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                  
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_a10.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                           
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_cseb_a10.sv\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                 
    lappend design_files "ncvlog -sv $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_monitor_a10.sv\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"              
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_trigger_a10.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                   
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_tlp_inspector_pcsig_drive_a10.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"               
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_a10_gbfifo.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                                  
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_scfifo_a10.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                                  
    lappend design_files "ncvlog $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/../altera_pcie_a10_hip_180/sim/altpcie_a10_scfifo_ext.v\"  -work altera_pcie_a10_hip_180 -cdslib  ./cds_libs/altera_pcie_a10_hip_180.cds.lib"                              
    lappend design_files "ncvlog -compcnfg $USER_DEFINED_VERILOG_COMPILE_OPTIONS $USER_DEFINED_COMPILE_OPTIONS  \"$QSYS_SIMDIR/p1b.v\"  -work p1b"                                                                                                                                              
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
