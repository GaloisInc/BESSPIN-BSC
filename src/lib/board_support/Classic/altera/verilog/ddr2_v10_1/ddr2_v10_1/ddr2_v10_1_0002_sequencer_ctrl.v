//megafunction wizard: %Altera SOPC Builder%
//GENERATION: STANDARD
//VERSION: WM1.0


//Legal Notice: (C)2010 Altera Corporation. All rights reserved.  Your
//use of Altera Corporation's design tools, logic functions and other
//software and tools, and its AMPP partner logic functions, and any
//output files any of the foregoing (including device programming or
//simulation files), and any associated documentation or information are
//expressly subject to the terms and conditions of the Altera Program
//License Subscription Agreement or other applicable license agreement,
//including, without limitation, that your use is for the sole purpose
//of programming logic devices manufactured by Altera and sold by Altera
//or its authorized distributors.  Please refer to the applicable
//agreement for further details.

// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on

// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_rom_bridge_0_avalon_master_arbitrator (
                                               // inputs:
                                                clk,
                                                d1_sequencer_rom_s2_end_xfer,
                                                reset_n,
                                                rom_bridge_0_avalon_master_address,
                                                rom_bridge_0_avalon_master_write,
                                                rom_bridge_0_avalon_master_writedata,
                                                rom_bridge_0_granted_sequencer_rom_s2,
                                                rom_bridge_0_qualified_request_sequencer_rom_s2,
                                                rom_bridge_0_requests_sequencer_rom_s2,

                                               // outputs:
                                                rom_bridge_0_avalon_master_address_to_slave,
                                                rom_bridge_0_avalon_master_waitrequest
                                             )
;

  output  [ 13: 0] rom_bridge_0_avalon_master_address_to_slave;
  output           rom_bridge_0_avalon_master_waitrequest;
  input            clk;
  input            d1_sequencer_rom_s2_end_xfer;
  input            reset_n;
  input   [ 13: 0] rom_bridge_0_avalon_master_address;
  input            rom_bridge_0_avalon_master_write;
  input   [ 31: 0] rom_bridge_0_avalon_master_writedata;
  input            rom_bridge_0_granted_sequencer_rom_s2;
  input            rom_bridge_0_qualified_request_sequencer_rom_s2;
  input            rom_bridge_0_requests_sequencer_rom_s2;

  reg              active_and_waiting_last_time;
  wire             r_0;
  reg     [ 13: 0] rom_bridge_0_avalon_master_address_last_time;
  wire    [ 13: 0] rom_bridge_0_avalon_master_address_to_slave;
  wire             rom_bridge_0_avalon_master_run;
  wire             rom_bridge_0_avalon_master_waitrequest;
  reg              rom_bridge_0_avalon_master_write_last_time;
  reg     [ 31: 0] rom_bridge_0_avalon_master_writedata_last_time;
  //r_0 master_run cascaded wait assignment, which is an e_assign
  assign r_0 = 1 & ((~rom_bridge_0_qualified_request_sequencer_rom_s2 | ~(rom_bridge_0_avalon_master_write) | (1 & (rom_bridge_0_avalon_master_write))));

  //cascaded wait assignment, which is an e_assign
  assign rom_bridge_0_avalon_master_run = r_0;

  //optimize select-logic by passing only those address bits which matter.
  assign rom_bridge_0_avalon_master_address_to_slave = rom_bridge_0_avalon_master_address[13 : 0];

  //actual waitrequest port, which is an e_assign
  assign rom_bridge_0_avalon_master_waitrequest = ~rom_bridge_0_avalon_master_run;


//synthesis translate_off
//////////////// SIMULATION-ONLY CONTENTS
  //rom_bridge_0_avalon_master_address check against wait, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          rom_bridge_0_avalon_master_address_last_time <= 0;
      else 
        rom_bridge_0_avalon_master_address_last_time <= rom_bridge_0_avalon_master_address;
    end


  //rom_bridge_0/avalon_master waited last time, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          active_and_waiting_last_time <= 0;
      else 
        active_and_waiting_last_time <= rom_bridge_0_avalon_master_waitrequest & (rom_bridge_0_avalon_master_write);
    end


  //rom_bridge_0_avalon_master_address matches last port_name, which is an e_process
  always @(posedge clk)
    begin
      if (active_and_waiting_last_time & (rom_bridge_0_avalon_master_address != rom_bridge_0_avalon_master_address_last_time))
        begin
          $write("%0d ns: rom_bridge_0_avalon_master_address did not heed wait!!!", $time);
          $stop;
        end
    end


  //rom_bridge_0_avalon_master_write check against wait, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          rom_bridge_0_avalon_master_write_last_time <= 0;
      else 
        rom_bridge_0_avalon_master_write_last_time <= rom_bridge_0_avalon_master_write;
    end


  //rom_bridge_0_avalon_master_write matches last port_name, which is an e_process
  always @(posedge clk)
    begin
      if (active_and_waiting_last_time & (rom_bridge_0_avalon_master_write != rom_bridge_0_avalon_master_write_last_time))
        begin
          $write("%0d ns: rom_bridge_0_avalon_master_write did not heed wait!!!", $time);
          $stop;
        end
    end


  //rom_bridge_0_avalon_master_writedata check against wait, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          rom_bridge_0_avalon_master_writedata_last_time <= 0;
      else 
        rom_bridge_0_avalon_master_writedata_last_time <= rom_bridge_0_avalon_master_writedata;
    end


  //rom_bridge_0_avalon_master_writedata matches last port_name, which is an e_process
  always @(posedge clk)
    begin
      if (active_and_waiting_last_time & (rom_bridge_0_avalon_master_writedata != rom_bridge_0_avalon_master_writedata_last_time) & rom_bridge_0_avalon_master_write)
        begin
          $write("%0d ns: rom_bridge_0_avalon_master_writedata did not heed wait!!!", $time);
          $stop;
        end
    end



//////////////// END SIMULATION-ONLY CONTENTS

//synthesis translate_on

endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_bridge_0_avalon_slave_arbitrator (
                                                    // inputs:
                                                     clk,
                                                     reset_n,
                                                     sequencer_bridge_0_avalon_slave_readdata,
                                                     sequencer_bridge_0_avalon_slave_waitrequest,
                                                     sequencer_cpu_data_master_address_to_slave,
                                                     sequencer_cpu_data_master_read,
                                                     sequencer_cpu_data_master_waitrequest,
                                                     sequencer_cpu_data_master_write,
                                                     sequencer_cpu_data_master_writedata,

                                                    // outputs:
                                                     d1_sequencer_bridge_0_avalon_slave_end_xfer,
                                                     sequencer_bridge_0_avalon_slave_address,
                                                     sequencer_bridge_0_avalon_slave_read,
                                                     sequencer_bridge_0_avalon_slave_readdata_from_sa,
                                                     sequencer_bridge_0_avalon_slave_reset_n,
                                                     sequencer_bridge_0_avalon_slave_waitrequest_from_sa,
                                                     sequencer_bridge_0_avalon_slave_write,
                                                     sequencer_bridge_0_avalon_slave_writedata,
                                                     sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave,
                                                     sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave,
                                                     sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave,
                                                     sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave
                                                  )
;

  output           d1_sequencer_bridge_0_avalon_slave_end_xfer;
  output  [ 15: 0] sequencer_bridge_0_avalon_slave_address;
  output           sequencer_bridge_0_avalon_slave_read;
  output  [ 31: 0] sequencer_bridge_0_avalon_slave_readdata_from_sa;
  output           sequencer_bridge_0_avalon_slave_reset_n;
  output           sequencer_bridge_0_avalon_slave_waitrequest_from_sa;
  output           sequencer_bridge_0_avalon_slave_write;
  output  [ 31: 0] sequencer_bridge_0_avalon_slave_writedata;
  output           sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave;
  output           sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave;
  output           sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave;
  output           sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave;
  input            clk;
  input            reset_n;
  input   [ 31: 0] sequencer_bridge_0_avalon_slave_readdata;
  input            sequencer_bridge_0_avalon_slave_waitrequest;
  input   [ 18: 0] sequencer_cpu_data_master_address_to_slave;
  input            sequencer_cpu_data_master_read;
  input            sequencer_cpu_data_master_waitrequest;
  input            sequencer_cpu_data_master_write;
  input   [ 31: 0] sequencer_cpu_data_master_writedata;

  reg              d1_reasons_to_wait;
  reg              d1_sequencer_bridge_0_avalon_slave_end_xfer;
  reg              enable_nonzero_assertions;
  wire             end_xfer_arb_share_counter_term_sequencer_bridge_0_avalon_slave;
  wire             in_a_read_cycle;
  wire             in_a_write_cycle;
  wire    [ 15: 0] sequencer_bridge_0_avalon_slave_address;
  wire             sequencer_bridge_0_avalon_slave_allgrants;
  wire             sequencer_bridge_0_avalon_slave_allow_new_arb_cycle;
  wire             sequencer_bridge_0_avalon_slave_any_bursting_master_saved_grant;
  wire             sequencer_bridge_0_avalon_slave_any_continuerequest;
  wire             sequencer_bridge_0_avalon_slave_arb_counter_enable;
  reg              sequencer_bridge_0_avalon_slave_arb_share_counter;
  wire             sequencer_bridge_0_avalon_slave_arb_share_counter_next_value;
  wire             sequencer_bridge_0_avalon_slave_arb_share_set_values;
  wire             sequencer_bridge_0_avalon_slave_beginbursttransfer_internal;
  wire             sequencer_bridge_0_avalon_slave_begins_xfer;
  wire             sequencer_bridge_0_avalon_slave_end_xfer;
  wire             sequencer_bridge_0_avalon_slave_firsttransfer;
  wire             sequencer_bridge_0_avalon_slave_grant_vector;
  wire             sequencer_bridge_0_avalon_slave_in_a_read_cycle;
  wire             sequencer_bridge_0_avalon_slave_in_a_write_cycle;
  wire             sequencer_bridge_0_avalon_slave_master_qreq_vector;
  wire             sequencer_bridge_0_avalon_slave_non_bursting_master_requests;
  wire             sequencer_bridge_0_avalon_slave_read;
  wire    [ 31: 0] sequencer_bridge_0_avalon_slave_readdata_from_sa;
  reg              sequencer_bridge_0_avalon_slave_reg_firsttransfer;
  wire             sequencer_bridge_0_avalon_slave_reset_n;
  reg              sequencer_bridge_0_avalon_slave_slavearbiterlockenable;
  wire             sequencer_bridge_0_avalon_slave_slavearbiterlockenable2;
  wire             sequencer_bridge_0_avalon_slave_unreg_firsttransfer;
  wire             sequencer_bridge_0_avalon_slave_waitrequest_from_sa;
  wire             sequencer_bridge_0_avalon_slave_waits_for_read;
  wire             sequencer_bridge_0_avalon_slave_waits_for_write;
  wire             sequencer_bridge_0_avalon_slave_write;
  wire    [ 31: 0] sequencer_bridge_0_avalon_slave_writedata;
  wire             sequencer_cpu_data_master_arbiterlock;
  wire             sequencer_cpu_data_master_arbiterlock2;
  wire             sequencer_cpu_data_master_continuerequest;
  wire             sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave;
  wire             sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave;
  wire             sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave;
  wire             sequencer_cpu_data_master_saved_grant_sequencer_bridge_0_avalon_slave;
  wire    [ 18: 0] shifted_address_to_sequencer_bridge_0_avalon_slave_from_sequencer_cpu_data_master;
  wire             wait_for_sequencer_bridge_0_avalon_slave_counter;
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_reasons_to_wait <= 0;
      else 
        d1_reasons_to_wait <= ~sequencer_bridge_0_avalon_slave_end_xfer;
    end


  assign sequencer_bridge_0_avalon_slave_begins_xfer = ~d1_reasons_to_wait & ((sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave));
  //assign sequencer_bridge_0_avalon_slave_readdata_from_sa = sequencer_bridge_0_avalon_slave_readdata so that symbol knows where to group signals which may go to master only, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_readdata_from_sa = sequencer_bridge_0_avalon_slave_readdata;

  assign sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave = ({sequencer_cpu_data_master_address_to_slave[18] , 18'b0} == 19'h40000) & (sequencer_cpu_data_master_read | sequencer_cpu_data_master_write);
  //assign sequencer_bridge_0_avalon_slave_waitrequest_from_sa = sequencer_bridge_0_avalon_slave_waitrequest so that symbol knows where to group signals which may go to master only, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_waitrequest_from_sa = sequencer_bridge_0_avalon_slave_waitrequest;

  //sequencer_bridge_0_avalon_slave_arb_share_counter set values, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_arb_share_set_values = 1;

  //sequencer_bridge_0_avalon_slave_non_bursting_master_requests mux, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_non_bursting_master_requests = sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave;

  //sequencer_bridge_0_avalon_slave_any_bursting_master_saved_grant mux, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_any_bursting_master_saved_grant = 0;

  //sequencer_bridge_0_avalon_slave_arb_share_counter_next_value assignment, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_arb_share_counter_next_value = sequencer_bridge_0_avalon_slave_firsttransfer ? (sequencer_bridge_0_avalon_slave_arb_share_set_values - 1) : |sequencer_bridge_0_avalon_slave_arb_share_counter ? (sequencer_bridge_0_avalon_slave_arb_share_counter - 1) : 0;

  //sequencer_bridge_0_avalon_slave_allgrants all slave grants, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_allgrants = |sequencer_bridge_0_avalon_slave_grant_vector;

  //sequencer_bridge_0_avalon_slave_end_xfer assignment, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_end_xfer = ~(sequencer_bridge_0_avalon_slave_waits_for_read | sequencer_bridge_0_avalon_slave_waits_for_write);

  //end_xfer_arb_share_counter_term_sequencer_bridge_0_avalon_slave arb share counter enable term, which is an e_assign
  assign end_xfer_arb_share_counter_term_sequencer_bridge_0_avalon_slave = sequencer_bridge_0_avalon_slave_end_xfer & (~sequencer_bridge_0_avalon_slave_any_bursting_master_saved_grant | in_a_read_cycle | in_a_write_cycle);

  //sequencer_bridge_0_avalon_slave_arb_share_counter arbitration counter enable, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_arb_counter_enable = (end_xfer_arb_share_counter_term_sequencer_bridge_0_avalon_slave & sequencer_bridge_0_avalon_slave_allgrants) | (end_xfer_arb_share_counter_term_sequencer_bridge_0_avalon_slave & ~sequencer_bridge_0_avalon_slave_non_bursting_master_requests);

  //sequencer_bridge_0_avalon_slave_arb_share_counter counter, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_bridge_0_avalon_slave_arb_share_counter <= 0;
      else if (sequencer_bridge_0_avalon_slave_arb_counter_enable)
          sequencer_bridge_0_avalon_slave_arb_share_counter <= sequencer_bridge_0_avalon_slave_arb_share_counter_next_value;
    end


  //sequencer_bridge_0_avalon_slave_slavearbiterlockenable slave enables arbiterlock, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_bridge_0_avalon_slave_slavearbiterlockenable <= 0;
      else if ((|sequencer_bridge_0_avalon_slave_master_qreq_vector & end_xfer_arb_share_counter_term_sequencer_bridge_0_avalon_slave) | (end_xfer_arb_share_counter_term_sequencer_bridge_0_avalon_slave & ~sequencer_bridge_0_avalon_slave_non_bursting_master_requests))
          sequencer_bridge_0_avalon_slave_slavearbiterlockenable <= |sequencer_bridge_0_avalon_slave_arb_share_counter_next_value;
    end


  //sequencer_cpu/data_master sequencer_bridge_0/avalon_slave arbiterlock, which is an e_assign
  assign sequencer_cpu_data_master_arbiterlock = sequencer_bridge_0_avalon_slave_slavearbiterlockenable & sequencer_cpu_data_master_continuerequest;

  //sequencer_bridge_0_avalon_slave_slavearbiterlockenable2 slave enables arbiterlock2, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_slavearbiterlockenable2 = |sequencer_bridge_0_avalon_slave_arb_share_counter_next_value;

  //sequencer_cpu/data_master sequencer_bridge_0/avalon_slave arbiterlock2, which is an e_assign
  assign sequencer_cpu_data_master_arbiterlock2 = sequencer_bridge_0_avalon_slave_slavearbiterlockenable2 & sequencer_cpu_data_master_continuerequest;

  //sequencer_bridge_0_avalon_slave_any_continuerequest at least one master continues requesting, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_any_continuerequest = 1;

  //sequencer_cpu_data_master_continuerequest continued request, which is an e_assign
  assign sequencer_cpu_data_master_continuerequest = 1;

  assign sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave = sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave & ~((sequencer_cpu_data_master_read & (~sequencer_cpu_data_master_waitrequest)) | ((~sequencer_cpu_data_master_waitrequest) & sequencer_cpu_data_master_write));
  //sequencer_bridge_0_avalon_slave_writedata mux, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_writedata = sequencer_cpu_data_master_writedata;

  //master is always granted when requested
  assign sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave = sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave;

  //sequencer_cpu/data_master saved-grant sequencer_bridge_0/avalon_slave, which is an e_assign
  assign sequencer_cpu_data_master_saved_grant_sequencer_bridge_0_avalon_slave = sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave;

  //allow new arb cycle for sequencer_bridge_0/avalon_slave, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_allow_new_arb_cycle = 1;

  //placeholder chosen master
  assign sequencer_bridge_0_avalon_slave_grant_vector = 1;

  //placeholder vector of master qualified-requests
  assign sequencer_bridge_0_avalon_slave_master_qreq_vector = 1;

  //sequencer_bridge_0_avalon_slave_reset_n assignment, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_reset_n = reset_n;

  //sequencer_bridge_0_avalon_slave_firsttransfer first transaction, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_firsttransfer = sequencer_bridge_0_avalon_slave_begins_xfer ? sequencer_bridge_0_avalon_slave_unreg_firsttransfer : sequencer_bridge_0_avalon_slave_reg_firsttransfer;

  //sequencer_bridge_0_avalon_slave_unreg_firsttransfer first transaction, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_unreg_firsttransfer = ~(sequencer_bridge_0_avalon_slave_slavearbiterlockenable & sequencer_bridge_0_avalon_slave_any_continuerequest);

  //sequencer_bridge_0_avalon_slave_reg_firsttransfer first transaction, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_bridge_0_avalon_slave_reg_firsttransfer <= 1'b1;
      else if (sequencer_bridge_0_avalon_slave_begins_xfer)
          sequencer_bridge_0_avalon_slave_reg_firsttransfer <= sequencer_bridge_0_avalon_slave_unreg_firsttransfer;
    end


  //sequencer_bridge_0_avalon_slave_beginbursttransfer_internal begin burst transfer, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_beginbursttransfer_internal = sequencer_bridge_0_avalon_slave_begins_xfer;

  //sequencer_bridge_0_avalon_slave_read assignment, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_read = sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave & sequencer_cpu_data_master_read;

  //sequencer_bridge_0_avalon_slave_write assignment, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_write = sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave & sequencer_cpu_data_master_write;

  assign shifted_address_to_sequencer_bridge_0_avalon_slave_from_sequencer_cpu_data_master = sequencer_cpu_data_master_address_to_slave;
  //sequencer_bridge_0_avalon_slave_address mux, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_address = shifted_address_to_sequencer_bridge_0_avalon_slave_from_sequencer_cpu_data_master >> 2;

  //d1_sequencer_bridge_0_avalon_slave_end_xfer register, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_sequencer_bridge_0_avalon_slave_end_xfer <= 1;
      else 
        d1_sequencer_bridge_0_avalon_slave_end_xfer <= sequencer_bridge_0_avalon_slave_end_xfer;
    end


  //sequencer_bridge_0_avalon_slave_waits_for_read in a cycle, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_waits_for_read = sequencer_bridge_0_avalon_slave_in_a_read_cycle & sequencer_bridge_0_avalon_slave_waitrequest_from_sa;

  //sequencer_bridge_0_avalon_slave_in_a_read_cycle assignment, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_in_a_read_cycle = sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave & sequencer_cpu_data_master_read;

  //in_a_read_cycle assignment, which is an e_mux
  assign in_a_read_cycle = sequencer_bridge_0_avalon_slave_in_a_read_cycle;

  //sequencer_bridge_0_avalon_slave_waits_for_write in a cycle, which is an e_mux
  assign sequencer_bridge_0_avalon_slave_waits_for_write = sequencer_bridge_0_avalon_slave_in_a_write_cycle & sequencer_bridge_0_avalon_slave_waitrequest_from_sa;

  //sequencer_bridge_0_avalon_slave_in_a_write_cycle assignment, which is an e_assign
  assign sequencer_bridge_0_avalon_slave_in_a_write_cycle = sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave & sequencer_cpu_data_master_write;

  //in_a_write_cycle assignment, which is an e_mux
  assign in_a_write_cycle = sequencer_bridge_0_avalon_slave_in_a_write_cycle;

  assign wait_for_sequencer_bridge_0_avalon_slave_counter = 0;

//synthesis translate_off
//////////////// SIMULATION-ONLY CONTENTS
  //sequencer_bridge_0/avalon_slave enable non-zero assertions, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          enable_nonzero_assertions <= 0;
      else 
        enable_nonzero_assertions <= 1'b1;
    end



//////////////// END SIMULATION-ONLY CONTENTS

//synthesis translate_on

endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_cpu_jtag_debug_module_arbitrator (
                                                    // inputs:
                                                     clk,
                                                     reset_n,
                                                     sequencer_cpu_data_master_address_to_slave,
                                                     sequencer_cpu_data_master_byteenable,
                                                     sequencer_cpu_data_master_debugaccess,
                                                     sequencer_cpu_data_master_read,
                                                     sequencer_cpu_data_master_waitrequest,
                                                     sequencer_cpu_data_master_write,
                                                     sequencer_cpu_data_master_writedata,
                                                     sequencer_cpu_instruction_master_address_to_slave,
                                                     sequencer_cpu_instruction_master_read,
                                                     sequencer_cpu_jtag_debug_module_readdata,
                                                     sequencer_cpu_jtag_debug_module_resetrequest,

                                                    // outputs:
                                                     d1_sequencer_cpu_jtag_debug_module_end_xfer,
                                                     sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module,
                                                     sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module,
                                                     sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module,
                                                     sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module,
                                                     sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module,
                                                     sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module,
                                                     sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module,
                                                     sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module,
                                                     sequencer_cpu_jtag_debug_module_address,
                                                     sequencer_cpu_jtag_debug_module_begintransfer,
                                                     sequencer_cpu_jtag_debug_module_byteenable,
                                                     sequencer_cpu_jtag_debug_module_chipselect,
                                                     sequencer_cpu_jtag_debug_module_debugaccess,
                                                     sequencer_cpu_jtag_debug_module_readdata_from_sa,
                                                     sequencer_cpu_jtag_debug_module_reset_n,
                                                     sequencer_cpu_jtag_debug_module_resetrequest_from_sa,
                                                     sequencer_cpu_jtag_debug_module_write,
                                                     sequencer_cpu_jtag_debug_module_writedata
                                                  )
;

  output           d1_sequencer_cpu_jtag_debug_module_end_xfer;
  output           sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module;
  output           sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module;
  output           sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module;
  output           sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module;
  output           sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module;
  output           sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module;
  output           sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module;
  output           sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module;
  output  [  8: 0] sequencer_cpu_jtag_debug_module_address;
  output           sequencer_cpu_jtag_debug_module_begintransfer;
  output  [  3: 0] sequencer_cpu_jtag_debug_module_byteenable;
  output           sequencer_cpu_jtag_debug_module_chipselect;
  output           sequencer_cpu_jtag_debug_module_debugaccess;
  output  [ 31: 0] sequencer_cpu_jtag_debug_module_readdata_from_sa;
  output           sequencer_cpu_jtag_debug_module_reset_n;
  output           sequencer_cpu_jtag_debug_module_resetrequest_from_sa;
  output           sequencer_cpu_jtag_debug_module_write;
  output  [ 31: 0] sequencer_cpu_jtag_debug_module_writedata;
  input            clk;
  input            reset_n;
  input   [ 18: 0] sequencer_cpu_data_master_address_to_slave;
  input   [  3: 0] sequencer_cpu_data_master_byteenable;
  input            sequencer_cpu_data_master_debugaccess;
  input            sequencer_cpu_data_master_read;
  input            sequencer_cpu_data_master_waitrequest;
  input            sequencer_cpu_data_master_write;
  input   [ 31: 0] sequencer_cpu_data_master_writedata;
  input   [ 16: 0] sequencer_cpu_instruction_master_address_to_slave;
  input            sequencer_cpu_instruction_master_read;
  input   [ 31: 0] sequencer_cpu_jtag_debug_module_readdata;
  input            sequencer_cpu_jtag_debug_module_resetrequest;

  reg              d1_reasons_to_wait;
  reg              d1_sequencer_cpu_jtag_debug_module_end_xfer;
  reg              enable_nonzero_assertions;
  wire             end_xfer_arb_share_counter_term_sequencer_cpu_jtag_debug_module;
  wire             in_a_read_cycle;
  wire             in_a_write_cycle;
  reg              last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_cpu_jtag_debug_module;
  reg              last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_arbiterlock;
  wire             sequencer_cpu_data_master_arbiterlock2;
  wire             sequencer_cpu_data_master_continuerequest;
  wire             sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_saved_grant_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_arbiterlock;
  wire             sequencer_cpu_instruction_master_arbiterlock2;
  wire             sequencer_cpu_instruction_master_continuerequest;
  wire             sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_saved_grant_sequencer_cpu_jtag_debug_module;
  wire    [  8: 0] sequencer_cpu_jtag_debug_module_address;
  wire             sequencer_cpu_jtag_debug_module_allgrants;
  wire             sequencer_cpu_jtag_debug_module_allow_new_arb_cycle;
  wire             sequencer_cpu_jtag_debug_module_any_bursting_master_saved_grant;
  wire             sequencer_cpu_jtag_debug_module_any_continuerequest;
  reg     [  1: 0] sequencer_cpu_jtag_debug_module_arb_addend;
  wire             sequencer_cpu_jtag_debug_module_arb_counter_enable;
  reg              sequencer_cpu_jtag_debug_module_arb_share_counter;
  wire             sequencer_cpu_jtag_debug_module_arb_share_counter_next_value;
  wire             sequencer_cpu_jtag_debug_module_arb_share_set_values;
  wire    [  1: 0] sequencer_cpu_jtag_debug_module_arb_winner;
  wire             sequencer_cpu_jtag_debug_module_arbitration_holdoff_internal;
  wire             sequencer_cpu_jtag_debug_module_beginbursttransfer_internal;
  wire             sequencer_cpu_jtag_debug_module_begins_xfer;
  wire             sequencer_cpu_jtag_debug_module_begintransfer;
  wire    [  3: 0] sequencer_cpu_jtag_debug_module_byteenable;
  wire             sequencer_cpu_jtag_debug_module_chipselect;
  wire    [  3: 0] sequencer_cpu_jtag_debug_module_chosen_master_double_vector;
  wire    [  1: 0] sequencer_cpu_jtag_debug_module_chosen_master_rot_left;
  wire             sequencer_cpu_jtag_debug_module_debugaccess;
  wire             sequencer_cpu_jtag_debug_module_end_xfer;
  wire             sequencer_cpu_jtag_debug_module_firsttransfer;
  wire    [  1: 0] sequencer_cpu_jtag_debug_module_grant_vector;
  wire             sequencer_cpu_jtag_debug_module_in_a_read_cycle;
  wire             sequencer_cpu_jtag_debug_module_in_a_write_cycle;
  wire    [  1: 0] sequencer_cpu_jtag_debug_module_master_qreq_vector;
  wire             sequencer_cpu_jtag_debug_module_non_bursting_master_requests;
  wire    [ 31: 0] sequencer_cpu_jtag_debug_module_readdata_from_sa;
  reg              sequencer_cpu_jtag_debug_module_reg_firsttransfer;
  wire             sequencer_cpu_jtag_debug_module_reset_n;
  wire             sequencer_cpu_jtag_debug_module_resetrequest_from_sa;
  reg     [  1: 0] sequencer_cpu_jtag_debug_module_saved_chosen_master_vector;
  reg              sequencer_cpu_jtag_debug_module_slavearbiterlockenable;
  wire             sequencer_cpu_jtag_debug_module_slavearbiterlockenable2;
  wire             sequencer_cpu_jtag_debug_module_unreg_firsttransfer;
  wire             sequencer_cpu_jtag_debug_module_waits_for_read;
  wire             sequencer_cpu_jtag_debug_module_waits_for_write;
  wire             sequencer_cpu_jtag_debug_module_write;
  wire    [ 31: 0] sequencer_cpu_jtag_debug_module_writedata;
  wire    [ 18: 0] shifted_address_to_sequencer_cpu_jtag_debug_module_from_sequencer_cpu_data_master;
  wire    [ 16: 0] shifted_address_to_sequencer_cpu_jtag_debug_module_from_sequencer_cpu_instruction_master;
  wire             wait_for_sequencer_cpu_jtag_debug_module_counter;
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_reasons_to_wait <= 0;
      else 
        d1_reasons_to_wait <= ~sequencer_cpu_jtag_debug_module_end_xfer;
    end


  assign sequencer_cpu_jtag_debug_module_begins_xfer = ~d1_reasons_to_wait & ((sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module | sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module));
  //assign sequencer_cpu_jtag_debug_module_readdata_from_sa = sequencer_cpu_jtag_debug_module_readdata so that symbol knows where to group signals which may go to master only, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_readdata_from_sa = sequencer_cpu_jtag_debug_module_readdata;

  assign sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module = ({sequencer_cpu_data_master_address_to_slave[18 : 11] , 11'b0} == 19'h800) & (sequencer_cpu_data_master_read | sequencer_cpu_data_master_write);
  //sequencer_cpu_jtag_debug_module_arb_share_counter set values, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_arb_share_set_values = 1;

  //sequencer_cpu_jtag_debug_module_non_bursting_master_requests mux, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_non_bursting_master_requests = sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module |
    sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module |
    sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module |
    sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module;

  //sequencer_cpu_jtag_debug_module_any_bursting_master_saved_grant mux, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_any_bursting_master_saved_grant = 0;

  //sequencer_cpu_jtag_debug_module_arb_share_counter_next_value assignment, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_arb_share_counter_next_value = sequencer_cpu_jtag_debug_module_firsttransfer ? (sequencer_cpu_jtag_debug_module_arb_share_set_values - 1) : |sequencer_cpu_jtag_debug_module_arb_share_counter ? (sequencer_cpu_jtag_debug_module_arb_share_counter - 1) : 0;

  //sequencer_cpu_jtag_debug_module_allgrants all slave grants, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_allgrants = (|sequencer_cpu_jtag_debug_module_grant_vector) |
    (|sequencer_cpu_jtag_debug_module_grant_vector) |
    (|sequencer_cpu_jtag_debug_module_grant_vector) |
    (|sequencer_cpu_jtag_debug_module_grant_vector);

  //sequencer_cpu_jtag_debug_module_end_xfer assignment, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_end_xfer = ~(sequencer_cpu_jtag_debug_module_waits_for_read | sequencer_cpu_jtag_debug_module_waits_for_write);

  //end_xfer_arb_share_counter_term_sequencer_cpu_jtag_debug_module arb share counter enable term, which is an e_assign
  assign end_xfer_arb_share_counter_term_sequencer_cpu_jtag_debug_module = sequencer_cpu_jtag_debug_module_end_xfer & (~sequencer_cpu_jtag_debug_module_any_bursting_master_saved_grant | in_a_read_cycle | in_a_write_cycle);

  //sequencer_cpu_jtag_debug_module_arb_share_counter arbitration counter enable, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_arb_counter_enable = (end_xfer_arb_share_counter_term_sequencer_cpu_jtag_debug_module & sequencer_cpu_jtag_debug_module_allgrants) | (end_xfer_arb_share_counter_term_sequencer_cpu_jtag_debug_module & ~sequencer_cpu_jtag_debug_module_non_bursting_master_requests);

  //sequencer_cpu_jtag_debug_module_arb_share_counter counter, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_jtag_debug_module_arb_share_counter <= 0;
      else if (sequencer_cpu_jtag_debug_module_arb_counter_enable)
          sequencer_cpu_jtag_debug_module_arb_share_counter <= sequencer_cpu_jtag_debug_module_arb_share_counter_next_value;
    end


  //sequencer_cpu_jtag_debug_module_slavearbiterlockenable slave enables arbiterlock, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_jtag_debug_module_slavearbiterlockenable <= 0;
      else if ((|sequencer_cpu_jtag_debug_module_master_qreq_vector & end_xfer_arb_share_counter_term_sequencer_cpu_jtag_debug_module) | (end_xfer_arb_share_counter_term_sequencer_cpu_jtag_debug_module & ~sequencer_cpu_jtag_debug_module_non_bursting_master_requests))
          sequencer_cpu_jtag_debug_module_slavearbiterlockenable <= |sequencer_cpu_jtag_debug_module_arb_share_counter_next_value;
    end


  //sequencer_cpu/data_master sequencer_cpu/jtag_debug_module arbiterlock, which is an e_assign
  assign sequencer_cpu_data_master_arbiterlock = sequencer_cpu_jtag_debug_module_slavearbiterlockenable & sequencer_cpu_data_master_continuerequest;

  //sequencer_cpu_jtag_debug_module_slavearbiterlockenable2 slave enables arbiterlock2, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_slavearbiterlockenable2 = |sequencer_cpu_jtag_debug_module_arb_share_counter_next_value;

  //sequencer_cpu/data_master sequencer_cpu/jtag_debug_module arbiterlock2, which is an e_assign
  assign sequencer_cpu_data_master_arbiterlock2 = sequencer_cpu_jtag_debug_module_slavearbiterlockenable2 & sequencer_cpu_data_master_continuerequest;

  //sequencer_cpu/instruction_master sequencer_cpu/jtag_debug_module arbiterlock, which is an e_assign
  assign sequencer_cpu_instruction_master_arbiterlock = sequencer_cpu_jtag_debug_module_slavearbiterlockenable & sequencer_cpu_instruction_master_continuerequest;

  //sequencer_cpu/instruction_master sequencer_cpu/jtag_debug_module arbiterlock2, which is an e_assign
  assign sequencer_cpu_instruction_master_arbiterlock2 = sequencer_cpu_jtag_debug_module_slavearbiterlockenable2 & sequencer_cpu_instruction_master_continuerequest;

  //sequencer_cpu/instruction_master granted sequencer_cpu/jtag_debug_module last time, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_cpu_jtag_debug_module <= 0;
      else 
        last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_cpu_jtag_debug_module <= sequencer_cpu_instruction_master_saved_grant_sequencer_cpu_jtag_debug_module ? 1 : (sequencer_cpu_jtag_debug_module_arbitration_holdoff_internal | ~sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module) ? 0 : last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_cpu_jtag_debug_module;
    end


  //sequencer_cpu_instruction_master_continuerequest continued request, which is an e_mux
  assign sequencer_cpu_instruction_master_continuerequest = last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_cpu_jtag_debug_module & sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module;

  //sequencer_cpu_jtag_debug_module_any_continuerequest at least one master continues requesting, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_any_continuerequest = sequencer_cpu_instruction_master_continuerequest |
    sequencer_cpu_data_master_continuerequest;

  assign sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module = sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module & ~(((~sequencer_cpu_data_master_waitrequest) & sequencer_cpu_data_master_write) | sequencer_cpu_instruction_master_arbiterlock);
  //sequencer_cpu_jtag_debug_module_writedata mux, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_writedata = sequencer_cpu_data_master_writedata;

  assign sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module = (({sequencer_cpu_instruction_master_address_to_slave[16 : 11] , 11'b0} == 17'h800) & (sequencer_cpu_instruction_master_read)) & sequencer_cpu_instruction_master_read;
  //sequencer_cpu/data_master granted sequencer_cpu/jtag_debug_module last time, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_cpu_jtag_debug_module <= 0;
      else 
        last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_cpu_jtag_debug_module <= sequencer_cpu_data_master_saved_grant_sequencer_cpu_jtag_debug_module ? 1 : (sequencer_cpu_jtag_debug_module_arbitration_holdoff_internal | ~sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module) ? 0 : last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_cpu_jtag_debug_module;
    end


  //sequencer_cpu_data_master_continuerequest continued request, which is an e_mux
  assign sequencer_cpu_data_master_continuerequest = last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_cpu_jtag_debug_module & sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module;

  assign sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module = sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module & ~(sequencer_cpu_data_master_arbiterlock);
  //allow new arb cycle for sequencer_cpu/jtag_debug_module, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_allow_new_arb_cycle = ~sequencer_cpu_data_master_arbiterlock & ~sequencer_cpu_instruction_master_arbiterlock;

  //sequencer_cpu/instruction_master assignment into master qualified-requests vector for sequencer_cpu/jtag_debug_module, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_master_qreq_vector[0] = sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module;

  //sequencer_cpu/instruction_master grant sequencer_cpu/jtag_debug_module, which is an e_assign
  assign sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module = sequencer_cpu_jtag_debug_module_grant_vector[0];

  //sequencer_cpu/instruction_master saved-grant sequencer_cpu/jtag_debug_module, which is an e_assign
  assign sequencer_cpu_instruction_master_saved_grant_sequencer_cpu_jtag_debug_module = sequencer_cpu_jtag_debug_module_arb_winner[0] && sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module;

  //sequencer_cpu/data_master assignment into master qualified-requests vector for sequencer_cpu/jtag_debug_module, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_master_qreq_vector[1] = sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module;

  //sequencer_cpu/data_master grant sequencer_cpu/jtag_debug_module, which is an e_assign
  assign sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module = sequencer_cpu_jtag_debug_module_grant_vector[1];

  //sequencer_cpu/data_master saved-grant sequencer_cpu/jtag_debug_module, which is an e_assign
  assign sequencer_cpu_data_master_saved_grant_sequencer_cpu_jtag_debug_module = sequencer_cpu_jtag_debug_module_arb_winner[1] && sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module;

  //sequencer_cpu/jtag_debug_module chosen-master double-vector, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_chosen_master_double_vector = {sequencer_cpu_jtag_debug_module_master_qreq_vector, sequencer_cpu_jtag_debug_module_master_qreq_vector} & ({~sequencer_cpu_jtag_debug_module_master_qreq_vector, ~sequencer_cpu_jtag_debug_module_master_qreq_vector} + sequencer_cpu_jtag_debug_module_arb_addend);

  //stable onehot encoding of arb winner
  assign sequencer_cpu_jtag_debug_module_arb_winner = (sequencer_cpu_jtag_debug_module_allow_new_arb_cycle & | sequencer_cpu_jtag_debug_module_grant_vector) ? sequencer_cpu_jtag_debug_module_grant_vector : sequencer_cpu_jtag_debug_module_saved_chosen_master_vector;

  //saved sequencer_cpu_jtag_debug_module_grant_vector, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_jtag_debug_module_saved_chosen_master_vector <= 0;
      else if (sequencer_cpu_jtag_debug_module_allow_new_arb_cycle)
          sequencer_cpu_jtag_debug_module_saved_chosen_master_vector <= |sequencer_cpu_jtag_debug_module_grant_vector ? sequencer_cpu_jtag_debug_module_grant_vector : sequencer_cpu_jtag_debug_module_saved_chosen_master_vector;
    end


  //onehot encoding of chosen master
  assign sequencer_cpu_jtag_debug_module_grant_vector = {(sequencer_cpu_jtag_debug_module_chosen_master_double_vector[1] | sequencer_cpu_jtag_debug_module_chosen_master_double_vector[3]),
    (sequencer_cpu_jtag_debug_module_chosen_master_double_vector[0] | sequencer_cpu_jtag_debug_module_chosen_master_double_vector[2])};

  //sequencer_cpu/jtag_debug_module chosen master rotated left, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_chosen_master_rot_left = (sequencer_cpu_jtag_debug_module_arb_winner << 1) ? (sequencer_cpu_jtag_debug_module_arb_winner << 1) : 1;

  //sequencer_cpu/jtag_debug_module's addend for next-master-grant
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_jtag_debug_module_arb_addend <= 1;
      else if (|sequencer_cpu_jtag_debug_module_grant_vector)
          sequencer_cpu_jtag_debug_module_arb_addend <= sequencer_cpu_jtag_debug_module_end_xfer? sequencer_cpu_jtag_debug_module_chosen_master_rot_left : sequencer_cpu_jtag_debug_module_grant_vector;
    end


  assign sequencer_cpu_jtag_debug_module_begintransfer = sequencer_cpu_jtag_debug_module_begins_xfer;
  //sequencer_cpu_jtag_debug_module_reset_n assignment, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_reset_n = reset_n;

  //assign sequencer_cpu_jtag_debug_module_resetrequest_from_sa = sequencer_cpu_jtag_debug_module_resetrequest so that symbol knows where to group signals which may go to master only, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_resetrequest_from_sa = sequencer_cpu_jtag_debug_module_resetrequest;

  assign sequencer_cpu_jtag_debug_module_chipselect = sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module | sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module;
  //sequencer_cpu_jtag_debug_module_firsttransfer first transaction, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_firsttransfer = sequencer_cpu_jtag_debug_module_begins_xfer ? sequencer_cpu_jtag_debug_module_unreg_firsttransfer : sequencer_cpu_jtag_debug_module_reg_firsttransfer;

  //sequencer_cpu_jtag_debug_module_unreg_firsttransfer first transaction, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_unreg_firsttransfer = ~(sequencer_cpu_jtag_debug_module_slavearbiterlockenable & sequencer_cpu_jtag_debug_module_any_continuerequest);

  //sequencer_cpu_jtag_debug_module_reg_firsttransfer first transaction, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_jtag_debug_module_reg_firsttransfer <= 1'b1;
      else if (sequencer_cpu_jtag_debug_module_begins_xfer)
          sequencer_cpu_jtag_debug_module_reg_firsttransfer <= sequencer_cpu_jtag_debug_module_unreg_firsttransfer;
    end


  //sequencer_cpu_jtag_debug_module_beginbursttransfer_internal begin burst transfer, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_beginbursttransfer_internal = sequencer_cpu_jtag_debug_module_begins_xfer;

  //sequencer_cpu_jtag_debug_module_arbitration_holdoff_internal arbitration_holdoff, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_arbitration_holdoff_internal = sequencer_cpu_jtag_debug_module_begins_xfer & sequencer_cpu_jtag_debug_module_firsttransfer;

  //sequencer_cpu_jtag_debug_module_write assignment, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_write = sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module & sequencer_cpu_data_master_write;

  assign shifted_address_to_sequencer_cpu_jtag_debug_module_from_sequencer_cpu_data_master = sequencer_cpu_data_master_address_to_slave;
  //sequencer_cpu_jtag_debug_module_address mux, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_address = (sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module)? (shifted_address_to_sequencer_cpu_jtag_debug_module_from_sequencer_cpu_data_master >> 2) :
    (shifted_address_to_sequencer_cpu_jtag_debug_module_from_sequencer_cpu_instruction_master >> 2);

  assign shifted_address_to_sequencer_cpu_jtag_debug_module_from_sequencer_cpu_instruction_master = sequencer_cpu_instruction_master_address_to_slave;
  //d1_sequencer_cpu_jtag_debug_module_end_xfer register, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_sequencer_cpu_jtag_debug_module_end_xfer <= 1;
      else 
        d1_sequencer_cpu_jtag_debug_module_end_xfer <= sequencer_cpu_jtag_debug_module_end_xfer;
    end


  //sequencer_cpu_jtag_debug_module_waits_for_read in a cycle, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_waits_for_read = sequencer_cpu_jtag_debug_module_in_a_read_cycle & sequencer_cpu_jtag_debug_module_begins_xfer;

  //sequencer_cpu_jtag_debug_module_in_a_read_cycle assignment, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_in_a_read_cycle = (sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module & sequencer_cpu_data_master_read) | (sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module & sequencer_cpu_instruction_master_read);

  //in_a_read_cycle assignment, which is an e_mux
  assign in_a_read_cycle = sequencer_cpu_jtag_debug_module_in_a_read_cycle;

  //sequencer_cpu_jtag_debug_module_waits_for_write in a cycle, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_waits_for_write = sequencer_cpu_jtag_debug_module_in_a_write_cycle & 0;

  //sequencer_cpu_jtag_debug_module_in_a_write_cycle assignment, which is an e_assign
  assign sequencer_cpu_jtag_debug_module_in_a_write_cycle = sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module & sequencer_cpu_data_master_write;

  //in_a_write_cycle assignment, which is an e_mux
  assign in_a_write_cycle = sequencer_cpu_jtag_debug_module_in_a_write_cycle;

  assign wait_for_sequencer_cpu_jtag_debug_module_counter = 0;
  //sequencer_cpu_jtag_debug_module_byteenable byte enable port mux, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_byteenable = (sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module)? sequencer_cpu_data_master_byteenable :
    -1;

  //debugaccess mux, which is an e_mux
  assign sequencer_cpu_jtag_debug_module_debugaccess = (sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module)? sequencer_cpu_data_master_debugaccess :
    0;


//synthesis translate_off
//////////////// SIMULATION-ONLY CONTENTS
  //sequencer_cpu/jtag_debug_module enable non-zero assertions, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          enable_nonzero_assertions <= 0;
      else 
        enable_nonzero_assertions <= 1'b1;
    end


  //grant signals are active simultaneously, which is an e_process
  always @(posedge clk)
    begin
      if (sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module + sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module > 1)
        begin
          $write("%0d ns: > 1 of grant signals are active simultaneously", $time);
          $stop;
        end
    end


  //saved_grant signals are active simultaneously, which is an e_process
  always @(posedge clk)
    begin
      if (sequencer_cpu_data_master_saved_grant_sequencer_cpu_jtag_debug_module + sequencer_cpu_instruction_master_saved_grant_sequencer_cpu_jtag_debug_module > 1)
        begin
          $write("%0d ns: > 1 of saved_grant signals are active simultaneously", $time);
          $stop;
        end
    end



//////////////// END SIMULATION-ONLY CONTENTS

//synthesis translate_on

endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_cpu_data_master_arbitrator (
                                              // inputs:
                                               clk,
                                               d1_sequencer_bridge_0_avalon_slave_end_xfer,
                                               d1_sequencer_cpu_jtag_debug_module_end_xfer,
                                               d1_sequencer_ram_s1_end_xfer,
                                               d1_sequencer_rom_s1_end_xfer,
                                               registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1,
                                               registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1,
                                               reset_n,
                                               sequencer_bridge_0_avalon_slave_readdata_from_sa,
                                               sequencer_bridge_0_avalon_slave_waitrequest_from_sa,
                                               sequencer_cpu_data_master_address,
                                               sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave,
                                               sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module,
                                               sequencer_cpu_data_master_granted_sequencer_ram_s1,
                                               sequencer_cpu_data_master_granted_sequencer_rom_s1,
                                               sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave,
                                               sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module,
                                               sequencer_cpu_data_master_qualified_request_sequencer_ram_s1,
                                               sequencer_cpu_data_master_qualified_request_sequencer_rom_s1,
                                               sequencer_cpu_data_master_read,
                                               sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave,
                                               sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module,
                                               sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1,
                                               sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1,
                                               sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave,
                                               sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module,
                                               sequencer_cpu_data_master_requests_sequencer_ram_s1,
                                               sequencer_cpu_data_master_requests_sequencer_rom_s1,
                                               sequencer_cpu_data_master_write,
                                               sequencer_cpu_jtag_debug_module_readdata_from_sa,
                                               sequencer_ram_s1_readdata_from_sa,
                                               sequencer_rom_s1_readdata_from_sa,

                                              // outputs:
                                               sequencer_cpu_data_master_address_to_slave,
                                               sequencer_cpu_data_master_readdata,
                                               sequencer_cpu_data_master_waitrequest
                                            )
;

  output  [ 18: 0] sequencer_cpu_data_master_address_to_slave;
  output  [ 31: 0] sequencer_cpu_data_master_readdata;
  output           sequencer_cpu_data_master_waitrequest;
  input            clk;
  input            d1_sequencer_bridge_0_avalon_slave_end_xfer;
  input            d1_sequencer_cpu_jtag_debug_module_end_xfer;
  input            d1_sequencer_ram_s1_end_xfer;
  input            d1_sequencer_rom_s1_end_xfer;
  input            registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1;
  input            registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1;
  input            reset_n;
  input   [ 31: 0] sequencer_bridge_0_avalon_slave_readdata_from_sa;
  input            sequencer_bridge_0_avalon_slave_waitrequest_from_sa;
  input   [ 18: 0] sequencer_cpu_data_master_address;
  input            sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave;
  input            sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module;
  input            sequencer_cpu_data_master_granted_sequencer_ram_s1;
  input            sequencer_cpu_data_master_granted_sequencer_rom_s1;
  input            sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave;
  input            sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module;
  input            sequencer_cpu_data_master_qualified_request_sequencer_ram_s1;
  input            sequencer_cpu_data_master_qualified_request_sequencer_rom_s1;
  input            sequencer_cpu_data_master_read;
  input            sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave;
  input            sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module;
  input            sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1;
  input            sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1;
  input            sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave;
  input            sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module;
  input            sequencer_cpu_data_master_requests_sequencer_ram_s1;
  input            sequencer_cpu_data_master_requests_sequencer_rom_s1;
  input            sequencer_cpu_data_master_write;
  input   [ 31: 0] sequencer_cpu_jtag_debug_module_readdata_from_sa;
  input   [ 31: 0] sequencer_ram_s1_readdata_from_sa;
  input   [ 31: 0] sequencer_rom_s1_readdata_from_sa;

  wire    [ 31: 0] p1_registered_sequencer_cpu_data_master_readdata;
  wire             r_0;
  reg     [ 31: 0] registered_sequencer_cpu_data_master_readdata;
  wire    [ 18: 0] sequencer_cpu_data_master_address_to_slave;
  wire    [ 31: 0] sequencer_cpu_data_master_readdata;
  wire             sequencer_cpu_data_master_run;
  reg              sequencer_cpu_data_master_waitrequest;
  //r_0 master_run cascaded wait assignment, which is an e_assign
  assign r_0 = 1 & (sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave | ~sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave) & ((~sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave | ~(sequencer_cpu_data_master_read | sequencer_cpu_data_master_write) | (1 & ~sequencer_bridge_0_avalon_slave_waitrequest_from_sa & (sequencer_cpu_data_master_read | sequencer_cpu_data_master_write)))) & ((~sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave | ~(sequencer_cpu_data_master_read | sequencer_cpu_data_master_write) | (1 & ~sequencer_bridge_0_avalon_slave_waitrequest_from_sa & (sequencer_cpu_data_master_read | sequencer_cpu_data_master_write)))) & 1 & (sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module | ~sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module) & (sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module | ~sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module) & ((~sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module | ~sequencer_cpu_data_master_read | (1 & 1 & sequencer_cpu_data_master_read))) & ((~sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module | ~sequencer_cpu_data_master_write | (1 & sequencer_cpu_data_master_write))) & 1 & (sequencer_cpu_data_master_qualified_request_sequencer_ram_s1 | registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1 | ~sequencer_cpu_data_master_requests_sequencer_ram_s1) & ((~sequencer_cpu_data_master_qualified_request_sequencer_ram_s1 | ~sequencer_cpu_data_master_read | (registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1 & sequencer_cpu_data_master_read))) & ((~sequencer_cpu_data_master_qualified_request_sequencer_ram_s1 | ~(sequencer_cpu_data_master_read | sequencer_cpu_data_master_write) | (1 & (sequencer_cpu_data_master_read | sequencer_cpu_data_master_write)))) & 1 & (sequencer_cpu_data_master_qualified_request_sequencer_rom_s1 | registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1 | ~sequencer_cpu_data_master_requests_sequencer_rom_s1) & (sequencer_cpu_data_master_granted_sequencer_rom_s1 | ~sequencer_cpu_data_master_qualified_request_sequencer_rom_s1) & ((~sequencer_cpu_data_master_qualified_request_sequencer_rom_s1 | ~sequencer_cpu_data_master_read | (registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1 & sequencer_cpu_data_master_read))) & ((~sequencer_cpu_data_master_qualified_request_sequencer_rom_s1 | ~(sequencer_cpu_data_master_read | sequencer_cpu_data_master_write) | (1 & (sequencer_cpu_data_master_read | sequencer_cpu_data_master_write))));

  //cascaded wait assignment, which is an e_assign
  assign sequencer_cpu_data_master_run = r_0;

  //optimize select-logic by passing only those address bits which matter.
  assign sequencer_cpu_data_master_address_to_slave = sequencer_cpu_data_master_address[18 : 0];

  //unpredictable registered wait state incoming data, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          registered_sequencer_cpu_data_master_readdata <= 0;
      else 
        registered_sequencer_cpu_data_master_readdata <= p1_registered_sequencer_cpu_data_master_readdata;
    end


  //registered readdata mux, which is an e_mux
  assign p1_registered_sequencer_cpu_data_master_readdata = {32 {~sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave}} | sequencer_bridge_0_avalon_slave_readdata_from_sa;

  //sequencer_cpu/data_master readdata mux, which is an e_mux
  assign sequencer_cpu_data_master_readdata = ({32 {~sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave}} | registered_sequencer_cpu_data_master_readdata) &
    ({32 {~sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module}} | sequencer_cpu_jtag_debug_module_readdata_from_sa) &
    ({32 {~sequencer_cpu_data_master_requests_sequencer_ram_s1}} | sequencer_ram_s1_readdata_from_sa) &
    ({32 {~sequencer_cpu_data_master_requests_sequencer_rom_s1}} | sequencer_rom_s1_readdata_from_sa);

  //actual waitrequest port, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_data_master_waitrequest <= ~0;
      else 
        sequencer_cpu_data_master_waitrequest <= ~((~(sequencer_cpu_data_master_read | sequencer_cpu_data_master_write))? 0: (sequencer_cpu_data_master_run & sequencer_cpu_data_master_waitrequest));
    end



endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_cpu_instruction_master_arbitrator (
                                                     // inputs:
                                                      clk,
                                                      d1_sequencer_cpu_jtag_debug_module_end_xfer,
                                                      d1_sequencer_rom_s1_end_xfer,
                                                      reset_n,
                                                      sequencer_cpu_instruction_master_address,
                                                      sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module,
                                                      sequencer_cpu_instruction_master_granted_sequencer_rom_s1,
                                                      sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module,
                                                      sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1,
                                                      sequencer_cpu_instruction_master_read,
                                                      sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module,
                                                      sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1,
                                                      sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module,
                                                      sequencer_cpu_instruction_master_requests_sequencer_rom_s1,
                                                      sequencer_cpu_jtag_debug_module_readdata_from_sa,
                                                      sequencer_rom_s1_readdata_from_sa,

                                                     // outputs:
                                                      sequencer_cpu_instruction_master_address_to_slave,
                                                      sequencer_cpu_instruction_master_readdata,
                                                      sequencer_cpu_instruction_master_waitrequest
                                                   )
;

  output  [ 16: 0] sequencer_cpu_instruction_master_address_to_slave;
  output  [ 31: 0] sequencer_cpu_instruction_master_readdata;
  output           sequencer_cpu_instruction_master_waitrequest;
  input            clk;
  input            d1_sequencer_cpu_jtag_debug_module_end_xfer;
  input            d1_sequencer_rom_s1_end_xfer;
  input            reset_n;
  input   [ 16: 0] sequencer_cpu_instruction_master_address;
  input            sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module;
  input            sequencer_cpu_instruction_master_granted_sequencer_rom_s1;
  input            sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module;
  input            sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1;
  input            sequencer_cpu_instruction_master_read;
  input            sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module;
  input            sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1;
  input            sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module;
  input            sequencer_cpu_instruction_master_requests_sequencer_rom_s1;
  input   [ 31: 0] sequencer_cpu_jtag_debug_module_readdata_from_sa;
  input   [ 31: 0] sequencer_rom_s1_readdata_from_sa;

  reg              active_and_waiting_last_time;
  wire             r_0;
  reg     [ 16: 0] sequencer_cpu_instruction_master_address_last_time;
  wire    [ 16: 0] sequencer_cpu_instruction_master_address_to_slave;
  reg              sequencer_cpu_instruction_master_read_last_time;
  wire    [ 31: 0] sequencer_cpu_instruction_master_readdata;
  wire             sequencer_cpu_instruction_master_run;
  wire             sequencer_cpu_instruction_master_waitrequest;
  //r_0 master_run cascaded wait assignment, which is an e_assign
  assign r_0 = 1 & (sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module | ~sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module) & (sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module | ~sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module) & ((~sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module | ~sequencer_cpu_instruction_master_read | (1 & ~d1_sequencer_cpu_jtag_debug_module_end_xfer & sequencer_cpu_instruction_master_read))) & 1 & (sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1 | sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1 | ~sequencer_cpu_instruction_master_requests_sequencer_rom_s1) & (sequencer_cpu_instruction_master_granted_sequencer_rom_s1 | ~sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1) & ((~sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1 | ~sequencer_cpu_instruction_master_read | (sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1 & sequencer_cpu_instruction_master_read)));

  //cascaded wait assignment, which is an e_assign
  assign sequencer_cpu_instruction_master_run = r_0;

  //optimize select-logic by passing only those address bits which matter.
  assign sequencer_cpu_instruction_master_address_to_slave = {sequencer_cpu_instruction_master_address[16],
    2'b0,
    sequencer_cpu_instruction_master_address[13 : 0]};

  //sequencer_cpu/instruction_master readdata mux, which is an e_mux
  assign sequencer_cpu_instruction_master_readdata = ({32 {~sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module}} | sequencer_cpu_jtag_debug_module_readdata_from_sa) &
    ({32 {~sequencer_cpu_instruction_master_requests_sequencer_rom_s1}} | sequencer_rom_s1_readdata_from_sa);

  //actual waitrequest port, which is an e_assign
  assign sequencer_cpu_instruction_master_waitrequest = ~sequencer_cpu_instruction_master_run;


//synthesis translate_off
//////////////// SIMULATION-ONLY CONTENTS
  //sequencer_cpu_instruction_master_address check against wait, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_instruction_master_address_last_time <= 0;
      else 
        sequencer_cpu_instruction_master_address_last_time <= sequencer_cpu_instruction_master_address;
    end


  //sequencer_cpu/instruction_master waited last time, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          active_and_waiting_last_time <= 0;
      else 
        active_and_waiting_last_time <= sequencer_cpu_instruction_master_waitrequest & (sequencer_cpu_instruction_master_read);
    end


  //sequencer_cpu_instruction_master_address matches last port_name, which is an e_process
  always @(posedge clk)
    begin
      if (active_and_waiting_last_time & (sequencer_cpu_instruction_master_address != sequencer_cpu_instruction_master_address_last_time))
        begin
          $write("%0d ns: sequencer_cpu_instruction_master_address did not heed wait!!!", $time);
          $stop;
        end
    end


  //sequencer_cpu_instruction_master_read check against wait, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_instruction_master_read_last_time <= 0;
      else 
        sequencer_cpu_instruction_master_read_last_time <= sequencer_cpu_instruction_master_read;
    end


  //sequencer_cpu_instruction_master_read matches last port_name, which is an e_process
  always @(posedge clk)
    begin
      if (active_and_waiting_last_time & (sequencer_cpu_instruction_master_read != sequencer_cpu_instruction_master_read_last_time))
        begin
          $write("%0d ns: sequencer_cpu_instruction_master_read did not heed wait!!!", $time);
          $stop;
        end
    end



//////////////// END SIMULATION-ONLY CONTENTS

//synthesis translate_on

endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_ram_s1_arbitrator (
                                     // inputs:
                                      clk,
                                      reset_n,
                                      sequencer_cpu_data_master_address_to_slave,
                                      sequencer_cpu_data_master_byteenable,
                                      sequencer_cpu_data_master_read,
                                      sequencer_cpu_data_master_waitrequest,
                                      sequencer_cpu_data_master_write,
                                      sequencer_cpu_data_master_writedata,
                                      sequencer_ram_s1_readdata,

                                     // outputs:
                                      d1_sequencer_ram_s1_end_xfer,
                                      registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1,
                                      sequencer_cpu_data_master_granted_sequencer_ram_s1,
                                      sequencer_cpu_data_master_qualified_request_sequencer_ram_s1,
                                      sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1,
                                      sequencer_cpu_data_master_requests_sequencer_ram_s1,
                                      sequencer_ram_s1_address,
                                      sequencer_ram_s1_byteenable,
                                      sequencer_ram_s1_chipselect,
                                      sequencer_ram_s1_clken,
                                      sequencer_ram_s1_readdata_from_sa,
                                      sequencer_ram_s1_write,
                                      sequencer_ram_s1_writedata
                                   )
;

  output           d1_sequencer_ram_s1_end_xfer;
  output           registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1;
  output           sequencer_cpu_data_master_granted_sequencer_ram_s1;
  output           sequencer_cpu_data_master_qualified_request_sequencer_ram_s1;
  output           sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1;
  output           sequencer_cpu_data_master_requests_sequencer_ram_s1;
  output  [ 10: 0] sequencer_ram_s1_address;
  output  [  3: 0] sequencer_ram_s1_byteenable;
  output           sequencer_ram_s1_chipselect;
  output           sequencer_ram_s1_clken;
  output  [ 31: 0] sequencer_ram_s1_readdata_from_sa;
  output           sequencer_ram_s1_write;
  output  [ 31: 0] sequencer_ram_s1_writedata;
  input            clk;
  input            reset_n;
  input   [ 18: 0] sequencer_cpu_data_master_address_to_slave;
  input   [  3: 0] sequencer_cpu_data_master_byteenable;
  input            sequencer_cpu_data_master_read;
  input            sequencer_cpu_data_master_waitrequest;
  input            sequencer_cpu_data_master_write;
  input   [ 31: 0] sequencer_cpu_data_master_writedata;
  input   [ 31: 0] sequencer_ram_s1_readdata;

  reg              d1_reasons_to_wait;
  reg              d1_sequencer_ram_s1_end_xfer;
  reg              enable_nonzero_assertions;
  wire             end_xfer_arb_share_counter_term_sequencer_ram_s1;
  wire             in_a_read_cycle;
  wire             in_a_write_cycle;
  wire             p1_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register;
  wire             registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1;
  wire             sequencer_cpu_data_master_arbiterlock;
  wire             sequencer_cpu_data_master_arbiterlock2;
  wire             sequencer_cpu_data_master_continuerequest;
  wire             sequencer_cpu_data_master_granted_sequencer_ram_s1;
  wire             sequencer_cpu_data_master_qualified_request_sequencer_ram_s1;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1;
  reg              sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register_in;
  wire             sequencer_cpu_data_master_requests_sequencer_ram_s1;
  wire             sequencer_cpu_data_master_saved_grant_sequencer_ram_s1;
  wire    [ 10: 0] sequencer_ram_s1_address;
  wire             sequencer_ram_s1_allgrants;
  wire             sequencer_ram_s1_allow_new_arb_cycle;
  wire             sequencer_ram_s1_any_bursting_master_saved_grant;
  wire             sequencer_ram_s1_any_continuerequest;
  wire             sequencer_ram_s1_arb_counter_enable;
  reg              sequencer_ram_s1_arb_share_counter;
  wire             sequencer_ram_s1_arb_share_counter_next_value;
  wire             sequencer_ram_s1_arb_share_set_values;
  wire             sequencer_ram_s1_beginbursttransfer_internal;
  wire             sequencer_ram_s1_begins_xfer;
  wire    [  3: 0] sequencer_ram_s1_byteenable;
  wire             sequencer_ram_s1_chipselect;
  wire             sequencer_ram_s1_clken;
  wire             sequencer_ram_s1_end_xfer;
  wire             sequencer_ram_s1_firsttransfer;
  wire             sequencer_ram_s1_grant_vector;
  wire             sequencer_ram_s1_in_a_read_cycle;
  wire             sequencer_ram_s1_in_a_write_cycle;
  wire             sequencer_ram_s1_master_qreq_vector;
  wire             sequencer_ram_s1_non_bursting_master_requests;
  wire    [ 31: 0] sequencer_ram_s1_readdata_from_sa;
  reg              sequencer_ram_s1_reg_firsttransfer;
  reg              sequencer_ram_s1_slavearbiterlockenable;
  wire             sequencer_ram_s1_slavearbiterlockenable2;
  wire             sequencer_ram_s1_unreg_firsttransfer;
  wire             sequencer_ram_s1_waits_for_read;
  wire             sequencer_ram_s1_waits_for_write;
  wire             sequencer_ram_s1_write;
  wire    [ 31: 0] sequencer_ram_s1_writedata;
  wire    [ 18: 0] shifted_address_to_sequencer_ram_s1_from_sequencer_cpu_data_master;
  wire             wait_for_sequencer_ram_s1_counter;
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_reasons_to_wait <= 0;
      else 
        d1_reasons_to_wait <= ~sequencer_ram_s1_end_xfer;
    end


  assign sequencer_ram_s1_begins_xfer = ~d1_reasons_to_wait & ((sequencer_cpu_data_master_qualified_request_sequencer_ram_s1));
  //assign sequencer_ram_s1_readdata_from_sa = sequencer_ram_s1_readdata so that symbol knows where to group signals which may go to master only, which is an e_assign
  assign sequencer_ram_s1_readdata_from_sa = sequencer_ram_s1_readdata;

  assign sequencer_cpu_data_master_requests_sequencer_ram_s1 = ({sequencer_cpu_data_master_address_to_slave[18 : 13] , 13'b0} == 19'h20000) & (sequencer_cpu_data_master_read | sequencer_cpu_data_master_write);
  //registered rdv signal_name registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1 assignment, which is an e_assign
  assign registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1 = sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register_in;

  //sequencer_ram_s1_arb_share_counter set values, which is an e_mux
  assign sequencer_ram_s1_arb_share_set_values = 1;

  //sequencer_ram_s1_non_bursting_master_requests mux, which is an e_mux
  assign sequencer_ram_s1_non_bursting_master_requests = sequencer_cpu_data_master_requests_sequencer_ram_s1;

  //sequencer_ram_s1_any_bursting_master_saved_grant mux, which is an e_mux
  assign sequencer_ram_s1_any_bursting_master_saved_grant = 0;

  //sequencer_ram_s1_arb_share_counter_next_value assignment, which is an e_assign
  assign sequencer_ram_s1_arb_share_counter_next_value = sequencer_ram_s1_firsttransfer ? (sequencer_ram_s1_arb_share_set_values - 1) : |sequencer_ram_s1_arb_share_counter ? (sequencer_ram_s1_arb_share_counter - 1) : 0;

  //sequencer_ram_s1_allgrants all slave grants, which is an e_mux
  assign sequencer_ram_s1_allgrants = |sequencer_ram_s1_grant_vector;

  //sequencer_ram_s1_end_xfer assignment, which is an e_assign
  assign sequencer_ram_s1_end_xfer = ~(sequencer_ram_s1_waits_for_read | sequencer_ram_s1_waits_for_write);

  //end_xfer_arb_share_counter_term_sequencer_ram_s1 arb share counter enable term, which is an e_assign
  assign end_xfer_arb_share_counter_term_sequencer_ram_s1 = sequencer_ram_s1_end_xfer & (~sequencer_ram_s1_any_bursting_master_saved_grant | in_a_read_cycle | in_a_write_cycle);

  //sequencer_ram_s1_arb_share_counter arbitration counter enable, which is an e_assign
  assign sequencer_ram_s1_arb_counter_enable = (end_xfer_arb_share_counter_term_sequencer_ram_s1 & sequencer_ram_s1_allgrants) | (end_xfer_arb_share_counter_term_sequencer_ram_s1 & ~sequencer_ram_s1_non_bursting_master_requests);

  //sequencer_ram_s1_arb_share_counter counter, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_ram_s1_arb_share_counter <= 0;
      else if (sequencer_ram_s1_arb_counter_enable)
          sequencer_ram_s1_arb_share_counter <= sequencer_ram_s1_arb_share_counter_next_value;
    end


  //sequencer_ram_s1_slavearbiterlockenable slave enables arbiterlock, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_ram_s1_slavearbiterlockenable <= 0;
      else if ((|sequencer_ram_s1_master_qreq_vector & end_xfer_arb_share_counter_term_sequencer_ram_s1) | (end_xfer_arb_share_counter_term_sequencer_ram_s1 & ~sequencer_ram_s1_non_bursting_master_requests))
          sequencer_ram_s1_slavearbiterlockenable <= |sequencer_ram_s1_arb_share_counter_next_value;
    end


  //sequencer_cpu/data_master sequencer_ram/s1 arbiterlock, which is an e_assign
  assign sequencer_cpu_data_master_arbiterlock = sequencer_ram_s1_slavearbiterlockenable & sequencer_cpu_data_master_continuerequest;

  //sequencer_ram_s1_slavearbiterlockenable2 slave enables arbiterlock2, which is an e_assign
  assign sequencer_ram_s1_slavearbiterlockenable2 = |sequencer_ram_s1_arb_share_counter_next_value;

  //sequencer_cpu/data_master sequencer_ram/s1 arbiterlock2, which is an e_assign
  assign sequencer_cpu_data_master_arbiterlock2 = sequencer_ram_s1_slavearbiterlockenable2 & sequencer_cpu_data_master_continuerequest;

  //sequencer_ram_s1_any_continuerequest at least one master continues requesting, which is an e_assign
  assign sequencer_ram_s1_any_continuerequest = 1;

  //sequencer_cpu_data_master_continuerequest continued request, which is an e_assign
  assign sequencer_cpu_data_master_continuerequest = 1;

  assign sequencer_cpu_data_master_qualified_request_sequencer_ram_s1 = sequencer_cpu_data_master_requests_sequencer_ram_s1 & ~((sequencer_cpu_data_master_read & ((|sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register))) | ((~sequencer_cpu_data_master_waitrequest) & sequencer_cpu_data_master_write));
  //sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register_in mux for readlatency shift register, which is an e_mux
  assign sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register_in = sequencer_cpu_data_master_granted_sequencer_ram_s1 & sequencer_cpu_data_master_read & ~sequencer_ram_s1_waits_for_read & ~(|sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register);

  //shift register p1 sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register in if flush, otherwise shift left, which is an e_mux
  assign p1_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register = {sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register, sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register_in};

  //sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register for remembering which master asked for a fixed latency read, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register <= 0;
      else 
        sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register <= p1_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register;
    end


  //local readdatavalid sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1, which is an e_mux
  assign sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1 = sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1_shift_register;

  //sequencer_ram_s1_writedata mux, which is an e_mux
  assign sequencer_ram_s1_writedata = sequencer_cpu_data_master_writedata;

  //mux sequencer_ram_s1_clken, which is an e_mux
  assign sequencer_ram_s1_clken = 1'b1;

  //master is always granted when requested
  assign sequencer_cpu_data_master_granted_sequencer_ram_s1 = sequencer_cpu_data_master_qualified_request_sequencer_ram_s1;

  //sequencer_cpu/data_master saved-grant sequencer_ram/s1, which is an e_assign
  assign sequencer_cpu_data_master_saved_grant_sequencer_ram_s1 = sequencer_cpu_data_master_requests_sequencer_ram_s1;

  //allow new arb cycle for sequencer_ram/s1, which is an e_assign
  assign sequencer_ram_s1_allow_new_arb_cycle = 1;

  //placeholder chosen master
  assign sequencer_ram_s1_grant_vector = 1;

  //placeholder vector of master qualified-requests
  assign sequencer_ram_s1_master_qreq_vector = 1;

  assign sequencer_ram_s1_chipselect = sequencer_cpu_data_master_granted_sequencer_ram_s1;
  //sequencer_ram_s1_firsttransfer first transaction, which is an e_assign
  assign sequencer_ram_s1_firsttransfer = sequencer_ram_s1_begins_xfer ? sequencer_ram_s1_unreg_firsttransfer : sequencer_ram_s1_reg_firsttransfer;

  //sequencer_ram_s1_unreg_firsttransfer first transaction, which is an e_assign
  assign sequencer_ram_s1_unreg_firsttransfer = ~(sequencer_ram_s1_slavearbiterlockenable & sequencer_ram_s1_any_continuerequest);

  //sequencer_ram_s1_reg_firsttransfer first transaction, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_ram_s1_reg_firsttransfer <= 1'b1;
      else if (sequencer_ram_s1_begins_xfer)
          sequencer_ram_s1_reg_firsttransfer <= sequencer_ram_s1_unreg_firsttransfer;
    end


  //sequencer_ram_s1_beginbursttransfer_internal begin burst transfer, which is an e_assign
  assign sequencer_ram_s1_beginbursttransfer_internal = sequencer_ram_s1_begins_xfer;

  //sequencer_ram_s1_write assignment, which is an e_mux
  assign sequencer_ram_s1_write = sequencer_cpu_data_master_granted_sequencer_ram_s1 & sequencer_cpu_data_master_write;

  assign shifted_address_to_sequencer_ram_s1_from_sequencer_cpu_data_master = sequencer_cpu_data_master_address_to_slave;
  //sequencer_ram_s1_address mux, which is an e_mux
  assign sequencer_ram_s1_address = shifted_address_to_sequencer_ram_s1_from_sequencer_cpu_data_master >> 2;

  //d1_sequencer_ram_s1_end_xfer register, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_sequencer_ram_s1_end_xfer <= 1;
      else 
        d1_sequencer_ram_s1_end_xfer <= sequencer_ram_s1_end_xfer;
    end


  //sequencer_ram_s1_waits_for_read in a cycle, which is an e_mux
  assign sequencer_ram_s1_waits_for_read = sequencer_ram_s1_in_a_read_cycle & 0;

  //sequencer_ram_s1_in_a_read_cycle assignment, which is an e_assign
  assign sequencer_ram_s1_in_a_read_cycle = sequencer_cpu_data_master_granted_sequencer_ram_s1 & sequencer_cpu_data_master_read;

  //in_a_read_cycle assignment, which is an e_mux
  assign in_a_read_cycle = sequencer_ram_s1_in_a_read_cycle;

  //sequencer_ram_s1_waits_for_write in a cycle, which is an e_mux
  assign sequencer_ram_s1_waits_for_write = sequencer_ram_s1_in_a_write_cycle & 0;

  //sequencer_ram_s1_in_a_write_cycle assignment, which is an e_assign
  assign sequencer_ram_s1_in_a_write_cycle = sequencer_cpu_data_master_granted_sequencer_ram_s1 & sequencer_cpu_data_master_write;

  //in_a_write_cycle assignment, which is an e_mux
  assign in_a_write_cycle = sequencer_ram_s1_in_a_write_cycle;

  assign wait_for_sequencer_ram_s1_counter = 0;
  //sequencer_ram_s1_byteenable byte enable port mux, which is an e_mux
  assign sequencer_ram_s1_byteenable = (sequencer_cpu_data_master_granted_sequencer_ram_s1)? sequencer_cpu_data_master_byteenable :
    -1;


//synthesis translate_off
//////////////// SIMULATION-ONLY CONTENTS
  //sequencer_ram/s1 enable non-zero assertions, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          enable_nonzero_assertions <= 0;
      else 
        enable_nonzero_assertions <= 1'b1;
    end



//////////////// END SIMULATION-ONLY CONTENTS

//synthesis translate_on

endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_rom_s1_arbitrator (
                                     // inputs:
                                      clk,
                                      reset_n,
                                      sequencer_cpu_data_master_address_to_slave,
                                      sequencer_cpu_data_master_byteenable,
                                      sequencer_cpu_data_master_read,
                                      sequencer_cpu_data_master_waitrequest,
                                      sequencer_cpu_data_master_write,
                                      sequencer_cpu_data_master_writedata,
                                      sequencer_cpu_instruction_master_address_to_slave,
                                      sequencer_cpu_instruction_master_read,
                                      sequencer_rom_s1_readdata,

                                     // outputs:
                                      d1_sequencer_rom_s1_end_xfer,
                                      registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1,
                                      sequencer_cpu_data_master_granted_sequencer_rom_s1,
                                      sequencer_cpu_data_master_qualified_request_sequencer_rom_s1,
                                      sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1,
                                      sequencer_cpu_data_master_requests_sequencer_rom_s1,
                                      sequencer_cpu_instruction_master_granted_sequencer_rom_s1,
                                      sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1,
                                      sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1,
                                      sequencer_cpu_instruction_master_requests_sequencer_rom_s1,
                                      sequencer_rom_s1_address,
                                      sequencer_rom_s1_byteenable,
                                      sequencer_rom_s1_chipselect,
                                      sequencer_rom_s1_clken,
                                      sequencer_rom_s1_readdata_from_sa,
                                      sequencer_rom_s1_write,
                                      sequencer_rom_s1_writedata
                                   )
;

  output           d1_sequencer_rom_s1_end_xfer;
  output           registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1;
  output           sequencer_cpu_data_master_granted_sequencer_rom_s1;
  output           sequencer_cpu_data_master_qualified_request_sequencer_rom_s1;
  output           sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1;
  output           sequencer_cpu_data_master_requests_sequencer_rom_s1;
  output           sequencer_cpu_instruction_master_granted_sequencer_rom_s1;
  output           sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1;
  output           sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1;
  output           sequencer_cpu_instruction_master_requests_sequencer_rom_s1;
  output  [ 11: 0] sequencer_rom_s1_address;
  output  [  3: 0] sequencer_rom_s1_byteenable;
  output           sequencer_rom_s1_chipselect;
  output           sequencer_rom_s1_clken;
  output  [ 31: 0] sequencer_rom_s1_readdata_from_sa;
  output           sequencer_rom_s1_write;
  output  [ 31: 0] sequencer_rom_s1_writedata;
  input            clk;
  input            reset_n;
  input   [ 18: 0] sequencer_cpu_data_master_address_to_slave;
  input   [  3: 0] sequencer_cpu_data_master_byteenable;
  input            sequencer_cpu_data_master_read;
  input            sequencer_cpu_data_master_waitrequest;
  input            sequencer_cpu_data_master_write;
  input   [ 31: 0] sequencer_cpu_data_master_writedata;
  input   [ 16: 0] sequencer_cpu_instruction_master_address_to_slave;
  input            sequencer_cpu_instruction_master_read;
  input   [ 31: 0] sequencer_rom_s1_readdata;

  reg              d1_reasons_to_wait;
  reg              d1_sequencer_rom_s1_end_xfer;
  reg              enable_nonzero_assertions;
  wire             end_xfer_arb_share_counter_term_sequencer_rom_s1;
  wire             in_a_read_cycle;
  wire             in_a_write_cycle;
  reg              last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_rom_s1;
  reg              last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_rom_s1;
  wire             p1_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register;
  wire             p1_sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register;
  wire             registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1;
  wire             sequencer_cpu_data_master_arbiterlock;
  wire             sequencer_cpu_data_master_arbiterlock2;
  wire             sequencer_cpu_data_master_continuerequest;
  wire             sequencer_cpu_data_master_granted_sequencer_rom_s1;
  wire             sequencer_cpu_data_master_qualified_request_sequencer_rom_s1;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1;
  reg              sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register_in;
  wire             sequencer_cpu_data_master_requests_sequencer_rom_s1;
  wire             sequencer_cpu_data_master_saved_grant_sequencer_rom_s1;
  wire             sequencer_cpu_instruction_master_arbiterlock;
  wire             sequencer_cpu_instruction_master_arbiterlock2;
  wire             sequencer_cpu_instruction_master_continuerequest;
  wire             sequencer_cpu_instruction_master_granted_sequencer_rom_s1;
  wire             sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1;
  wire             sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1;
  reg              sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register;
  wire             sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register_in;
  wire             sequencer_cpu_instruction_master_requests_sequencer_rom_s1;
  wire             sequencer_cpu_instruction_master_saved_grant_sequencer_rom_s1;
  wire    [ 11: 0] sequencer_rom_s1_address;
  wire             sequencer_rom_s1_allgrants;
  wire             sequencer_rom_s1_allow_new_arb_cycle;
  wire             sequencer_rom_s1_any_bursting_master_saved_grant;
  wire             sequencer_rom_s1_any_continuerequest;
  reg     [  1: 0] sequencer_rom_s1_arb_addend;
  wire             sequencer_rom_s1_arb_counter_enable;
  reg              sequencer_rom_s1_arb_share_counter;
  wire             sequencer_rom_s1_arb_share_counter_next_value;
  wire             sequencer_rom_s1_arb_share_set_values;
  wire    [  1: 0] sequencer_rom_s1_arb_winner;
  wire             sequencer_rom_s1_arbitration_holdoff_internal;
  wire             sequencer_rom_s1_beginbursttransfer_internal;
  wire             sequencer_rom_s1_begins_xfer;
  wire    [  3: 0] sequencer_rom_s1_byteenable;
  wire             sequencer_rom_s1_chipselect;
  wire    [  3: 0] sequencer_rom_s1_chosen_master_double_vector;
  wire    [  1: 0] sequencer_rom_s1_chosen_master_rot_left;
  wire             sequencer_rom_s1_clken;
  wire             sequencer_rom_s1_end_xfer;
  wire             sequencer_rom_s1_firsttransfer;
  wire    [  1: 0] sequencer_rom_s1_grant_vector;
  wire             sequencer_rom_s1_in_a_read_cycle;
  wire             sequencer_rom_s1_in_a_write_cycle;
  wire    [  1: 0] sequencer_rom_s1_master_qreq_vector;
  wire             sequencer_rom_s1_non_bursting_master_requests;
  wire    [ 31: 0] sequencer_rom_s1_readdata_from_sa;
  reg              sequencer_rom_s1_reg_firsttransfer;
  reg     [  1: 0] sequencer_rom_s1_saved_chosen_master_vector;
  reg              sequencer_rom_s1_slavearbiterlockenable;
  wire             sequencer_rom_s1_slavearbiterlockenable2;
  wire             sequencer_rom_s1_unreg_firsttransfer;
  wire             sequencer_rom_s1_waits_for_read;
  wire             sequencer_rom_s1_waits_for_write;
  wire             sequencer_rom_s1_write;
  wire    [ 31: 0] sequencer_rom_s1_writedata;
  wire    [ 18: 0] shifted_address_to_sequencer_rom_s1_from_sequencer_cpu_data_master;
  wire    [ 16: 0] shifted_address_to_sequencer_rom_s1_from_sequencer_cpu_instruction_master;
  wire             wait_for_sequencer_rom_s1_counter;
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_reasons_to_wait <= 0;
      else 
        d1_reasons_to_wait <= ~sequencer_rom_s1_end_xfer;
    end


  assign sequencer_rom_s1_begins_xfer = ~d1_reasons_to_wait & ((sequencer_cpu_data_master_qualified_request_sequencer_rom_s1 | sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1));
  //assign sequencer_rom_s1_readdata_from_sa = sequencer_rom_s1_readdata so that symbol knows where to group signals which may go to master only, which is an e_assign
  assign sequencer_rom_s1_readdata_from_sa = sequencer_rom_s1_readdata;

  assign sequencer_cpu_data_master_requests_sequencer_rom_s1 = ({sequencer_cpu_data_master_address_to_slave[18 : 14] , 14'b0} == 19'h10000) & (sequencer_cpu_data_master_read | sequencer_cpu_data_master_write);
  //registered rdv signal_name registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1 assignment, which is an e_assign
  assign registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1 = sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register_in;

  //sequencer_rom_s1_arb_share_counter set values, which is an e_mux
  assign sequencer_rom_s1_arb_share_set_values = 1;

  //sequencer_rom_s1_non_bursting_master_requests mux, which is an e_mux
  assign sequencer_rom_s1_non_bursting_master_requests = sequencer_cpu_data_master_requests_sequencer_rom_s1 |
    sequencer_cpu_instruction_master_requests_sequencer_rom_s1 |
    sequencer_cpu_data_master_requests_sequencer_rom_s1 |
    sequencer_cpu_instruction_master_requests_sequencer_rom_s1;

  //sequencer_rom_s1_any_bursting_master_saved_grant mux, which is an e_mux
  assign sequencer_rom_s1_any_bursting_master_saved_grant = 0;

  //sequencer_rom_s1_arb_share_counter_next_value assignment, which is an e_assign
  assign sequencer_rom_s1_arb_share_counter_next_value = sequencer_rom_s1_firsttransfer ? (sequencer_rom_s1_arb_share_set_values - 1) : |sequencer_rom_s1_arb_share_counter ? (sequencer_rom_s1_arb_share_counter - 1) : 0;

  //sequencer_rom_s1_allgrants all slave grants, which is an e_mux
  assign sequencer_rom_s1_allgrants = (|sequencer_rom_s1_grant_vector) |
    (|sequencer_rom_s1_grant_vector) |
    (|sequencer_rom_s1_grant_vector) |
    (|sequencer_rom_s1_grant_vector);

  //sequencer_rom_s1_end_xfer assignment, which is an e_assign
  assign sequencer_rom_s1_end_xfer = ~(sequencer_rom_s1_waits_for_read | sequencer_rom_s1_waits_for_write);

  //end_xfer_arb_share_counter_term_sequencer_rom_s1 arb share counter enable term, which is an e_assign
  assign end_xfer_arb_share_counter_term_sequencer_rom_s1 = sequencer_rom_s1_end_xfer & (~sequencer_rom_s1_any_bursting_master_saved_grant | in_a_read_cycle | in_a_write_cycle);

  //sequencer_rom_s1_arb_share_counter arbitration counter enable, which is an e_assign
  assign sequencer_rom_s1_arb_counter_enable = (end_xfer_arb_share_counter_term_sequencer_rom_s1 & sequencer_rom_s1_allgrants) | (end_xfer_arb_share_counter_term_sequencer_rom_s1 & ~sequencer_rom_s1_non_bursting_master_requests);

  //sequencer_rom_s1_arb_share_counter counter, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_rom_s1_arb_share_counter <= 0;
      else if (sequencer_rom_s1_arb_counter_enable)
          sequencer_rom_s1_arb_share_counter <= sequencer_rom_s1_arb_share_counter_next_value;
    end


  //sequencer_rom_s1_slavearbiterlockenable slave enables arbiterlock, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_rom_s1_slavearbiterlockenable <= 0;
      else if ((|sequencer_rom_s1_master_qreq_vector & end_xfer_arb_share_counter_term_sequencer_rom_s1) | (end_xfer_arb_share_counter_term_sequencer_rom_s1 & ~sequencer_rom_s1_non_bursting_master_requests))
          sequencer_rom_s1_slavearbiterlockenable <= |sequencer_rom_s1_arb_share_counter_next_value;
    end


  //sequencer_cpu/data_master sequencer_rom/s1 arbiterlock, which is an e_assign
  assign sequencer_cpu_data_master_arbiterlock = sequencer_rom_s1_slavearbiterlockenable & sequencer_cpu_data_master_continuerequest;

  //sequencer_rom_s1_slavearbiterlockenable2 slave enables arbiterlock2, which is an e_assign
  assign sequencer_rom_s1_slavearbiterlockenable2 = |sequencer_rom_s1_arb_share_counter_next_value;

  //sequencer_cpu/data_master sequencer_rom/s1 arbiterlock2, which is an e_assign
  assign sequencer_cpu_data_master_arbiterlock2 = sequencer_rom_s1_slavearbiterlockenable2 & sequencer_cpu_data_master_continuerequest;

  //sequencer_cpu/instruction_master sequencer_rom/s1 arbiterlock, which is an e_assign
  assign sequencer_cpu_instruction_master_arbiterlock = sequencer_rom_s1_slavearbiterlockenable & sequencer_cpu_instruction_master_continuerequest;

  //sequencer_cpu/instruction_master sequencer_rom/s1 arbiterlock2, which is an e_assign
  assign sequencer_cpu_instruction_master_arbiterlock2 = sequencer_rom_s1_slavearbiterlockenable2 & sequencer_cpu_instruction_master_continuerequest;

  //sequencer_cpu/instruction_master granted sequencer_rom/s1 last time, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_rom_s1 <= 0;
      else 
        last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_rom_s1 <= sequencer_cpu_instruction_master_saved_grant_sequencer_rom_s1 ? 1 : (sequencer_rom_s1_arbitration_holdoff_internal | ~sequencer_cpu_instruction_master_requests_sequencer_rom_s1) ? 0 : last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_rom_s1;
    end


  //sequencer_cpu_instruction_master_continuerequest continued request, which is an e_mux
  assign sequencer_cpu_instruction_master_continuerequest = last_cycle_sequencer_cpu_instruction_master_granted_slave_sequencer_rom_s1 & sequencer_cpu_instruction_master_requests_sequencer_rom_s1;

  //sequencer_rom_s1_any_continuerequest at least one master continues requesting, which is an e_mux
  assign sequencer_rom_s1_any_continuerequest = sequencer_cpu_instruction_master_continuerequest |
    sequencer_cpu_data_master_continuerequest;

  assign sequencer_cpu_data_master_qualified_request_sequencer_rom_s1 = sequencer_cpu_data_master_requests_sequencer_rom_s1 & ~((sequencer_cpu_data_master_read & ((|sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register))) | ((~sequencer_cpu_data_master_waitrequest) & sequencer_cpu_data_master_write) | sequencer_cpu_instruction_master_arbiterlock);
  //sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register_in mux for readlatency shift register, which is an e_mux
  assign sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register_in = sequencer_cpu_data_master_granted_sequencer_rom_s1 & sequencer_cpu_data_master_read & ~sequencer_rom_s1_waits_for_read & ~(|sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register);

  //shift register p1 sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register in if flush, otherwise shift left, which is an e_mux
  assign p1_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register = {sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register, sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register_in};

  //sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register for remembering which master asked for a fixed latency read, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register <= 0;
      else 
        sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register <= p1_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register;
    end


  //local readdatavalid sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1, which is an e_mux
  assign sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1 = sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1_shift_register;

  //sequencer_rom_s1_writedata mux, which is an e_mux
  assign sequencer_rom_s1_writedata = sequencer_cpu_data_master_writedata;

  //mux sequencer_rom_s1_clken, which is an e_mux
  assign sequencer_rom_s1_clken = 1'b1;

  assign sequencer_cpu_instruction_master_requests_sequencer_rom_s1 = (({sequencer_cpu_instruction_master_address_to_slave[16 : 14] , 14'b0} == 17'h10000) & (sequencer_cpu_instruction_master_read)) & sequencer_cpu_instruction_master_read;
  //sequencer_cpu/data_master granted sequencer_rom/s1 last time, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_rom_s1 <= 0;
      else 
        last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_rom_s1 <= sequencer_cpu_data_master_saved_grant_sequencer_rom_s1 ? 1 : (sequencer_rom_s1_arbitration_holdoff_internal | ~sequencer_cpu_data_master_requests_sequencer_rom_s1) ? 0 : last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_rom_s1;
    end


  //sequencer_cpu_data_master_continuerequest continued request, which is an e_mux
  assign sequencer_cpu_data_master_continuerequest = last_cycle_sequencer_cpu_data_master_granted_slave_sequencer_rom_s1 & sequencer_cpu_data_master_requests_sequencer_rom_s1;

  assign sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1 = sequencer_cpu_instruction_master_requests_sequencer_rom_s1 & ~((sequencer_cpu_instruction_master_read & ((|sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register))) | sequencer_cpu_data_master_arbiterlock);
  //sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register_in mux for readlatency shift register, which is an e_mux
  assign sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register_in = sequencer_cpu_instruction_master_granted_sequencer_rom_s1 & sequencer_cpu_instruction_master_read & ~sequencer_rom_s1_waits_for_read & ~(|sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register);

  //shift register p1 sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register in if flush, otherwise shift left, which is an e_mux
  assign p1_sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register = {sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register, sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register_in};

  //sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register for remembering which master asked for a fixed latency read, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register <= 0;
      else 
        sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register <= p1_sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register;
    end


  //local readdatavalid sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1, which is an e_mux
  assign sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1 = sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1_shift_register;

  //allow new arb cycle for sequencer_rom/s1, which is an e_assign
  assign sequencer_rom_s1_allow_new_arb_cycle = ~sequencer_cpu_data_master_arbiterlock & ~sequencer_cpu_instruction_master_arbiterlock;

  //sequencer_cpu/instruction_master assignment into master qualified-requests vector for sequencer_rom/s1, which is an e_assign
  assign sequencer_rom_s1_master_qreq_vector[0] = sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1;

  //sequencer_cpu/instruction_master grant sequencer_rom/s1, which is an e_assign
  assign sequencer_cpu_instruction_master_granted_sequencer_rom_s1 = sequencer_rom_s1_grant_vector[0];

  //sequencer_cpu/instruction_master saved-grant sequencer_rom/s1, which is an e_assign
  assign sequencer_cpu_instruction_master_saved_grant_sequencer_rom_s1 = sequencer_rom_s1_arb_winner[0] && sequencer_cpu_instruction_master_requests_sequencer_rom_s1;

  //sequencer_cpu/data_master assignment into master qualified-requests vector for sequencer_rom/s1, which is an e_assign
  assign sequencer_rom_s1_master_qreq_vector[1] = sequencer_cpu_data_master_qualified_request_sequencer_rom_s1;

  //sequencer_cpu/data_master grant sequencer_rom/s1, which is an e_assign
  assign sequencer_cpu_data_master_granted_sequencer_rom_s1 = sequencer_rom_s1_grant_vector[1];

  //sequencer_cpu/data_master saved-grant sequencer_rom/s1, which is an e_assign
  assign sequencer_cpu_data_master_saved_grant_sequencer_rom_s1 = sequencer_rom_s1_arb_winner[1] && sequencer_cpu_data_master_requests_sequencer_rom_s1;

  //sequencer_rom/s1 chosen-master double-vector, which is an e_assign
  assign sequencer_rom_s1_chosen_master_double_vector = {sequencer_rom_s1_master_qreq_vector, sequencer_rom_s1_master_qreq_vector} & ({~sequencer_rom_s1_master_qreq_vector, ~sequencer_rom_s1_master_qreq_vector} + sequencer_rom_s1_arb_addend);

  //stable onehot encoding of arb winner
  assign sequencer_rom_s1_arb_winner = (sequencer_rom_s1_allow_new_arb_cycle & | sequencer_rom_s1_grant_vector) ? sequencer_rom_s1_grant_vector : sequencer_rom_s1_saved_chosen_master_vector;

  //saved sequencer_rom_s1_grant_vector, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_rom_s1_saved_chosen_master_vector <= 0;
      else if (sequencer_rom_s1_allow_new_arb_cycle)
          sequencer_rom_s1_saved_chosen_master_vector <= |sequencer_rom_s1_grant_vector ? sequencer_rom_s1_grant_vector : sequencer_rom_s1_saved_chosen_master_vector;
    end


  //onehot encoding of chosen master
  assign sequencer_rom_s1_grant_vector = {(sequencer_rom_s1_chosen_master_double_vector[1] | sequencer_rom_s1_chosen_master_double_vector[3]),
    (sequencer_rom_s1_chosen_master_double_vector[0] | sequencer_rom_s1_chosen_master_double_vector[2])};

  //sequencer_rom/s1 chosen master rotated left, which is an e_assign
  assign sequencer_rom_s1_chosen_master_rot_left = (sequencer_rom_s1_arb_winner << 1) ? (sequencer_rom_s1_arb_winner << 1) : 1;

  //sequencer_rom/s1's addend for next-master-grant
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_rom_s1_arb_addend <= 1;
      else if (|sequencer_rom_s1_grant_vector)
          sequencer_rom_s1_arb_addend <= sequencer_rom_s1_end_xfer? sequencer_rom_s1_chosen_master_rot_left : sequencer_rom_s1_grant_vector;
    end


  assign sequencer_rom_s1_chipselect = sequencer_cpu_data_master_granted_sequencer_rom_s1 | sequencer_cpu_instruction_master_granted_sequencer_rom_s1;
  //sequencer_rom_s1_firsttransfer first transaction, which is an e_assign
  assign sequencer_rom_s1_firsttransfer = sequencer_rom_s1_begins_xfer ? sequencer_rom_s1_unreg_firsttransfer : sequencer_rom_s1_reg_firsttransfer;

  //sequencer_rom_s1_unreg_firsttransfer first transaction, which is an e_assign
  assign sequencer_rom_s1_unreg_firsttransfer = ~(sequencer_rom_s1_slavearbiterlockenable & sequencer_rom_s1_any_continuerequest);

  //sequencer_rom_s1_reg_firsttransfer first transaction, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_rom_s1_reg_firsttransfer <= 1'b1;
      else if (sequencer_rom_s1_begins_xfer)
          sequencer_rom_s1_reg_firsttransfer <= sequencer_rom_s1_unreg_firsttransfer;
    end


  //sequencer_rom_s1_beginbursttransfer_internal begin burst transfer, which is an e_assign
  assign sequencer_rom_s1_beginbursttransfer_internal = sequencer_rom_s1_begins_xfer;

  //sequencer_rom_s1_arbitration_holdoff_internal arbitration_holdoff, which is an e_assign
  assign sequencer_rom_s1_arbitration_holdoff_internal = sequencer_rom_s1_begins_xfer & sequencer_rom_s1_firsttransfer;

  //sequencer_rom_s1_write assignment, which is an e_mux
  assign sequencer_rom_s1_write = sequencer_cpu_data_master_granted_sequencer_rom_s1 & sequencer_cpu_data_master_write;

  assign shifted_address_to_sequencer_rom_s1_from_sequencer_cpu_data_master = sequencer_cpu_data_master_address_to_slave;
  //sequencer_rom_s1_address mux, which is an e_mux
  assign sequencer_rom_s1_address = (sequencer_cpu_data_master_granted_sequencer_rom_s1)? (shifted_address_to_sequencer_rom_s1_from_sequencer_cpu_data_master >> 2) :
    (shifted_address_to_sequencer_rom_s1_from_sequencer_cpu_instruction_master >> 2);

  assign shifted_address_to_sequencer_rom_s1_from_sequencer_cpu_instruction_master = sequencer_cpu_instruction_master_address_to_slave;
  //d1_sequencer_rom_s1_end_xfer register, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_sequencer_rom_s1_end_xfer <= 1;
      else 
        d1_sequencer_rom_s1_end_xfer <= sequencer_rom_s1_end_xfer;
    end


  //sequencer_rom_s1_waits_for_read in a cycle, which is an e_mux
  assign sequencer_rom_s1_waits_for_read = sequencer_rom_s1_in_a_read_cycle & 0;

  //sequencer_rom_s1_in_a_read_cycle assignment, which is an e_assign
  assign sequencer_rom_s1_in_a_read_cycle = (sequencer_cpu_data_master_granted_sequencer_rom_s1 & sequencer_cpu_data_master_read) | (sequencer_cpu_instruction_master_granted_sequencer_rom_s1 & sequencer_cpu_instruction_master_read);

  //in_a_read_cycle assignment, which is an e_mux
  assign in_a_read_cycle = sequencer_rom_s1_in_a_read_cycle;

  //sequencer_rom_s1_waits_for_write in a cycle, which is an e_mux
  assign sequencer_rom_s1_waits_for_write = sequencer_rom_s1_in_a_write_cycle & 0;

  //sequencer_rom_s1_in_a_write_cycle assignment, which is an e_assign
  assign sequencer_rom_s1_in_a_write_cycle = sequencer_cpu_data_master_granted_sequencer_rom_s1 & sequencer_cpu_data_master_write;

  //in_a_write_cycle assignment, which is an e_mux
  assign in_a_write_cycle = sequencer_rom_s1_in_a_write_cycle;

  assign wait_for_sequencer_rom_s1_counter = 0;
  //sequencer_rom_s1_byteenable byte enable port mux, which is an e_mux
  assign sequencer_rom_s1_byteenable = (sequencer_cpu_data_master_granted_sequencer_rom_s1)? sequencer_cpu_data_master_byteenable :
    -1;


//synthesis translate_off
//////////////// SIMULATION-ONLY CONTENTS
  //sequencer_rom/s1 enable non-zero assertions, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          enable_nonzero_assertions <= 0;
      else 
        enable_nonzero_assertions <= 1'b1;
    end


  //grant signals are active simultaneously, which is an e_process
  always @(posedge clk)
    begin
      if (sequencer_cpu_data_master_granted_sequencer_rom_s1 + sequencer_cpu_instruction_master_granted_sequencer_rom_s1 > 1)
        begin
          $write("%0d ns: > 1 of grant signals are active simultaneously", $time);
          $stop;
        end
    end


  //saved_grant signals are active simultaneously, which is an e_process
  always @(posedge clk)
    begin
      if (sequencer_cpu_data_master_saved_grant_sequencer_rom_s1 + sequencer_cpu_instruction_master_saved_grant_sequencer_rom_s1 > 1)
        begin
          $write("%0d ns: > 1 of saved_grant signals are active simultaneously", $time);
          $stop;
        end
    end



//////////////// END SIMULATION-ONLY CONTENTS

//synthesis translate_on

endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_rom_s2_arbitrator (
                                     // inputs:
                                      clk,
                                      reset_n,
                                      rom_bridge_0_avalon_master_address_to_slave,
                                      rom_bridge_0_avalon_master_write,
                                      rom_bridge_0_avalon_master_writedata,
                                      sequencer_rom_s2_readdata,

                                     // outputs:
                                      d1_sequencer_rom_s2_end_xfer,
                                      rom_bridge_0_granted_sequencer_rom_s2,
                                      rom_bridge_0_qualified_request_sequencer_rom_s2,
                                      rom_bridge_0_requests_sequencer_rom_s2,
                                      sequencer_rom_s2_address,
                                      sequencer_rom_s2_byteenable,
                                      sequencer_rom_s2_chipselect,
                                      sequencer_rom_s2_clken,
                                      sequencer_rom_s2_readdata_from_sa,
                                      sequencer_rom_s2_write,
                                      sequencer_rom_s2_writedata
                                   )
;

  output           d1_sequencer_rom_s2_end_xfer;
  output           rom_bridge_0_granted_sequencer_rom_s2;
  output           rom_bridge_0_qualified_request_sequencer_rom_s2;
  output           rom_bridge_0_requests_sequencer_rom_s2;
  output  [ 11: 0] sequencer_rom_s2_address;
  output  [  3: 0] sequencer_rom_s2_byteenable;
  output           sequencer_rom_s2_chipselect;
  output           sequencer_rom_s2_clken;
  output  [ 31: 0] sequencer_rom_s2_readdata_from_sa;
  output           sequencer_rom_s2_write;
  output  [ 31: 0] sequencer_rom_s2_writedata;
  input            clk;
  input            reset_n;
  input   [ 13: 0] rom_bridge_0_avalon_master_address_to_slave;
  input            rom_bridge_0_avalon_master_write;
  input   [ 31: 0] rom_bridge_0_avalon_master_writedata;
  input   [ 31: 0] sequencer_rom_s2_readdata;

  reg              d1_reasons_to_wait;
  reg              d1_sequencer_rom_s2_end_xfer;
  reg              enable_nonzero_assertions;
  wire             end_xfer_arb_share_counter_term_sequencer_rom_s2;
  wire             in_a_read_cycle;
  wire             in_a_write_cycle;
  wire             rom_bridge_0_avalon_master_arbiterlock;
  wire             rom_bridge_0_avalon_master_arbiterlock2;
  wire             rom_bridge_0_avalon_master_continuerequest;
  wire             rom_bridge_0_granted_sequencer_rom_s2;
  wire             rom_bridge_0_qualified_request_sequencer_rom_s2;
  wire             rom_bridge_0_requests_sequencer_rom_s2;
  wire             rom_bridge_0_saved_grant_sequencer_rom_s2;
  wire    [ 11: 0] sequencer_rom_s2_address;
  wire             sequencer_rom_s2_allgrants;
  wire             sequencer_rom_s2_allow_new_arb_cycle;
  wire             sequencer_rom_s2_any_bursting_master_saved_grant;
  wire             sequencer_rom_s2_any_continuerequest;
  wire             sequencer_rom_s2_arb_counter_enable;
  reg              sequencer_rom_s2_arb_share_counter;
  wire             sequencer_rom_s2_arb_share_counter_next_value;
  wire             sequencer_rom_s2_arb_share_set_values;
  wire             sequencer_rom_s2_beginbursttransfer_internal;
  wire             sequencer_rom_s2_begins_xfer;
  wire    [  3: 0] sequencer_rom_s2_byteenable;
  wire             sequencer_rom_s2_chipselect;
  wire             sequencer_rom_s2_clken;
  wire             sequencer_rom_s2_end_xfer;
  wire             sequencer_rom_s2_firsttransfer;
  wire             sequencer_rom_s2_grant_vector;
  wire             sequencer_rom_s2_in_a_read_cycle;
  wire             sequencer_rom_s2_in_a_write_cycle;
  wire             sequencer_rom_s2_master_qreq_vector;
  wire             sequencer_rom_s2_non_bursting_master_requests;
  wire    [ 31: 0] sequencer_rom_s2_readdata_from_sa;
  reg              sequencer_rom_s2_reg_firsttransfer;
  reg              sequencer_rom_s2_slavearbiterlockenable;
  wire             sequencer_rom_s2_slavearbiterlockenable2;
  wire             sequencer_rom_s2_unreg_firsttransfer;
  wire             sequencer_rom_s2_waits_for_read;
  wire             sequencer_rom_s2_waits_for_write;
  wire             sequencer_rom_s2_write;
  wire    [ 31: 0] sequencer_rom_s2_writedata;
  wire    [ 13: 0] shifted_address_to_sequencer_rom_s2_from_rom_bridge_0_avalon_master;
  wire             wait_for_sequencer_rom_s2_counter;
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_reasons_to_wait <= 0;
      else 
        d1_reasons_to_wait <= ~sequencer_rom_s2_end_xfer;
    end


  assign sequencer_rom_s2_begins_xfer = ~d1_reasons_to_wait & ((rom_bridge_0_qualified_request_sequencer_rom_s2));
  //assign sequencer_rom_s2_readdata_from_sa = sequencer_rom_s2_readdata so that symbol knows where to group signals which may go to master only, which is an e_assign
  assign sequencer_rom_s2_readdata_from_sa = sequencer_rom_s2_readdata;

  assign rom_bridge_0_requests_sequencer_rom_s2 = ((1) & (rom_bridge_0_avalon_master_write)) & rom_bridge_0_avalon_master_write;
  //sequencer_rom_s2_arb_share_counter set values, which is an e_mux
  assign sequencer_rom_s2_arb_share_set_values = 1;

  //sequencer_rom_s2_non_bursting_master_requests mux, which is an e_mux
  assign sequencer_rom_s2_non_bursting_master_requests = rom_bridge_0_requests_sequencer_rom_s2;

  //sequencer_rom_s2_any_bursting_master_saved_grant mux, which is an e_mux
  assign sequencer_rom_s2_any_bursting_master_saved_grant = 0;

  //sequencer_rom_s2_arb_share_counter_next_value assignment, which is an e_assign
  assign sequencer_rom_s2_arb_share_counter_next_value = sequencer_rom_s2_firsttransfer ? (sequencer_rom_s2_arb_share_set_values - 1) : |sequencer_rom_s2_arb_share_counter ? (sequencer_rom_s2_arb_share_counter - 1) : 0;

  //sequencer_rom_s2_allgrants all slave grants, which is an e_mux
  assign sequencer_rom_s2_allgrants = |sequencer_rom_s2_grant_vector;

  //sequencer_rom_s2_end_xfer assignment, which is an e_assign
  assign sequencer_rom_s2_end_xfer = ~(sequencer_rom_s2_waits_for_read | sequencer_rom_s2_waits_for_write);

  //end_xfer_arb_share_counter_term_sequencer_rom_s2 arb share counter enable term, which is an e_assign
  assign end_xfer_arb_share_counter_term_sequencer_rom_s2 = sequencer_rom_s2_end_xfer & (~sequencer_rom_s2_any_bursting_master_saved_grant | in_a_read_cycle | in_a_write_cycle);

  //sequencer_rom_s2_arb_share_counter arbitration counter enable, which is an e_assign
  assign sequencer_rom_s2_arb_counter_enable = (end_xfer_arb_share_counter_term_sequencer_rom_s2 & sequencer_rom_s2_allgrants) | (end_xfer_arb_share_counter_term_sequencer_rom_s2 & ~sequencer_rom_s2_non_bursting_master_requests);

  //sequencer_rom_s2_arb_share_counter counter, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_rom_s2_arb_share_counter <= 0;
      else if (sequencer_rom_s2_arb_counter_enable)
          sequencer_rom_s2_arb_share_counter <= sequencer_rom_s2_arb_share_counter_next_value;
    end


  //sequencer_rom_s2_slavearbiterlockenable slave enables arbiterlock, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_rom_s2_slavearbiterlockenable <= 0;
      else if ((|sequencer_rom_s2_master_qreq_vector & end_xfer_arb_share_counter_term_sequencer_rom_s2) | (end_xfer_arb_share_counter_term_sequencer_rom_s2 & ~sequencer_rom_s2_non_bursting_master_requests))
          sequencer_rom_s2_slavearbiterlockenable <= |sequencer_rom_s2_arb_share_counter_next_value;
    end


  //rom_bridge_0/avalon_master sequencer_rom/s2 arbiterlock, which is an e_assign
  assign rom_bridge_0_avalon_master_arbiterlock = sequencer_rom_s2_slavearbiterlockenable & rom_bridge_0_avalon_master_continuerequest;

  //sequencer_rom_s2_slavearbiterlockenable2 slave enables arbiterlock2, which is an e_assign
  assign sequencer_rom_s2_slavearbiterlockenable2 = |sequencer_rom_s2_arb_share_counter_next_value;

  //rom_bridge_0/avalon_master sequencer_rom/s2 arbiterlock2, which is an e_assign
  assign rom_bridge_0_avalon_master_arbiterlock2 = sequencer_rom_s2_slavearbiterlockenable2 & rom_bridge_0_avalon_master_continuerequest;

  //sequencer_rom_s2_any_continuerequest at least one master continues requesting, which is an e_assign
  assign sequencer_rom_s2_any_continuerequest = 1;

  //rom_bridge_0_avalon_master_continuerequest continued request, which is an e_assign
  assign rom_bridge_0_avalon_master_continuerequest = 1;

  assign rom_bridge_0_qualified_request_sequencer_rom_s2 = rom_bridge_0_requests_sequencer_rom_s2;
  //sequencer_rom_s2_writedata mux, which is an e_mux
  assign sequencer_rom_s2_writedata = rom_bridge_0_avalon_master_writedata;

  //mux sequencer_rom_s2_clken, which is an e_mux
  assign sequencer_rom_s2_clken = 1'b1;

  //master is always granted when requested
  assign rom_bridge_0_granted_sequencer_rom_s2 = rom_bridge_0_qualified_request_sequencer_rom_s2;

  //rom_bridge_0/avalon_master saved-grant sequencer_rom/s2, which is an e_assign
  assign rom_bridge_0_saved_grant_sequencer_rom_s2 = rom_bridge_0_requests_sequencer_rom_s2;

  //allow new arb cycle for sequencer_rom/s2, which is an e_assign
  assign sequencer_rom_s2_allow_new_arb_cycle = 1;

  //placeholder chosen master
  assign sequencer_rom_s2_grant_vector = 1;

  //placeholder vector of master qualified-requests
  assign sequencer_rom_s2_master_qreq_vector = 1;

  assign sequencer_rom_s2_chipselect = rom_bridge_0_granted_sequencer_rom_s2;
  //sequencer_rom_s2_firsttransfer first transaction, which is an e_assign
  assign sequencer_rom_s2_firsttransfer = sequencer_rom_s2_begins_xfer ? sequencer_rom_s2_unreg_firsttransfer : sequencer_rom_s2_reg_firsttransfer;

  //sequencer_rom_s2_unreg_firsttransfer first transaction, which is an e_assign
  assign sequencer_rom_s2_unreg_firsttransfer = ~(sequencer_rom_s2_slavearbiterlockenable & sequencer_rom_s2_any_continuerequest);

  //sequencer_rom_s2_reg_firsttransfer first transaction, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          sequencer_rom_s2_reg_firsttransfer <= 1'b1;
      else if (sequencer_rom_s2_begins_xfer)
          sequencer_rom_s2_reg_firsttransfer <= sequencer_rom_s2_unreg_firsttransfer;
    end


  //sequencer_rom_s2_beginbursttransfer_internal begin burst transfer, which is an e_assign
  assign sequencer_rom_s2_beginbursttransfer_internal = sequencer_rom_s2_begins_xfer;

  //sequencer_rom_s2_write assignment, which is an e_mux
  assign sequencer_rom_s2_write = rom_bridge_0_granted_sequencer_rom_s2 & rom_bridge_0_avalon_master_write;

  assign shifted_address_to_sequencer_rom_s2_from_rom_bridge_0_avalon_master = rom_bridge_0_avalon_master_address_to_slave;
  //sequencer_rom_s2_address mux, which is an e_mux
  assign sequencer_rom_s2_address = shifted_address_to_sequencer_rom_s2_from_rom_bridge_0_avalon_master >> 2;

  //d1_sequencer_rom_s2_end_xfer register, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          d1_sequencer_rom_s2_end_xfer <= 1;
      else 
        d1_sequencer_rom_s2_end_xfer <= sequencer_rom_s2_end_xfer;
    end


  //sequencer_rom_s2_waits_for_read in a cycle, which is an e_mux
  assign sequencer_rom_s2_waits_for_read = sequencer_rom_s2_in_a_read_cycle & 0;

  //sequencer_rom_s2_in_a_read_cycle assignment, which is an e_assign
  assign sequencer_rom_s2_in_a_read_cycle = 0;

  //in_a_read_cycle assignment, which is an e_mux
  assign in_a_read_cycle = sequencer_rom_s2_in_a_read_cycle;

  //sequencer_rom_s2_waits_for_write in a cycle, which is an e_mux
  assign sequencer_rom_s2_waits_for_write = sequencer_rom_s2_in_a_write_cycle & 0;

  //sequencer_rom_s2_in_a_write_cycle assignment, which is an e_assign
  assign sequencer_rom_s2_in_a_write_cycle = rom_bridge_0_granted_sequencer_rom_s2 & rom_bridge_0_avalon_master_write;

  //in_a_write_cycle assignment, which is an e_mux
  assign in_a_write_cycle = sequencer_rom_s2_in_a_write_cycle;

  assign wait_for_sequencer_rom_s2_counter = 0;
  //sequencer_rom_s2_byteenable byte enable port mux, which is an e_mux
  assign sequencer_rom_s2_byteenable = (rom_bridge_0_granted_sequencer_rom_s2)? {4 {1'b1}} :
    -1;


//synthesis translate_off
//////////////// SIMULATION-ONLY CONTENTS
  //sequencer_rom/s2 enable non-zero assertions, which is an e_register
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          enable_nonzero_assertions <= 0;
      else 
        enable_nonzero_assertions <= 1'b1;
    end



//////////////// END SIMULATION-ONLY CONTENTS

//synthesis translate_on

endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_ctrl_reset_clk_0_domain_synch_module (
                                                        // inputs:
                                                         clk,
                                                         data_in,
                                                         reset_n,

                                                        // outputs:
                                                         data_out
                                                      )
;

  output           data_out;
  input            clk;
  input            data_in;
  input            reset_n;

  reg              data_in_d1 /* synthesis ALTERA_ATTRIBUTE = "{-from \"*\"} CUT=ON ; PRESERVE_REGISTER=ON ; SUPPRESS_DA_RULE_INTERNAL=R101"  */;
  reg              data_out /* synthesis ALTERA_ATTRIBUTE = "PRESERVE_REGISTER=ON ; SUPPRESS_DA_RULE_INTERNAL=R101 ; GLOBAL_SIGNAL=OFF"  */;
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          data_in_d1 <= 0;
      else 
        data_in_d1 <= data_in;
    end


  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          data_out <= 0;
      else 
        data_out <= data_in_d1;
    end



endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_ctrl_reset_rom_write_clock_domain_synch_module (
                                                                  // inputs:
                                                                   clk,
                                                                   data_in,
                                                                   reset_n,

                                                                  // outputs:
                                                                   data_out
                                                                )
;

  output           data_out;
  input            clk;
  input            data_in;
  input            reset_n;

  reg              data_in_d1 /* synthesis ALTERA_ATTRIBUTE = "{-from \"*\"} CUT=ON ; PRESERVE_REGISTER=ON ; SUPPRESS_DA_RULE_INTERNAL=R101"  */;
  reg              data_out /* synthesis ALTERA_ATTRIBUTE = "PRESERVE_REGISTER=ON ; SUPPRESS_DA_RULE_INTERNAL=R101"  */;
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          data_in_d1 <= 0;
      else 
        data_in_d1 <= data_in;
    end


  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          data_out <= 0;
      else 
        data_out <= data_in_d1;
    end



endmodule



// turn off superfluous verilog processor warnings 
// altera message_level Level1 
// altera message_off 10034 10035 10036 10037 10230 10240 10030 

module ddr2_v10_1_0002_sequencer_ctrl (
                        // 1) global signals:
                         clk_0,
                         reset_n,
                         rom_write_clock,

                        // the_rom_bridge_0
                         init_busy_from_the_rom_bridge_0,
                         init_to_the_rom_bridge_0,
                         rom_address_from_the_rom_bridge_0,
                         rom_data_ready_to_the_rom_bridge_0,
                         rom_data_to_the_rom_bridge_0,
                         rom_rden_from_the_rom_bridge_0,

                        // the_sequencer_bridge_0
                         avlt_address_from_the_sequencer_bridge_0,
                         avlt_clk_from_the_sequencer_bridge_0,
                         avlt_read_from_the_sequencer_bridge_0,
                         avlt_readdata_to_the_sequencer_bridge_0,
                         avlt_reset_n_from_the_sequencer_bridge_0,
                         avlt_waitrequest_to_the_sequencer_bridge_0,
                         avlt_write_from_the_sequencer_bridge_0,
                         avlt_writedata_from_the_sequencer_bridge_0
                      )
;

  output  [ 15: 0] avlt_address_from_the_sequencer_bridge_0;
  output           avlt_clk_from_the_sequencer_bridge_0;
  output           avlt_read_from_the_sequencer_bridge_0;
  output           avlt_reset_n_from_the_sequencer_bridge_0;
  output           avlt_write_from_the_sequencer_bridge_0;
  output  [ 31: 0] avlt_writedata_from_the_sequencer_bridge_0;
  output           init_busy_from_the_rom_bridge_0;
  output  [ 13: 0] rom_address_from_the_rom_bridge_0;
  output           rom_rden_from_the_rom_bridge_0;
  input   [ 31: 0] avlt_readdata_to_the_sequencer_bridge_0;
  input            avlt_waitrequest_to_the_sequencer_bridge_0;
  input            clk_0;
  input            init_to_the_rom_bridge_0;
  input            reset_n;
  input            rom_data_ready_to_the_rom_bridge_0;
  input   [ 31: 0] rom_data_to_the_rom_bridge_0;
  input            rom_write_clock;

  wire    [ 15: 0] avlt_address_from_the_sequencer_bridge_0;
  wire             avlt_clk_from_the_sequencer_bridge_0;
  wire             avlt_read_from_the_sequencer_bridge_0;
  wire             avlt_reset_n_from_the_sequencer_bridge_0;
  wire             avlt_write_from_the_sequencer_bridge_0;
  wire    [ 31: 0] avlt_writedata_from_the_sequencer_bridge_0;
  wire             clk_0_reset_n;
  wire             d1_sequencer_bridge_0_avalon_slave_end_xfer;
  wire             d1_sequencer_cpu_jtag_debug_module_end_xfer;
  wire             d1_sequencer_ram_s1_end_xfer;
  wire             d1_sequencer_rom_s1_end_xfer;
  wire             d1_sequencer_rom_s2_end_xfer;
  wire             init_busy_from_the_rom_bridge_0;
  wire             registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1;
  wire             registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1;
  wire             reset_n_sources;
  wire    [ 13: 0] rom_address_from_the_rom_bridge_0;
  wire    [ 13: 0] rom_bridge_0_avalon_master_address;
  wire    [ 13: 0] rom_bridge_0_avalon_master_address_to_slave;
  wire             rom_bridge_0_avalon_master_waitrequest;
  wire             rom_bridge_0_avalon_master_write;
  wire    [ 31: 0] rom_bridge_0_avalon_master_writedata;
  wire             rom_bridge_0_granted_sequencer_rom_s2;
  wire             rom_bridge_0_qualified_request_sequencer_rom_s2;
  wire             rom_bridge_0_requests_sequencer_rom_s2;
  wire             rom_rden_from_the_rom_bridge_0;
  wire             rom_write_clock_reset_n;
  wire    [ 15: 0] sequencer_bridge_0_avalon_slave_address;
  wire             sequencer_bridge_0_avalon_slave_read;
  wire    [ 31: 0] sequencer_bridge_0_avalon_slave_readdata;
  wire    [ 31: 0] sequencer_bridge_0_avalon_slave_readdata_from_sa;
  wire             sequencer_bridge_0_avalon_slave_reset_n;
  wire             sequencer_bridge_0_avalon_slave_waitrequest;
  wire             sequencer_bridge_0_avalon_slave_waitrequest_from_sa;
  wire             sequencer_bridge_0_avalon_slave_write;
  wire    [ 31: 0] sequencer_bridge_0_avalon_slave_writedata;
  wire    [ 18: 0] sequencer_cpu_data_master_address;
  wire    [ 18: 0] sequencer_cpu_data_master_address_to_slave;
  wire    [  3: 0] sequencer_cpu_data_master_byteenable;
  wire             sequencer_cpu_data_master_debugaccess;
  wire             sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave;
  wire             sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_granted_sequencer_ram_s1;
  wire             sequencer_cpu_data_master_granted_sequencer_rom_s1;
  wire    [ 31: 0] sequencer_cpu_data_master_irq;
  wire             sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave;
  wire             sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_qualified_request_sequencer_ram_s1;
  wire             sequencer_cpu_data_master_qualified_request_sequencer_rom_s1;
  wire             sequencer_cpu_data_master_read;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1;
  wire             sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1;
  wire    [ 31: 0] sequencer_cpu_data_master_readdata;
  wire             sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave;
  wire             sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_data_master_requests_sequencer_ram_s1;
  wire             sequencer_cpu_data_master_requests_sequencer_rom_s1;
  wire             sequencer_cpu_data_master_waitrequest;
  wire             sequencer_cpu_data_master_write;
  wire    [ 31: 0] sequencer_cpu_data_master_writedata;
  wire    [ 16: 0] sequencer_cpu_instruction_master_address;
  wire    [ 16: 0] sequencer_cpu_instruction_master_address_to_slave;
  wire             sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_granted_sequencer_rom_s1;
  wire             sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1;
  wire             sequencer_cpu_instruction_master_read;
  wire             sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1;
  wire    [ 31: 0] sequencer_cpu_instruction_master_readdata;
  wire             sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module;
  wire             sequencer_cpu_instruction_master_requests_sequencer_rom_s1;
  wire             sequencer_cpu_instruction_master_waitrequest;
  wire    [  8: 0] sequencer_cpu_jtag_debug_module_address;
  wire             sequencer_cpu_jtag_debug_module_begintransfer;
  wire    [  3: 0] sequencer_cpu_jtag_debug_module_byteenable;
  wire             sequencer_cpu_jtag_debug_module_chipselect;
  wire             sequencer_cpu_jtag_debug_module_debugaccess;
  wire    [ 31: 0] sequencer_cpu_jtag_debug_module_readdata;
  wire    [ 31: 0] sequencer_cpu_jtag_debug_module_readdata_from_sa;
  wire             sequencer_cpu_jtag_debug_module_reset_n;
  wire             sequencer_cpu_jtag_debug_module_resetrequest;
  wire             sequencer_cpu_jtag_debug_module_resetrequest_from_sa;
  wire             sequencer_cpu_jtag_debug_module_write;
  wire    [ 31: 0] sequencer_cpu_jtag_debug_module_writedata;
  wire    [ 10: 0] sequencer_ram_s1_address;
  wire    [  3: 0] sequencer_ram_s1_byteenable;
  wire             sequencer_ram_s1_chipselect;
  wire             sequencer_ram_s1_clken;
  wire    [ 31: 0] sequencer_ram_s1_readdata;
  wire    [ 31: 0] sequencer_ram_s1_readdata_from_sa;
  wire             sequencer_ram_s1_write;
  wire    [ 31: 0] sequencer_ram_s1_writedata;
  wire    [ 11: 0] sequencer_rom_s1_address;
  wire    [  3: 0] sequencer_rom_s1_byteenable;
  wire             sequencer_rom_s1_chipselect;
  wire             sequencer_rom_s1_clken;
  wire    [ 31: 0] sequencer_rom_s1_readdata;
  wire    [ 31: 0] sequencer_rom_s1_readdata_from_sa;
  wire             sequencer_rom_s1_write;
  wire    [ 31: 0] sequencer_rom_s1_writedata;
  wire    [ 11: 0] sequencer_rom_s2_address;
  wire    [  3: 0] sequencer_rom_s2_byteenable;
  wire             sequencer_rom_s2_chipselect;
  wire             sequencer_rom_s2_clken;
  wire    [ 31: 0] sequencer_rom_s2_readdata;
  wire    [ 31: 0] sequencer_rom_s2_readdata_from_sa;
  wire             sequencer_rom_s2_write;
  wire    [ 31: 0] sequencer_rom_s2_writedata;
  ddr2_v10_1_0002_rom_bridge_0_avalon_master_arbitrator the_rom_bridge_0_avalon_master
    (
      .clk                                             (rom_write_clock),
      .d1_sequencer_rom_s2_end_xfer                    (d1_sequencer_rom_s2_end_xfer),
      .reset_n                                         (rom_write_clock_reset_n),
      .rom_bridge_0_avalon_master_address              (rom_bridge_0_avalon_master_address),
      .rom_bridge_0_avalon_master_address_to_slave     (rom_bridge_0_avalon_master_address_to_slave),
      .rom_bridge_0_avalon_master_waitrequest          (rom_bridge_0_avalon_master_waitrequest),
      .rom_bridge_0_avalon_master_write                (rom_bridge_0_avalon_master_write),
      .rom_bridge_0_avalon_master_writedata            (rom_bridge_0_avalon_master_writedata),
      .rom_bridge_0_granted_sequencer_rom_s2           (rom_bridge_0_granted_sequencer_rom_s2),
      .rom_bridge_0_qualified_request_sequencer_rom_s2 (rom_bridge_0_qualified_request_sequencer_rom_s2),
      .rom_bridge_0_requests_sequencer_rom_s2          (rom_bridge_0_requests_sequencer_rom_s2)
    );

  ddr2_v10_1_0002_rom_bridge_0 the_rom_bridge_0
    (
      .avlm_address     (rom_bridge_0_avalon_master_address),
      .avlm_waitrequest (rom_bridge_0_avalon_master_waitrequest),
      .avlm_wren        (rom_bridge_0_avalon_master_write),
      .avlm_writedata   (rom_bridge_0_avalon_master_writedata),
      .init             (init_to_the_rom_bridge_0),
      .init_busy        (init_busy_from_the_rom_bridge_0),
      .rom_address      (rom_address_from_the_rom_bridge_0),
      .rom_data         (rom_data_to_the_rom_bridge_0),
      .rom_data_ready   (rom_data_ready_to_the_rom_bridge_0),
      .rom_rden         (rom_rden_from_the_rom_bridge_0),
      .write_clock      (rom_write_clock)
    );

  ddr2_v10_1_0002_sequencer_bridge_0_avalon_slave_arbitrator the_sequencer_bridge_0_avalon_slave
    (
      .clk                                                                         (clk_0),
      .d1_sequencer_bridge_0_avalon_slave_end_xfer                                 (d1_sequencer_bridge_0_avalon_slave_end_xfer),
      .reset_n                                                                     (clk_0_reset_n),
      .sequencer_bridge_0_avalon_slave_address                                     (sequencer_bridge_0_avalon_slave_address),
      .sequencer_bridge_0_avalon_slave_read                                        (sequencer_bridge_0_avalon_slave_read),
      .sequencer_bridge_0_avalon_slave_readdata                                    (sequencer_bridge_0_avalon_slave_readdata),
      .sequencer_bridge_0_avalon_slave_readdata_from_sa                            (sequencer_bridge_0_avalon_slave_readdata_from_sa),
      .sequencer_bridge_0_avalon_slave_reset_n                                     (sequencer_bridge_0_avalon_slave_reset_n),
      .sequencer_bridge_0_avalon_slave_waitrequest                                 (sequencer_bridge_0_avalon_slave_waitrequest),
      .sequencer_bridge_0_avalon_slave_waitrequest_from_sa                         (sequencer_bridge_0_avalon_slave_waitrequest_from_sa),
      .sequencer_bridge_0_avalon_slave_write                                       (sequencer_bridge_0_avalon_slave_write),
      .sequencer_bridge_0_avalon_slave_writedata                                   (sequencer_bridge_0_avalon_slave_writedata),
      .sequencer_cpu_data_master_address_to_slave                                  (sequencer_cpu_data_master_address_to_slave),
      .sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave           (sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave),
      .sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave (sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave),
      .sequencer_cpu_data_master_read                                              (sequencer_cpu_data_master_read),
      .sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave   (sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave),
      .sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave          (sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave),
      .sequencer_cpu_data_master_waitrequest                                       (sequencer_cpu_data_master_waitrequest),
      .sequencer_cpu_data_master_write                                             (sequencer_cpu_data_master_write),
      .sequencer_cpu_data_master_writedata                                         (sequencer_cpu_data_master_writedata)
    );

  ddr2_v10_1_0002_sequencer_bridge_0 the_sequencer_bridge_0
    (
      .avlf_address     (sequencer_bridge_0_avalon_slave_address),
      .avlf_clk         (clk_0),
      .avlf_read        (sequencer_bridge_0_avalon_slave_read),
      .avlf_readdata    (sequencer_bridge_0_avalon_slave_readdata),
      .avlf_reset_n     (sequencer_bridge_0_avalon_slave_reset_n),
      .avlf_waitrequest (sequencer_bridge_0_avalon_slave_waitrequest),
      .avlf_write       (sequencer_bridge_0_avalon_slave_write),
      .avlf_writedata   (sequencer_bridge_0_avalon_slave_writedata),
      .avlt_address     (avlt_address_from_the_sequencer_bridge_0),
      .avlt_clk         (avlt_clk_from_the_sequencer_bridge_0),
      .avlt_read        (avlt_read_from_the_sequencer_bridge_0),
      .avlt_readdata    (avlt_readdata_to_the_sequencer_bridge_0),
      .avlt_reset_n     (avlt_reset_n_from_the_sequencer_bridge_0),
      .avlt_waitrequest (avlt_waitrequest_to_the_sequencer_bridge_0),
      .avlt_write       (avlt_write_from_the_sequencer_bridge_0),
      .avlt_writedata   (avlt_writedata_from_the_sequencer_bridge_0)
    );

  ddr2_v10_1_0002_sequencer_cpu_jtag_debug_module_arbitrator the_sequencer_cpu_jtag_debug_module
    (
      .clk                                                                                (clk_0),
      .d1_sequencer_cpu_jtag_debug_module_end_xfer                                        (d1_sequencer_cpu_jtag_debug_module_end_xfer),
      .reset_n                                                                            (clk_0_reset_n),
      .sequencer_cpu_data_master_address_to_slave                                         (sequencer_cpu_data_master_address_to_slave),
      .sequencer_cpu_data_master_byteenable                                               (sequencer_cpu_data_master_byteenable),
      .sequencer_cpu_data_master_debugaccess                                              (sequencer_cpu_data_master_debugaccess),
      .sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module                  (sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module        (sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_data_master_read                                                     (sequencer_cpu_data_master_read),
      .sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module          (sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module                 (sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_data_master_waitrequest                                              (sequencer_cpu_data_master_waitrequest),
      .sequencer_cpu_data_master_write                                                    (sequencer_cpu_data_master_write),
      .sequencer_cpu_data_master_writedata                                                (sequencer_cpu_data_master_writedata),
      .sequencer_cpu_instruction_master_address_to_slave                                  (sequencer_cpu_instruction_master_address_to_slave),
      .sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module           (sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module (sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_instruction_master_read                                              (sequencer_cpu_instruction_master_read),
      .sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module   (sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module          (sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_jtag_debug_module_address                                            (sequencer_cpu_jtag_debug_module_address),
      .sequencer_cpu_jtag_debug_module_begintransfer                                      (sequencer_cpu_jtag_debug_module_begintransfer),
      .sequencer_cpu_jtag_debug_module_byteenable                                         (sequencer_cpu_jtag_debug_module_byteenable),
      .sequencer_cpu_jtag_debug_module_chipselect                                         (sequencer_cpu_jtag_debug_module_chipselect),
      .sequencer_cpu_jtag_debug_module_debugaccess                                        (sequencer_cpu_jtag_debug_module_debugaccess),
      .sequencer_cpu_jtag_debug_module_readdata                                           (sequencer_cpu_jtag_debug_module_readdata),
      .sequencer_cpu_jtag_debug_module_readdata_from_sa                                   (sequencer_cpu_jtag_debug_module_readdata_from_sa),
      .sequencer_cpu_jtag_debug_module_reset_n                                            (sequencer_cpu_jtag_debug_module_reset_n),
      .sequencer_cpu_jtag_debug_module_resetrequest                                       (sequencer_cpu_jtag_debug_module_resetrequest),
      .sequencer_cpu_jtag_debug_module_resetrequest_from_sa                               (sequencer_cpu_jtag_debug_module_resetrequest_from_sa),
      .sequencer_cpu_jtag_debug_module_write                                              (sequencer_cpu_jtag_debug_module_write),
      .sequencer_cpu_jtag_debug_module_writedata                                          (sequencer_cpu_jtag_debug_module_writedata)
    );

  ddr2_v10_1_0002_sequencer_cpu_data_master_arbitrator the_sequencer_cpu_data_master
    (
      .clk                                                                         (clk_0),
      .d1_sequencer_bridge_0_avalon_slave_end_xfer                                 (d1_sequencer_bridge_0_avalon_slave_end_xfer),
      .d1_sequencer_cpu_jtag_debug_module_end_xfer                                 (d1_sequencer_cpu_jtag_debug_module_end_xfer),
      .d1_sequencer_ram_s1_end_xfer                                                (d1_sequencer_ram_s1_end_xfer),
      .d1_sequencer_rom_s1_end_xfer                                                (d1_sequencer_rom_s1_end_xfer),
      .registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1       (registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1),
      .registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1       (registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1),
      .reset_n                                                                     (clk_0_reset_n),
      .sequencer_bridge_0_avalon_slave_readdata_from_sa                            (sequencer_bridge_0_avalon_slave_readdata_from_sa),
      .sequencer_bridge_0_avalon_slave_waitrequest_from_sa                         (sequencer_bridge_0_avalon_slave_waitrequest_from_sa),
      .sequencer_cpu_data_master_address                                           (sequencer_cpu_data_master_address),
      .sequencer_cpu_data_master_address_to_slave                                  (sequencer_cpu_data_master_address_to_slave),
      .sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave           (sequencer_cpu_data_master_granted_sequencer_bridge_0_avalon_slave),
      .sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module           (sequencer_cpu_data_master_granted_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_data_master_granted_sequencer_ram_s1                          (sequencer_cpu_data_master_granted_sequencer_ram_s1),
      .sequencer_cpu_data_master_granted_sequencer_rom_s1                          (sequencer_cpu_data_master_granted_sequencer_rom_s1),
      .sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave (sequencer_cpu_data_master_qualified_request_sequencer_bridge_0_avalon_slave),
      .sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module (sequencer_cpu_data_master_qualified_request_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_data_master_qualified_request_sequencer_ram_s1                (sequencer_cpu_data_master_qualified_request_sequencer_ram_s1),
      .sequencer_cpu_data_master_qualified_request_sequencer_rom_s1                (sequencer_cpu_data_master_qualified_request_sequencer_rom_s1),
      .sequencer_cpu_data_master_read                                              (sequencer_cpu_data_master_read),
      .sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave   (sequencer_cpu_data_master_read_data_valid_sequencer_bridge_0_avalon_slave),
      .sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module   (sequencer_cpu_data_master_read_data_valid_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1                  (sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1),
      .sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1                  (sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1),
      .sequencer_cpu_data_master_readdata                                          (sequencer_cpu_data_master_readdata),
      .sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave          (sequencer_cpu_data_master_requests_sequencer_bridge_0_avalon_slave),
      .sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module          (sequencer_cpu_data_master_requests_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_data_master_requests_sequencer_ram_s1                         (sequencer_cpu_data_master_requests_sequencer_ram_s1),
      .sequencer_cpu_data_master_requests_sequencer_rom_s1                         (sequencer_cpu_data_master_requests_sequencer_rom_s1),
      .sequencer_cpu_data_master_waitrequest                                       (sequencer_cpu_data_master_waitrequest),
      .sequencer_cpu_data_master_write                                             (sequencer_cpu_data_master_write),
      .sequencer_cpu_jtag_debug_module_readdata_from_sa                            (sequencer_cpu_jtag_debug_module_readdata_from_sa),
      .sequencer_ram_s1_readdata_from_sa                                           (sequencer_ram_s1_readdata_from_sa),
      .sequencer_rom_s1_readdata_from_sa                                           (sequencer_rom_s1_readdata_from_sa)
    );

  ddr2_v10_1_0002_sequencer_cpu_instruction_master_arbitrator the_sequencer_cpu_instruction_master
    (
      .clk                                                                                (clk_0),
      .d1_sequencer_cpu_jtag_debug_module_end_xfer                                        (d1_sequencer_cpu_jtag_debug_module_end_xfer),
      .d1_sequencer_rom_s1_end_xfer                                                       (d1_sequencer_rom_s1_end_xfer),
      .reset_n                                                                            (clk_0_reset_n),
      .sequencer_cpu_instruction_master_address                                           (sequencer_cpu_instruction_master_address),
      .sequencer_cpu_instruction_master_address_to_slave                                  (sequencer_cpu_instruction_master_address_to_slave),
      .sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module           (sequencer_cpu_instruction_master_granted_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_instruction_master_granted_sequencer_rom_s1                          (sequencer_cpu_instruction_master_granted_sequencer_rom_s1),
      .sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module (sequencer_cpu_instruction_master_qualified_request_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1                (sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1),
      .sequencer_cpu_instruction_master_read                                              (sequencer_cpu_instruction_master_read),
      .sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module   (sequencer_cpu_instruction_master_read_data_valid_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1                  (sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1),
      .sequencer_cpu_instruction_master_readdata                                          (sequencer_cpu_instruction_master_readdata),
      .sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module          (sequencer_cpu_instruction_master_requests_sequencer_cpu_jtag_debug_module),
      .sequencer_cpu_instruction_master_requests_sequencer_rom_s1                         (sequencer_cpu_instruction_master_requests_sequencer_rom_s1),
      .sequencer_cpu_instruction_master_waitrequest                                       (sequencer_cpu_instruction_master_waitrequest),
      .sequencer_cpu_jtag_debug_module_readdata_from_sa                                   (sequencer_cpu_jtag_debug_module_readdata_from_sa),
      .sequencer_rom_s1_readdata_from_sa                                                  (sequencer_rom_s1_readdata_from_sa)
    );

  ddr2_v10_1_0002_sequencer_cpu the_sequencer_cpu
    (
      .clk                                   (clk_0),
      .d_address                             (sequencer_cpu_data_master_address),
      .d_byteenable                          (sequencer_cpu_data_master_byteenable),
      .d_irq                                 (sequencer_cpu_data_master_irq),
      .d_read                                (sequencer_cpu_data_master_read),
      .d_readdata                            (sequencer_cpu_data_master_readdata),
      .d_waitrequest                         (sequencer_cpu_data_master_waitrequest),
      .d_write                               (sequencer_cpu_data_master_write),
      .d_writedata                           (sequencer_cpu_data_master_writedata),
      .i_address                             (sequencer_cpu_instruction_master_address),
      .i_read                                (sequencer_cpu_instruction_master_read),
      .i_readdata                            (sequencer_cpu_instruction_master_readdata),
      .i_waitrequest                         (sequencer_cpu_instruction_master_waitrequest),
      .jtag_debug_module_address             (sequencer_cpu_jtag_debug_module_address),
      .jtag_debug_module_begintransfer       (sequencer_cpu_jtag_debug_module_begintransfer),
      .jtag_debug_module_byteenable          (sequencer_cpu_jtag_debug_module_byteenable),
      .jtag_debug_module_debugaccess         (sequencer_cpu_jtag_debug_module_debugaccess),
      .jtag_debug_module_debugaccess_to_roms (sequencer_cpu_data_master_debugaccess),
      .jtag_debug_module_readdata            (sequencer_cpu_jtag_debug_module_readdata),
      .jtag_debug_module_resetrequest        (sequencer_cpu_jtag_debug_module_resetrequest),
      .jtag_debug_module_select              (sequencer_cpu_jtag_debug_module_chipselect),
      .jtag_debug_module_write               (sequencer_cpu_jtag_debug_module_write),
      .jtag_debug_module_writedata           (sequencer_cpu_jtag_debug_module_writedata),
      .reset_n                               (sequencer_cpu_jtag_debug_module_reset_n)
    );

  ddr2_v10_1_0002_sequencer_ram_s1_arbitrator the_sequencer_ram_s1
    (
      .clk                                                                   (clk_0),
      .d1_sequencer_ram_s1_end_xfer                                          (d1_sequencer_ram_s1_end_xfer),
      .registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1 (registered_sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1),
      .reset_n                                                               (clk_0_reset_n),
      .sequencer_cpu_data_master_address_to_slave                            (sequencer_cpu_data_master_address_to_slave),
      .sequencer_cpu_data_master_byteenable                                  (sequencer_cpu_data_master_byteenable),
      .sequencer_cpu_data_master_granted_sequencer_ram_s1                    (sequencer_cpu_data_master_granted_sequencer_ram_s1),
      .sequencer_cpu_data_master_qualified_request_sequencer_ram_s1          (sequencer_cpu_data_master_qualified_request_sequencer_ram_s1),
      .sequencer_cpu_data_master_read                                        (sequencer_cpu_data_master_read),
      .sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1            (sequencer_cpu_data_master_read_data_valid_sequencer_ram_s1),
      .sequencer_cpu_data_master_requests_sequencer_ram_s1                   (sequencer_cpu_data_master_requests_sequencer_ram_s1),
      .sequencer_cpu_data_master_waitrequest                                 (sequencer_cpu_data_master_waitrequest),
      .sequencer_cpu_data_master_write                                       (sequencer_cpu_data_master_write),
      .sequencer_cpu_data_master_writedata                                   (sequencer_cpu_data_master_writedata),
      .sequencer_ram_s1_address                                              (sequencer_ram_s1_address),
      .sequencer_ram_s1_byteenable                                           (sequencer_ram_s1_byteenable),
      .sequencer_ram_s1_chipselect                                           (sequencer_ram_s1_chipselect),
      .sequencer_ram_s1_clken                                                (sequencer_ram_s1_clken),
      .sequencer_ram_s1_readdata                                             (sequencer_ram_s1_readdata),
      .sequencer_ram_s1_readdata_from_sa                                     (sequencer_ram_s1_readdata_from_sa),
      .sequencer_ram_s1_write                                                (sequencer_ram_s1_write),
      .sequencer_ram_s1_writedata                                            (sequencer_ram_s1_writedata)
    );

  ddr2_v10_1_0002_sequencer_ram the_sequencer_ram
    (
      .address    (sequencer_ram_s1_address),
      .byteenable (sequencer_ram_s1_byteenable),
      .chipselect (sequencer_ram_s1_chipselect),
      .clk        (clk_0),
      .clken      (sequencer_ram_s1_clken),
      .readdata   (sequencer_ram_s1_readdata),
      .write      (sequencer_ram_s1_write),
      .writedata  (sequencer_ram_s1_writedata)
    );

  ddr2_v10_1_0002_sequencer_rom_s1_arbitrator the_sequencer_rom_s1
    (
      .clk                                                                   (clk_0),
      .d1_sequencer_rom_s1_end_xfer                                          (d1_sequencer_rom_s1_end_xfer),
      .registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1 (registered_sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1),
      .reset_n                                                               (clk_0_reset_n),
      .sequencer_cpu_data_master_address_to_slave                            (sequencer_cpu_data_master_address_to_slave),
      .sequencer_cpu_data_master_byteenable                                  (sequencer_cpu_data_master_byteenable),
      .sequencer_cpu_data_master_granted_sequencer_rom_s1                    (sequencer_cpu_data_master_granted_sequencer_rom_s1),
      .sequencer_cpu_data_master_qualified_request_sequencer_rom_s1          (sequencer_cpu_data_master_qualified_request_sequencer_rom_s1),
      .sequencer_cpu_data_master_read                                        (sequencer_cpu_data_master_read),
      .sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1            (sequencer_cpu_data_master_read_data_valid_sequencer_rom_s1),
      .sequencer_cpu_data_master_requests_sequencer_rom_s1                   (sequencer_cpu_data_master_requests_sequencer_rom_s1),
      .sequencer_cpu_data_master_waitrequest                                 (sequencer_cpu_data_master_waitrequest),
      .sequencer_cpu_data_master_write                                       (sequencer_cpu_data_master_write),
      .sequencer_cpu_data_master_writedata                                   (sequencer_cpu_data_master_writedata),
      .sequencer_cpu_instruction_master_address_to_slave                     (sequencer_cpu_instruction_master_address_to_slave),
      .sequencer_cpu_instruction_master_granted_sequencer_rom_s1             (sequencer_cpu_instruction_master_granted_sequencer_rom_s1),
      .sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1   (sequencer_cpu_instruction_master_qualified_request_sequencer_rom_s1),
      .sequencer_cpu_instruction_master_read                                 (sequencer_cpu_instruction_master_read),
      .sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1     (sequencer_cpu_instruction_master_read_data_valid_sequencer_rom_s1),
      .sequencer_cpu_instruction_master_requests_sequencer_rom_s1            (sequencer_cpu_instruction_master_requests_sequencer_rom_s1),
      .sequencer_rom_s1_address                                              (sequencer_rom_s1_address),
      .sequencer_rom_s1_byteenable                                           (sequencer_rom_s1_byteenable),
      .sequencer_rom_s1_chipselect                                           (sequencer_rom_s1_chipselect),
      .sequencer_rom_s1_clken                                                (sequencer_rom_s1_clken),
      .sequencer_rom_s1_readdata                                             (sequencer_rom_s1_readdata),
      .sequencer_rom_s1_readdata_from_sa                                     (sequencer_rom_s1_readdata_from_sa),
      .sequencer_rom_s1_write                                                (sequencer_rom_s1_write),
      .sequencer_rom_s1_writedata                                            (sequencer_rom_s1_writedata)
    );

  ddr2_v10_1_0002_sequencer_rom_s2_arbitrator the_sequencer_rom_s2
    (
      .clk                                             (rom_write_clock),
      .d1_sequencer_rom_s2_end_xfer                    (d1_sequencer_rom_s2_end_xfer),
      .reset_n                                         (rom_write_clock_reset_n),
      .rom_bridge_0_avalon_master_address_to_slave     (rom_bridge_0_avalon_master_address_to_slave),
      .rom_bridge_0_avalon_master_write                (rom_bridge_0_avalon_master_write),
      .rom_bridge_0_avalon_master_writedata            (rom_bridge_0_avalon_master_writedata),
      .rom_bridge_0_granted_sequencer_rom_s2           (rom_bridge_0_granted_sequencer_rom_s2),
      .rom_bridge_0_qualified_request_sequencer_rom_s2 (rom_bridge_0_qualified_request_sequencer_rom_s2),
      .rom_bridge_0_requests_sequencer_rom_s2          (rom_bridge_0_requests_sequencer_rom_s2),
      .sequencer_rom_s2_address                        (sequencer_rom_s2_address),
      .sequencer_rom_s2_byteenable                     (sequencer_rom_s2_byteenable),
      .sequencer_rom_s2_chipselect                     (sequencer_rom_s2_chipselect),
      .sequencer_rom_s2_clken                          (sequencer_rom_s2_clken),
      .sequencer_rom_s2_readdata                       (sequencer_rom_s2_readdata),
      .sequencer_rom_s2_readdata_from_sa               (sequencer_rom_s2_readdata_from_sa),
      .sequencer_rom_s2_write                          (sequencer_rom_s2_write),
      .sequencer_rom_s2_writedata                      (sequencer_rom_s2_writedata)
    );

  ddr2_v10_1_0002_sequencer_rom the_sequencer_rom
    (
      .address     (sequencer_rom_s1_address),
      .address2    (sequencer_rom_s2_address),
      .byteenable  (sequencer_rom_s1_byteenable),
      .byteenable2 (sequencer_rom_s2_byteenable),
      .chipselect  (sequencer_rom_s1_chipselect),
      .chipselect2 (sequencer_rom_s2_chipselect),
      .clk         (clk_0),
      .clk2        (rom_write_clock),
      .clken       (sequencer_rom_s1_clken),
      .clken2      (sequencer_rom_s2_clken),
      .readdata    (sequencer_rom_s1_readdata),
      .readdata2   (sequencer_rom_s2_readdata),
      .write       (sequencer_rom_s1_write),
      .write2      (sequencer_rom_s2_write),
      .writedata   (sequencer_rom_s1_writedata),
      .writedata2  (sequencer_rom_s2_writedata)
    );

  //reset is asserted asynchronously and deasserted synchronously
  ddr2_v10_1_0002_sequencer_ctrl_reset_clk_0_domain_synch_module sequencer_ctrl_reset_clk_0_domain_synch
    (
      .clk      (clk_0),
      .data_in  (1'b1),
      .data_out (clk_0_reset_n),
      .reset_n  (reset_n_sources)
    );

  //reset sources mux, which is an e_mux
  assign reset_n_sources = ~(~reset_n |
    0 |
    sequencer_cpu_jtag_debug_module_resetrequest_from_sa |
    sequencer_cpu_jtag_debug_module_resetrequest_from_sa |
    0);

  //reset is asserted asynchronously and deasserted synchronously
  ddr2_v10_1_0002_sequencer_ctrl_reset_rom_write_clock_domain_synch_module sequencer_ctrl_reset_rom_write_clock_domain_synch
    (
      .clk      (rom_write_clock),
      .data_in  (1'b1),
      .data_out (rom_write_clock_reset_n),
      .reset_n  (reset_n_sources)
    );

  //sequencer_cpu_data_master_irq of type irq does not connect to anything so wire it to default (0)
  assign sequencer_cpu_data_master_irq = 0;


endmodule


