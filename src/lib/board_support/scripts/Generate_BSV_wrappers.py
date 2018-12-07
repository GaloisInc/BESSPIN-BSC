#!/usr/bin/python

# Copyright (c) 2105 Bluespec, Inc.  All Rights Reserved
# Author: Rishiyur S. Nikhil

# ================================================================
# This program reads one input file and writes two output files.

# Usage:    Generate_BSV_wrappers.py  pin_file  dir_for_output_files

# The input file usually has a '.pin' extension, e.g., DUT.pin
# It is a text file containing a description of the pins (input
# and output ports) of a Verilog module 'DUT'. Specifically:
#  - the clock port    (at the moment, only 1 clock)
#  - the reset port    (at the moment, only 1 reset)
#  - each 'raw' INPUT port (no handshaking)
#  - each 'raw' OUTPUT port (no handshaking)
#  - each set of ports representing a GET interface, which is
#      - a collection of output data ports
#      - an output 'ready' signal indicating data is available
#      - an input 'enable' signal indicating data has been captured
#  - each set of ports representing a PUT interface, which is
#      - a collection of input data ports
#      - an output 'ready' signal indicating ready to accept data
#      - an input 'enable' signal indicating data is valid
#  - up to one set of ports representing a MEMORY interface
#      This is structured into two sub-interfaces
#      - The REQUEST sub-interface is essentially a GET interface
#          with an output data port carrying address, data, byte-enables
#          and 1-bit indicating whether it's a read or write,
#          plus the handshake 'ready' and 'enable' signals
#      - The RESPONSE sub-interface is essentially a PUT interface
#          for returning data for memory read-requests,
#          with an input data port carrying the from-memory data
#          plus the handshake 'ready' and 'enable' signals

# Alternatively, the input file can specify a DUT with a lockstep
# interface.  In that case, there is again required to be exactly
# one clock and exactly one reset and all other ports are declared
# as 'lockstep'.  The generated modules will have different
# implementations (than those for port/handshake interfaces), to
# ensure lockstep communication.

# From the pin description, this program generates two output files
# which are BSV source code wrappers for the user's DUT, to be used in
# Bluespec's Emulation setup. In such a setup, the top of the module
# hierarchy that is synthesized to the FPGA consists of:
#    module mkBridge                 // BSV, from Bluespec library
#      module mkSceMiLayer           // BSV, generated here
#        module mkBsvDUT             // BSV, generated here
#          module mkBsvDUT_raw       // BSV, generated here
#            module DUT              // User's Verilog

# This program generates two BSV source files:
#   SceMiLayer.bsv     // contains module mkSceMiLayer
#   BsvDUT.bsv         // contains modules mkBsvDUT and mkBsvDUT_raw

# SceMiLayer.bsv contains all the SceMi transactors (incl. readback transctor)
# mkBsvDUT        converts raw imported ports to Put/Get/Mem/... interfaces
# mkBsvDUT_raw    uses BSV's 'import' mechanism to import user's Verilog

# ================================================================
# Standard Python lib imports

import sys
import os

# ================================================================
# Project imports

from ParsePin import *    # The parser for .pin files

# ================================================================
# Generate BsvDUT file

def genBsvDUTFile (f_out, dut_ifc, dut_name, BsvDUT):

  f_out.write ("// Copyright (c) 2013-2015 Bluespec Inc. All Rights Reserved.\n")
  f_out.write ("// This file is program generated; please do not edit!\n")
  f_out.write ("\n")
  f_out.write ("package %s;\n" % BsvDUT)
  f_out.write ("\n")
  f_out.write ("import GetPut::*;\n")
  if dut_ifc.expects_memory ():
    f_out.write ("import ClientServer::*;\n")
    f_out.write ("import Memory::*;\n")
  f_out.write ("\n")
  f_out.write ("// ----------------------------------------------------------------\n")
  f_out.write ("// Wrapper layer 1 interface\n")
  f_out.write ("\n")

  # Generate interface of layer 1 module (raw import of Verilog)
  f_out.write ("interface %s_raw_IFC;\n" % BsvDUT)

  # generate methods for each interface
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    f_out.write ("\n")
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("   // ---- Raw/PIPE input interface: %s\n" % xactor_name)
      f_out.write ("   (* always_ready, always_enabled *)\n")
      f_out.write ("   method Action raw_in_%s (Bit #(%d) x);\n" % (xactor_name, xactor.field_width))

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("   // ---- Raw/PIPE output interface: %s\n" % xactor_name)
      f_out.write ("   (* always_ready *)\n")
      f_out.write ("   method Bit #(%d) raw_out_%s ();\n" % (xactor.field_width, xactor_name))

    elif isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("   // ---- GET/PIPEGET interface: %s\n" % xactor_name)
      f_out.write ("   method Action raw_get_%s ();    // Get\n" % xactor_name)
      for j in range (len (xactor.verilog_names)):
        f_out.write ("   method Bit #(%d) raw_get_%s_field_%s ();\n"
                     % (xactor.field_widths [j], xactor_name, xactor.verilog_names [j]))

    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("   // ---- PUT/PIPEPUT interface: %s\n" % xactor_name)
      f_out.write ("   method Action raw_put_%s (" % xactor_name)
      for j in range (len (xactor.verilog_names)):
        if (j != 0): f_out.write (", ")
        f_out.write ("Bit #(%d) field_%s" % (xactor.field_widths [j], xactor.verilog_names [j]))
      f_out.write (");\n")

    elif isinstance (xactor, Xactor_MEM_IFC):
      f_out.write ("   // ---- Memory interface: %s\n" % xactor_name)
      f_out.write ("   method ActionValue #(Bit #(%d)) raw_mem_%s_request ();    // Mem Req Get\n"
                   % (xactor.address_width + xactor.data_width + xactor.data_width/8 + 1,
                      xactor_name))
      f_out.write ("   method Action raw_mem_%s_response (Bit #(%d) data);    // Mem Resp Put\n"
                   % (xactor_name, xactor.data_width))

    else:
      print "Error: Unexpected xactor:\n", xactor.show ("  ")
      return 1

  f_out.write ("endinterface: %s_raw_IFC\n" % BsvDUT)

  f_out.write ("\n")
  f_out.write ("// ----------------------------------------------------------------\n")
  f_out.write ("// Wrapper layer 1 module\n")
  f_out.write ("\n")
  f_out.write ('import "BVI" %s =\n' % dut_name)
  f_out.write ("module [Module] mk%s_raw (%s_raw_IFC ifc);\n" % (BsvDUT, BsvDUT))
  f_out.write ("\n")
  f_out.write ("   default_clock (%s, (*unused*) GATE);\n" % (dut_ifc.clock.verilog_name))
  if dut_ifc.reset == None:
    f_out.write ("   no_reset;\n")
  else:
    f_out.write ("   default_reset (%s);\n" % (dut_ifc.reset.verilog_name))

  # BSV requires declaring a name for the non-existent enable port, and it must be unique
  inhigh_uid = 0;

  # Generate methods for each verilog port
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    f_out.write ("\n")

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("   // ---- Raw_In/PIPE_IN interface %s\n" % xactor_name)
      f_out.write ("   method raw_in_%s (%s) enable((*inhigh*) EN_inhigh%d);    // raw/PIPE input\n"
                   % (xactor_name, xactor_name, inhigh_uid))
      inhigh_uid += 1

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("   // ---- Raw_Out/PIPE_OUT interface %s\n" % xactor_name)
      f_out.write ("   method %s raw_out_%s ();    // raw/PIPE output\n"
                   % (xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("   // ---- GET/PIPEGET interface %s\n" % xactor_name)
      f_out.write ("   method raw_get_%s () enable (%s) ready (%s);\n"
                   % (xactor_name, xactor.en, xactor.rdy))
      for j in range (len (xactor.verilog_names)):
        f_out.write ("   method %s raw_get_%s_field_%s ();    // Get\n"
                     % (xactor.verilog_names [j], xactor_name, xactor.verilog_names [j]))

    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("   // ---- PUT/PIPEPUT interface %s\n" % xactor_name)
      f_out.write ("   method raw_put_%s (" % xactor_name)
      for j in range (len (xactor.verilog_names)):
        if (j != 0): f_out.write (", ")
        f_out.write ("%s" % xactor.verilog_names [j])
      f_out.write (") enable (%s) ready (%s);\n"
                   % (xactor.en, xactor.rdy))

    elif isinstance (xactor, Xactor_MEM_IFC):
      f_out.write ("   // ---- Mem request Get interface %s\n" % xactor_name)
      f_out.write ("   method %s raw_mem_%s_request () enable (%s) ready (%s);\n"
                   % (xactor.req_addr_data_verilog_name, xactor_name, xactor.req_en, xactor.req_rdy))
      f_out.write ("   // ---- Mem response Put interface %s\n" % xactor_name)
      f_out.write ("   method raw_mem_%s_response (%s) enable (%s) ready (%s);\n"
                   % (xactor_name, xactor.resp_data_verilog_name, xactor.resp_en, xactor.resp_rdy))

  # Generate schedule
  # Each raw output method is CF with itself; each other method is C with itself
  # Each method is CF with each other method
  # Create a list xys of all (method_name, is_raw_output)
  xys = []
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      xys.append (("raw_in_" + xactor_name, False))

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      xys.append (("raw_out_" + xactor_name, True))

    elif isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      xys.append (("raw_get_" + xactor_name, False))
      for j in range (len (xactor.verilog_names)):
        xys.append (("raw_get_%s_field_%s" % (xactor_name, xactor.verilog_names [j]), True))

    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      xys.append (("raw_put_" + xactor_name, False))

    elif isinstance (xactor, Xactor_MEM_IFC):
      xys.append (("raw_mem_%s_request" % xactor_name, False))
      xys.append (("raw_mem_%s_response" % xactor_name, False))

  # Write out the schedule
  f_out.write ("\n")
  for i in range (len (xys)):
    (method_name, is_raw_out) = xys [i]
    # Schedule with itself
    if is_raw_out:
      f_out.write ("   schedule (%s) CF (%s);\n" % (method_name, method_name))
    else:
      f_out.write ("   schedule (%s) C (%s);\n" % (method_name, method_name))
    # Schedule with the rest, if any
    if (i < (len (xys) - 1)):
      f_out.write ("   schedule (%s) CF (" % method_name)
      for j in range (i+1, len (xys)):
        (method_name_J, is_raw_out_J) = xys [j]
        if (i+1) < j: f_out.write (", ")
        f_out.write ("%s" % method_name_J)
      f_out.write (");\n")

  f_out.write ("\n")
  f_out.write ("endmodule: mk%s_raw\n" % BsvDUT)

  # ----------------------------------------------------------------
  # Generate the layer 2 interface and module

  f_out.write ("\n")
  f_out.write ("// ----------------------------------------------------------------\n")
  f_out.write ("// Wrapper layer 2 interface\n")

  # Generate a struct for each Get/Put interface
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if (isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC)
        or isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC)):
      f_out.write ("\n")
      f_out.write ("typedef struct {\n")
      for j in range (len (xactor.verilog_names)):
        verilog_name = xactor.verilog_names [j]
        field_width = xactor.field_widths [j]
        f_out.write ("   Bit #(%d) field_%s;\n" % (field_width, verilog_name))
      f_out.write ("} %s_%s\n" % (BsvDUT, xactor_name))
      f_out.write ("deriving (Bits);\n")

  # For raw ports, we still need to create a type alias, so that the type name will
  # be predictable to later stages in the flow
  first = 1
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if (isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC)
        or isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC)):
      if (first == 1):
        f_out.write("\n")
        first = 0
      f_out.write ("typedef Bit #(%d) %s_%s;\n" % (xactor.field_width, BsvDUT, xactor_name))

  # Generate the layer 2 interface
  f_out.write ("\n")
  f_out.write ("interface %s_IFC;\n" % BsvDUT)
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("   interface Put #(%s_%s) in_%s;\n" % (BsvDUT, xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("   interface Get #(%s_%s) out_%s;\n" % (BsvDUT, xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("   interface Get #(%s_%s) get_%s;\n" % (BsvDUT, xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("   interface Put #(%s_%s) put_%s;\n" % (BsvDUT, xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_MEM_IFC):
      f_out.write ("   interface MemoryClient #(%d, %d) mem_%s;\n"
                   % (xactor.address_width, xactor.data_width, xactor_name))

  f_out.write ("endinterface: %s_IFC\n" % BsvDUT)

  f_out.write ("\n")
  f_out.write ("// ----------------------------------------------------------------\n")
  f_out.write ("// Wrapper layer 2 module\n")
  f_out.write ("\n")
  f_out.write ("(* synthesize *)\n")
  f_out.write ("module [Module] mk%s (%s_IFC ifc);\n" % (BsvDUT, BsvDUT))
  f_out.write ("\n")
  f_out.write ("   %s_raw_IFC ifc_raw <- mk%s_raw;\n" % (BsvDUT, BsvDUT))

  # For each raw in/out, create a register, and a rule to drive/capture
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("\n")
      f_out.write ("   Reg #(Bit #(%d)) rg_%s <- mkRegU;\n" % (xactor.field_width, xactor_name))
      f_out.write ("\n")
      f_out.write ("   (* fire_when_enabled, no_implicit_conditions *)\n")
      f_out.write ("   rule rl_drive_%s;\n" % xactor_name)
      f_out.write ("      ifc_raw.raw_in_%s (rg_%s);\n" % (xactor_name, xactor_name))
      f_out.write ("   endrule\n")

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("\n")
      f_out.write ("   Reg #(Bit #(%d)) rg_%s <- mkRegU;\n" % (xactor.field_width, xactor_name))
      f_out.write ("\n")
      f_out.write ("   (* fire_when_enabled, no_implicit_conditions *)\n")
      f_out.write ("   rule rl_capture_%s;\n" % xactor_name)
      f_out.write ("      rg_%s <= ifc_raw.raw_out_%s;\n" % (xactor_name, xactor_name))
      f_out.write ("   endrule\n")

  # Define each interface
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("\n")
      f_out.write ("   // ---- Raw/PIPE in interface %s\n" % xactor_name)
      f_out.write ("   interface Put in_%s;\n" % xactor_name)
      f_out.write ("      method put (val_%s);\n" % xactor_name)
      f_out.write ("         action\n")
      f_out.write ("            rg_%s <= val_%s;\n" % (xactor_name, xactor_name))
      f_out.write ("         endaction\n")
      f_out.write ("      endmethod\n")
      f_out.write ("   endinterface\n")

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("\n")
      f_out.write ("   // ---- Raw/PIPE out interface %s\n" % xactor_name)
      f_out.write ("   interface Get out_%s;\n" % xactor_name)
      f_out.write ("      method get ();\n")
      f_out.write ("         actionvalue\n")
      f_out.write ("            return rg_%s;\n" % xactor_name)
      f_out.write ("         endactionvalue\n")
      f_out.write ("      endmethod\n")
      f_out.write ("   endinterface\n")

    elif isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("\n")
      f_out.write ("   // ---- GET/PIPEGET interface %s\n" % xactor_name)
      f_out.write ("   interface Get get_%s;\n" % xactor_name)
      f_out.write ("      method get ();\n")
      f_out.write ("         actionvalue\n")
      f_out.write ("            ifc_raw.raw_get_%s ();    // For RDY/EN synchronization\n" % xactor_name)
      f_out.write ("            return %s_%s {\n" % (BsvDUT, xactor_name))
      for verilog_name in xactor.verilog_names:
        f_out.write ("                field_%s: ifc_raw.raw_get_%s_field_%s ()\n"
                     % (verilog_name, xactor_name, verilog_name))
      f_out.write ("            };\n")
      f_out.write ("         endactionvalue\n")
      f_out.write ("      endmethod\n")
      f_out.write ("   endinterface\n")

    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("\n")
      f_out.write ("   // ---- PUT/PIPEPUT interface %s\n" % xactor_name)
      f_out.write ("   interface Put put_%s;\n" % xactor_name)
      f_out.write ("      method put (x);\n")
      f_out.write ("         action\n")
      f_out.write ("            ifc_raw.raw_put_%s (" % xactor_name)
      for j in range (len (xactor.verilog_names)):
        if (j != 0): f_out.write (", ")
        f_out.write ("x.field_%s" % xactor.verilog_names [j])
      f_out.write (");\n")
      f_out.write ("         endaction\n")
      f_out.write ("      endmethod\n")
      f_out.write ("   endinterface\n")

    elif isinstance (xactor, Xactor_MEM_IFC):
      f_out.write ("\n")
      f_out.write ("   // ---- Memory interface mem\n")
      f_out.write ("   interface MemoryClient mem_%s;\n" % xactor_name)
      f_out.write ("      interface Get request;\n")
      f_out.write ("         method get ();\n")
      f_out.write ("            actionvalue\n")
      f_out.write ("               let req <- ifc_raw.raw_mem_%s_request;\n" % xactor_name)
      f_out.write ("               return unpack (req);\n")
      f_out.write ("            endactionvalue\n")
      f_out.write ("         endmethod\n")
      f_out.write ("      endinterface\n")
      f_out.write ("      interface Put response;\n")
      f_out.write ("         method put (rsp);\n")
      f_out.write ("            action\n")
      f_out.write ("               ifc_raw.raw_mem_%s_response (pack (rsp));\n" % xactor_name)
      f_out.write ("            endaction\n")
      f_out.write ("         endmethod\n")
      f_out.write ("      endinterface\n")
      f_out.write ("   endinterface\n")

  f_out.write ("\n")
  f_out.write ("endmodule: mk%s\n" % BsvDUT)
  f_out.write ("\n")
  f_out.write ("endpackage: %s\n" % BsvDUT)

# ================================================================
# Generate SceMiLayer file

def genSceMiLayerFile (f_out, dut_ifc, BsvDUT):

  f_out.write ("// Copyright (c) 2013-2015 Bluespec Inc. All Rights Reserved.\n")
  f_out.write ("// This file is program generated; please do not edit!\n")
  f_out.write ("\n")
  f_out.write ("package SceMiLayer;\n")

  f_out.write ("\n")
  f_out.write ("import Clocks::*;\n")
  f_out.write ("import Connectable::*;\n")
  f_out.write ("import DefaultValue::*;\n")
  f_out.write ("import GetPut::*;\n")
  f_out.write ("import Readback::*;\n")
  f_out.write ("import SceMi::*;\n")
  if dut_ifc.expects_memory ():
    f_out.write ("\n")
    f_out.write ("import ClientServer::*;\n")
    f_out.write ("import Memory::*;\n")
    f_out.write ("import RegFile::*;\n")

  f_out.write ("\n")
  f_out.write ("import %s::*;\n" % BsvDUT)

  f_out.write ("\n")
  f_out.write ("`include \"Readback.defines\"\n")

  f_out.write ("\n")
  if dut_ifc.expects_memory ():
    f_out.write ("typedef MemoryClient#(32, 256) SceMiLayer;\n")
    f_out.write ("\n")
    f_out.write ("`ifdef SCEMI_TCP\n")
    f_out.write ("module [SceMiModule] mkSceMiLayer();\n")
    f_out.write ("`else\n")
    f_out.write ("module [SceMiModule] mkSceMiLayer(SceMiLayer);\n")
    f_out.write ("`endif\n")
  else:
    f_out.write ("module [SceMiModule] mkSceMiLayer();\n")

  f_out.write ("\n")
  f_out.write ("   SceMiClockConfiguration conf = defaultValue;\n")
  f_out.write ("   SceMiClockPortIfc clk_port <- mkSceMiClockPort(conf);\n")

  f_out.write ("\n")
  f_out.write ("   %s_IFC dut <- buildDut(mk%s, clk_port);\n" % (BsvDUT, BsvDUT))

  # Instantiate transactors for each data input and output interface (not clk, reset, memory)
  f_out.write ("\n")
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC):
      f_out.write ("   let put_%s <- mkSemuInPortXactor(dut.in_%s, LooselyCoupled, conf);\n"
                   % (xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("   let put_%s <- mkSemuInPortPipeXactor(dut.in_%s, LooselyCoupled, 4096, Fifo, conf);\n"
                   % (xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_Raw_Out_IFC):
      f_out.write ("   let put_%s <- mkSemuOutPortXactor(dut.out_%s, LooselyCoupled, conf);\n"
                   % (xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("   let get_%s <- mkSemuOutPortPipeXactor(dut.out_%s, LooselyCoupled, 4096, Fifo, conf);\n"
                   % (xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_PUT_IFC):
      f_out.write ("   Get#(%s_%s) put_%s <- mkSemuRdyEnableInPortXactor(clk_port);\n"
                   % (BsvDUT, xactor_name, xactor_name))
      f_out.write ("   mkConnection(put_%s, dut.put_%s);\n"
                   % (xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("   Get #(%s_%s) put_%s <- mkInPipeXactor(4096, Fifo, clk_port);\n"
                   % (BsvDUT, xactor_name, xactor_name))
      f_out.write ("   mkConnection(put_%s, dut.put_%s);\n"
                   % (xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_GET_IFC):
      f_out.write ("   Put #(%s_%s) get_%s <- mkSemuRdyEnableOutPortXactor(clk_port);\n"
                   % (BsvDUT, xactor_name, xactor_name))
      f_out.write ("   mkConnection(dut.get_%s, get_%s);\n"
                   % (xactor_name, xactor_name))

    elif isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("   Put #(%s_%s) get_%s <- mkOutPipeXactor(4096, Fifo, clk_port);\n"
                   % (BsvDUT, xactor_name, xactor_name))
      f_out.write ("   mkConnection(dut.get_%s, get_%s);\n"
                   % (xactor_name, xactor_name))

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_MEM_IFC):
      f_out.write ("\n")
      f_out.write ("   Clock uclock <- sceMiGetUClock;\n")
      f_out.write ("   Reset ureset <- sceMiGetUReset;\n")
      f_out.write ("   let cclock = clk_port.cclock;\n")
      f_out.write ("   let creset = clk_port.creset;\n")
      f_out.write ("\n")
      f_out.write ("`ifdef SCEMI_TCP\n")
      f_out.write ("   // RegFile for memory model simulation\n")
      f_out.write ("   RegFile#(Bit#(20), Bit#(%d)) mem <- mkRegFileFull(clocked_by cclock, reset_by creset);\n"
                   % xactor.data_width)
      f_out.write ("\n")
      f_out.write ("   mkConnection(dut.mem_%s, mem, clocked_by cclock, reset_by creset);\n"
                   % xactor_name)
      f_out.write ("`else\n")
      f_out.write ("   SyncFIFOIfc#(MemoryRequest#(%d,%d)) dut_mem_req <- mkSyncFIFO(1, cclock, creset, uclock);\n"
                   % (xactor.address_width, xactor.data_width))
      f_out.write ("   SyncFIFOIfc#(MemoryResponse#(%d)) dut_mem_resp <- mkSyncFIFO(1, uclock, ureset, cclock);\n"
                   % xactor.data_width)
      f_out.write ("\n")
      f_out.write ("   mkConnection(dut.mem_%s.request, toPut(dut_mem_req));\n"
                   % xactor_name)
      f_out.write ("   mkConnection(toGet(dut_mem_resp), dut.mem_%s.response);\n"
                   % xactor_name)
      f_out.write ("`endif\n")

  f_out.write ("\n")
  f_out.write ("   SceMiResetXactorIfc reset <- mkSceMiResetXactor(clk_port);\n")
  f_out.write ("   Empty     control <- mkShutdownXactor();\n")

  f_out.write ("\n")
  f_out.write ("   `READBACKCORE(simControl, conf);\n")

  f_out.write ("\n")
  f_out.write ("   Empty  tbsimControl <- mkSimulationControl(conf);\n")

  if dut_ifc.expects_memory ():
    f_out.write ("\n")
    f_out.write ("`ifndef SCEMI_TCP\n")
    f_out.write ("   interface request = toGet(dut_mem_req);\n")
    f_out.write ("   interface response = toPut(dut_mem_resp);\n")
    f_out.write ("`endif\n")

  f_out.write ("\n")
  f_out.write ("endmodule\n")
  f_out.write ("\n")
  f_out.write ("endpackage\n")

# ================================================================
# Generate BsvDUT file when the ports are lockstep

def genLockstepBsvDUTFile (f_out, dut_ifc, dut_name, BsvDUT):
  f_out.write ("// Copyright (c) 2013-2015 Bluespec Inc. All Rights Reserved.\n")
  f_out.write ("// This file is program generated; please do not edit!\n")
  f_out.write ("\n")
  f_out.write ("package %s;\n" % BsvDUT)
  f_out.write ("\n")
  f_out.write ("// ----------------------------------------------------------------\n")
  f_out.write ("// Imported interface\n")

  # For raw ports, we still need to create a type alias, so that the type name will
  # be predictable to later stages in the flow
  first = 1
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Lockstep_In_IFC) or isinstance (xactor, Xactor_Lockstep_Out_IFC):
      if (first == 1):
        f_out.write("\n")
        first = 0
      f_out.write ("typedef Bit #(%d) %s_%s;\n" % (xactor.field_width, BsvDUT, xactor_name))
    else:
      print "Error: Unexpected xactor:\n", xactor.show ("  ")
      return 1

  # Generate interface
  f_out.write ("\n")
  f_out.write ("interface %s_IFC;\n" % BsvDUT)

  if dut_ifc.reset != None:
    f_out.write ("\n")
    f_out.write ("   // ---- Lockstep reset: rst_n\n")
    f_out.write ("   (* always_ready, always_enabled *)\n")
    f_out.write ("   method Action rst_n (Bit#(1) x);\n")

  # generate methods for each port
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    f_out.write ("\n")
    if isinstance (xactor, Xactor_Lockstep_In_IFC):
      f_out.write ("   // ---- Lockstep input interface: %s\n" % xactor_name)
      f_out.write ("   (* always_ready, always_enabled *)\n")
      f_out.write ("   method Action in_%s (%s_%s x);\n" % (xactor_name, BsvDUT, xactor_name))

    elif isinstance (xactor, Xactor_Lockstep_Out_IFC):
      f_out.write ("   // ---- Lockstep output interface: %s\n" % xactor_name)
      f_out.write ("   (* always_ready *)\n")
      f_out.write ("   method %s_%s out_%s ();\n" % (BsvDUT, xactor_name, xactor_name))

  f_out.write ("\n")
  f_out.write ("endinterface: %s_IFC\n" % BsvDUT)

  f_out.write ("\n")
  f_out.write ("// ----------------------------------------------------------------\n")
  f_out.write ("// Imported module\n")
  f_out.write ("\n")
  f_out.write ('import "BVI" %s =\n' % dut_name)
  f_out.write ("module [Module] mk%s_raw (%s_IFC ifc);\n" % (BsvDUT, BsvDUT))
  f_out.write ("\n")
  f_out.write ("   default_clock (%s, (*unused*) GATE);\n" % (dut_ifc.clock.verilog_name))
  f_out.write ("   no_reset;\n")

  # BSV requires declaring a name for the non-existent enable port, and it must be unique
  inhigh_uid = 0;

  if dut_ifc.reset != None:
    f_out.write ("\n")
    f_out.write ("   // ---- Lockstep reset: rst_n\n")
    f_out.write ("   method rst_n (%s) enable((*inhigh*) EN_inhigh%d);\n"
                 % (dut_ifc.reset.verilog_name, inhigh_uid))
    inhigh_uid += 1

  # Generate methods for each verilog port
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    f_out.write ("\n")

    if isinstance (xactor, Xactor_Lockstep_In_IFC):
      f_out.write ("   // ---- Lockstep input interface: %s\n" % xactor_name)
      f_out.write ("   method in_%s (%s) enable((*inhigh*) EN_inhigh%d);\n"
                   % (xactor_name, xactor_name, inhigh_uid))
      inhigh_uid += 1

    elif isinstance (xactor, Xactor_Lockstep_Out_IFC):
      f_out.write ("   // ---- Lockstep output interface: %s\n" % xactor_name)
      f_out.write ("   method %s out_%s ();\n" % (xactor_name, xactor_name))

  # Generate schedule
  # Each output method is CF with itself; each input method is C with itself
  # Each method is CF with each other method
  # Create a list xys of all (method_name, is_output)
  xys = [('rst_n', False)]
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Lockstep_In_IFC):
      xys.append (("in_" + xactor_name, False))

    elif isinstance (xactor, Xactor_Lockstep_Out_IFC):
      xys.append (("out_" + xactor_name, True))

  # Write out the schedule
  f_out.write ("\n")
  for i in range (len (xys)):
    (method_name, is_raw_out) = xys [i]
    # Schedule with itself
    if is_raw_out:
      f_out.write ("   schedule (%s) CF (%s);\n" % (method_name, method_name))
    else:
      f_out.write ("   schedule (%s) C (%s);\n" % (method_name, method_name))
    # Schedule with the rest, if any
    if (i < (len (xys) - 1)):
      f_out.write ("   schedule (%s) CF (" % method_name)
      for j in range (i+1, len (xys)):
        (method_name_J, is_raw_out_J) = xys [j]
        if (i+1) < j: f_out.write (", ")
        f_out.write ("%s" % method_name_J)
      f_out.write (");\n")

  f_out.write ("\n")
  f_out.write ("endmodule: mk%s_raw\n" % BsvDUT)

  f_out.write ("\n")
  f_out.write ("// ----------------------------------------------------------------\n")
  f_out.write ("// Wrapper layer module\n")
  f_out.write ("\n")
  f_out.write ("// In order to make the hierarchical location to the DUT the same\n")
  f_out.write ("// for lockstep and non-lockstep SceMiLayers, we add this wrapper.\n")
  f_out.write ("\n")

  f_out.write ("(* synthesize *)\n")
  f_out.write ("module [Module] mk%s (%s_IFC ifc);\n" % (BsvDUT, BsvDUT))
  f_out.write ("\n")
  f_out.write ("   %s_IFC ifc_raw <- mk%s_raw;\n" % (BsvDUT, BsvDUT))
  f_out.write ("\n")
  f_out.write ("   return ifc_raw;\n")
  f_out.write ("\n")
  f_out.write ("endmodule: mk%s\n" % BsvDUT)

  f_out.write ("\n")
  f_out.write ("endpackage: %s\n" % BsvDUT)

# ================================================================
# Generate SceMiLayer file when the ports are lockstep

def genLockstepSceMiLayerFile (f_out, dut_ifc, BsvDUT):

  # Individual ports are communicated through a single proxy that
  # concatenates all inputs (including reset) and all outputs
  #
  if dut_ifc.reset == None:
    inputs_width = 0
  else:
    inputs_width = 1

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Lockstep_In_IFC):
      inputs_width += xactor.field_width

  outputs_width = 0
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Lockstep_Out_IFC):
      outputs_width += xactor.field_width

  f_out.write ("// Copyright (c) 2013-2015 Bluespec Inc. All Rights Reserved.\n")
  f_out.write ("// This file is program generated; please do not edit!\n")
  f_out.write ("\n")
  f_out.write ("package SceMiLayer;\n")

  f_out.write ("\n")
  f_out.write ("import Clocks::*;\n")
  f_out.write ("import Connectable::*;\n")
  f_out.write ("import GetPut::*;\n")
  f_out.write ("import DefaultValue::*;\n")
  f_out.write ("import Readback::*;\n")
  f_out.write ("import SceMi::*;\n")
  f_out.write ("import SceMiDefines::*;\n")
  f_out.write ("import SceMiLockStepXactors::*;\n")

  f_out.write ("\n")
  f_out.write ("import %s::*;\n" % BsvDUT)

  f_out.write ("\n")
  f_out.write ("`include \"Readback.defines\"\n")

  f_out.write ("\n")
  f_out.write ("module [SceMiModule] mkSceMiLayer();\n")

  f_out.write ("\n")
  f_out.write ("   SceMiClockConfiguration conf = defaultValue;\n")
  f_out.write ("   conf.clockNum        = 0;\n")
  f_out.write ("   conf.resetCycles     = 4;\n")
  f_out.write ("   conf.ratioNumerator  = 2;\n")
  f_out.write ("   SceMiClockPortIfc clk_port <- mkSceMiClockPort(conf);\n")

  f_out.write ("\n")
  f_out.write ("   %s_IFC dut <- buildDut(mk%s, clk_port);\n" % (BsvDUT, BsvDUT))

  f_out.write ("\n")
  f_out.write ("   let cclock = clk_port.cclock;\n")
  f_out.write ("   let creset = clk_port.creset;\n")

  f_out.write ("\n")
  f_out.write ("   SceMiResetXactorIfc reset <- mkSceMiResetXactor(clk_port);\n")
  f_out.write ("   Empty     control <- mkShutdownXactor();\n")

  f_out.write ("\n")
  f_out.write ("   `READBACKCORE(simControl, conf);\n")

  f_out.write ("\n")
  f_out.write ("   Empty  tbsimControl <- mkSimulationControl(conf);\n")

  # Lockstep transactor
  f_out.write ("\n")
  f_out.write ("   Clock iclock <- invertCurrentClock(clocked_by cclock, reset_by creset);\n")
  f_out.write ("   Reset ireset <- mkAsyncResetFromCR(0, iclock);\n")
  f_out.write ("\n")
  f_out.write ("   SceMiPipe#(Bit#(%s), Bit#(%s)) lockstep <- mkLockStepPipeXactor(conf.clockNum, 1024, clk_port, iclock, ireset);\n"
               % (inputs_width, outputs_width))
  f_out.write ("\n")

  pre_str = "   Bit#(%s) outputs = { " % outputs_width
  f_out.write (pre_str)
  sep_str = ",\n" + (" " * len(pre_str))

  first = True
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Lockstep_Out_IFC):
      if first:
        first = False
      else:
        f_out.write (sep_str)
      f_out.write ("dut.out_%s" % xactor_name)
  f_out.write (" };\n")
  f_out.write ("\n")

  f_out.write ("   rule every_out;\n")
  f_out.write ("      lockstep.outputs.put(outputs);\n")
  f_out.write ("   endrule\n")

  f_out.write ("\n")
  f_out.write ("   rule every_in;\n")
  f_out.write ("      let value <- lockstep.inputs.get;\n")

  ptr = inputs_width

  if dut_ifc.reset != None:
    hi = ptr - 1
    lo = hi
    f_out.write ("      dut.rst_n(value[%d:%d]);\n" % (hi, lo))
    ptr = lo

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Lockstep_In_IFC):
      hi = ptr - 1
      lo = ptr - xactor.field_width
      f_out.write ("      dut.in_%s(value[%d:%d]);\n" % (xactor_name, hi, lo))
      ptr = lo

  f_out.write ("   endrule\n")

  f_out.write ("\n")
  f_out.write ("endmodule\n")
  f_out.write ("\n")
  f_out.write ("endpackage\n")

# ================================================================
# Main program when executed from the cmd line

def main (argv = None):
  if (len (sys.argv) != 3):
    print "%s  <pin_file_name>  <dir for BsvVMOD.bsv and SceMiLayer.bsv>" % sys.argv [0]
    return 1

  pin_filename = sys.argv [1]
  output_dir = sys.argv [2]
  (dut_name, ext) = os.path.splitext (os.path.basename (pin_filename))
  SceMiLayer_filename = os.path.join (output_dir, "SceMiLayer.bsv")
  BsvDUT = "Bsv" + dut_name
  BsvDUT_filename = os.path.join (output_dir, BsvDUT + ".bsv")

  try:
    f_in = open (pin_filename, "r")
  except:
    print "Error opening pin file: ", pin_filename
    return 1

  dut_ifc = parse_pin_file (f_in, pin_filename)
  if isinstance (dut_ifc, DUT_IFC):
    dut_ifc.show ("  ")

  try:
    f_out = open (SceMiLayer_filename, "w")
  except:
    print "Error opening file: ", SceMiLayer_filename
    return 1

  print "Generating file: ", SceMiLayer_filename
  if dut_ifc.is_lockstep ():
    genLockstepSceMiLayerFile (f_out, dut_ifc, BsvDUT)
  else:
    genSceMiLayerFile (f_out, dut_ifc, BsvDUT)

  try:
    f_out = open (BsvDUT_filename, "w")
  except:
    print "Error opening file: ", BsvDUT_filename
    return 1

  print "Generating file: ", BsvDUT_filename
  if dut_ifc.is_lockstep ():
    genLockstepBsvDUTFile (f_out, dut_ifc, dut_name, BsvDUT)
  else:
    genBsvDUTFile (f_out, dut_ifc, dut_name, BsvDUT)

  return 0

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main ())
