#!/usr/bin/python

# Copyright (c) 2105 Bluespec, Inc.  All Rights Reserved
# Author: J Schwartz

# ================================================================

# Usage:    Generate_TBTemplate.py  pin_file  tb_src_dir  [gen_gui]

# XXX ...

# ================================================================
# Standard Python lib imports

import sys
import os
import shutil

# ================================================================
# Project imports

from ParsePin import *    # The parser for .pin files

# ================================================================
# Generate <DUT>_proxy.v

def genDUTProxy (output_dir, dut_ifc, new_module_name):

  proxy_filename = os.path.join (output_dir, "%s_proxy.v" % new_module_name)
  try:
    f_proxy = open (proxy_filename, "w")
  except:
    print "Error opening file: ", proxy_filename
    return 1

  print "Generating file: ", proxy_filename
  if dut_ifc.is_lockstep ():
    genLockstepDUTProxyFile (f_proxy, dut_ifc, new_module_name)
  else:
    genDUTProxyFile (f_proxy, dut_ifc, new_module_name)

  f_proxy.close()

  return 0


# Utility to write a width as "[N-1 : 0] " or empty string if N==1
def prWidth (width):
  if (width > 1):
    return ("[%d : 0] " % (width - 1))
  else:
    return ""

# Utility to write an extraction range, given the high index and the width
def prRange (hi, width):
  lo = hi - width + 1
  return ("[%d:%d]" % (hi, lo))

def writeReg(f_out, reg, clk, rst, init, en, din):
  f_out.write ("   always@(posedge %s)\n" % clk)
  f_out.write ("     begin\n")
  if (rst != ""):
    f_out.write ("       if (%s == `BSV_RESET_VALUE)\n" % rst)
    f_out.write ("         %s <= `BSV_ASSIGNMENT_DELAY %s;\n" % (reg, init))
    f_out.write ("       else\n")
    f_out.write ("         begin\n")
    f_out.write ("           if (%s)\n" % en)
    f_out.write ("             %s <= `BSV_ASSIGNMENT_DELAY %s;\n" % (reg, din))
    f_out.write ("         end // else: !if(RST == `BSV_RESET_VALUE)\n")
  else:
    f_out.write ("       if (%s)\n" % en)
    f_out.write ("         %s <= `BSV_ASSIGNMENT_DELAY %s;\n" % (reg, din))
  f_out.write ("     end\n")

def genDUTProxyFile (f_out, dut_ifc, module_name):

  f_out.write ("// Host-side proxy for '%s'\n" % module_name)
  f_out.write ("//\n")
  f_out.write ("// This module can be instantiated in a Verilog testbench in place\n")
  f_out.write ("// of the DUT, allowing the simulated testbench to communicate with\n")
  f_out.write ("// the actual DUT on an FPGA (or simulating in another process).\n")
  f_out.write ("//\n")
  f_out.write ("// This is not a 'lockstep' proxy.  This proxy does not make any\n")
  f_out.write ("// guarantees about the number of clock cycles to the DUT per cycle\n")
  f_out.write ("// to the proxy; inputs and outputs arrive when they can.  The proxy\n")
  f_out.write ("// expects to communicate with a non-lockstep wrapper around the DUT.\n")
  f_out.write ("//\n")
  f_out.write ("\n")

  f_out.write ("`ifdef BSV_ASSIGNMENT_DELAY\n")
  f_out.write ("`else\n")
  f_out.write ("  `define BSV_ASSIGNMENT_DELAY\n")
  f_out.write ("`endif\n")
  f_out.write ("\n")

  f_out.write ("`ifdef BSV_POSITIVE_RESET\n")
  f_out.write ("  `define BSV_RESET_VALUE 1'b1\n")
  f_out.write ("  `define BSV_RESET_EDGE posedge\n")
  f_out.write ("`else\n")
  f_out.write ("  `define BSV_RESET_VALUE 1'b0\n")
  f_out.write ("  `define BSV_RESET_EDGE negedge\n")
  f_out.write ("`endif\n")
  f_out.write ("\n")

  inst_prefix = "module %s_proxy (" % module_name
  f_out.write (inst_prefix)

  # Each port after the first will be indented
  sep_str_first = ",\n\n" + (" " * len(inst_prefix))
  sep_str_rest = ",\n" + (" " * len(inst_prefix))

  # There must always be a clock
  f_out.write (dut_ifc.clock.verilog_name)

  # If there is a reset
  if dut_ifc.reset != None:
    f_out.write ("%s%s" % (sep_str_rest, dut_ifc.reset.verilog_name))

  # Declare the ports for each get/put interface
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("%s%s" % (sep_str_first, xactor.rdy))
      f_out.write ("%s%s" % (sep_str_rest, xactor.en))
      for p in xactor.verilog_names:
        f_out.write ("%s%s" % (sep_str_rest, p))

    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("%s%s" % (sep_str_first, xactor.rdy))
      f_out.write ("%s%s" % (sep_str_rest, xactor.en))
      for p in xactor.verilog_names:
        f_out.write ("%s%s" % (sep_str_rest, p))

  # Declare the individual ports
  first = True
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if (isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC)
        or isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC)):
      if first:
        f_out.write ("%s%s" % (sep_str_first, xactor_name))
        first = False
      else:
        f_out.write ("%s%s" % (sep_str_rest, xactor_name))

#  # Declare the ports for the memory interface
#  for xactor_name in dut_ifc.xactors:
#    xactor = dut_ifc.xactors [xactor_name]
#    if isinstance (xactor, Xactor_MEM_IFC):
#      f_out.write ("%s%s" % (sep_str_first, xactor.req_rdy))
#      f_out.write ("%s%s" % (sep_str_rest, xactor.req_en))
#      f_out.write ("%s%s" % (sep_str_rest, xactor.req_addr_data_verilog_name))
#      f_out.write ("%s%s" % (sep_str_rest, xactor.resp_rdy))
#      f_out.write ("%s%s" % (sep_str_rest, xactor.resp_en))
#      f_out.write ("%s%s" % (sep_str_rest, xactor.resp_data_verilog_name))

  f_out.write (");\n\n")

  f_out.write ("  input %s;\n" % dut_ifc.clock.verilog_name)
  if dut_ifc.reset != None:
    f_out.write ("  input %s;\n" % dut_ifc.reset.verilog_name)
  f_out.write ("\n")
  
  # Declare the signals for each get/putinterface
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("  // Get interface %s\n" % xactor_name)
      for j in range (len (xactor.verilog_names)):
        f_out.write ("  output %s%s;\n" % (prWidth(xactor.field_widths [j]), xactor.verilog_names [j]))
      f_out.write ("  output %s;\n" % xactor.rdy)
      f_out.write ("  input %s;\n" % xactor.en)
      f_out.write ("\n")

    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("  // Put interface %s\n" % xactor_name)
      for j in range (len (xactor.verilog_names)):
        f_out.write ("  input %s%s;\n" % (prWidth(xactor.field_widths [j]), xactor.verilog_names [j]))
      f_out.write ("  output %s;\n" % xactor.rdy)
      f_out.write ("  input %s;\n" % xactor.en)
      f_out.write ("\n")

  # Declare the signals for individual ports
  first = True
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      if first:
        f_out.write ("  // individual ports\n")
        first = False
      f_out.write ("  input %s%s;\n" % (prWidth(xactor.field_width), xactor_name))

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      if first:
        f_out.write ("  // individual ports\n")
        first = False
      f_out.write ("  output %s%s;\n" % (prWidth(xactor.field_width), xactor_name))

  if not first:
    f_out.write ("\n")

#  # Declare the signals for the memory interface
#  for xactor_name in dut_ifc.xactors:
#    xactor = dut_ifc.xactors [xactor_name]
#    if isinstance (xactor, Xactor_MEM_IFC):
#      f_out.write ("  // memory interface %s\n" % xactor_name)
#      req_width = xactor.address_width + xactor.data_width + xactor.data_width/8 + 1
#      resp_width = xactor.data_width
#      f_out.write ("  output %s;\n" % xactor.req_rdy)
#      f_out.write ("  input %s;\n" % xactor.req_en)
#      f_out.write ("  output %s%s;\n" % (prWidth(req_width), xactor.req_addr_data_verilog_name))
#      f_out.write ("  output %s;\n" % xactor.resp_rdy)
#      f_out.write ("  input %s;\n" % xactor.resp_en)
#      f_out.write ("  input %s%s;\n" % (prWidth(resp_width), xactor.resp_data_verilog_name))
#      f_out.write ("\n")

  # Declare wires for proxy submodule instantiations (Get/Put)
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      proxy_width = sum(xactor.field_widths)
      f_out.write ("  // ports of Put proxy submodule for %s\n" % xactor_name)
      f_out.write ("  wire %s%s_data_in$DATA;\n" % (prWidth(proxy_width), xactor_name))
      f_out.write ("  wire %s_data_in$DATA_EN, %s_data_in$DATA_RDY;\n" % (xactor_name, xactor_name))
      f_out.write ("\n")

    elif isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      proxy_width = sum(xactor.field_widths)
      f_out.write ("  // ports of Get proxy submodule for %s\n" % xactor_name)
      f_out.write ("  wire %s%s_data_out$DATA;\n" % (prWidth(proxy_width), xactor_name))
      f_out.write ("  wire %s_data_out$DATA_EN, %s_data_out$DATA_RDY;\n" % (xactor_name, xactor_name))
      f_out.write ("\n")

  # Declare wires for proxy submodule instantiations (individual ports)
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("  // ports of inport proxy submodule for %s\n" % xactor_name)
      f_out.write ("  wire %s%s_data_in$DATA;\n" % (prWidth(xactor.field_width), xactor_name))
      f_out.write ("  wire %s_data_in$DATA_EN, %s_data_in$DATA_RDY;\n" % (xactor_name, xactor_name))
      f_out.write ("  // register for detecting a change in the input value\n")
      f_out.write ("  reg %s%s_data_in$REG;\n" % (prWidth(xactor.field_width), xactor_name))
      f_out.write ("\n")

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("  // ports of outport proxy submodule for %s\n" % xactor_name)
      f_out.write ("  wire %s%s_data_out$DATA;\n" % (prWidth(xactor.field_width), xactor_name))
      f_out.write ("  wire %s_data_out$DATA_EN, %s_data_out$DATA_RDY;\n" % (xactor_name, xactor_name))
      f_out.write ("  // register for holding the output value\n")
      f_out.write ("  reg %s%s_data_out$REG;\n" % (prWidth(xactor.field_width), xactor_name))
      f_out.write ("\n")

  # Assign values to the outputs (Get/Put)
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if (isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC)):
      f_out.write ("  // ports of inport proxy %s\n" % xactor_name)
      f_out.write ("  assign %s = %s_data_in$DATA_RDY;\n" % (xactor.rdy, xactor_name))
      f_out.write ("\n")

    elif (isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC)):
      f_out.write ("  // actionvalue method %s\n" % xactor_name)
      if (len (xactor.verilog_names) == 1):
        f_out.write ("  assign %s = %s_data_out$DATA;\n" % (xactor.verilog_names [0], xactor_name))
      else:
        # Generate a separate assignment for each
        #hi = sum (xactor.field_widths) - 1
        #for j in range (len (xactor.verilog_names)):
        #  width = xactor.field_widths [j]
        #  f_out.write ("  assign %s = %s_data_out$DATA%s;\n" % (xactor.verilog_names [j], xactor_name, prRange(hi, width)))
        #  hi = hi - width

        # The above code generates a seperate assignment for each, but it
        # may be easier to edit for the user if we generate one assignment?
        f_out.write ("  assign { ")
        first = True
        for p in xactor.verilog_names:
          if first:
            first = False
          else:
            f_out.write (", ")
          f_out.write (p)
        f_out.write (" } = %s_data_out$DATA;\n" % xactor_name)

      f_out.write ("  assign %s = %s_data_out$DATA_RDY;\n" % (xactor.rdy, xactor_name))
      f_out.write ("\n")

  # Assign values to the outputs (individual ports)
  first = True
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      if first:
        f_out.write ("  // individual ports\n")
        first = False
      f_out.write ("  assign %s = %s_data_out$REG;\n" % (xactor_name, xactor_name))

  if not first:
    f_out.write ("\n")

  # Proxy submodule instantiations for Get/Put
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      if isinstance (xactor, Xactor_PUT_IFC):
        port_name = "scemi_put_%s_inport" % xactor_name
      else:
        port_name = "scemi_put_%s_inpipe" % xactor_name
      proxy_width = sum(xactor.field_widths)
      f_out.write ("  // submodule %s_data_in\n" % xactor_name)
      # Wire assignments
      f_out.write ("  assign %s_data_in$DATA = { " % xactor_name)
      first = True
      for p in xactor.verilog_names:
        if first:
          first = False
        else:
          f_out.write (", ")
        f_out.write (p)
      f_out.write (" };\n")
      f_out.write ("  assign %s_data_in$DATA_EN = %s;\n" % (xactor_name, xactor.en))
      # Module instantiation
      f_out.write ("  SceMiInPipeProxyPut #(.WIDTH(%d),\n" % proxy_width)
      f_out.write ("                        .paramFile(\"\"),\n")
      f_out.write ("                        .transactorName(\"\"),\n")
      f_out.write ("                        .portName(\"%s\")) %s_data_in (" % (port_name, xactor_name))
      indent_str = " " * (45 + len(xactor_name) + 11 + len(xactor_name) + 10)
      f_out.write (".CLK(%s),\n" % dut_ifc.clock.verilog_name)
      if dut_ifc.reset != None:
        f_out.write ("%s.RST_N(%s),\n" % (indent_str, dut_ifc.reset.verilog_name))
      else:
        f_out.write ("%s.RST_N(),\n" % indent_str)
      f_out.write ("%s.DATA(%s_data_in$DATA),\n" % (indent_str, xactor_name))
      f_out.write ("%s.DATA_EN(%s_data_in$DATA_EN),\n" % (indent_str, xactor_name))
      f_out.write ("%s.DATA_RDY(%s_data_in$DATA_RDY));\n" % (indent_str, xactor_name))
      f_out.write ("\n")

    elif isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      if isinstance (xactor, Xactor_GET_IFC):
        port_name = "scemi_get_%s_outport" % xactor_name
      else:
        port_name = "scemi_get_%s_outpipe" % xactor_name
      proxy_width = sum(xactor.field_widths)
      f_out.write ("  // submodule %s_data_out\n" % xactor_name)
      # Wire assignments
      f_out.write ("  assign %s_data_out$DATA_EN = %s;\n" % (xactor_name, xactor.en))
      # Module instantiation
      f_out.write ("  SceMiOutPipeProxyGet #(.WIDTH(%d),\n" % proxy_width)
      f_out.write ("                         .paramFile(\"\"),\n")
      f_out.write ("                         .transactorName(\"\"),\n")
      f_out.write ("                         .portName(\"%s\")) %s_data_out (" % (port_name, xactor_name))
      indent_str = " " * (46 + len(xactor_name) + 12 + len(xactor_name) + 11)
      f_out.write (".CLK(%s),\n" % dut_ifc.clock.verilog_name)
      if dut_ifc.reset != None:
        f_out.write ("%s.RST_N(%s),\n" % (indent_str, dut_ifc.reset.verilog_name))
      else:
        f_out.write ("%s.RST_N(),\n" % indent_str)
      f_out.write ("%s.DATA(%s_data_out$DATA),\n" % (indent_str, xactor_name))
      f_out.write ("%s.DATA_EN(%s_data_out$DATA_EN),\n" % (indent_str, xactor_name))
      f_out.write ("%s.DATA_RDY(%s_data_out$DATA_RDY));\n" % (indent_str, xactor_name))
      f_out.write ("\n")

  # Proxy submodule instantiations for individual ports
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      if isinstance (xactor, Xactor_Raw_In_IFC):
        port_name = "scemi_put_%s_inport" % xactor_name
      else:
        port_name = "scemi_put_%s_inpipe" % xactor_name
      f_out.write ("  // submodule %s_data_in\n" % xactor_name)
      # Wire assignments
      f_out.write ("  assign %s_data_in$DATA = %s;\n" % (xactor_name, xactor_name))
      pre_str = "  assign %s_data_in$DATA_EN = " % xactor_name
      f_out.write ("%s%s_data_in$DATA_RDY &&\n" % (pre_str, xactor_name))
      f_out.write ("%s(%s_data_in$REG != %s);\n" % (" " * len(pre_str), xactor_name, xactor_name))
      # Module instantiation
      f_out.write ("  SceMiInPipeProxyPut #(.WIDTH(%d),\n" % xactor.field_width)
      f_out.write ("                        .paramFile(\"\"),\n")
      f_out.write ("                        .transactorName(\"\"),\n")
      f_out.write ("                        .portName(\"%s\")) %s_data_in (" % (port_name, xactor_name))
      indent_str = " " * (45 + len(xactor_name) + 11 + len(xactor_name) + 10)
      f_out.write (".CLK(%s),\n" % dut_ifc.clock.verilog_name)
      if dut_ifc.reset != None:
        f_out.write ("%s.RST_N(%s),\n" % (indent_str, dut_ifc.reset.verilog_name))
      else:
        f_out.write ("%s.RST_N(),\n" % indent_str)
      f_out.write ("%s.DATA(%s_data_in$DATA),\n" % (indent_str, xactor_name))
      f_out.write ("%s.DATA_EN(%s_data_in$DATA_EN),\n" % (indent_str, xactor_name))
      f_out.write ("%s.DATA_RDY(%s_data_in$DATA_RDY));\n" % (indent_str, xactor_name))
      f_out.write ("\n")

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      if isinstance (xactor, Xactor_Raw_Out_IFC):
        port_name = "scemi_get_%s_outport" % xactor_name
      else:
        port_name = "scemi_get_%s_outpipe" % xactor_name
      f_out.write ("  // submodule %s_data_out\n" % xactor_name)
      # Wire assignments
      f_out.write ("  assign %s_data_out$DATA_EN = %s_data_out$DATA_RDY;\n" % (xactor_name, xactor_name))
      # Module instantiation
      f_out.write ("  SceMiOutPipeProxyGet #(.WIDTH(%d),\n" % xactor.field_width)
      f_out.write ("                         .paramFile(\"\"),\n")
      f_out.write ("                         .transactorName(\"\"),\n")
      f_out.write ("                         .portName(\"%s\")) %s_data_out (" % (port_name, xactor_name))
      indent_str = " " * (46 + len(xactor_name) + 12 + len(xactor_name) + 11)
      f_out.write (".CLK(%s),\n" % dut_ifc.clock.verilog_name)
      if dut_ifc.reset != None:
        f_out.write ("%s.RST_N(%s),\n" % (indent_str, dut_ifc.reset.verilog_name))
      else:
        f_out.write ("%s.RST_N(),\n" % indent_str)
      f_out.write ("%s.DATA(%s_data_out$DATA),\n" % (indent_str, xactor_name))
      f_out.write ("%s.DATA_EN(%s_data_out$DATA_EN),\n" % (indent_str, xactor_name))
      f_out.write ("%s.DATA_RDY(%s_data_out$DATA_RDY));\n" % (indent_str, xactor_name))
      f_out.write ("\n")

  # Always blocks to manage the individual ports
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("  // Recorded transmitted value of %s\n" % xactor_name)
      reg = "%s_data_in$REG" % xactor_name
      # 'xactor.clock' contains the symbolic name, we want the Verilog signal name.
      # For now, there's only one clock, so the lookup is easy.
      clk = dut_ifc.clock.verilog_name
      # The pin file does not associate ports with resets, but for now there is only one.
      if dut_ifc.reset != None:
        rst = dut_ifc.reset.verilog_name
      else:
        rst = ""
      init = 0
      en = "%s_data_in$DATA_EN" % xactor_name
      din = xactor_name
      writeReg (f_out, reg, clk, rst, init, en, din)
      f_out.write ("\n")

    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("  // Recorded received value of %s\n" % xactor_name)
      reg = "%s_data_out$REG" % xactor_name
      # 'xactor.clock' contains the symbolic name, we want the Verilog signal name.
      # For now, there's only one clock, so the lookup is easy.
      clk = dut_ifc.clock.verilog_name
      # The pin file does not associate ports with resets, but for now there is only one.
      if dut_ifc.reset != None:
        rst = dut_ifc.reset.verilog_name
      else:
        rst = ""
      init = 0
      en = "%s_data_out$DATA_EN" % xactor_name
      din = "%s_data_out$DATA" % xactor_name
      writeReg (f_out, reg, clk, rst, init, en, din)
      f_out.write ("\n")

  f_out.write ("endmodule  // %s_proxy\n" % module_name)


def genLockstepDUTProxyFile (f_out, dut_ifc, module_name):

  f_out.write ("// Host-side lockstep proxy for '%s'\n" % module_name)
  f_out.write ("//\n")
  f_out.write ("// This module can be instantiated in a Verilog testbench in place\n")
  f_out.write ("// of the DUT, allowing the simulated testbench to communicate with\n")
  f_out.write ("// the actual DUT on an FPGA (or simulating in another process).\n")
  f_out.write ("//\n")
  f_out.write ("// This is a 'lockstep' proxy that, in coordination with a lockstep\n")
  f_out.write ("// wrapper around the DUT, will cycle the DUT's clock once per cycle\n")
  f_out.write ("// of the proxy clock, keeping the testbench and the DUT in lockstep.\n")
  f_out.write ("//\n")
  f_out.write ("\n")

  #f_out.write ("`ifdef BSV_ASSIGNMENT_DELAY\n")
  #f_out.write ("`else\n")
  #f_out.write ("  `define BSV_ASSIGNMENT_DELAY\n")
  #f_out.write ("`endif\n")
  #f_out.write ("\n")

  #f_out.write ("`ifdef BSV_POSITIVE_RESET\n")
  #f_out.write ("  `define BSV_RESET_VALUE 1'b1\n")
  #f_out.write ("  `define BSV_RESET_EDGE posedge\n")
  #f_out.write ("`else\n")
  #f_out.write ("  `define BSV_RESET_VALUE 1'b0\n")
  #f_out.write ("  `define BSV_RESET_EDGE negedge\n")
  #f_out.write ("`endif\n")
  #f_out.write ("\n")

  inst_prefix = "module %s_proxy (" % module_name
  f_out.write (inst_prefix)

  # Each port after the first will be indented
  sep_str_first = ",\n\n" + (" " * len(inst_prefix))
  sep_str_rest = ",\n" + (" " * len(inst_prefix))

  # There must always be a clock
  f_out.write (dut_ifc.clock.verilog_name)

  # If there is a reset
  if dut_ifc.reset != None:
    f_out.write ("%s%s" % (sep_str_rest, dut_ifc.reset.verilog_name))

  # Declare the lockstep ports
  first = True
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Lockstep_In_IFC) or isinstance (xactor, Xactor_Lockstep_Out_IFC):
      if first:
        f_out.write ("%s%s" % (sep_str_first, xactor_name))
        first = False
      else:
        f_out.write ("%s%s" % (sep_str_rest, xactor_name))

  f_out.write (");\n\n")

  f_out.write ("  input %s;\n" % dut_ifc.clock.verilog_name)
  if dut_ifc.reset != None:
    f_out.write ("  input %s;\n" % dut_ifc.reset.verilog_name)
  f_out.write ("\n")
  
  # Declare the signals for lockstep ports
  first = True
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Lockstep_In_IFC):
      if first:
        first = False
      f_out.write ("  input %s%s;\n" % (prWidth(xactor.field_width), xactor_name))

    elif isinstance (xactor, Xactor_Lockstep_Out_IFC):
      if first:
        first = False
      f_out.write ("  output %s%s;\n" % (prWidth(xactor.field_width), xactor_name))

  # Individual ports are communicated through a single proxy that
  # concatenates all inputs (including reset) and all outputs
  #
  if dut_ifc.reset == None:
    inputs = []
    inputs_width = 0
  else:
    inputs = [ dut_ifc.reset.verilog_name ]
    inputs_width = 1

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Lockstep_In_IFC):
      inputs.append (xactor_name)
      inputs_width += xactor.field_width

  outputs = []
  outputs_width = 0
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Lockstep_Out_IFC):
      outputs.append (xactor_name)
      outputs_width += xactor.field_width

  if ((outputs_width > 0) or (inputs_width > 0)):
    f_out.write ("  // ports of submodule data\n")
    if (inputs_width > 0):
      f_out.write ("  wire %sdata$INPUTS;\n" % prWidth(inputs_width))
    if (outputs_width > 0):
      f_out.write ("  wire %sdata$OUTPUTS;\n" % prWidth(outputs_width))
    f_out.write ("\n")

  if (inputs_width > 0):
    f_out.write ("  // Input assignment\n")
    f_out.write ("  assign data$INPUTS = { ")
    first = True
    for p in inputs:
      if first:
        first = False
      else:
        f_out.write (", ")
      f_out.write (p)
    f_out.write (" };\n\n")

  if (outputs_width > 0):
    f_out.write ("  // Output assignment\n")
    f_out.write ("  assign { ")
    first = True
    for p in outputs:
      if first:
        first = False
      else:
        f_out.write (", ")
      f_out.write (p)
    f_out.write (" } = data$OUTPUTS;\n\n")

  if ((outputs_width > 0) or (inputs_width > 0)):
    f_out.write ("  // submodule data proxy\n")
    f_out.write ("  SceMiPipeProxyRS #(.WIDTH_IN(%d),\n" % max(inputs_width,1))
    f_out.write ("                     .WIDTH_OUT(%d),\n" % max(outputs_width,1))
    f_out.write ("                     .paramFile(\"\"),\n")
    f_out.write ("                     .transactorName(\"\"),\n")
    f_out.write ("                     .portNameIn(\"scemi_lockstep_lock_inpipe\"),\n")
    f_out.write ("                     .portNameOut(\"scemi_lockstep_lock_outpipe\")) data (")
    indent_str = "                                                                        "
    f_out.write (".CLK(%s),\n" % dut_ifc.clock.verilog_name)
    if dut_ifc.reset != None:
      f_out.write ("%s.RST_N(%s),\n" % (indent_str, dut_ifc.reset.verilog_name))
    else:
      f_out.write ("%s.RST_N(),\n" % indent_str)
    if (inputs_width > 0):
      f_out.write ("%s.INPUTS(data$INPUTS),\n" % indent_str)
    else:
      f_out.write ("%s.INPUTS(0),\n" % indent_str)
    if (outputs_width > 0):
      f_out.write ("%s.OUTPUTS(data$OUTPUTS));\n\n" % indent_str)
    else:
      f_out.write ("%s.OUTPUTS());\n\n" % indent_str)

  f_out.write ("endmodule  // %s_proxy\n" % module_name)


# ================================================================
# Main program when executed from the cmd line

def main (argv = None):
  if (len (sys.argv) < 3) or (len (sys.argv) > 4):
    print "Usage:  %s  <pin_file_name>  <tb_src_dir>  [gen_gui]" % os.path.basename(sys.argv [0])
    return 1

  pin_filename = sys.argv [1]
  tb_src_dir = sys.argv [2]
  gen_gui = False
  if (len (sys.argv) > 3):
    gen_gui = (sys.argv [3]).lower() in ("yes", "true", "t", "1")

  if not os.path.exists(tb_src_dir):
    os.makedirs(tb_src_dir)

  (dut_name, ext) = os.path.splitext (os.path.basename (pin_filename))

  try:
    f_pin = open (pin_filename, "r")
  except:
    print "Error opening pin file: ", pin_filename
    return 1

  dut_ifc = parse_pin_file (f_pin, pin_filename)
  #if isinstance (dut_ifc, DUT_IFC):
  #  dut_ifc.show ("  ")

  # Generate a template DUT proxy
  #
  if genDUTProxy (tb_src_dir, dut_ifc, dut_name):
    # error message will have been displayed
    return 1

  # Copy the simtb GUI into the TB directory
  # The GUI will do this separately, but we include it as an option here
  # for command-line users
  #
  if gen_gui:
    gui_dut_src = os.path.join(os.getenv('BLUESPECDIR'), "tcllib", "simtb", "simtb_gui_dut.tcl")
    gui_dut_dst = os.path.join(tb_src_dir, "gui_dut.tcl")
    shutil.copy2 (gui_dut_src, gui_dut_dst)
    print "Generating file: ", gui_dut_dst

  return 0

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main ())
