#!/usr/bin/python

# Copyright (c) 2105 Bluespec, Inc.  All Rights Reserved
# Author: J Schwartz

# ================================================================

# Usage:    Generate_TBTemplate.py  pin_file  tb_src_dir  cpp_src_dir

# XXX ...

# ================================================================
# Standard Python lib imports

import sys
import os

# ================================================================
# Project imports

from ParsePin import *    # The parser for .pin files

from Generate_TestBench import genTclGui
from Generate_TestBench import genTclTb
from Generate_TestBench import genDutXactor

# ================================================================
# Generate TclTb.cpp

def genUserTemplate (output_dir, dut_ifc, new_module_name):

  template_filename = os.path.join (output_dir, "usertb.cpp")
  try:
    f_template = open (template_filename, "w")
  except:
    print "Error opening file: ", template_filename
    return 1

  print "Generating file: ", template_filename
  genUserTemplateFile (f_template, dut_ifc, new_module_name)
  f_template.close()

  return 0


def genUserTemplateFile (f_out, dut_ifc, new_module_name):

  f_out.write ("// Copyright Bluespec Inc. 2010-2011\n")
  f_out.write ("// By: GenTestbench tool\n\n")

  f_out.write ("#include <iostream>\n\n")
  f_out.write ("// Boilerplate file in $BLUESPECDIR/SceMi/capi/\n")
  f_out.write ("#include \"usertb.h\"\n\n")
  f_out.write ("// Locally generated file specific to the DUT pin file, which includes\n")
  f_out.write ("// boilerplate from $BLUESPECDIR/SceMi/capi/semu_capi.h\n")
  f_out.write ("#include \"capi.h\"\n\n")
  f_out.write ("////////////////////////////////////////////////////////////////////////////////\n")
  f_out.write ("//\n")
  f_out.write ("//  This is a basic template for a C++ user testbench\n")
  f_out.write ("//\n")
  f_out.write ("//  Please refer to the capi.h file in $project_dir/build/cpp for the C API.\n")
  f_out.write ("//  The capi.h file documents all of the C API calls for Semu, including\n")
  f_out.write ("//  the interface-customized calls to communicate to your DUT.\n")
  f_out.write ("//\n")
  f_out.write ("////////////////////////////////////////////////////////////////////////////////\n\n")
  f_out.write ("int do_test() {\n\n")

  f_out.write ("  //////////////////////////////////////////////////////////////////////////////\n")
  f_out.write ("  //////////////////////////////////////////////////////////////////////////////\n")
  f_out.write ("  //\n")
  f_out.write ("  //  If you do NOT plan to actively control the DUT clock from within your C\n")
  f_out.write ("  //  testbench, then you should UNcomment the following lines of code.  When\n")
  f_out.write ("  //  this code is active, then the DUT clock will start running as soon as\n")
  f_out.write ("  //  the user clicks 'Run' in the Emulation Control Panel.\n")
  f_out.write ("  //\n")
  f_out.write ("  //  If, instead, you plan to control the clock (e.g. single-step the clock)\n")
  f_out.write ("  //  from within your C testbench, then leave it 'as is' (that is, commented\n")
  f_out.write ("  //  out).\n")
  f_out.write ("  //\n")
  f_out.write ("  //////////////////////////////////////////////////////////////////////////////\n")
  f_out.write ("  //////////////////////////////////////////////////////////////////////////////\n\n")

  f_out.write ("/*\n")
  f_out.write ("  if (!semu_start_controlled_clock()) {\n")
  f_out.write ("    cerr << \"Something wrong, cannot start controlled clock\" << endl;\n")
  f_out.write ("    return 0;\n")
  f_out.write ("  }\n")
  f_out.write ("*/\n\n")

  f_out.write ("  //////////////////////////////////////////////////////////////////////////////\n")
  f_out.write ("  //////////////////////////////////////////////////////////////////////////////\n\n\n")
  f_out.write ("  //  ************    Here's where you insert testbench code   *************\n\n\n")

  f_out.write ("  //////////////////////////////////////////////////////////////////////////////\n")
  f_out.write ("  //////////////////////////////////////////////////////////////////////////////\n\n")
  f_out.write ("  return 1;\n")
  f_out.write ("}\n\n")


# ================================================================
# Main program when executed from the cmd line

def main (argv = None):
  if (len (sys.argv) != 4):
    print "Usage:  %s  <pin_file_name>  <tb_src_dir>  <cpp_src_dir>" % os.path.basename(sys.argv [0])
    return 1

  pin_filename = sys.argv [1]
  tb_src_dir = sys.argv [2]
  cpp_src_dir = sys.argv [3]

  usertb = True

  if not os.path.exists(tb_src_dir):
    os.makedirs(tb_src_dir)
  if not os.path.exists(cpp_src_dir):
    os.makedirs(cpp_src_dir)

  (dut_name, ext) = os.path.splitext (os.path.basename (pin_filename))
  BsvDUT = "Bsv" + dut_name

  try:
    f_pin = open (pin_filename, "r")
  except:
    print "Error opening pin file: ", pin_filename
    return 1

  dut_ifc = parse_pin_file (f_pin, pin_filename)
  #if isinstance (dut_ifc, DUT_IFC):
  #  dut_ifc.show ("  ")

  # Generate a template TB that uses the C API.
  #
  if genUserTemplate (tb_src_dir, dut_ifc, BsvDUT):
    # error message will have been displayed
    return 1

  # Generate the usertb DUT GUI in the TB directory
  # (The user may want to edit it, though this is not necessary.)
  if genTclGui (tb_src_dir, dut_ifc, BsvDUT, usertb):
    # error message will have been displayed
    return 1

  # Generate a TclTb.cpp file, and place it in the TB directory
  # (user should not edit this file, though)
  if genTclTb (tb_src_dir, dut_ifc, BsvDUT, usertb):
    # error message will have been displayed
    return 1

  # The template files will refer to the DutXactor and C CAPI.
  # If those files haven't already been created, create it here.
  #
  if genDutXactor (cpp_src_dir, dut_ifc, BsvDUT):
    # error message will have been displayed
    return 1

  return 0

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main ())
