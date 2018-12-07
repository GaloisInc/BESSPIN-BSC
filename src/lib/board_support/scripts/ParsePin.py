#!/usr/bin/python

import sys
import os
import locale

_debug = 0

# ================================================================
# Exception for pin file errors

class PinFileError (Exception):
  def __init__ (self, message, filepos):
    (file_name, line_num, line) = filepos
    self.message = message
    self.file_name = file_name
    self.line_num = line_num
    self.line = line

# ================================================================
# Classes to represent transactor interfaces

# ----------------
class Xactor_Clock:
  def __init__ (self, v_name, bsv_name):
    self.verilog_name = v_name
    self.bsv_name = bsv_name
  def show (self, indent):
    print "%sClock verilog_name: %s  bsv_name: %s" % (indent, self.verilog_name, self.bsv_name)

# ----------------
class Xactor_Reset:
  def __init__ (self, v_name, bsv_name):
    self.verilog_name = v_name
    self.bsv_name = bsv_name
  def show (self, indent):
    print "%sReset verilog_name: %s  bsv_name: %s" % (indent, self.verilog_name, self.bsv_name)

# ----------------------------------------------------------------
# This is a 'dummy' superclass for all the Xactor_xxx_IFC types that
# follow, to assist in quick typechecking.

class Xactor_IFC:
  pass

# ----------------
# Raw inputs using Port transactors (OBSOLETE)

class Xactor_Raw_In_IFC (Xactor_IFC):
  def __init__ (self):
    self.clock = None               # => string
    self.field_width  = None        # int

  proto_type = 'Raw_In_IFC'

  def show (self, indent):
    print "%sfield_width: %d" % (indent, self.field_width)

# ----------------
# Raw inputs using Pipe transactors

class Xactor_PIPE_IN_IFC (Xactor_IFC):
  def __init__ (self):
    self.clock = None               # => string
    self.field_width  = None        # int

  proto_type = 'PIPE_IN_IFC'

  def show (self, indent):
    print "%sfield_width: %d" % (indent, self.field_width)

# ----------------
# Raw outputs using Port transactors (OBSOLETE)

class Xactor_Raw_Out_IFC (Xactor_IFC):
  def __init__ (self):
    self.clock = None               # => string
    self.field_width  = None        # int

  proto_type = 'Raw_Out_IFC'

  def show (self, indent):
    print "%sfield_width: %d" % (indent, self.field_width)

# ----------------
# Raw outputs using Pipe transactors

class Xactor_PIPE_OUT_IFC (Xactor_IFC):
  def __init__ (self):
    self.clock = None               # => string
    self.field_width  = None        # int

  proto_type = 'PIPE_OUT_IFC'

  def show (self, indent):
    print "%sfield_width: %d" % (indent, self.field_width)

# ----------------
# Handshake interfaces are GET/PIPEGET, PUT/PIPEPUT
# (Non-pipe versions are OBSOLETE)

class Xactor_Handshake_IFC (Xactor_IFC):
  def __init__ (self):
    self.clock = None               # => string
    self.en  = None                # => string
    self.rdy = None                # => string
    self.verilog_names = []        # [string]
    self.field_widths = []         # [string]

  def show (self, indent):
    print "%sen: %s" % (indent, self.en)
    print "%srdy: %s" % (indent, self.rdy)
    for j in range (len (self.verilog_names)):
      print "%sverilog_name: %s    field_width: %d" % (indent, self.verilog_names [j], self.field_widths [j])

class Xactor_GET_IFC (Xactor_Handshake_IFC):
  def __init__ (self):
    Xactor_Handshake_IFC.__init__ (self)
  proto_type = 'GET_IFC'

class Xactor_PIPEGET_IFC (Xactor_Handshake_IFC):
  def __init__ (self):
    Xactor_Handshake_IFC.__init__ (self)
  proto_type = 'PIPEGET_IFC'

class Xactor_PUT_IFC (Xactor_Handshake_IFC):
  def __init__ (self):
    Xactor_Handshake_IFC.__init__ (self)
  proto_type = 'PUT_IFC'

class Xactor_PIPEPUT_IFC (Xactor_Handshake_IFC):
  def __init__ (self):
    Xactor_Handshake_IFC.__init__ (self)
  proto_type = 'PIPEPUT_IFC'

# ----------------
# Memory transactor

class Xactor_MEM_IFC (Xactor_Handshake_IFC):
  def __init__ (self):
    self.clock = None                         # => string
    self.address_width = None                 # => int
    self.data_width = None                    # => int
    self.req_en  = None                       # => string
    self.req_rdy = None                       # => string
    self.req_addr_data_verilog_name = None    # => string
    self.resp_en  = None                      # => string
    self.resp_rdy = None                      # => string
    self.resp_data_verilog_name = None        # => string
  proto_type = 'MEM_IFC'

  def show (self, indent):
    print "%sclock: %s" % (indent, self.clock)
    print "%saddress_width: %s" % (indent, self.address_width)
    print "%sdata_width: %s" % (indent, self.data_width)
    print "%sreq_en: %s" % (indent, self.req_en)
    print "%sreq_rdy: %s" % (indent, self.req_rdy)
    print "%sreq_addr_data_verilog_name: %s" % (indent, self.req_addr_data_verilog_name)
    print "%sresp_en: %s" % (indent, self.resp_en)
    print "%sresp_rdy: %s" % (indent, self.resp_rdy)
    print "%sresp_data_verilog_name: %s" % (indent, self.resp_data_verilog_name)

# ----------------
# Lockstep inputs

class Xactor_Lockstep_In_IFC (Xactor_IFC):
  def __init__ (self):
    self.clock = None               # => string
    self.field_width  = None        # int

  proto_type = 'Lockstep_In_IFC'

  def show (self, indent):
    print "%sfield_width: %d" % (indent, self.field_width)

# ----------------
# Lockstep outputs

class Xactor_Lockstep_Out_IFC (Xactor_IFC):
  def __init__ (self):
    self.clock = None               # => string
    self.field_width  = None        # int

  proto_type = 'Lockstep_Out_IFC'

  def show (self, indent):
    print "%sfield_width: %d" % (indent, self.field_width)

# ================================================================
# The full DUT interface

class DUT_IFC:
  def __init__ (self):
    self.clock = None    # => Xactor_Clock
    self.reset = None    # => Xactor_Reset
    self.xactors = {}    # map from bsv_ifc_name -> Xactor_IFC

  # This could be member value that is set when 'insert_mem' is called
  def expects_memory (self):
    for xactor_name in self.xactors:
      xactor = self.xactors [xactor_name]
      if isinstance (xactor, Xactor_MEM_IFC):
        return True
    return False

  # This could be member value that is set when 'insert_mem' is called
  def is_lockstep (self):
    for xactor_name in self.xactors:
      xactor = self.xactors [xactor_name]
      if isinstance (xactor, Xactor_Lockstep_In_IFC) or isinstance (xactor, Xactor_Lockstep_Out_IFC):
        return True
    return False

  def insert_mem_widths (self, filepos, bsv_ifc_name, address_width_s, data_width_s):
    if not address_width_s.isdigit():
      raise PinFileError ("Mem address width %s is not a decimal number" % address_width_s, filepos)
    if not data_width_s.isdigit():
      raise PinFileError ("Mem data width %s is not a decimal number" % data_width_s, filepos)
    address_width = locale.atoi (address_width_s)
    data_width = locale.atoi (data_width_s)
    
    if bsv_ifc_name not in self.xactors:    xactor = Xactor_MEM_IFC ()
    else:                                   xactor = self.xactors [bsv_ifc_name]

    if xactor.address_width != None:    raise PinFileError ("Duplicate mem address width", filepos)
    xactor.address_width = address_width

    if xactor.data_width != None:    raise PinFileError ("Duplicate mem data width", filepos)
    xactor.data_width = data_width

  def insert_mem (self, filepos, direction, v_name, width_s, bsv_clock, proto_part, proto_type, bsv_ifc_name):
    if not width_s.isdigit():
      raise PinFileError ("Data signal width is not a decimal number", filepos)
    width = locale.atoi (width_s)
    if (proto_part == 'EN' or proto_part == 'RDY') and width != 1:
      raise PinFileError ("EN or RDY width is not 1", filepos)
    if proto_part == 'DATA' and width < 0:
      raise PinFileError ("DATA field width is negative", filepos)

    if proto_part == 'EN' and direction != 'input':
      raise PinFileError ("EN direction is not input", filepos)
    elif proto_part == 'RDY' and direction != 'output':
      raise PinFileError ("RDY direction is not output", filepos)
    elif proto_part == 'DATA' and direction != 'output' and proto_type == 'MEMREQ':
      raise PinFileError ("MEMREQ data direction is not output", filepos)
    elif proto_part == 'DATA' and direction != 'input'  and proto_type == 'MEMRESP':
      raise PinFileError ("MEMRESP data direction is not input", filepos)

    if bsv_ifc_name not in self.xactors:    xactor = Xactor_MEM_IFC ()
    else:                                   xactor = self.xactors [bsv_ifc_name]

    if not isinstance (xactor, Xactor_MEM_IFC):
      raise PinFileError ("MEMREQ/RESP field being added to transactor not previously spec'd as MEM_IFC", filepos)

    if xactor.clock is None:
      xactor.clock = bsv_clock
    elif (xactor.clock != bsv_clock):
      raise PinFileError ("Transactor part's clock inconsistent with previous spec", filepos)

    if proto_type == 'MEMREQ':
      if proto_part == 'DATA':
        if xactor.req_addr_data_verilog_name != None:
          raise PinFileError ("Duplicate Verilog port name", filepos)
        xactor.req_addr_data_verilog_name = v_name
      elif proto_part == 'EN':
        if (xactor.req_en != None):
          raise PinFileError ("Duplicate EN signal for MEMREQ", filepos)
        xactor.req_en = v_name
      elif proto_part == 'RDY':
        if (xactor.req_rdy != None):
          raise PinFileError ("Duplicate RDY signal for MEMREQ", filepos)
        xactor.req_rdy = v_name
      else:
        raise PinFileError ("Unknown proto_part for MEMREQ (expecting DATA/EN/RDY)", filepos)
    else:
      assert proto_type == 'MEMRESP'
      if proto_part == 'DATA':
        if xactor.resp_data_verilog_name != None:
          raise PinFileError ("Duplicate Verilog port name", filepos)
        xactor.resp_data_verilog_name = v_name
      elif proto_part == 'EN':
        if (xactor.resp_en != None):
          raise PinFileError ("Duplicate EN signal for MEMRESP", filepos)
        xactor.resp_en = v_name
      elif proto_part == 'RDY':
        if (xactor.resp_rdy != None):
          raise PinFileError ("Duplicate RDY signal for MEMRESP", filepos)
        xactor.resp_rdy = v_name
      else:
        raise PinFileError ("Unknown proto_part for MEMRESP (expecting DATA/EN/RDY)", filepos)

    self.xactors [bsv_ifc_name] = xactor

  def insert_handshake (self, filepos, direction, v_name, width_s, bsv_clock, proto_part, proto_type, bsv_ifc_name):
    if not width_s.isdigit():
      raise PinFileError ("Data signal width is not a decimal number", filepos)
    width = locale.atoi (width_s)
    if (proto_part == 'EN' or proto_part == 'RDY') and width != 1:
      raise PinFileError ("EN or RDY width is not 1", filepos)
    if proto_part == 'DATA' and width < 0:
      raise PinFileError ("DATA field width is negative", filepos)

    if proto_part == 'EN' and direction != 'input':
      raise PinFileError ("EN direction is not input", filepos)
    elif proto_part == 'RDY' and direction != 'output':
      raise PinFileError ("RDY direction is not output", filepos)
    elif proto_part == 'DATA' and direction != 'output' and proto_type in ['GET', 'PIPEGET']:
      raise PinFileError ("GET/PIPEGET data direction is not output", filepos)
    elif proto_part == 'DATA' and direction != 'input'  and proto_type in ['PUT', 'PIPEPUT']:
      raise PinFileError ("PUT/PIPEPUT data direction is not input", filepos)

    if bsv_ifc_name not in self.xactors:
      if proto_type == 'GET':        xactor = Xactor_GET_IFC ()
      elif proto_type == 'PIPEGET':  xactor = Xactor_PIPEGET_IFC ()
      elif proto_type == 'PUT':      xactor = Xactor_PUT_IFC ()
      elif proto_type == 'PIPEPUT':  xactor = Xactor_PIPEPUT_IFC ()
      else: assert 0, "insert_handshake proto_type %s is not recognized" % proto_type
    else:
      xactor = self.xactors [bsv_ifc_name]

    if proto_type == 'GET' and not isinstance (xactor, Xactor_GET_IFC):
      raise PinFileError ("GET field being added to transactor not previously spec'd as GET", filepos)
    elif proto_type == 'PIPEGET' and not isinstance (xactor, Xactor_PIPEGET_IFC):
      raise PinFileError ("PIPEGET field being added to transactor not previously spec'd as PIPEGET", filepos)
    elif proto_type == 'PUT' and not isinstance (xactor, Xactor_PUT_IFC):
      raise PinFileError ("PUT field being added to transactor not previously spec'd as PUT", filepos)
    elif proto_type == 'PIPEPUT' and not isinstance (xactor, Xactor_PIPEPUT_IFC):
      raise PinFileError ("PIPEPUT field being added to transactor not previously spec'd as PIPEPUT", filepos)

    if xactor.clock is None:
      xactor.clock = bsv_clock
    elif (xactor.clock != bsv_clock):
      raise PinFileError ("Transactor part's clock inconsistent with previous spec", filepos)

    if proto_part == 'DATA':
      if (v_name in xactor.verilog_names):
        raise PinFileError ("Duplicate Verilog port name", filepos)
      xactor.verilog_names.append (v_name)
      xactor.field_widths.append (width)
    elif proto_part == 'EN':
      if (xactor.en != None):    raise PinFileError ("Duplicate EN signal for handshake interface", filepos)
      xactor.en = v_name
    elif proto_part == 'RDY':
      if (xactor.rdy != None):    raise PinFileError ("Duplicate RDY signal for handshake interface", filepos)
      xactor.rdy = v_name
    self.xactors [bsv_ifc_name] = xactor

  def insert_raw_in_out (self, filepos, direction, v_name, width_s, bsv_clock):
    if not width_s.isdigit():
      raise PinFileError ("Data signal width is not a decimal number", filepos)
    width = locale.atoi (width_s)
    if v_name in self.xactors:
      raise PinFileError ("Raw_in/out port name duplicates earlier interface", filepos)
    if direction == "input":
      xactor = Xactor_Raw_In_IFC ()
    else:
      xactor = Xactor_Raw_Out_IFC ()
    xactor.field_width = width
    xactor.clock = bsv_clock
    self.xactors [v_name] = xactor

  def insert_pipe_in_out (self, filepos, direction, v_name, width_s, bsv_clock):
    if not width_s.isdigit():
      raise PinFileError ("Data signal width is not a decimal number", filepos)
    width = locale.atoi (width_s)
    if v_name in self.xactors:
      raise PinFileError ("Pipe_in/out port name duplicates earlier interface", filepos)

    if direction == "input":    xactor = Xactor_PIPE_IN_IFC ()
    else:                       xactor = Xactor_PIPE_OUT_IFC ()

    xactor.field_width = width
    xactor.clock = bsv_clock
    self.xactors [v_name] = xactor

  def insert_lockstep_in_out (self, filepos, direction, v_name, width_s, bsv_clock):
    if not width_s.isdigit():
      raise PinFileError ("Data signal width is not a decimal number", filepos)
    width = locale.atoi (width_s)
    if v_name in self.xactors:
      raise PinFileError ("Lockstep_in/out port name duplicates earlier interface", filepos)
    if direction == "input":
      xactor = Xactor_Lockstep_In_IFC ()
    else:
      xactor = Xactor_Lockstep_Out_IFC ()
    xactor.field_width = width
    xactor.clock = bsv_clock
    self.xactors [v_name] = xactor

  def check_clocks (self, filepos):
    if self.clock is None:    raise PinFileError ("No clock define", filepos)
    #if self.reset is None:    raise PinFileError ("No reset define", filepos)
    for xactor_name in self.xactors:
      xactor = self.xactors [xactor_name]
      if xactor.clock != self.clock.bsv_name:
        raise PinFileError ("Clock of transactor %s is different from the interface clock" % xactor_name, filepos)

  def check_lockstep (self, filepos):
    has_lockstep = False;
    has_other = False;
    for xactor_name in self.xactors:
      xactor = self.xactors [xactor_name]
      if isinstance (xactor, Xactor_Lockstep_In_IFC) or isinstance (xactor, Xactor_Lockstep_Out_IFC):
        if has_other:
          raise PinFileError ("Cannot mix lockstep transactors with port, handshake, or memory transactors", filepos)
        else:
          has_lockstep = True;
      else:
        if has_lockstep:
          raise PinFileError ("Cannot mix lockstep transactors with port, handshake, or memory transactors", filepos)
        else:
          has_other = True;

  def show (self, indent):
    print "%s%s" % (indent, "DUT_IFC:")
    if self.clock is None:    print "%s%s" % (indent, "  No clock")
    else:                     self.clock.show (indent + "  ")

    if self.reset is None:    print "%sNo reset" % indent
    else:                     self.reset.show (indent + "  ")

    if self.xactors == {}:    print "%s%s" % (indent, "  No transactors")
    else:
      for xactor_name in self.xactors:
        xactor = self.xactors [xactor_name]
        print "%s  %s:%s" % (indent, xactor_name, xactor.proto_type)
        xactor.show (indent + "    ")

# ================================================================
# parse_pin_file () parses the pin file and returns an 'interface'
# descriptor:
#    'clock' -> (signal_name, bsv_name)
#    'reset' -> (signal_name, bsv_name)
#    xactor_name -> (PUT/GET, clock, rdy_signal_name, ena_signal_name, [(data_signal_or_field_names, width)])
#    xactor_name -> (RAW_IN/RAW_OUT, clock, signal_name, width)
# Also does various syntactic and semantic checks on the input file

def parse_pin_file (fileobj, file_name):
  try:
    line_num = 0
    dut_ifc = DUT_IFC ();
    for line in fileobj:
      line_num += 1
      line = line.rstrip()    # remove trailing whitespace and newline
      filepos = (file_name, line_num, line)

      words = line.split()

      # ----------------
      # blank lines
      if (len (words) == 0):
        pass

      # ----------------
      # clock lines
      elif (words [0] == 'input') and (len (words) == 4) and words [-1].startswith ('clock:'):
        if _debug:    print "Input line (clock): %s:%d:%s" % filepos
        v_name = words [1]
        width_s = words [2]
        bsv_name = words [3][len ('clock:'):]
        if dut_ifc.clock is not None:    raise PinFileError ("Duplicate definition of clock", filepos)
        elif width_s != '1':             raise PinFileError ("Clock width is not 1", filepos)
        else:                            dut_ifc.clock = Xactor_Clock (v_name, bsv_name)

      # ----------------
      # reset lines
      elif (words [0] == 'input') and (len (words) == 4) and words [-1].startswith ('reset:'):
        if _debug:    print "Input line (reset): %s:%d:%s" % filepos
        v_name = words [1]
        width_s = words [2]
        bsv_name = words [3][len ('reset:'):]
        if dut_ifc.reset is not None:    raise PinFileError ("Duplicate definition of reset", filepos)
        elif width_s != '1':             raise PinFileError ("Reset width is not 1", filepos)
        else:                            dut_ifc.reset = Xactor_Reset (v_name, bsv_name)

      # ----------------
      # Handshake lines: GET/PUT (OBSOLETE), PIPEGET/PIPEPUT
      elif ((words [0] == 'input') or (words [0] == 'output')) \
           and (len (words) == 7) \
           and ((words [5] == 'GET') or (words [5] == 'PIPEGET')
                or (words [5] == 'PUT') or (words [5] == 'PIPEPUT')):
        if _debug:    print "Input line (GET/PIPEGET/PUT/PIPEPUT): %s:%d:%s" % filepos

        direction = words [0]
        v_name = words [1]
        width_s = words [2]
        bsc_clock = words [3]
        proto_part = words [4]    # DATA/EN/RDY
        proto_type = words [5]    # GET/PIPEGET/ PUT/PIPEPUT/ MEMREQ MEMRSP
        bsv_ifc_name = words [6]
        dut_ifc.insert_handshake (filepos, direction, v_name, width_s, bsc_clock, proto_part, proto_type, bsv_ifc_name)

      # ----------------
      # Memory interface lines: MEMREQ/MEMRESP
      elif ((words [0] == 'input') or (words [0] == 'output')) \
           and (len (words) == 7) \
           and ((words [5] == 'MEMREQ') or (words [5] == 'MEMRESP')):
        if _debug:    print "Input line (MEMREQ/MEMRESP): %s:%d:%s" % filepos

        direction = words [0]
        v_name = words [1]
        width_s = words [2]
        bsc_clock = words [3]
        proto_part = words [4]    # DATA/EN/RDY
        proto_type = words [5]    # MEMREQ MEMRSP
        bsv_ifc_name = words [6]
        dut_ifc.insert_mem (filepos, direction, v_name, width_s, bsc_clock, proto_part, proto_type, bsv_ifc_name)

      # ----------------
      # Raw input/output lines (OBSOLETE)
      elif ((words [0] == 'input') or (words [0] == 'output')) and (len (words) == 4):
        if _debug:    print "Input line (Raw_in/out): %s:%d:%s" % filepos
        direction = words [0]
        v_name = words [1]
        width_s = words [2]
        bsc_clock = words [3]
        dut_ifc.insert_raw_in_out (filepos, direction, v_name, width_s, bsc_clock)

      # ----------------
      # Raw input/output lines with PIPE
      elif ((words [0] == 'input') or (words [0] == 'output')) and (len (words) == 5) and (words [4] == 'PIPE'):
        if _debug:    print "Input line (Pipe_in/out): %s:%d:%s" % filepos
        direction = words [0]
        v_name = words [1]
        width_s = words [2]
        bsc_clock = words [3]
        dut_ifc.insert_pipe_in_out (filepos, direction, v_name, width_s, bsc_clock)

      # ----------------
      # Lockstep input/output lines
      elif ((words [0] == 'input') or (words [0] == 'output')) and (len (words) == 5) and (words [4] == 'LOCKSTEP'):
        if _debug:    print "Input line (Lockstep_in/out): %s:%d:%s" % filepos
        direction = words [0]
        v_name = words [1]
        width_s = words [2]
        bsc_clock = words [3]
        dut_ifc.insert_lockstep_in_out (filepos, direction, v_name, width_s, bsc_clock)

      # ----------------
      # Memory interface address and data width
      elif (len (words) == 3) and words [1].startswith ('address=') and words [2].startswith ('data='):
        bsv_ifc_name = words [0]
        address_width_s = words [1] [len ('address='):]
        data_width_s = words [2] [len ('data='):]
        dut_ifc.insert_mem_widths (filepos, bsv_ifc_name, address_width_s, data_width_s)

      # ----------------
      # ignored lines
      elif (words [0] == 'input') or (words [0] == 'output'):
        print "Ignoring file %s line %d: %s" % filepos
      elif _debug:
        print "Ignoring file %s line %d: %s" % filepos

    #end: for line in fileobj:

    dut_ifc.check_clocks ((file_name, line_num, "EOF"))
    dut_ifc.check_lockstep ((file_name, line_num, "EOF"))
    if _debug:
      dut_ifc.show ("  ")

    return dut_ifc

  except PinFileError, pfe:
    print "Pin file error:", pfe.message
    print "File %s Line %d: %s" % (pfe.file_name, pfe.line_num, pfe.line)
    print "Interface so far:"
    dut_ifc.show ("  ")
    return None

# ================================================================
# Test program when executed from the cmd line

def main (argv = None):
  if (len (sys.argv) != 2):
    print "%s  <pin_file_name>" % sys.argv [0]
    return 1

  file_name = sys.argv [1]
  try:
    fileobj = open (file_name, "r")
  except:
    print "Error opening file: ", file_name
    return 1

  dut_ifc = parse_pin_file (fileobj, file_name)
  if isinstance (dut_ifc, DUT_IFC):
    dut_ifc.show ("  ")
  return 0


# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit(main())
