// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsDefines;

import DefaultValue::*;
import SceMiDefines::*;

`include "TLM.defines"


// Configuration structure for Transactors

typedef struct {Bool      big_endian;
		Bool      keep_bursts;
		Bool      write_bypass;
                Integer   interleave_depth;
                UInt#(32) burst_limit;
                UInt#(32) max_flight;
		UInt#(32) reorder_depth;
		UInt#(32) flow_depth;
		UInt#(6)  xactor_id;
		String    xactor_comment;

                // Controls for the SceMi Layer
                Bool       usePipes;
                Integer    inPipeDepth;
                Visibility inPipeVis;
                Integer    outPipeDepth;
                Visibility outPipeVis;

                } TLMXActorArgs;


instance DefaultValue #(TLMXActorArgs);
   function defaultValue ();
      TLMXActorArgs arguments;
      arguments.big_endian     = False;
      arguments.keep_bursts    = True;
      arguments.write_bypass   = False;
      arguments.interleave_depth = 1;
      arguments.burst_limit    = 0;
      arguments.max_flight     = 5;
      arguments.reorder_depth  = 1;
      arguments.flow_depth     = 0;
      arguments.xactor_id      = 0;
      arguments.xactor_comment = "";
      //
      arguments.usePipes       = True;
      arguments.inPipeDepth    = 32;
      arguments.inPipeVis      = Fifo;
      arguments.outPipeDepth   = 32;
      arguments.outPipeVis     = Fifo;
      //
      return arguments;
   endfunction
endinstance



////////////////////////////////////////////////////////////////////////////////
/// Ahb
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
/// Apb
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
/// Axi
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
/// Axi4
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// Axi4L
////////////////////////////////////////////////////////////////////////////////



endpackage
