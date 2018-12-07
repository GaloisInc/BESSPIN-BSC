// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package ICapFSMCreate;

import ICapFSMDefines::*;
import ICap::*;
import StmtFSM::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module createICapFSM (Empty);
 
   Bit#(32) dummy = 32'hFFFFFFFF;
   Bit#(32) sync  = 32'hAA995566;
   
   // command codes
   Bit#(32) rcfg_cmd = 32'h00000004;
   Bit#(32) gcap_cmd = 32'h0000000C;
   
   // configuration registers
   
   Bit#(14) far_reg  = 'h1;
   Bit#(14) fdro_reg = 'h3;
   Bit#(14) cmd_reg  = 'h4;
   Bit#(14) ctl_reg  = 'h5;
   Bit#(14) id_reg   = 'hC;
   
   Reg#(Bit#(7))  addr <- mkReg(0);
   
   Action noOpAction = (action
			ICapInputs obj = ?;
			obj.csb    = False;
			obj.rdwr   = False; 
			obj.datain = tagged Direct pack(noOp);
			$display("%h // addr = X%h ", pack(obj), addr, fshow(obj), " (noOp)");
			addr <= addr + 1;
			endaction);
   
   function Stmt sub_pre ();
      seq
	 action
	    ICapInputs obj = ?;
	    obj.csb    = True;
	    obj.rdwr   = False; // change to WRITE;
	    obj.datain = tagged Direct 0;
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj),  " (change to WRITE)");
	    addr <= addr + 1;
	 endaction
	 action
	    ICapInputs obj = ?;
	    obj.csb    = False;
	    obj.rdwr   = False; 
	    obj.datain = tagged Direct 0;
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj));
	    addr <= addr + 1;
	 endaction
	 action
	    ICapInputs obj = ?;
	    obj.csb    = False;
	    obj.rdwr   = False; 
	    obj.datain = tagged Direct dummy;
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj), " (DUMMY)");
	    addr <= addr + 1;
	 endaction
	 action
	    ICapInputs obj = ?;
	    obj.csb    = False;
	    obj.rdwr   = False; 
	    obj.datain = tagged Direct sync;
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj), " (SYNC)");
	    addr <= addr + 1;
	 endaction
      endseq;
   endfunction
   
   function Stmt sub_post ();
      seq
	 action
	    ICapInputs obj = ?;
	    obj.csb    = True;
	    obj.rdwr   = False; 
	    obj.datain = tagged Direct 0;
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj));
	    addr <= addr + 1;
	 endaction
	 action
	    ICapInputs obj = ?;
	    obj.csb    = True;
	    obj.rdwr   = True; // change to READ;
	    obj.datain = tagged Direct 0;
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj),  " (change to READ)");
	    addr <= addr + 1;
	 endaction
	 action
	    ICapInputs obj = ?;
	    obj.csb    = True;
	    obj.rdwr   = True; // change to READ;
	    obj.datain = tagged Direct 0;
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj));
	    addr <= addr + 1;
	 endaction
      endseq;
   endfunction
   
   
   function Stmt sub_write(Bit#(14) reg_addr, ICapData value);
      seq
	 noOpAction;
      	 action
	    ICapInputs obj = ?;
	    obj.csb    = False;
	    obj.rdwr   = False; 
	    obj.datain = tagged Direct pack(writeOp(reg_addr, 1));
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj), " (Write to X%h)", reg_addr);
	    addr <= addr + 1;
	 endaction
	 action
	    ICapInputs obj = ?;
	    obj.csb    = False;
	    obj.rdwr   = False; 
	    obj.datain = value;
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj));
	    addr <= addr + 1;
	 endaction
	 noOpAction;
      endseq;
   endfunction

   function Stmt sub_read(Bit#(14) reg_addr, Bit#(11) cnt);
      seq
	 noOpAction;
	 action
	    ICapInputs obj = ?;
	    obj.csb    = False;
	    obj.rdwr   = False; 
	    obj.datain = tagged Direct pack(readOp(reg_addr, cnt));
	    $display("%h // addr = X%h ", pack(obj), addr, fshow(obj), " (Read from X%h)", reg_addr);
	    addr <= addr + 1;
	 endaction
	 noOpAction;
	 
      endseq;
   endfunction
   
   Stmt s_icap = (seq
		     action
			ICapInputs obj = ?;
			obj.csb    = True;
			obj.rdwr   = True; 
			obj.datain = tagged Direct 0;
			$display("%h // addr = X%h ", pack(obj), addr, fshow(obj), " (default value)");
			addr <= addr + 1;
		     endaction
		     $display("// Code: Bit#(7) read_ctl_first  = 'h%h;", addr);
		     sub_pre;
		     noOpAction;
		     sub_read(ctl_reg, 1);
		     $display("// Code: Bit#(7) read_ctl_last   = 'h%h;", addr - 1);
      		     $display("// Code: Bit#(7) write_ctl_first = 'h%h;", addr);
      		     sub_pre;
		     noOpAction;
		     sub_write(ctl_reg, tagged Reg);
      		     $display("// Code: Bit#(7) write_ctl_last  = 'h%h;", addr - 1);
      		     $display("// Code: Bit#(7) capture_first   = 'h%h;", addr);
      		     sub_pre;
		     noOpAction;
		     sub_write(cmd_reg, tagged Direct gcap_cmd);
      		     $display("// Code: Bit#(7) capture_last    = 'h%h;", addr - 1);
      		     $display("// Code: Bit#(7) read_first      = 'h%h;", addr);
      		     sub_pre;
		     noOpAction;
		     sub_write(cmd_reg, tagged Direct rcfg_cmd);
		     sub_write(far_reg, tagged Reg);
		     sub_read(fdro_reg, 163);
      		     $display("// Code: Bit#(7) read_last       = 'h%h;", addr - 1);
            	     $display("// Code: Bit#(7) post_first      = 'h%h;", addr);
      		     noOpAction;
		     sub_post;
            	     $display("// Code: Bit#(7) post_last       = 'h%h;", addr - 1);
		  endseq);
   
   let fsm <- mkAutoFSM(s_icap);
   
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function FPacket noOp();
   let packet = tagged One (FHeader1 { op: NOOP, addr: 0, reserved: 0, count: 0});
   return packet;
endfunction

function FPacket readOp(Bit#(14) addr, Bit#(11) count);
   let packet = tagged One (FHeader1 { op: Read, addr: addr, reserved: 0, count: count});
   return packet;
endfunction

function FPacket writeOp(Bit#(14) addr, Bit#(11) count);
   let packet = tagged One (FHeader1 { op: Write, addr: addr, reserved: 0, count: count});
   return packet;
endfunction

endpackage
