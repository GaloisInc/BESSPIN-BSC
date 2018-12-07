////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : Xilinx10GE.bsv
//  Description   : 10GE PCS/PMA core with XGMII.
////////////////////////////////////////////////////////////////////////////////
package Xilinx10GE;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Clocks            ::*;
import I2C               ::*;
import DefaultValue      ::*;
import GetPut            ::*;
import Connectable       ::*;
import BUtils            ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////
export XGMII(..);
export SFP_Pins(..);
export XGE(..);

export mkXGMIITransceiver;
export XGMIIData(..);

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef struct {
   Bit#(8)     ctrl;
   Bit#(64)    data;
} XGMIIData deriving (Bits, Eq);   

instance DefaultValue#(XGMIIData);
   defaultValue = XGMIIData {
      ctrl:   '1,
      data:   duplicate(8'h07)
      };
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_enabled, always_ready *)
interface XGMII;
   interface Clock                     clk;
   interface Get#(XGMIIData)           ingress;
   interface Put#(XGMIIData)           egress;
endinterface


(* always_enabled, always_ready *)
interface SFP_Pins;
   (* prefix = "" *)
   method    Action              rxn((* port = "RXN" *)Bit#(1) i);
   (* prefix = "" *)
   method    Action              rxp((* port = "RXP" *)Bit#(1) i);
   (* prefix = "", result = "TXN" *)
   method    Bit#(1)             txn();
   (* prefix = "", result = "TXP" *)
   method    Bit#(1)             txp();
   (* prefix = "", result = "TXDIS" *)
   method    Bit#(1)             tx_disable();
   (* prefix = "" *)
   method    Action              los((* port = "LOS" *)Bool i);
   (* prefix = "" *)
   method    Action              tx_fault((* port = "TXFAULT" *)Bit#(1) i);
endinterface

interface XGE;
   (* prefix = "" *)
   interface SFP_Pins     sfp;
   interface XGMII        xgmii;
endinterface

(* always_enabled, always_ready *)
interface VXGMII;
   method    Bit#(8)     rxc;
   method    Bit#(64)    rxd;
   interface Clock       clk;
   method    Action      txc(Bit#(8) i);
   method    Action      txd(Bit#(64) i);
endinterface

(* always_enabled, always_ready *)
interface VSFP;
   method    Action              rxn(Bit#(1) i);
   method    Action              rxp(Bit#(1) i);
   method    Bit#(1)             txn();
   method    Bit#(1)             txp();
   method    Action              signal_detect(Bool i);
   method    Action              tx_fault(Bit#(1) i);
   method    Bit#(1)             tx_disable();
   method    Action              set_pma_link_status(Bool i);
   method    Action              set_pcs_link_status(Bool i);
endinterface

interface VXGE;
   (* prefix = "" *)
   interface VSFP         sfp;
   interface VXGMII       xgmii;
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
`define IMPORT_BVI_SFP(NUM) \
import "BVI" sfp``NUM``_example_design =\
module vMkVirtex6HXT_10GE_PCS_PMA_``NUM``#(Clock refclk_p, Clock refclk_n)(VXGE);\
   let reset_i <- invertCurrentReset;\
\
   default_clock clk(dclk);\
   default_reset rstn(reset) = reset_i;\
   \
   input_clock refclk_p(refclk_p) = refclk_p;\
   input_clock refclk_n(refclk_n) = refclk_n;\
   \
   port pma_loopback       	   = (Bit#(1)'(0));\
   port pma_reset          	   = (Bit#(1)'(0));\
   port global_tx_disable  	   = (Bit#(1)'(0));\
   port pma_vs_loopback    	   = (Bit#(4)'(0));\
   port pcs_loopback       	   = (Bit#(1)'(0));\
   port pcs_reset          	   = (Bit#(1)'(0));\
   port test_patt_a        	   = (Bit#(58)'(0));\
   port test_patt_b        	   = (Bit#(58)'(0));\
   port data_patt_sel      	   = (Bit#(1)'(0));\
   port test_patt_sel      	   = (Bit#(1)'(0));\
   port rx_test_patt_en    	   = (Bit#(1)'(0));\
   port tx_test_patt_en    	   = (Bit#(1)'(0));\
   port prbs31_tx_en       	   = (Bit#(1)'(0));\
   port prbs31_rx_en               = (Bit#(1)'(0));\
   port pcs_vs_loopback            = (Bit#(2)'(0));\
   port clear_pcs_status2          = (Bit#(1)'(0));\
   port clear_test_patt_err_count  = (Bit#(1)'(0));\
   \
   interface VSFP sfp;\
      method                         rxn(rxn) enable((*inhigh*)en8) clocked_by(refclk_p) reset_by(no_reset);\
      method                         rxp(rxp) enable((*inhigh*)en9) clocked_by(refclk_p) reset_by(no_reset);\
      method txn                     txn clocked_by(no_clock) reset_by(no_reset);\
      method txp                     txp clocked_by(no_clock) reset_by(no_reset);\
      method                         signal_detect(signal_detect) enable((*inhigh*)en10) clocked_by(refclk_p) reset_by(no_reset);\
      method                         tx_fault(tx_fault) enable((*inhigh*)en11) clocked_by(refclk_p) reset_by(no_reset);\
      method tx_disable              tx_disable clocked_by(no_clock) reset_by(no_reset);\
      method                         set_pma_link_status(set_pma_link_status) enable((*inhigh*)en12) clocked_by(refclk_p) reset_by(no_reset);\
      method                         set_pcs_link_status(set_pcs_link_status) enable((*inhigh*)en13) clocked_by(refclk_p) reset_by(no_reset);\
   endinterface   \
   \
   interface VXGMII xgmii;\
      output_clock     clk(core_clk156_out);\
      method xgmii_rxd rxd                                  clocked_by(xgmii_clk) reset_by(no_reset);\
      method xgmii_rxc rxc                                  clocked_by(xgmii_clk) reset_by(no_reset);\
      method           txd(xgmii_txd) enable((*inhigh*)en2) clocked_by(xgmii_clk) reset_by(no_reset);\
      method           txc(xgmii_txc) enable((*inhigh*)en3) clocked_by(xgmii_clk) reset_by(no_reset);\
   endinterface\
   \
   schedule (sfp_signal_detect, sfp_tx_fault, sfp_tx_disable, sfp_set_pma_link_status, sfp_set_pcs_link_status, sfp_rxn, sfp_rxp, sfp_txn, sfp_txp) CF (sfp_signal_detect, sfp_tx_fault, sfp_tx_disable, sfp_set_pma_link_status, sfp_set_pcs_link_status, \
	     sfp_tx_disable, sfp_rxn, sfp_rxp, sfp_txn, sfp_txp);\
\
   schedule (xgmii_rxd, xgmii_rxc) CF (xgmii_txd, xgmii_txc);\
   schedule (xgmii_txd) C (xgmii_txd);\
   schedule (xgmii_txc) C (xgmii_txc);\
   schedule (xgmii_txd) CF (xgmii_txc);\
   schedule (xgmii_rxd, xgmii_rxc) CF (xgmii_rxd, xgmii_rxc);\
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
`IMPORT_BVI_SFP(0)

`IMPORT_BVI_SFP(1)

`IMPORT_BVI_SFP(2)

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
module mkXGMIITransceiver#(Integer index, Clock sfp_refclk_p, Clock sfp_refclk_n)(XGE);
   
   if (index < 0 || index > 2) error("Only transceivers 0, 1, and 2 are implemented");
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   VXGE                            _transceiver;
   
   if      (index == 0) _transceiver <- vMkVirtex6HXT_10GE_PCS_PMA_0(sfp_refclk_p, sfp_refclk_n);
   else if (index == 1) _transceiver <- vMkVirtex6HXT_10GE_PCS_PMA_1(sfp_refclk_p, sfp_refclk_n);
   else                 _transceiver <- vMkVirtex6HXT_10GE_PCS_PMA_2(sfp_refclk_p, sfp_refclk_n);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface SFP_Pins sfp;
      method rxn(i) 	  = _transceiver.sfp.rxn(i);
      method rxp(i) 	  = _transceiver.sfp.rxp(i);
      method txn()  	  = _transceiver.sfp.txn;
      method txp()  	  = _transceiver.sfp.txp;
      method tx_disable() = _transceiver.sfp.tx_disable;
      method tx_fault(i)  = _transceiver.sfp.tx_fault(i);
      method Action los(i);
 	 _transceiver.sfp.signal_detect(!i);
 	 _transceiver.sfp.set_pma_link_status(!i);
 	 _transceiver.sfp.set_pcs_link_status(!i);
      endmethod
   endinterface
   
   interface XGMII xgmii;
      interface clk = _transceiver.xgmii.clk;
      interface Get ingress;
	 method ActionValue#(XGMIIData) get;
	    return XGMIIData { ctrl: _transceiver.xgmii.rxc(), data: _transceiver.xgmii.rxd() };
	 endmethod
      endinterface
      interface Put egress;
	 method Action put(XGMIIData i);
	    _transceiver.xgmii.txc(i.ctrl);
	    _transceiver.xgmii.txd(i.data);
	 endmethod
      endinterface
   endinterface	 
endmodule

endpackage: Xilinx10GE

