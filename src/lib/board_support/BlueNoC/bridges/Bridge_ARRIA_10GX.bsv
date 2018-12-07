import SceMi      :: *;
import SceMiLayer :: *;

// Setup for SCE-MI over PCIE to a Arria 10 GX

import Clocks ::*;
import DefaultValue ::*;
import TieOff       :: *;

import ArriaBVI         ::*;
import SceMiArria10PCIE ::*;

interface Arria10;
   (* prefix="PCIE" *)
   interface Pcie pcie;
   (* always_ready *)
   method    Bit#(8)      leds;
endinterface

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(  Clock ref_clk
		  , Reset pin_perst
		  )
                  (Arria10);

   SceMiA10PCIEArgs pcie_args;
   pcie_args.ref_clk = ref_clk;
   pcie_args.pin_perst = pin_perst;
   pcie_args.clock_period  = `SCEMI_CLOCK_PERIOD;
   pcie_args.link_type     = PCIE_ARRIA10;

   SceMiA10PCIEIfc#(Empty, 8) scemi <- buildSceMi(mkSceMiLayer, pcie_args);

   mkTieOff(scemi.noc_cont);

   rule drive_memory_calibration;
      scemi.isDDRReady(False); // DDR memory not activated by us
   endrule

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface pcie = scemi.pcie;

   method leds = zeroExtend({pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
                            });
endmodule: mkBridge
