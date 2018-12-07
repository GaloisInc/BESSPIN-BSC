import SceMi::*;
import SceMiLayer::*;

// Setup for SCE-MI over PCIE to a Dini board
import Dini::*;
import DiniPCIE::*;
import Clocks::*;
import DummyDriver::*;

SceMiLinkType lt = PCIE_DINI;

(* synthesize *)
module mkBridge #(Clock sample_clk, Reset sample_rstn, Clock g1_clk, Reset g1_rstn) (DN7406_FPGA_A);

   SceMiDiniPCIEArgs pcie_args;
   pcie_args.pci_sample_clk = sample_clk;
   pcie_args.pci_sample_rstn = sample_rstn;
   pcie_args.link_type = lt;
   SceMiDiniPCIEIfc#(Empty) scemi <- buildSceMi(mkSceMiLayer, pcie_args);

   RS232_Pins unused_rs232 <- mkStub();

   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface pcie  = scemi.pcie;
   interface rs232 = unused_rs232;
   method leds = zeroExtend({pack(_isClockAdvancing)
			    ,pack(_isOutOfReset)
			    });
endmodule: mkBridge
