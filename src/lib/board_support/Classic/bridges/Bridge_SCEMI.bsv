import SceMi::*;
import SceMiLayer::*;

// Setup for SCE-MI over SCEMI

SceMiLinkType lt = `SCEMI_LT;

(* synthesize *)
module mkBridge ();

   Empty scemi <- buildSceMi(mkSceMiLayer, lt);

endmodule: mkBridge

