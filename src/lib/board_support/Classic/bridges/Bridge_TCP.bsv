import SceMi::*;
import SceMiLayer::*;

(* synthesize *)
module mkBridge ();

   Empty scemi <- buildSceMi(mkSceMiLayer, TCP);

endmodule: mkBridge
