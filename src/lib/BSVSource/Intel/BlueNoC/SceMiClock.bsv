package SceMiClock;

import ArriaIOPll::*;
import DefaultValue::*;

import "BVI" vArriaSceMiClock =
module mkSceMiClock#(parameter Integer outClockPeriod)(IOPll);
   default_clock clk(CLK);
   default_reset rstn(RST_N);

   let hi = (outClockPeriod+1)/2;
   let period = hi*2;

   parameter hi = hi;
   parameter lo = hi;
   parameter outClockPeriod = integerToString(period) + " ns";

   output_clock clkout(clkout);
   method locked locked;
   schedule locked CF locked;
endmodule

endpackage
