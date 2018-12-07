#include <iostream>
#include <fstream>
#include <cstdlib>
#include "Design.hpp"

extern int vchdebug;

int main(int argc, char ** argv)
{
    Design design;

    if (argc < 5)
    {
        std::cerr << std::endl << "usage:  design design.vvp design.ll design.edf design.synth.log" << std::endl << std::endl;
        return 1;
    } else {
      design.parse_edf(argv[3]);
      design.parse_vvp(argv[1]);
      design.parse_ll(argv[2]);
      design.parse_log(argv[4]);

      Signal * signal = design.findSignal("/mkBridge/scemi_dut_dutIfc/dmac/ifc_cnfReqF/D_OUT");

      if (signal == NULL) {
	printf("FFFFFF No signal found !\n");
	return 0;
      }

      design.enableNet(signal->getNet());
      //      design.updateMode();
      design.disableNet(signal->getNet());
      return 0;
    }
}
