// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package ICapFSMDefines;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef union tagged {Bit#(32)        Direct;
		      void            Reg;
                      } ICapData      deriving(Eq, Bits, Bounded);

typedef struct {Bool     rdwr;
		Bool     csb;
		ICapData datain;
		} ICapInputs deriving (Eq, Bits, Bounded);

instance FShow#(ICapInputs);
   function Fmt fshow (ICapInputs obj);
      Fmt f = $format("<ICapInputs RDWR=%b CSB=%b", obj.rdwr, obj.csb);
      if (obj.datain matches tagged Direct .d)
	 f = f + $format(" %08h >", d);
      if (obj.datain matches tagged Reg)
	 f = f + $format(" REG >");
      return f;
   endfunction
endinstance

endpackage
