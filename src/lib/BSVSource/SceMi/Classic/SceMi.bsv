// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMi;

import SceMiCore::*;
import SceMiXactors::*;
import SceMiLockStepXactors:: *;
import SceMiProxies::*;
import SimTbProxies::*;
import SceMiSerialProbe::*;
import SceMiSharedMemory::*;
import SceMiScan::*;
import ModuleContext::*;
import SceMiMemoryXactor :: *;

export SceMiCore::*;
export SceMiXactors::*;
export SceMiLockStepXactors:: *;
export SceMiProxies::*;
export SimTbProxies::*;
export SceMiSerialProbe::*;
export SceMiSharedMemory::*;
export SceMiScan::*;
export ModuleContext::*;
export SceMiMemoryXactor :: *;

export scemiBlueNoC;
export scemiClassic;

Bool   scemiBlueNoC      = False;
Bool   scemiClassic      = True;

endpackage: SceMi
