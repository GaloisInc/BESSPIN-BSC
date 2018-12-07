////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxPCIE.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxPCIE;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import XilinxVirtex5PCIE :: *;
import XilinxVirtex6PCIE :: *;
import XilinxKintex7PCIE :: *;
import XilinxVirtex7PCIE :: *;
import XilinxVirtexUltraScalePCIE :: *;
import PCIE              :: *;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////
export XilinxVirtex5PCIE :: *;
export XilinxVirtex6PCIE :: *;
export XilinxKintex7PCIE :: *;
export XilinxVirtex7PCIE :: *;
export XilinxVirtexUltraScalePCIE :: *;
export PCIE              :: *;

endpackage: XilinxPCIE

