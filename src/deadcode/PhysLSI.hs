module PhysLSI(physLSI) where
import Prim
import APhysical

physLSI :: Physical
physLSI = Physical { pSize = lSize, pFanin = lFanin, pDelay = lDelay }

lSize :: PrimOp -> Integer -> Int -> PSize
lSize _ s _ = fromInteger s

lFanin :: PrimOp -> Integer -> Int -> Int -> PFanout
lFanin _ _ _ _ = 1

lDelay :: PrimOp -> Integer -> PFanout -> PSize -> PDelay
lDelay _ _ _ _ = 1
