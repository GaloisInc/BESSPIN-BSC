module IOMutVar(id_IOMutVar,
                MutableVar, newVar, readVar, writeVar) where

import Data.IORef

id_IOMutVar = "$Id$"

type MutableVar a = IORef a

newVar = newIORef
readVar = readIORef
writeVar = writeIORef
