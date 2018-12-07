module Main where

import BatchResult

main = parseAllBatches >>= storeDatabase
