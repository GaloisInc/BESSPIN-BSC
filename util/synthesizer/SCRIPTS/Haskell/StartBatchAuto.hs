#!/usr/bin/runhugs

-----------------------------------------------------------------------------
-- This script starts an automated batch. It is supposed to be in the      --
-- crontab of bluetinder, so that it's automatically started each Sunday   --
-- morning. Unlike StartBatchCGI.hs, this script does not use ssh to run   --
-- the batch at bluetinder, but assumes that it is already running on the  --
-- correct machine, and only invokes the batch script.                     --
-----------------------------------------------------------------------------

module Main where

import System
import Directory

import BatchConfig
import Design
import Utils(beginsWith)

main = do
  designs <- getAllDesigns

  let batchConfig = BatchConfig
         { bcCompiler = BCCHead False
         , bcDesigns = map dsgnName designs
         , bcReports = BCContinuous
         , bcReportsAddr = []
         , bcIsAuto = True
         }

  scriptFile <- batchScript batchConfig

  rslt <- system (scriptFile)

  let scriptFileRev = reverse scriptFile

  if scriptFileRev `beginsWith` (reverse "run-batch")
    then do let batchDir = reverse (drop (length "run-batch") scriptFileRev)
            finalRsltsExist <- doesFileExist (batchDir ++ "final_results.txt")
            if finalRsltsExist
              then return ()
              else do
                   writeFile (batchDir ++ "final_results.txt") $ unlines
                             ["FAIL: unknown"]

    else return ()
      

