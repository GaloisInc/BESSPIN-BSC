-- Data types for representing the history of benchmarks

module History where

import Time

type QORHistory = History SynResults

type History a = [(CalendarTime, a)]

type BatchID = Int

data Batch a = BRun BatchID BatchType [(String, a)]
             | BError BatchID String
             | BComment BatchID String
  deriving (Eq, Show, Read)

data BatchType = ETAutomatic
               | ETManual CompilerVersion 
  deriving (Eq, Show, Read)

data CompilerVersion = CVHead
                     | CVRevision Int
                     | CVCustom 
                     | CVLast
  deriving (Eq, Show, Read)

data SynResults = SynResults { rSlack :: Integer -- in femtoseconds
                             , rArea :: Integer -- in um^2  
