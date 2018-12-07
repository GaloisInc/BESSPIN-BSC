-- This module contains datastructures to represent the results of a
-- batch (i.e. a set of synthesis runs, performed at a specific time).
--
-- The function parseBatch reads the output file from a batch (which
-- are usually found in e.g. ~synthesizer/DATABASE/BATCHES/42/) and
-- extracts the useful information. parseAllBatches does this for all
-- existing batches. In order to not have to read through all files
-- in all directories each time, it is possible to store the database
-- in a single file, with the storeDatabase and loadDatabase functions.

module BatchResult (
  BatchResult(..),
  BatchOutcome(..),
  DesignOutcome(..),
  
  getDesignNames,  -- [BatchResult] -> [String]

  parseBatchNr,    -- Int -> IO (Maybe BatchResults)
  parseBatch,      -- Filepath -> IO BatchResults
                   -- Given a directory containing results of a batch, parseBatch
                   -- reads all files and parses them

  parseAllBatches, -- IO [BatchResult]

  storeDatabase,   -- [BatchResult] -> IO ()
  loadDatabase,    -- IO [BatchResult]  (can throw exceptions on failure)

  avgSynthesisTimes, -- [BatchResult] -> [(String, Int)]
 ) where

import Directory
import IO
import Char(isDigit, toLower)
import List(nub,sortBy)

import Design
import Utils

data BatchResult = BatchResult { brDate :: String
                               , brBatchID :: Int
                               , brBSCVersion :: String
                               , brFinished :: Bool
                               , brOutcome :: BatchOutcome
                               , brIsAutomatic :: Bool
                               , brWarnings :: [String]
                               }
  deriving (Eq, Show, Read)

data BatchOutcome = BOSuccess [(Design,DesignOutcome)]
                  | BOFailure String
                  | BOComment String
  deriving (Eq, Show, Read)


data DesignOutcome = DOSuccess { dosSlack :: Double -- in ps
                               , dosArea :: Int -- in um^2
                               , dosCPUTime :: Int -- in seconds
                               }
                   | DOFailure String
   deriving (Eq, Show, Read)


-----------------------------------------------------------------------------

getDesignNames :: [BatchResult] -> [String]
getDesignNames = sortBy caseInsensitive . nub . concatMap gDN
  where
  gDN br = case brOutcome br of
             BOSuccess ds -> map (dsgnName.fst) ds
             _ -> []

  caseInsensitive str1 str2 = compare (map toLower str1) (map toLower str2)

-----------------------------------------------------------------------------
-- parseAllBatches

parseAllBatches :: IO [BatchResult]
parseAllBatches = do
  let batchesDir = "/export/home/synthesizer/DATABASE/BATCHES/"
  allBatchDirs <- getDirectoryContents batchesDir >>= 
                  return . map (batchesDir++) . filter (all isDigit)
  mapM parseBatch allBatchDirs
  
  

--------------------------------------------------------------------------------
-- parseBatch: reads through the files in a directory with the results
--             of one batch, and parses them
parseBatchNr :: Int -> IO (Maybe BatchResult)
parseBatchNr nr = tryMaybe (parseBatch ("/export/home/synthesizer/DATABASE/BATCHES/" ++ show nr))


parseBatch :: FilePath -> IO BatchResult
parseBatch dir' = do
  let dir = if dir' `endsWith` "/"
            then dir'
            else dir' ++ "/"
                      
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then return (BatchResult { brDate = ""
			     , brBatchID = (-1)
			     , brBSCVersion = ""
			     , brFinished = False
			     , brOutcome = BOComment ("Error: the batch result directory " 
                                                      ++ dir ++ " does not exist")
			     , brIsAutomatic = False
			     , brWarnings = []
			     })
    else do 
      allFilesInDir <- getDirectoryContents dir >>= return . filter (not . (`beginsWith` "."))

      configFile <- tryReadFile (dir ++ "configuration.txt") >>= 
                    return . parseConfigFile . maybe [] lines

      let dOutcomeFileNames = filter ((`beginsWith` "rslt_") &&& 
                                      (`endsWith` ".txt"))
                                     allFilesInDir

      dOutcomeFiles <- mapM (\f -> tryReadFileStrict (dir++f) >>= 
                             \cs -> return (reverse $ 
                                            drop 4 $ 
                                            reverse $ 
                                            drop 5 f,cs)) dOutcomeFileNames >>= 
                       return . map (second (\(Just a) -> a)) . filter ((Nothing/=).snd)

      let designOutcomes = map (second parseDesignOutcome) dOutcomeFiles

      let designs = matchDesigns (cfDesigns configFile) designOutcomes

      (finished, batchError) <- do contents <- tryReadFile (dir ++ "final_results.txt")
                                   return (case contents of
                                            Nothing -> (False, Nothing)
                                            Just str 
                                              | str `beginsWith` "OK" -> 
                                                   (True, Nothing)
                                              | str `beginsWith` "FAIL" -> 
                                                   (True, Just (head $ lines str))
                                              | otherwise -> 
                                                   (True, Just ("Unknown batch error"))
                                          )
       
      let batchOutcome = case batchError of
                           Nothing -> BOSuccess designs
                           Just err -> BOFailure err

      let batchIdStr = reverse $
                       takeWhile ('/'/=) $
                       drop 1 $
                       reverse $
                       dir

      let batchId = if all isDigit batchIdStr
                    then read batchIdStr
                    else (-1)
      
      let bscVersion = if (cfReportedBSCRev configFile) `beginsWith` "Revision: "
                       then drop 10 (cfReportedBSCRev configFile)
                       else cfRequestedBSCRev configFile
      
      return $ BatchResult { brDate = cfDate configFile
                           , brBatchID = batchId
                           , brBSCVersion = bscVersion
                           , brFinished = finished
                           , brOutcome = batchOutcome
                           , brIsAutomatic = cfIsAutomatic configFile
                           , brWarnings = []
                           }



-----------------------------------------------------------------------------
-- parseConfigFile

data ConfigFile = CF { cfDate :: String
                     , cfRequestedBSCRev :: String
                     , cfReportedBSCRev :: String
                     , cfIsAutomatic :: Bool
                     , cfDesigns :: [Design]
                     , cfParseErrors :: [String]
                     }
  deriving Show

parseConfigFile :: [String] -> ConfigFile
parseConfigFile = parseCF (CF "" "" "" False [] [])
  where
  parseCF acc [] = acc
  parseCF acc (s:ss)
     | s `beginsWith` "DATE: " = parseCF (acc {cfDate = drop 6 s}) ss
     | s `beginsWith` "BSC_REV: " = parseCF (acc {cfRequestedBSCRev = drop 9 s}) ss
     | s `beginsWith` "IS_AUTOMATIC: FALSE" = parseCF (acc {cfIsAutomatic = False}) ss
     | s `beginsWith` "IS_AUTOMATIC: TRUE" = parseCF (acc {cfIsAutomatic = True}) ss
     | s `beginsWith` "REPORTED_BSC_REV: " = parseCF (acc {cfReportedBSCRev = drop 18 s}) ss
     | s `beginsWith` "DESIGN_" = parseCF (updDesigns acc (drop 7 s)) ss
     | otherwise = parseCF (acc{cfParseErrors = cfParseErrors acc ++ [s]}) ss

  updDesigns cf s =let (field,rest1) = splitWhenT ('('==) s
                       (dName,rest2) = splitWhenT (')'==) rest1
                       value = drop 2 rest2
                       update = case field of 
                                  "TITLE" -> (\d->d{dsgnTitle=value})
                                  "PERIOD" -> (\d->d{dsgnPeriod=read value})
                                  "UTIL" -> (\d->d{dsgnUtilization=read value})
                                  "VOLCANO" -> (\d->d{dsgnVolcano=value})
                   in cf{cfDesigns = applyToDesign (cfDesigns cf) dName update}

  applyToDesign :: [Design] -> String -> (Design->Design) -> [Design]
  applyToDesign []     name f = [f (Design name "" (-1) 0 "")]
  applyToDesign (d:ds) name f 
    | dsgnName d == name = f d : ds
    | otherwise = d : applyToDesign ds name f


-----------------------------------------------------------------------------
-- parseDesignOutcome

parseDesignOutcome :: String -> DesignOutcome
parseDesignOutcome = pdo . lines
  where 
  pdo [] = DOFailure "Result file is empty"
  pdo (str:_)
    | str `beginsWith` "OK: " = case splitWhenAll (' '==) str of
                                  ["OK:", slack, area, cpu] -> 
                                     DOSuccess (1e12*(read slack::Double))
                                               (round (1e12*(read area::Double)))
                                               (round (read cpu::Double))
                                  _ -> DOFailure ("Parse error: " ++ str)
    | str `beginsWith` "FAIL: " = DOFailure str

                                               

-----------------------------------------------------------------------------
-- matchDesigns

matchDesigns :: [Design] -> [(String, DesignOutcome)] -> [(Design,DesignOutcome)]
matchDesigns [] outcomes = []
matchDesigns (d:ds) outcomes = 
    case lookup (dsgnName d) outcomes of
      Nothing -> (d,DOFailure "Cannot find results (not finished yet?)") : matchDesigns ds outcomes
      Just outcome -> (d,outcome) : matchDesigns ds outcomes
      


                                



-----------------------------------------------------------------------------
-- storeDatabase, loadDatabase

dbFile = "/export/home/synthesizer/DATABASE/all_batches.txt" 

storeDatabase :: [BatchResult] -> IO ()
storeDatabase bs = writeFile dbFile (show bs)

loadDatabase :: IO [BatchResult]
loadDatabase = do
  raw <- readFile dbFile
  case readList raw of
    ((batchResults, _):_) -> return batchResults
    _ -> fail "Couldn't parse database file"


--------------------------------------------------------------------------------
-- avgSynthesisTimes

avgSynthesisTimes :: [BatchResult] -> [(String, Int)]  -- the string contains design name
avgSynthesisTimes brs = avgTimes
  where
  avgTimes = map (second avg) allTimes

  allTimes = foldr collectTimes0 [] brs

  collectTimes0 :: BatchResult -> [(String,[Int])] -> [(String,[Int])]
  collectTimes0 br = case brOutcome br of
                       BOSuccess dsgnOutcomes -> \times -> foldr collectTimes1 times dsgnOutcomes
                       _ -> id

  collectTimes1 :: (Design,DesignOutcome) -> [(String,[Int])] -> [(String,[Int])]
  collectTimes1 (dsgn,outcome) = case outcome of
                                  DOSuccess _ _ cpuTime -> update (dsgnName dsgn) [cpuTime] (cpuTime:)
                                  _ -> id

  avg :: [Int] -> Int
  avg [] = 0
  avg ns = sum ns `div` length ns

