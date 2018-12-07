-- This module contains an interface to the design database. It defines
-- a data type, Design, that represents individual designs. The most
-- important field is dsgnName, which is a short name that serves as an
-- identifier for the design. The design itself will always be found in
-- the directory ~synthesizer/DESIGNS/<name>, where <name> is the dsgnName.
--
-- There are also functions to look up designs in the database, store
-- designs, and retrieve a complete list of all designs.

module Design(
  Design(..),    -- type (see below)
  lookupDesign,  -- String -> IO (Maybe Design)
  storeDesign,   -- Design -> IO ()
  getAllDesigns, -- IO [Design]
  dsgnPath,      -- String -> FilePath
 ) where

import IO
import Directory
import Maybe(catMaybes)

import Utils(beginsWith)

--------------------------------------------------------------------------------
-- Data type Design

data Design = Design { dsgnName :: String  -- short name used in path names etc
                     , dsgnTitle :: String -- long name used in presentation
                     , dsgnPeriod :: Double -- in picoseconds
                     , dsgnUtilization :: Int -- in percents
                     , dsgnVolcano :: FilePath -- path to volcano
                     }
  deriving (Eq, Show, Read)

defaultDesign name = Design { dsgnName = name
                            , dsgnTitle = name
                            , dsgnPeriod = 10000  -- default 100MHz
                            , dsgnUtilization = 75
                            , dsgnVolcano = "/tools/magma/lib/csm13rvt_8lm.volcano/"
                            }


--------------------------------------------------------------------------------
-- Database location

designsPath = "/export/home/synthesizer/DESIGNS/"
dsgnPath designName = designsPath ++ designName ++ "/"
dbFile designName = dsgnPath designName ++ "synth-params"

--------------------------------------------------------------------------------
-- Database manipulation operations

lookupDesign :: String -> IO (Maybe Design)
lookupDesign designName = try (do
    handle <- openFile (dbFile designName) ReadMode
    rslt <- parseFile' handle (defaultDesign designName)
    hClose handle
    return rslt  
  ) >>= \rslt -> case rslt of
                    Left err -> return Nothing
                    Right rslt -> return (Just rslt)
  
  where
  parseFile' handle design = 
     do eof <- hIsEOF handle
        if eof
          then return design
          else do line <- hGetLine handle
                  case line of
                    _ | line `beginsWith` "TITLE: " ->
                                     parseFile' handle design{dsgnTitle = drop 7 line}
                    _ | line `beginsWith` "PERIOD: " ->
                                     parseFile' handle design{dsgnPeriod = read (drop 8 line)}
                    _ | line `beginsWith` "UTIL: " -> 
                                     parseFile' handle design{dsgnUtilization = read (drop 6 line)}
                    _ | line `beginsWith` "VOLCANO: " ->
                                     parseFile' handle design{dsgnVolcano = drop 9 line}
                    _ -> parseFile' handle design

storeDesign :: Design -> IO ()
storeDesign design = writeFile (dbFile (dsgnName design)) $ unlines 
  [ "TITLE: " ++ dsgnTitle design
  , "PERIOD: " ++ show (dsgnPeriod design)
  , "UTIL: " ++ show (dsgnUtilization design)
  , "VOLCANO: " ++ dsgnVolcano design
  ]


getAllDesigns :: IO [Design]
getAllDesigns = do 
  designDirs <- getDirectoryContents designsPath
  let designDirs' = filter (not.(`beginsWith` ".")) designDirs
  mDesigns <- mapM lookupDesign designDirs'
  return (catMaybes mDesigns)



--------------------------------------------------------------------------------


--pongDesign = Design "pong" "Pong with oscillating island" 4000000 75 "/tools/magma/lib/csm13rvt_8lm.volcano/"

--upconvDesign = Design "upconv" "STE up converter" "upconv" 4000000 75 "/tools/magma/lib/csm13rvt_8lm.volcano/"



