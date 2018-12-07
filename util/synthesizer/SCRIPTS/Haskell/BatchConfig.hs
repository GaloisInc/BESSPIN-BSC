------------------------------------------------------------------------------
-- Contains data type to represent configurations of a batch, and           --
-- functions to generate the bash script file that runs the batch.          --
--                                                                          --
-- Calling batchScript does the following:                                  --
--    - Creates the directory to run the script in. The directory will      --
--      always be of the form /export/home/synthesizer/DATABASE/BATCHES/{n} --
--      where {n} is a number larger than all existing ones                 --
--    - Creates a script in the directory, that (when run) goes             --
--      through a full synthesis batch, and stores the results in           --
--      the newly created directory                                         --
--    - Creates files with information about current settings and time      --
--    - Returns an absolute path to the newly created script.               --
--                                                                          --
-- If batchScript returns the file name X, an appropriate command line      --
-- to start the entire batch is:                                            --
--   ssh -l synthesizer bluetinder "nohup X &> strange_errors.txt &"        --
--                                                                          --
-- I'm terribly sorry for all the hardcoded paths etc in this file. However,--
-- I think this relatively small files become a lot harder to read if all   --
-- those things are put at the top.                                         --
------------------------------------------------------------------------------

module BatchConfig(
  BatchConfig(..),   -- data type
  ForceCO,           -- type ForceCO = Bool, force svn co instead of svn update
  BCCompiler(..),    -- data type
  BCReports(..),     -- data type

  batchScript,       -- BatchConfig -> IO FilePath
                     -- Creates a directory in the BATCHES subdirectory of the
                     -- local database, and puts the script there (returning
                     -- its filename). Makes the script file executable.
 ) where

import List(intersperse)
import Directory
import Char(isDigit)
import System.Time

import Design
import Utils(showNumLen)

--------------------------------------------------------------------------------
-- Data types

data BatchConfig = BatchConfig { bcCompiler :: BCCompiler
                               , bcDesigns :: [String]
                               , bcReports :: BCReports
                               , bcReportsAddr :: [String]
                               , bcIsAuto :: Bool
                               } deriving (Eq, Show, Read)

type ForceCO = Bool

data BCCompiler = BCCHead ForceCO
                | BCCRev String ForceCO
                | BCCCurrent
                | BCExisting FilePath FilePath String
  deriving (Eq, Show, Read)

data BCReports = BCNothing | BCErrors | BCFinal | BCContinuous
  deriving (Eq, Ord, Show, Read)


getNextBatchID :: IO Int
getNextBatchID = do
   fileNames <- getDirectoryContents "/export/home/synthesizer/DATABASE/BATCHES"
   let batchDirNames = filter (all isDigit) fileNames
   let lastExistingBatch = maximum ((0::Int):(map read batchDirNames))
   return (lastExistingBatch+1)

forceCO :: BatchConfig -> Bool
forceCO bc = case bcCompiler bc of
               BCCHead fco -> fco
               BCCRev _ fco -> fco
               _ -> False

updateCompiler :: BatchConfig -> Bool
updateCompiler bc = case bcCompiler bc of
                      BCCHead _ -> True
                      BCCRev _ _ -> True
                      BCCCurrent -> False
                      BCExisting _ _ _ -> False

--------------------------------------------------------------------------------
-- Utils for generating bash scripts

--checkedCommand: produce a bash if statement which checks if a command is successful, and executes either a success handler or an error handler depending on the results
checkedCommand :: String -> [String] -> [String] -> [String]
checkedCommand cmd successH errorH = 
    concat [["if " ++ cmd ++ " ; then"]
           ,map ("  "++) successH
           ,["else"]
           ,map ("  "++) errorH
           ,["fi"]]

--checkFailCmd: produce a bash statement that checks if a command is successful, and handles errors (does nothing on success)
checkFailCmd :: String -> [String] -> [String]
checkFailCmd cmd errorH = checkedCommand cmd ["echo > /dev/null"] errorH


--sendOneLineMail: produce a bash statement that sends a one line email to a number of recipients. DO NOT try to be smart and use newlines. Also refrain from using "", since that will also break the command line.
sendOneLineMail :: String -> String -> [String] -> String
sendOneLineMail subject body [] = ""
sendOneLineMail subject body to =
   concat ["echo "
          ,show body
          ," | "
          ,"mail -s "
          ,show subject
          ,concatMap (' ':) to
          ]


--sendNotification: produce a bash statement that sends an email to the right addresses if the report level is high enough (produces empty line otherwise)
sendNotification :: BCReports -> BatchConfig -> String -> String
sendNotification level bc subject 
  | bcReports bc >= level = sendOneLineMail subject "See https://svn.bluespec.com:8080/~synthesizer/ for more info" (bcReportsAddr bc)
  | otherwise = ""

--sendFileMail: produce a bash statement that sends an email to a number of recipients, reading the mail body from an existing text file
sendFileMail :: String -> FilePath -> [String] -> String
sendFileMail subject inFile [] = ""
sendFileMail subject inFile to =
  concat ["mail -s "
         ,show subject
         ,concatMap (' ':) to
         ," < "
         ,inFile]

--------------------------------------------------------------------------------
-- Functions to generate scripts

batchScript :: BatchConfig -> IO FilePath
batchScript bc = do
  startDesigns <- mapM (oneDesign bc) (bcDesigns bc)
  batchID <- getNextBatchID
  let activeDir = "/export/home/synthesizer/DATABASE/BATCHES/" ++ show batchID
  let scriptFileName = activeDir ++ "/run-batch"
  createDirectory activeDir
  createSettingsFile bc batchID activeDir 
  writeFile scriptFileName $ unlines $ concat 
    [["#!/bin/bash"
      ,""
      ,"SYNHOMEDIR=/export/home/synthesizer"
      ,"RUNID=" ++ show batchID
      ,"TEMPDIR=$SYNHOMEDIR/TEMP/$RUNID"
      ,"DBDIR=$SYNHOMEDIR/DATABASE/BATCHES/$RUNID"
      ,"SCRIPTSDIR=$SYNHOMEDIR/SCRIPTS/"
      ,"PROGRFILE=$DBDIR/progress.txt"
      ,"RESULTFILE=$DBDIR/final_results.txt"
      ,"export PATH=$PATH:/usr/local/bin"  -- needed when started by crontab
      ,case bcCompiler bc of
         BCCRev rev co -> "BSCREVSN=" ++ rev
         _ -> "BSCREVSN=\"\""
      ,""
      ,case bcCompiler bc of
         -- If a custom binary is specified, then we create our own executable bsc file,
         -- that only starts the specified one, with the extra arguments
         BCExisting path bsdir args -> unlines ["echo \'#!/bin/bash\' > $DBDIR/bsc"
                                               ,"echo \'" ++ path ++ " " ++ args ++ 
                                                              " $*\' >> $DBDIR/bsc" 
                                               ,"chmod a+x $DBDIR/bsc"
                                               ,"export PATH=$DBDIR/:$PATH"
                                               ,"export BLUESPECDIR=" ++ bsdir
                                               ,"export LM_LICENSE_FILE=27000@indigo"]
         _ -> "source $SCRIPTSDIR/bsc_setup"
      ,""
     ]
    ,checkFailCmd "mkdir $TEMPDIR"
       ["echo \"Error: Couldn't create temporary directory $TEMPDIR. Giving up.\" >> $PROGRFILE"
        ,"echo \"FAIL: Couldn't create temporary directory $TEMPDIR\" >> $RESULTFILE"
        ,sendNotification BCErrors bc "Synthesizer failed - couldn't create temporary directory ($TEMPDIR)"
        ,"exit 1"]
    ,if updateCompiler bc
     then concat [["echo \"********* Updating and building bsc *********\" >> $PROGRFILE"]
                 ,if forceCO bc
                  then ["echo `date` Removing the bsc directory >> $PROGRFILE"
                       ,"rm -rf $SYNHOMEDIR/bsc"]
                  else []
                 ,checkedCommand "$SCRIPTSDIR/update-bsc $DBDIR/compiler_rslt.txt $PROGRFILE $BSCREVSN"
                    [-- if compiler successfully built
                     "$SCRIPTSDIR/updateDB"
                    ,if bcReports bc >= BCContinuous
                     then sendFileMail "Synthesizer progress report: bsc successfully built" "$PROGRFILE" (bcReportsAddr bc)
                     else "" 
                    ]
                    [-- if compiler couldn't be built
                     "mv $DBDIR/compiler_rslt.txt $RESULTFILE"
                    ,"rm -rf $TEMPDIR"
                    ,sendNotification BCErrors bc "Synthesizer failed - couldn't build compiler"
                    ,"exit 1"]
                 ]
     else []
    , -- Check which revision of bsc that is being used
      case bcCompiler bc of
        BCExisting _ _ _ -> []  -- irrelevant if custom compiler being used
        _ -> ["pushd $SYNHOMEDIR/bsc/ > /dev/null" 
             ,"echo REPORTED_BSC_REV: `svn info | grep Revision` >> $DBDIR/configuration.txt"
             ,"popd > /dev/null"]

    ,concat startDesigns -- here is where all individual designs are started

    ,["echo \"********* Final clean up *********\" >> $PROGRFILE"
      ,"rm -rf $TEMPDIR"
      ,"echo `date` Everything finished. >> $PROGRFILE"
      ,if bcReports bc >= BCFinal
      then sendFileMail "Synthesizer finished" "$PROGRFILE" (bcReportsAddr bc)
      else ""
      ,"echo OK > $RESULTFILE"
     ]
    ]
  permissions <- getPermissions scriptFileName
  setPermissions scriptFileName (permissions{executable=True})
  return scriptFileName


oneDesign :: BatchConfig -> String -> IO [String]
oneDesign bc designName = do
  mDesign <- lookupDesign designName
  case mDesign of
    Nothing -> return ["echo \"Error: couldn't find design named " ++ designName ++ " in database. Skipping.\" >> $PROGRFILE"
                      ,sendNotification BCContinuous bc ("Couldn't find design named " ++ designName ++ " in the database. I'm skipping that design")
                      ]
    Just design -> return 
       ["echo \"********* Synthesizing design " ++ show designName ++ " *********\" >> $PROGRFILE"
       ,concat $ intersperse " "
           ["$SCRIPTSDIR/run-design"
           ,"$SYNHOMEDIR/DESIGNS/" ++ designName
           ,show (dsgnPeriod design) ++ "p"
           ,show (dsgnUtilization design)
           ,dsgnVolcano design
           ,"$TEMPDIR"
           ,localRsltFile
           ,"$PROGRFILE"
           ]
       ,"head -1 " ++ localRsltFile ++ " >> $PROGRFILE"
       ,"$SCRIPTSDIR/updateDB"
       ,if bcReports bc >= BCContinuous
        then sendFileMail ("Synthesizer progress report: " ++ designName ++ " finished")
                          "$PROGRFILE" 
                          (bcReportsAddr bc)
        else ""
       ]
  where
  localRsltFile = "$DBDIR/rslt_" ++ designName ++ ".txt"


--------------------------------------------------------------------------------
-- Function to generate settings file

createSettingsFile :: BatchConfig -> Int -> FilePath -> IO ()
createSettingsFile bc batchID activeDir = do
  let fname = activeDir ++ "/configuration.txt"
  currTime <- (getClockTime >>= toCalendarTime)

  designs <- getAllDesigns
  let selectedDesigns = filter (\d -> (dsgnName d `elem` (bcDesigns bc))) designs
  
  writeFile fname $ unlines $ concat $
      [concat ["DATE: "
              ,showNumLen 4 (ctYear currTime)
              ,"-"
              ,showNumLen 2 (fromEnum (ctMonth currTime)+1)
              ,"-"
              ,showNumLen 2 (ctDay currTime)
              ," "
              ,showNumLen 2 (ctHour currTime)
              ,":"
              ,showNumLen 2 (ctMin currTime)
              ]
      ,"BSC_REV: " ++ case bcCompiler bc of 
                        BCCHead _ -> "HEAD"
                        BCCRev rev _ -> rev
                        BCCCurrent -> "CURRENT"
                        BCExisting _ _ _ -> "CUSTOM"
      ,"IS_AUTOMATIC: " ++ if bcIsAuto bc
                           then "TRUE"
                           else "FALSE"
      ] : map (\design -> let name = dsgnName design
                          in ["DESIGN_TITLE(" ++ name ++ "): " ++ dsgnTitle design
                             ,"DESIGN_PERIOD(" ++ name ++ "): " ++ show (dsgnPeriod design)
                             ,"DESIGN_UTIL(" ++ name ++ "): " ++ show (dsgnUtilization design)
                             ,"DESIGN_VOLCANO(" ++ name ++ "): " ++ dsgnVolcano design])
              selectedDesigns




