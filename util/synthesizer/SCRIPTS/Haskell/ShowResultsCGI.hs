module Main where

import Network.CGI
import Text.Html
import IO
import Maybe(catMaybes)
import List(intersperse,sortBy)
import System

import BatchResult
import Design
import CGIUtils
import Utils(transpose,tryReadFile,tryMaybe,beginsWith,compareNum)

--------------------------------------------------------------------------------

linkToScript = "results"

--------------------------------------------------------------------------------
-- in this representation of the result, each column has a number 

data Statistics = Statistics { sColTitles :: [String]
                             , sRows :: [[SEntry]]
                             }
  deriving Show

data SEntry = SEEmpty
            | SEHtml Html
            | SEString String
            | SEInt Int
            | SEAbsolute Double
            | SEOffset Double Double  -- an offset plus a relative value
  deriving Show

--------------------------------------------------------------------------------
-- make statistics

data StatSettings = SS { includePeriod :: Bool
                       , includeArea :: Bool
                       , includeCPU :: Bool
                       , includeDate :: Bool
                       , showGraphs :: Bool
                       , graphWidth :: Int
                       , sortByCol :: SortStatBy
                       }

data SortStatBy = SSBatch | SSDate | SSRevision
  deriving Eq

makeStat :: StatSettings -> [BatchResult] -> Statistics
makeStat ss brs' = Statistics colNames rows
  where
  brs = reverse $ 
        case sortByCol ss of
          SSBatch -> sortBy (\b1 b2 -> compare (brBatchID b1) (brBatchID b2)) brs'
          SSDate -> sortBy (\b1 b2 -> compare (brDate b1) (brDate b2)) brs'
          SSRevision -> sortBy (\b1 b2 -> compareNum (brBSCVersion b1) (brBSCVersion b2)) brs'
                     

  allDesigns = getDesignNames brs

  colNames = ["Batch", "BSC rev"] ++
             (if includeDate ss then ["Date"] else []) ++
             concatMap (\dname -> (if includePeriod ss
                                   then ((dname ++ " period"):)
                                   else id) $
                                  (if includeArea ss
                                   then ((dname ++ " area"):)
                                   else id) $
                                  (if includeCPU ss
                                   then ((dname ++ " CPU"):)
                                   else id) $
                                  []) 
                       allDesigns
                                  
                    
  makeRow :: BatchResult -> [(String, SEntry)]
  makeRow br = 
     [("Batch", (SEHtml (toHtml (hotlink (linkToBatchPage (brBatchID br)) 
                                         [toHtml (show (brBatchID br))]))))
     ,("Date", SEString (brDate br))
     ,("BSC rev", SEString (brBSCVersion br))
     ] ++ case brOutcome br of
            BOSuccess dsgns -> concatMap makeDesign dsgns
            _ -> []

  makeDesign :: (Design,DesignOutcome) -> [(String,SEntry)]
  makeDesign (dsgn, DOSuccess slack area cpu) = [(dsgnName dsgn ++ " period"
                                                 ,SEOffset (dsgnPeriod dsgn) (-slack))
                                                ,(dsgnName dsgn ++ " area"
                                                 ,SEAbsolute (fromIntegral area))
                                                ,(dsgnName dsgn ++ " CPU"
                                                 ,SEAbsolute (fromIntegral cpu))
                                                ]
  makeDesign (dsgn, DOFailure _) = []
                                                           
  rows = map (fitRowInColumns colNames . makeRow) brs


fitRowInColumns :: [String] -> [(String, SEntry)] -> [SEntry]
fitRowInColumns columns explicitRows =
   map (maybe SEEmpty id . (\col -> lookup col explicitRows)) columns

--------------------------------------------------------------------------------
-- Show statistics

getBounds :: Statistics -> [(String,Maybe Double)]
getBounds (Statistics titles rows) = zip titles bounds
  where
  interpretElement :: SEntry -> Maybe Double
  interpretElement (SEAbsolute d) = Just d
  interpretElement (SEOffset off rel) = Just (max off (off+rel))
  interpretElement _ = Nothing

  columns = transpose rows
  interpretedColumns = map (catMaybes . map interpretElement) columns
  bounds = map (\ds -> if null ds
                       then Nothing
                       else Just (maximum ds)) interpretedColumns
  
 

showStats :: StatSettings -> Statistics -> Html
showStats ss stats@(Statistics colTitles rows) =
    (table ! [intAttr "border" 1])
	(concatHtml (showTitles : map showRow rows))
  where
  showTitles = tr (concatHtml (map (td.bold.toHtml) colTitles))

  bounds = map snd $ getBounds stats
  
  showRow row = tr (concatHtml (zipWith (\b e -> td $ showElm b e) bounds row))

  showElm _ SEEmpty = noHtml
  showElm _ (SEHtml h) = h
  showElm _ (SEString s) = toHtml s
  showElm _ (SEInt i) = toHtml $ show i
  showElm (Just bound) (SEAbsolute x) 
    | showGraphs ss = mkNormBar "#0000a0" (graphWidth ss) bound x
    | otherwise = toHtml $ show x
  showElm (Just bound) (SEOffset off rel) 
    | showGraphs ss = mkNormOffsetBar (graphWidth ss) bound off rel
    | otherwise = if rel >= 0
                  then toHtml (show off ++ " + " ++ show rel)
                  else toHtml (show off ++ " - " ++ show (-rel))
  showElm b e = toHtml ("Error: showElm " ++ show b ++ " " ++ show e)



--------------------------------------------------------------------------------

main = wrapper cgiMain

cgiMain :: [(String,String)] -> IO Html
cgiMain env = case lookup "QUERY_STRING" env of
                Nothing -> defaultPage []
                Just queries -> let parsedQueries = parseQueries queries
                                in case lookup "page" parsedQueries of
                                     Just "oneBatch" -> oneBatchPage parsedQueries
                                     Just "showFile" -> showFilePage parsedQueries
                                     Just "deleteBatch" -> deleteBatchPage parsedQueries
                                     _ -> defaultPage parsedQueries 




--------------------------------------------------------------------------------

defaultPageLink = linkToScript

defaultPage :: [(String,String)] -> IO Html
defaultPage queries = (try $ do
    -- Load or refresh database
    let doRefresh = lookup "refresh" queries /= Nothing

    database <- if doRefresh
                then do db <- parseAllBatches
                        storeDatabase db
                        return db
                else do rslt <- try loadDatabase
                        case rslt of 
                          Right db -> return db
                          Left err -> do db <- parseAllBatches
                                         storeDatabase db
                                         return db

    -- Check settings
    let showPeriods = lookup "hidePeriod" queries == Nothing
        showArea = lookup "hideArea" queries == Nothing
        showCPU = lookup "showCPU" queries /= Nothing
        showDates = lookup "showDate" queries /= Nothing
        showGraphs = lookup "numeric" queries == Nothing
        graphWidth = maybe 100 read (lookup "width" queries)
        sortByCol = case lookup "sort" queries of
                      Just "batch" -> SSBatch
                      Just "date" -> SSDate
                      Just "rev" -> SSRevision
                      _ -> SSBatch
        statSettings = SS showPeriods showArea showCPU showDates showGraphs graphWidth sortByCol

    return $ mkPage "Synthesizer results" 
        (h1 (toHtml "Synthesizer results") +++

         toHtml (hotlink (defaultPageLink ++ show statSettings ++ "&refresh") [toHtml "Refresh database now (if results are missing)"]) +++
            
         -- Include the table
         (showStats statSettings $ makeStat statSettings database) +++

         -- Link to instructions
         toHtml (hotlink "../results-guide.html"
                         [toHtml "What does this mean?"]) +++

         -- Include options for presentation
         h3 (toHtml "Change presentation settings:") +++
         toHtml (hotlink defaultPageLink [toHtml "Back to default"]) +++
         mkTable [[bold (toHtml "Period: ")
                  ,toHtml (hotlink (defaultPageLink ++ show (statSettings{includePeriod=True}))
                                   [toHtml "show"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{includePeriod=False}))
                                   [toHtml "hide"])
                  ]
                 ,[bold (toHtml "Area: ")
                  ,toHtml (hotlink (defaultPageLink ++ show (statSettings{includeArea=True}))
                                   [toHtml "show"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{includeArea=False}))
                                   [toHtml "hide"])
                  ]
                 ,[bold (toHtml "CPU time: ")
                  ,toHtml (hotlink (defaultPageLink ++ show (statSettings{includeCPU=True}))
                                   [toHtml "show"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{includeCPU=False}))
                                   [toHtml "hide"])
                  ]
                 ,[bold (toHtml "Date: ")
                  ,toHtml (hotlink (defaultPageLink ++ show (statSettings{includeDate=True}))
                                   [toHtml "show"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{includeDate=False}))
                                   [toHtml "hide"])
                  ]
                 ,[bold (toHtml "Representation: ")
                  ,toHtml (hotlink (defaultPageLink ++ show (statSettings{showGraphs=True}))
                                   [toHtml "graphic"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{showGraphs=False}))
                                   [toHtml "numeric"])
                  ]
                 ,[bold (toHtml "Graph size: ")
                  ,toHtml (hotlink (defaultPageLink ++ show (statSettings{graphWidth=50}))
                                   [toHtml "small"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{graphWidth=100}))
                                   [toHtml "medium"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{graphWidth=200}))
                                   [toHtml "large"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{graphWidth=500}))
                                   [toHtml "huge"])
                  ]
                 ,[bold (toHtml "Sort by: ")
                  ,toHtml (hotlink (defaultPageLink ++ show (statSettings{sortByCol=SSBatch}))
                                   [toHtml "Batch id"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{sortByCol=SSDate}))
                                   [toHtml "Date"]) +++ (toHtml " ") +++
                   toHtml (hotlink (defaultPageLink ++ show (statSettings{sortByCol=SSRevision}))
                                   [toHtml "BSC revision"])
                  ]
                 ] +++

         paragraph (toHtml (hotlink "https://svn.bluespec.com:8080/~synthesizer"
                                    [toHtml "Back to synthesizer main page"])) +++

         noHtml
        )

  ) >>= \rslt -> case rslt of
                   Right page -> return page
                   Left err -> exceptionPage queries err


--------------------------------------------------------------------------------

linkToBatchPage :: Int -> String
linkToBatchPage n = linkToScript ++ "?page=oneBatch&batch=" ++ show n

oneBatchPage :: [(String,String)] -> IO Html
oneBatchPage queries = (try $ do
  ourBatchId <- case lookup "batch" queries of
                     Nothing -> fail "No batch given"
                     Just id -> return $ (read id :: Int)
  
  batchResult <- parseBatchNr ourBatchId >>= \rslt ->
                 case rslt of
                   Nothing -> fail ("Cannot read batch " ++ show ourBatchId)
                   Just br -> return br

  progressFile <- readBatchFile ourBatchId "progress.txt"

  return $ mkPage ("Batch " ++ show ourBatchId)
     (h1 (toHtml ("Batch " ++ show ourBatchId)) +++
      
      mkTable [[bold (toHtml "Id number: ")
               ,toHtml (show (brBatchID batchResult))]
              ,[bold (toHtml "Start time: ")
               ,toHtml (brDate batchResult)]
              ,[bold (toHtml "Started by: ")
               ,if brIsAutomatic batchResult
                then toHtml "Automatic script"
                else toHtml "User"]
              ,[bold (toHtml "Finished: ")
               ,if brFinished batchResult
                then toHtml "Yes"
                else toHtml "No"]
              ,[bold (toHtml "BSC Revision: ")
               ,toHtml (brBSCVersion batchResult)]
              ,[bold (toHtml "Warnings: ")
               ,if null (brWarnings batchResult)
                then (toHtml "None")
                else ulist (concatHtml (map (\w -> li (toHtml w))
                                            (brWarnings batchResult)))]
              ] +++

      (case brOutcome batchResult of
        BOSuccess dos -> 
           (paragraph (bold (toHtml "BSC successfully built, running designs:"))) +++
           concatHtml 
              (map (\(dsgn,doc) -> 
                        h2 (toHtml (dsgnTitle dsgn ++ " (" ++ 
                                    dsgnName dsgn ++ ")")) +++
                        h3 (toHtml "Settings:") +++
                        mkTable [[bold (toHtml "Utilization:")
                                 ,toHtml (show (dsgnUtilization dsgn) ++ " percent")]
                                ,[bold (toHtml "Volcano: ")
                                 ,toHtml (dsgnVolcano dsgn)]
                                ,[bold (toHtml "Period: ")
                                 ,toHtml (show (dsgnPeriod dsgn) ++ " picoseconds")]
                                ] +++
                        h3 (toHtml "Result:") +++
                        case doc of
                          DOSuccess _ _ _ ->  
                             mkTable [[bold (toHtml "Slack: ")
                                      ,toHtml (show (dosSlack doc) ++ " picoseconds")]
                                     ,[bold (toHtml "Area: ")
                                      ,toHtml (show (dosArea doc) ++ " um")]
                                     ,[bold (toHtml "Magma CPU time:")
                                      ,toHtml (show (dosCPUTime doc) ++ " seconds")]
                                     ]
                          DOFailure msg -> 
                              paragraph (bold (toHtml "Error: ") +++
                                         toHtml msg) +++
                              paragraph (toHtml 
                                         (hotlink (linkToShowFilePage ourBatchId 
                                                                      ("rslt_" ++
                                                                       dsgnName dsgn ++
                                                                       ".txt"))
                                                  [toHtml "See error messages"]))
                   ) dos)

        BOFailure msg -> (paragraph (bold (toHtml "Outcome: ") +++ toHtml "Failure") +++
                          paragraph (bold (toHtml "Reason: ") +++ toHtml msg) +++
                          paragraph (toHtml
                                     (hotlink (linkToShowFilePage ourBatchId 
                                                                  "final_results.txt")
                                              [toHtml "See error messages"])))
        BOComment msg -> paragraph (bold (toHtml "Outcome: ") +++ toHtml ("Comment:" ++ msg))
      )+++    
      
      h2 (toHtml "Progress file") +++

      (case progressFile of
          Nothing -> toHtml "Couldn't read progress file" 
          Just contents -> showText contents
      ) +++

      paragraph (toHtml (hotlink (linkToShowFilePage ourBatchId "run-batch") [toHtml "View batch script"])) +++

      h2 (toHtml "Delete this batch") +++
      deleteBatchForm ourBatchId +++

      paragraph (toHtml (hotlink defaultPageLink [toHtml "Back to table"])) +++

      noHtml
     )
  ) >>= \rslt -> case rslt of
                   Right page -> return page
                   Left err -> exceptionPage queries err


--------------------------------------------------------------------------------

readBatchFile :: Int -> String -> IO (Maybe String)
readBatchFile batch file = tryReadFile ("/export/home/synthesizer/DATABASE/BATCHES/" ++ show batch ++ "/" ++ file )

htmlifyLine :: String -> Html
htmlifyLine str = spaces +++ dropSpaces +++ br
  where
  initialSpaces = length (takeWhile (' '==) str)
  spaces = concatHtml (replicate initialSpaces spaceHtml)
  dropSpaces = drop initialSpaces str

showText :: String -> Html
showText str = concatHtml $ map (htmlifyLine) $ lines str

linkToShowFilePage :: Int -> String -> String
linkToShowFilePage batch file = concat [linkToScript
                                       ,"?page=showFile&batch="
                                       ,show batch
                                       ,"&file="
                                       ,file]

showFilePage :: [(String,String)] -> IO Html
showFilePage queries = (try $ do
  ourBatchId <- case lookup "batch" queries of
                     Nothing -> fail "No batch given"
                     Just id -> return $ (read id :: Int)

  fileName <- case lookup "file" queries of
                Nothing -> fail "No file name given"
                Just name -> if ((name `beginsWith` ".") ||
                                 ('/' `elem` name))
                             then fail ("I will not show the file " ++ show name ++ 
                                        " for security reasons. No paths are allowed in the file names.")
                             else return name

  file <- do rslt <- readBatchFile ourBatchId fileName
             case rslt of
               Nothing -> fail "Couldn't read file"
               Just f -> return f

  return $ mkPage ("File contents")
     (h1 (toHtml ("Contents of file " ++ show fileName ++ " in batch " ++ show ourBatchId)) +++
      showText file +++
      br +++
      hotlink (linkToBatchPage ourBatchId)
             [toHtml "Back to batch"]
     )

  ) >>= \rslt -> case rslt of
                   Right page -> return page
                   Left err -> exceptionPage queries err
--------------------------------------------------------------------------------

deleteBatchForm :: Int -> Html
deleteBatchForm batch = (form ! [strAttr "action" linkToScript
                                ,strAttr "method" "GET"])
                          (submit "action" "Delete" +++
                           hidden "batch" (show batch) +++
                           hidden "page" "deleteBatch" +++
                           toHtml " Yes, I am serious" +++
                           (input ! [strAttr "type" "checkbox"
                                    ,strAttr "name" "serious"])
                           )

deleteBatchPage :: [(String,String)] -> IO Html
deleteBatchPage queries = (try $ do
  ourBatchId <- case lookup "batch" queries of
                     Nothing -> fail "No batch given"
                     Just id -> return $ (read id :: Int)

  serious <- case lookup "serious" queries of
               Just "on" -> return True
               _ -> return False

  let delCmd = "rm -rf /export/home/synthesizer/DATABASE/BATCHES/" ++ show ourBatchId
              
  rslt <- if serious
          then system delCmd
          else return $ ExitFailure 0


  if serious && (rslt == ExitSuccess)
    then defaultPage [("refresh","")]
    else return $ mkPage ("Delete batch")
       (h1 (toHtml ("Delete batch " ++ show ourBatchId)) +++
       
       if serious 
          then (case rslt of
                  ExitSuccess -> (toHtml "Batch deleted. I suggest that you " +++
                                  hotlink (linkToScript ++ "?refresh")
                                          [toHtml "refresh the database now."])
                  ExitFailure n -> (toHtml ("rm returned an error code (" ++
                                            show n ++ "). I suggest that you ") +++
                                    hotlink (linkToScript ++ "?refresh")
                                          [toHtml "refresh the database, and see what happened"])
               )
          else (toHtml "This really deletes all traces of the batch. Do you want to continue? " +++
                hotlink (linkToScript ++ "?page=deleteBatch&serious=on&batch=" ++ 
                         show ourBatchId)
                        [toHtml "Yes"] +++
                toHtml " " +++
                hotlink (linkToBatchPage ourBatchId)
                        [toHtml "No"]
               )
       
       )

  ) >>= \rslt -> case rslt of
                   Right page -> return page
                   Left err -> exceptionPage queries err


--------------------------------------------------------------------------------

exceptionPage :: [(String,String)] -> IOError -> IO Html
exceptionPage queries err = return $ mkPage "Error" 
                                   (h1 (toHtml "Error") +++
                                    bold (toHtml "Reason: ") +++
                                    toHtml (show err) +++

                                    noHtml
                                    )

--------------------------------------------------------------------------------

instance Show StatSettings where
  show (SS includePeriod includeArea includeCPU includeDate showGraphs graphWidth sortByCol) = 
    '?' : (concat $ intersperse "&" $ catMaybes 
             [if includePeriod then Nothing else Just "hidePeriod"
             ,if includeArea then Nothing else Just "hideArea"
             ,if includeCPU then Just "showCPU" else Nothing
             ,if includeDate then Just "showDate" else Nothing
             ,if showGraphs then Nothing else Just "numeric" 
             ,Just ("width="++show graphWidth)
             ,case sortByCol of
                SSBatch -> Just ("sort=batch")
                SSDate -> Just ("sort=date")
                SSRevision -> Just ("sort=rev")
             ]
          )
          



--------------------------------------------------------------------------------

--errorPage :: String -> IO Html



                




