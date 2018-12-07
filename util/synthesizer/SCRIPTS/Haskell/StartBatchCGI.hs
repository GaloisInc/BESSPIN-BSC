-----------------------------------------------------------------------------
-- This is the main module of the CGI script that starts a batch manually. --
-- The script can be run through runhugs, or compiled by GHC.              --
-----------------------------------------------------------------------------

module Main where

import Network.CGI
import Text.Html
import Char(isSpace)
import System
import IO

import Design
import BatchConfig
import BatchResult
import Utils(beginsWith, splitWhenT, splitWhenAll)
import CGIUtils

main = wrapper cgiMain

cgiMain :: [(String,String)] -> IO Html
cgiMain env = case lookup "QUERY_STRING" env of
                Nothing -> defaultPage env
                Just queries -> let parsedQueries = parseQueries queries
                                in case lookup "action" parsedQueries of
                                      Just "Start" -> executePage env parsedQueries
                                      _ -> defaultPage env


--------------------------------------------------------------------------------
-- Default page

defaultPage :: [(String,String)] -> IO Html
defaultPage env = do
  designs <- getAllDesigns

  avgTimes <- (try loadDatabase) >>=
              \rslt -> case rslt of
                         Left _ -> return []
                         Right batchRslt -> return (avgSynthesisTimes batchRslt)
  
  return $ mkPage ("Start synthesizer batch manually") 
           (h1 (toHtml "Start synthesizer batch manually") +++
            (form ! [strAttr "action" "https://svn.bluespec.com:8080/~synthesizer/cgi-bin/start?page=execute"
                    ,strAttr "method" "GET"])
              (h3 (toHtml "Which bsc version to use") +++
                  ((mkTable ! [intAttr "border" 0
                             ,intAttr "cellPadding" 5])
                     ([[(radio ! [emptyAttr "checked"]) "compiler" "head", 
                        bold (toHtml "Check out the head of the repository")  +++
                        br +++
                        (input ! [strAttr "type" "checkbox"
                                 ,strAttr "name" "forceCO"]) +++
                        toHtml "Do a full svn checkout without first attempting to do svn update (this option also works in the following case)" 
                       ]
                      ,[radio "compiler" "rev", 
                        bold (toHtml "Check out a specific revision: ") +++
                        (input ! [strAttr "type" "text"
                                 ,strAttr "name" "revNr"
                                 ,intAttr "size" 8
                                 ,strAttr "value" "0"])
                       ]
                      ,[radio "compiler" "old",
                        bold (toHtml "Use whatever binary that is currently in the synthesizer home directory")
                       ]
                      ,[radio "compiler" "custom",
                        bold (toHtml "Use an existing binary:") +++
                        (mkTable ! [intAttr "border" 0])
                          [[toHtml "Name of binary (with full path): "
                           ,(input ! [strAttr "type" "text"
                                     ,strAttr "name" "cPath"
                                     ,intAttr "size" 50
                                     ,strAttr "value" "/export/home/synthesizer/bsc/src/bsc"])
                           ]
                          ,[toHtml "Extra arguments to bsc: "
                           ,(input ! [strAttr "type" "text"
                                     ,strAttr "name" "cArgs"
                                     ,intAttr "size" 50
                                     ,strAttr "value" ""])
                           ]
                          ,[toHtml "$BLUESPECDIR:"
                           ,(input ! [strAttr "type" "text"
                                     ,strAttr "name" "cBSDIR"
                                     ,intAttr "size" 50
                                     ,strAttr "value" "/export/home/synthesizer/bsc/src/lib"]) +++
                            paragraph (italics ((bold (toHtml "Note:")) +++ toHtml " Make sure that the synthesizer user has permission to execute the binary you specify, and that it can run on bluetinder.")) +++
                            paragraph (italics ((bold (toHtml "Note:")) +++ toHtml " The extra arguments will be given before any arguments specified by the respective makefiles. If you want to give special arguments to a bsc version that is in the repository, first start a batch that only checks out the revision you want (deselect all designs below), and when it's finished, start a batch using an existing binary (the default one), with your special arguments."))
                           ]
                          ]
                       ]
                      ]
                     )
                  ) +++

              h3 (toHtml "Designs to include") +++
              (mkTable ! [intAttr "cellpadding" 5])
                (map (\design -> [checkedBox ("incl_" ++ dsgnName design)
                                 ,bold (toHtml (dsgnName design))
                                 ,italics (toHtml (dsgnTitle design))
                                 ,case lookup (dsgnName design) avgTimes of
                                    Nothing -> toHtml ""
                                    Just s -> toHtml (show (s `div` 60) ++ " minutes")])
                     designs) +++
              toHtml "The rightmost column tells the average cpu time for synthesis. This does not include compile time. Often tinderbox is running on the same machine, so the real time is often twice the cpu time." +++


              h3 (toHtml "Send progress notification emails") +++
              (mkTable ! [intAttr "cellpadding" 5])
                [[radio "reports" "cont"
                 ,toHtml "After each design"]
                ,[(radio ! [emptyAttr "checked"]) "reports" "final"
                 ,toHtml "After entire batch"]
                ,[radio "reports" "errs"
                 ,toHtml "Only if building compiler fails"]
                ,[radio "reports" "no"
                 ,toHtml "Never"]] +++

              (toHtml "Email addresses (comma separated):") +++
              (input ! [strAttr "type" "text"
                       ,strAttr "name" "eAddrs"
                       ,intAttr "size" 40
                       ,strAttr "value" ""]) +++

              paragraph (italics (bold (toHtml "Note:") +++ toHtml " Probably only bluespec's own mail server will accept these mails, since they are sent from a machine that is not visible from the outside (bluetinder).")) +++

              br +++ br +++

              submit "action" "Start" +++
              reset "Reset form" "Reset" +++

              paragraph ((bold (toHtml "Warning:")) +++ toHtml " Before starting a batch manually, you should make sure that no other batch is already running. Otherwise you're likely to replace the version of bsc that is used in that batch. Also be aware that if your batch is not finished when the weekly scheduled batch begins, bsc will be checked out and recompiled again, possibly replacing the one used in your batch.") +++

              noHtml
              )
           )
      

          

--------------------------------------------------------------------------------
-- Execute page (the page that does the action)

executePage :: [(String,String)] -> [(String,String)] -> IO Html
executePage env queries = do
  case createBatchCfg queries of
    Nothing -> errorPage (h1 (toHtml "Error") +++
                          toHtml "Couldn't parse queries")
    Just batchCfg -> do
       scriptFile <- batchScript batchCfg
       
       let startCommand = concat ["ssh "
                                 ,"-i /export/home/synthesizer/.ssh/id_dsa bluetinder "
                                 ,"\""
                                 ,"nohup "
                                 ,scriptFile
                                 ," &> "
                                 ,"/export/home/synthesizer/public_html/errors.txt "
                                 ,"& "
                                 ,"\""]

       result <- system startCommand
       case result of
         ExitFailure n -> return $ mkPage ("Batch probably not started")
           (h1 (toHtml "Batch probably not started") +++
            toHtml "Attempted to execute the following command:" +++
            tt (toHtml startCommand) +++ 
            br +++ br +++
            toHtml ("But I got back an exit code: " ++ show n ++ ". This probably means that the batch wasn't started. If you want to find out what's wrong, I'd start by investigating if it is still possible to do an ssh as synthesizer from the webserver (which is currently lapis) to bluetinder without entering a password. Optionally, you might want to log into bluetinder and see if the batch was started anyway.")
           )
         ExitSuccess -> return $ mkPage ("Batch started") 
                 (h1 (toHtml "Batch started") +++
                  paragraph (toHtml ("The batch seems to have started! Well, I'm just a silly script, so what do I know - but at least I executed the following command line, and it didn't return failure: ") +++ tt (toHtml startCommand)) +++

                  paragraph (toHtml ("If the batch runs into any of the usual errors, you will see that on the result presentation page. Unexpected errors, and some random unpiped output is written to ") +++ hotlink "https://svn.bluespec.com:8080/~synthesizer/errors.txt" [toHtml "https://svn.bluespec.com:8080/~synthesizer/errors.txt"] +++ ". Note that this file is overwritten each time a new batch starts.") +++

                  paragraph (toHtml "You can follow the progress at " +++
                             hotlink "https://svn.bluespec.com:8080/~synthesizer/cgi-bin/results" ([toHtml "https://svn.bluespec.com:8080/~synthesizer/cgi-bin/results"]) +++
                             toHtml ". The batch will not appear in the list until compilation of bsc is done (or you request an update).") +++

                  paragraph (toHtml ("Please don't press reload on this page, since it will start an identical batch.")) +++

                  paragraph (toHtml (hotlink "https://svn.bluespec.com:8080/~synthesizer"
                                        [toHtml "Back to synthesizer main page"])) +++

                  noHtml
                 )
            

createBatchCfg :: [(String,String)] -> Maybe BatchConfig 
createBatchCfg queries = do 
  comp <- lookup "compiler" queries
  let compiler = case comp of
                   "head" -> let forceCO = maybe False ("on"==) (lookup "forceCO" queries)
                             in BCCHead forceCO
                   "rev"  -> let forceCO = maybe False ("on"==) (lookup "forceCO" queries)
                                 rev = maybe "" id (lookup "revNr" queries)
                             in BCCRev rev forceCO
                   "old"  -> BCCCurrent
                   "custom" -> let path = maybe (error "missing path to bsc") unMime 
                                                (lookup "cPath" queries)
                                   bsdir = maybe (error "missing value for BLUESPECDIR") unMime
                                                 (lookup "cBSDIR" queries)
                                   args = maybe "" unMime (lookup "cArgs" queries)
                               in BCExisting path bsdir args
                   _ -> error ("Unknown value for query \"compiler\": " ++ show comp)
  
  let designs = let designQueries = filter (\ (qry,arg) -> (qry `beginsWith` "incl_")
                                                        && (arg == "on")) queries
                in map (drop 5 . fst) designQueries
             
  reps <- lookup "reports" queries
  let reports = case reps of
                  "cont" -> BCContinuous
                  "final" -> BCFinal
                  "errs" -> BCErrors
                  "no" -> BCNothing
                  _ -> error ("Unknown value for query \"reports\": " ++ show reps)

  let addBluespecDotCom str
         | '@' `elem` str = str
         | otherwise      = str ++ "@bluespec.com"

  let reportsAddr = map addBluespecDotCom $
                    maybe [] (splitWhenAll (','==) . filter (not.isSpace) . unMime)
                             (lookup "eAddrs" queries)

  let automatic = False

  return $ BatchConfig compiler designs reports reportsAddr automatic


--------------------------------------------------------------------------------

errorPage :: Html -> IO Html
errorPage body = return $ mkPage "Error" body



