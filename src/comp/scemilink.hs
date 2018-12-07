{-# LANGUAGE CPP #-}
module Main_scemilink(main) where

-- GHC 6.12 and beyond honor the default character encoding
-- based on the current locale.  We have to set it explicitly
-- to Latin1 for backward compatibility.
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 611)
#define SET_LATIN1_ENCODING
#endif

import Exceptions(bsCatch)
import Version
import ABin(ABinModInfo(..))
import ABinUtil(getABIHierarchy, assertNoSchedErr)
import Error(internalError, EMsg, WMsg, ErrMsg(..),
             ErrorHandle, initErrorHandle,
             exitOK, exitFail, bsWarning, bsError, bsErrorNoExit,
             convErrorTToIO)
import Position(noPosition)
import Util(fromJustOrErr)
import IOUtil(getEnvDef)
import TopUtils(dfltBluespecDir)
import FileNameUtil
import Id(getIdString, mk_homeless_id)
import CSyntax(CPackage(..), CImport(..))
import ISyntax(IPackage(..))
import BinUtil(readBin, sortImports)
import MakeSymTab(mkSymTab)
import Flags(Flags(..), setVerbose)
import FlagsDecode(defaultFlags, adjustFinalFlags, splitPath)
import ListUtil(splitBy)
import SimCCBlock(SimCCBlock(..), primBlocks, pfxMod, pfxInst, pfxMeth)
import CCSyntax
import PPrint
import Authorization(checkoutSceMiLinkLicenses)
import SceMiElement

import System.Environment(getArgs)
import System.Console.GetOpt
import System.IO
--import System.IO.Unsafe(unsafePerformIO)
import System.Time
import System.Directory(getCurrentDirectory)
import Control.Monad(msum, when)
import Data.Maybe(fromMaybe, fromJust, isJust)
import Data.List(intersperse, isSuffixOf, groupBy, partition, intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Regex

-- import Debug.Trace
-- import Util(traceM)

-- Version string (matches main BSC version numbering)
versionString :: String
versionString = bluespec ++ " scemilink utility, version " ++ versionnum ++
                " (" ++ build ++ versiondate ++ ")"

-- -------------------------------------------------------------------
-- Option processing

-- Structure which holds all option settings
data Options = Options { optVerbose      :: Bool
                       , optDebug        :: Maybe FilePath
                       , optShowVersion  :: Bool
                       , optShowHelp     :: Bool
                       , optTopModule    :: Maybe String
                       , optIfcPathRaw   :: [String]
                       , optParamFile    :: Maybe FilePath
                       , optGenSim       :: Bool
                       , optSceMiClassic :: Bool
                       , optSimOut       :: Maybe FilePath
                       , optGenVlog      :: Bool
                       , optVlogOut      :: Maybe FilePath
                       , optTCPPort      :: String
                       , optWorkDir      :: FilePath
                       , optDesFeat      :: FilePath
                       , optEveMode      :: String
                       , optShowLic      :: Bool
                       }
  deriving (Show)

-- Default settings
defaultOptions :: String -> Options
defaultOptions bluespecdir =
    Options { optVerbose      = False
            , optDebug        = Nothing
            , optShowVersion  = False
            , optShowHelp     = False
            , optTopModule    = Nothing
            , optIfcPathRaw   = ifcPathRaw (defaultFlags bluespecdir)
            , optParamFile    = Nothing
            , optGenSim       = False
            , optSceMiClassic = False
            , optSimOut       = Nothing
            , optGenVlog      = False
            , optVlogOut      = Nothing
            , optTCPPort      = "4321"
            , optWorkDir      = "work"
            , optDesFeat      = "designFeatures"
            , optEveMode      = "ZTIDE"
            , optShowLic      = False
            }

-- Description of command-line options
options :: String -> [OptDescr (Options -> Options)]
options bluespecdir =
    [ Option ['v']      ["verbose"]
             (NoArg (\opts -> opts { optVerbose = True }))
             "write verbose status messages"
    , Option []         ["debug"]
             (OptArg (\mf opts -> opts { optDebug = Just (fromMaybe "" mf) }) "FILE")
             "generate very detailed debug traces"
    , Option ['V']      ["version"]
             (NoArg (\opts -> opts { optShowVersion = True }))
             "show version information and exit"
    , Option ['h','?']  ["help"]
             (NoArg (\opts -> opts { optShowHelp = True }))
             "print usage information and exit"
    , Option []         ["designfeatures"]
             (ReqArg (\s opts -> opts { optDesFeat = s }) "FILE")
             "specify the design features file location for use with EVE"
    , Option ['p']      ["path"]
             (ReqArg (\s opts -> let p = optIfcPathRaw opts
                                 in opts { optIfcPathRaw = splitPath bluespecdir p s }) "PATH")
             ".ba file search path (% = $BLUESPECDIR and + = original path)"
    , Option []         ["params"]
             (ReqArg (\s opts -> opts { optParamFile = Just s }) "FILE")
             "override the name of the generated parameters file"
    , Option []         ["port"]
             (ReqArg (\s opts -> opts { optTCPPort = s }) "PORT")
             "set the port number for the TCP link"
    , Option []         ["scemi-classic"]
             (NoArg (\opts -> opts { optSceMiClassic = True }))
             "using the Classic SceMi infrastructure mode"
    , Option []         ["sim"]
             (NoArg (\opts -> opts { optGenSim = True }))
             "generate Sce-Mi linkage files for Bluesim"
    , Option []         ["simdir"]
             (ReqArg (\s opts -> opts { optSimOut = Just s }) "DIR")
             "specify the output directory for Bluesim linkage files"
    , Option []         ["verilog"]
             (NoArg (\opts -> opts { optGenVlog = True }))
             "generate Sce-Mi linkage files for Verilog"
    , Option []         ["vdir"]
             (ReqArg (\s opts -> opts { optVlogOut = Just s }) "DIR")
             "specify the output directory for Verilog linkage files"
    , Option []         ["work"]
             (ReqArg (\s opts -> opts { optWorkDir = s }) "DIR")
             "specify the work directory for EVE ZTide execution"
    , Option []         ["zebu"]
             (NoArg (\opts -> opts { optEveMode = "ZEBU" }))
             "generate Sce-Mi parameters for Zebu emulation"
    , Option []         ["show-license-detail"]
             (NoArg (\opts -> opts { optShowLic = True }))
             "shows information regarding the license checkout"
    ]

-- Header used in usage message
usage_header :: String
usage_header = "Usage: scemilink [OPTIONS] <MODULE>"

-- Parse the command line to produce the option structure,
-- and convert it to Flags, and return any warnings or errors
parseOpts :: [String] -> String -> (Options, Flags, [WMsg], [EMsg])
parseOpts argv bluespecdir =
    let (opts,args0,errs) = getOpt Permute (options bluespecdir) argv
        options0 = foldl (flip id) (defaultOptions bluespecdir) opts
        emsgs0 = map toEMsg errs
        (options1,emsgs1) = checkMods args0 (options0,emsgs0)
        (wmsgs2, emsgs2, flags) = optsToFlags [] emsgs1 bluespecdir options1
    in (options1, flags, wmsgs2, emsgs2)
    where checkMods fs (os,es) =
              if (length fs /= 1)
              then (os,es ++ [(noPosition, EExactlyOneArgRequired "name for the top BSV module" fs)])
              else (os { optTopModule = Just (head fs) }, es)

-- Produce a standard EMsg value from an option parser error string
toEMsg :: String -> EMsg
toEMsg s = fromMaybe (noPosition, EGeneric s) $
           msum [f s | f <- [ bad_option
                            , missing_arg
                            ]]
  where bad_option_regex = mkRegex "unrecognized option `(.*)'"
        bad_option x = do [opt] <- matchRegex bad_option_regex s
                          return (noPosition, EUnknownFlag opt)
        missing_arg_regex = mkRegex "option `(.*)' requires an argument .*"
        missing_arg x = do [opt] <- matchRegex missing_arg_regex s
                           return (noPosition, EOneArgFlag opt)

-- Validate command-line and process help requests
checkCmdLine :: ErrorHandle -> Options -> String -> [WMsg] -> [EMsg] -> IO ()
checkCmdLine errh opts bluespecdir warns errs =
  do when (optShowVersion opts) $
          do hPutStrLn stdout versionString
             hPutStrLn stdout copyright
     when (optShowHelp opts) $
          hPutStrLn stdout (usageInfo usage_header (options bluespecdir))
     when ((optShowVersion opts) || (optShowHelp opts)) $
          exitOK errh
     when (not (null warns)) $
          bsWarning errh warns
     when (not (null errs)) $
          do bsErrorNoExit errh errs
             hPutStrLn stderr (usageInfo usage_header (options bluespecdir))
             exitFail errh

-- -------------------------------------------------------------------
-- Main program routine

main :: IO ()
main = do
          hSetBuffering stdout LineBuffering
          hSetBuffering stderr LineBuffering
#ifdef SET_LATIN1_ENCODING
          hSetEncoding stdout latin1
          hSetEncoding stderr latin1
#endif
          argv <- getArgs
          -- catch errors, print them nicely, and exit
          bsCatch (hmain argv)

hmain :: [String] -> IO ()
hmain argv = do
          errh <- initErrorHandle
          pwd <- getCurrentDirectory
          bluespecdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
          -- parse command line arguments
          let (opts, flags, wmsgs, emsgs) = parseOpts argv bluespecdir

          -- determine debug logging target
          dbg_target <- case (optDebug opts) of
                          Nothing   -> return Nothing
                          (Just "") -> return (Just stdout)
                          (Just f)  -> do hdl <- openFile f WriteMode
                                          return (Just hdl)

          -- write debug information for command line
          dbg dbg_target
              ((unlines [ "---- PARSING COMMAND LINE ----"
                        , "ARGS:        " ++ (show argv)
                        , "BLUESPECDIR: " ++ (show bluespecdir)
                        ]) ++
               (show opts) ++ "\n" ++
               "-------\n")

          -- handle errors and/or request for help and version info
          checkCmdLine errh opts bluespecdir wmsgs emsgs

          -- extract options and create Flags structure
          let top_mod = fromJust (optTopModule opts)
              verbose = optVerbose opts
              ba_path = ifcPath flags
              cwdname = createEncodedFullFilePath "placeholder" pwd
              cwdprefix = (dirName cwdname) ++ "/"

          -- checkout a license
          checkoutSceMiLinkLicenses errh flags

          -- read in .ba file data
          let prim_names = map sb_name primBlocks
          putStrLn "Reading design data from .ba files..."
          (_, hier_map, inst_map, _, _, _, abemis_by_name)
              <- convErrorTToIO errh $
                 getABIHierarchy errh verbose ba_path Nothing prim_names top_mod []
          abmis_by_name <- convErrorTToIO errh $ assertNoSchedErr abemis_by_name

          -- dump .ba file debug info
          dbg dbg_target
              ("---- BA FILE DATA ----\n" ++
               "INSTANCE MAP:\n" ++
               (unlines [ inst ++ " -> " ++ mod
                        | (inst,mod) <- M.toList inst_map
                        ]) ++
               "ABMI MAP:\n" ++
               (concat [ "ABMI FOR MODULE: " ++ mod ++ "\n" ++ (ppReadable abmi)
                       | (mod,abmi) <- abmis_by_name
                       ]) ++
               "-------\n")

          -- read in .bo file data
          let err = internalError "Failed to locate abmi for top module"
              top_abmi = maybe err fst (lookup top_mod abmis_by_name)
              top_pkg_name = mk_homeless_id (abmi_src_name top_abmi)
          putStrLn "Reading symbol information from .bo files..."
          (binmap, _, _, ps) <- readBin errh flags Nothing
                                    M.empty M.empty top_pkg_name
          let bininfos = let lookupFn i = fromJustOrErr "tclPackage load" $
                                            M.lookup (getIdString i) binmap
                         in  map lookupFn ps

          -- build symbol table
          let mkCImp (_, _, bo_sig, (IPackage iid _ _ _), _) =
                  -- XXX is False OK here?
                  CImpSign (getIdString iid) False bo_sig
              cimps = sortImports (map mkCImp bininfos)
              cpack = CPackage top_pkg_name (Right []) cimps [] [] []
          symtab <- mkSymTab errh cpack

          -- dump symbol table debug info
          dbg dbg_target
              ("---- SYMBOL TABLE ----\n" ++
               ppReadable symtab ++
               "-------\n")

          -- locate Sce-Mi elements in the design
          putStrLn "Identifying transactors and SCE-MI elements..."
          let scemi_map = getSceMiMap abmis_by_name

          -- write debug information on identified Sce-Mi types
          dbg dbg_target
              ("---- SCE-MI MAP ----\n" ++
               (ppReadable scemi_map) ++
               "-------\n")

          let pkg_map = M.fromList [ (mod, abmi_apkg abmi) | (mod,(abmi,_)) <- abmis_by_name ]
              scemi_els = findSceMiElements flags
                                            symtab
                                            ("",top_mod)
                                            (Nothing,[])
                                            hier_map
                                            inst_map
                                            pkg_map
                                            scemi_map
          if (S.null scemi_els)
           then putStrLn "WARNING: no Sce-Mi elements found in design!"
           else when verbose $
                  do let msg = "Found Sce-Mi elements:\n" ++
                               (unlines [ "  " ++ (fmtSceMiEl el)
                                        | el <- S.toList scemi_els
                                        ])
                     putStr msg

          -- write debug information on identified Sce-Mi elements
          dbg dbg_target
              ("---- SCE-MI ELEMENTS ----\n" ++
               unlines [ show el | el <- S.toList scemi_els ] ++
               "-------\n")

          -- assign unique channel IDs (if not already assigned)
          let isMsgPort (SceMiInPort {})  = True
              isMsgPort (SceMiOutPort {}) = True
              isMsgPort _                 = False
              msg_ports = [ (xactor el, port_name el, chan_num el)
                          | el <- S.toList scemi_els
                          , isMsgPort el
                          ]
              (pre_assigned, not_assigned) = partition (\(_,_,x) -> isJust x) msg_ports
              channel_ids0 = [ ((xact,port), n) | (xact,port,Just n) <- pre_assigned ]
              used_ids = S.fromList $ map snd channel_ids0
              unused_ids = filter (flip S.notMember used_ids) [(1::Integer)..]
              channel_ids1 = zip [(xact,port) | (xact,port,Nothing) <- not_assigned] unused_ids
              channel_ids = M.fromList $ channel_ids0 ++ channel_ids1

          -- match clock ports and controllers
          let isClock (SceMiClock {}) = True
              isClock _               = False
              clk_names_by_number = M.fromList [ (clk_num el, clk_name el)
                                               | el <- S.toList scemi_els
                                               , isClock el
                                               ]

          -- determine linkage type and ensure that all modules
          -- have the same linkage type
          let notASerial (SceMiSerial {}) = False
              notASerial _                = True
              gs = groupBy (\(lt1,_) (lt2,_) -> lt1 == lt2)
                           [ (link_type e, fmtSceMiEl e) | e <- S.toList scemi_els, notASerial e ]
              lt_errs = if (length gs <= 1)
                        then []
                        else [ toEMsg "Mixing Sce-Mi elements with different linkage types" ]
              link = if (null gs) then TCP else fst (head (head gs))

          -- for TCP link type, we need to locate the tcp_port_number register
          let tcp_port_register_candidates = if (link == TCP)
                                             then [ inst
                                                  | (inst,mod) <- M.toList inst_map
                                                  , "tcp_port_number" `isSuffixOf` inst
                                                  , mod == "RegUN"
                                                  ]
                                             else []

          -- generate parameters file
          generateParamsFile opts top_mod scemi_els channel_ids clk_names_by_number link

          -- generate C++ Sce-Mi connection file
          sim_errs <- if (optGenSim opts)
                      then if (link == TCP)
                           then if ((length tcp_port_register_candidates) == 1)
                                then do generateBluesimCode opts cwdprefix top_mod scemi_els channel_ids (head tcp_port_register_candidates)
                                        return []
                                else do let msg = "Failed to identify a unique tcp_port_number register.\nCandidates: " ++ (show tcp_port_register_candidates)
                                        return [ toEMsg msg ]
                           else do let msg = "--sim option is not valid with " ++
                                             (show link) ++
                                             " linkage type"
                                   return [ toEMsg msg ]
                      else return []

          -- generate Verilog Sce-Mi connection file
          vlog_errs <- if (optGenVlog opts)
                       then if (link == TCP)
                            then if ((length tcp_port_register_candidates) == 1)
                                 then do generateVerilogCode opts cwdprefix scemi_els channel_ids (head tcp_port_register_candidates)
                                         return []
                                 else do let msg = "Failed to identify a unique tcp_port_number register.\nCandidates: " ++ (show tcp_port_register_candidates)
                                         return [ toEMsg msg ]
                            else do let msg = "--verilog option is not valid with " ++
                                              (show link) ++
                                              " linkage type"
                                    return [ toEMsg msg ]
                       else return []

          -- print errors and exit
          let errs = lt_errs ++ sim_errs ++ vlog_errs
          if (null errs)
           then exitOK errh
           else bsError errh errs

-- -------------------------------------------------------------------
-- Parameter file generation

generateParamsFile :: Options -> String -> S.Set SceMiElement ->
                      M.Map (String,String) Integer ->
                      M.Map Integer String -> SceMiLinkType -> IO ()
generateParamsFile opts top_mod scemi_els channel_ids clk_names_by_number lt =
    do ct <- getClockTime
       now <- toCalendarTime ct
       let defaultParamFile = top_mod ++ ".params"
           paramFile = fromMaybe defaultParamFile (optParamFile opts)
           dateString = calendarTimeToString now
           header = unlines $ [ "// Sce-Mi parameters file generated on: " ++ dateString
                              , "// By: " ++ versionString
                              , ""
                              , "// ObjectKind Index AttributeName Value"
                              ]
           elemPrefix cmap el@(SceMiInPort {}) =
               let n = M.findWithDefault (0::Integer) "MessageInPort" cmap
                   cmap' = M.insert "MessageInPort" (n+1) cmap
               in (cmap', "MessageInPort " ++ (show n))
           elemPrefix cmap el@(SceMiOutPort {}) =
               let n = M.findWithDefault (0::Integer) "MessageOutPort" cmap
                   cmap' = M.insert "MessageOutPort" (n+1) cmap
               in (cmap', "MessageOutPort " ++ (show n))
           elemPrefix cmap el@(SceMiInputPipe {}) =
               let n = M.findWithDefault (0::Integer) "InputPipe" cmap
                   cmap' = M.insert "InputPipe" (n+1) cmap
               in (cmap', "InputPipe " ++ (show n))
           elemPrefix cmap el@(SceMiOutputPipe {}) =
               let n = M.findWithDefault (0::Integer) "OutputPipe" cmap
                   cmap' = M.insert "OutputPipe" (n+1) cmap
               in (cmap', "OutputPipe " ++ (show n))
           elemPrefix cmap el@(SceMiClock {}) =
               let n = M.findWithDefault (0::Integer) "Clock" cmap
                   cmap' = M.insert "Clock" (n+1) cmap
               in (cmap', "Clock " ++ (show n))
           elemPrefix cmap el@(SceMiClockBinding {}) =
               let n = M.findWithDefault (0::Integer) "ClockBinding" cmap
                   cmap' = M.insert "ClockBinding" (n+1) cmap
               in (cmap', "ClockBinding " ++ (show n))
           elemPrefix cmap el@(SceMiSerial {}) =
               let n = M.findWithDefault (0::Integer) "Serial" cmap
                   cmap' = M.insert "Serial" (n+1) cmap
               in (cmap', "Serial " ++ (show n))
--           elemPrefix cmap _ = (cmap, "?")
           elemDesc (counts,groups) el@(SceMiInPort {}) =
               let (counts',pfx) = elemPrefix counts el
                   chan = M.findWithDefault 0 (xactor el, port_name el) channel_ids
                   grp = [ pfx ++ " TransactorName \"" ++ (xactor el) ++ "\""
                         , pfx ++ " PortName       \"" ++ (port_name el) ++ "\""
                         , pfx ++ " PortWidth      " ++ (show (port_size el))
                         , pfx ++ " ChannelId      " ++ (show chan)
                         , pfx ++ " Type           " ++ (show (port_type el))
                         ]
               in (counts',grp:groups)
           elemDesc (counts,groups) el@(SceMiOutPort {}) =
               let (counts',pfx) = elemPrefix counts el
                   chan = M.findWithDefault 0 (xactor el, port_name el) channel_ids
                   grp = [ pfx ++ " TransactorName \"" ++ (xactor el) ++ "\""
                         , pfx ++ " PortName       \"" ++ (port_name el) ++ "\""
                         , pfx ++ " PortWidth      " ++ (show (port_size el))
                         , pfx ++ " ChannelId      " ++ (show chan)
                         , pfx ++ " Type           " ++ (show (port_type el))
                         ]
               in (counts',grp:groups)
           elemDesc (counts,groups) el@(SceMiInputPipe {}) =
               let (counts',pfx) = elemPrefix counts el
                   grp = [ pfx ++ " TransactorName \"" ++ (xactor el) ++ "\""
                         , pfx ++ " PipeName       \"" ++ (pipe_name el) ++ "\""
                         , pfx ++ " PipeWidth      " ++ (show (pipe_size el))
                         , pfx ++ " PipeDepth      " ++ (show (pipe_depth el))
                         , pfx ++ " Visibility     \"" ++ (pipe_visibility el) ++ "\""
                         , pfx ++ " PipeNum        " ++ (show (pipe_num el))
                         , pfx ++ " Type           " ++ (show (pipe_type el))
                         ]
               in (counts',grp:groups)
           elemDesc (counts,groups) el@(SceMiOutputPipe {}) =
               let (counts',pfx) = elemPrefix counts el
                   grp = [ pfx ++ " TransactorName \"" ++ (xactor el) ++ "\""
                         , pfx ++ " PipeName       \"" ++ (pipe_name el) ++ "\""
                         , pfx ++ " PipeWidth      " ++ (show (pipe_size el))
                         , pfx ++ " PipeDepth      " ++ (show (pipe_depth el))
                         , pfx ++ " Visibility     \"" ++ (pipe_visibility el) ++ "\""
                         , pfx ++ " PipeNum        " ++ (show (pipe_num el))
                         , pfx ++ " Type           " ++ (show (pipe_type el))
                         ]
               in (counts',grp:groups)
           elemDesc (counts,groups) el@(SceMiClock {}) =
               let (counts',pfx) = elemPrefix counts el
                   grp = [ pfx ++ " ClockName          \"" ++ (clk_name el) ++ "\""
                         , pfx ++ " RatioNumerator     " ++ (show (clk_numerator el))
                         , pfx ++ " RatioDenominator   " ++ (show (clk_denominator el))
                         , pfx ++ " DutyHi             " ++ (show (clk_duty_hi el))
                         , pfx ++ " DutyLo             " ++ (show (clk_duty_lo el))
                         , pfx ++ " Phase              " ++ (show (clk_phase el))
                         , pfx ++ " ResetCycles        " ++ (show (clk_reset_cycles el))
                         ] ++
                         case (clk_group el) of
                           Just n  -> [pfx ++ " ClockGroup         " ++ (show n)]
                           Nothing -> []
               in (counts',grp:groups)
           elemDesc (counts,groups) el@(SceMiClockBinding {}) =
               let (counts',pfx) = elemPrefix counts el
                   name = M.findWithDefault "?" (clk_num el) clk_names_by_number
                   grp = [ pfx ++ " TransactorName \"" ++ (xactor el) ++ "\""
                         , pfx ++ " ClockName      \"" ++ name ++ "\""
                         ]
               in (counts',grp:groups)
           elemDesc (counts,groups) el@(SceMiSerial {}) =
               let (counts',pfx) = elemPrefix counts el
                   grp = [ pfx ++ " Path           " ++ (show (ser_path el))
                         , pfx ++ " PrbNum         " ++ (show (ser_num el))
                         , pfx ++ " Label          " ++ (show (ser_label el))
                         , pfx ++ " Kind           " ++ (show (ser_kind el))
                         , pfx ++ " Samples        " ++ (show (ser_samples el))
                         , pfx ++ " Offset         " ++ (show (ser_offset el))
                         , pfx ++ " Width          " ++ (show (ser_width el))
                         , pfx ++ " Type           " ++ (show (ser_type el))
                         ]
               in (counts',grp:groups)
--           elemDesc (counts,groups) _ = (counts,groups)
           el_groups = snd $ foldl elemDesc (M.empty,[]) (S.toList scemi_els)
           elemStr = unlines $ concat $ intersperse [""] el_groups
           linkStr = case lt of
                       TCP -> let port = optTCPPort opts
                              in unlines [ "Link 0 LinkType   \"TCP\""
                                         , "Link 0 TCPAddress \"127.0.0.1\""
                                         , "Link 0 TCPPort    " ++ port
                                         ]
                       EVE -> let work = optWorkDir opts
                                  df = optDesFeat opts
                                  eve_mode = optEveMode opts
                              in unlines [ "Link 0 LinkType \"EVE\""
                                         , "Link 0 EveWorkDir " ++ (show work)
                                         , "Link 0 EveDesignFeaturesFile " ++ (show df)
                                         , "Link 0 EveSimMode " ++ (show eve_mode)
                                         ]
                       PCIE_VIRTEX5 ->
                              let link_name = "PCIE_VIRTEX5"
                                  dev_name  = "/dev/bluenoc_1"
                                  link      = "Link 0 LinkType " ++ (show link_name)
                                  dev       = "Link 0 DeviceName " ++ (show dev_name)
                              in  if optSceMiClassic opts
                                  then unlines [ link ]
                                  else unlines [ link, dev ]
                       PCIE_VIRTEX6 ->
                              let link_name = "PCIE_VIRTEX6"
                                  dev_name  = "/dev/bluenoc_1"
                                  link      = "Link 0 LinkType " ++ (show link_name)
                                  dev       = "Link 0 DeviceName " ++ (show dev_name)
                              in  if optSceMiClassic opts
                                  then unlines [ link ]
                                  else unlines [ link, dev ]
                       PCIE_DINI ->
                              let link_name = "PCIE_DINI"
                                  dev_name  = "/dev/bluenoc_1"
                                  link      = "Link 0 LinkType " ++ (show link_name)
                                  dev       = "Link 0 DeviceName " ++ (show dev_name)
                              in  if optSceMiClassic opts
                                  then unlines [ link ]
                                  else unlines [ link, dev ]
                       PCIE_KINTEX7 ->
                              let link_name = "PCIE_KINTEX7"
                                  dev_name  = "/dev/bluenoc_1"
                                  link      = "Link 0 LinkType " ++ (show link_name)
                                  dev       = "Link 0 DeviceName " ++ (show dev_name)
                              in  if optSceMiClassic opts
                                  then unlines [ link ]
                                  else unlines [ link, dev ]
                       PCIE_VIRTEX7 ->
                              let link_name = "PCIE_VIRTEX7"
                                  dev_name  = "/dev/bluenoc_1"
                                  link      = "Link 0 LinkType " ++ (show link_name)
                                  dev       = "Link 0 DeviceName " ++ (show dev_name)
                              in  if optSceMiClassic opts
                                  then unlines [ link ]
                                  else unlines [ link, dev ]
                       PCIE_VIRTEXU ->
                              let link_name = "PCIE_VIRTEXU"
                                  dev_name  = "/dev/bluenoc_1"
                                  link      = "Link 0 LinkType " ++ (show link_name)
                                  dev       = "Link 0 DeviceName " ++ (show dev_name)
                              in  if optSceMiClassic opts
                                  then unlines [ link ]
                                  else unlines [ link, dev ]
                       PCIE_ARRIA10 ->
                              let link_name = "PCIE_ARRIA10"
                                  dev_name  = "/dev/bluenoc_1"
                                  link      = "Link 0 LinkType " ++ (show link_name)
                                  dev       = "Link 0 DeviceName " ++ (show dev_name)
                              in  if optSceMiClassic opts
                                  then unlines [ link ]
                                  else unlines [ link, dev ]
                       _   -> ""
           paramStr  = concat $ intersperse "\n" [header, elemStr, linkStr]
       putStrLn $ "Writing parameters to file " ++ paramFile ++ "..."
       writeFile paramFile paramStr

-- -------------------------------------------------------------------
-- Bluesim C++ file generation

generateBluesimCode :: Options -> String -> String ->
                       S.Set SceMiElement -> M.Map (String,String) Integer -> String -> IO ()
generateBluesimCode opts prefix top_mod scemi_els channel_ids port_number_reg =
    do let cName    = mkCxxName (optSimOut opts) prefix "scemilink"
           hName    = mkHName   (optSimOut opts) prefix "scemilink"
           cNameRel = getRelativeFilePath cName
       putStrLn $ "Writing Bluesim Sce-Mi linkage files: " ++ (dropSuf cNameRel) ++
                  ".{" ++ cxxSuffix ++ "," ++ hSuffix ++ "}..."
       let setup_fn    = function void (mkVar "scemi_setup")
                             [ (userType "tSimStateHdl") (mkVar "simHdl") ]
           shutdown_fn = function void (mkVar "scemi_shutdown")
                             [ (userType "tSimStateHdl") (mkVar "simHdl") ]
           h_fragments = [ cpp_include "bluesim_types.h"
                         , blankLines 1
                         , decl setup_fn
                         , decl shutdown_fn
                         ]
       writeFile hName (ppReadable (program [ protect_header "scemilink" h_fragments]))
       let top_mod_header = mkHName Nothing "" top_mod
           top_mod_type  = ptrType $ classType (pfxMod ++ top_mod)
           channel_id_stmts =
             if optSceMiClassic opts
             then concatMap (setChannelId channel_ids) (S.toList scemi_els)
             else []
           port = read (optTCPPort opts)
           port_number_const = intercalate "." [ "INST_" ++ s | s <- splitBy (== '.') port_number_reg ]
           link_setup_stmts =
             if optSceMiClassic opts
             then [ comment "Open TCP socket" $
                    stmt $ (var "bscemi_open_socket") `cCall` [mkUInt32 port, mkUInt32 0]
                  ]
             else [ comment "Set TCP port number" $
                    stmt $ (var "top") `cArrow` port_number_const `cDot` "METH_write" `cCall` [mkUInt32 port]
                  ]
           link_shutdown_stmts =
             if optSceMiClassic opts
             then [ comment "Close TCP socket" $
                    stmt $ (var "bscemi_close_socket") `cCall` []
                  ]
             else []
           setup_defn    = define setup_fn
                           (block $ [ decl $ (mkVar "top") `ofType` top_mod_type
                                    , (mkVar "top") `assign`
                                          (top_mod_type `cCast`
                                               ((var "bk_get_model_instance") `cCall` [var "simHdl"]))
                                    ] ++
                                    channel_id_stmts ++
                                    link_setup_stmts
                           )
           shutdown_defn = define shutdown_fn
                           (block $ link_shutdown_stmts)
           c_fragments = [ cpp_include "scemilink.h"
                         , cpp_include "bluesim_kernel_api.h"
                         , cpp_include top_mod_header
                         , blankLines 1
                         ] ++
                         (if optSceMiClassic opts
                             then [ comment "defined in libscemiHW" (blankLines 0)
                                  , externC [ decl $ function void (mkVar "bscemi_open_socket")
                                                              [ unsigned $ CCSyntax.int (mkVar "port")
                                                              , unsigned $ CCSyntax.int (mkVar "autoclose")
                                                              ]
                                            , decl $ function void (mkVar "bscemi_close_socket") []
                                            ]
                                  , blankLines 1
                                  ]
                             else []) ++
                         [ setup_defn
                         , shutdown_defn
                         ]
       writeFile cName (ppReadable (program c_fragments))


setChannelId :: M.Map (String,String) Integer -> SceMiElement -> [CCFragment]
setChannelId channel_ids el@(SceMiInPort {}) =
    let chan = M.findWithDefault 0 (xactor el, port_name el) channel_ids
        segments = splitBy (=='.') (xactor el)
        xactfn = foldl (\fn s -> cDot (fn (pfxInst ++ s))) (cArrow (var "top")) segments
        meth = xactfn (pfxInst ++ (port_name el ++ "_index")) `cDot` (pfxMeth ++ "write")
    in [ stmt $ meth `cCall` [mkUInt32 chan] ]
setChannelId channel_ids el@(SceMiOutPort {}) =
    let chan = M.findWithDefault 0 (xactor el, port_name el) channel_ids
        segments = splitBy (=='.') (xactor el)
        xactfn = foldl (\fn s -> cDot (fn (pfxInst ++ s))) (cArrow (var "top")) segments
        meth = xactfn (pfxInst ++ (port_name el ++ "_index")) `cDot` (pfxMeth ++ "write")
    in [ stmt $ meth `cCall` [mkUInt32 chan] ]
setChannelId _ _ = []

-- -------------------------------------------------------------------
-- Verilog file generation

generateVerilogCode :: Options -> String -> S.Set SceMiElement ->
                       M.Map (String,String) Integer -> String -> IO ()
generateVerilogCode opts prefix scemi_els channel_ids port_number_reg =
    do let vName    = (mkPre (optVlogOut opts) prefix) ++ "scemilink.vlog_fragment"
           vNameRel = getRelativeFilePath vName
       putStrLn $ "Writing Verilog Sce-Mi linkage file: " ++ vNameRel ++ "..."
       let channel_id_stmts =
             if optSceMiClassic opts
             then [ "      // Assign virtual channel numbers" ] ++
                  (concatMap (assignChannelId channel_ids) (S.toList scemi_els))
             else []
           port = optTCPPort opts
           link_setup_stmts =
             if optSceMiClassic opts
             then [ ""
                  , "      // open TCP socket"
                  , "      $imported_bscemi_open_socket(" ++ port ++ ",1);"
                  ]
             else [ ""
                  , "      // Set TCP port number"
                  , "      top." ++ port_number_reg ++ " = " ++ port ++ ";"
                  ]
           contents =
               unlines $ [ "  initial"
                         , "    begin"
                         , "      #0;"
                         ] ++
                         channel_id_stmts ++
                         link_setup_stmts ++
                         [ "    end" ]
       writeFile vName contents

assignChannelId :: M.Map (String,String) Integer -> SceMiElement -> [String]
assignChannelId channel_ids el@(SceMiInPort {}) =
    let chan = M.findWithDefault 0 (xactor el, port_name el) channel_ids
        name = (joinStrings "." [port_name el, xactor el, "top"]) ++ "_index"
    in [ "      " ++ name ++ " = " ++ (show chan) ++ ";" ]
assignChannelId channel_ids el@(SceMiOutPort {}) =
    let chan = M.findWithDefault 0 (xactor el, port_name el) channel_ids
        name = (joinStrings "." [port_name el, xactor el, "top"]) ++ "_index"
    in [ "      " ++ name ++ " = " ++ (show chan) ++ ";" ]
assignChannelId _ _ = []

-- -------------------------------------------------------------------
-- Utility functions

-- Note: This assumes that list needs to be reversed!
joinStrings :: String -> [String] -> String
joinStrings sep ss =
    let ss' = [ s | s <- ss, not (null s), (head s) /= '_' ]
    in  concat (intersperse sep (reverse ss'))

-- Write to a debug log, if one is active
dbg :: (Maybe Handle) -> String -> IO ()
dbg Nothing    _ = return ()
dbg (Just hdl) s = do hPutStr hdl s
                      hFlush hdl

{-
-- Like dbg, but used in non-IO context (uses unsafePerformIO)
{-# NOINLINE dbg_trace #-}
dbg_trace :: (Maybe Handle) -> String -> a -> a
dbg_trace Nothing    _ x = x
dbg_trace (Just hdl) s x = unsafePerformIO $ do hPutStr hdl s
                                                hFlush hdl
                                                return x
-}

-- Build Flags structure from Options
optsToFlags :: [WMsg] -> [EMsg] -> String -> Options -> ([WMsg], [EMsg], Flags)
optsToFlags ws0 es0 bluespecDir opts =
    let flags0 = defaultFlags bluespecDir
        flags1 = setVerbose (optVerbose opts) flags0
        flags2 = flags1 { genSceMiClassic = optSceMiClassic opts
                        , ifcPathRaw = optIfcPathRaw opts
                        , cdir    = (optSimOut opts)
                        , vdir    = (optVlogOut opts)
                        , showLicense = (optShowLic opts)
                        }
    in adjustFinalFlags ws0 es0 flags2

-- -------------------------------------------------------------------
