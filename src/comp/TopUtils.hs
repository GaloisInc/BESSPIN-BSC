{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, CPP #-}
module TopUtils where
-- Haskell libs
import System.Process(runInteractiveCommand)
import System.Directory(getDirectoryContents, doesDirectoryExist)
import Text.Printf(printf)
import System.IO(hFlush, hGetLine, stdout)
import System.CPUTime(getCPUTime)
import qualified Control.Exception as CE
import Control.Monad(when, unless, filterM)
import Control.Monad.Trans(MonadIO(..))
import Data.List((\\))
import System.Time -- XXX: from old-time package
-- hbc libs
import PFPrint
-- utility libs
import Util(itos)
import FileNameUtil(baseName, dropSuf)
import FileIOUtil(writeFileCatch)

-- compiler libs
import Flags( Flags(..), verbose, quiet,
             DumpFlag(..), dumpInfo)
-- import CSyntax
import CVPrint
import IdPrint
import ISyntax(IPackage(..), IModule(..),
               IStateVar(..), IRules(..))
import ASyntax(APackage(..), ASPackage(..), ARule(..),
               aIfaceName)
import SystemVerilogTokens(SV_Token(..))
import Version(bluespec, version)

import Position(noPosition)
import Error(ErrMsg(..), ErrorHandle, bsError, exitOK)

import Eval
--import Trace

id_bsc = " $Id$"

#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ >= 609)
type ExceptionType = CE.SomeException
#else
type ExceptionType = CE.Exception
#endif


dfltBluespecDir = "/usr/local/lib/" ++ bluespec
dfltVSim = "iverilog"
dfltMACRODEF = "-D"

dfltCCompile = "cc"
dfltCxxCompile = "c++"
dfltCFLAGS = "-O3"
dfltBSC_CFLAGS = "-Wall -Wno-unused -D_FILE_OFFSET_BITS=64"
dfltCXXFLAGS = "-O3"
dfltBSC_CXXFLAGS = "-Wall -Wno-unused -D_FILE_OFFSET_BITS=64"

dfltMake = "make"
-- MAKEFLAGS is a reserved variable that make uses for recursive calls;
-- it should not be explicitly added to calls to 'make'
--dfltMAKEFLAGS = ""
dfltBSC_MAKEFLAGS = ""

-- Make a call to bsenv
getBSEnv :: ErrorHandle -> Flags -> String -> IO String
getBSEnv errh flags args =
  do let bsdir = bluespecDir flags
         cmd   = bsdir ++ "/bin/bsenv " ++ args
     (_,cmd_out,_,_) <- runInteractiveCommand cmd
     let handler :: ExceptionType -> IO String
         handler _ = return "script error"
     CE.catch (hGetLine cmd_out) handler

-- the supported C++ compiler families are determined by looking at all of the
-- subdirectories in the $BLUESPECDIR/Bluesim/ area.
supportedCxxFamilies :: Flags -> IO [String]
supportedCxxFamilies flags =
  do let bsdir = bluespecDir flags
     let handler :: ExceptionType -> IO [String]
         handler _ = return []
     contents <- CE.catch (getDirectoryContents (bsdir ++ "/Bluesim")) handler
     let contents' = contents \\ [".", ".."]
     filterM (\d -> doesDirectoryExist (bsdir ++ "/Bluesim/" ++ d)) contents'

-- Test the c++ compiler version to determine the ABI family name.
-- If the family is not a supported family, this will issue an error
-- to the user and abort the compilation.
getCxxFamily :: ErrorHandle -> Flags -> IO String
getCxxFamily errh flags =
  do version <- getBSEnv errh flags "c++_family"
     supported <- supportedCxxFamilies flags
     if version `elem` supported
      then
        return version
      else
        do _ <- bsError errh [(noPosition,EBluesimBadCxxFamily version supported)]
           return "unknown"

fmtDouble :: Double -> String
fmtDouble = printf "%.2f"

start :: Flags -> DumpFlag -> IO ()
start flags d = when (verbose flags) (putStrLnF ("starting " ++ drop 2 (show d)) >> hFlush stdout)

type DumpNames = (String {- file name (last path component) -}, String {- package name -}, String {- module name or empty -})

dump :: (PPrint a, Hyper a) =>
        ErrorHandle -> Flags -> TimeInfo -> DumpFlag -> DumpNames -> a
     -> IO TimeInfo
dump errh flags t d names a =
        hyper a $	-- force evaluation
        dumpStr errh flags t d names (ppReadable a)

ddump :: (PPrint a, Hyper a) =>
        ErrorHandle -> Flags -> TimeInfo -> DumpFlag -> DumpNames -> a
     -> IO TimeInfo
ddump errh flags t d names a =
        hyper a $	-- force evaluation
        dumpStr errh flags t d names (ppDebug a)

vdump :: (PVPrint a, Hyper a) =>
        ErrorHandle -> Flags -> TimeInfo -> DumpFlag -> DumpNames -> a
     -> IO TimeInfo
vdump errh flags t d names a =
        hyper a $	-- force evaluation
        dumpStr errh flags t d names (pvpReadable a)


sdump :: (Show a, Hyper a) =>
        ErrorHandle -> Flags -> TimeInfo -> DumpFlag -> DumpNames -> a
     -> IO TimeInfo
sdump errh flags t d names a =
        hyper a $	-- force evaluation
        dumpStr errh flags t d names (show a)


dumpStr :: ErrorHandle -> Flags -> TimeInfo -> DumpFlag -> DumpNames -> String
        -> IO TimeInfo
dumpStr errh flags t d names@(file, pkg, mod) a = do
    -- the name of this stage
    let sname = drop 2 (show d)
    -- first, dump the info appropriately
    case (dumpInfo flags d) of
        Just (Just file) -> do
            writeFileCatch errh (substNames names file) a
            when (verbose flags) $ putStrLnF (sname ++ " done")
        Just Nothing -> do
            unless (quiet flags) $ putStrLnF ("=== " ++ sname ++ ":\n" ++ a ++ "\n-----\n")
        Nothing -> do
            when (verbose flags) $ putStrLnF (sname ++ " done")
    -- second, dump the timestamp (and get the new time)
    t' <- timestamp flags t
    -- finally, decide whether to exit here
    case (kill flags) of
        (Just (pass, Nothing))
          | pass == d -> do
             putStrLnF ("\ncompilation stopped because of -KILL" ++
                        sname ++ " flag")
             exitOK errh
        (Just (pass, Just pkg_or_mod))
          | pass == d && (pkg_or_mod == pkg || pkg_or_mod == mod) -> do
             putStrLnF ("\ncompilation stopped because of -KILL" ++
                        sname ++ "=" ++ pkg_or_mod ++ " flag")
             exitOK errh
        _ -> -- don't exit here, return the new time
             return t'

substNames _ "" = ""
substNames names@(file,pkg,mod) ('%':c:cs) = subst ++ substNames names cs
    where subst = case c of
                  '%' -> "%"
                  'f' -> file
                  'p' -> pkg
                  'm' -> mod
                  c'  -> [c']
substNames names (c:cs) = c : substNames names cs

timestamp :: Flags -> TimeInfo -> IO TimeInfo
timestamp flags t = do
        t' <- getNow
        when (verbose flags) $
            printElapsed t t'
        return t'

-- a simplfiied "dumpStr" which only prints a title and the timestamp
timestampStr :: Flags -> String -> TimeInfo -> IO TimeInfo
timestampStr flags title t = do
        t' <- getNow
        when (verbose flags) $ do
            putStrLnF title
            printElapsed t t'
        return t'

withElapsed :: MonadIO m => m a -> m a
withElapsed m = do
  t   <- liftIO getNow
  res <- m
  t'  <- liftIO getNow
  liftIO $ printElapsed t t'
  return res

printElapsed :: TimeInfo -> TimeInfo -> IO ()
printElapsed t t' = do
        let (dc, dr) = diffTimeInfo t t'
        putStrLnF (" elapsed time: CPU " ++ fmtDouble dc ++ "s, real " ++ fmtDouble dr ++ "s\n")


stats :: (Stats a) => Flags -> DumpFlag -> a -> IO ()
stats flags df a =
    if not (showStats flags) then
        return ()
    else do
        putStrLn (pretty 78 78 (text ("stats " ++ drop 2 (show df) ++ ":") $+$ (text "  " <> pStats (verbose flags) a)))

--hyperEval x = hyper x (return ())


-----------------------------------------------------------------------------------------------------
putInDir :: Maybe String -> String -> String -> String
putInDir Nothing name suf = dropSuf name ++ "." ++ suf
putInDir (Just d) name suf = d ++ "/" ++ baseName (dropSuf name) ++ "." ++ suf

-----

commentC ls = unlines (["/*"] ++ map (" * " ++) ls ++ [" */"])
commentV ls = unlines (["//"] ++ map ("// " ++) ls ++ ["//"])

-----

getCPUTimeDoublePortable :: IO Double
getCPUTimeDoublePortable = do
        t <- getCPUTime
        return (fromInteger t * 1.0e-12)

data TimeInfo = TimeInfo Double ClockTime
        deriving (Show)

getNow :: IO TimeInfo
getNow = do
        t <- getCPUTimeDoublePortable
        ct <- getClockTime
        return (TimeInfo t ct)

diffTimeInfo :: TimeInfo -> TimeInfo -> (Double, Double)
diffTimeInfo (TimeInfo t ct) (TimeInfo t' ct') = (t'-t, tdToDouble (diffClockTimes ct' ct))
  where tdToDouble d = fromIntegral ((tdHour d * 60 + tdMin d) * 60 + tdSec d) + fromInteger (tdPicosec d) * 1.0e-12

putStrLnF s = do putStrLn s; hFlush stdout

-----

class Stats a where
    pStats :: Bool -> a -> Doc

instance Stats CPackage where
    pStats _ (CPackage _ _ _ _ ds _) =
        showLen ds "definitions:" $+$
        (text "  " <> showLen [ () | CValueSign _ <- ds ] "values")

instance Stats (IPackage a) where
    pStats _ (IPackage _ _ _ ds) =
        showLen ds "definitions"

instance Stats (IModule a) where
    pStats _ (IModule { imod_name        = i,
                        imod_wire_args   = as,
                        imod_state_insts = vs,
                        imod_local_defs  = ds,
                        imod_rules       = rs,
                        imod_interface   = ifc }) =
        (text "module" <+> ppId PDReadable i) $+$
        (text "  " <> (
        showLen as "arguments" $+$
        showLen vs "state elements" $+$
        (text "  " <> showLen [ () | (_, IStateVar { isv_is_arg = False }) <- vs ] "interface arguments") $+$
        showLen ds "local definitions" $+$
        showCnt (countRules rs) "rules" $+$
        showLen ifc "interface methods"
        ))
          where countRules (IRules _ rs) = length rs

instance Stats APackage where
    pStats v apkg =
        (text "module" <+> ppId PDReadable (apkg_name apkg)) $+$
        (text "  " <> (
        showLen (apkg_size_params apkg) "arguments" $+$
        showLen (apkg_state_instances apkg) "state elements" $+$
        showLen (apkg_local_defs apkg) "local definitions" $+$
        (showLen (apkg_rules apkg) "rules" <>
         if v then text "" <+>pPrint PDReadable 0 [ i | ARule { arule_id = i } <- apkg_rules apkg ] else text "") $+$
        (showLen (apkg_interface apkg) "interface methods" <>
         if v then text "" <+> pPrint PDReadable 0 (map aIfaceName (apkg_interface apkg)) else text "")
        ))

instance Stats ASPackage where
    pStats _ (ASPackage i _ _ os is ios vs sos ds _ fs _ _ _) =
        (text "module" <+> ppId PDReadable i) $+$
        (text "  " <> (
        showLen os "outputs" $+$
        showLen is "inputs" $+$
        showLen ios "inouts" $+$
        showLen vs "state elements" $+$
        showLen sos "state elements outputs" $+$
        showLen ds "local definitions" $+$
        showLen fs "foreign function calls"))

instance Stats [SV_Token] where
    pStats _ toks = showLen toks "tokens"

-- verilog preprocessor output
newtype VPPOut = VPPOut (String, [String])
  deriving(Hyper)

instance PPrint VPPOut where
  pPrint d p (VPPOut (source,includes)) = text source

instance Stats VPPOut where
    pStats _ (VPPOut (source,includes)) =
      showLen (lines source) "post-preprocessing source lines" $+$
      showLen includes "included files"

showLen :: [a] -> String -> Doc
showLen xs s = showCnt (length xs) s

showCnt :: Int -> String -> Doc
showCnt cnt s = text $ itos cnt ++ " " ++ s

-- ------------

-- | Makes a clock suffix, respecting the timeStamps flag
getClockSuffix :: Flags -> IO String
getClockSuffix flags
  | timeStamps flags = getClockTime >>= return . ("On " ++) . show
  | otherwise        = return ""

mkGeneratedCComment :: Flags -> IO [String]
mkGeneratedCComment flags = do
  suffix <- getClockSuffix flags
  return $ ["Generated by " ++ version, "", suffix, ""]
