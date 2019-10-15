{-# LANGUAGE BangPatterns, CPP #-}
module Main_bsc(id_bsc, main, hmain) where

-- hack around base-3 and base-4 incompatibility
#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ >= 609)
#define NEW_EXCEPTION_API
#endif

-- GHC 6.12 and beyond honor the default character encoding
-- based on the current locale.  We have to set it explicitly
-- to Latin1 for backward compatibility.
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 611)
#define SET_LATIN1_ENCODING
#endif

-- Haskell libs
import Prelude
import System.Environment(getArgs, getProgName)
import System.IO(stdout, stderr, hSetBuffering, BufferMode(LineBuffering))
#ifdef SET_LATIN1_ENCODING
import System.IO(hSetEncoding, latin1)
#endif
import System.Directory(getCurrentDirectory, createDirectoryIfMissing)
import System.Time(getClockTime) -- XXX: from old-time package
import Data.List(intersect, intersperse, unzip5)
import Data.Time.Clock.POSIX(getPOSIXTime)

import qualified Data.ByteString as BS
import System.FilePath (takeFileName)

import Control.Monad(when, foldM)
import qualified Data.Map as M

import ListMap(lookupWithDefault)
import SCC(scc)

-- utility libs
import ParseOp
import PFPrint
import Util(headOrErr, fromJustOrErr)
import FileNameUtil(baseName, hasDotSuf, dropSuf, dirName,
                    bscSrcSuffix, bseSrcSuffix, binSuffix,
                    createEncodedFullFilePath,
                    getRelativeFilePath)
import TopUtils
import SystemCheck(doSystemCheck)
import IOUtil(getEnvDef)

-- compiler libs
--import FStringCompat
import Exceptions(bsCatch)
import Flags(
        Flags(..),
        DumpFlag(..),
        hasDump,
        verbose, quiet)
import FlagsDecode(
        Decoded(..),
        decodeArgs,
        showFlags,
        showFlagsRaw,
        exitWithUsage,
        exitWithHelp,
        exitWithHelpHidden)
import Error(internalError, ErrMsg(..),
             ErrorHandle, initErrorHandle, setErrorHandleFlags,
             bsError, bsWarning, exitFail, exitOK)
import Position(noPosition)
import CVPrint
import Id
import Deriving(derive)
import SymTab
import MakeSymTab(mkSymTab, cConvInst)
import TypeCheck(cCtxReduceIO, cTypeCheck)
import GenSign(genUserSign, genEverythingSign)
import Simplify(simplify)
import ISyntax(IPackage(..), IDef(..), IExpr(..), fdVars)
import ISyntaxUtil(iMkRealBool, iMkLitSize, iMkString)
import IConv(iConvPackage)
import FixupDefs(fixupDefs)
import ISyntaxCheck(tCheckIPackage)
import ISimplify(iSimplify)
import BinUtil(BinMap, HashMap, readImports, replaceImports)
import GenBin(genBinFile)
import GenWrap(genWrap, WrapInfo(..))
import GenFuncWrap(genFuncWrap, addFuncWrap)
import GenForeign(genForeign)
import IExpandUtils(HeapData)
import Depend
import Version(version, copyright, buildnum)
import Authorization(expiry, getReportedBSCFeatures,
                     checkoutBSCLicenses, BSCLicMode(..), checkinAllLicenses)
import Classic
import SAT(checkSATFlags)

-- Required for BESSPIN AST export
import qualified MakeSymTab
import Debug.Trace
import CSyntaxToCbor

--import Debug.Trace
--import Util(traceM)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
#ifdef SET_LATIN1_ENCODING
    hSetEncoding stdout latin1
    hSetEncoding stderr latin1
#endif
    args <- getArgs
    -- bsc can raise exception,  catch them here  print the message and exit out.
    bsCatch (hmain args)

-- Use with hugs top level
hmain :: [String] -> IO ()
hmain args = do
    pprog <- getProgName
    cdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
    bscopts <- getEnvDef "BSC_OPTIONS" ""
    let args' = words bscopts ++ args
    let expiryMsg _ = unlines $ concatMap expiry getReportedBSCFeatures
    -- reconstruct original command line (modulo whitespace)
    -- add a newline at the end so it is offset
    let cmdLine = concat ("Invoking command line:\n" : (intersperse " " (pprog:args'))) ++ "\n"
    let showPreamble flags = do
          when ((verbose flags) || (printExpiry flags)) $ putStrLnF version
          when ((verbose flags) || (printExpiry flags)) $ putStrLnF copyright
          when (printExpiry flags) $ putStrLn (expiryMsg flags)
          when ((verbose flags) || (printFlags flags)) $ putStrLnF cmdLine
          when ((printFlags flags) || (printFlagsHidden flags)) $
                putStrLnF (showFlags flags)
          when (printFlagsRaw flags) $ putStrLnF (showFlagsRaw flags)
    let (warnings, decoded) = decodeArgs (baseName pprog) args' cdir
    errh <- initErrorHandle
    let doWarnings = when ((not . null) warnings) $ bsWarning errh warnings
        setFlags = setErrorHandleFlags errh
    case decoded of
        DHelp flags ->
            do { setFlags flags; doWarnings;
                 exitWithHelp errh pprog args' cdir (expiryMsg flags) }
        DHelpHidden flags ->
            do { setFlags flags; doWarnings;
                 exitWithHelpHidden errh pprog args' cdir (expiryMsg flags) }
        DUsage -> exitWithUsage errh pprog
        DError msgs -> bsError errh msgs
        DNoSrc flags ->
            -- XXX we want to allow "bsc -v" and "bsc -print-expiration"
            -- XXX to print version, etc, but if there are other flags
            -- XXX on the command-line we probably should report an error
            do { setFlags flags; doWarnings; showPreamble flags;
                 exitOK errh }
        DBlueSrc flags src ->
            do { setFlags flags; doWarnings; showPreamble flags;
                 main' errh flags src;
                 exitOK errh }
        DVerLink flags top verSrcFiles abinFiles cSrcFiles ->
            error $ "verilog linking not supported in this build"
        DSimLink flags top abinFiles cSrcFiles ->
            error $ "sim linking not supported in this build"


main' :: ErrorHandle -> Flags -> String -> IO ()
main' errh flags name =  do
    tStart <- getNow

    let dumpnames = (baseName (dropSuf name), "", "")

    start flags DFlicense
    (stat,flags') <- checkoutBSCLicenses errh flags Compile
    when (not stat) $ exitFail errh
    _ <- dump errh flags' tStart DFlicense dumpnames stat

    flags'' <- checkSATFlags errh flags'

    -- check system requirements
    doSystemCheck errh

    let comp = if updCheck flags''
               then compile_with_deps
               else compile_no_deps

    success <- comp errh flags'' name

    -- checkin isn't necessary (but leave for verbose purposes)
    checkinAllLicenses flags''
    _ <- timestampStr flags' "total" tStart

    if success then
      return ()
     else
       exitFail errh

compile_with_deps :: ErrorHandle -> Flags -> String -> IO (Bool)
compile_with_deps errh flags name = do
    let
        verb = showUpds flags && not (quiet flags)
        -- the flags to "compileFile" when re-compiling depended modules
        flags_depend = flags { updCheck = False,
                               genName = [],
                               showCodeGen = verb }
        -- the flags to "compileFile" when re-compiling this module
        flags_this = flags_depend { genName = genName flags }
        comp (success, binmap0, hashmap0) fn = do
            when (verb) $ putStrLnF ("compiling " ++ fn)
            let fl = if (fn == name)
                     then flags_this
                     else flags_depend
            (cur_success, binmap, hashmap)
                <- compileFile errh fl binmap0 hashmap0 fn
            return (cur_success && success, binmap, hashmap)
    when (verb) $ putStrLnF "checking package dependencies"

    t <- getNow
    let dumpnames = (baseName (dropSuf name), "", "")

    -- get the list of depended files which need recompiling
    start flags DFdepend
    fs <- chkDeps errh flags name
    _ <- dump errh flags t DFdepend dumpnames fs

    -- compile them
    (ok, _, _) <- foldM comp (True, M.empty, M.empty) fs

    when (verb) $
      if ok then
          putStrLnF "All packages are up to date."
      else putStrLnF "All packages compiled (some with errors)."

    return ok

compile_no_deps :: ErrorHandle -> Flags -> String -> IO (Bool)
compile_no_deps errh flags name = do
  (ok, _, _) <- compileFile errh flags M.empty M.empty name
  return ok

-- returns whether the compile errored or not
compileFile :: ErrorHandle -> Flags -> BinMap HeapData -> HashMap -> String ->
               IO (Bool, BinMap HeapData, HashMap)
compileFile errh flags binmap hashmap name_orig = do
    pwd <- getCurrentDirectory
    let name = (createEncodedFullFilePath name_orig pwd)
        name_rel = (getRelativeFilePath name)

    let syntax = (if      hasDotSuf bscSrcSuffix name then CLASSIC
                  else if hasDotSuf bseSrcSuffix name then ESE
                  else BSV)
    setSyntax syntax

    t <- getNow
    let dumpnames = (baseName (dropSuf name), "", "")

    start flags DFcpp
    file <- doCPP errh flags name
    _ <- dumpStr errh flags t DFcpp dumpnames file

    -- ===== the break point between file manipulation and compilation

    -- We don't start and dump this stage because that is handled inside
    -- the "parseSrc" function (since BSV parsing has multiple stages)
    (pkg@(CPackage i _ _ _ _ _), t)
        <- parseSrc (syntax == CLASSIC) errh flags True name file
    when (getIdString i /= baseName (dropSuf name)) $
         bsWarning errh
             [(noPosition, WFilePackageNameMismatch name_rel (pfpString i))]

    -- dump CSyntax
    when (showCSyntax flags) (putStrLnF (show pkg))
    -- dump stats
    stats flags DFparsed pkg

    let dumpnames = (baseName (dropSuf name), getIdString (unQualId i), "")
    compilePackage errh flags dumpnames t binmap hashmap name pkg

-------------------------------------------------------------------------

compilePackage ::
    ErrorHandle ->
    Flags ->
    DumpNames ->
    TimeInfo ->
    BinMap HeapData ->
    HashMap ->
    String ->
    CPackage ->
    IO (Bool, BinMap HeapData, HashMap)
compilePackage
    errh
    flags		-- user switches
    dumpnames
    tStart
    binmap0
    hashmap0
    name -- String --
    min@(CPackage pkgId _ _ _ _ _) = do

    clkTime <- getClockTime
    epochTime <- getPOSIXTime

    -- Values needed for the Environment module
    let env =
            [("compilerVersion",iMkString $ version),
             ("date",		iMkString $ show clkTime),
             ("epochTime",      iMkLitSize 32 $ floor epochTime),
             ("buildVersion",   iMkLitSize 32 $ buildnum),
             ("genPackageName", iMkString $ getIdBaseString pkgId),
             ("testAssert",	iMkRealBool $ testAssert flags)
            ]

    start flags DFimports
    -- Read imported signatures
    (mimp@(CPackage _ _ imps _ _ _), binmap, hashmap)
        <- readImports errh flags binmap0 hashmap0 min
    when (hasDump flags DFimports) $
      let imps' = [ppReadable s |  (CImpSign _ _ s) <- imps]
      in mapM_ (putStr) imps'
         --mapM_ (\ (CImpSign _ _ s) -> putStr (ppReadable s)) imps
    t <- dump errh flags tStart DFimports dumpnames mimp

    start flags DFopparse
    mop <- parseOps errh mimp
    t <- dump errh flags t DFopparse dumpnames mop

    -- Generate a global symbol table
    -- This is the second out of 3 times
    -- note that mkSymTab requires that imports be topologically sorted
    start flags DFsymbols
    symt00 <- mkSymTab errh mop
    t <- dump errh flags t DFsymbols dumpnames symt00

    -- whether we are doing code generation for modules
    let generating = backend flags /= Nothing

    -- Turn `noinline' into module definitions
    start flags DFgenfuncwrap
    (mfwrp, symt0, funcs) <- genFuncWrap errh flags generating mop symt00
    t <- dump errh flags t DFgenfuncwrap dumpnames mfwrp

    -- Generate wrapper for Verilog interface.
    start flags DFgenwrap
    (mwrp, gens) <- genWrap errh flags (genName flags) generating mfwrp symt0
    t <- dump errh flags t DFgenwrap dumpnames mwrp

    -- Rebuild the symbol table because GenWrap added new types
    -- and typeclass instances for those types
    start flags DFsymbols
    symt1 <- mkSymTab errh mwrp
    t <- dump errh flags t DFsymbols dumpnames symt1

    -- Re-add function definitions for `noinline'
    mfawrp <- addFuncWrap errh symt1 funcs mwrp

    -- Turn deriving into instance declarations
    start flags DFderiving
    mder <- derive errh flags symt1 mfawrp
    t <- dump errh flags t DFderiving dumpnames mder

    start flags DFsymbols
    symt11 <- mkSymTab errh mder
    t <- dump errh flags t DFsymbols dumpnames symt11

    -- Reduce the contexts as far as possible
    start flags DFctxreduce
    mctx <- cCtxReduceIO errh flags symt11 mder
    t <- dump errh flags t DFctxreduce dumpnames mctx

    -- Generate new global symbol table
    -- Third time's the charm!
    -- XXX could reuse part of the old!
    -- note that mkSymTab requires that imports be topologically sorted
    start flags DFsymbols
    symt <- mkSymTab errh mctx
    t <- dump errh flags t DFsymbols dumpnames symt

    -- Turn instance declarations into ordinary definitions
    start flags DFconvinst
    let minst = cConvInst errh symt mctx
    t <- dump errh flags t DFconvinst dumpnames minst

    -- Type check and insert dictionaries
    start flags DFtypecheck
    (mod, tcErrors) <- cTypeCheck errh flags symt minst
    --putStr (ppReadable mod)
    t <- dump errh flags t DFtypecheck dumpnames mod

    modExport <- prepareCborExport errh flags symt mod
    let cbor_filename = putInDir (bdir flags) name "cbor"
    putStrLn $ "writing " ++ cbor_filename
    BS.writeFile cbor_filename $ cPackageToCborBytes modExport

    --when (early flags) $ return ()
    let prefix = dirName name ++ "/"

    -- Generate wrapper info for foreign function imports
    -- (this always happens, even when not generating for modules)
    start flags DFgenforeign
    foreign_func_info <- genForeign errh flags prefix mod
    t <- dump errh flags t DFgenforeign dumpnames foreign_func_info

    -- Simplify a little
    start flags DFsimplified
    let	mod' = simplify flags mod
    t <- dump errh flags t DFsimplified dumpnames mod'
    stats flags DFsimplified mod'

    --------------------------------------------
    -- Convert to internal abstract syntax
    --------------------------------------------
    start flags DFinternal
    imod <- iConvPackage errh flags symt mod'
    iPCheck flags symt imod "internal"
    t <- dump errh flags t DFinternal dumpnames imod
    when (showISyntax flags) (putStrLnF (show imod))
    stats flags DFinternal imod

    -- Read binary interface files
    start flags DFbinary
    let (_, _, impsigs, binmods0, pkgsigs) =
            let findFn i = fromJustOrErr "bsc: binmap" $ M.lookup i binmap
                sorted_ps = [ getIdString i
                               | CImpSign _ _ (CSignature i _ _ _) <- imps ]
            in  unzip5 $ map findFn sorted_ps

    -- injects the "magic" variables genC and genVerilog
    -- should probably be done via primitives
    -- XXX does this interact with signature matching
    -- or will it be caught by flag-matching?
    let adjEnv ::
            [(String, IExpr a)] ->
            (IPackage a) ->
            (IPackage a)
        adjEnv env (IPackage i lps ps ds)
                            | getIdString i == "Prelude" =
                    IPackage i lps ps (map adjDef ds)
            where
                adjDef (IDef i t x p) =
                    case lookup (getIdString (unQualId i)) env of
                        Just e ->  IDef i t e p
                        Nothing -> IDef i t x p
        adjEnv _ p = p

    let
        -- adjust the "raw" packages and then add back their signatures
        -- so they can be put into the current IPackage for linking info
        binmods = zip (map (adjEnv env) binmods0) pkgsigs

    t <- dump errh flags t DFbinary dumpnames binmods

    -- For "genModule" we construct a symbol table that includes all defs,
    -- not just those that are user visible.
    -- XXX This is needed for inserting RWire primitives in AAddSchedAssumps
    -- XXX but is it needed anywhere else?
    start flags DFsymbols
    -- XXX The way we construct the symtab is to replace the user-visible
    -- XXX imports with the full imports.
    let mint = replaceImports mctx impsigs
    internalSymt <- mkSymTab errh mint
    t <- dump errh flags t DFsymbols dumpnames mint

    start flags DFfixup
    let (imodf, _) = fixupDefs imod binmods
    iPCheck flags symt imodf "fixup"
    t <- dump errh flags t DFfixup dumpnames imodf

    start flags DFisimplify
    let imods :: IPackage HeapData
        imods = iSimplify imodf
    iPCheck flags symt imods "isimplify"
    t <- dump errh flags t DFisimplify dumpnames imods
    stats flags DFisimplify imods

    let orderGens :: IPackage HeapData -> [WrapInfo] -> [WrapInfo]
        orderGens (IPackage pid _ _ ds) gs =
                --trace (ppReadable (gis, g, os)) $
                                              map get os
          where gis = [ qualId pid i
                                | (WrapInfo i _ _ _ _ _) <- gs ]
                tr = [ (qualId pid i_, qualId pid i)
                                | (WrapInfo i _ _ i_ _ _) <- gs ]
                ds' = [ IDef (lookupWithDefault tr i i) t e p
                                | IDef i t e p <- ds, i `notElem` gis ]
                is = [ i | IDef i _ _ _ <- ds' ]
                g  = [ (i, fdVars e `intersect` is) | IDef i _ e _ <- ds' ]
                iis = scc g
                os = concat iis `intersect` gis
                get i = headOrErr "bsc.orderGens: no WrapInfo"
                                  [ x | x@(WrapInfo i' _ _ _ _ _) <- gs,
                                                unQualId i == i' ]
        ordgens :: [WrapInfo]
        ordgens = orderGens imods gens

    when (verbose flags) $
        putStr ("modules: " ++
                    ppReadable [ i | (WrapInfo { mod_nm = i }) <- ordgens ] ++
                    "\n")

    (imodr, success) <- return (imods, True)

    t <- getNow
    -- Finally, generate interface files
    start flags DFwriteBin

    -- Generate the user-visible type signature
    bi_sig <- genUserSign errh symt mctx
    -- Generate a type signature where everything is visible
    bo_sig <- genEverythingSign errh symt mctx

    -- Generate binary version of the internal tree .bo file
    let bin_filename = putInDir (bdir flags) name binSuffix
    genBinFile errh bin_filename bi_sig bo_sig imodr

    -- Print one message for the two files
    let rel_binname = getRelativeFilePath bin_filename
    when (verbose flags) $
         putStrLnF ("Compiled package file created: " ++ rel_binname)
    t <- dumpStr errh flags t DFwriteBin dumpnames bin_filename

    -- XXX We could add the generated .bo directly to the maps,
    -- XXX but we lack the hash (which is generated when reading in a file)
    -- let binfile = (rel_binname, bi_sig, bo_sig, modr, ...)
    --     binmap' = ...
    --     hashmap' = ...

    return (success && not tcErrors, binmap, hashmap)

iPCheck :: Flags -> SymTab -> IPackage a -> String -> IO ()
iPCheck flags symt ipkg desc = --hyper ipkg $
        if doICheck flags && not (tCheckIPackage flags symt ipkg)
            then internalError (
                "internal typecheck failed (iPCheck after " ++
                desc ++ ")")
            else
                if (verbose flags)
                    then putStrLnF "types OK"
                    else return ()


prepareCborExport :: ErrorHandle -> Flags -> SymTab -> CPackage -> IO CPackage
prepareCborExport errh flags symt (CPackage i ex im fx ds inc) = do
    ds' <- mapM go ds
    return (CPackage i ex im fx ds' inc)
  where
    go (Ctype idk vs ty) = case MakeSymTab.convCType symt ty of
        Left msg -> bsError errh [msg]
        Right ty' -> do
            return (Ctype idk vs ty')
    go d = return d
