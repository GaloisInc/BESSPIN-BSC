{-# LANGUAGE CPP #-}
module Authorization( id_Authorization

                    , expiry

                    -- this is used in BSC IExpand
                    , checkoutAnyFeature

                    -- BSC
                    , getReportedBSCFeatures
                    , BSCLicMode(..)
                    , checkoutBSCLicenses
                    , checkBSCElaborationLimit
                    , checkBSCModuleGen

                    -- Workstation / blutecl
		    , WSLicMode(..)
                    , checkoutWSLicenses

                    -- SceMiLink
                    , checkoutSceMiLinkLicenses

                    -- common functions for returning licenses
                    , checkinLicense
                    , checkinAllLicenses

                    ) where

import System.Posix(EpochTime, epochTime)
import System.IO.Unsafe
import System.IO(stderr,hPutStrLn)
import Control.Monad(when,foldM)
import Flexlm
import Flags( Flags(..)
            , showLicense, licenseWarning, waitLicense
            , esecomp, genSysC, runTimeLic, licenseType
            , LicenseType(..)
            )
import Backend
import Error(ErrMsg(..), EMsg, ErrorHandle, bsError, bsWarning, bsMessage)
import Position(noPosition)
import Version

import Data.List(find, groupBy, partition, delete, nub)
import Data.Char(isDigit)
import Data.Maybe(listToMaybe, fromMaybe)
import Util(headOrErr)

-- Needed by the XGen policy for checking modules
import ASyntax(APackage(..), AVInst(..))
import VModInfo(VModInfo(..), getVNameString)
import Id(getIdString)

id_Authorization = "$Id$"


-- For debug
flexlmVerbose = False

-- -------------------------
-- Policy versions

data LicPolicy = DEFAULT
               | XGEN
               | DATE_REVERT EpochTime

licPolicy :: LicPolicy
licPolicy =
#if defined(BSC_LIC_POLICY_XGEN)
    XGEN
#elif defined(BSC_LIC_POLICY_DATE_REVERT)
    DATE_REVERT BSC_LIC_POLICY_DATE_REVERT
#else
    DEFAULT
#endif

xgenCustomer :: String
xgenCustomer = ""

timeCheck :: EpochTime -> IO a -> IO a -> IO a
timeCheck exp_time unexpAction expAction = do
  cur_time <- epochTime
  if (cur_time < exp_time)
    then unexpAction
    else expAction

-- -------------------------
-- Features

templateLic :: LM_Feature
templateLic =  LM_Feature { lmf_name = "XXX", lmf_majVer = versionMajor, lmf_minVer =versionMinor, lmf_cnt = 1,
                           lmf_attr = LM_CO_NOWAIT, lmf_dups = [],
                           lmf_linger = 0, lmf_coStr = "" }

floatingLic :: LM_Feature
floatingLic = templateLic { lmf_name = "BCOMP" }

bluesimOnlyLic :: LM_Feature
bluesimOnlyLic = templateLic { lmf_name = "BCOMP_Bluesim" }

ecompLic :: LM_Feature
ecompLic = templateLic { lmf_name = "BESECOMP" }

seatLic :: LM_Feature
seatLic = templateLic { lmf_name = "BlueSeat",
                        lmf_dups = [LM_DUP_USER],
                        lmf_linger = 60*60*16 }

compseatLic :: LM_Feature
compseatLic  = templateLic { lmf_name = "BCOMPSeat" }

unrestrictedSYSCLic :: LM_Feature
unrestrictedSYSCLic = templateLic { lmf_name = "SYSCUNLIC" }

unrestrictedBSIMLic :: LM_Feature
unrestrictedBSIMLic = templateLic { lmf_name = "BSIMUNLIC" }

-- -------------------------
-- Workstation and bluetcl licenses

workstationLic :: LM_Feature
workstationLic = templateLic {  lmf_name = "workstation" }

-- these licenses checked out by the workstation, and allow duplicates so compiles
-- can use the same license on the same tty.
wsCompSeatLic :: LM_Feature
wsCompSeatLic = templateLic { lmf_name = "BCOMPSeat"
                            , lmf_dups = [LM_DUP_USER,LM_DUP_HOST,LM_DUP_DISP,LM_DUP_VENDOR]
                            , lmf_coStr = "Workstation" }

wsCompLic :: LM_Feature
wsCompLic = templateLic { lmf_name = "BCOMP"
                        , lmf_dups = [LM_DUP_USER,LM_DUP_HOST,LM_DUP_DISP,LM_DUP_VENDOR]
                        , lmf_coStr = "Workstation" }


wsLics :: [LM_Feature]
wsLics = [workstationLic, wsCompSeatLic, wsCompLic]

-- -------------------------
-- SceMiLink licenses

scemilinkLic :: LM_Feature
scemilinkLic = templateLic { lmf_name = "SceMiLink"
                           , lmf_attr =LM_CO_LOCALTEST
                           }

-- -------------------------
-- License properties

seatLics :: [String]
seatLics  = ["BCOMPSeat"]

isSeatLic :: LM_Feature -> Bool
isSeatLic f = lmf_name f `elem` seatLics

waitableLics :: [String]
waitableLics = ["BCOMP", "BCOMPSeat"]

isWaitLic :: LM_Feature -> Bool
isWaitLic f = lmf_name f `elem` waitableLics

waitableStatus :: [LM_Status] -> [LM_Status]
waitableStatus stats = filter isWait stats
    where isWait s | lms_code s == lm_MAXUSERS    = isWaitLic $ lms_feat s
                   | lms_code s == lm_USERSQUEUED = isWaitLic $ lms_feat s
                   | otherwise = False 

-- Extract the "S" option from a split Vendor string
getLingerVendorOption :: [String] -> LM_Feature -> LM_Feature
getLingerVendorOption [] f = f
getLingerVendorOption (s:x:rest) f | s == "S" =
    let f' = f { lmf_linger = (read x) }
    in  getLingerVendorOption rest f'
getLingerVendorOption (_:rest) f  = getLingerVendorOption rest f

-- -------------------------
-- Messages

featureExpMsg :: Int -> String
featureExpMsg day =
    if ( day < 0) then
        ("in an unknown number of days;  Check license file or LM_LICENSE_FILE environment.")
    else
        ("in " ++ show day ++ " days." )

licenseWarningMsg :: ErrorHandle -> String -> Int -> IO ()
licenseWarningMsg errh feature day =
    bsWarning errh [(noPosition, (WLicenseExpires feature (featureExpMsg day)))]

licenseExpired :: ErrorHandle -> [String] -> IO a
licenseExpired errh msg = bsError errh [(noPosition,(ELicenseUnavailable msg))]

licenseSearchPath :: ErrorHandle -> IO ()
licenseSearchPath errh =
    do servers <- lm_simple_search_path
       bsMessage errh [(noPosition,WLicenseSearchPath servers)]

-- This function is called at the end of each of the checkout entry points
-- to report the status (as indicated by the verbosity level)
reportCheckoutStatus :: ErrorHandle -> Flags -> LM_Status -> IO ()
reportCheckoutStatus errh flags stat | lms_code stat /= 0 =
  licenseExpired errh (lms_msgs stat)
reportCheckoutStatus errh flags stat = do
  let days = licenseWarning flags
      verbose = (showLicense flags)
      feat = lmf_name $ lms_feat stat
      reportMsgs :: [LM_Config] -> IO ()
      reportMsgs [] = return ()
      reportMsgs (lc:_) = do
        when (lmc_expDays lc <= days) $
             licenseWarningMsg errh feat (lmc_expDays lc)
        when verbose $
             hPutStrLn stderr ( "License feature " ++ feat ++ " has been checked out." )
  --
  lmcs <- lm_getAuthData $ lms_feat stat
  reportMsgs lmcs
  when (verbose) $ licenseSearchPath errh

emsgXGen :: ErrMsg
emsgXGen =
    let customer = if (xgenCustomer == "") || (xgenCustomer == ("BSC_" ++ "XGEN_CUSTOMER"))
                   then "third-party"
                   else xgenCustomer
        msg = "NOTICE: This version of the " ++ bluespec ++ " Compiler " ++
              "is only for building transactors within " ++ customer ++ " products " ++
              "and may not be used for other purposes."
    in  EGeneric msg

licenseXGen :: ErrorHandle -> IO ()
licenseXGen errh = bsError errh [(noPosition, emsgXGen)]

-- -------------------------
-- Expiration message

{-# NOINLINE expiry #-}
expiry :: LM_Feature -> [String]
expiry f = unsafePerformIO $ expiryM f
-- the transformation is safe, since there are no side effects from calling
-- the license manager in this feature

expiryM :: LM_Feature -> IO [String]
expiryM feature =
    case licPolicy of
      XGEN -> return []
      DATE_REVERT exp_time ->
        let expiryM_unlic = do
              cur_time <- epochTime
              let secs = fromEnum (exp_time - cur_time)
                  days = round ( (toRational secs) / (60 * 60 * 24) )
              return (expiryMsg feature days)
        in  timeCheck exp_time expiryM_unlic (expiryM_default feature)
      DEFAULT -> expiryM_default feature

expiryMsg :: LM_Feature -> Int -> [String]
expiryMsg feature days =
    ["License " ++  lmf_name feature ++ " expires " ++ featureExpMsg days]

expiryM_default :: LM_Feature -> IO [String]
expiryM_default feature = do
  cs <- lm_getTestConfig feature
  return $ case (listToMaybe cs) of
             Nothing -> []
             Just day -> expiryMsg feature (lmc_expDays day)

-- -------------------------
-- Basic, generic license operations

-- return True if feature is present in license file
-- Does not checkout a license nor Verify that the license can be checked out
testFeature :: LM_Feature -> IO Bool
testFeature feat = do
  cs <- lm_getTestConfig feat
  when flexlmVerbose $ putStrLn $ "test feature: " ++ show feat
  when flexlmVerbose $ mapM_ (putStrLn . show) cs
  return $ length cs /= 0

checkoutFeature :: ErrorHandle -> Flags -> LM_Feature -> IO ()
checkoutFeature errh flags feature = do
  stat <- lm_checkout False feature
  reportCheckoutStatus errh flags stat

-- Checkout one of these features.
-- featStr is a space separate list of features to try
checkoutAnyFeature :: Bool -> ErrorHandle -> Flags -> String -> IO ()
checkoutAnyFeature verb errh flags featStr =
  let dflt = checkoutAnyFeature_default verb errh flags featStr
  in
    case licPolicy of
      XGEN -> licenseXGen errh
      DATE_REVERT exp_time -> timeCheck exp_time (return ()) dflt
      DEFAULT -> dflt

checkoutAnyFeature_default :: Bool -> ErrorHandle -> Flags -> String -> IO ()
checkoutAnyFeature_default verb errh flags featStr = do
  let feats = words featStr
      tryFeature :: [LM_Status] -> [String] -> IO [LM_Status]
      tryFeature stats [] = return stats
      tryFeature stats (f:rest) = do
        let feature = templateLic { lmf_name = f }
        s <- lm_checkout verb feature
        if lm_status_ok [s] then return [s] else tryFeature (s:stats) rest
  stats <- tryFeature [] feats
  mapM_ (reportCheckoutStatus errh flags) stats

coLicenseWithSeat :: Bool -> LM_Feature -> LM_Feature ->  IO [LM_Status]
coLicenseWithSeat verb seat feat = do
  co1 <- lm_checkout verb feat
  if (lms_code co1 == 0) then do
        s' <- getSeatLingerTime feat seat
        coS <- lm_checkout verb s'
        when verb $ putStrLn $ "Checking  seat :: " ++ (show $ coS)
        if (lms_code coS == 0) then
            return [coS, co1]
         else do --lm_checkin verb [lms_feat co1]
                 -- XXX flex does not seem to want to checkin this feature here?
                 -- so we check them all in!
                 when verb $ putStrLn $ "Checking IN FAILED seat :: " ++ (show $ coS)
                 --lm_checkin verb [feat, seat]
                 lm_checkin_all
                 return [coS]
   else return [co1]

-- returns modified feature s based on license line
getSeatLingerTime :: LM_Feature -> LM_Feature -> IO (LM_Feature)
getSeatLingerTime test s = do
  let testf ::  LM_Config -> Bool
      testf c = lmf_name test == lmc_feature c
  --
  cfgs <- lm_getAuthData test
  let vs = case (find testf  cfgs) of
             Just c -> lmc_vendor_str c
             Nothing -> ""
  when flexlmVerbose $  putStrLn $ "Vendor String :: " ++ vs
  let splits = groupBy (\a b -> isDigit a == isDigit b) vs
  return $ getLingerVendorOption splits s

checkoutWithTest :: [LM_Status] -> LM_Feature -> IO [LM_Status]
checkoutWithTest prevStat f | lm_status_ok prevStat = return prevStat
                            | otherwise = do
  let verb = flexlmVerbose
  avail <- testFeature f
  if (not avail) then return prevStat else do
        s <- if (isSeatLic f) then coLicenseWithSeat verb seatLic f
              else lm_checkout verb f >>= (\s -> return [s])
        if (lm_status_ok s) then return s else return (s++prevStat)

-- wait for one license priority given to floating licenses
-- this function blocks !
checkoutWithWait :: ErrorHandle -> Bool -> [LM_Status] -> IO [LM_Status]
checkoutWithWait _ _ [] = fail "Nothing to wait for !"
checkoutWithWait errh verb stats = do
  let (seats,floats) = partition (isSeatLic . lms_feat) stats
  bsWarning errh [(noPosition, WWaitForLicense)]
  if (not $ null floats) then do
        let f = lms_feat $ headOrErr "checkoutWithWait" floats
            f' = f { lmf_attr = LM_CO_WAIT }
        s <- lm_checkout verb f'
        return [s]
   else do
        let f = lms_feat $ headOrErr "checkoutWithWait" seats
            f' = f { lmf_attr = LM_CO_WAIT }
        s <- coLicenseWithSeat verb seatLic f'
        return s

checkinLicense :: Bool -> String -> IO ()
checkinLicense verb fname =
  let dflt = lm_checkin verb [(templateLic {lmf_name = fname})]
  in
    case licPolicy of
      XGEN -> return ()
      DATE_REVERT exp_time -> timeCheck exp_time (return ()) dflt
      DEFAULT -> dflt

checkinAllLicenses :: Flags ->  IO ()
checkinAllLicenses flgs =
  let dflt = lm_checkin_all
  in
    case licPolicy of
      XGEN -> return ()
      DATE_REVERT exp_time -> timeCheck exp_time (return ()) dflt
      DEFAULT -> dflt

-- -------------------------
-- BSC entry points

-- The features reported in the BSC help message
getReportedBSCFeatures :: [LM_Feature]
getReportedBSCFeatures =
  let dflt = [ seatLic
             , compseatLic
             , floatingLic
             ]
  in
    case licPolicy of
        XGEN -> []
        DATE_REVERT exp_time ->
          unsafePerformIO $ timeCheck exp_time (return [floatingLic]) (return dflt)
        DEFAULT -> dflt

data BSCLicMode = Compile | Link
    deriving (Eq)

-- Top level entry for bsc compiler   checkout a compiler license
-- and other auxilary licenses
checkoutBSCLicenses :: ErrorHandle -> Flags -> BSCLicMode -> IO (Bool,Flags)
checkoutBSCLicenses errh flags bscmode =
  let dflt = checkoutBSCLicenses_default errh flags bscmode
  in
    case licPolicy of
      XGEN -> do
        -- don't allow dumping
        let badFlags =
                (dumpAll flags) ||
                (not (null (dumps flags))) ||
                (showIESyntax flags) ||
                (showISyntax flags) ||
                (showSchedule flags)
        when badFlags $ licenseXGen errh
        -- only allow Compile and only to Verilog
        case bscmode of
          Compile | (backend flags == Just Verilog) -> return (True, flags)
          otherwise -> do licenseXGen errh
                          return (False, flags)
      DATE_REVERT exp_time -> timeCheck exp_time (return (True, flags)) dflt
      DEFAULT -> dflt

-- XXX This should consult the BSCLicMode
checkoutBSCLicenses_default :: ErrorHandle -> Flags -> BSCLicMode -> IO (Bool,Flags)
checkoutBSCLicenses_default errh flags bscmode = do
  -- compiler features
  (comps,flags') <- bsccompco errh flags
  when flexlmVerbose $ lm_getJob >>= putStrLn . show
  -- Aux features
  s1 <- if (lm_status_ok comps) then do
              let feats = getRequiredFeatures flags bscmode
              s2 <- lm_co_all flexlmVerbose feats
              return (comps ++ s2)
         else return comps
  mapM_ (reportCheckoutStatus errh flags) s1
  return (lm_status_ok s1,flags')

-- checkout a compiler license  floating or seat
bsccompco :: ErrorHandle -> Flags -> IO ([LM_Status],Flags)
bsccompco errh flags = do
  let verb = flexlmVerbose
      wait = waitLicense flags
  stats <- bscCO verb flags
  s <- if (lm_status_ok stats) then return stats
   else do
         stats2 <- tryDuplicates verb stats
         if (lm_status_ok stats2) then return stats2
          else let waitable =  waitableStatus stats
               in  if (wait && not (null waitable))
                   then checkoutWithWait errh verb waitable
                   else return stats
  flags' <- updateFlagsWithLicenseLimits s flags
  return (s,flags')

-- check out a bscseat or float license, which ever one is available
bscCO :: Bool -> Flags -> IO [LM_Status]
bscCO verb flags = do
  let -- License types priority order
      lics = [Seat, Floating, BlueSimOnly]
  --
  let 
      checkoutFirst :: [LM_Status] -> [LicenseType] -> IO [LM_Status]
      checkoutFirst [] [] =  do
        s <- lm_checkout verb floatingLic
        return [s]
      checkoutFirst ls [] = return ls
      checkoutFirst ls (Floating:rest) = do
        test <- testFeature floatingLic
        if test then do
          s <- lm_checkout verb floatingLic
          if (lm_status_ok [s]) then return [s]  else checkoutFirst (s:ls) rest
          else checkoutFirst ls rest
      checkoutFirst ls (BlueSimOnly:rest) = do
        test <- testFeature bluesimOnlyLic
        if test then do
          s <- lm_checkout verb bluesimOnlyLic
          if (lm_status_ok [s]) then return [s]  else checkoutFirst (s:ls) rest
          else checkoutFirst ls rest
      checkoutFirst ls (Seat:rest) = do
        test <- testFeature seatLic
        if test then do
          cs <- coLicenseWithSeat verb seatLic compseatLic
          if (lm_status_ok cs) then return cs  else checkoutFirst (cs ++ ls) rest
          else checkoutFirst ls rest
      checkoutFirst ls (Any:rest) = do
        s <- lm_checkout verb floatingLic
        if (lm_status_ok [s]) then return [s]  else checkoutFirst (s:ls) rest
      --
  --
  let ltypes = let lt = licenseType flags
           in if (lt == Any) then lics else [lt]
      ltypes1 = if (backend flags) == Just Verilog then
                  delete BlueSimOnly ltypes else ltypes
  --
  s <- checkoutFirst [] ltypes1
  return s

-- Check if any failed features can be checked out with "duplicate"
-- attributes via the workstation
tryDuplicates :: Bool -> [LM_Status] -> IO [LM_Status]
tryDuplicates verb stats = do
  case (waitableStatus stats) of
    [] -> return stats
    ss -> foldM tryDup [] ss
    where tryDup :: [LM_Status] -> LM_Status -> IO [LM_Status]
          tryDup prev s | lm_status_ok prev = return prev
          tryDup prev s = do
            let f = (lms_feat s) {lmf_dups = [LM_DUP_USER,LM_DUP_HOST,LM_DUP_DISP,LM_DUP_VENDOR]
                                 , lmf_coStr = "Workstation" }
            stats <- if (isSeatLic f) then coLicenseWithSeat verb seatLic f else
                         mapM (lm_checkout verb) [f]
            return $ stats ++ prev

updateFlagsWithLicenseLimits :: [LM_Status] -> Flags -> IO Flags
updateFlagsWithLicenseLimits ss flags = do
  let mkPairs :: [(String, Int)] -> [String]-> [(String, Int)]
      mkPairs init [] = init
      mkPairs init (s:x:rest) = mkPairs ((s,read x):init) rest
      mkPairs init (_:rest) = mkPairs init rest
      extractValues :: String -> [(String, Int)]
      extractValues str = let gs =  groupBy (\a b -> isDigit a == isDigit b) str
                          in mkPairs [] gs
  cfgss <- mapM  lm_getAuthData (nub (map lms_feat ss))
  let cfgs = concat cfgss
      strs = nub $ concatMap (extractValues . lmc_vendor_str) cfgs
      elimit = fromMaybe 0 (lookup "R" strs)
      flags' = flags {elaborationSizeLimit = elimit} 
  when flexlmVerbose $  putStrLn $ "Vendor String: " ++ show strs ++ " Limit: " ++ (show elimit)
  return flags'

-- Features required in addition to compiler license
-- XXX This should consult the BSCLicMode
getRequiredFeatures :: Flags -> BSCLicMode -> [LM_Feature]
getRequiredFeatures flags _ =
    let comp = if (esecomp flags)
               then [ecompLic]
               else []
        gen  = if (runTimeLic flags)
               then []
               else if (genSysC flags)
                    then [unrestrictedSYSCLic]
                    else [unrestrictedBSIMLic]
    in comp ++ gen


checkBSCElaborationLimit :: ErrorHandle -> Int -> (String,Int) -> IO ()
checkBSCElaborationLimit errh limit (mod, size) = do
  if ((limit > 0) && (size > limit)) then do
    bsError errh [(noPosition,(ELicenseElabLimit mod limit size))]
    else  return ()


checkBSCModuleGen :: ErrorHandle -> APackage -> IO ()
checkBSCModuleGen errh amod =
    case licPolicy of
      DEFAULT -> return ()
      DATE_REVERT _ -> return ()
      XGEN | (getIdString (apkg_name amod) == "ptm_top") ->
        -- exempt "ptm_top" as long as it has an instance of "ptm_bridge" named "bridge"
        let is_bridge avi = (getIdString (avi_vname avi) == "bridge") &&
                            (getVNameString (vName (avi_vmi avi)) == "ptm_bridge")
        in  case (filter is_bridge (apkg_state_instances amod)) of
              [_] -> return ()
              _   -> licenseXGen errh
      XGEN | otherwise ->
        let has_mod m = m `elem` (map (getVNameString . vName . avi_vmi) (apkg_state_instances amod))
            has_link_type = has_mod "mkSceMiLinkTypeParameter"
            has_param32   = has_mod "mkSceMiUInt32Parameter"
            has_param64   = has_mod "mkSceMiUInt64Parameter"
        in  if (has_link_type && (has_param32 || has_param64))
            then return ()
            else licenseXGen errh

-- -------------------------
-- Workstation / bluetcl entry points

data WSLicMode = Package | Module | Depend | Parse
    deriving (Eq)

-- failure on license checkout throws an IOError
-- no waiting is allowed.  This can be done in tcl if desired.
checkoutWSLicenses :: ErrorHandle -> Flags -> WSLicMode -> IO [EMsg]
checkoutWSLicenses errh flgs wsmode =
  let dflt = checkoutWSLicenses_default errh flgs wsmode
  in
    case licPolicy of
      XGEN ->
        if ((wsmode == Package) || (wsmode == Module))
        then return []
	else return [(noPosition, emsgXGen)]
      DATE_REVERT exp_time -> timeCheck exp_time (return []) dflt
      DEFAULT -> dflt

checkoutWSLicenses_default :: ErrorHandle -> Flags -> WSLicMode -> IO [EMsg]
checkoutWSLicenses_default errh flgs wsmode = do
  -- See if a ws feature is checked out
  let checkStatus :: Bool -> LM_Feature -> IO Bool
      checkStatus False f = do
                    s <- lm_status f
                    return $ (0 == s)
      checkStatus True _ = return True
  stat <- foldM checkStatus  False wsLics
  if (stat) then return []  else firstWSCO errh flgs

-- Checkout a workstation license for the first time
firstWSCO :: ErrorHandle -> Flags -> IO [EMsg]
firstWSCO errh flgs = do
  let verb = flexlmVerbose
      wait = waitLicense flgs
      report :: [LM_Status] -> IO [EMsg]
      report lss = mapM (reportCheckoutStatus errh flgs) lss >> return []
      dummystat f = do lm_checkout verb f  >>= (\s -> return [s])
  --
  stats1 <- foldM checkoutWithTest [] wsLics
  -- wait for license if required and requested
  -- wait will be for the first license in wsLics which was present in users licfile 
  -- that will probably be the workstation feature
  stats <- case ((lm_status_ok stats1),wait) of
    (True,_)      -> return stats1
    (False,False) -> return stats1
    (False,True)  -> checkoutWithWait errh verb $ reverse stats1
  -- report results here
  if (lm_status_ok stats) then report stats else do
        -- no licenses availble  and maybe no stats
        stats' <- if (null stats)
                  then dummystat (headOrErr "firstWSCO" wsLics)
                  else return stats
        let errs = map ((\m -> (noPosition,(ELicenseUnavailable m))) . lms_msgs) stats'
        return errs

-- -------------------------
-- SceMiLink entry points

checkoutSceMiLinkLicenses :: ErrorHandle -> Flags -> IO ()
checkoutSceMiLinkLicenses errh flags =
  let dflt = checkoutFeature errh flags scemilinkLic
  in
    case licPolicy of
      XGEN -> return ()
      DATE_REVERT exp_time -> timeCheck exp_time (return ()) dflt
      DEFAULT -> dflt

-- -------------------------
