module Flexlm (
    lm_simple_co,
    lm_simple_ci,    
    lm_simple_expire_days,
    lm_simple_errstring
    )where

-- import Foreign
-- import CString
-- import Foreign.Marshal.Array
-- import Monad(when)

-- -- -- FlexLM Job structure
-- -- data JobD = JobD
-- -- type Job = Ptr JobD

-- -- -- FlexLM Vendor Code pointer
-- -- data VendorCodeD = VendorCodeD 
-- -- type VendorCode = Ptr VendorCodeD 

-- foreign import ccall "flexifc.h"
--         hflexSimpleCO :: CString -> CString -> Int -> IO Int

-- foreign import ccall "flexifc.h"
--         hflexSimpleCI :: CString -> IO () 

-- foreign import ccall "flexifc.h"
--         hflexSimpleExpDays :: CString -> CString -> IO (Int) 

-- foreign import ccall "flexifc.h"
--         hflexSimpleGetErrString :: IO (CString) 

-- -- Simple interface for checking out a feature under flexlm
-- -- The simple interfaces only work 
-- -- XXX todo check the string size length ( version ++ subversion) <= 10 
-- lm_simple_co :: Bool -> String -> String -> String -> Int -> IO (Int)
-- lm_simple_co verbose feature version subversion number =
--     do
--     featureS <- newCString feature
--     versionS <- newCString (version ++ "." ++ subversion)
--     when (verbose) $ putStrLn ( "Checking out feature " ++ show feature ) 
--     status <- hflexSimpleCO featureS versionS number 
--     when (verbose) $ putStrLn ( "Checkout status is " ++ show status )
--     return status 

-- -- Return the license to the server
-- lm_simple_ci :: Bool -> String -> IO ()
-- lm_simple_ci verbose feature =
--     do
--     newCString feature  >>= hflexSimpleCI 
--     when (verbose) $ putStrLn ( "Checked in feature " ++ show feature ) 

-- -- get the number of days when the feature is set to expire
-- -- a -1 indicates an error or that the feature has already expired.
-- lm_simple_expire_days :: String -> String -> String -> IO Int 
-- lm_simple_expire_days feature version subversion =
--     do
--     featureS <- newCString feature
--     versionS <- newCString (version ++ "." ++ subversion)
--     hflexSimpleExpDays featureS versionS 

-- -- create a error string from a return status
-- lm_simple_errstring :: IO ([String])
-- lm_simple_errstring =
--     do
--     pcstr <- hflexSimpleGetErrString
--     mstr <- peekArray0 0 pcstr
--     return ( lines $ map castCCharToChar mstr )

-- lm_checkout :: IO (Bool)
-- lm_checkout = return True 


-- lm_checkin :: IO ()
-- lm_checkin = return () 

-- lm_newjob :: Job -> IO (Status,Job, VendorCode)
-- lm_newjob :: IO () 
-- lm_newjob = return ()

-- lm_freejob :: IO ()
-- lm_freejob = return () 

       
-- lm_expire_days :: IO ( Int )
-- lm_expire_days = return 13 

lm_simple_co :: Bool -> String -> String -> String -> Int -> IO (Int)
lm_simple_co verbose feature version subversion number = return 0

lm_simple_ci :: Bool -> String -> IO ()
lm_simple_ci verbose feature = return ()

lm_simple_expire_days :: String -> String -> String -> IO Int 
lm_simple_expire_days feature version subversion = return 13

lm_simple_errstring :: IO ([String])
lm_simple_errstring = return []