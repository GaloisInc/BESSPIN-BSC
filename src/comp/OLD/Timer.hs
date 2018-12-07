module Timer where

import Control.Exception(evaluate)
import System.IO.Unsafe(unsafePerformIO)
import System.CPUTime(getCPUTime)
import Data.Ratio((%))
import Numeric(showFFloat)

import Eval

{- NOINLINE -}
time :: (Hyper a) => String -> a -> a
time name x =
    unsafePerformIO $ do t0 <- getCPUTime
                         prefix <- evaluate (hyper x (name ++ ": "))
                         t1 <- getCPUTime
                         putStrLn $ prefix ++ (showTimeDiff t0 t1)
                         return x
        where showTimeDiff t0 t1 =
                  let t = t1 - t0
                  in if (t < 1000)
                     then (pr t 1) ++ " ps"
                     else if (t < 1000000)
                          then (pr t 1000) ++ " ns"
                          else if (t < 1000000000)
                               then (pr t 1000000) ++ " us"
                               else if (t < 1000000000000)
                                    then (pr t 1000000000) ++ " ms"
                                    else (pr t 1000000000000) ++ " s"
              pr v d = showFFloat (Just 2) ((fromRational $ v % d)::Double) ""
