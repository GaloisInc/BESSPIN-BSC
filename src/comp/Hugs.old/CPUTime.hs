module CPUTime(getCPUTime, cpuTimePrecision) where

getCPUTime :: IO Integer
getCPUTime = return 0

cpuTimePrecision :: Integer
cpuTimePrecision = 1000000000000
