module Time(ClockTime, TimeDiff(..), diffClockTimes, addToClockTime, getClockTime, Month(..), toCalendarTime, CalendarTime(..), Day(..)) where
data ClockTime = ClockTime
	deriving (Eq, Ord, Show)
data TimeDiff = TimeDiff { tdYear, tdMonth, tdDay, tdHour, tdMin, tdSec :: Int, tdPicosec :: Integer }
	deriving (Eq, Ord, Show)

getClockTime :: IO ClockTime
getClockTime = return ClockTime

diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes _ _ = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour = 0, tdMin = 0, tdSec = 0, tdPicosec = 0 }

addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime _ _ = ClockTime

data CalendarTime = CalendarTime {
        ctYear :: Int,
        ctMonth :: Month,
        ctDay  :: Int,
        ctHour :: Int,
        ctMin  :: Int,
        ctSec  :: Int,
        ctPicosec :: Integer,
        ctWDay :: Day,
        ctYDay :: Int,
        ctTZName:: String,
        ctTZ   :: Int,
        ctIsDST:: Bool
        } deriving (Eq, Ord, Show, Read)

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
        deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data Month = January | February | March | April | May | June | 
        July | August | September | October | November | December
        deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime _ = return $ CalendarTime { ctYear = 2000, ctMonth = January }
