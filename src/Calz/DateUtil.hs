module Calz.DateUtil where

import           Data.Time
import           Data.Time.Calendar.WeekDate

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z


----- SundayWeekDate ----------------------------------------------------------

-- | Like toWeekDate, but uses 1 for Sunday and 7 for Saturday.
toSundayWeekDate :: Day -> (Integer, Int, Int)
toSundayWeekDate = toWeekDate . succ

-- | Like fromWeekDate, but uses 1 for Sunday and 7 for Saturday.
fromSundayWeekDate :: (Integer, Int, Int) -> Day
fromSundayWeekDate = pred . uncurry3 fromWeekDate

-- | Get the Sunday that starts the week for a given day
firstOfSundayWeek :: Day -> Day
firstOfSundayWeek day = fromSundayWeekDate (y, w, 1)
  where (y, w, _) = toSundayWeekDate day

-- | Get the Saturday that ends the week for a given day
lastOfSundayWeek :: Day -> Day
lastOfSundayWeek day = fromSundayWeekDate (y, w, 7)
  where (y, w, _) = toSundayWeekDate day

-- | Extract the week of a day, treating Sunday as the first day of the week.
getSundayWeek :: Day -> (Integer, Int)
getSundayWeek day = (y, w) where (y, w, _) = toSundayWeekDate day

-- | Check whether the weeks of two dates are the same, starting with Sundays
sameSundayWeek :: Day -> Day -> Bool
sameSundayWeek day1 day2 = getSundayWeek day1 == getSundayWeek day2


----- Projections -------------------------------------------------------------

getDay :: Day -> Int
getDay day = let (_, _, d) = toGregorian day in d

getMonth :: Day -> (Integer, Int)
getMonth day = let (y, m, _) = toGregorian day in (y, m)

getYear :: Day -> Integer
getYear day = let (y, _, _) = toGregorian day in y


----- Computing offsets -------------------------------------------------------

addMonth :: Day -> Day
addMonth = addGregorianMonthsClip 1

addYear :: Day -> Day
addYear = addGregorianMonthsClip 12

----- Month beginnings and endings --------------------------------------------

firstDayOfMonth :: (Integer, Int) -> Day
firstDayOfMonth (y, m) = fromGregorian y m 1

thisMonth :: Day -> Day
thisMonth day = firstDayOfMonth . getMonth $ day

nextMonth :: Day -> Day
nextMonth = addMonth . thisMonth
