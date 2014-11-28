module Meetup (Weekday(..), Schedule(..), meetupDay)
where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

data Weekday = Sunday |
               Monday |
               Tuesday |
               Wednesday |
               Thursday |
               Friday |
               Saturday
            deriving (Enum)

data Schedule = First |
                Second |
                Third |
                Fourth |
                Last |
                Teenth
            deriving (Enum)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay Last weekDay year month = fromGregorian year month calculatedMeetUpDay
    where weekDayNumber = fromEnum weekDay
          lastDayOfMonth = removeDay $ fromGregorian year (month `mod` 12 + 1) 1
              where removeDay day = case toGregorian day of
                                    (year,1,1) -> fromGregorian year 12 31
                                    _ -> addDays (-1) day
          (_,_,lastDay30Number) = toGregorian lastDayOfMonth
          lastDay7Number = snd $ sundayStartWeek lastDayOfMonth
          diff = (lastDay7Number - weekDayNumber) `mod` 7
          calculatedMeetUpDay = lastDay30Number - diff

meetupDay Teenth weekDay year month = firstMeetupDayAfter 10 weekDay year month
meetupDay First weekDay year month = firstMeetupDayAfter 1 weekDay year month
meetupDay schedule weekDay year month = addDays (toInteger diff) resultForFirst
    where resultForFirst = meetupDay First weekDay year month
          diff = 7 * (fromEnum schedule - fromEnum First)

firstMeetupDayAfter :: Int -> Weekday -> Integer -> Int -> Day
firstMeetupDayAfter initialDayNum weekDay year month = fromGregorian year month calculatedMeetUpDay
    where weekDayNumber = fromEnum weekDay
          initialDayNumber = snd $ sundayStartWeek $ fromGregorian year month initialDayNum
          diff = (weekDayNumber - initialDayNumber) `mod` 7
          calculatedMeetUpDay = initialDayNum + diff

--Wish I could use this version but apparently I can't use different number of arguments
--for the same definiton (for the Last case), is there an extension to bypass this?
{-meetupDay First = firstMeetupDayAfter 1-}
{-meetupDay Teenth = firstMeetupDayAfter 10-}
{-meetupDay schedule = fromGregorian year month . addDays diff . meetupDay First-}
    {-where diff = 7 * (fromEnum schedule - fromEnum First)-}
