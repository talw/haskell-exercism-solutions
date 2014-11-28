module Gigasecond(fromDay)
where

import Data.Time

fromDay :: Day -> Day
fromDay = addDays $ round (10**9 / 3600 / 24)
