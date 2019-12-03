-- dates.hs
module Dates where

-- Define the day of the week.
data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    -- derive from `Show` in order to be able to execute print methods, called
    -- naturally by GHCi.
    --
    -- derive from `Ord` to be able to set a comparison between days of the
    -- week.
    deriving (Ord, Show)

-- Define the day of the week and numerical day of the month.
data Date =
    Date DayOfWeek Int

-- Implement equality for data type 'DayOfWeek'.
instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    -- Unconditional case, for checking mismatched cases.
    (==) _ _ = False

-- Implement equality for data type 'Date'.
instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'
