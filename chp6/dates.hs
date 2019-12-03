-- dates.hs
module Dates where

-- Define the day of the week.
data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    -- derive from `Show` in order to be able to execute print methods, called
    -- naturally by GHCi.
    --
    -- derive from `Ord` to be able to set a custom comparison between days of
    -- the week. If a custom comparison is not implemented, then comparison will
    -- set values to the left of the instantiation to be less than the values on
    -- the right.
    deriving (Show)

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

-- Implement a custom comparison where Friday is always the greatest day of the
-- week.
--
-- Note that this comparison does not specify equality for same dates besides
-- Friday. However, since Ord inherits Eq, the underlying Eq comparison takes
-- over and operations like `Mon == Mon` are still True.
instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ

-- Implement equality for data type 'Date'.
instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'
