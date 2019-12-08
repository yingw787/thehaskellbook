-- employeeRank.hs
--
-- Manipulate how functions are applied to arguments.
--
-- Prelude> :t compare
-- compare :: Ord a => a -> a -> Ordering
-- Prelude> :info Ordering
-- data Ordering = LT | EQ | GT 	-- Defined in ‘GHC.Types’
-- Prelude> compare 10 9
-- GT
-- Prelude> compare 9 9
-- EQ
-- Prelude> compare 9 10
-- LT
module EmployeeRank where

data Employee =
    Coder |
    Manager |
    Veep |
    CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
    putStrLn $ show e ++
        " is the boss of " ++
        show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
    case compare e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'
