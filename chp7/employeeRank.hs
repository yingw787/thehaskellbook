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

-- First-order approximation of method `employeeRank`.
--
-- ```haskell
-- *EmployeeRank> employeeRank Veep CEO
-- CEO is the boss of Veep
-- *EmployeeRank> employeeRank CEO Veep
-- CEO is the boss of Veep
-- ```
--
-- employeeRank :: Employee -> Employee -> IO ()
-- employeeRank e e' =
--     case compare e e' of
--         GT -> reportBoss e e'
--         EQ -> putStrLn "Neither employee is the boss"
--         -- Flip order of reporting by passing method `reportBoss` to method
--         -- `flip`.
--         LT -> (flip reportBoss) e e'

-- Second implementation of method `employeeRank`.
--
-- Now, method `employeeRank` leverages an *ordering function* that generates a
-- custom ordering. `employeeRank` becomes a higher-order function.
--
-- ```haskell
-- *EmployeeRank> employeeRank compare Veep CEO
-- CEO is the boss of Veep
-- *EmployeeRank> employeeRank compare CEO Veep
-- CEO is the boss of Veep
-- ```
employeeRank :: (
    Employee ->
        Employee ->
            Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'
