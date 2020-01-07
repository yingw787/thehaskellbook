-- ReplaceExperiment.hs
module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

-- Using [Char] to denote list type of String
lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- More concrete type signature
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- More specific functor '[]', as in '[Char]'.
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace
