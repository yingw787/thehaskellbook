-- Madness.hs
module Madness where

import Data.Monoid

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."


main :: IO ()
main = do
    print $ madlibbin'
            ("Alas!" :: Exclamation)
            ("fiercely" :: Adjective)
            ("dog" :: Noun)
            ("hairy" :: Adjective)
