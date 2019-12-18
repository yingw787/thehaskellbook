-- src/Main.hs
module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  -- (INCORRECT, COMPILE-TIME ERROR)
  -- randomIndex <- randomRIO (0, (length allWords - 1))
  --
  -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char]
--       1      2            3
-- 1: Word we're trying to guess
-- 2: Characters filled in so far
-- 3: Letters guessed so far

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
    fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
freshPuzzle str = Puzzle str (map (const Nothing) str) []

charInWord :: Puzzle -> Char -> Bool
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
charInWord (Puzzle str _ _) char = elem char str

alreadyGuessed :: Puzzle -> Char -> Bool
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
alreadyGuessed (Puzzle _ _ guessed) char = elem char guessed

renderPuzzleChar :: Maybe Char -> Char
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s) where
  zipper guessed wordChar guessChar = if wordChar == guessed then Just wordChar else guessChar
  newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick\
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)

-- gameOver :: Puzzle -> IO ()
-- gameOver (Puzzle wordToGuess _ guessed) =
--   if (length guessed) > 7
--   then
--     do  putStrLn "You lose!"
--         putStrLn $ "The word was: " ++ wordToGuess
--       exitSuccess
--   else return ()

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: Not sure why there are so many "unexpected do block in
-- function application" errors when running "stack build")
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7
  then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else
       return ()

-- gameWin :: Puzzle -> IO ()
-- gameWin (Puzzle _ filledInSoFar _) =
--   if all isJust filledInSoFar then
--     do putStrLn "You win!"
--       exitSuccess
--   else return ()

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: Not sure why there are so many "unexpected do block in
-- function application" errors when running "stack build")
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledIn _) =
  if all isJust filledIn
  then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
