module NonMonadic
  ( playOnce
  ) where

import Control.Monad (forM_)
import Control.Exception (bracket_)

import System.IO (hSetBuffering, BufferMode(..), hSetEcho, stdout, stdin)

{-
  - Setter: sets some numbers.
  - Player: guesses the numbers Setter sets.
-}

guess
  :: [Int] -- Numbers that's not matched yet.
  -> Int   -- Number input by Player.
  -> ([Ordering], [Int])
guess leftNs inputN = (results, map snd onlyNonMatched)
  where
    nonMatched (result, _) = result /= EQ
    results = map (compare inputN) leftNs
    onlyNonMatched = filter nonMatched (zip results leftNs)

playOnce :: IO ()
playOnce = do
  hSetBuffering stdout NoBuffering
  initialNs <- askNumbers
  putStrLn ""
  playOnceWith initialNs

playOnceWith
  :: [Int]  -- Inital numbers the Setter set.
  -> IO () -- Count how many times Player guesses till all the numbers matches
playOnceWith initialNs =
  playOnceCountingFrom
    1 -- this is the 1st play
    initialNs

playOnceCountingFrom :: Int -> [Int] -> IO ()
playOnceCountingFrom count ns = do
  putStrLn ""
  putStrLn ("Left numbers: " ++ show (length ns))

  putStr "Guess a number: "
  input <- getLine
  let (results, leftNs) = guess ns (read input)

  forM_ results tellResult

  if null leftNs
    then do
      putStrLn ""
      putStrLn ("Congratulations! All numbers have matched!")
      putStrLn ("You have guessed " ++ show count ++ " times.")
    else do
      playOnceCountingFrom (count + 1) leftNs

tellResult :: Ordering -> IO ()
tellResult EQ = putStrLn $ "  - Matched!"
tellResult GT = putStrLn $ "  - Too large!"
tellResult LT = putStrLn $ "  - Too small!"

-- From http://stackoverflow.com/questions/4064378/prompting-for-a-password-in-haskell-command-line-application
askNumbers :: IO [Int]
askNumbers = do
  putStr "Enter some numbers separated by space: "
  s <- bracket_ (hSetEcho stdin False) (hSetEcho stdin True) getLine
  return (map read (words s))
