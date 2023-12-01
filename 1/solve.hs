#!/usr/bin/env stack
-- stack script --resolver lts-21.13 --package text

{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Debug.Trace
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  -- Choose the correct algorithm based on which part we want to solve.
  let recoverCalibrationNums = case args of
        ("p1" : _) -> recoverCalibrationNumsFromDigits
        ("p2" : _) -> recoverCalibrationNumsFromDigitsAndWords
        _ -> error "first argument should be `p1' or `p2'"

  -- Read input from stdin and print the sum of the calibration numbers.
  input <- TIO.getContents
  let calibrationNums = recoverCalibrationNums input
  print $ sum calibrationNums

-- | Takes in input and returns a list of the calibration number on each line.
-- Used in solving part 1 of this puzzle.
recoverCalibrationNumsFromDigits :: Text -> [Int]
recoverCalibrationNumsFromDigits text = map recoverCalibrationNum $ T.lines text
  where
    -- Finds the first and last digit in a string and combines them into a number.
    recoverCalibrationNum :: Text -> Int
    recoverCalibrationNum =
      (read :: String -> Int)
        . (\(a, b) -> [a, b])
        . (T.head &&& T.last)
        . T.filter isDigit

-- | Takes in input and returns a list of the calibraiton number on each line.
-- Used in solving part 2 of this puzzle.
recoverCalibrationNumsFromDigitsAndWords :: Text -> [Int]
recoverCalibrationNumsFromDigitsAndWords = map recoverCalibrationNum . T.lines
  where
    -- Finds the first and last number in a line (whether that number is written
    -- as a digit or as a word like `one') and combines them.
    recoverCalibrationNum :: Text -> Int
    recoverCalibrationNum =
      (read :: String -> Int)
        . (\(a, b) -> [a, b])
        . (head &&& last)
        . collectDigits

    -- Finds all of the digits in a string. A digit is either a single character
    -- or a word like `one'. This will find words that share some letters, for
    -- example:
    --
    -- >>> collectDigits "oneightwone"
    -- [1, 8, 2, 1]
    --
    -- which is important for solving the puzzle, even though it isn't super
    -- clear from the puzzle description!
    collectDigits :: Text -> [Char]
    collectDigits text = case T.uncons text of
      Nothing -> []
      Just (c, cs) ->
        -- For the uninitiated, there's a fun little optimization based on laziness
        -- where this "wordAsDigit" variable isn't computed if the first character
        -- is a digit. Magic.
        let wordAsDigit =
              case find (\(word, _) -> word `T.isPrefixOf` text) digitNames of
                Nothing -> Nothing
                Just (_, digit) -> Just digit
            digit =
              if isDigit c
                then Just c
                else wordAsDigit
         in maybeToList digit ++ collectDigits cs

    digitNames =
      [ ("zero", '0'),
        ("one", '1'),
        ("two", '2'),
        ("three", '3'),
        ("four", '4'),
        ("five", '5'),
        ("six", '6'),
        ("seven", '7'),
        ("eight", '8'),
        ("nine", '9')
      ]
