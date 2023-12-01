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

  let recoverCalibrationNums = case args of
        ("p1" : _) -> recoverCalibrationNumsFromDigits
        ("p2" : _) -> recoverCalibrationNumsFromDigitsAndWords
        _ -> error "first argument should be `p1' or `p2'"

  input <- TIO.getContents
  let calibrationNums = recoverCalibrationNums input
  print $ sum calibrationNums

recoverCalibrationNumsFromDigits :: Text -> [Int]
recoverCalibrationNumsFromDigits text = map recoverCalibrationNum $ T.lines text
  where
    recoverCalibrationNum :: Text -> Int
    recoverCalibrationNum =
      (read :: String -> Int)
        . (\(a, b) -> [a, b])
        . (T.head &&& T.last)
        . T.filter isDigit

recoverCalibrationNumsFromDigitsAndWords :: Text -> [Int]
recoverCalibrationNumsFromDigitsAndWords = map recoverCalibrationNum . T.lines
  where
    recoverCalibrationNum :: Text -> Int
    recoverCalibrationNum =
      (read :: String -> Int)
        . (\(a, b) -> [a, b])
        . (head &&& last)
        . collectDigits

    collectDigits :: Text -> [Char]
    collectDigits text = case T.uncons text of
      Nothing -> []
      Just (c, cs) ->
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
