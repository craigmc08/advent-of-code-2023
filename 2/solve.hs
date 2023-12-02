#!/usr/bin/env stack
-- stack script --resolver lts-21.13 --package text

{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl', intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

newtype Color = Color {colorName :: Text} deriving (Eq, Show)

type CubeSet = [(Color, Int)]

data Game = Game
  { gmId :: !Int,
    gmHandfuls :: ![CubeSet]
  }
  deriving (Eq, Show)

-- | Parses a game from a line of text. Expected format:
--
-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
parseGame :: Text -> Either Text Game
parseGame gameText = case T.splitOn ": " gameText of
  [idText, handfulsText] -> Game <$> parseId idText <*> parseHandfuls handfulsText
  _ -> Left "no parse: game: not exactly two parts split on `: `"
  where
    parseId :: Text -> Either Text Int
    parseId idText = case T.words idText of
      [_, digits] -> case readMaybe (T.unpack digits) of
        Just id -> Right id
        Nothing -> Left "no parse: game: id: number part isn't a number"
      _ -> Left "no parse: game: id: not exactly two words"

    parseHandfuls :: Text -> Either Text [CubeSet]
    parseHandfuls handfulsText = mapM parseHandful $ T.splitOn "; " handfulsText

    parseHandful :: Text -> Either Text CubeSet
    parseHandful handfulText = mapM parseCount $ T.splitOn ", " handfulText

    parseCount :: Text -> Either Text (Color, Int)
    parseCount countText = case T.words countText of
      [digits, color] -> case readMaybe (T.unpack digits) of
        Just count -> Right (Color color, count)
        Nothing -> Left "no parse: game: handfuls: count isn't a number"
      _ -> Left "no parse: game: handfuls: count isn't exactly two words"

showCubeSet :: CubeSet -> String
showCubeSet cs = intercalate ", " $ map (\(color, amount) -> unwords [show amount, T.unpack $ colorName color]) cs

showGame :: Game -> Text
showGame game = T.pack $ "Game " <> show (gmId game) <> ":" <> intercalate "; " (map showCubeSet $ gmHandfuls game)

-- | Lower upper bound of two cube sets.
cubeSetJoin :: CubeSet -> CubeSet -> CubeSet
cubeSetJoin x y =
  map
    ( \(color, countX) -> (color, max countX $ fromMaybe 0 $ lookup color y)
    )
    x

cubeSetPower :: CubeSet -> Int
cubeSetPower cubeSet = product $ map snd cubeSet

main :: IO ()
main = do
  input <- TIO.getContents
  games <- case mapM parseGame (T.lines input) of
    Right games -> return games
    Left err -> TIO.putStrLn err >> exitFailure

  args <- getArgs
  case args of
    "p1" : _ -> solveP1 games
    "p2" : _ -> solveP2 games
    _ -> error "first arg should be `p1` or `p2`"

solveP1 :: [Game] -> IO ()
solveP1 games =
  let available = [(Color "red", 12), (Color "green", 13), (Color "blue", 14)]
      isPossibleCount :: (Color, Int) -> Bool
      isPossibleCount (color, amount) = amount <= fromMaybe 0 (lookup color available)
      possibleGames = filter (all (all isPossibleCount) . gmHandfuls) games
      sumOfPossibleIds = sum $ map gmId possibleGames
   in print sumOfPossibleIds

solveP2 :: [Game] -> IO ()
solveP2 games =
  let minCubeSets = map (findMinCubeSet . gmHandfuls) games
      powers = map cubeSetPower minCubeSets
   in print (map showCubeSet minCubeSets) >> print (sum powers)
  where
    findMinCubeSet :: [CubeSet] -> CubeSet
    findMinCubeSet cubeSets = foldl' cubeSetJoin [(Color "red", 0), (Color "green", 0), (Color "blue", 0)] cubeSets
