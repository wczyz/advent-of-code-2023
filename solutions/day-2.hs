module Main where

import Data.Attoparsec.ByteString (Parser, choice, many1, parseOnly, sepBy, string)
import Data.Attoparsec.ByteString.Char8 (digit)
import Data.ByteString qualified as B
import Data.Either (fromRight)
import System.Environment (getArgs)

data Color = Red | Green | Blue
    deriving (Show, Eq, Enum, Bounded)

data Element = Element
    { _color :: Color
    , _count :: Int
    }
    deriving (Show, Eq)

type Set = [Element]

data Game = Game
    { _id :: Int
    , _sets :: [Set]
    }
    deriving (Show, Eq)

data Configuration = Configuration
    { _red :: Int
    , _green :: Int
    , _blue :: Int
    }
    deriving (Show, Eq)

type Input = [Game]
type Solution = Int

parser :: B.ByteString -> Input
parser = fromRight [] . parseOnly (pGame `sepBy` string "\n")
  where
    pGame :: Parser Game
    pGame = do
        _ <- string "Game "
        gameId <- pInt
        _ <- string ": "
        sets <- pSet `sepBy` string "; "
        return $ Game gameId sets

    pSet :: Parser Set
    pSet = pElement `sepBy` string ", "

    pElement :: Parser Element
    pElement = do
        count <- pInt
        _ <- string " "
        color <- pColor
        return $ Element color count

    pInt :: Parser Int
    pInt = read <$> many1 digit

    pColor :: Parser Color
    pColor =
        choice
            [ Red <$ string "red"
            , Green <$ string "green"
            , Blue <$ string "blue"
            ]

maxColor :: Color -> Int
maxColor = \case
    Red -> 12
    Green -> 13
    Blue -> 14

solve1 :: Input -> Int
solve1 =
    sum
        . map _id
        . filter isGamePossible
  where
    isGamePossible :: Game -> Bool
    isGamePossible (Game _ sets) =
        all isSetPossible sets

    isSetPossible :: Set -> Bool
    isSetPossible =
        all (\(Element color count) -> count <= maxColor color)

solve2 :: Input -> Solution
solve2 = sum . map (powerValue . getConfiguration)
  where
    powerValue :: Configuration -> Int
    powerValue (Configuration red green blue) =
        red * green * blue

    getConfiguration :: Game -> Configuration
    getConfiguration (Game _ sets) =
        Configuration
            { _red = colorConfig Red sets
            , _green = colorConfig Green sets
            , _blue = colorConfig Blue sets
            }

    colorConfig :: Color -> [Set] -> Int
    colorConfig color sets =
        case colorCounts color sets of
            [] -> 0
            xs -> maximum xs

    colorCounts :: Color -> [Set] -> [Int]
    colorCounts color sets =
        map _count $ filter (\(Element color' _) -> color' == color) (concat sets)

main :: IO ()
main = do
    [part, filepath] <- getArgs
    input <- parser <$> B.readFile filepath
    if read @Int part == 1
        then do
            putStrLn "solution to problem 1 is:"
            print $ solve1 input
        else do
            putStrLn "solution to problem 2 is:"
            print $ solve2 input
