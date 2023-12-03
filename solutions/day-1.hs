module Main where

import Control.Applicative (Alternative (many))
import Data.Attoparsec.ByteString (Parser, choice, parseOnly, string, try)
import Data.Attoparsec.ByteString.Char8 (anyChar)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Char (digitToInt, isDigit)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

type Input = [[Char]]
type Solution = Int

parser :: B.ByteString -> Input
parser = map B8.unpack . B8.lines

solve1 :: Input -> Solution
solve1 = sum . map (createInt . map digitToInt . filter isDigit)

createInt :: [Int] -> Int
createInt xs = head xs * 10 + last xs

solve2 :: Input -> Solution
solve2 = sum . map (createInt . extractInts)

numMap :: [(B.ByteString, Int)]
numMap =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    , ("1", 1)
    , ("2", 2)
    , ("3", 3)
    , ("4", 4)
    , ("5", 5)
    , ("6", 6)
    , ("7", 7)
    , ("8", 8)
    , ("9", 9)
    ]

numParser :: Parser [Int]
numParser = many loop
  where
    loop = choice [try helper, anyChar >> loop]
    helper = do
        byteValue <- choice $ map (string . fst) numMap
        return $ (fromJust . lookup byteValue) numMap

extractInts :: [Char] -> [Int]
extractInts [] = []
extractInts (x : xs) =
    case helper (x : xs) of
        [] -> extractInts xs
        xs' -> head xs' : extractInts xs
  where
    helper = fromRight [] . parseOnly numParser . B8.pack

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
