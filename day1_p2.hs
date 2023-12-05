import Data.Char (isDigit, digitToInt, isAlpha)
import Data.List (isInfixOf, findIndex, tails)
import Data.Text (splitOn, replace, pack, unpack, Text, isPrefixOf)
import Text.Read (Lexeme(String))
import Data.Either (lefts)
import Data.Maybe (fromJust, fromMaybe, isJust, catMaybes, mapMaybe)
import Control.Arrow (Arrow(first))

firstDigits :: String -> Maybe Int
firstDigits [] = Nothing
firstDigits (x:xs)
   | isDigit x = Just (read [x] :: Int)
   | pack "one" `isPrefixOf` pack (x:xs) = Just 1
   | pack "two" `isPrefixOf` pack (x:xs) = Just 2
   | pack "three" `isPrefixOf` pack (x:xs) = Just 3
   | pack "four" `isPrefixOf` pack (x:xs) = Just 4
   | pack "five" `isPrefixOf` pack (x:xs) = Just 5
   | pack "six" `isPrefixOf` pack (x:xs) = Just 6
   | pack "seven" `isPrefixOf` pack (x:xs) = Just 7
   | pack "eight" `isPrefixOf` pack (x:xs) = Just 8
   | pack "nine" `isPrefixOf` pack (x:xs) = Just 9
   | otherwise = Nothing

readLines :: FilePath -> IO Int
readLines = fmap parseLines . readFile

parseLines :: String -> Int
parseLines = sum . map convertToMaybeInt . lines

convertToMaybeInt :: String -> Int
convertToMaybeInt = do sumFirstLast . mapMaybe firstDigits . tails
                    where 
                        sumFirstLast [x] = sum [x*10,x]
                        sumFirstLast (x:xs) = sum [x*10, last xs]

main :: IO Int
main = readLines "input/day1.txt"
