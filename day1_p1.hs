import Data.Char (isDigit)

readLines :: FilePath -> IO Int
readLines = fmap parseLines . readFile

getFirstLastNumber :: String -> String
getFirstLastNumber str = do
    let numbers = filter isDigit str
    case numbers of
        (x:xs) -> case xs of
            [] -> x:xs ++ x:xs
            _  -> x : take 1 (reverse xs)

parseLines :: String -> Int
parseLines = sum . map (read . getFirstLastNumber) . lines

main :: IO Int
main = readLines "input/day1.txt"