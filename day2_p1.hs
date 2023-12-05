

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseLines :: String -> [String]
parseLines = words

main :: IO [String]
main = readLines "input/day2.txt"

