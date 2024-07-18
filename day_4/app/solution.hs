import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (find)

main :: IO ()
main = do
    cards <- readFile "input.txt" >>= return . lines
    print $ sum $ map (getCardValue . getWinningNumberCount) cards

getCardValue:: Int -> Int
getCardValue 0 = 0
getCardValue n = 2 ^ (n - 1)

data Function = Head | Last deriving (Eq)

getWinningNumberCount :: String -> Int
getWinningNumberCount card =
    let winning = getNumbers Head card
        numbers = getNumbers Last card
    in length $ filter (numberIsWinning winning) numbers

getNumbers :: Function -> String -> [Int]
getNumbers func card =
    let parts = splitOn "|" card
        numbers = case func of
            Head -> head parts
            Last -> last parts
        processedNumbers = if func == Head
                            then drop 1 $ dropWhile (/= ':') numbers
                            else numbers
    in mapMaybe readMaybe (words processedNumbers)

numberIsWinning :: [Int] -> Int -> Bool
numberIsWinning winning x = maybe False (const True) $ find (== x) winning
