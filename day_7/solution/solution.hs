import Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
    handsUnparsed <- readFile "input.txt" >>= return . lines
    let handsWithBids = Prelude.map handStringToTuple handsUnparsed
    let sortedHandsIndexed = addIndices $ sortBy compareRanks handsWithBids
    print $ sum [ bid * index | (_, bid, index) <- sortedHandsIndexed]

compareRanks :: (String, Int) -> (String, Int) -> Ordering
compareRanks (hand1, _) (hand2, _) =
    case getBiggerHand hand1 hand2 of
        x | x == hand1  -> GT
          | x == hand2 -> LT

getBiggerHand :: String -> String -> String
getBiggerHand hand1 hand2
    | getRelativeRank hand1 > getRelativeRank hand2 = hand1
    | getRelativeRank hand1 < getRelativeRank hand2 = hand2
    | otherwise = compareEqualRanks hand1 hand2

getRelativeRank :: String -> Int
getRelativeRank hand =
    let cardCount = fromList [(card, length (Prelude.filter (==card) hand)) | card <- cards]
        counts = Map.elems cardCount
    in deterMineHandRank counts

deterMineHandRank :: [Int] -> Int
deterMineHandRank counts
    | 5 `elem` counts = 7
        | 4 `elem` counts = 6
        | 3 `elem` counts && 2 `elem` counts = 5
        | 3 `elem` counts = 4
        | (length $ Prelude.filter (==2) counts) == 2 = 3
        | 2 `elem` counts = 2
        | otherwise = 1

compareEqualRanks :: String -> String -> String
compareEqualRanks hand1 hand2
    | compareEqualRanksHelper hand1 hand2 == LT = hand1
    | compareEqualRanksHelper hand1 hand2 == GT = hand2

compareEqualRanksHelper :: String -> String -> Ordering
compareEqualRanksHelper (card1:hand1) (card2:hand2) =
    compare (fromJust (elemIndex card1 cards)) (fromJust (elemIndex card2 cards)) <> compareEqualRanksHelper hand1 hand2

handStringToTuple :: String -> (String, Int)
handStringToTuple s = (hand, read bid)
    where
        (hand, bidUnparsed) = break isSpace s
        bid = dropWhile isSpace bidUnparsed

addIndices :: [(String, Int)] -> [(String, Int, Int)]
addIndices xs = zipWith (\(s, i) index -> (s, i, index)) xs [1..]

cards :: [Char]
cards = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']