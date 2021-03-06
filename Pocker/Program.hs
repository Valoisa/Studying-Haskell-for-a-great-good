import Poker
import Data.List

readCard :: String -> Card
readCard xs = Card (readValue $ head xs) (readSuit $ last xs)
    where
        readValue a
            | a == '2'  = C2
            | a == '3'  = C3
            | a == '4'  = C4
            | a == '5'  = C5
            | a == '6'  = C6
            | a == '7'  = C7
            | a == '8'  = C8
            | a == '9'  = C9
            | a == 'T'  = C10
            | otherwise = (read::String -> Value) [a]
        readSuit b = (read::String -> Suit) [b]

readCardList :: [String] -> [([Card], [Card])]
readCardList xs = makePairs $ makeTens $ map readCard xs

makeHand :: ([Card], [Card]) -> (Hand, Hand)
makeHand (a, b) = (identHand a, identHand b)

decideTheWinner :: [([Card], [Card])] -> [Bool]
decideTheWinner xs = map (compareHand . makeHand) xs    

main = do
	count <- fmap words $ readFile "poker.txt"
	putStrLn $ "The first player has won " ++ show (length $ last $ groupAlike $ sort $ decideTheWinner $ readCardList count) ++ " times"
