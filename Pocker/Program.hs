import Poker

readCard :: String -> Card
readCard xs = Card (readValue $ head xs) (readSuit $ last xs)
	where
		readValue a
			| a == '2' 	= C2
			| a == '3' 	= C3
			| a == '4' 	= C4
			| a == '5' 	= C5
			| a == '6' 	= C6
			| a == '7' 	= C7
			| a == '8'	= C8
			| a == '9' 	= C9
			| a == 'T'	= C10
			| otherwise = (read::String -> Value) [a]
		readSuit b = (read::String -> Suit) [b]

readCardList :: [String] -> [Card]
readCardList xs = map readCard xs		

--main = do
--		list <- 
