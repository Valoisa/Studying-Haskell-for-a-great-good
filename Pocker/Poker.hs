import Data.List

data Suit = C | D | H | S deriving (Show, Read, Eq)

data Value = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | A 
	deriving (Ord, Eq, Read, Show)

data Card = Card Value Suit deriving (Eq)
instance Ord Card where                 -- в одну строчку!
	(Card a _) `compare`  (Card b _) = a `compare` b 


data HandType = HighCard | OnePair | TwoPairs |
			ThreeOfAKind | Straight | Flush |
			FullHouse | FourOfAKind | StraightFlush | RoyalFlush
			deriving (Ord, Eq)

type Hand = (HandType, [Card])

-- Вспомогательные функции
makeTens :: [a] -> [[a]]
makeTens xs = map (take 10) 
	$ takeWhile (\xs -> length xs > 0) $ iterate (drop 10) xs

makePairs :: (Ord a) => [[a]] -> [([a], [a])]
makePairs ys = map (\xs -> (sort (take 5 xs), sort (drop 5 xs))) ys

groupAlike :: (Eq a) => [a] -> [[a]]
groupAlike xs = groupBy (\x y -> x == y) xs

-- Проверки на ту или иную ставку: 
isFlush :: [Card] -> Bool
isFlush xs = length (nub $ foldl (\Card a b acc -> b:acc) [] xs) == 1

isStraight:: [Card] -> Bool
isStraight xs = valList == take 5 $ [a..]
	where 
		valList = foldl (\Card a b acc -> a:acc) [] xs
		a = head valList

isStraightFlush :: [Card] -> Bool
isStraightFlush xs = (isFlush xs) && (isStraight xs)
 
isRoyalFlush :: [Card] -> Bool
isRoyalFlush xs = (isFlush xs) && 
				(foldl (\Card a b acc -> a:acc) [] xs) == [C10 .. A]

isFourOfAKind :: [Card] -> Bool
isFourOfAKind xs = (take 4 valList) == take 4 (iterate (\x -> x) a) 
					|| (drop 1 valList) == take 4 (iterate (\x -> x) a)
	where 
		valList = foldl (\Card a b acc -> a:acc) [] xs
		a = head valList

isFullHouse :: [Card] -> Bool
isFullHouse xs = length (nub valList) == 2
	where
		valList = foldl (\Card a b acc -> a:acc) [] xs

isOnePair :: [Card] -> Bool
isOnePair xs = length (nub valList) == 4
	where
		valList = foldl (\Card a b acc -> a:acc) [] xs

isTwoPairs :: [Card] -> Bool
isTwoPairs xs = (sort $ nub $ map length $ groupAlike valList) == [1, 2, 2]
	where
		valList = foldl (\Card a b acc -> a:acc) [] xs

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind xs = elem 3 $ map length $ groupAlike valList
	where
		valList = foldl (\Card a b acc -> a:acc) [] xs

getHighCard :: [Card] -> Card
getHighCard xs = last xs

--Функции для проверки на выигрыш
identHand :: [Card] -> Hand
identHand xs 
	| isRoyalFlush xs		= (RoyalFlush, [Card])
	| isStraightFlush xs	= (StraightFlush, [Card])
	| isStraight xs			= (Straight, [Card])
	| isFourOfAKind xs		= (FourOfAKind, [Card])
	| isFullHouse xs		= (FullHouse, [Card])
	| isThreeOfAKind xs		= (ThreeOfAKind, [Card])
	| isTwoPairs xs 		= (TwoPairs, [Card])
	| isOnePair xs 			= (OnePair, [Card])
	| otherwise 			= (HighCard, [Card])

compareAlikeHand :: Hand -> Hand -> Bool
compareAlikeHand (a1, xs) (a2, ys)
	| (a1 == HighCard) || (a1 == Flush) 
		|| (a1 == Straight) || (a1 == StraightFlush) 
			= (getHighCard xs) > (getHighCard ys)
	| (a1 == OnePair) || (a1 == ThreeOfAKind) || (a1 == FullHouse) 
		|| (a1 == FourOfAKind) = (getHigh xs) > (getHigh ys)
	| otherwise  = (getHigh' xs) > (getHigh' ys)
	where 
		getHigh zs = head $ tail 
			$ sortBy (\p q -> compare (length p) (length q)) $ groupAlike zs
		getHigh' zs = maximum $ drop 3 $ concat 
			$ sortBy (\p q -> compare (length p) (length q)) $ groupAlike zs

compareHand :: Hand -> Hand -> Bool
compareHand (a1, xs) (a2, ys)
	| a1 < a2 									= False
	| a1 > a2 									= True
	| (a1 == RoyalFlush) && (a2 == RoyalFlush) 	= False
	| otherwise 								= 
		compareAlikeHand (a1, xs) (a2, ys)
