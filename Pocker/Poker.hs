module Pocker where
import Data.List

data Suit = C | D | H | S deriving (Show, Read, Eq)

data Value = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | A 
	deriving (Ord, Eq, Read, Show, Enum)

data Card = Card Value Suit deriving (Eq)
instance Ord Card where                 -- в одну строчку!
	(Card a _) `compare`  (Card b _) = a `compare` b 


data HandType = HighCard | OnePair | TwoPairs |
			ThreeOfAKind | Straight | Flush |
			FullHouse | FourOfAKind | StraightFlush | RoyalFlush
			deriving (Ord, Eq)

type Hand = (HandType, [Card])

{- ******* Вспомогательные функции ******* -}
-- Превращает список карт из файла в список десяток
makeTens :: [a] -> [[a]]
makeTens xs = map (take 10) 
	$ takeWhile (\xs -> length xs > 0) $ iterate (drop 10) xs

-- Превращает список десяток в список пар, элементы которых - упорядоченные пятёрки	(ставки)
makePairs :: (Ord a) => [[a]] -> [([a], [a])]
makePairs ys = map (\xs -> (sort (take 5 xs), sort (drop 5 xs))) ys

-- Группирует стоящие рядом одинаковые карты
groupAlike :: (Eq a) => [a] -> [[a]]
groupAlike xs = groupBy (\x y -> x == y) xs

{- ******* Проверки на ту или иную ставку: ******* -}
-- Флэш 
isFlush :: [Card] -> Bool
isFlush xs = length (nub $ foldl (\acc (Card a b) -> b : acc) [] xs) == 1

-- Стрит
isStraight:: [Card] -> Bool
isStraight xs = valList ==((take 5) [h..])
	where 
		valList = foldl (\acc (Card a b) -> a : acc) [] xs
		h = head valList

-- Стрит флэш		
isStraightFlush :: [Card] -> Bool
isStraightFlush xs = (isFlush xs) && (isStraight xs)

-- Флэш рояль
isRoyalFlush :: [Card] -> Bool
isRoyalFlush xs = (isFlush xs) && 
				(foldl (\acc (Card a b) -> a : acc) [] xs) == [C10 .. A]

-- Каре
isFourOfAKind :: [Card] -> Bool
isFourOfAKind xs = (take 4 valList) == take 4 (iterate (\x -> x) h) 
					|| (drop 1 valList) == take 4 (iterate (\x -> x) h)
	where 
		valList = foldl (\acc (Card a b) -> a : acc) [] xs
		h = head valList

-- Фулл хаус		
isFullHouse :: [Card] -> Bool
isFullHouse xs = length (nub valList) == 2
	where
		valList = foldl (\acc (Card a b) -> a:acc) [] xs

-- Одна пара		
isOnePair :: [Card] -> Bool
isOnePair xs = length (nub valList) == 4
	where
		valList = foldl (\acc (Card a b) -> a:acc) [] xs

-- Две пары		
isTwoPairs :: [Card] -> Bool
isTwoPairs xs = (sort $ nub $ map length $ groupAlike valList) == [1, 2, 2]
	where
		valList = foldl (\acc (Card a b) -> a:acc) [] xs

-- Тройка		
isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind xs = elem 3 $ map length $ groupAlike valList
	where
		valList = foldl (\acc (Card a b) -> a : acc) [] xs

-- Возвращает старшую карту (поскольку ставки упорядочены, то она будет в конце)		
getHighCard :: [Card] -> Card
getHighCard xs = last xs


--Функции для проверки на выигрыш
identHand :: [Card] -> Hand
identHand xs 
	| isRoyalFlush xs		= (RoyalFlush, xs)
	| isStraightFlush xs	= (StraightFlush, xs)
	| isStraight xs			= (Straight, xs)
	| isFourOfAKind xs		= (FourOfAKind, xs)
	| isFullHouse xs		= (FullHouse, xs)
	| isThreeOfAKind xs		= (ThreeOfAKind, xs)
	| isTwoPairs xs 		= (TwoPairs, xs)
	| isOnePair xs 			= (OnePair, xs)
	| otherwise 			= (HighCard, xs)

-- Если у игроков одинаковые ставки	
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
