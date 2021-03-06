module Poker where
import Data.List
import Data.Ord (comparing)

data Suit = C | D | H | S deriving (Show, Read, Eq)

data Value = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | A 
	deriving (Ord, Eq, Read, Show, Enum)

data Card = Card Value Suit deriving (Eq, Show)
instance Ord Card where
	(Card a _) `compare`  (Card b _) = a `compare` b 

data HandType = HighCard | OnePair | TwoPairs |
			ThreeOfAKind | Straight | Flush |
			FullHouse | FourOfAKind | StraightFlush | RoyalFlush
			deriving (Ord, Eq, Show)

type Hand = (HandType, [Card])

type Game = (Hand, Hand)

{- ******* Вспомогательные функции ******* -}
-- Превращает список карт из файла в список десяток
makeTens :: [a] -> [[a]]
makeTens xs = map (take 10) 
	$ takeWhile (not . null) $ iterate (drop 10) xs

-- Превращает список десяток в список пар, 
-- элементы которых - упорядоченные пятёрки	(ставки)
makePairs :: (Ord a) => [[a]] -> [([a], [a])]
makePairs ys = map (\xs -> (take 5 xs, drop 5 xs)) ys
				--(\cs -> let (f, s) = splitAt 5 cs in (sort f, sort s)) ys
				-- ((fork identHand) . splitAt 5 cs) -- если sort делать в identHand
				
-- Группирует стоящие рядом одинаковые карты
groupAlike :: (Eq a) => [a] -> [[a]] -- ~ group (Data.List)
groupAlike xs = groupBy (\x y -> x == y) xs

-- Список достоинств
valList :: [Card] -> [Value]
valList xs = reverse $ foldl (\acc (Card a b) -> a : acc) [] xs

{- ******* Проверки на ту или иную ставку: ******* -}
-- Флэш 
isFlush :: [Card] -> Bool
isFlush xs = length (nub $ foldl (\acc (Card a b) -> b : acc) [] xs) == 1

-- Стрит
isStraight:: [Card] -> Bool
isStraight xs = valList xs ==((take 5) [h..]) || valList xs == [A, C2, C3,C4, C5]
	where 
		h = head $ valList xs

-- Стрит флэш		
isStraightFlush :: [Card] -> Bool
isStraightFlush xs = (isFlush xs) && (isStraight xs)

-- Флэш рояль
isRoyalFlush :: [Card] -> Bool
isRoyalFlush xs = (isFlush xs) && 
				(foldl (\acc (Card a b) -> a : acc) [] xs) == [C10 .. A]

-- Каре
isFourOfAKind :: [Card] -> Bool
isFourOfAKind xs = take 4 (valList xs) == replicate 4 h
					|| drop 1  (valList xs) == replicate 4 h'
	where 
		h = head $ valList xs
		h' = last $ valList xs

-- Фулл хаус		
isFullHouse :: [Card] -> Bool
isFullHouse xs = length (nub $ valList xs) == 2
	
-- Одна пара		
isOnePair :: [Card] -> Bool
isOnePair xs = length (nub $ valList xs) == 4

-- Две пары		
isTwoPairs :: [Card] -> Bool
isTwoPairs xs = (sort $ map length $ groupAlike $ valList xs) == [1, 2, 2]


-- Тройка		
isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind xs = elem 3 $ map length $ groupAlike $ valList xs


-- Возвращает старшую карту
getHighCard :: [Card] -> Card
getHighCard xs = maximum xs

{----Функции для проверки на выигрыш---}
identHand :: [Card] -> Hand
identHand xs 
	| isRoyalFlush $ sort xs		= (RoyalFlush, sort xs)
	| isStraightFlush $ sort xs		= (StraightFlush, sort xs)
	| isFlush $ sort xs				= (Flush, sort xs)
	| isStraight $ sort xs			= (Straight, sort xs)
	| isFourOfAKind $ sort xs		= (FourOfAKind, sort xs)
	| isFullHouse $ sort xs			= (FullHouse, sort xs)
	| isThreeOfAKind $ sort xs		= (ThreeOfAKind, sort xs)
	| isTwoPairs $ sort xs 			= (TwoPairs, sort xs)
	| isOnePair $ sort xs 			= (OnePair, sort xs)
	| otherwise 					= (HighCard, sort xs)

-- Если у игроков одинаковые ставки	
compareAlikeHand :: (Hand, Hand) -> Bool
compareAlikeHand ((a1, xs), (a2, ys))
	| a1 `elem` [HighCard, Flush, Straight, StraightFlush] 
			= (getHighCard xs) > (getHighCard ys)
	| a1 `elem` [OnePair, ThreeOfAKind, FourOfAKind]
			= (getHigh xs) > (getHigh ys)
	| a1 == FullHouse
			= compareFullHouse xs ys
	| otherwise  = (getHigh' xs) > (getHigh' ys)
	where 
		sortByLength = sortBy (\a b -> length a `compare` length b)
		getHigh zs =  head $ last $ sortByLength $ groupAlike zs
		getHigh' zs = maximum $ drop 3 $ concat 
			$ sortByLength $ groupAlike zs
		compareFullHouse xs ys
			| (getHigh xs) > (getHigh ys) 	= True
			| (getHigh xs) < (getHigh ys) 	= False
			| otherwise						= (getHighInPair xs) > (getHighInPair ys)
				where
					getHighInPair zs = head $ head $ sortByLength $ groupAlike zs
			
compareHand :: (Hand, Hand) -> Bool
compareHand ((a1, xs), (a2, ys))
	| a1 < a2 									= False
	| a1 > a2 									= True
	| (a1 == RoyalFlush) && (a2 == RoyalFlush) 	= False
	| otherwise 								= 
		compareAlikeHand ((a1, xs), (a2, ys))
