{-
	Написать функцию reduce принимающую один целочисленный аргумент
	a и возвращающую 0, если аргумент делится на 3, a2, если он
	на 3 не делится и является при этом нечётным, a3 в остальных случаях. 
	Напишите для этой функции наименее ограничивающую аннотацию типов, 
	выяснив, какой класс типов достаточен для выполнения указанных операций.
-}

import System.Environment
import System.IO

reduce :: (Integral a) => a -> a
reduce a
        | a `mod` 3 == 0        = 0
        | a `mod` 2 == 0        = a^3
        | otherwise             = a^2


{- 
	Написать функцию, применяющую определённую выше функцию reduce 
	заданное количество раз к значению в контексте, являющемся функтором:
	reduceNF :: Functor f => Int -> f a -> f a
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n c1 =  last $ take n $ iterate (fmap reduce) c1
	
{-	
	Дан текстовый файл, содержащий пары положительных целых чисел в каждой 
	строке. Придумать способы преобразования этого списка к значению в 
	следующих контекстах:
	

    список;
    Maybe;
    Either;
    IO.

-}

makePairs :: (Integral a) => [a] -> [(a, a)]
--makePairs :: [Int] -> [(Int, Int)]
makePairs [] = []
makePairs (x:y:xs) = (x, y) : makePairs xs

readTuples :: String -> IO [(Int, Int)]
readTuples name = do
	conents <- readFile name
	return $ makePairs $ map read $ words conents

unpair :: (Integral a) => [(a, a)] -> [a]
unpair xs = foldl (\acc (x, y) -> acc ++ [x] ++ [y]) [] xs

doAll = do
	prs <- readTuples "abc.txt"
	print $ pairlist2maybe prs 
	
pairlist2maybe :: (Integral a) => [(a, a)] -> [Maybe (a, a)]
pairlist2maybe xs = foldl 
	(\acc (x, y) -> 
		if x /= 0 && y /= 0 
			then acc ++ [Just (x, y)] 
			else acc ++ [Nothing]) [] xs

pairlist2either :: (Integral a) => [(a, a)] -> [Either a a]
pairlist2either xs = foldl 
	(\acc (x, y) -> 
		if x /= 0 && y /= 0 
			then Left  x : acc
			else Right y : acc) [] xs

pairlist2IO :: (Integral a) => [(a, a)] -> IO [(a, a)]
pairlist2IO xs = undefined
