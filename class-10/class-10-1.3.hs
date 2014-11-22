{- 
    Написать функцию build :: Char -> Int -> Maybe [String], которая по 
    заданному символу и целому числу n строит список строк, содержащих 1, 
    2, ..., n символов. Функция должна возвращать Nothing, если n=0.

-}
import System.Environment
import Data.List
import Data.Char

build' :: (Char, Int) -> [String]
build' (a, n) = take n $ iterate  (++ [a]) [a]

buildRes :: (Char, Int) -> Maybe [String]
buildRes (a, n)
    | n == 0    = Nothing
    | otherwise = fmap  build' $ Just (a, n)

main = do
	putStrLn "Enter a char"
	a <- getChar
	getChar
	putStrLn "Enter an integer"
	nstr <- getLine
	let n = read nstr :: Int
	print $ buildRes (a,n)
