import System.Environment
import Data.List
import Data.Char

build :: Char -> Int -> Maybe [String]
build a 0 = Nothing
build a n = Just $ take n $ iterate  (++ [a]) [a]

main = do
	putStrLn "Enter a char"
	a <- getChar
	putStrLn "Enter an integer"
	n <- fmap ord getChar
	print $ build a n