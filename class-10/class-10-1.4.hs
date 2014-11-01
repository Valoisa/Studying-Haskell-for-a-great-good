import System.Environment
import Data.List
import Data.Char

totalLength :: [String] -> Int
totalLength xs = sum $ map (length) xs

build :: Char -> Int -> Maybe [String]
build a 0 = Nothing
build a n = Just $ take n $ iterate  (++ [a]) [a]

main = do
	a <- fmap concat getArgs
	let b = fmap totalLength $ build (head a) $ (-)(ord $ last a) 48
	putStrLn $ "Total Length: " ++ show b