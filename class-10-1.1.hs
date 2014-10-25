import System.Environment
totalLength :: [String] -> Int
totalLength xs = sum $ map (length) xs

main = do
	a <- fmap totalLength getArgs
	putStrLn $ "Total Length of the entered Strings: " ++ show a