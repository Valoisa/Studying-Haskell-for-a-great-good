import System.IO
totalLength :: [String] -> Int
totalLength xs = sum $ map (length) xs

main = do
	handle <- openFile "strings.txt" ReadMode
	{-a <- hGetContents handle
	lines a
	putStrLn a -} -- Выводит содержимое правильно
	a <- fmap (totalLength.lines) $ hGetContents handle
        hClose handle
	putStrLn $ "Total Length of the entered Strings: " ++ show a -- a = 0 
	
