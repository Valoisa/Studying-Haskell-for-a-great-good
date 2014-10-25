import System.IO
totalLength :: [String] -> Int
totalLength xs = sum $ map (length) xs

main = do
	handle <- openFile "strings.txt" ReadMode        
	a <- fmap totalLength $ hGetContents handle
        hClose handle
	putStrLn $ "Total Length of the entered Strings: " ++ show a
