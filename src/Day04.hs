module Day04 where
    import           Data.List.Split
    import           Data.Sort


    processInput :: String -> (Int, Int)
    processInput = f . (splitOn "-")
      where 
        f (x:y:_) = (read x, read y)


    isAscend :: String -> Bool
    isAscend x = sort x == x

    digs = ['0'..'9']

    hasDubs :: String -> Bool
    hasDubs str = or $ map f digs
      where
        f c = (length $ filter (==c) str) > 1

    onlyDubs :: String -> Bool
    onlyDubs str = or $ map f digs
      where
        f c = (length $ filter (==c) str) == 2
    
    part1 :: [String] -> Int
    part1 =  length . filter (hasDubs) 
    
    part2 :: [String] -> Int
    part2 =  length . filter (onlyDubs)
    

    day04 input = do
        let (x,y) = processInput input
            proc  = filter (isAscend) $ map show [x..y]
        putStr "Part 1: "
        print $ part1 proc
        putStr "Part 2: "
        print $ part2 proc