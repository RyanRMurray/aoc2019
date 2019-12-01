module Day01 where
    import Data.List.Split

    processInput :: String -> [Int]
    processInput = map read . splitOn "\n" 

    part1 :: Int -> Int
    part1 m = (div m 3) - 2

    part2 :: Int -> Int
    part2 m
        | fuel <= 0 = 0
        | otherwise = fuel + part2 fuel
      where
        fuel = part1 m

    solve :: (Int -> Int) -> [Int] -> IO ()
    solve f x= putStrLn . show . sum $ map f x 
    
    day01 input = do
        putStr "Part 1: "
        solve part1 proc
        putStr "Part 2: "
        solve part2 proc
      where
        proc = processInput input