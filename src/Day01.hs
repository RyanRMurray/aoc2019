module Day01 where
    import Data.List.Split

    processInput :: String -> [Int]
    processInput input =
        map read $ splitOn "\n" input

    fuelFunc :: Int -> Int
    fuelFunc m = (div m 3) - 2

    allFuelFunc :: Int -> Int
    allFuelFunc m
        | fuel <= 0 = 0
        | otherwise = fuel + allFuelFunc fuel
      where
        fuel = fuelFunc m

    part01 :: String -> IO ()
    part01 input = 
        putStrLn $ show $ sum $ map fuelFunc $ processInput input

    part02 :: String -> IO ()
    part02 input =
        putStrLn $ show $ sum $ map allFuelFunc $ processInput input
    
    day01 input = do
        putStr "Part 1: "
        part01 input
        putStr "Part 2: "
        part02 input