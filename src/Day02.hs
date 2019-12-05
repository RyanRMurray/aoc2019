module Day02 where
    import           Data.List.Split
    import qualified Data.Map as M
    import           Data.Map (Map)

    import           IntCode

    processInput :: String -> [Int]
    processInput = (map read) .  splitOn ","

    part1 :: Memory -> Int
    part1 mem = (M.!) (fst $ execVariedIntCode mem [] [] 0) 0

    part2 :: Memory -> [(Int,Int)] -> Int
    part2 mem ((n,v):nvs)
        | res == 19690720 = 100 * n + v
        | otherwise       = part2 mem nvs
      where
        res = part1 (M.insert 2 v $ M.insert 1 n $ mem)

    nvPairs :: Int -> Int -> [(Int,Int)]
    nvPairs n v = [(xn,xv) | xn <- [0..n], xv <- [0..v]]
    
    day02 input = do
        let proc = processInput input
        putStr "Part 1: "
        putStrLn . show . part1 $ loadMemoryWithNV 12 2 proc
        putStr "Part 2: "
        putStrLn . show $ part2 (loadMemory proc) (nvPairs 99 99)
