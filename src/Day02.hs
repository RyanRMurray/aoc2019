module Day02 where
    import Data.List.Split
    import qualified Data.Map as M
    import           Data.Map (Map)

    loadMemoryWithNV ::  Int -> Int -> [Int] -> Map Int Int
    loadMemoryWithNV n v mem = M.insert 2 v $ M.insert 1 n $ 
        foldl (\l (i,m) -> M.insert i m l) (M.empty) (zip [0..] mem)

    loadMemory ::  [Int] -> Map Int Int
    loadMemory mem = 
        foldl (\l (i,m) -> M.insert i m l) (M.empty) (zip [0..] mem)

    processInput :: String -> [Int]
    processInput = (map read) .  splitOn ","

    loadOps :: Int -> Map Int Int -> [Int]
    loadOps i m = foldl (\l ix -> (m M.! ix) : l) [] [ix | x <- [3,2,1,0], let ix = i + x]

    part1 :: Int -> Map Int Int -> Int
    part1 ptr mem 
        | o == 1    = part1 (ptr + 4) $ M.insert c ((mem M.! a) + (mem M.! b)) mem
        | o == 2    = part1 (ptr + 4) $ M.insert c ((mem M.! a) * (mem M.! b)) mem
        | o == 99   = mem M.! 0
      where
        (o:a:b:c:_) = loadOps ptr mem

    part2 :: Map Int Int -> [(Int,Int)] -> Int
    part2 mem ((n,v):nvs)
        | res == 19690720 = 100 * n + v
        | otherwise       = part2 mem nvs
      where
        res = part1 0 (M.insert 2 v $ M.insert 1 n $ mem)

    nvPairs :: Int -> Int -> [(Int,Int)]
    nvPairs n v = [(xn,xv) | xn <- [0..n], xv <- [0..v]]
    

    day02 input = do
        let proc = processInput input
        putStr "Part 1: "
        putStrLn . show $ part1 0 $ loadMemoryWithNV 12 2 proc
        putStr "Part 2: "
        putStrLn. show $ part2 (loadMemory proc) (nvPairs 100 100)
