module Day19 where
    import Data.List.Split
    import Data.List
    import Data.Maybe

    import IntCode

    type Pos = (Int, Int)

    processInput :: String -> [Int]
    processInput i = map read $ splitOn "," i

    part1CoOrds :: [Int]
    part1CoOrds = concat [[x,y] | x <- [0..49], y <- [0..49]]

    deployDrone :: IntMachine -> [Int] -> Int
    deployDrone m ins = head $ output $ runIntMachine $ addInput m ins

    part1 :: Memory -> Int
    part1 mem = sum $ map (deployDrone m) $ chunksOf 2 part1CoOrds
      where
        m = IntMachine mem [] [] 0 0


    checkForSanta :: IntMachine -> Int -> Int -> (Bool,Int)
    checkForSanta m y xMin =
        (out == 1,x)
      where
        x = fromJust $ find (\xx -> deployDrone m [xx,y] == 1) $ [xMin..]
        out = deployDrone m [x+99,y-99]

    findSanta :: IntMachine -> Int -> Int -> Pos
    findSanta m y xMin
        | res       = (newX,y-99)
        | otherwise = findSanta m (y+1) newX
      where
        (res,newX) = checkForSanta m y xMin

    part2 :: Memory -> Int
    part2 mem =
        (x*10000) + y
      where
        m = IntMachine mem [] [] 0 0
        (x,y) = findSanta m 100 0


    day19 input = do
        let mem = loadMemory $ processInput input
        putStr "Part 1: "
        print $ part1 mem
        putStr "Part 2: "
        print $ part2 mem