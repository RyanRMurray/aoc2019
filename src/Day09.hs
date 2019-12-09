module Day09 where
    import Data.List.Split

    import IntCode

    processInput :: String -> Memory
    processInput = loadMemory . (map read) . (splitOn ",")

    part1 :: Memory -> [Int]
    part1 mem = output $ runIntMachine $ IntMachine mem [1] [] 0 0

    part2 :: Memory -> [Int]
    part2 mem = output $ runIntMachine $ IntMachine mem [2] [] 0 0

    
    day09 input = do
        let prog = processInput input
        putStr "Part 1: "
        print $ part1 prog
        putStr "Part 2: "
        print $ part2 prog