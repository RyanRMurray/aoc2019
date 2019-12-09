module Day07 where
    import Data.List.Split
    import Data.List


    import IntCode

    processInput :: String -> [Int]
    processInput = map read .  splitOn ","

    execAmplifiers :: Memory -> [Int] -> Int
    execAmplifiers mem modes = 
        execArray ms (addInput m [0]) []  
      where
        (m:ms) = map (\x -> IntMachine mem [x] [] 0 0) modes

    maxOutput :: Memory -> [Int] -> Int
    maxOutput mem modes = maximum . map (execAmplifiers mem) $ permutations modes
    
    day07 input = do
        let prog = loadMemory $ processInput input
        putStr "Part 1: "
        print $ maxOutput prog [0,1,2,3,4]
        putStr "Part 2: "
        print $ maxOutput prog [5,6,7,8,9]
