module Day07 where
    import Data.List.Split
    import Data.List
    import Debug.Trace
    import qualified Data.Map as M

    import IntCode

    processInput :: String -> [Int]
    processInput = map read .  splitOn ","

    
    execAmpProg :: Memory -> [Int] -> Int
    execAmpProg mem input = head . output . runIntMachine $ 
        IntMachine mem input [] 0

    execSequence :: Memory -> Int -> [Int] -> Int
    execSequence _   input []      = input
    execSequence mem input (a:as)  = 
        execSequence mem (execAmpProg mem [a,input]) as


    part1 :: Memory -> [Int] -> Int
    part1 mem modes =  maximum . map (execSequence mem 0) $ permutations modes

    execLoopSequence :: Memory -> [Int] -> Int
    execLoopSequence mem modes = 
        feedBackLoop ms (addInput m [0]) []  
      where
        (m:ms) = map (\x -> IntMachine mem [x] [] 0) modes

    part2 :: Memory -> [Int] -> Int
    part2 mem modes = maximum . map (execLoopSequence mem) $ permutations modes
    

    day07 input = do
        let prog = loadMemory $ processInput input
        putStr "Part 1: "
        print $ part1 prog [0,1,2,3,4]
        putStr "Part 2: "
        print $ part2 prog [5,6,7,8,9]
