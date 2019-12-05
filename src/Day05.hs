module Day05 where
    import           Data.List.Split
    import qualified Data.Map as M
    import           Data.Map (Map)

    import IntCode
    
    processInput :: String -> [Int]
    processInput = (map read) .  splitOn ","

    diagnose :: Memory -> Int -> [Int]
    diagnose mem i = output $ execIntCode (IntMachine mem [i] [] 0)

    day05 input = do
        let proc = loadMemory $ processInput input
        putStr "Part 1: "
        print $ diagnose proc 1
        putStr "Part 2: "
        print $ diagnose proc 5