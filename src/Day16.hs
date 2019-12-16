{-# LANGUAGE BangPatterns #-}
module Day16 where
    import Data.List
    import Data.List.Split
    import Data.Digits
    import Debug.Trace

    processInput :: String -> [Int]
    processInput = (map read) . (chunksOf 1)

    patternAtElem :: Int -> [Int] ->  [Int]
    patternAtElem n p = tail . cycle . concat $ map (replicate n) p

    doPhase :: [Int] -> [Int] -> [Int]
    doPhase ins pattern = 
        [ mod (abs $ sum proc) 10
        | ix <- [1.. length ins]
        , let proc = zipWith (*) ins (patternAtElem ix pattern)
        ]

    doPhases :: Int -> [Int] -> [Int] -> [Int]
    doPhases 0 ins _ = ins

    doPhases x ins pattern = doPhases (x-1) (doPhase ins pattern) pattern

    part1 :: [Int] -> [Int]
    part1 input = take 8 $ doPhases 100 input [0,1,0,-1]

    
    --these won't solve part1 because i'm being a sneaky cheater and ignoring most of the inputs, since
    --they get zeroed by the time we reach the offset anyway. then the rest are all multiplied by one
    doKindaPhase :: [Int] -> [Int]
    doKindaPhase ins = 
       scanl' (\x y -> mod (x+y) 10) (head ins) $ tail ins
    
    doKindaPhases :: Int -> [Int] -> [Int]
    doKindaPhases 0 !ins = ins

    doKindaPhases x !ins = doKindaPhases (x-1) (doKindaPhase ins)

    part2 :: [Int] -> [Int]
    part2 input = 
        take 8 $ reverse $ doKindaPhases 100 active
      where
        --get the offset from the first input
        offset = unDigits 10 $ take 7 input
        --get the digits after the offset. since im technically making an infinite list,
        --i need to drop the offset then take however many are needed to get 10000 times the input
        active = reverse $ take ((length input) * 10000 - offset) $ drop offset $ cycle input


    day16 input = do
        let ins = processInput input
        putStr "Part 1: "
        print $ part1 ins
        putStr "Part 2: "
        print $ part2 ins
