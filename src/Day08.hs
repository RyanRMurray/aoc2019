module Day08 where
    import Data.List.Split
    import Data.List
    import Data.Ord


    processInput :: String -> [Int]
    processInput input = map read $ chunksOf 1 input

    makeImage :: [Int] -> Int -> Int -> [[[Int]]]
    makeImage input x y = map (chunksOf x) $ chunksOf (x*y) input

    part1 :: [[[Int]]] -> Int
    part1 img = snd $ minimumBy (comparing fst) 
        [(zeroes, checksum) | layer <- img, 
            let zeroes = count 0 layer, 
            let checksum = count 1 layer * count 2 layer
        ]
      where
        count x = length . (filter (==x)) . concat

    overlay :: Int -> Int -> Int
    overlay 2 x = x
    overlay y x = y

    part2 :: [[[Int]]] -> [[Int]]
    part2 (l:ls) = foldl (zipWith overlayLine) l ls
      where
        overlayLine = zipWith overlay


    printImg :: [[Int]] -> IO ()
    printImg = mapM_ (print . ln)
      where
        ln = map (\x -> if x == 0 then ' ' else 'X')

    
    day08 input = do
        let proc = makeImage (processInput input) 25 6
        putStr "Part 1: "
        print $ part1 proc
        putStrLn "Part 2: "
        printImg $ part2 $ proc

