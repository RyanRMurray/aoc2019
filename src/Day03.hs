module Day03 where
    import           Data.List.Split
    import qualified Data.Map as M
    import           Data.Map (Map)

    type CoOrd = (Int,Int)
    type Wire = [(Char, Int)]
    type WireSteps = [Char]
    type WireGrid = Map CoOrd Int
    data Stepper = Stepper
      { directions :: WireSteps
      , location   :: CoOrd
      , distance   :: Int
      , path       :: WireGrid
      }

    newStepper :: WireSteps -> Stepper
    newStepper w = Stepper w (0,0) 0 M.empty

    step :: Stepper -> Stepper
    step (Stepper (d:ds) l s m) = Stepper ds (next l d) (s+1) (M.insert l s m)

    next :: CoOrd -> Char -> CoOrd
    next (x,y)  'U' = (x,y+1)
    next (x,y)  'D' = (x,y-1)
    next (x,y)  'R' = (x+1,y)
    next (x,y)  'L' = (x-1,y)

    processInput :: String -> [Wire]
    processInput = 
        (map (map f))  .
        (map (splitOn ",")) . 
        (splitOn "\n")
      where
        f = \(c:i) -> (c,read i)

    processSteps :: Wire -> [Char]
    processSteps w = concat [replicate s d | (d,s) <- w]

    insertWires :: [Wire] -> WireGrid
    insertWires ws = M.unionsWith (+) $ map insertWire ws

    insertWire :: Wire -> WireGrid
    insertWire ws = M.unions . fst $ foldl insertSegment ([M.singleton (0,0) 1],(0,0)) ws

    insertSegment :: ([WireGrid], CoOrd) -> (Char, Int) -> ([WireGrid], CoOrd)
    insertSegment (w,loc) (d,steps) = (M.fromList [(s,1) | s <- scanl next loc (replicate steps d)] : w, foldl next loc (replicate steps d))
    
    manDis (x,y) = abs x + abs y

    findPeriods :: Stepper ->  Stepper -> [Int] -> [Int]
    findPeriods (Stepper [] _ _ _) _ pds = pds
    findPeriods _ (Stepper [] _ _ _) pds = pds

    findPeriods stepper1@(Stepper _ loc1 s1 m1) stepper2@(Stepper _ loc2 s2 m2) pds
        | M.member loc1 m2 = findPeriods (step stepper1) (step stepper2) (s1 + m2 M.! loc1 :pds)
        | M.member loc2 m1 = findPeriods (step stepper1) (step stepper2) (s2 + m1 M.! loc2 :pds)
        | otherwise        = findPeriods (step stepper1) (step stepper2) pds

    part1 :: [Wire] -> Int
    part1 input = minimum . (map manDis) . tail . M.keys . (M.filter (>1)) $ insertWires input

    part2 :: [Wire] -> Int
    part2 input = minimum $ findPeriods (newStepper w1) (newStepper w2) []
      where
        (w1:w2:_) = map processSteps input

    day03 input = do
        let proc = processInput input
        putStr "Part 1: "
        putStrLn . show $ part1 proc
        putStr "Part 2: "
        putStrLn . show $ part2 proc
        