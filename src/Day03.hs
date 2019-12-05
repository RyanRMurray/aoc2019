module Day03 where
    import           Data.List.Split
    import           Data.Sort

    import qualified Data.Map as M
    import           Data.Map (Map)
    import qualified Data.Set as S
    import           Data.Set (Set)

    type CoOrd = (Int,Int)
    type Wire = [(Char, Int)]
    type WireSteps = String
    type WireGrid = Map CoOrd Int
    type WireSpace = Set CoOrd
    data Stepper = Stepper
        { directions :: WireSteps
        , location   :: CoOrd
        , distance   :: Int
        , path       :: WireGrid
        }

    newStepper :: WireSteps -> Stepper
    newStepper w = Stepper w (0,0) 0 M.empty

    --move stepper pointer based on next direction, recording last location to path
    step :: Stepper -> Stepper
    step (Stepper (d:ds) l s m) = Stepper ds (next l d) (s+1) (M.insert l s m)

    next :: CoOrd -> Char -> CoOrd
    next (x,y)  'U' = (x,y+1)
    next (x,y)  'D' = (x,y-1)
    next (x,y)  'R' = (x+1,y)
    next (x,y)  'L' = (x-1,y)
    
    manDis :: CoOrd -> Int
    manDis (x,y) = abs x + abs y

    processInput :: String -> [Wire]
    processInput = 
        map (map f . splitOn ",") . splitOn "\n"
      where
        f (c:i) = (c,read i)

    processSteps :: Wire -> WireSteps
    processSteps w = concat [replicate s d | (d,s) <- w]

-- part 1 : find distance between origin and nearest intersection of two wires
---------------------------------------------------------------------------------------------------

    spaceOfWire :: Wire -> WireSpace
    spaceOfWire ws = fst $ foldl insertSegment (S.singleton (0,0), (0,0)) ws

    --a segment is a straight piece of wire, its direction and length given by a char and int pair
    insertSegment :: (WireSpace, CoOrd) -> (Char, Int) -> (WireSpace, CoOrd)
    insertSegment (w,loc) (d,steps) = 
        (S.union w $ S.fromList visited , last visited)
      where
        visited = scanl next loc (replicate steps d)

    part1 :: [Wire] -> Int
    part1 (w1:w2:_) = S.findMin . S.deleteMin . (S.map manDis) $ S.intersection (spaceOfWire w1) (spaceOfWire w2)

-- part 2 : find shortest collective distance between two intersections
---------------------------------------------------------------------------------------------------

    --step each stepper until one runs out of directions, record instances of one crossing another's path
    findPeriods :: Stepper ->  Stepper -> [Int] -> [Int]
    findPeriods (Stepper [] _ _ _) _ pds = pds
    findPeriods _ (Stepper [] _ _ _) pds = pds

    findPeriods stepper1@(Stepper _ loc1 s1 m1) stepper2@(Stepper _ loc2 s2 m2) pds
        | M.member loc1 m2 = f (s1 + m2 M.! loc1 :pds)
        | M.member loc2 m1 = f (s2 + m1 M.! loc2 :pds)
        | otherwise        = f pds
      where
        f = findPeriods (step stepper1) (step stepper2)

    part2 :: [Wire] -> Int
    part2 input = minimum $ findPeriods (newStepper w1) (newStepper w2) []
      where
        (w1:w2:_) = map processSteps input

---------------------------------------------------------------------------------------------------

    day03 input = do
        let proc = processInput input
        putStr "Part 1: "
        print $ part1 proc
        putStr "Part 2: "
        print $ part2 proc