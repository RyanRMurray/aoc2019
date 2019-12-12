module Day12 where
    import Data.List.Split
    import Data.List
    import Data.Maybe
    import Debug.Trace

    type Pos = (Int,Int,Int)

    data Moon = Moon
        { pos :: Pos
        , vel :: Pos
        } deriving (Show, Eq)

    processInput :: String  -> [Moon]
    processInput str = 
        map (\[x,y,z] -> Moon (x,y,z) (0,0,0)) ints
      where
        ints = map ((map read) . (splitOn ",") . f) $ lines str
        f    = filter (\c -> notElem c "<>=xyz")

    addPos :: Pos -> Pos -> Pos
    addPos (a,b,c) (d,e,f) = (a+d,b+e,c+f)

    getMagnitude :: Pos -> Int
    getMagnitude (x,y,z) = sum [abs x, abs y, abs z]

    getInfluence :: Moon -> Moon -> Pos
    getInfluence (Moon (ex,ey,ez) _) (Moon (tx,ty,tz) _) = 
        (f ex tx, f ey ty, f ez tz)
      where
        f e t
            | e == t    = 0
            | e >  t    = 1
            | otherwise = -1

    stepMoon :: [Moon] -> Moon -> Moon
    stepMoon effectors target@(Moon pos vel) = Moon newPos newVel
      where
        newVel = foldl (\v m -> addPos v $ getInfluence m target) vel effectors
        newPos = addPos pos newVel
    
    stepAll :: [Moon] -> [Moon]
    stepAll moons = map f moons
      where
        f m = stepMoon (filter (/=m) moons) m

    getEnergy :: [Moon] -> Int
    getEnergy moons = sum $ map f moons
      where
        f (Moon pos vel) = getMagnitude pos * getMagnitude vel

    part1 :: [Moon] -> Int
    part1 moons = getEnergy $ iterate stepAll moons !! 1000

    
    --find period of each axis, adding 1 to account for indexing
    periodInAxis :: [Moon] -> ([Moon] -> [Int]) -> Int
    periodInAxis moons axis = (+) 1 $
        fromJust $ findIndex isRepeat $ tail $ iterate stepAll moons
      where
        initial    = axis moons
        isRepeat x = initial == axis x

    axisX :: [Moon] -> [Int]
    axisX = foldl (\l (Moon (x,_,_) (vx,_,_)) -> x : vx : l) []

    axisY :: [Moon] -> [Int]
    axisY = foldl (\l (Moon (_,x,_) (_,vx,_)) -> x : vx : l) []

    axisZ :: [Moon] -> [Int]
    axisZ = foldl (\l (Moon (_,_,x) (_,_,vx)) -> x : vx : l) []
    
    part2 :: [Moon] -> Int
    part2 moons = foldl lcm 1 $ map (periodInAxis moons) [axisX,axisY,axisZ]


    day12 input = do
        let moons = processInput input
        putStr "Part 1: "
        print $ part1 moons
        putStr "Part 2: "
        print $ part2 moons