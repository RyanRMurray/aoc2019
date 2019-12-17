module Day17 where
    import Data.List
    import Data.List.Split
    import Data.Map (Map)
    import qualified Data.Map as M

    import Control.Monad
    import Data.Maybe
    import Data.Char

    import IntCode

    type Pos = (Int,Int)
    type Grid = Map Pos Char
    type Instr = (Char, Int)

    directions = [(0,-1),(1,0),(0,1),(-1,0)]

    addPos :: Pos -> Pos -> Pos
    addPos (a,b) (c,d) = (a+c,b+d)

    turnLeft :: Pos -> Pos
    turnLeft (0,-1) = (-1,0)
    turnLeft (1,0)  = (0,-1)
    turnLeft (0,1)  = (1,0)
    turnLeft (-1,0) = (0,1)

    turnRight :: Pos -> Pos
    turnRight (0,-1) = (1,0)
    turnRight (1,0)  = (0,1)
    turnRight (0,1)  = (-1,0)
    turnRight (-1,0) = (0,-1)

    checkLeft :: Grid -> Pos -> Pos -> Char
    checkLeft g p d = M.findWithDefault 'X' (addPos p (turnLeft d)) g

    checkRight :: Grid -> Pos -> Pos -> Char
    checkRight g p d = M.findWithDefault 'X' (addPos p (turnRight d)) g

    checkAhead :: Grid -> Pos ->Pos ->  Char
    checkAhead g p d = M.findWithDefault 'X' (addPos p d) g

    processInput :: String -> [Int]
    processInput i = map read $ splitOn "," i

    getMap :: Grid -> Pos -> [Int] -> Grid
    getMap g _ [] = g

    getMap g (x,y) (10:is) = getMap g (0,y+1) is

    getMap g (x,y) (i:is) = 
        getMap 
            (M.insert (x,y) (chr i) g) 
            (x+1,y)
            is

    getIntersections :: Grid -> [Pos]
    getIntersections g =
        filter isIntersection $ M.keys $ filtered
      where
        filtered = M.filter ( == '#') g
        isScaff p = (M.findWithDefault 'F' p g) == '#'
        isIntersection p = and $ map (\d -> isScaff $ addPos p d) directions

    part1 :: Memory -> (Int, Grid)
    part1 mem = 
        (sum
        $ map (\(a,b) -> a*b)
        $ getIntersections g
        , g
        )
      where
        g = getMap M.empty (0,0) 
            $ output $ runIntMachine (IntMachine mem [] [] 0 0)


    findPath :: Grid -> Pos -> Pos -> [Instr]
    findPath g pos dir
        | nextDir == 'L' = ('L', length stepsToNext) : (findPath g (last stepsToNext) $ turnLeft dir)
        | nextDir == 'R' = ('R', length stepsToNext) : (findPath g (last stepsToNext) $ turnRight dir)
        | otherwise      = []
      where
        nextDir =
            if (checkLeft g pos dir == '#') then
                'L'
                else if (checkRight g pos dir == '#') then
                    'R'
                    else 'X'
        newD 'L' = turnLeft dir
        newD 'R' = turnRight dir
        stepsToNext = 
            takeWhile (\x -> M.findWithDefault 'X' x g == '#') 
            $ iterate (addPos (newD nextDir)) (addPos (newD nextDir) pos)
    

    findUnique :: Eq a => [a] -> [a]
    findUnique = foldl' (\l i -> if elem i l then l else i : l) []

    
    validate :: [Instr] -> [[Instr]] -> Maybe [[Instr]]
    validate path (a:b:_) 
        | valid = Just [a,b,head uniqueSubs]
        | otherwise = Nothing
      where
        uniqueSubs = 
            findUnique
            $ filter (/= [])
            $ concat $ map (splitOn b) $ splitOn a path
        valid = 
            length uniqueSubs == 1
            && length (head uniqueSubs) <= 4
    
    findFunctions :: [Instr] -> [[Instr]]
    findFunctions p =
        fromJust $ msum $ map (validate p) possibleAB
      where
        possibleAB = replicateM 2 $ replicateM 3 $ findUnique p
    
    
    makeMainFunction :: [Instr] -> [[Instr]] -> [Char]
    makeMainFunction path fs@(a:b:c:_) 
        | length path == 0   = []
        | headMatches path a = "A," ++ makeMainFunction (drop' a) fs
        | headMatches path b = "B," ++ makeMainFunction (drop' b) fs
        | otherwise          = "C," ++ makeMainFunction (drop' c) fs
      where
        headMatches l x = and $ zipWith (==) l x
        drop'         x = drop (length x) path

    encodeFunction :: [Instr] -> [Char]
    encodeFunction [] = []

    encodeFunction ((c,n):cs) = [c, ','] ++ digs ++ [',']  ++ encodeFunction cs
      where
        digs
            | n > 9 = [intToDigit 1, intToDigit (mod n 10)]
            | otherwise = [intToDigit n]


    part2 :: Memory -> Grid -> Int
    part2 mem g =
        last $ output $ runIntMachine machine
      where
        bot       = head $ M.keys $ M.filter (=='^') g
        path      = findPath g bot (0,-1)
        functions = findFunctions path
        main      = (init $ makeMainFunction path functions) ++ "\n"
        encodedFs = foldl' (\l f -> l ++ (init $ encodeFunction f) ++ "\n") [] functions
        inputs    = map ord $ main ++ encodedFs ++ "n\n"
        machine   = IntMachine (M.insert 0 2 mem) inputs [] 0 0



    drawMap :: Grid -> [String]
    drawMap g =
        [l | y <- [ly..uy], let l = drawLine g y lx ux]
      where 
        (lx,ux,ly,uy) = findBounds (M.keys g) (0,0,0,0)

    drawLine g y lx ux = [c | x <- [lx..ux], let c = g M.! (x,y)]

    findBounds :: [Pos] -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
    findBounds [] res = res

    findBounds ((x,y):next) (lx,ux,ly,uy) = 
        findBounds next (nlx,nux,nly,nuy)
        where
        nlx = min lx x
        nux = max ux x
        nly = min ly y
        nuy = max uy y

    day17 input = do
        let mem = loadMemory $ processInput input
            (p1,g) = part1 mem
        putStr "Part 1: "
        print $ p1
        putStr "Part 2: "
        print $ part2 mem g

