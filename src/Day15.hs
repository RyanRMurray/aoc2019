module Day15 where
    import Data.List
    import Data.List.Split
    import Data.Map (Map)
    import qualified Data.Map as M
    import Data.Set (Set)
    import qualified Data.Set as S
    import Debug.Trace

    import Data.Maybe
    
    import IntCode

    type Pos = (Int, Int)

    type Grid = Map Pos Int

    type Distances = Map Pos Int

    type Visited = Set Pos

    type Unvisited = Set Pos
    
    type Paths = Map Pos Pos

    processInput :: String -> [Int]
    processInput i = map read $ splitOn "," i

    dMap = M.fromList [((0,1),1), ((0,-1),2), ((-1,0),3), ((1,0),4)]

    directions = [(0,1),(1,0),(0,-1),(-1,0)]

    startUnv :: Unvisited
    startUnv = S.fromList
        [ (1,0)
        , (0,1)
        , (-1,0)
        , (0,-1)
        ]

    startPaths :: Pos -> Paths
    startPaths p = M.fromList
        [ (addPos p (1,0),p)
        , (addPos p (0,1),p)
        , (addPos p (-1,0),p)
        , (addPos p (0,-1),p)
        ]

    startDists :: Distances
    startDists = M.fromList
        [ ((1,0),1)
        , ((0,1),1)
        , ((-1,0),1)
        , ((0,-1),1)
        ]

    addPos :: Pos -> Pos -> Pos
    addPos (a,b) (c,d) = (a+c,b+d)

    directionTo :: Pos -> Pos -> Pos
    directionTo (cx,cy) target = 
        (\(a,b) -> (a-cx,b-cy))
        $ fromJust
        $ find (== target) 
        $ zipWith (\(a,b) (c,d) -> (a+c,b+d)) directions
        $ replicate 4 (cx,cy)
    
    visit :: IntMachine -> Paths -> Pos -> Pos -> Int
    visit prog paths home target = 
        head $ output $ go loadedProg
      where
        path p     = if (p == home) then [] else p : path (paths M.! p)
        directions = map (\x -> dMap M.! x) $ zipWith directionTo (home: (reverse $ path target)) (reverse $ path target)
        loadedProg = foldl' (\m i -> addInput m [i]) prog directions

    go :: IntMachine -> IntMachine
    go prog@(IntMachine mem is _ p r)
        | instr == 4 && length is == 0 = stepIntMachine $ IntMachine mem [] [] p r
        | otherwise                    = go $ stepIntMachine prog
      where
        instr = mem M.! p
    
    djikstra :: IntMachine -> Unvisited -> Visited -> Paths -> Distances -> (Paths, Distances, Pos)
    djikstra prog unv vis paths dists
        | res == 2  = (newP, dists, next)
        | res == 0  = djikstra prog (S.delete next unv) (S.insert next vis) paths dists
        | otherwise = djikstra prog (S.union newUnv $ S.delete next unv) (S.insert next vis) newP newD
      where
        next = snd $ minimum $ S.foldl' (\l n -> l ++ [(dists M.! n, n)] ) [] unv
        res  = visit prog paths (0,0) next
        neighbors = S.fromList $ zipWith addPos directions $ replicate 4 next
        newUnv    = S.difference neighbors vis
        (newD, newP) = updatePaths paths dists next newUnv

    updatePaths :: Paths -> Distances -> Pos -> Unvisited -> (Distances, Paths)
    updatePaths paths dists next newUnv = 
        S.foldl' 
            (\(d,ps) t ->
                if (((+) 1 $ dists M.! next) < M.findWithDefault 99999 t dists) 
                    then ( M.insert t ((+) 1 $ dists M.! next) d 
                         , M.insert t next ps 
                         )
                    else (d,ps)) 
            (dists, paths) newUnv

    part1 :: Memory -> (Int,Pos, Paths)
    part1 mem =
        (ds M.! target,target, ps)
      where
        machine = IntMachine mem [] [] 0 0
        (ps, ds,target) = djikstra machine startUnv (S.singleton (0,0)) (startPaths (0,0)) startDists


    mapRadius :: IntMachine -> Pos -> Unvisited -> Visited -> Paths -> Distances -> Int
    mapRadius prog home unv vis paths dists
        | length unv == 0  = maximum $ M.elems dists
        | res == 2         = mapRadius prog home (S.union newUnv $ S.delete next unv) (S.insert next vis) newP newD
        | res == 1         = mapRadius prog home (S.union newUnv $ S.delete next unv) (S.insert next vis) newP newD
        | otherwise        = mapRadius prog home (S.delete next unv) (S.insert next vis) paths dists
        where
        next = snd $ minimum $ S.foldl' (\l n -> l ++ [(dists M.! n, n)] ) [] unv
        res  = visit prog paths home next
        neighbors = S.fromList $ zipWith addPos directions $ replicate 4 next
        newUnv    = S.difference neighbors vis
        (newD, newP) = updatePaths paths dists next newUnv


    part2 :: Memory -> Pos -> Paths -> Int
    part2 mem target paths =
        (+) (-1) $ mapRadius oxMachine target neighbors (S.singleton target) (startPaths target) dists
      where
        path (0,0) = []
        path p     = p : path (paths M.! p)
        toOxy      = map (\x -> dMap M.! x) $ zipWith directionTo ((0,0): (reverse $ path target)) (reverse $ path target)
        oxMachine  = go $ foldl' (\m i -> addInput m [i]) (IntMachine mem [] [] 0 0) toOxy
        neighbors  = S.fromList $ zipWith addPos directions $ replicate 4 target
        dists      = M.fromList $ zip (S.toList neighbors) $ replicate 4 1

    day15 input = do
        let mem = loadMemory $ processInput input
        let (p1, oxygen, paths) = part1 mem
        putStr "Part 1: "
        print $ p1
        putStr "Part 2: "
        print $ part2 mem oxygen paths
