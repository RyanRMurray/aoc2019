module Day10 where
    import Data.List.Split
    import Data.Set (Set)
    import qualified Data.Set as S
    import Data.Map (Map)
    import qualified Data.Map as M
    import Data.Ord
    import Data.List

    type Pos = (Rational,Rational)

    data Asteroid = Asteroid
        { angle    :: Float
        , distance :: Float
        , pos      :: Pos
        } deriving (Eq, Ord, Show)

    type AsteroidField = Map Float (Set Asteroid)

    processInput :: String -> Set Pos
    processInput str = S.fromList $ concat $ map parseLine $ zip [0..] $ lines str
      where
        parseLine (y,l) = [(x,y) | (x,ob) <- (zip [0..] l), ob == '#'] 

    part1 :: Set Pos -> (Int, Pos)
    part1 roids = 
        maximumBy (comparing fst) 
            [(length $ M.keys relField, p) 
            | p <- S.toList roids
            , let relField = makeField . (S.map getPolar) $ centerOn p roids
            ]
    
    centerOn :: Pos -> Set Pos -> Set Pos
    centerOn (x,y) = S.map (\(ox,oy) -> (ox-x,oy-y))

    getPolar :: Pos -> Asteroid
    getPolar (x,y) = Asteroid a2 d (x,y)
      where
        d = sqrt ((realToFrac y)^2 + (realToFrac x)^2)
        a = realToFrac $ atan2 (realToFrac x) (realToFrac (-y))
        a2 = if a < 0.0 then a + (2 * pi) else a
   
    makeField :: Set Asteroid -> AsteroidField
    makeField roids = S.fold (\as@(Asteroid an _ _) m -> M.insertWith (S.union) an (S.singleton as) m) M.empty roids
    
    explode :: Float -> AsteroidField -> (Float, AsteroidField, Pos)
    explode last field = (next, field2, pos $ exploded)
      where
        poss = filter (>last) $ M.keys field
        next = if length poss == 0 then minimum $ M.keys field else minimum poss
        exploded = S.findMin $ field M.! next
        field2 = M.adjust (S.delete exploded) next field

    explodeX :: (Float, AsteroidField, Pos) -> Int -> Pos
    explodeX (_,_,p) 0 = p

    explodeX (last, field, _) toExplode =
        explodeX (n,f2,(x,y)) (toExplode - 1)
      where
        (n,f2,(x,y)) = explode last field

    part2 :: Set Pos -> Pos
    part2 roids = explodeX (-0.000000001, field, (0,0)) 200
      where
        field = makeField $ S.map getPolar roids

    day10 input = do
        let proc = processInput input
            (p1,(x,y)) = part1 proc
            p2Set      = centerOn (x,y) proc
            (x2,y2)    = part2 p2Set
        putStr "Part 1: "
        print p1
        putStr "Part 2: "
        print $ floor $ ((x+x2) * 100) + (y2+y)