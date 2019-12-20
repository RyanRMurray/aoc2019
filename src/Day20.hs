module Day20 where
    import Data.List
    import qualified Data.Map as M
    import Data.Map (Map)
    import qualified Data.Tree as T
    import Data.Tree (Tree)
    import qualified Data.Set as S
    import Data.Set (Set)

    import Data.Maybe
    import Data.Char
    import Data.List.Split

    import Debug.Trace


    type Pos = (Int,Int)
    type RecurPos = (Int,Pos)
    type Grid = Map Pos Char
    type Portals = Map Pos Pos
    type Visited = [Pos]
    type RecurVisited = Set RecurPos

    directions = [(0,-1),(1,0),(0,1),(-1,0)]

    addPos :: Pos -> Pos -> Pos
    addPos (a,b) (c,d) = (a+c,b+d)

    subPos :: Pos -> Pos -> Pos
    subPos (a,b) (c,d) = (a-c,b-d)

    check :: Grid -> Pos -> Char
    check g p = M.findWithDefault '#' p g

    processInput :: String -> (Grid, Portals, Pos, Pos)
    processInput ins =
        (g, portals, start, end)
      where
        processLine m (y,l) = 
            foldl' (\mx (x,c) -> M.insert (x,y) c mx) m 
            $ zip [0..] l
        g = foldl' processLine M.empty $ zip [0..] $ lines ins
        ts = triplets $ fst $ fromJust $ M.lookupMax g
        labels = mapMaybe (findPortalSpot g) ts
        portals =
            foldl' (\m [(_,a),(_,b)] -> M.insert b a $ M.insert a b m) M.empty
            $ chunksOf 2 $ sort
            $ nub $ filter (\(l,_) -> notElem l ["AA","ZZ"])
            $ labels
        start = snd $ fromJust $ find (\(l,_) -> l == "AA") labels
        end   = snd $ fromJust $ find (\(l,_) -> l == "ZZ") labels

    triplets :: Pos -> [(Pos,Pos,Pos)]
    triplets (mx,my) =
        (concat $ map horis [0..my]) ++ (concat $ map verts [0..mx])
      where
        horis y = [((l,y),(l+1,y),(l+2,y)) | l <- [0.. mx-2]]
        verts x = [((x,t),(x,t+1),(x,t+2)) | t <- [0.. my-2]]

    findPortalSpot :: Grid -> (Pos,Pos,Pos) -> Maybe (String,Pos)
    findPortalSpot g (l,m,r)
        | leftOpen  = Just ([check g m, check g r], l)
        | rightOpen = Just ([check g l, check g m], r)
        | otherwise = Nothing
      where
        leftOpen  = isUpper (check g m) && isUpper (check g r) && (check g l == '.')
        rightOpen = isUpper (check g m) && isUpper (check g l) && (check g r == '.')

    getNeighbors :: Grid -> Portals -> Visited -> Pos -> ([Pos],Visited)
    getNeighbors grid portals visited at = 
        (filter (\n -> notElem n visited) $ directNs ++ portalNs, at:visited)
      where
        directNs = filter (\n -> check grid n == '.') $ map (addPos at) directions
        portalNs = if M.member at portals then [portals M.! at] else []

    getNextLevel :: Grid -> Portals -> Visited -> [Pos] -> ([Pos], Visited)
    getNextLevel g p v level =
        foldl' (\(ox,v) px -> (\(nn,nv) -> (nn++ox,nv)) $ getNeighbors g p v px) ([],v) level
      
    part1 :: Grid -> Portals -> Pos -> Pos -> Int
    part1 g p s e =
        fromJust $ findIndex (\l -> elem e l) levels
      where
        levels = map fst $ iterate (\(l,v) -> getNextLevel g p v l) ([s],[])


    findInnerOuterPortals :: Grid -> Portals -> (Portals,Portals)
    findInnerOuterPortals g ps =
        (inners,outers)
      where
        (mx,my) = fst $ M.findMax g
        outers = M.filterWithKey (\p _ -> f p) ps
        f (x,y) = or
          [ x == 2
          , y == 2
          , x == mx - 2
          , y == my - 2
          ]
        inners = foldl' (\m k -> M.delete k m) ps $ M.keys outers

    getRecursiveNeighbors :: Grid -> Portals -> Portals -> RecurVisited -> RecurPos -> ([RecurPos], RecurVisited)
    getRecursiveNeighbors grid inners outers visited x@(level,at)
        | isInner   = (filter (\n -> S.notMember n visited) $ (level + 1, inners M.! at) : neighbors, S.insert x visited)
        | isOuter   = (filter (\n -> S.notMember n visited) $ (level - 1, outers M.! at) : neighbors, S.insert x visited)
        | otherwise = (filter (\n -> S.notMember n visited) $ neighbors, S.insert x visited)
      where
        isInner = level < 40 && M.member at inners
        isOuter = level > 0 && M.member at outers
        neighbors = filter (\(_,n) -> check grid n == '.') $ map (\d -> (level, addPos at d)) directions

    getRecursiveNextLevel :: Grid -> Portals -> Portals -> RecurVisited -> [RecurPos] -> ([RecurPos], RecurVisited)
    getRecursiveNextLevel g ip op v level =
        foldl' (\(ox,v) px -> (\(nn,nv) -> (nn++ox,nv)) $ getRecursiveNeighbors g ip op v px) ([],v) level
    
    part2 :: Grid -> Portals -> Portals -> Pos -> Pos -> Int
    part2 g ip op s e =
        fromJust $ findIndex (\l -> elem (0,e) l) $ take 10000 $ levels
      where
        levels = map fst $ iterate (\(l,v) -> getRecursiveNextLevel g ip op v l) ([(0,s)],S.empty)


    day20 input = do
        let (g,p,s,e) = processInput input
        let (ip,op)   = findInnerOuterPortals g p
        putStr "Part 1: "
        print $ part1 g p s e
        putStr "Part 2: "
        print $ part2 g ip op s e