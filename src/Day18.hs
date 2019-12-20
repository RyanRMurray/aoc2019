module Day18 where
    import Data.List.Split
    import Data.List
    import qualified Data.Map as M
    import Data.Map (Map)
    import qualified Data.Set as S
    import Data.Set (Set)
    import qualified Data.Graph as G
    import Data.Graph (Graph)
    import qualified Data.Tree as T
    import Data.Tree (Tree)
    import qualified Data.Heap as H
    import Data.Heap (MinHeap)
    import Data.Char
    import Data.Maybe
    import Control.Monad
    import Debug.Trace


    type Pos = (Int,Int)
    type Grid = Map Pos Char
    type Paths = Map (Char,Char) (Int, Set Char, Set Char)
    type StepperHeap = MinHeap Stepper

    data Maze = Maze
        { graph :: Graph
        , verts :: Int -> (Int, Pos, [Pos])
        , lup   :: Pos -> Maybe Int
        }

    data Stepper = Stepper
        { weight    :: Int
        , remaining :: [Char]
        , opened    :: Set Char
        } deriving (Show)

    instance Eq Stepper where
      (Stepper w1 _ _) == (Stepper w2 _ _) = w1 == w2

    instance Ord Stepper where
      (Stepper w1 _ _) `compare` (Stepper w2 _ _) = compare w1 w2

    directions = [(0,-1),(1,0),(0,1),(-1,0)]

    addPos :: Pos -> Pos -> Pos
    addPos (a,b) (c,d) = (a+c,b+d)

    fst' (a,_,_) = a
    snd' (_,b,_) = b
    thd  (_,_,c) = c


    processInput :: String -> (Grid, Maze)
    processInput str =
        (g, Maze m v l)
      where
        processLine m (y,l) = 
            foldl' (\mx (x,c) -> M.insert (x,y) c mx) m 
            $ zip [0..] l
        g = foldl' processLine M.empty $ zip [0..] $ lines str
        p = M.keys $ M.filter (/='#') g
        ns n = filter (\nx -> elem nx p) $ map (addPos n) directions
        makeNode (i,pos) = (i, pos, ns pos)
        (m,v,l) = G.graphFromEdges $ foldl' (\l new -> makeNode new : l) [] $ zip [0..] p

    findPath :: Maze -> Int -> Int -> [Int]
    findPath m@(Maze g vs ls) start end =
        tail $ head $ findPath' end tree
      where
        tree = head $ G.dfs g [start]
    
    findPath' end (T.Node y ns) = [[end] | end == y] ++ map (y:) (findPath' end =<< ns)

    analysePath :: Grid -> Maze -> [Int] -> (Int, Set Char, Set Char)
    analysePath g m@(Maze graph verts lup) p =
        ( length p
        , foldl' (\s k -> if (isUpper $ g M.! k) then S.insert (g M.! k) s else s) S.empty 
        $ map (snd' . verts) p
        , foldl' (\s k -> if (isLower $ g M.! k) then S.insert (g M.! k) s else s) S.empty 
        $ map (snd' . verts) $ init p
        )

    getPathsFrom :: Grid -> Maze -> Pos -> Paths
    getPathsFrom g m@(Maze graph verts lup) p =
        M.fromList $ map path others
      where
        key x = fromJust $ lup x
        startc = g M.! p
        others = filter (\(px,_) -> px /= p) $ M.toList $ M.filter isLower g
        path (px,c) = 
            ( (startc,c)
            , analysePath g m $ findPath m (key p) (key px)
            )

    getAllPaths :: Grid -> Maze -> Paths
    getAllPaths g m =
        M.unions $ map (getPathsFrom g m) froms
      where
        froms = (head $ M.keys $ M.filter (== '@') g) : (M.keys $ M.filter isLower g)

    reachableTreeGen :: Grid -> (Char,Pos,Set Pos) -> ((Char,Pos), [(Char,Pos,Set Pos)])
    reachableTreeGen g (l,p,visited)
        | elem l ".@" = ((l,p), neighbors)
        | otherwise   = ((l,p), [])
      where
        neighbors = 
          map (\(c,px) -> (c,px, S.insert p visited))
          $ filter (\(c,px) -> 
            and 
              [ not $ isUpper c
              , c /= '#'
              , not $ S.member px visited
              ]
          )
          $ map (\px -> (g M.! p,px)) 
          $ map (addPos p) directions
        
    findReachable :: Grid -> [Char]
    findReachable g =
        nub $ filter isLower $ map fst $ T.flatten tree
      where
        start = head $ M.keys $ M.filter (== '@') g
        tree  = T.unfoldTree (reachableTreeGen g) ('@',start, S.singleton start)

    findOptimum :: Paths -> StepperHeap -> Int
    findOptimum paths heap 
        | length s == 2   = w + nw
        | valid           = findOptimum paths $ H.insert n $ H.drop 1 heap
        | otherwise       = findOptimum paths $ H.drop 1 heap
      where
        t@(Stepper w s@(from:to:r) unlocked) = trace(show $ H.size heap) head $ H.take 1 heap
        (nw, doors, keys)                    = paths M.! (from,to)
        valid = and 
          [ (S.difference doors $ S.map toUpper unlocked) == S.empty
          , S.difference keys unlocked == S.empty 
          ]
        n = Stepper (w+nw) (to:r) (S.insert to unlocked)



    pathPerms :: Grid -> [[Char]]
    pathPerms g = 
        map (\ks -> '@' :ks)
        $ filter (\l -> elem (head l) reachable)
        $ permutations keys
      where
        reachable = findReachable g
        keys      = M.elems $ M.filter isLower g


    makeSteppers :: [[Char]] -> StepperHeap
    makeSteppers = foldl' (\h c -> H.insert (Stepper 0 c S.empty) h) H.empty
        

    
    
    day18 input = do
        let (g,m) = processInput input
        putStr "Part 1: "