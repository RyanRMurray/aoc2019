module Day06 where
    import Data.List.Split

    import           Data.Graph
    import qualified Data.Map as M
    import           Data.Map (Map)

    processInput :: String -> [(String,String)]
    processInput = map (f . splitOn ")") . splitOn "\n"
      where
        f [x,y] = (y,x)

    mapNodes :: [(String, String)] -> Map String [String]
    mapNodes es = foldl (\m (a,b) -> M.insertWith (++) a [b] m) M.empty es

    makeTree :: Map String [String] -> (Graph, Vertex -> (Int, String, [String]), String -> Maybe Vertex)
    makeTree es = graphFromEdges $ zipWith f [0..] $ M.toList es
      where
        f x (y,z) = (x,y,z)


    part1 :: Graph -> Int
    part1 g = sum $ map (length . reachable g) $ vertices g

    part2 :: Graph -> (String -> Maybe Vertex) -> Maybe Int
    part2 g keys = do
        youP <- reverse . tail . reachable g <$> keys "YOU"
        sanP <- reverse . tail . reachable g <$> keys "SAN"
        Just $ findDiff youP sanP

    findDiff :: [Int] -> [Int] -> Int
    findDiff (x:xs) (y:ys)
        | x == y    = findDiff xs ys
        | otherwise = 2 + length xs + length ys

    
    day06 input = do
        let (g,_,keys) = makeTree . mapNodes $ processInput input
        putStr "Part 1: " 
        print $ part1 g
        putStr "Part 2: "
        print $ part2 g keys
        