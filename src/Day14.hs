module Day14 where
    import Data.List.Split
    import Data.List
    import qualified Data.Map as M
    import Data.Map (Map)
    import qualified Data.Tree as T
    import Data.Tree (Tree)
    import Data.Maybe
    import Debug.Trace
    import Control.Monad.State

    type Formulas = Map String [ChemAm]

    type Amounts = Map String Int

    type Stock = Map String Int

    type ChemAm = (Int, String)

    data RecipeTree = RecipeTree 
        { label       :: ChemAm
        , children    :: [RecipeTree]
        } deriving (Show)

    (*/) :: Int -> Int -> Int
    (*/) a b  = fromJust $ findIndex (>= a) $ iterate ((+) b) 0

    (*^) :: Int -> Int -> Int
    (*^) a b = fromJust $ find (>=a) $ iterate ((+) b) b

    processInput :: String -> (Formulas, Amounts)
    processInput input = foldl' f (M.empty, M.empty) ls
      where
        ls = map processLine $ lines input
        f (fs, as) (newIngrs, (newA, newTy)) = (M.insert newTy newIngrs fs, M.insert newTy newA as)

    processLine :: String -> ([ChemAm], ChemAm)
    processLine ln = (ingrs, head res)
      where
        [ingrs,res] = map (map makeIngrs)
            $ map ((map (splitOn " ")) . (splitOn ", "))
            $ splitOn " => " ln

    makeIngrs :: [String] -> ChemAm
    makeIngrs (am:ty) = (read am, head ty)

    rev (a,b) = (b,a)
    
    getReqs :: Formulas -> Amounts -> (Stock,Stock) -> [ChemAm] -> (Stock,Stock)
    getReqs _ _ (free,used) [] = (free,used)

    getReqs fs ams (free,used) ((lots,chem):todo) =
        trace (show ingrs) getReqs fs ams (addedFree, finalUsed) todo
      where
        ingrs = filter (\(n,c) -> c /= "ORE") $ map (\(n,c) -> (n * lots, c)) $ fs M.! chem
        missing = filter (\(n,c) -> n > 0) $ map (\(n,c) -> (n - M.findWithDefault 0 c free,c)) ingrs
        depletedFree = foldl' (\m (n,c) -> M.insertWith (+) c (-n) m) free ingrs
        cLots (n,c) = (n */ (ams M.! c), c)
        (addedFree, newUsed) = getReqs fs ams (depletedFree, used) $ map cLots missing
        finalUsed = foldl' (\m (n,c) -> M.insertWith (+) c n m) newUsed ingrs
        generated = lots * (ams M.! chem)


    filterBase :: Formulas -> [ChemAm] -> [ChemAm] 
    filterBase fs t = 
      map rev
      $ M.toList
      $ foldl' (\m (n,c) -> M.insertWith (+) c n m) M.empty
      $ filter (\(_,c) -> (snd $ head $ fs M.! c) == "ORE") t

    sumOre :: Formulas -> Amounts -> [ChemAm] -> Int
    sumOre fs as basics = sum
        $ [times * ores | (am,c) <- basics, let times = am */ (as M.! c), let ores = fst $ head $ fs M.! c]

    day14 input = do
        let (fs,as) = processInput input
        putStr "Part 1: "
        --print $ part1 fs as