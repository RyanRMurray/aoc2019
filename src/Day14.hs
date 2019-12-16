module Day14 where
    import Data.List.Split
    import Data.List
    import qualified Data.Map as M
    import Data.Map (Map)
    import Data.Maybe

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


    ingredientsFor :: Formulas -> ChemAm -> [ChemAm]
    ingredientsFor fs (batches, chem) = 
      filter (\(_,c) -> c /= "ORE")
      $ map (\(n,c) -> (n * batches,c)) 
      $ fs M.! chem

    ingredientsForLot :: Formulas -> Amounts -> [ChemAm] -> [ChemAm]
    ingredientsForLot fs as chems =
        map rev $ M.toList sumIngredients
      where
        batches = map (\(n,c) -> (n */ (as M.! c) ,c)) chems
        ingredients = concat $ map (ingredientsFor fs) batches
        sumIngredients = foldl' (\m (n,c) -> M.insertWith (+) c n m) M.empty ingredients

    recipeTree :: Formulas -> Amounts -> ChemAm -> [[ChemAm]]
    recipeTree fs as chem = takeWhile (/=[]) $ iterate (ingredientsForLot fs as) [chem]

    makeIngredient :: Formulas -> Amounts -> Stock -> ChemAm -> Stock
    makeIngredient fs as st (amount, chem) 
        | first == "ORE" = foldl' (\s (n,c) -> M.insertWith (+) c n s) st [(generated,chem), (oam * batches, "ORE")]
        | otherwise      = M.insertWith (+) chem generated depleted
      where
        ingredients = fs M.! chem
        (oam,first) = head $ ingredients
        batches = amount */ (as M.! chem)
        required = map (\(n,c) -> (n * batches,c)) ingredients
        depleted = foldl' (\s (n,c) -> M.insertWith (subtract) c n s) st required
        generated = amount *^ (as M.! chem)
    
    makeLot :: Formulas -> Amounts -> Stock -> [ChemAm] -> Stock
    makeLot fs as st chems = foldl' generate st chems
      where
        missing (n,c) = (n - (M.findWithDefault 0 c st),c)
        generate s ch
          | fst (missing ch) > 0 = makeIngredient fs as s (missing ch)
          | otherwise            = s


    make :: Formulas -> Amounts -> ChemAm -> Stock
    make fs as target =
        head . snd $ span (\s -> missing s /= []) $ iterate (\st -> fillIn st $ missing st) generated
      where
        prereqs       = reverse $ recipeTree fs as target
        generated     = foldl' (makeLot fs as) M.empty prereqs
        missing s     = map (\(n,c) -> (-n,c))$ map rev $ filter (\(c,n) -> n < 0)  $ M.toList s
        fillIn s miss = foldl' (makeIngredient fs as) s miss

    rev (a,b) = (b,a)
    
    part1 :: Formulas -> Amounts -> Int
    part1 fs as = fromJust $ M.lookup "ORE" $ make fs as (1,"FUEL")

    day14 input = do
        let (fs,as) = processInput input
        putStr "Part 1: "
        print $ part1 fs as