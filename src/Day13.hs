module Day13 where
    import qualified Data.Map as M
    import           Data.Map (Map)
    import Data.List
    import Data.List.Split
    
    import IntCode

    type Pos = (Int,Int)
    type Grid = Map Pos Int

    processInput :: String -> Memory
    processInput = loadMemory . (map read) . (splitOn ",")


    drawTile :: Grid -> [Int] -> Grid
    drawTile g [x,y,t] = M.insert (x,y) t g

    part1 :: Memory -> Int
    part1 mem = length $ filter (==2) $ M.elems $ foldl' drawTile M.empty tiles
      where
        tiles = (chunksOf 3) . output . runIntMachine $ IntMachine mem [] [] 0 0


    breakoutBot :: IntMachine -> Int -> Pos -> Pos -> Int
    breakoutBot game@(IntMachine mem ins out ptr rel) score ppos bpos
        | instr == 99 = newScore
        | instr ==  3 = breakoutBot (stepIntMachine inputtedGame) newScore nppos nbpos
        | otherwise   = breakoutBot (stepIntMachine game)         score    ppos  bpos
      where
        instr        = mem M.! ptr
        tiles        = chunksOf 3 out 
        nppos        = updateObject tiles 3 ppos
        nbpos        = updateObject tiles 4 bpos
        newScore     = updateScore tiles score
        xDiff        = fst nbpos - fst nppos
        input        = if xDiff == 0 then 0 else (if xDiff > 0 then 1 else -1)
        inputtedGame = IntMachine mem [input] [] ptr rel

    updateScore :: [[Int]] -> Int -> Int
    updateScore ins sc
        | length objInstr > 0 = nsc
        | otherwise           = sc
      where
        objInstr = filter (\t -> init t == [-1,0]) ins
        nsc      = (last . head) objInstr

    updateObject :: [[Int]] -> Int -> Pos -> Pos
    updateObject ins obj pos
        | length objInstr > 0 = npos
        | otherwise           = pos
      where
        objInstr = filter (\t -> t !! 2 == obj) ins
        objInfo  = (init . head) objInstr
        npos      = (head objInfo, last objInfo)

    part2 :: Memory -> Int
    part2 mem = breakoutBot game 0 (0,0) (0,0)
      where
        game = IntMachine (M.insert 0 2 mem) [] [] 0 0


    day13 input = do
        let prog = processInput input
        putStr "Part 1: "
        print $ part1 prog
        putStr "Part 2: "
        print $ part2 prog

