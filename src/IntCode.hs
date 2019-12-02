module IntCode where
    import qualified Data.Map as M
    import           Data.Map (Map)

    type Memory = Map Int Int


    loadMemoryWithNV ::  Int -> Int -> [Int] -> Memory
    loadMemoryWithNV n v mem = M.insert 2 v $ M.insert 1 n $ 
        foldl (\l (i,m) -> M.insert i m l) (M.empty) (zip [0..] mem)

    loadMemory ::  [Int] -> Memory
    loadMemory mem = 
        foldl (\l (i,m) -> M.insert i m l) (M.empty) (zip [0..] mem)

    loadOps :: Int -> Memory -> [Int]
    loadOps i m = foldl (\l ix -> (m M.! ix) : l) [] [ix | x <- [3,2,1,0], let ix = i + x]

    execIntInstr :: Int -> Int -> Int -> Int -> Memory -> Memory
    execIntInstr o a b c mem = 
        M.insert c ((f o) (mem M.! a) (mem M.! b)) mem
      where
        f 1         = (+)
        f 2         = (*)

    execIntCode :: Int -> Memory -> Memory
    execIntCode ptr mem 
        | o == 99   = mem 
        | otherwise = execIntCode (ptr+4) $ execIntInstr o a b c mem
      where
        (o:a:b:c:_) = loadOps ptr mem