module IntCode where
    import qualified Data.Map as M
    import           Data.Map (Map)

    type Memory = Map Int Int

    loadMemoryWithNV ::  Int -> Int -> [Int] -> Memory
    loadMemoryWithNV n v  = (M.insert 2 v) . (M.insert 1 n) . loadMemory        

    loadMemory ::  [Int] -> Memory
    loadMemory = M.fromList . zip [0..]

    loadInstr :: Int -> Memory -> [Int]
    loadInstr i m = [val | x <- [0..3], let val = m M.! (i + x)]

    execIntInstr :: Int -> Int -> Int -> Int -> Memory -> Memory
    execIntInstr o a b c mem = 
        M.insert c ((f o) (mem M.! a) (mem M.! b)) mem
      where
        f 1 = (+)
        f 2 = (*)

    execIntCode :: Int -> Memory -> Memory
    execIntCode ptr mem 
        | o == 99   = mem 
        | otherwise = execIntCode (ptr+4) $ execIntInstr o a b c mem
      where
        (o:a:b:c:_) = loadInstr ptr mem