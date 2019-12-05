module IntCode where
    import qualified Data.Map as M
    import           Data.Map (Map)
    import           Data.Digits
    import Debug.Trace

    type Memory = Map Int Int

    loadMemoryWithNV ::  Int -> Int -> [Int] -> Memory
    loadMemoryWithNV n v  = (M.insert 2 v) . (M.insert 1 n) . loadMemory        

    loadMemory ::  [Int] -> Memory
    loadMemory = M.fromList . zip [0..]

    loadInstr :: Int -> Memory -> [Int]
    loadInstr i m = [val | x <- [0..3], let val = m M.! (i + x)]


    loadVariedInstr :: Int -> Memory -> [Int]
    loadVariedInstr ix mem =
        (op:vals)
      where
        instr = reverse . digits 10 $ mem M.! ix
        (op:_:modes) = padInstr instr
        mems  = loadInts mem ix (length modes)
        vals  = map (loadVals mem) $ zip modes mems

    padInstr (1:rest) = (1:rest) ++ replicate (3 - length rest) 0 ++ [1]
    padInstr (2:rest) = (2:rest) ++ replicate (3 - length rest) 0 ++ [1]
    padInstr (3:rest) = (3:rest) ++ replicate (2 - length rest) 1
    padInstr (4:rest) = (4:rest) ++ replicate (2 - length rest) 0
    padInstr (5:rest) = (5:rest) ++ replicate (3 - length rest) 0
    padInstr (6:rest) = (6:rest) ++ replicate (3 - length rest) 0
    padInstr (7:rest) = (7:rest) ++ replicate (3 - length rest) 0 ++ [1]
    padInstr (8:rest) = (8:rest) ++ replicate (3 - length rest) 0 ++ [1]


    loadInts :: Memory  -> Int -> Int -> [Int]
    loadInts mem ix len = [val | x <- [1..len], let val = mem M.! (ix + x)]

    loadVals :: Memory -> (Int,Int) -> Int
    loadVals _ (1,v) = v
    loadVals mem (0,i) = mem M.! i
      
    
    execVariedInstr :: Memory -> [Int] -> [Int] -> Int -> [Int] -> (Memory, [Int], [Int], Int)
    execVariedInstr mem i      o p [1,a,b,c] = (M.insert c (b+a) mem, i, o, p)
    execVariedInstr mem i      o p [2,a,b,c] = (M.insert c (b*a) mem, i, o, p)
    execVariedInstr mem (i:ix) o p [3,c]     = (M.insert c i mem, ix, o, p)
    execVariedInstr mem i      o p [4,c]     = (mem, i, (o ++ [c]), p)
    execVariedInstr mem i      o p [5,a,b]   = (mem, i, o, if a /= 0 then b else p)
    execVariedInstr mem i      o p [6,a,b]   = (mem, i, o, if a == 0 then b else p)
    execVariedInstr mem i      o p [7,a,b,c] = (if a < b then M.insert c 1 mem else M.insert c 0 mem, i, o, p)
    execVariedInstr mem i      o p [8,a,b,c] = (if a == b then M.insert c 1 mem else M.insert c 0 mem, i, o, p)

    execVariedIntCode :: Memory -> [Int] -> [Int] -> Int -> (Memory, [Int])
    execVariedIntCode mem i o ptr
        | mem M.! ptr == 99 = (mem, o)
        | np         == ptr = execVariedIntCode nm ni no (ptr + length instr)
        | otherwise         = execVariedIntCode nm ni no np
      where 
        instr = loadVariedInstr ptr mem
        (nm, ni, no, np) = execVariedInstr mem i o ptr instr


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