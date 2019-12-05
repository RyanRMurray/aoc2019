module IntCode where
    import qualified Data.Map as M
    import           Data.Map (Map)
    import           Data.Digits

    type Memory = Map Int Int

    data IntMachine = IntMachine 
      { memory :: Memory
      , input  :: [Int]
      , output :: [Int]
      , ptr    :: Int
      }

    setPtr :: IntMachine -> Int -> IntMachine
    setPtr (IntMachine m i o _) np = IntMachine m i o np

    outputMachine :: IntMachine -> Int -> IntMachine
    outputMachine (IntMachine m i o p) no = IntMachine 
        m i 
        (o ++ [no])
        p

    inputMachine :: IntMachine -> Int -> IntMachine
    inputMachine (IntMachine m (i:is) o p) loc = IntMachine 
        (M.insert loc i m) 
        is o p

    setMem :: IntMachine -> Int -> Int -> IntMachine
    setMem (IntMachine m i o p) loc nv = IntMachine 
      (M.insert loc nv m)
      i o p

    loadMemoryWithNV ::  Int -> Int -> [Int] -> Memory
    loadMemoryWithNV n v  = (M.insert 2 v) . (M.insert 1 n) . loadMemory        

    loadMemory ::  [Int] -> Memory
    loadMemory = M.fromList . zip [0..]

    loadInstr :: Int -> Memory -> [Int]
    loadInstr ix mem =
        (op:vals)
      where
        instr = reverse . digits 10 $ mem M.! ix
        (op:_:modes) = padInstr instr
        mems  = loadInts mem ix (length modes)
        vals  = map (loadVals mem) $ zip modes mems

    padInstr :: [Int] -> [Int]
    --standard 2 op, 1 addr instructions
    padInstr (1:rest) = (1:rest) ++ replicate (3 - length rest) 0 ++ [1]
    padInstr (2:rest) = (2:rest) ++ replicate (3 - length rest) 0 ++ [1]
    padInstr (7:rest) = (7:rest) ++ replicate (3 - length rest) 0 ++ [1]
    padInstr (8:rest) = (8:rest) ++ replicate (3 - length rest) 0 ++ [1]
    --input always refers to an address
    padInstr (3:rest) = (3:rest) ++ replicate (2 - length rest) 1
    --output is 1 op stored in 1 addr
    padInstr (4:rest) = (4:rest) ++ replicate (2 - length rest) 0
    --control flow is 1 value 1 addr
    padInstr (5:rest) = (5:rest) ++ replicate (3 - length rest) 0
    padInstr (6:rest) = (6:rest) ++ replicate (3 - length rest) 0

    loadInts :: Memory  -> Int -> Int -> [Int]
    loadInts mem ix len = [val | x <- [1..len], let val = mem M.! (ix + x)]

    loadVals :: Memory -> (Int,Int) -> Int
    loadVals _ (1,v) = v
    loadVals mem (0,i) = mem M.! i
      
    execInstr :: IntMachine -> [Int] -> IntMachine
    execInstr machine [1,a,b,c] = setMem machine c (b+a)
    execInstr machine [2,a,b,c] = setMem machine c (b*a)
    execInstr machine [7,a,b,c] = setMem machine c $ fromEnum $ a < b
    execInstr machine [8,a,b,c] = setMem machine c $ fromEnum $ a == b
    execInstr machine [3,c]     = inputMachine machine c
    execInstr machine [4,c]     = outputMachine machine c
    execInstr machine [5,a,b]   = if a /= 0 then setPtr machine b else machine
    execInstr machine [6,a,b]   = if a == 0 then setPtr machine b else machine

    execIntCode :: IntMachine -> IntMachine
    execIntCode m1@(IntMachine mem _ _ ptr)
        | mem M.! ptr == 99  = m1
        | np          == ptr = execIntCode $ setPtr m2 (ptr + length instr)
        | otherwise          = execIntCode m2
      where 
        instr = loadInstr ptr mem
        m2@(IntMachine _ _ _ np) = execInstr m1 instr