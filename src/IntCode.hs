module IntCode where
    import qualified Data.Map as M
    import           Data.Map (Map)
    import           Data.Digits
    import Debug.Trace

    type Memory = Map Int Int

    data IntMachine = IntMachine 
      { memory :: Memory
      , input  :: [Int]
      , output :: [Int]
      , ptr    :: Int
      } deriving (Show)

    setPtr :: IntMachine -> Int -> IntMachine
    setPtr (IntMachine m i o _) = IntMachine m i o

    outputMachine :: IntMachine -> Int -> IntMachine
    outputMachine (IntMachine m i o p) no = IntMachine 
        m i 
        (o ++ [no])
        p

    inputMachine :: IntMachine -> Int -> IntMachine
    inputMachine (IntMachine m (i:is) o p) loc = IntMachine 
        (M.insert loc i m) 
        is o p

    addInput :: IntMachine -> [Int] -> IntMachine
    addInput (IntMachine m i o p) is = IntMachine
      m
      (i++is)
      o p

    dropOut :: IntMachine -> IntMachine
    dropOut (IntMachine m i (o:os) p) = IntMachine m i os p

    setMem :: IntMachine -> Int -> Int -> IntMachine
    setMem (IntMachine m i o p) loc nv = IntMachine 
      (M.insert loc nv m)
      i o p

    loadMemoryWithNV ::  Int -> Int -> [Int] -> Memory
    loadMemoryWithNV n v  = M.insert 2 v . M.insert 1 n . loadMemory        

    loadMemory ::  [Int] -> Memory
    loadMemory = M.fromList . zip [0..]

    loadInstr :: Int -> Memory -> [Int]
    loadInstr ix mem =
        op : vals
      where
        instr        = reverse . digits 10 $ mem M.! ix
        (op:_:modes) = padInstr instr
        mems         = loadInts mem ix (length modes)
        vals         = map (loadVals mem) $ zip modes mems

    padInstr :: [Int] -> [Int]
    padInstr i@(op:rest) = zipWith xor (i ++ [0,0..]) (defaultOf op) 

    defaultOf :: Int -> [Int]
    defaultOf 3 = [0,0,1]
    defaultOf 4 = [0,0,0]
    defaultOf 5 = [0,0,0,0]
    defaultOf 6 = [0,0,0,0]
    defaultOf _ = [0,0,0,0,1]

    xor :: Int -> Int -> Int
    xor 0 0 = 0
    xor 0 1 = 1
    xor a _ = a

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

    runIntMachine :: IntMachine -> IntMachine
    runIntMachine m1@(IntMachine mem _ _ ptr)
        | mem M.! ptr == 99  = m1
        | np          == ptr = runIntMachine $ setPtr m2 (ptr + length instr)
        | otherwise          = runIntMachine m2
      where 
        instr = loadInstr ptr mem
        m2@(IntMachine _ _ _ np) = execInstr m1 instr


    stepIntMachine :: IntMachine -> IntMachine
    stepIntMachine m1@(IntMachine mem _ _ ptr)
        | mem M.! ptr == 99  = m1
        | np          == ptr = setPtr m2 (ptr + length instr)
        | otherwise          = m2
      where 
        instr = loadInstr ptr mem
        m2@(IntMachine _ _ _ np) = execInstr m1 instr


    feedBackLoop :: [IntMachine] -> IntMachine -> IntMachine
    feedBackLoop [] m = runIntMachine m

    feedBackLoop (r:rs) running@(IntMachine mem ins outs pt)
        | next == 99                   = feedBackLoop rs r
        | next == 4                    = feedBackLoop (rs++[stepped]) (addInput r $ output stepped)
        | next == 3 && length ins == 0 = feedBackLoop (rs++[running]) r
        | otherwise                    = feedBackLoop (r:rs) stepped
      where
        next = mem M.! pt
        stepped = stepIntMachine running