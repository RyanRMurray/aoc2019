module IntCode where
    import qualified Data.Map as M
    import           Data.Map (Map)
    import           Data.Digits

--Intmachine functions and definitions
---------------------------------------------------------------------------------------------------
    type Memory = Map Int Int

    (@!) :: Memory -> Int -> Int
    (@!) mem i = M.findWithDefault 0 i mem

    data IntMachine = IntMachine 
      { memory :: Memory
      , input  :: [Int]
      , output :: [Int]
      , ptr    :: Int
      , rel    :: Int
      } deriving (Show)

    setPtr :: IntMachine -> Int -> IntMachine
    setPtr (IntMachine m i o _ r) np = IntMachine m i o np r

    outputMachine :: IntMachine -> Int -> IntMachine
    outputMachine (IntMachine m i o p r) no = IntMachine 
        m i 
        (o ++ [m M.! no])
        p r

    inputMachine :: IntMachine -> Int -> IntMachine
    inputMachine (IntMachine m (i:is) o p r) loc = IntMachine 
        (M.insert loc i m) 
        is o p r

    addInput :: IntMachine -> [Int] -> IntMachine
    addInput (IntMachine m i o p r) is = IntMachine
      m
      (i++is)
      o p r

    dropOut :: IntMachine -> IntMachine
    dropOut (IntMachine m i (o:os) p r) = IntMachine m i os p r

    addOut :: IntMachine -> [Int] -> IntMachine
    addOut (IntMachine m i os p r) o = IntMachine m i (o++os) p r

    setMem :: IntMachine -> Int -> Int -> IntMachine
    setMem (IntMachine m i o p r) loc nv = IntMachine 
      (M.insert loc nv m)
      i o p r

    setRel :: IntMachine -> Int -> IntMachine
    setRel (IntMachine m i o p r) nr = IntMachine m i o p (r+nr)

    loadMemoryWithNV ::  Int -> Int -> [Int] -> Memory
    loadMemoryWithNV n v  = M.insert 2 v . M.insert 1 n . loadMemory        

    loadMemory ::  [Int] -> Memory
    loadMemory = M.fromList . zip [0..]

--Core machine execution functions
---------------------------------------------------------------------------------------------------

    loadInstr :: Memory -> Int -> Int -> [Int]
    loadInstr mem ix rel =
        op : values ++ [final]
      where 
        (op:_:modes) = makeInstr . reverse . digits 10 $ mem @! ix
        operands     = loadInts mem ix $ length modes
        isStore      = elem op [1,2,3,4,7,8]
        values       = zipWith (loadOperands mem rel) modes (init operands)
        -- final operand refers to an address if the function stores any data (this includes outputting!)
        final        = if isStore then loadStoreLoc mem rel (last modes) (last operands)
                                  else loadOperands mem rel (last modes) (last operands)
    
    loadOperands :: Memory -> Int -> Int -> Int -> Int
    loadOperands mem _   0 at = mem @! at
    loadOperands mem _   1 at = at
    loadOperands mem rel 2 at = mem @! (at+rel)
    
    loadStoreLoc :: Memory  -> Int -> Int -> Int -> Int
    loadStoreLoc _   _   1 at = at
    loadStoreLoc mem rel 2 at = at + rel
    
    makeInstr :: [Int] -> [Int]
    makeInstr i@(op:is) = zipWith mask (i ++ [0,0..]) (defaults op)

    mask :: Int -> Int -> Int
    mask 0 x = x
    mask x _ = x    
    
    defaults :: Int -> [Int]
    defaults 3 = [3,0,1]
    defaults 4 = [4,0,1]
    defaults 5 = [5,0,0,0]
    defaults 6 = [6,0,0,0]
    defaults 7 = [7,0,0,0,1]
    defaults 8 = [8,0,0,0,1]
    defaults 9 = [9,0,0]
    defaults x = [x,0,0,0,1]

    loadInts :: Memory  -> Int -> Int -> [Int]
    loadInts mem ix len = [val | x <- [1..len], let val = mem @! (ix + x)]

    execInstr :: IntMachine -> [Int] -> IntMachine
    execInstr machine [1,a,b,c] = setMem machine c (b+a)
    execInstr machine [2,a,b,c] = setMem machine c (b*a)
    execInstr machine [7,a,b,c] = setMem machine c $ fromEnum $ a < b
    execInstr machine [8,a,b,c] = setMem machine c $ fromEnum $ a == b
    execInstr machine [3,c]     = inputMachine machine c
    execInstr machine [4,c]     = outputMachine machine c
    execInstr machine [9,c]     = setRel machine c
    execInstr machine [5,a,b]   = if a /= 0 then setPtr machine b else machine
    execInstr machine [6,a,b]   = if a == 0 then setPtr machine b else machine

    runIntMachine :: IntMachine -> IntMachine
    runIntMachine m1@(IntMachine mem _ _ ptr rel)
        | mem @! ptr == 99  = m1
        | np         == ptr = runIntMachine $ setPtr m2 $ ptr + length instr
        | otherwise         = runIntMachine m2
      where 
        instr = loadInstr mem ptr rel
        m2@(IntMachine _ _ _ np _) = execInstr m1 instr

--functions for running an array of intmachines that feed into eachother
---------------------------------------------------------------------------------------------------

    stepIntMachine :: IntMachine -> IntMachine
    stepIntMachine m1@(IntMachine mem _ _ ptr rel)
        | mem @! ptr == 99  = m1
        | np          == ptr = setPtr m2 $ ptr + length instr
        | otherwise          = m2
      where 
        instr = loadInstr mem ptr rel
        m2@(IntMachine _ _ _ np _) = execInstr m1 instr


    execArray :: [IntMachine] -> IntMachine -> [Int] -> Int
    execArray [] _ outs = last outs

    execArray inactive@(next:is) running@(IntMachine mem ins _ pt _) loopOuts
        | instr == 99                   = execArray is                      next                 loopOuts
        | instr == 4                    = execArray (is++[dropOut stepped]) (addInput next outs) (loopOuts ++ outs)
        | instr == 3 && length ins == 0 = execArray (is++[running])         next                 loopOuts
        | otherwise                     = execArray inactive                stepped              loopOuts
      where
        instr    = mem @! pt
        stepped@(IntMachine _ _ outs _ _) = stepIntMachine running