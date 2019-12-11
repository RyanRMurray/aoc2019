module Day11 where
    import Data.List.Split
    import Data.Map (Map)
    import qualified Data.Map as M
    import Debug.Trace

    import IntCode

    type Pos = (Int, Int)
    type Surface = Map Pos Int

    data Robot = Robot
        { pos :: Pos
        , dir :: Int
        } deriving (Show)

    dirs = [(0,1),(1,0),(0,-1),(-1,0)] -- U R D L

    move :: Robot -> Int -> Robot
    move r@(Robot (x,y) d) t = Robot (x+ox,y+oy) newD
      where
        step    = if t == 0 then -1 else 1
        newD    = if (d + step) < 0 then 3 else mod (d + step) 4
        (ox,oy) = dirs !! newD

    processInput :: String -> Memory
    processInput = loadMemory . (map read) . (splitOn ",")

    getNextInput :: Pos -> Surface -> Int
    getNextInput = M.findWithDefault 0

    paint :: Robot -> Surface -> Maybe IntMachine -> Surface
    paint _ s Nothing = s
    
    paint r@(Robot p _) surface (Just prog@(IntMachine _ _ [col,dir] _ _)) = 
        paint nr painted $ runPaintProg nextProg
      where
        painted         = M.insert p col surface
        nr@(Robot np _) = move r dir
        input           = getNextInput np painted
        nextProg        = addInput (dropOut $ dropOut prog) [input]


    runPaintProg :: IntMachine -> Maybe IntMachine
    runPaintProg prog@(IntMachine mem _ out ptr _)
        | instr == 99     = Nothing
        | length out == 2 = Just prog
        | otherwise       = runPaintProg $ stepIntMachine prog
      where
        instr = mem M.! ptr

    part1 :: Memory -> Int
    part1 mem = length . M.keys $ paint (Robot (0,0) 0) M.empty $ runPaintProg (IntMachine mem [0] [] 0 0)

    write :: Surface -> [String]
    write s = reverse [line | y <- [ly..uy], let line = writeline s y lx ux]
      where
        (lx,ux,ly,uy) = findBounds (M.keys s) (0,0,0,0)

    writeline :: Surface -> Int -> Int -> Int -> String
    writeline s y lx ux = 
        [c 
        | x <- [lx..ux]
        , let found = getNextInput (x,y) s
        , let c     = if found == 0 then ' ' else '#'
        ]

    findBounds :: [Pos] -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
    findBounds [] res = res

    findBounds ((x,y):next) (lx,ux,ly,uy) = 
        findBounds next (nlx,nux,nly,nuy)
      where
        nlx = min lx x
        nux = max ux x
        nly = min ly y
        nuy = max uy y

    part2 :: Memory -> [String]
    part2 mem =  write $ paint (Robot (0,0) 0) M.empty $ runPaintProg (IntMachine mem [1] [] 0 0)



    day11 input = do
        let prog = processInput input
        putStr "Part 1: "
        print $ part1 prog
        putStrLn "Part 2: "
        mapM_ print $ part2 prog

