module Main where
    import System.Environment  
    import System.IO

    import Data.List

    import Day01
    import Day02
    import Day03
    import Day04
    import Day05
    import Day06
    import Day07
    import Day08


    solutions :: [String -> IO ()]
    solutions = [day01,day02,day03,day04,day05,day06,day07,day08]

    findSolution :: Int -> Maybe (String -> IO ())
    findSolution i
        | i-1 < length solutions = Just (solutions !! (i-1))
        | otherwise              = Nothing

    main :: IO ()
    main = do 
        (day:file:_) <- getArgs
        input <- readFile file
        case findSolution $ read day of
            Nothing -> putStrLn "No solution for that day."
            Just sol-> sol input




