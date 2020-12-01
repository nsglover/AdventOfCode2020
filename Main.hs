module Main where

import Language.Haskell.Interpreter
import Text.Printf
import Control.Exception
import System.CPUTime

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation Time: %0.3f sec\n" (diff :: Double)
    return v

-- | solve b d => solves both parts of the day d puzzle (with computation time, if b is true) 
solve :: Int -> Bool -> IO ()
solve day timed = do
    let expr = "let p i ls = if i == 1 then part1 Day ls else part2 Day ls in p"
    r <- runInterpreter $ do
            loadModules ["Puzzles/Day" ++ (show day) ++ ".hs", "Common/Solution.hs"]
            setTopLevelModules ["Puzzles.Day" ++ (show day), "Common.Solution"]
            setImports ["Prelude"]
            interpret expr (as :: Int -> [String] -> String)
    case r of
        Left err -> putStrLn $ "Error: " ++ (show err)
        Right p  -> do
          file <- readFile $ "input/day" ++ (show day) ++ ".txt"
          let ls = lines file
          putStrLn $ "\n--- Day " ++ (show day) ++ " Puzzle Solutions ---\n"
          putStrLn $ "Part 1: " ++ p 1 ls
          if timed then 
            time $ p 1 ls `seq` return ()
          else return ()
          putStrLn $ "Part 2: " ++ p 2 ls
          if timed then 
            time $ p 2 ls `seq` return ()
          else return ()
          putStrLn $ ""

-- | Solves all puzzles between the lower and upper bounds and prints the output.
solveAll :: Bool -> Int -> Int -> IO ()
solveAll timed lo hi
  | lo <= hi = do solve lo timed; solveAll timed (lo + 1) hi
  | lo > hi  = return ()

-- | Solves all the puzzles and prints the output. This might take a little while.
main :: IO ()
main = solveAll True 1 25