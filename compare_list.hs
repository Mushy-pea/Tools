module Main where

import System.IO
import System.Environment
import Data.List.Split

label_list :: [[Char]] -> Int -> [Char]
label_list [] i = []
label_list (x:xs) i = show i ++ ": " ++ x ++ " " ++ label_list xs (i + 1)

compare_list :: [[Char]] -> [[Char]] -> Int -> [Char]
compare_list [] [] i = []
compare_list (x:xs) (y:ys) i =
  if x == y then compare_list xs ys (i + 1)
  else "|" ++ show i ++ " A " ++ x ++ " B " ++ y ++ "| " ++ compare_list xs ys (i + 1)

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode
  h1 <- openFile (args !! 1) ReadMode
  h2 <- openFile (args !! 2) WriteMode
  contents0 <- hGetContents h0
  contents1 <- hGetContents h1
  hPutStr h2 ("List A -> " ++ label_list (splitOn ", " contents0) 0)
  hPutStr h2 ("\n\nList B -> " ++ label_list (splitOn ", " contents1) 0)
  hPutStr h2 ("\n\nComparison: " ++ compare_list (splitOn ", " contents0) (splitOn ", " contents1) 0)
  hClose h0
  hClose h1
  hClose h2

