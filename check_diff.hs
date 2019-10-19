module Main where

import System.IO
import System.Environment
import Data.List.Split

proc_ints :: [[Char]] -> [Int]
proc_ints [] = []
proc_ints (x:xs) = (read x :: Int) : proc_ints xs

check_diff2 :: Int -> [Char]
check_diff2 0 = "signal block"
check_diff2 1 = "code block"
check_diff2 2 = "data block"

check_diff1 :: Int -> Int -> Int -> Int -> [Int] -> ([Char], [Char])
check_diff1 prog section limit c [] = ("Program: " ++ show prog, "Section: " ++ check_diff2 section)
check_diff1 prog section limit c (x:xs) =
  if c > limit then ("Program: " ++ show prog, "Section: " ++ check_diff2 section)
  else if x == 536870911 && section == 2 then check_diff1 (prog + 1) 0 limit (c + 1) xs
  else if x == 536870911 then check_diff1 prog (section + 1) limit (c + 1) xs
  else check_diff1 prog section limit (c + 1) xs

check_diff0 :: [Int] -> [Int] -> ([Int], [Int])
check_diff0 [] [] = ([], [])
check_diff0 (x:xs) (y:ys) =
  if x == y then check_diff0 xs ys
  else (x:xs, y:ys)

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode
  h1 <- openFile (args !! 1) ReadMode
  contents0 <- hGetContents h0
  contents1 <- hGetContents h1
  check_data contents0 contents1
  hClose h0
  hClose h1

check_data :: [Char] -> [Char] -> IO ()
check_data c0 c1 =
  let check_diff0_ = check_diff0 (proc_ints (splitOn ", " c0)) (proc_ints (splitOn ", " c1))
      check_diff1_ = check_diff1 1 0 (length (fst check_diff0_)) 0 (proc_ints (splitOn ", " c0))
  in do
  putStr ("\nSub list0: " ++ show (fst check_diff0_) ++ "\nSub list1: " ++ show (snd check_diff0_) ++ "\n\n" ++ fst check_diff1_ ++ "\n" ++ snd check_diff1_)
