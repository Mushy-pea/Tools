module Main where

import System.IO
import System.Environment
import Data.Array.IArray
import Data.List.Split
import Control.Exception

load_map :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) [Char] -> Array (Int, Int, Int) [Char]
load_map [] w u v u_limit v_limit w_grid = w_grid
load_map (x:xs) w u v u_limit v_limit w_grid =
  if u == u_limit && v == v_limit then load_map xs (w + 1) 0 0 u_limit v_limit (w_grid // [((w, u, v), x)])
  else if u == u_limit then load_map xs w 0 (v + 1) u_limit v_limit (w_grid // [((w, u, v), x)])
  else load_map xs w (u + 1) v u_limit v_limit (w_grid // [((w, u, v), x)])

save_map :: Int -> Int -> Int -> Int -> Int -> Int -> [Char] -> Array (Int, Int, Int) [Char] -> [Char]
save_map w u v w_limit u_limit v_limit acc w_grid =
  if w == w_limit && u == u_limit && v == v_limit then (acc ++ " " ++ w_grid ! (w, u, v))
  else if u == u_limit && v == v_limit then save_map (w + 1) 0 0 w_limit u_limit v_limit (acc ++ " " ++ w_grid ! (w, u, v) ++ "\n~\n") w_grid
  else if v == v_limit then save_map w (u + 1) v w_limit u_limit v_limit (acc ++ " " ++ w_grid ! (w, u, v) ++ "\n") w_grid
  else if u == 0 then save_map w (u + 1) v w_limit u_limit v_limit (acc ++ w_grid ! (w, u, v)) w_grid
  else save_map w (u + 1) v w_limit u_limit v_limit (acc ++ " " ++ w_grid ! (w, u, v)) w_grid

add_map_border :: Int -> Int -> Int -> Int -> Int -> Array (Int, Int, Int) [Char] -> Array (Int, Int, Int) [Char]
add_map_border w u v u_limit v_limit w_grid =
  if w > 2 then w_grid
  else if v > v_limit then add_map_border (w + 1) 0 0 u_limit v_limit w_grid
  else if u > u_limit then add_map_border w 0 (v + 1) u_limit v_limit w_grid
  else if v == 0 then add_map_border w (u + 1) v u_limit v_limit (w_grid // [((w, u, v), 'c' : tail (w_grid ! (w, u, v)))])
  else if v == v_limit then add_map_border w (u + 1) v u_limit v_limit (w_grid // [((w, u, v), 'b' : tail (w_grid ! (w, u, v)))])
  else if u == 0 then add_map_border w (u + 1) v u_limit v_limit (w_grid // [((w, u, v), 'i' : tail (w_grid ! (w, u, v)))])
  else if u == u_limit then add_map_border w (u + 1) v u_limit v_limit (w_grid // [((w, u, v), 'e' : tail (w_grid ! (w, u, v)))])
  else add_map_border w (u + 1) v u_limit v_limit w_grid

main = do
  args <- getArgs
  contents <- bracket (openFile (args !! 0) ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nmap file size: " ++ show (length contents)); return contents)
  proc_map contents (args !! 1) (read (args !! 2)) (read (args !! 3))
  
proc_map :: [Char] -> [Char] -> Int -> Int -> IO ()
proc_map contents path u_limit v_limit =
  let text_blocks = splitOneOf " \n" (((splitOn "\n~\n" contents) !! 0) ++ "\n" ++ ((splitOn "\n~\n" contents) !! 1) ++ "\n" ++ ((splitOn "\n~\n" contents) !! 2))
  in do
  h <- openFile path WriteMode
  hPutStr h (save_map 0 0 0 2 u_limit v_limit [] (load_map text_blocks 0 0 0 u_limit v_limit (array ((0, 0, 0), (2, u_limit, v_limit)) [((w, u, v), []) | w <- [0..2], u <- [0..u_limit], v <- [0..v_limit]])))
--  hPutStr h (save_map 0 0 0 2 u_limit v_limit [] (add_map_border 0 0 0 u_limit v_limit (load_map text_blocks 0 0 0 u_limit v_limit (array ((0, 0, 0), (2, u_limit, v_limit)) [((w, u, v), []) | w <- [0..2], u <- [0..u_limit], v <- [0..v_limit]]))))
  hClose h

