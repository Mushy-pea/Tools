module Main where

import System.IO
import System.Environment
import Control.Exception
import Data.Array.IArray
import Data.List.Split

update_grid :: [Char] -> Int -> Int -> Int -> Char -> Char -> Char -> Array (Int, Int, Int) [Char] -> Array (Int, Int, Int) [Char]
update_grid [] w u v def_0 def_1 w_config w_grid = w_grid // [((w, u, v), [w_config, def_0, def_0, def_0, def_0, def_1])]
update_grid upd w u v def_0 def_1 w_config w_grid = w_grid // [((w, u, v), upd)]

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
  else if u == u_limit then save_map w 0 (v + 1) w_limit u_limit v_limit (acc ++ " " ++ w_grid ! (w, u, v) ++ "\n") w_grid
  else if u == 0 then save_map w (u + 1) v w_limit u_limit v_limit (acc ++ w_grid ! (w, u, v)) w_grid
  else save_map w (u + 1) v w_limit u_limit v_limit (acc ++ " " ++ w_grid ! (w, u, v)) w_grid

main = do
  args <- getArgs
  contents <- bracket (openFile (args !! 0) ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nmap file size: " ++ show (length contents)); return contents)
  init_edit (read (args !! 1)) (read (args !! 2)) (read (args !! 3)) (args !! 0) (splitOneOf " \n" (((splitOn "\n~\n" contents) !! 0) ++ "\n" ++ ((splitOn "\n~\n" contents) !! 1) ++ "\n" ++ ((splitOn "\n~\n" contents) !! 2)))

init_edit :: Int -> Int -> Int -> [Char] -> [[Char]] -> IO ()
init_edit w_limit u_limit v_limit map_name contents = do
  new_grid <- run_command (0, 0, 0) w_limit u_limit v_limit [] [] (load_map contents 0 0 0 u_limit v_limit (array ((0, 0, 0), (w_limit, u_limit, v_limit)) [((w, u, v), []) | w <- [0..w_limit], u <- [0..u_limit], v <- [0..v_limit]]))
  if fst new_grid == 0 then return ()
  else do
    h <- openFile map_name WriteMode
    hPutStr h (save_map 0 0 0 w_limit u_limit v_limit [] (snd new_grid))
    hClose h

run_command :: (Int, Int, Int) -> Int -> Int -> Int -> [Char] -> [Char] -> Array (Int, Int, Int) [Char] -> IO (Int, Array (Int, Int, Int) [Char])
run_command (w, u, v) w_limit u_limit v_limit def_0 def_1 w_grid = do
  putStr ("\nposition: " ++ show (w, u, v) ++ " value: " ++ w_grid ! (w, u, v) ++ " ")
  com <- getLine
  if (splitOn " " com) !! 0 == "mc" then run_command (read ((splitOn " " com) !! 1), read ((splitOn " " com) !! 2), read ((splitOn " " com) !! 3)) w_limit u_limit v_limit def_0 def_1 w_grid
  else if (splitOn " " com) !! 0 == "sd" then run_command (w, u, v) w_limit u_limit v_limit ((splitOn " " com) !! 1) ((splitOn " " com) !! 2) w_grid
  else if (splitOn " " com) !! 0 == "me" then run_command (w, u, v) w_limit u_limit v_limit def_0 def_1 (update_grid ((splitOn " " com) !! 1) w u v (head def_0) (head def_1) 'a' w_grid)
  else if (splitOn " " com) !! 0 == "save" then return (1, w_grid)
  else if (splitOn " " com) !! 0 == "exit" then return (0, w_grid)
  else if u == u_limit && v == v_limit then run_command (w, 0, 0) w_limit u_limit v_limit def_0 def_1 (update_grid [] w u v (head def_0) (head def_1) (head com) w_grid)
  else if u == u_limit then run_command (w, 0, v + 1) w_limit u_limit v_limit def_0 def_1 (update_grid [] w u v (head def_0) (head def_1) (head com) w_grid)
  else run_command (w, u + 1, v) w_limit u_limit v_limit def_0 def_1 (update_grid [] w u v (head def_0) (head def_1) (head com) w_grid)
