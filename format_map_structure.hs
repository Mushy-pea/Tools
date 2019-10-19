module Main where

import System.IO
import System.Environment
import Data.List
import Data.List.Split

format_text :: [[Char]] -> [Char]
format_text [] = []
format_text (x0:x1:x2:x3:x4:x5:x6:xs) =
  if x5 == "n" then intercalate " " (x0:x1:x2:x3:x4:[x5]) ++ "\n~\n" ++ format_text (x6:xs)
  else intercalate " " (x0:x1:x2:x3:x4:x5:[x6] ++ take (read x6) xs) ++ "\n~\n" ++ format_text (drop (read x6) xs)

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode
  h1 <- openFile (args !! 1) WriteMode
  contents <- hGetContents h0
  hPutStr h1 (format_text (splitOn ", " contents))
  hClose h0
  hClose h1

