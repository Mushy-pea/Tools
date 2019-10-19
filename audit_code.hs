module Main where

import System.IO
import System.Environment
import Data.List.Split
import Control.Exception

audit_code :: [[Char]] -> Int -> Int -> (Int, Int)
audit_code [] c0 c1 = (c0, c1)
audit_code (x:xs) c0 c1 =
  if take 2 x == "//" then audit_code xs (c0 + 1) (c1 + 1)
  else audit_code xs (c0 + 1) c1

main = do
  args <- getArgs
  code <- load_code args []
  proc_code (splitOn "\n" code) (length code)

load_code :: [[Char]] -> [Char] -> IO [Char]
load_code [] code = return code
load_code (x:xs) code = do
  contents <- bracket (openFile x ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nfile size: " ++ show (length contents)); return contents)
  load_code xs (code ++ contents)

proc_code :: [[Char]] -> Int -> IO ()
proc_code code len =
  let audit_code_ = audit_code code 0 0
      comment_pc = (fromIntegral (snd audit_code_)) / (fromIntegral (fst audit_code_)) * 100
      avg_len = (fromIntegral len) / (fromIntegral (fst audit_code_))
  in do
  putStr ("\n\nNumber of lines: " ++ show (fst audit_code_) ++ "\nProportion of comments: " ++ show comment_pc ++ "%\nAverage line length: " ++ show avg_len)
