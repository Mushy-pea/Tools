-- This is an automation tool that substitutes occurances of Prelude.(!!) in Haskell code for a customised version that shows the location of
-- "index too large" exceptions.  Written to deal with the perennial problem of the Haskell runtime reporting these exceptions without source
-- code line numbers.  For an example of usage in a project see Mushy-pea/Game-Dangerous.

module Main where

import System.IO
import System.Environment
import Data.List.Split
import Data.Array
import qualified Data.Sequence as SEQ
import Data.Foldable

-- These two functions are placed in either the single module of the target program or in multiple module programs, one that is imported by all the targetted modules.

-- This function is a wrapper for the Prelude.(!!) function, which shows where an index too large exception has happened.
(!!) :: ([a], PREL.Int) -> PREL.Int -> a
(ls, location) !! i =
  if i PREL.>= PREL.length ls then PREL.error ("List index too large.  location: " PREL.++ PREL.show location PREL.++ " index: " PREL.++ PREL.show i PREL.++ " max: " PREL.++ PREL.show ((PREL.length ls) PREL.- 1))
  else ls PREL.!! i

-- The commenting out of these two functions can be inverted to switch off the debugging provided by the index wrapper system and save most of the runtime overhead.
--(ls, location) !! i = ls PREL.!! i

build_seq :: Array Int Char -> SEQ.Seq Char -> Int -> Int -> Bool -> SEQ.Seq Char
build_seq whole part i limit step =
  if step == True && i > limit then part
  else if step == False && i < limit then part
  else if step == True then build_seq whole (part SEQ.>< SEQ.singleton (whole ! i)) (i + 1) limit step
  else build_seq whole (part SEQ.>< SEQ.singleton (whole ! i)) (i - 1) limit step

-- Right term, with brackets
sub_index4 :: Array Int Char -> Int -> Int -> Int -> (SEQ.Seq Char, Int)
sub_index4 whole i0 i1 0 = (SEQ.singleton '(' SEQ.>< (build_seq whole SEQ.empty i0 (i1 - 1) True), i1 - i0 - 1)
sub_index4 whole i0 i1 c =
  if whole ! i1 == '(' then sub_index4 whole i0 (i1 + 1) (c + 1)
  else if whole ! i1 == ')' then sub_index4 whole i0 (i1 + 1) (c - 1)
  else sub_index4 whole i0 (i1 + 1) c

-- Left term, with brackets
sub_index3 :: Array Int Char -> Int -> Int -> Int -> (SEQ.Seq Char, Int)
sub_index3 whole i0 i1 0 = (SEQ.reverse (build_seq whole SEQ.empty i0 (i1 + 1) False) SEQ.>< SEQ.singleton ')', i0 - i1 - 1)
sub_index3 whole i0 i1 c =
  if whole ! i1 == '(' then sub_index3 whole i0 (i1 - 1) (c + 1)
  else if whole ! i1 == ')' then sub_index3 whole i0 (i1 - 1) (c - 1)
  else sub_index3 whole i0 (i1 - 1) c

-- Right term, no brackets
sub_index2 :: Array Int Char -> Int -> Int -> (SEQ.Seq Char, Int)
sub_index2 whole i0 i1 =
  if whole ! i1 == ' ' || whole ! i1 == ')' || whole ! i1 == ']' || whole ! i1 == '}' || whole ! i1 == ',' then (build_seq whole SEQ.empty i0 (i1 - 1) True, i1 - i0)
  else sub_index2 whole i0 (i1 + 1)

-- Left term, no brackets
sub_index1 :: Array Int Char -> Int -> Int -> (SEQ.Seq Char, Int)
sub_index1 whole i0 i1 =
  if whole ! i1 == ' ' || whole ! i1 == '(' || whole ! i1 == '[' then (SEQ.reverse (build_seq whole SEQ.empty i0 (i1 + 1) False), i0 - i1)
  else sub_index1 whole i0 (i1 - 1)

sub_index0 :: Array Int Char -> SEQ.Seq Char -> Int -> Int -> Int -> (SEQ.Seq Char, Int)
sub_index0 code_in code_out i limit c =
  let sub_index1_ = sub_index1 code_in (i - 2) (i - 2)
      sub_index2_ = sub_index2 code_in (i + 3) (i + 3)
      sub_index3_ = sub_index3 code_in (i - 3) (i - 3) (-1)
      sub_index4_ = sub_index4 code_in (i + 4) (i + 4) 1
      op = SEQ.fromList " !! "
      sp = SEQ.singleton ' '
      left_tuple = \x -> SEQ.fromList "(" SEQ.>< x SEQ.>< SEQ.fromList ", " SEQ.>< SEQ.fromList (show c) SEQ.>< SEQ.fromList ")"
      diff_i = (SEQ.length code_out) - i
  in
  if i > limit then (code_out, c)
  else if code_in ! i == '!' && code_in ! (i + 1) == '!' && code_in ! (i - 1) == ' ' && code_in ! (i + 2) == ' ' then
    if code_in ! (i - 2) /= ')' && code_in ! (i + 3) /= '(' then sub_index0 code_in (SEQ.take (i - (snd sub_index1_) - 1 + diff_i) code_out SEQ.>< left_tuple (fst sub_index1_) SEQ.>< op SEQ.>< fst sub_index2_) (i + (snd sub_index2_) + 3) limit (c + 1)
    else if code_in ! (i - 2) == ')' && code_in ! (i + 3) /= '(' then sub_index0 code_in (SEQ.take (i - (snd sub_index3_) - 3 + diff_i) code_out SEQ.>< left_tuple (fst sub_index3_) SEQ.>< op SEQ.>< fst sub_index2_) (i + (snd sub_index2_) + 3) limit (c + 1)
    else if code_in ! (i - 2) /= ')' && code_in ! (i + 3) == '(' then sub_index0 code_in (SEQ.take (i - (snd sub_index1_) - 1 + diff_i) code_out SEQ.>< left_tuple (fst sub_index1_) SEQ.>< op SEQ.>< fst sub_index4_) (i + (snd sub_index4_) + 5) limit (c + 1)
    else sub_index0 code_in (SEQ.take (i - (snd sub_index3_) - 3 + diff_i) code_out SEQ.>< left_tuple (fst sub_index3_) SEQ.>< op SEQ.>< fst sub_index4_) (i + (snd sub_index4_) + 5) limit (c + 1)
  else sub_index0 code_in (code_out SEQ.>< SEQ.singleton (code_in ! i)) (i + 1) limit c

main = do
  args <- getArgs
  h0 <- openFile (args !! 0) ReadMode
  contents <- hGetContents h0
  add_debug_points args contents
  hClose h0

add_debug_points :: [[Char]] -> [Char] -> IO ()
add_debug_points args contents =
  let sub_index = sub_index0 (listArray (0, ((length contents) - 1)) contents) SEQ.empty 0 ((length contents) - 1) (read (args !! 2))
  in do
  h1 <- openFile (args !! 1) WriteMode
  hPutStr h1 (toList (fst (sub_index0 (listArray (0, ((length contents) - 1)) contents) SEQ.empty 0 ((length contents) - 1) (read (args !! 2)))))
  putStr ("\nFinal c value: " ++ show (snd sub_index))
  hClose h1
