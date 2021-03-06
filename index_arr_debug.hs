module Main where

import System.IO
import System.Environment
import Data.List.Split
import Data.Array.IArray
import qualified Data.Sequence as SEQ
import Data.Foldable

fst__ (a, b, c) = a
snd__ (a, b, c) = b
third_ (a, b, c) = c

class Sub_index i where
  sub_j :: IArray a e => Int -> a i e -> i -> e

instance Sub_index Int where
  sub_j location arr i =
    let bd = bounds arr
    in
    if i < fst bd || i > snd bd then error ("Error in array index.  location: " ++ show location ++ " index: " ++ show i ++ " bounds: " ++ show bd)
    else arr ! i

instance Sub_index (Int, Int) where
  sub_j location arr (i0, i1) =
    let bd = bounds arr
    in
    if i0 < fst (fst bd) || i0 > fst (snd bd) || i1 < snd (fst bd) || i1 > snd (snd bd) then error ("Error in array index.  location: " ++ show location ++ " index: " ++ show (i0, i1) ++ " bounds: " ++ show bd)
    else arr ! (i0, i1)

instance Sub_index (Int, Int, Int) where
  sub_j location arr (i0, i1, i2) =
    let bd = bounds arr
    in
    if i0 < fst__ (fst bd) || i0 > fst__ (snd bd) || i1 < snd__ (fst bd) || i1 > snd__ (snd bd) || i2 < third_ (fst bd) || i2 > third_ (snd bd) then error ("Error in array index.  location: " ++ show location ++ " index: " ++ show (i0, i1, i2) ++ " bounds: " ++ show bd)
    else arr ! (i0, i1, i2)

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
  if whole ! i1 == ' ' || whole ! i1 == ')' || whole ! i1 == ']' || whole ! i1 == '}' || whole ! i1 == ',' || whole ! i1 == '\n' then (build_seq whole SEQ.empty i0 (i1 - 1) True, i1 - i0)
  else sub_index2 whole i0 (i1 + 1)

-- Left term, no brackets
sub_index1 :: Array Int Char -> Int -> Int -> (SEQ.Seq Char, Int)
sub_index1 whole i0 i1 =
  if whole ! i1 == ' ' || whole ! i1 == '(' || whole ! i1 == '[' then (SEQ.reverse (build_seq whole SEQ.empty i0 (i1 + 1) False), i0 - i1)
  else sub_index1 whole i0 (i1 - 1)

sub_index0 :: Array Int Char -> SEQ.Seq Char -> Int -> Int -> Int -> (SEQ.Seq Char, Int)
sub_index0 code_in code_out i limit c =
  let sub_index1_ = sub_index1 code_in (i - 2) (i - 2)
      sub_index2_ = sub_index2 code_in (i + 2) (i + 2)
      sub_index3_ = sub_index3 code_in (i - 3) (i - 3) (-1)
      sub_index4_ = sub_index4 code_in (i + 3) (i + 3) 1
      os = SEQ.fromList "(sub_j "
      sp = SEQ.singleton ' '
      cs = SEQ.singleton ')'
      diff_i = (SEQ.length code_out) - i
  in
  if i > limit then (code_out, c)
  else if code_in ! i == '!' && code_in ! (i - 1) == ' ' && code_in ! (i + 1) == ' ' then
    if code_in ! (i - 2) /= ')' && code_in ! (i + 2) /= '(' then sub_index0 code_in (SEQ.take (i - (snd sub_index1_) - 1 + diff_i) code_out SEQ.>< os SEQ.>< SEQ.fromList (show c) SEQ.>< sp SEQ.>< fst sub_index1_ SEQ.>< sp SEQ.>< fst sub_index2_ SEQ.>< cs) (i + (snd sub_index2_) + 2) limit (c + 1)
    else if code_in ! (i - 2) == ')' && code_in ! (i + 2) /= '(' then sub_index0 code_in (SEQ.take (i - (snd sub_index3_) - 3 + diff_i) code_out SEQ.>< os SEQ.>< SEQ.fromList (show c) SEQ.>< sp SEQ.>< fst sub_index3_ SEQ.>< sp SEQ.>< fst sub_index2_ SEQ.>< cs) (i + (snd sub_index2_) + 2) limit (c + 1)
    else if code_in ! (i - 2) /= ')' && code_in ! (i + 2) == '(' then sub_index0 code_in (SEQ.take (i - (snd sub_index1_) - 1 + diff_i) code_out SEQ.>< os SEQ.>< SEQ.fromList (show c) SEQ.>< sp SEQ.>< fst sub_index1_ SEQ.>< sp SEQ.>< fst sub_index4_ SEQ.>< cs) (i + (snd sub_index4_) + 4) limit (c + 1)
    else sub_index0 code_in (SEQ.take (i - (snd sub_index3_) - 3 + diff_i) code_out SEQ.>< os SEQ.>< SEQ.fromList (show c) SEQ.>< sp SEQ.>< fst sub_index3_ SEQ.>< sp SEQ.>< fst sub_index4_ SEQ.>< cs) (i + (snd sub_index4_) + 4) limit (c + 1)
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
