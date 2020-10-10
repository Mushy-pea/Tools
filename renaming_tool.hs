-- This is an automation tool I wrote to rename the functions in the Game :: Dangerous code using camel case.

module Main where

import System.IO
import System.Environment
import Data.List.Split
import Data.Char
import qualified Data.Sequence as SEQ
import Data.Foldable
import Control.Exception

data NameLookup = NameLookup {currentName :: [Char], newName :: [Char]} deriving Show

detNewName :: [Char] -> [Char] -> [Char]
detNewName ('_' : x1 : xs) acc = detNewName xs (acc ++ [toUpper x1])
detNewName (x0:x1:xs) acc = detNewName (x1:xs) (acc ++ [x0])
detNewName (x0:xs) acc = acc ++ [x0]
detNewName [] acc = acc

buildNameLookup :: [[Char]] -> [NameLookup] -> [NameLookup]
buildNameLookup ("\n" : x1 : "::" : xs) lookup = buildNameLookup xs (NameLookup {currentName = x1, newName = detNewName x1 []} : lookup)
buildNameLookup (x0:x1:x2:xs) lookup = buildNameLookup (x1:x2:xs) lookup
buildNameLookup (x0:x1:xs) lookup = lookup
buildNameLookup (x0:xs) lookup = lookup

modNameLookup :: [NameLookup] -> [NameLookup]
modNameLookup [] = []
modNameLookup (x:xs) = x : NameLookup {currentName = '(' : currentName x, newName = '(' : newName x} : modNameLookup xs

separateLineBreaks1 :: [Char] -> [Char] -> [[Char]]
separateLineBreaks1 [] acc = [acc]
separateLineBreaks1 ('\n' : xs) acc = acc : "\n" : separateLineBreaks1 xs []
separateLineBreaks1 (x:xs) acc = separateLineBreaks1 xs (acc ++ [x])

separateLineBreaks0 :: [[Char]] -> [[Char]]
separateLineBreaks0 [] = []
separateLineBreaks0 (x:xs) = separateLineBreaks1 x [] ++ separateLineBreaks0 xs

checkForNewName :: [NameLookup] -> [Char] -> [Char]
checkForNewName [] word = []
checkForNewName (x:xs) word =
  if word == currentName x then newName x
  else checkForNewName xs word

subNewNames :: [[Char]] -> SEQ.Seq [Char] -> [NameLookup] -> SEQ.Seq [Char]
subNewNames [] acc lookup = acc
subNewNames (x:xs) acc lookup =
  let substitution = checkForNewName lookup x
  in
  if substitution == [] then subNewNames xs (acc SEQ.>< SEQ.singleton x) lookup
  else subNewNames xs (acc SEQ.>< SEQ.singleton substitution) lookup

addSpaces :: SEQ.Seq [Char] -> SEQ.Seq [Char] -> SEQ.Seq [Char]
addSpaces codeIn codeOut =
  if SEQ.length codeIn == 1 then codeOut
  else if SEQ.index codeIn 1 == "\n" || SEQ.index codeIn 0 == "\n" then addSpaces (SEQ.drop 1 codeIn) (codeOut SEQ.>< SEQ.take 1 codeIn)
  else addSpaces (SEQ.drop 1 codeIn) (codeOut SEQ.>< SEQ.singleton ((SEQ.index codeIn 0) ++ " "))

main = do
  args <- getArgs
  contents <- concatModules (drop 2 args) (head args) []
  applyRenaming contents (args !! 1)

concatModules :: [[Char]] -> [Char] -> [Char] -> IO [Char]
concatModules [] path acc = return acc
concatModules (x:xs) path acc = do
  contents <- bracket (openFile (path ++ x) ReadMode) (hClose) (\h -> do contents <- hGetContents h; putStr ("\nmodule file size: " ++ show (length contents)); return contents)
  concatModules xs path (acc ++ contents)

applyRenaming :: [Char] -> [Char] -> IO ()
applyRenaming contents outputPath =
  let processedCode = separateLineBreaks0 (splitOn " " contents)
  in do
  h <- openFile outputPath WriteMode
  hPutStr h (concat (toList (addSpaces (subNewNames processedCode SEQ.Empty (modNameLookup (buildNameLookup processedCode []))) SEQ.Empty)))
  hClose h
