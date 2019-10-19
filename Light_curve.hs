module Light_curve where

proc_curve :: [Float] -> [Char]
proc_curve [] = []
proc_curve (x:xs) = show x ++ ", " ++ proc_curve xs
