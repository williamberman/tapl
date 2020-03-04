module Common.Utils (safeMaximum) where

safeMaximum [] = 0
safeMaximum xs = maximum xs
