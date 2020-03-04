module Common.Utils (safeMaximum) where

-- TODO this needs to be more generic
safeMaximum :: Integer -> [Integer] -> Integer
safeMaximum i [] = i
safeMaximum i xs = maximum xs
