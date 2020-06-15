module Common.Utils
  ( safeMaximum
  , trim
  ) where

import           Data.Char

-- TODO this needs to be more generic
safeMaximum :: Integer -> [Integer] -> Integer
safeMaximum i [] = i
safeMaximum i xs = maximum xs

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
  | isSpace x = dropSpaceTail (x : maybeStuff) xs
  | null maybeStuff = x : dropSpaceTail "" xs
  | otherwise = reverse maybeStuff ++ x : dropSpaceTail "" xs
