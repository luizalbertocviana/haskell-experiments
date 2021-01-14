{-# LANGUAGE TupleSections #-}

module Combinatorial where

import Data.Word (Word64)

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = (,) <$> xs <*> ys

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x,) xs ++ pairs xs
