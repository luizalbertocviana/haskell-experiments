module Memo where

import Prelude (Bool, Int, (+), (-), (*), (==),
                rem, div, (<$>))

import Bool
import Functions
import InfinityTree

memo :: (Int -> b) -> Int -> b
memo f = find ftree where
    natTree = Branch 0 oddTree evenTree where
        oddTree = ((+1) . (*2)) <$> natTree
        evenTree = ((+2) . (*2)) <$> natTree
    ftree = f <$> natTree

fib :: Int -> Int
fib = fix (memo . f) where
    f _ 0 = 0
    f _ 1 = 1
    f rec n = rec (n - 1) + rec (n - 2)
