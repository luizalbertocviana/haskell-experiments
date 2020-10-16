module InfinityTree (InfinityTree(Branch), find) where

import Prelude (Int, (==), (-), divMod,
                Functor(fmap), Applicative(pure, (<*>)),
                (+), (*), (<$>))

import Bool
import Functions

data InfinityTree a = Branch a (InfinityTree a) (InfinityTree a)

instance Functor InfinityTree where
    fmap f (Branch x lt rt) = Branch (f x) (fmap f lt) (fmap f rt)

instance Applicative InfinityTree where
    pure x = Branch x (pure x) (pure x)
    
    (Branch f flt frt) <*> (Branch x xlt xrt) =
        Branch (f x) (flt <*> xlt) (frt <*> xrt)

find :: InfinityTree a -> Int -> a
find (Branch x _ _) 0 = x
find (Branch _ lt rt) n = cond (r == 1)
                               (find lt q)
                               (find rt (q - 1)) where
    (q, r) = divMod n 2

toList :: InfinityTree a -> [a]
toList (Branch x lt rt) = x : f ps qs where
    ps = toList lt
    qs = toList rt
    f (x:xs) (y:ys) = x : y : f xs ys
    
natTree :: InfinityTree Int
natTree = Branch 0 oddTree evenTree where
    oddTree = ((+1) . (*2)) <$> natTree
    evenTree = ((+2) . (*2)) <$> natTree
