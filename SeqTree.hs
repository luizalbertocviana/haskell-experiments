module SeqTree (SeqTree, empty, insert, update, lookup, remove, map) where

import Prelude (Int, (+), (-), (^),
                Ord, (<), (<=), max, Show)

import Bool
import Maybe

data SeqTree a = Empty
               | Leaf a
               | Branch Int Int (SeqTree a) (SeqTree a) deriving Show
 
height :: SeqTree a -> Int
height Empty = 0
height (Leaf _) = 0
height (Branch h _ _ _) = h

leaves :: SeqTree a -> Int
leaves Empty = 0
leaves (Leaf _) = 1
leaves (Branch _ l _ _) = l

makeBranch :: SeqTree a -> SeqTree a -> SeqTree a
makeBranch tx ty = Branch h' l' tx ty where
    h' = max (height tx) (height ty) + 1
    l' = leaves tx + leaves ty

leftTree :: SeqTree a -> SeqTree a
leftTree Empty = Empty
leftTree (Leaf _) = Empty
leftTree (Branch _ _ ltree _) = ltree

rightTree :: SeqTree a -> SeqTree a
rightTree Empty = Empty
rightTree (Leaf _) = Empty
rightTree (Branch _ _ _ rtree) = rtree

-- Aqui o L representa "menor que" e o G "maior ou igual"
-- As duas letras representam a comparação de um elemento em relação a dois outros.
data Ordering = LL | LG | GL | GG

compare :: Ord a => a -> a -> a -> Ordering
compare x p q = cond (x < p)
                     (cond (x < q) LL LG)
                     (cond (x < q) GL GG)

insert :: SeqTree a -> a -> SeqTree a
insert Empty x = Leaf x
insert ltree@(Leaf _) x = makeBranch ltree (Leaf x)
insert branch x = decide (compare (leaves branch) halfCapacity capacity) where
    decide LL = makeBranch lbranch' rbranch
    decide LG = branch
    decide GL = makeBranch lbranch rbranch'
    decide GG = makeBranch branch (Leaf x)
    lbranch = leftTree branch
    rbranch = rightTree branch
    lbranch' = insert lbranch x
    rbranch' = insert rbranch x
    halfCapacity = 2^(height branch - 1)
    capacity = 2^(height branch)

lookup :: SeqTree a -> Int -> Maybe a
lookup Empty _ = Nothing
lookup (Leaf x) 1 = Just x
lookup (Leaf _) _ = Nothing
lookup branch n = cond  (n <= halfCapacity)
                        (lookup lbranch n)
                        (lookup rbranch n') where
    halfCapacity = 2^(height branch - 1)
    lbranch = leftTree branch
    rbranch = rightTree branch
    n' = n - halfCapacity

update :: SeqTree a -> Int -> (a -> a) -> SeqTree a
update Empty _ _ = Empty
update (Leaf x) 1 f = Leaf (f x)
update leaf@(Leaf _) _ _ = leaf
update branch n f = cond (n <= halfCapacity)
                         (makeBranch lbranch' rbranch)
                         (makeBranch lbranch rbranch') where
    halfCapacity = 2^(height branch - 1)
    lbranch = leftTree branch
    rbranch = rightTree branch
    lbranch' = update lbranch n f
    rbranch' = update rbranch n' f
    n' = n - halfCapacity

remove :: SeqTree a -> SeqTree a
remove Empty = Empty
remove (Leaf _) = Empty
remove (Branch _ _ ltree Empty) = remove ltree
remove (Branch _ _ ltree rtree) = makeBranch ltree (remove rtree)

map :: (a -> b) -> SeqTree a -> SeqTree b
map _ Empty = Empty
map f (Leaf x) = Leaf (f x)
map f (Branch _ _ ltree rtree) = makeBranch ltree' rtree' where
    ltree' = map f ltree
    rtree' = map f rtree

empty :: SeqTree a
empty = Empty
