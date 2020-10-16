module Heap (Heap, empty, lookup, insert, pop, changeKey) where

import Prelude (Ord, (<=), Show)

import Bool
import Maybe

data Heap k v = Empty
              | Heap (k, v) [Heap k v]

lookup :: Heap k v -> Maybe (k, v)
lookup Empty = Nothing
lookup (Heap keyval _) = Just keyval

merge :: Ord k => Heap k v -> Heap k v -> Heap k v
merge Empty h2 = h2
merge h1 Empty = h1
merge h1@(Heap x@(key1, _) hs1) h2@(Heap y@(key2, _) hs2) =
    cond (key1 <= key2) h1' h2' where
        h1' = Heap x (h2:hs1)
        h2' = Heap y (h1:hs2)

insert :: Ord k => Heap k v -> (k, v) -> Heap k v
insert h keyval = merge (Heap keyval []) h

mergeAll :: Ord k => [Heap k v] -> Heap k v
mergeAll [] = Empty
mergeAll [h] = h
mergeAll (h1:h2:hs) = merge (merge h1 h2) (mergeAll hs)

pop :: Ord k => Heap k v -> Heap k v
pop Empty = Empty
pop (Heap _ hs) = mergeAll hs

changeKey :: Ord k => Heap k v -> k -> Heap k v
changeKey Empty _ = Empty
changeKey h@(Heap (_, val) _) key' = insert (pop h) (key', val)

empty :: Heap k v
empty = Empty
