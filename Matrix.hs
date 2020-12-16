{-# language ScopedTypeVariables #-}

module Matrix where

import Prelude ((*), (+), repeat, zip, (<$>), map, max, (.), Num, Functor(fmap), Applicative(pure, (<*>)), Show(show), Maybe(Just, Nothing))
import Data.Int (Int)
import Data.Eq (Eq((==)))
import Data.Array.IArray ((!), Array)
import qualified Data.Array.IArray as Arr
import Data.List (intercalate)
import qualified Data.List as List
import Data.Foldable (Foldable(sum))

data Matrix a = M (Int, Int) ((Int, Int) -> a)

elem :: Matrix a -> (Int, Int) -> a
elem (M _ f)  idx = f idx

elements :: Matrix a -> [(Int, Int)] -> [a]
elements m idxs = map (elem m) idxs

row :: Matrix a -> Int -> [a]
row m@(M (_, c) _) i = elements m idxs where
  idxs = zip (repeat i) [1..c]

column :: Matrix a -> Int -> [a]
column m@(M (r, _) _) j = elements m idxs where
  idxs = zip [1..r] (repeat j)

instance Show a => Show (Matrix a) where
  show m@(M (r, _) _) = intercalate "\n" lines where
    lines = map (show . row m) [1..r]

identity :: Num a => Int -> Matrix a
identity n = M (n, n) id where
  id (i, j)  = if i == j then 1 else 0

instance Functor Matrix where
  fmap f (M dim e) = M dim (f . e)

instance Applicative Matrix where
  pure x = M (0, 0) (\_ -> x)
  (M (r1, c1) f) <*> (M (r2, c2) e) = M dim g where
    g pos = (f pos) (e pos)
    dim = (max r1 r2, max c1 c2)

zipWith :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipWith f ma mb = f <$> ma <*> mb

add :: Num a => Matrix a -> Matrix a -> Matrix a
add = zipWith (+)

multiply :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
multiply m1@(M (r1, c1) _) m2@(M (r2, c2) _) =
  if c1 == r2 then Just result else Nothing where
    result = M (r1, c2) h
    h (i, j) = sum (List.zipWith (*) (row m1 i) (column m2 j))

freeze :: forall a. Matrix a -> Matrix a
freeze (M dim@(r, c) f) = M dim g where
  g pos = arr ! pos
  arr :: Array (Int, Int) a
  arr = Arr.listArray ((1, 1), dim) (map f ((,) <$> [1..r] <*> [1..c]))
