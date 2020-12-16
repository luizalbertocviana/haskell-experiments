{-# language ScopedTypeVariables #-}

module Matrix where

import Prelude ((*), (+), repeat, zip, (<$>), map, max, (.), Num, Functor(fmap), Applicative(pure, (<*>)), Show(show), error)
import Data.Eq (Eq((==)))
import Data.List (intercalate)
import qualified Data.List as List
import Data.Foldable (Foldable(sum))
import Data.Array (listArray, (!), Array)
import Data.Word (Word)
import Data.Foldable (Foldable(foldl'))

type RectIdx = (Word, Word)

data Matrix a = M RectIdx (RectIdx -> a)

elem :: Matrix a -> RectIdx -> a
elem (M _ f)  idx = f idx

elements :: Matrix a -> [RectIdx] -> [a]
elements m idxs = map (elem m) idxs

row :: Matrix a -> Word -> [a]
row m@(M (_, c) _) i = elements m idxs where
  idxs = zip (repeat i) [1..c]

column :: Matrix a -> Word -> [a]
column m@(M (r, _) _) j = elements m idxs where
  idxs = zip [1..r] (repeat j)

instance Show a => Show (Matrix a) where
  show m@(M (r, _) _) = intercalate "\n" lines where
    lines = map (show . row m) [1..r]

identity :: Num a => Word -> Matrix a
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

multiply :: Num a => Matrix a -> Matrix a -> Matrix a
multiply m1@(M (r1, c1) _) m2@(M (r2, c2) _) =
  if c1 == r2 then result else exception where
    result = M (r1, c2) h
    h (i, j) = sum (List.zipWith (*) (row m1 i) (column m2 j))
    exception = error "Matrix.multiply: matrices with incompatible dimensions"

transpose :: Matrix a -> Matrix a
transpose (M (r, c) f) = M (c, r) ft where
  ft (i, j) = f (j, i)

update :: Matrix a -> RectIdx -> a -> Matrix a
update (M dim f) pos val = M dim g where
  g pos' = if pos' == pos then val else f pos'

updates :: Matrix a -> [(RectIdx, a)] -> Matrix a
updates m = foldl' f m where
  f mtx (pos, val) = update mtx pos val

freeze :: forall a. Matrix a -> Matrix a
freeze (M dim@(r, c) f) = M dim g where
  g pos = arr ! pos
  arr :: Array RectIdx a
  arr = listArray ((1, 1), dim) (map f ((,) <$> [1..r] <*> [1..c]))

fromList :: forall a. RectIdx -> [a] -> Matrix a
fromList dim xs = M dim f where
  arr :: Array RectIdx a
  arr = listArray ((1, 1), dim) xs
  f pos = arr ! pos
