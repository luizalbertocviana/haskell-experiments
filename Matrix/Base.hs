{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Matrix.Base where

import Prelude (Show(show), Functor(fmap), Applicative(pure, (<*>)), Num, (<$>), id, snd, zip, repeat, fst, (.), const, max, (+), ($), (*), error)
import Data.Eq (Eq((==)))
import qualified Data.List as List
import Data.Foldable (Foldable(sum))
import Data.Word (Word)
import Combinatorial (cartesian)

type RectIdx = (Word, Word)

class MatrixRep r a where
  elem :: r a -> RectIdx -> a
  dim :: r a -> RectIdx
  toFn :: r a -> Fn a
  toFn rep = Fn (dim rep) (elem rep)
  fromFn :: Fn a -> r a
  indices :: r a -> [RectIdx]
  indices rep = cartesian [1..r] [1..c] where
    (r, c) = dim rep

data Fn a = Fn RectIdx (RectIdx -> a)

instance MatrixRep Fn a where
  elem (Fn _ f) pos = f pos
  dim (Fn dim _) = dim
  toFn = id
  fromFn = id

newtype Matrix r a = M (r a)

elements :: MatrixRep r a => Matrix r a -> [RectIdx] -> [a]
elements (M rep) = fmap (elem rep)

row :: MatrixRep r a => Matrix r a -> Word -> [a]
row m@(M rep) i = elements m idxs where
  c = snd (dim rep)
  idxs = zip (repeat i) [1..c]

column :: MatrixRep r a => Matrix r a -> Word -> [a]
column m@(M rep) j = elements m idxs where
  r = fst (dim rep)
  idxs = zip [1..r] (repeat j)

identity :: Num a => Word -> Matrix Fn a
identity n = M (Fn (n, n) id) where
  id (i, j)  = if i == j then 1 else 0

instance (Show a, MatrixRep r a) => Show (Matrix r a) where
  show m@(M rep) = List.intercalate "\n" lines where
    r = fst (dim rep)
    lines = fmap (show . row m) [1..r]

instance Functor (Matrix Fn) where
  fmap f (M (Fn dim g)) = M (Fn dim (f . g))

instance Applicative (Matrix Fn) where
  pure x = M (Fn (0, 0) (const x))
  (M (Fn (r1, c1) f)) <*> (M (Fn (r2, c2) e)) = M (Fn dim g) where
    g pos = f pos (e pos)
    dim = (max r1 r2, max c1 c2)

zipWith :: (a -> b -> c) -> Matrix Fn a -> Matrix Fn b -> Matrix Fn c
zipWith f ma mb = f <$> ma <*> mb

add :: Num a => Matrix Fn a -> Matrix Fn a -> Matrix Fn a
add = zipWith (+)

multiply :: (Num a, MatrixRep r a) => Matrix r a -> Matrix r a -> Matrix Fn a
multiply m1@(M rep1) m2@(M rep2) =
  if c1 == r2 then result else exception where
    (r1, c1) = dim rep1
    (r2, c2) = dim rep2
    result = M (Fn (r1, c2) h)
    h (i, j) = sum $ List.zipWith (*) (row m1 i) (column m2 j)
    exception = error "Matrix.multiply: matrices with incompatible dimensions"

transpose :: MatrixRep r a => Matrix r a -> Matrix Fn a
transpose (M rep) = M  (Fn (c, r) f) where
  (r, c) = dim rep
  f (i, j) = elem rep (j, i)
