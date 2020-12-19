{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Matrix where

import Prelude (($), (*), (+), repeat, zip, (<$>), map, max, (.), Num, Functor(fmap), Applicative(pure, (<*>)), Show(show), error)
import Data.Eq (Eq((==)))
import Data.List (intercalate)
import Data.List (foldl')
import qualified Data.List as List
import Data.Foldable (Foldable(sum))
import Data.Array (listArray, (!), Array)
import Data.Word (Word)
import Data.Array.Unsafe (unsafeThaw)
import Data.Array.ST (writeArray, runSTArray)

type RectIdx = (Word, Word)

class MatrixRep r where
  elem :: r a -> RectIdx -> a

newtype Fn a = Fn (RectIdx -> a)

newtype Arr a = Arr (Array RectIdx a)

instance MatrixRep Fn where
  elem (Fn f) pos = f pos

instance MatrixRep Arr where
  elem (Arr arr) pos = arr ! pos

data Matrix r a = M RectIdx (r a)

elements :: MatrixRep r => Matrix r a -> [RectIdx] -> [a]
elements (M _ rep) idxs = map (elem rep) idxs

row :: MatrixRep r => Matrix r a -> Word -> [a]
row m@(M (_, c) _) i = elements m idxs where
  idxs = zip (repeat i) [1..c]

column :: MatrixRep r => Matrix r a -> Word -> [a]
column m@(M (r, _) _) j = elements m idxs where
  idxs = zip [1..r] (repeat j)

identity :: Num a => Word -> Matrix Fn a
identity n = M (n, n) (Fn id) where
  id (i, j)  = if i == j then 1 else 0

toFn :: Matrix Arr a -> Matrix Fn a
toFn (M dim (Arr arr)) = M dim (Fn f) where
  f pos = arr ! pos

toArr :: Matrix Fn a -> Matrix Arr a
toArr (M dim@(r, c) (Fn f)) = M dim (Arr arr) where
  arr = listArray ((1, 1), dim) (map f ((,) <$> [1..r] <*> [1..c]))

instance (Show a, MatrixRep r) => Show (Matrix r a) where
  show m@(M (r, _) _) = intercalate "\n" lines where
    lines = map (show . row m) [1..r]

instance Functor (Matrix Fn) where
  fmap f (M dim (Fn g)) = M dim (Fn (f . g))

instance Applicative (Matrix Fn) where
  pure x = M (0, 0) (Fn (\_ -> x))
  (M (r1, c1) (Fn f)) <*> (M (r2, c2) (Fn e)) = M dim (Fn g) where
    g pos = (f pos) (e pos)
    dim = (max r1 r2, max c1 c2)

zipWith :: (a -> b -> c) -> Matrix Fn a -> Matrix Fn b -> Matrix Fn c
zipWith f ma mb = f <$> ma <*> mb

add :: Num a => Matrix Fn a -> Matrix Fn a -> Matrix Fn a
add = zipWith (+)

multiply :: (Num a, MatrixRep r) => Matrix r a -> Matrix r a -> Matrix Fn a
multiply m1@(M (r1, c1) _) m2@(M (r2, c2) _) =
  if c1 == r2 then result else exception where
    result = M (r1, c2) (Fn h)
    h (i, j) = sum $ List.zipWith (*) (row m1 i) (column m2 j)
    exception = error "Matrix.multiply: matrices with incompatible dimensions"

transpose :: MatrixRep r => Matrix r a -> Matrix Fn a
transpose (M (r, c) rep) = M (c, r) (Fn f) where
  f (i, j) = elem rep (j, i)

update :: Matrix Arr a -> RectIdx -> a -> Matrix Arr a
update (M dim (Arr arr)) pos val = M dim (Arr arr') where
  arr' = runSTArray $ do
    marr <- unsafeThaw arr
    writeArray marr pos val
    pure marr

updates :: Matrix Arr a -> [(RectIdx, a)] -> Matrix Arr a
updates m = foldl' f m where
  f mtx (pos, val) = update mtx pos val

fromList :: forall a. RectIdx -> [a] -> Matrix Arr a
fromList dim xs = M dim (Arr arr) where
  arr = listArray ((1, 1), dim) xs
