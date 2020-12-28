{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Matrix where

import Prelude (const, Bool, Char, Double, Float, id, ($), (*), (+), repeat, zip, (<$>), map, max, (.), Num, Functor(fmap), Applicative(pure, (<*>)), Show(show), error, snd, fst)
import Data.Eq (Eq((==)))
import Data.List (intercalate, foldl')
import qualified Data.List as List
import Data.Foldable (Foldable(sum))
import Data.Array.IArray (listArray, (!), Array, bounds)
import Data.Int
import Data.Word
import Data.Array.Unsafe (unsafeThaw)
import Data.Array.ST (writeArray, runSTArray)
import Data.Array.Unboxed (UArray)

type RectIdx = (Word, Word)

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = (,) <$> xs <*> ys

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

newtype Arr a = Arr (Array RectIdx a)

instance MatrixRep Arr a where
  elem (Arr arr) pos = arr ! pos
  dim (Arr arr) = snd $ bounds arr
  fromFn rep@(Fn _ f) = Arr arr where
    arr = listArray ((1, 1), dim rep) (map f $ indices rep)

newtype UArr a = UArr (UArrayOf a)

instance UArrayElement a => MatrixRep UArr a where
  elem (UArr uarr) pos = elementAt uarr pos
  dim (UArr uarr) = dimOf uarr
  fromFn rep@(Fn _ f) = UArr (uarrayFromList (dim rep) xs) where
    xs = map f $ indices rep

class UArrayElement a where
  data UArrayOf a :: *
  elementAt :: UArrayOf a -> RectIdx -> a
  dimOf :: UArrayOf a -> RectIdx
  uarrayFromList :: RectIdx -> [a] -> UArrayOf a

instance UArrayElement Bool where
  newtype UArrayOf Bool = UArrayBool (UArray RectIdx Bool)
  elementAt (UArrayBool arr) pos = arr ! pos
  dimOf (UArrayBool arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayBool $ listArray ((1, 1), dim) xs

instance UArrayElement Char where
  newtype UArrayOf Char = UArrayChar (UArray RectIdx Char)
  elementAt (UArrayChar arr) pos = arr ! pos
  dimOf (UArrayChar arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayChar $ listArray ((1, 1), dim) xs

instance UArrayElement Double where
  newtype UArrayOf Double = UArrayDouble (UArray RectIdx Double)
  elementAt (UArrayDouble arr) pos = arr ! pos
  dimOf (UArrayDouble arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayDouble $ listArray ((1, 1), dim) xs

instance UArrayElement Float where
  newtype UArrayOf Float = UArrayFloat (UArray RectIdx Float)
  elementAt (UArrayFloat arr) pos = arr ! pos
  dimOf (UArrayFloat arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayFloat $ listArray ((1, 1), dim) xs

instance UArrayElement Int where
  newtype UArrayOf Int = UArrayInt (UArray RectIdx Int)
  elementAt (UArrayInt arr) pos = arr ! pos
  dimOf (UArrayInt arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayInt $ listArray ((1, 1), dim) xs

instance UArrayElement Int8 where
  newtype UArrayOf Int8 = UArrayInt8 (UArray RectIdx Int8)
  elementAt (UArrayInt8 arr) pos = arr ! pos
  dimOf (UArrayInt8 arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayInt8 $ listArray ((1, 1), dim) xs

instance UArrayElement Int16 where
  newtype UArrayOf Int16 = UArrayInt16 (UArray RectIdx Int16)
  elementAt (UArrayInt16 arr) pos = arr ! pos
  dimOf (UArrayInt16 arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayInt16 $ listArray ((1, 1), dim) xs

instance UArrayElement Int32 where
  newtype UArrayOf Int32 = UArrayInt32 (UArray RectIdx Int32)
  elementAt (UArrayInt32 arr) pos = arr ! pos
  dimOf (UArrayInt32 arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayInt32 $ listArray ((1, 1), dim) xs

instance UArrayElement Int64 where
  newtype UArrayOf Int64 = UArrayInt64 (UArray RectIdx Int64)
  elementAt (UArrayInt64 arr) pos = arr ! pos
  dimOf (UArrayInt64 arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayInt64 $ listArray ((1, 1), dim) xs

instance UArrayElement Word where
  newtype UArrayOf Word = UArrayWord (UArray RectIdx Word)
  elementAt (UArrayWord arr) pos = arr ! pos
  dimOf (UArrayWord arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayWord $ listArray ((1, 1), dim) xs

instance UArrayElement Word8 where
  newtype UArrayOf Word8 = UArrayWord8 (UArray RectIdx Word8)
  elementAt (UArrayWord8 arr) pos = arr ! pos
  dimOf (UArrayWord8 arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayWord8 $ listArray ((1, 1), dim) xs

instance UArrayElement Word16 where
  newtype UArrayOf Word16 = UArrayWord16 (UArray RectIdx Word16)
  elementAt (UArrayWord16 arr) pos = arr ! pos
  dimOf (UArrayWord16 arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayWord16 $ listArray ((1, 1), dim) xs

instance UArrayElement Word32 where
  newtype UArrayOf Word32 = UArrayWord32 (UArray RectIdx Word32)
  elementAt (UArrayWord32 arr) pos = arr ! pos
  dimOf (UArrayWord32 arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayWord32 $ listArray ((1, 1), dim) xs

instance UArrayElement Word64 where
  newtype UArrayOf Word64 = UArrayWord64 (UArray RectIdx Word64)
  elementAt (UArrayWord64 arr) pos = arr ! pos
  dimOf (UArrayWord64 arr) = snd (bounds arr)
  uarrayFromList dim xs = UArrayWord64 $ listArray ((1, 1), dim) xs

newtype Matrix r a = M (r a)

elements :: MatrixRep r a => Matrix r a -> [RectIdx] -> [a]
elements (M rep) = map (elem rep)

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
  show m@(M rep) = intercalate "\n" lines where
    r = fst (dim rep)
    lines = map (show . row m) [1..r]

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

update :: Matrix Arr a -> RectIdx -> a -> Matrix Arr a
update (M (Arr arr)) pos val = M (Arr arr') where
  arr' = runSTArray $ do
    marr <- unsafeThaw arr
    writeArray marr pos val
    pure marr

updates :: Matrix Arr a -> [(RectIdx, a)] -> Matrix Arr a
updates = foldl' f where
  f mtx (pos, val) = update mtx pos val

fromList :: forall a. RectIdx -> [a] -> Matrix Arr a
fromList dim xs = M (Arr arr) where
  arr = listArray ((1, 1), dim) xs
