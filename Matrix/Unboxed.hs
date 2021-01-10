{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Matrix.Unboxed where

import Matrix.Base (Matrix(..), RectIdx, MatrixRep(..), Fn(..))
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Word
import Data.Array.IArray (listArray, IArray(bounds), (!))
import Data.Array.ST (writeArray, runSTUArray)
import Data.Array.Base (unsafeThaw)
import Data.List (foldl')

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
