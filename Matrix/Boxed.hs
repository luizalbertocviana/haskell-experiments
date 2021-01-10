{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies #-}

module Matrix.Boxed where

import Matrix.Base (Matrix(M),  RectIdx, MatrixRep(..), Fn(..))
import Data.Array (Array)
import Data.Array.IArray (listArray, IArray(bounds), (!))
import Data.Array.ST (writeArray, runSTArray)
import Data.Array.Base (unsafeThaw)
import Data.List (foldl')

newtype Arr a = Arr (Array RectIdx a)

instance MatrixRep Arr a where
  elem (Arr arr) pos = arr ! pos
  dim (Arr arr) = snd $ bounds arr
  fromFn rep@(Fn _ f) = Arr arr where
    arr = listArray ((1, 1), dim rep) (map f $ indices rep)

update :: Matrix Arr a -> RectIdx -> a -> Matrix Arr a
update (M (Arr arr)) pos val = M (Arr arr') where
  arr' = runSTArray $ do
    marr <- unsafeThaw arr
    writeArray marr pos val
    pure marr

updates :: Matrix Arr a -> [(RectIdx, a)] -> Matrix Arr a
updates = foldl' f where
  f mtx (pos, val) = update mtx pos val

fromList :: RectIdx -> [a] -> Matrix Arr a
fromList dim xs = M (Arr arr) where
  arr = listArray ((1, 1), dim) xs
