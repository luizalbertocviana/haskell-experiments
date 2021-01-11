module BitSet where

import Data.Bits ( Bits(xor, (.|.), (.&.), shift) )
import Data.Word (Word8, Word64)
import Data.Array.Unboxed ((!), listArray, UArray)
import Data.Array.ST (runSTUArray, writeArray)
import Data.Array.Base (unsafeThaw)

isZero :: (Num a, Eq a) => a -> Bool
isZero 0 = True
isZero _ = False

ith :: Word64 -> Word8
ith i = shift 1 (fromIntegral i)

newtype BitSet = BS (UArray Word64 Word8)

new :: Word64 -> BitSet
new n = BS arr where
  arr = listArray (0, p - 1) (repeat 0)
  (q, r) = divMod n 8
  p = q + if r == 0 then 0 else 1

get :: BitSet -> Word64 -> Bool
get (BS arr) i = not $ isZero $ byte .&. ith r where
  byte = arr ! q
  (q, r) = divMod i 8

set :: BitSet -> Word64 -> Bool -> BitSet
set (BS arr) i bit = BS arr' where
  arr' = runSTUArray $ do
    marr <- unsafeThaw arr
    writeArray marr q byte'
    pure marr
  byte = arr ! q
  byte' | bit = byte .|. mask
        | otherwise = xor byte mask
  mask = ith r
  (q, r) = divMod i 8
