module Either where

import Prelude (Bool(True, False))

import Bool
import Functions

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left x : xs) = x : lefts xs
lefts (Right _ : xs) = lefts xs

rights :: [Either a b] -> [b]
rights [] = []
rights (Left _ : xs) = rights xs
rights (Right x : xs) = x : rights xs

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x (Right _) = x

fromRight :: b -> Either a b -> b
fromRight x (Left _) = x
fromRight _ (Right x) = x
