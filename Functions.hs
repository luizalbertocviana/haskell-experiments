module Functions where

import Prelude (Applicative(pure, (*>)), Monad((>>=)), seq)

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = comp f g

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

($) :: (a -> b) -> a -> b
f $ x = f x

infixl 0 $

fix :: (a -> a) -> a
fix f = y where y = f y

id :: a -> a
id x = x

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = pure []
mapM f (x:xs) = f x >>= \x' ->
                mapM f xs >>= \xs' ->
                pure  (x' : xs')

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ _ [] = pure ()
mapM_ f (x:xs) = f x *> mapM_ f xs

forever :: Applicative f => f a -> f b
forever x = x *> forever x

($!) :: (a -> b) -> a -> b
f $! x = seq x (f x)

infixl 0 $!
