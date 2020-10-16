module Maybe where

import Prelude (Bool(True, False),
                Functor(fmap),
                Applicative(pure, (<*>)),
                Monad((>>=)), Show)

import Control.Applicative (Alternative(empty, (<|>)))

import Bool
import Functions

-- Aula 4

data Maybe a = Nothing | Just a deriving Show

maybe :: b -> (a -> b) -> Maybe a -> b
maybe y _ Nothing = y
maybe _ f (Just x) = f x

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

isJust :: Maybe a -> Bool
isJust = not . isNothing

fromMaybe :: a -> Maybe a -> a
fromMaybe y Nothing = y
fromMaybe _ (Just x) = x

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure x = Just x
    
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    (Just f) <*> (Just x) = Just (f x)

instance Alternative Maybe where
    empty = Nothing
    
    Nothing <|> y = y
    x <|> _ = x

instance Monad Maybe where
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x
