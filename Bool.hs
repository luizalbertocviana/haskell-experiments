module Bool where

import Prelude(Bool(True, False), Applicative(pure))

import Control.Applicative (Alternative(empty))

import Functions

not :: Bool -> Bool
not True = False
not False = True

and :: [Bool] -> Bool
and [] = True
and (True:xs) = and xs
and (False:_) = False

or :: [Bool] -> Bool
or [] = False
or (True:_) = True
or (False:xs) = or xs

cond :: Bool -> a -> a -> a
cond True x _ = x
cond False _ y = y

when :: Applicative f => Bool -> f () -> f ()
when True x = x
when False _ = pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless = when . not

guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty
