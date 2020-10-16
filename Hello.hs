module Main where

import Prelude (IO, putStrLn, getLine, (>>=))

import Functions
import List

main :: IO ()
main = getLine >>= putStrLn . reverse
