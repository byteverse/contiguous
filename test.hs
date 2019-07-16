module Main where

import Data.Primitive.Contiguous

main :: IO ()
main = print 1

list :: [Char]
list = [1..6] >>= \x -> "Foo" ++ show x ++ " "

arrList :: [Char]
arrList = toList
  $ (fromList [1..6] :: Array Int) `bind` (\x -> fromList ("Foo " ++ show x ++ " ") :: Array Char)

