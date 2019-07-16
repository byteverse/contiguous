module Main (main) where

import qualified Data.Primitive.Contiguous as C
import Data.Primitive.Contiguous
import Data.Proxy

import Test.QuickCheck
import Test.QuickCheck.Classes

import qualified GHC.Exts as Exts

main :: IO ()
main = lawsCheckMany laws

laws :: [(String, [Laws])]
laws =
  [ ("Arr", [functorLaws arr, applicativeLaws arr,monadLaws arr])
  ]

newtype Arr a = Arr { getArr :: Array a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Arr a) where
  arbitrary = fmap (Arr . Exts.fromList) arbitrary

arr :: Proxy Arr
arr = Proxy

instance Functor Arr where
  fmap f (Arr a) = Arr (C.map f a)
  a <$ (Arr bs) = Arr (a C.<$ bs)

instance Applicative Arr where
  pure = Arr . C.singleton
  Arr f <*> Arr x = Arr (C.ap f x)

instance Monad Arr where
  Arr x >>= k = Arr (C.bind x (getArr . k))
