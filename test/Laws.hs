{-# language InstanceSigs, TypeFamilies, UndecidableInstances #-}

module Main (main) where

import Data.Foldable
import Data.Primitive.Contiguous
import Data.Proxy
import Test.QuickCheck
import Test.QuickCheck.Classes
import qualified Data.Primitive.Contiguous as C
import qualified GHC.Exts as Exts

main :: IO ()
main = lawsCheckMany laws

laws :: [(String, [Laws])]
laws =
  [ ("Arr", [ functorLaws arr
            , applicativeLaws arr
            , foldableLaws arr
            , traversableLaws arr
            , isListLaws arr1
            ]
    )
  ]

newtype Arr a = Arr (Array a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Arr a) where
  arbitrary = fmap (Arr . Exts.fromList) arbitrary

arr :: Proxy Arr
arr = Proxy

arr1 :: Proxy (Arr Int)
arr1 = Proxy

instance Functor Arr where
  fmap f (Arr a) = Arr (C.map f a)
  a <$ (Arr bs) = Arr (a C.<$ bs)

instance Applicative Arr where
  pure = Arr . C.singleton
  Arr f <*> Arr x = Arr (C.ap f x)

instance Foldable Arr where
  foldMap f (Arr a) = C.foldMap f a
  foldr f z0 (Arr a) = C.foldr f z0 a
  foldr' f z0 (Arr a) = C.foldr' f z0 a
  foldl f z0 (Arr a) = C.foldl f z0 a
  foldl' f z0 (Arr a) = C.foldl' f z0 a
  toList (Arr a) = C.toList a
  null (Arr a) = C.null a
  length (Arr a) = C.size a

instance Traversable Arr where
  traverse :: Applicative f => (a -> f b) -> Arr a -> f (Arr b)
  traverse f (Arr a) = fmap Arr (C.traverse f a)

  sequenceA :: Applicative f => Arr (f a) -> f (Arr a)
  sequenceA (Arr f) = fmap Arr (C.sequence f)

instance Exts.IsList (Arr a) where
  type Item (Arr a) = a
  fromList = Arr . C.fromList
  fromListN len = Arr . C.fromListN len
  toList (Arr a) = Exts.toList a


