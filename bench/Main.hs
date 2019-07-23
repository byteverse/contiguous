{-# language
        BangPatterns
      , MagicHash
      , ScopedTypeVariables
      , TypeApplications
      , UnboxedTuples
  #-}

module Main (main) where

import Prelude hiding
  ( null, read, Foldable(..), map
  )

import Control.Monad
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Sum(..))
import Data.Primitive.Contiguous
import GHC.Exts (RealWorld)
import System.Random
import System.Random.Shuffle
import Weigh

main :: IO ()
main = do
  array10 <- randomC @Array 10
  array100 <- randomC @Array 100
  array1000 <- randomC @Array 1000
  smallArray10 <- randomC @SmallArray 10
  smallArray100 <- randomC @SmallArray 100
  smallArray1000 <- randomC @SmallArray 1000
  primArray10 <- randomC @PrimArray 10
  primArray100 <- randomC @PrimArray 100
  primArray1000 <- randomC @PrimArray 1000

  marray10 <- randomCM @Array 10
  marray100 <- randomCM @Array 100
  marray1000 <- randomCM @Array 1000
  msmallArray10 <- randomCM @SmallArray 10
  msmallArray100 <- randomCM @SmallArray 100
  msmallArray1000 <- randomCM @SmallArray 1000
  mprimArray10 <- randomCM @PrimArray 10
  mprimArray100 <- randomCM @PrimArray 100
  mprimArray1000 <- randomCM @PrimArray 1000

  mainWith $ do
    wgroup "0-allocation" $ do
      wgroup "size" $ do
        func "array10" size array10
        func "array100" size array100
        func "array1000" size array1000

        func "smallArray10" size smallArray10
        func "smallArray100" size smallArray100
        func "smallArray1000" size smallArray1000

        func "primArray10" size primArray10
        func "primArray100" size primArray100
        func "primArray1000" size primArray1000

        io "marray10" sizeMutable marray10
        io "marray100" sizeMutable marray100
        io "marray1000" sizeMutable marray1000

        io "msmallArray10" sizeMutable msmallArray10
        io "msmallArray100" sizeMutable msmallArray100
        io "msmallArray1000" sizeMutable msmallArray1000

        io "mprimArray10" sizeMutable mprimArray10
        io "mprimArray100" sizeMutable mprimArray100
        io "mprimArray1000" sizeMutable mprimArray1000
      wgroup "null" $ do
        func "array10" null array10
        func "array100" null array100
        func "array1000" null array1000

        func "smallArray10" null smallArray10
        func "smallArray100" null smallArray100
        func "smallArray1000" null smallArray1000

        func "primArray10" null primArray10
        func "primArray100" null primArray100
        func "primArray1000" null primArray1000
      wgroup "index/read" $ do
        func "array10: index#" (index## 5) array10
        func "array100: index#" (index## 50) array100
        func "array1000: index#" (index## 500) array1000

        func "smallArray10: index#" (index## 5) smallArray10
        func "smallArray100: index#" (index## 50) smallArray100
        func "smallArray1000: index#" (index## 500) smallArray1000

        func "primArray10: index#" (index## 5) primArray10
        func "primArray100: index#" (index## 50) primArray100
        func "primArray1000: index#" (index## 500) primArray1000

        func "array10: index" (flip index 5) array10
        func "array100: index" (flip index 50) array100
        func "array1000: index" (flip index 500) array1000

        func "smallArray10: index" (flip index 5) smallArray10
        func "smallArray100: index" (flip index 50) smallArray100
        func "smallArray1000: index" (flip index 500) smallArray1000

        func "primArray10: index" (flip index 5) primArray10
        func "primArray100: index" (flip index 50) primArray100
        func "primArray1000: index" (flip index 500) primArray1000

        io "marray10: read" (flip read 5) marray10
        io "marray100: read" (flip read 50) marray100
        io "marray1000: read" (flip read 500) marray1000

        io "msmallArray10: read" (flip read 5) msmallArray10
        io "msmallArray100: read" (flip read 50) msmallArray100
        io "msmallArray1000: read" (flip read 500) msmallArray1000

        io "mprimArray10: read" (flip read 5) mprimArray10
        io "mprimArray100: read" (flip read 50) mprimArray100
        io "mprimArray1000: read" (flip read 500) mprimArray1000
      wgroup "folds" $ do
        wgroup "foldMap" $ do
          func "array10: foldMap computes sum" (foldMap sum1) array10
          func "array100: foldMap computes sum" (foldMap sum1) array100
          func "array1000: foldMap computes sum" (foldMap sum1) array1000

          func "smallArray10: foldMap computes sum" (foldMap sum1) smallArray10
          func "smallArray100: foldMap computes sum" (foldMap sum1) smallArray100
          func "smallArray1000: foldMap computes sum" (foldMap sum1) smallArray1000

          func "primArray10: foldMap computes sum" (foldMap sum1) primArray10
          func "primArray100: foldMap computes sum" (foldMap sum1) primArray100
          func "primArray1000: foldMap computes sum" (foldMap sum1) primArray1000
        wgroup "foldMap'" $ do
          func "array10: foldMap' computes sum" (foldMap' sum1) array10
          func "array100: foldMap' computes sum" (foldMap' sum1) array100
          func "array1000: foldMap' computes sum" (foldMap' sum1) array1000

          func "smallArray10: foldMap' computes sum" (foldMap' sum1) smallArray10
          func "smallArray100: foldMap' computes sum" (foldMap' sum1) smallArray100
          func "smallArray1000: foldMap' computes sum" (foldMap' sum1) smallArray1000

          func "primArray10: foldMap' computes sum" (foldMap' sum1) primArray10
          func "primArray100: foldMap' computes sum" (foldMap' sum1) primArray100
          func "primArray1000: foldMap' computes sum" (foldMap' sum1) primArray1000
        wgroup "foldr" $ do
          func "array10: foldr computes sum" (foldr (+) 0) array10
          func "array100: foldr computes sum" (foldr (+) 0) array100
          func "array1000: foldr computes sum" (foldr (+) 0) array1000

          func "smallArray10: foldr computes sum" (foldr (+) 0) smallArray10
          func "smallArray100: foldr computes sum" (foldr (+) 0) smallArray100
          func "smallArray1000: foldr computes sum" (foldr (+) 0) smallArray1000

          func "primArray10: foldr computes sum" (foldr (+) 0) primArray10
          func "primArray100: foldr computes sum" (foldr (+) 0) primArray100
          func "primArray1000: foldr computes sum" (foldr (+) 0) primArray1000
        wgroup "foldr'" $ do
          func "array10: foldr' computes sum" (foldr' (+) 0) array10
          func "array100: foldr' computes sum" (foldr' (+) 0) array100
          func "array1000: foldr' computes sum" (foldr' (+) 0) array1000

          func "smallArray10: foldr' computes sum" (foldr' (+) 0) smallArray10
          func "smallArray100: foldr' computes sum" (foldr' (+) 0) smallArray100
          func "smallArray1000: foldr' computes sum" (foldr' (+) 0) smallArray1000

          func "primArray10: foldr' computes sum" (foldr' (+) 0) primArray10
          func "primArray100: foldr' computes sum" (foldr' (+) 0) primArray100
          func "primArray1000: foldr' computes sum" (foldr' (+) 0) primArray1000
        wgroup "foldl" $ do
          func "array10: foldl computes sum" (foldl (+) 0) array10
          func "array100: foldl computes sum" (foldl (+) 0) array100
          func "array1000: foldl computes sum" (foldl (+) 0) array1000

          func "smallArray10: foldl computes sum" (foldl (+) 0) smallArray10
          func "smallArray100: foldl computes sum" (foldl (+) 0) smallArray100
          func "smallArray1000: foldl computes sum" (foldl (+) 0) smallArray1000

          func "primArray10: foldl computes sum" (foldl (+) 0) primArray10
          func "primArray100: foldl computes sum" (foldl (+) 0) primArray100
          func "primArray1000: foldl computes sum" (foldl (+) 0) primArray1000
        wgroup "foldl'" $ do
          func "array10: foldl' computes sum" (foldl' (+) 0) array10
          func "array100: foldl' computes sum" (foldl' (+) 0) array100
          func "array1000: foldl' computes sum" (foldl' (+) 0) array1000

          func "smallArray10: foldl' computes sum" (foldl' (+) 0) smallArray10
          func "smallArray100: foldl' computes sum" (foldl' (+) 0) smallArray100
          func "smallArray1000: foldl' computes sum" (foldl' (+) 0) smallArray1000

          func "primArray10: foldl' computes sum" (foldl' (+) 0) primArray10
          func "primArray100: foldl' computes sum" (foldl' (+) 0) primArray100
          func "primArray1000: foldl' computes sum" (foldl' (+) 0) primArray1000
        wgroup "ifoldl'" $ do
          func "array10: ifoldl' computes sum" (ifoldl' add3 0) array10
          func "array100: ifoldl' computes sum" (ifoldl' add3 0) array100
          func "array1000: ifoldl' computes sum" (ifoldl' add3 0) array1000

          func "smallArray10: ifoldl' computes sum" (ifoldl' add3 0) smallArray10
          func "smallArray100: ifoldl' computes sum" (ifoldl' add3 0) smallArray100
          func "smallArray1000: ifoldl' computes sum" (ifoldl' add3 0) smallArray1000

          func "primArray10: ifoldl' computes sum" (ifoldl' add3 0) primArray10
          func "primArray100: ifoldl' computes sum" (ifoldl' add3 0) primArray100
          func "primArray1000: ifoldl' computes sum" (ifoldl' add3 0) primArray1000
        wgroup "ifoldr'" $ do
          func "array10: ifoldr' computes sum" (ifoldr' add3 0) array10
          func "array100: ifoldr' computes sum" (ifoldr' add3 0) array100
          func "array1000: ifoldr' computes sum" (ifoldr' add3 0) array1000

          func "smallArray10: ifoldr' computes sum" (ifoldr' add3 0) smallArray10
          func "smallArray100: ifoldr' computes sum" (ifoldr' add3 0) smallArray100
          func "smallArray1000: ifoldr' computes sum" (ifoldr' add3 0) smallArray1000

          func "primArray10: ifoldr' computes sum" (ifoldr' add3 0) primArray10
          func "primArray100: ifoldr' computes sum" (ifoldr' add3 0) primArray100
          func "primArray1000: ifoldr' computes sum" (ifoldr' add3 0) primArray1000
        wgroup "foldlMap'" $ do
          func "array10: foldlMap' computes sum" (foldMap' sum1) array10
          func "array100: foldlMap' computes sum" (foldMap' sum1) array100
          func "array1000: foldlMap' computes sum" (foldMap' sum1) array1000

          func "smallArray10: foldlMap' computes sum" (foldMap' sum1) smallArray10
          func "smallArray100: foldlMap' computes sum" (foldMap' sum1) smallArray100
          func "smallArray1000: foldlMap' computes sum" (foldMap' sum1) smallArray1000

          func "primArray10: foldlMap' computes sum" (foldMap' sum1) primArray10
          func "primArray100: foldlMap' computes sum" (foldMap' sum1) primArray100
          func "primArray1000: foldlMap' computes sum" (foldMap' sum1) primArray1000

        wgroup "ifoldlMap'" $ do
          func "array10: ifoldlMap' computes sum" (ifoldlMap' isumN) array10
          func "array100: ifoldlMap' computes sum" (ifoldlMap' isumN) array100
          func "array1000: ifoldlMap' computes sum" (ifoldlMap' isumN) array1000

          func "smallArray10: ifoldlMap' computes sum" (ifoldlMap' isumN) smallArray10
          func "smallArray100: ifoldlMap' computes sum" (ifoldlMap' isumN) smallArray100
          func "smallArray1000: ifoldlMap' computes sum" (ifoldlMap' isumN) smallArray1000

          func "primArray10: ifoldlMap' computes sum" (ifoldlMap' isumN) primArray10
          func "primArray100: ifoldlMap' computes sum" (ifoldlMap' isumN) primArray100
          func "primArray1000: ifoldlMap' computes sum" (ifoldlMap' isumN) primArray1000
        wgroup "ifoldlMap1'" $ do
          func "array10: ifoldlMap1' computes sum" (ifoldlMap1' isumN) array10
          func "array100: ifoldlMap1' computes sum" (ifoldlMap1' isumN) array100
          func "array1000: ifoldlMap1' computes sum" (ifoldlMap1' isumN) array1000

          func "smallArray10: ifoldlMap1' computes sum" (ifoldlMap1' isumN) smallArray10
          func "smallArray100: ifoldlMap1' computes sum" (ifoldlMap1' isumN) smallArray100
          func "smallArray1000: ifoldlMap1' computes sum" (ifoldlMap1' isumN) smallArray1000

          func "primArray10: ifoldlMap1' computes sum" (ifoldlMap1' isumN) primArray10
          func "primArray100: ifoldlMap1' computes sum" (ifoldlMap1' isumN) primArray100
          func "primArray1000: ifoldlMap1' computes sum" (ifoldlMap1' isumN) primArray1000
        wgroup "foldlM'" $ do
          func "array10: foldlM' computes sum" (foldlM' idM 0) array10
          func "array100: foldlM' computes sum" (foldlM' idM 0) array100
          func "array1000: foldlM' computes sum" (foldlM' idM 0) array1000

          func "smallArray10: foldlM' computes sum" (foldlM' idM 0) smallArray10
          func "smallArray100: foldlM' computes sum" (foldlM' idM 0) smallArray100
          func "smallArray1000: foldlM' computes sum" (foldlM' idM 0) smallArray1000

          func "primArray10: foldlM' computes sum" (foldlM' idM 0) primArray10
          func "primArray100: foldlM' computes sum" (foldlM' idM 0) primArray100
          func "primArray1000: foldlM' computes sum" (foldlM' idM 0) primArray1000
    wgroup "maps" $ do
      wgroup "map" $ do
        func "array10" mapPlus1 array10
        func "array100" mapPlus1 array100
        func "array1000" mapPlus1 array1000

        func "smallArray10" mapPlus1 smallArray10
        func "smallArray100" mapPlus1 smallArray100
        func "smallArray1000" mapPlus1 smallArray1000

        func "primArray10" mapPlus1 primArray10
        func "primArray100" mapPlus1 primArray100
        func "primArray1000" mapPlus1 primArray1000
      wgroup "map'" $ do
        func "array10" mapPlus1' array10
        func "array100" mapPlus1' array100
        func "array1000" mapPlus1' array1000

        func "smallArray10" mapPlus1' smallArray10
        func "smallArray100" mapPlus1' smallArray100
        func "smallArray1000" mapPlus1' smallArray1000

        func "primArray10" mapPlus1' primArray10
        func "primArray100" mapPlus1' primArray100
        func "primArray1000" mapPlus1' primArray1000
      wgroup "mapMaybe" $ do
        func "array10" mapMaybeJ array10
        func "array100" mapMaybeJ array100
        func "array1000" mapMaybeJ array1000

        func "smallArray10" mapMaybeJ smallArray10
        func "smallArray100" mapMaybeJ smallArray100
        func "smallArray1000" mapMaybeJ smallArray1000

        func "primArray10" mapMaybeJ primArray10
        func "primArray100" mapMaybeJ primArray100
        func "primArray1000" mapMaybeJ primArray1000

mapMaybeJ :: forall arr. (Contiguous arr, Element arr Int)
  => arr Int
  -> ()
mapMaybeJ arr =
  let !(arr' :: arr Int) = mapMaybe Just arr
   in ()

mapPlus1 :: forall arr. (Contiguous arr, Element arr Int)
  => arr Int -> ()
mapPlus1 arr = let !(arr' :: arr Int) = map (+1) arr in ()

mapPlus1' :: forall arr. (Contiguous arr, Element arr Int)
  => arr Int -> ()
mapPlus1' arr = let !(arr' :: arr Int) = map' (+1) arr in ()

plus1 :: Int -> Int
plus1 = (+1)

sum1 :: a -> Sum Int
sum1 = const (Sum 1)

isumN :: Int -> a -> Sum Int
isumN x = const (Sum x)

idM :: Int -> Int -> Identity Int
idM x y = Identity (x + y)

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

index## :: (Contiguous arr, Element arr a) => Int -> arr a -> ()
index## ix arr = case index# arr ix of !(# _x #) -> ()

randomList :: Int -> IO [Int]
randomList sz = replicateM sz (randomRIO (minBound,maxBound))

randomC :: (Contiguous arr, Element arr Int)
  => Int
  -> IO (arr Int)
randomC sz = do
  rList <- randomList sz
  rList' <- shuffleM rList
  pure (fromListN sz rList')

randomCM :: (Contiguous arr, Element arr Int)
  => Int
  -> IO (Mutable arr RealWorld Int)
randomCM sz = do
  rList <- randomList sz
  rList' <- shuffleM rList
  fromListMutableN sz rList'

