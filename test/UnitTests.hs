{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableInstances #-}

module Main (main) where

import Prelude
--import Data.Primitive.Contiguous (Contiguous,Element,Mutable)
import qualified Data.Primitive.Contiguous as C
import qualified Prelude as P
import qualified Data.Maybe as P
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified GHC.Exts as Exts
import Data.Primitive

main :: IO ()
main = unitTests  

unitTests :: IO ()
unitTests = mapM_ printAndTest
  [ ("Filter", prop_filter)
  , ("MapMaybe",prop_mapMaybe)
  , ("Reverse: reverse . reverse = id", prop_reverse1)
  , ("Contiguous.reverse = Data.List.reverse", prop_reverse2)
  ]

printAndTest :: (Testable prop) => (String,prop) -> IO ()
printAndTest (x,y) = do
  putStrLn $ P.replicate (length x + 6) '-'
  putStrLn $ "-- " ++ x ++ " --"
  putStrLn $ P.replicate (length x + 6) '-'
  putStr "\n"
  quickCheck y 
  putStr "\n"

newtype Arr = Arr (Array L)
  deriving (Eq,Show)

newtype L = L [Int]
  deriving (Eq,Exts.IsList)

instance Show L where
  show (L x) = show x

instance Arbitrary L where
  arbitrary = do
    j <- choose (2,4)
    fmap L $ vectorOf j arbitrary

instance Arbitrary Arr where
  arbitrary = do
    k <- choose (2,20)
    fmap (Arr . Exts.fromList) $ vectorOf k arbitrary
  shrink (Arr xs) = fmap Arr (fmap Exts.fromList $ shrink $ Exts.toList xs)

prop_filter :: Arr -> Property
prop_filter (Arr arr) = property $
  let arrList = C.toList arr
      p = \(L xs) -> all even xs
   in P.filter p arrList == C.toList (C.filter p arr)

prop_mapMaybe :: Arr -> Property
prop_mapMaybe (Arr arr) = property $
  let arrList = C.toList arr
      p = \(L xs) -> if all even xs then Just () else Nothing
   in P.mapMaybe p arrList == C.toList (C.mapMaybe p arr :: Array ())

prop_reverse1 :: Arr -> Property
prop_reverse1 (Arr arr) = property $
  C.reverse (C.reverse arr) == arr

prop_reverse2 :: Arr -> Property
prop_reverse2 (Arr arr) = property $
  let arrList = C.toList arr
   in P.reverse arrList == C.toList (C.reverse arr)

