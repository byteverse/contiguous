{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}

module Main (main) where

import Data.Functor.Identity (Identity(..))
import Data.Monoid
import Data.Primitive
import Prelude
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified Data.Maybe as P
import qualified Data.Primitive.Contiguous as C
import qualified GHC.Exts as Exts
import qualified Prelude as P
import qualified Data.List as P
import qualified Data.Vector as V

main :: IO ()
main = do
  putStr "\n"
  unitTests  

unitTests :: IO ()
unitTests = mapM_ printAndTest
  [ ("Contiguous.filter = Data.List.filter", prop_filter)
  , ("Contiguous.mapMaybe = Data.Maybe.mapMaybe",prop_mapMaybe)
  , ("Reverse: reverse . reverse = id", prop_reverse1)
  , ("Contiguous.reverse = Data.List.reverse", prop_reverse2)
  , ("Contiguous.map = Data.List.map", prop_map)
  , ("Contiguous.unfoldr = Data.List.unfoldr", \_ -> prop_unfoldr)
  , ("Contiguous.unfoldrN = Data.Vector.unfoldrN", \_ -> prop_unfoldrN)
  , ("Contiguous.traverse = Data.Traversable.traverse", prop_traverse)
  ]

printAndTest :: (Testable prop) => (String, prop) -> IO ()
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
    j <- choose (1,6)
    fmap L $ vectorOf j arbitrary

instance Arbitrary Arr where
  arbitrary = do
    k <- choose (2,20)
    fmap (Arr . Exts.fromList) $ vectorOf k arbitrary
  shrink (Arr xs) = fmap Arr (fmap Exts.fromList $ shrink $ Exts.toList xs)

mean :: forall t a. (Foldable t, Integral a) => t a -> a
mean xs =
  let (sum_ :: Sum a,len_ :: Sum a) = foldMap (\x -> (Sum x, Sum 1)) xs
  in (round :: Double -> a) $ (fromIntegral (getSum sum_) / fromIntegral (getSum len_))

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

prop_map :: Arr -> Property
prop_map (Arr arr) = property $
  let arrList = C.toList arr
      f = \(L xs) -> mean xs
   in P.map f arrList == C.toList (C.map f arr :: Array Int)

prop_unfoldr :: Property
prop_unfoldr = property $
  let f = \n -> if n == 0 then Nothing else Just (n,n-1)
      sz = 10
   in P.unfoldr f sz == C.toList (C.unfoldr f sz :: Array Int)

prop_unfoldrN :: Property
prop_unfoldrN = property $
  let f = \n -> if n == 0 then Nothing else Just (n,n-1)
      sz = 100
   in V.toList (V.unfoldrN sz f 10) == C.toList (C.unfoldrN sz f 10 :: Array Int)

prop_traverse :: Arr -> Property
prop_traverse (Arr arr) = property $
  let arrList = C.toList arr
      f = \(L xs) -> Identity (sum xs)
   in runIdentity (P.traverse f arrList) == C.toList (runIdentity (C.traverse f arr))

--prop_itraverse :: Arr -> Property
--prop_itraverse (Arr arr) = property $
--  let arrVec = V.fromList (C.toList arr)
--      f = \i (L xs) -> Identity (i + sum xs)
--   in V.toList (V.itraverse f arrVec) == C.toList (C.itraverse f arr)
