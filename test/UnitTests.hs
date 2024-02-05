{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import qualified Data.Either as P
import Data.Functor.Identity (Identity (..))
import qualified Data.List as P
import qualified Data.Maybe as P
import Data.Monoid
import Data.Primitive
import qualified Data.Primitive.Contiguous as C
import qualified Data.Vector as V
import qualified GHC.Exts as Exts
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Prelude
import qualified Prelude as P

main :: IO ()
main = unitTests

unitTests :: IO ()
unitTests =
  mapM_
    testC
    [ quiet "Contiguous.filter = Data.List.filter" prop_filter
    , quiet "Contiguous.mapMaybe = Data.Maybe.mapMaybe" prop_mapMaybe
    , quiet "Reverse: reverse . reverse = id" prop_reverse1
    , quiet "Contiguous.reverse = Data.List.reverse" prop_reverse2
    , quiet "Contiguous.map = Data.List.map" prop_map
    , quiet "Contiguous.unfoldr = Data.List.unfoldr" prop_unfoldr
    , quiet "Contiguous.unfoldrN = Data.Vector.unfoldrN" prop_unfoldrN
    , quiet "Contiguous.traverse = Data.Traversable.traverse" prop_traverse
    , quiet "Contiguous.find = Data.Foldable.find" prop_find
    , quiet "Contiguous.scanl = Data.List.scanl" prop_scanl
    , quiet "Contiguous.scanl' = Data.List.scanl'" prop_scanl'
    , quiet "Contiguous.prescanl = Data.Vector.prescanl" prop_prescanl
    , quiet "Contiguous.prescanl' = Data.Vector.prescanl'" prop_prescanl'
    , quiet "Contiguous.generate = Data.Vector.generate" prop_generate
    , quiet "Contiguous.generateM = Data.Vector.generateM" prop_generateM
    , quiet "Contiguous.minimum = Data.Foldable.minimum" prop_minimum
    , quiet "Contiguous.maximum = Data.Foldable.maximum" prop_maximum
    , quiet "Contiguous.zipWith = Data.List.zipWith" prop_zipWith
    , quiet "Contiguous.zip = Data.List.zip" prop_zip
    , quiet "Contiguous.lefts = Data.Either.lefts" prop_lefts
    , quiet "Contiguous.rights = Data.Either.rights" prop_rights
    , quiet "Contiguous.partitionEithers = Data.Either.partitionEithers" prop_partitionEithers
    ]

-- Verbosity with which to run tests.
data Verbosity = Quiet | Verbose

-- | Hide the prop type.
data Prop = forall prop. (Testable prop) => Prop prop

-- hack to let us get away with stuffing different
-- prop types in a list
data CTest = CTest
  { _verbosity :: Verbosity
  , _label :: String
  , _prop :: Prop
  }

-- quiet output of a test
quiet :: (Testable prop) => String -> prop -> CTest
quiet l p = CTest Quiet l (Prop p)

-- verbose output of a test
-- Useful for failing tests
_verbose :: (Testable prop) => String -> prop -> CTest
_verbose l p = CTest Verbose l (Prop p)

testC :: CTest -> IO ()
testC (CTest v lbl (Prop p)) = do
  putStrLn $ P.replicate (length lbl + 6) '-'
  putStrLn $ "-- " ++ lbl ++ " --"
  putStrLn $ P.replicate (length lbl + 6) '-'
  putStr "\n"
  ($ p) $ case v of Verbose -> verboseCheck; Quiet -> quickCheck
  putStr "\n"

newtype Arr = Arr (Array L)
  deriving (Eq, Show)

newtype L = L [Int]
  deriving (Eq, Ord, Exts.IsList)

instance Show L where
  show (L x) = show x

instance Arbitrary L where
  arbitrary = do
    j <- choose (1, 6)
    fmap L $ vectorOf j arbitrary

instance Arbitrary Arr where
  arbitrary = do
    k <- choose (2, 20)
    fmap (Arr . Exts.fromList) $ vectorOf k arbitrary
  shrink (Arr xs) = fmap Arr (fmap Exts.fromList $ shrink $ Exts.toList xs)

mean :: forall t a. (Foldable t, Integral a) => t a -> a
mean xs =
  let (sum_ :: Sum a, len_ :: Sum a) = foldMap (\x -> (Sum x, Sum 1)) xs
   in (round :: Double -> a) $ (fromIntegral (getSum sum_) / fromIntegral (getSum len_))

prop_filter :: Arr -> Property
prop_filter (Arr arr) =
  property $
    let arrList = C.toList arr
        p = \(L xs) -> all even xs
     in P.filter p arrList == C.toList (C.filter p arr)

prop_mapMaybe :: Arr -> Property
prop_mapMaybe (Arr arr) =
  property $
    let arrList = C.toList arr
        p = \(L xs) -> if all even xs then Just () else Nothing
     in P.mapMaybe p arrList == C.toList (C.mapMaybe p arr :: Array ())

prop_reverse1 :: Arr -> Property
prop_reverse1 (Arr arr) =
  property $
    C.reverse (C.reverse arr) == arr

prop_reverse2 :: Arr -> Property
prop_reverse2 (Arr arr) =
  property $
    let arrList = C.toList arr
     in P.reverse arrList == C.toList (C.reverse arr)

prop_map :: Arr -> Property
prop_map (Arr arr) =
  property $
    let arrList = C.toList arr
        f = \(L xs) -> mean xs
     in P.map f arrList == C.toList (C.map f arr :: Array Int)

prop_unfoldr :: Property
prop_unfoldr =
  property $
    let f = \n -> if n == 0 then Nothing else Just (n, n - 1)
        sz = 10
     in P.unfoldr f sz == C.toList (C.unfoldr f sz :: Array Int)

prop_unfoldrN :: Property
prop_unfoldrN =
  property $
    let f = \n -> if n == 0 then Nothing else Just (n, n - 1)
        sz = 100
     in V.toList (V.unfoldrN sz f 10) == C.toList (C.unfoldrN sz f 10 :: Array Int)

prop_traverse :: Arr -> Property
prop_traverse (Arr arr) =
  property $
    let arrList = C.toList arr
        f = \(L xs) -> Identity (sum xs)
     in runIdentity (P.traverse f arrList) == C.toList (runIdentity (C.traverse f arr :: Identity (Array Int)))

prop_generate :: Property
prop_generate =
  property $
    let f = \i -> if even i then Just i else Nothing
     in V.toList (V.generate 20 f) == C.toList (C.generate 20 f :: Array (Maybe Int))

prop_generateM :: Property
prop_generateM =
  property $
    let f = \i -> if even i then Just i else Nothing
     in fmap V.toList (V.generateM 20 f) == fmap C.toList (C.generateM 20 f :: Maybe (Array Int))

{-
prop_postscanl :: Arr -> Property
prop_postscanl (Arr arr) = property $
  let arrList = V.fromList (C.toList arr)
      f = \b (L a) -> b ++ a
  in V.toList (V.postscanl f [] arrList) == C.toList (C.postscanl f [] arr :: Array [Int])
-}

prop_prescanl :: Arr -> Property
prop_prescanl (Arr arr) =
  property $
    let arrList = V.fromList (C.toList arr)
        f = \b (L a) -> b ++ a
     in V.toList (V.prescanl f [] arrList) == C.toList (C.prescanl f [] arr :: Array [Int])

prop_prescanl' :: Arr -> Property
prop_prescanl' (Arr arr) =
  property $
    let arrList = V.fromList (C.toList arr)
        f = \b (L a) -> b ++ a
     in V.toList (V.prescanl' f [] arrList) == C.toList (C.prescanl' f [] arr :: Array [Int])

prop_find :: Arr -> Property
prop_find (Arr arr) =
  property $
    let arrList = C.toList arr
        f = \(L xs) -> even (sum xs)
     in P.find f arrList == C.find f arr

prop_zipWith :: Arr -> Arr -> Property
prop_zipWith (Arr arr1) (Arr arr2) =
  property $
    let arrList1 = C.toList arr1
        arrList2 = C.toList arr2
        f = \(L xs) (L ys) -> xs ++ ys
     in P.zipWith f arrList1 arrList2 == C.toList (C.zipWith f arr1 arr2 :: Array [Int])

prop_zip :: Arr -> Arr -> Property
prop_zip (Arr arr1) (Arr arr2) =
  property $
    let arrList1 = C.toList arr1
        arrList2 = C.toList arr2
     in P.zip arrList1 arrList2 == C.toList (C.zip arr1 arr2 :: Array (L, L))
prop_scanl :: Arr -> Property
prop_scanl (Arr arr) =
  property $
    let arrList = C.toList arr
        f = \b (L a) -> b ++ a
     in P.scanl f [] arrList == C.toList (C.scanl f [] arr :: Array [Int])

prop_scanl' :: Arr -> Property
prop_scanl' (Arr arr) =
  property $
    let arrList = C.toList arr
        f = \b (L a) -> b ++ a
     in P.scanl' f [] arrList == C.toList (C.scanl' f [] arr :: Array [Int])

prop_partitionEithers :: Array' (Either Int Bool) -> Property
prop_partitionEithers (Array' arr) =
  property $
    let arrList = C.toList arr
        rhs = case C.partitionEithers arr of (as, bs) -> (C.toList as, C.toList bs)
     in P.partitionEithers arrList == rhs

prop_rights :: Array' (Either Int Bool) -> Property
prop_rights (Array' arr) =
  property $
    let arrList = C.toList arr
     in P.rights arrList == C.toList (C.rights arr)

prop_lefts :: Array' (Either Int Bool) -> Property
prop_lefts (Array' arr) =
  property $
    let arrList = C.toList arr
     in P.lefts arrList == C.toList (C.lefts arr)

prop_minimum :: Arr -> Property
prop_minimum (Arr arr) =
  property $
    let arrList = C.toList arr
     in Just (minimum arrList) == C.minimum arr

prop_maximum :: Arr -> Property
prop_maximum (Arr arr) =
  property $
    let arrList = C.toList arr
     in Just (maximum arrList) == C.maximum arr

newtype Array' a = Array' {getArray' :: Array a}
  deriving (Eq, Show, Exts.IsList)

instance (Arbitrary a) => Arbitrary (Array' a) where
  arbitrary = do
    k <- choose (2, 20)
    fmap Exts.fromList $ vectorOf k arbitrary
  shrink xs = fmap Exts.fromList $ shrink $ Exts.toList xs

-- Get around quickcheck not generating multiple arrays
-- newtype GenArrM = GenArr { getGenArrM :: Array Int }
--  deriving (Eq, Show, Exts.IsList)

-- instance Arbitrary GenArrM where
--  arbitrary = do
--    k <- choose (2,20)
--    GenArrM <$> C.generateM k (const arbitrary)
--  shrink xs = fmap Exts.fromList $ shrink $ Exts.toList xs
