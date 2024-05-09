{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}

{- | The contiguous package presents a common API to a number of contiguous
array types and their mutable counterparts. This is enabled with the
'Contiguous' typeclass, which parameterises over a contiguous array type and
defines the core operations. However, the stable part of the interface is
contained in this module, which combines those primitives into common,
efficient array algorithms suitable for replacing pointer-heavy list
manipulations.
-}
module Data.Primitive.Contiguous
  ( -- * Accessors

    -- ** Length Information
    size
  , sizeMut
  , null

    -- ** Indexing
  , index
  , index#
  , read

    -- ** Monadic indexing
  , indexM

    -- * Construction

    -- ** Initialisation
  , empty
  , new
  , singleton
  , doubleton
  , tripleton
  , quadrupleton
  , quintupleton
  , sextupleton
  , replicate
  , replicateMut
  , generate
  , generateM
  , generateMutable
  , iterateN
  , iterateMutableN
  , write

    -- ** Fixed Length
  , construct1
  , construct2
  , construct3
  , construct4
  , construct5
  , construct6

    -- ** Running
  , run

    -- ** Monadic initialisation
  , replicateMutM
  , generateMutableM
  , iterateMutableNM
  , create
  , createT

    -- ** Unfolding
  , unfoldr
  , unfoldrN
  , unfoldrMutable

    -- ** Enumeration
  , enumFromN
  , enumFromMutableN

    -- ** Concatenation
  , append

    -- ** Splitting and Splicing
  , insertAt

    -- * Slicing
  , Slice
  , MutableSlice
  , slice
  , sliceMut
  , toSlice
  , toSliceMut

    -- * Modifying arrays
  , replaceAt
  , modifyAt
  , modifyAt'
  , modifyAtF
  , modifyAtF'
  , deleteAt

    -- ** Permutations
  , reverse
  , reverseMutable
  , reverseSlice

    -- ** Resizing
  , resize
  , shrink
  , unsafeShrinkAndFreeze

    -- * Elementwise operations

    -- ** Mapping
  , map
  , map'
  , mapMutable
  , mapMutable'
  , imap
  , imap'
  , imapMutable
  , imapMutable'
  , modify
  , modify'
  , mapMaybe

    -- ** Zipping
  , zip
  , zipWith
  , izipWith

    -- ** Specific elements
  , swap

    -- * Working with predicates

    -- ** Filtering
  , filter
  , ifilter
  , catMaybes
  , lefts
  , rights
  , partitionEithers

    -- ** Searching
  , find
  , findIndex
  , elem
  , maximum
  , minimum
  , maximumBy
  , minimumBy

    -- ** Comparing for equality
  , equals
  , equalsMut
  , same

    -- * Folds
  , foldl
  , foldl'
  , foldr
  , foldr'
  , foldMap
  , foldMap'
  , foldlMap'
  , ifoldl'
  , ifoldr
  , ifoldr'
  , ifoldlMap'
  , ifoldlMap1'
  , foldlM'
  , ifoldlM'
  , foldrM'
  , asum
  , all
  , any

    -- ** Zipping Folds
  , foldrZipWith
  , ifoldrZipWith
  , foldlZipWith'
  , ifoldlZipWith'
  , foldlZipWithM'
  , ifoldlZipWithM'

    -- * Traversals
  , traverse
  , traverse_
  , itraverse
  , itraverse_
  , traverseP
  , itraverseP
  , mapM
  , forM
  , mapM_
  , forM_
  , for
  , for_
  , sequence
  , sequence_

    -- * Typeclass method defaults
  , (<$)
  , ap

    -- * Prefix sums (scans)
  , scanl
  , scanl'
  , iscanl
  , iscanl'
  , prescanl
  , prescanl'
  , iprescanl
  , iprescanl'
  -- , postscanl
  -- , ipostscanl

  , mapAccum'
  , mapAccumLM'

    -- * Conversions

    -- ** Lists
  , fromList
  , fromListN
  , fromListMutable
  , fromListMutableN
  , unsafeFromListN
  , unsafeFromListReverseN
  , unsafeFromListReverseMutableN
  , toList
  , toListMutable

    -- ** Other array types
  , convert
  , lift
  , liftMut
  , unlift
  , unliftMut

    -- ** Between mutable and immutable variants
  , clone
  , cloneMut
  , copy
  , copyMut
  , freeze
  , thaw
  , unsafeFreeze

    -- * Hashing
  , liftHashWithSalt

    -- * Forcing an array and its contents
  , rnf

    -- * Classes
  , Contiguous (Mutable, Element, Sliced, MutableSliced)
  , ContiguousU
  , Always

    -- * Re-Exports
  , Array
  , MutableArray
  , SmallArray
  , SmallMutableArray
  , PrimArray
  , MutablePrimArray
  , UnliftedArray
  , MutableUnliftedArray
  ) where

import Control.Monad.Primitive
import Data.Primitive
import Data.Primitive.Unlifted.Array
import Prelude hiding (Foldable (..), all, any, filter, map, mapM, mapM_, read, replicate, reverse, scanl, sequence, sequence_, traverse, zip, zipWith, (<$))

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits (xor)
import Data.Coerce (coerce)
import Data.Foldable (length)
import Data.Primitive.Contiguous.Class (Always, Contiguous (..), ContiguousU (..), MutableSlice, Slice)
import Data.Semigroup (First (..))
import Data.Word (Word8)
import GHC.Base (build)
import GHC.Exts (Int (..), MutableArrayArray#, dataToTag#, isTrue#, sameMutableArrayArray#, unsafeCoerce#)

import qualified Control.Applicative as A
import qualified Prelude

construct1 ::
  (Contiguous arr, Element arr a) =>
  a ->
  arr a
{-# INLINE construct1 #-}
construct1 = singleton

construct2 ::
  (Contiguous arr, Element arr a) =>
  a ->
  a ->
  arr a
{-# INLINE construct2 #-}
construct2 = doubleton

construct3 ::
  (Contiguous arr, Element arr a) =>
  a ->
  a ->
  a ->
  arr a
{-# INLINE construct3 #-}
construct3 = tripleton

construct4 ::
  (Contiguous arr, Element arr a) =>
  a ->
  a ->
  a ->
  a ->
  arr a
{-# INLINE construct4 #-}
construct4 = quadrupleton

construct5 ::
  (Contiguous arr, Element arr a) =>
  a ->
  a ->
  a ->
  a ->
  a ->
  arr a
{-# INLINE construct5 #-}
construct5 = quintupleton

construct6 ::
  (Contiguous arr, Element arr a) =>
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  arr a
{-# INLINE construct6 #-}
construct6 = sextupleton

-- | Append two arrays.
append :: (Contiguous arr, Element arr a) => arr a -> arr a -> arr a
append !a !b = run $ do
  m <- new (size a + size b)
  copy m 0 (toSlice a)
  copy m (size a) (toSlice b)
  unsafeFreeze m
{-# INLINE append #-}

-- | Delete the element at the given position.
deleteAt :: (Contiguous arr, Element arr a) => arr a -> Int -> arr a
deleteAt src i = run $ do
  dst <- thaw (slice src 0 (size src - 1))
  let !i' = i + 1
  copy dst i (slice src i' (size src - i'))
  unsafeFreeze dst
{-# INLINE deleteAt #-}

{- | Create a copy of an array except the element at the index is replaced with
  the given value.
-}
replaceAt :: (Contiguous arr, Element arr a) => arr a -> Int -> a -> arr a
replaceAt src i x = create $ do
  dst <- thaw (toSlice src)
  write dst i x
  pure dst
{-# INLINE replaceAt #-}

modifyAt ::
  (Contiguous arr, Element arr a) =>
  (a -> a) ->
  arr a ->
  Int ->
  arr a
modifyAt f src i = replaceAt src i $ f (index src i)
{-# INLINE modifyAt #-}

{- | Variant of modifyAt that forces the result before installing it in the
array.
-}
modifyAt' ::
  (Contiguous arr, Element arr a) =>
  (a -> a) ->
  arr a ->
  Int ->
  arr a
modifyAt' f src i = replaceAt src i $! f (index src i)
{-# INLINE modifyAt' #-}

modifyAtF ::
  (Contiguous arr, Element arr a, Functor f) =>
  (a -> f a) ->
  arr a ->
  Int ->
  f (arr a)
modifyAtF f src i = replaceAt src i <$> f (index src i)
{-# INLINE modifyAtF #-}

{- | Variant of modifyAtF that forces the result before installing it in the
array. Note that this requires 'Monad' rather than 'Functor'.
-}
modifyAtF' ::
  (Contiguous arr, Element arr a, Monad f) =>
  (a -> f a) ->
  arr a ->
  Int ->
  f (arr a)
modifyAtF' f src i = do
  !r <- f (index src i)
  let !dst = replaceAt src i r
  pure dst
{-# INLINE modifyAtF' #-}

-- | Map over the elements of an array with the index.
imap ::
  (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) =>
  (Int -> b -> c) ->
  arr1 b ->
  arr2 c
imap f a = run $ do
  mb <- new (size a)
  let go !i
        | i == size a = pure ()
        | otherwise = do
            x <- indexM a i
            write mb i (f i x)
            go (i + 1)
  go 0
  unsafeFreeze mb
{-# INLINE imap #-}

{- | Map strictly over the elements of an array with the index.

  Note that because a new array must be created, the resulting
  array type can be /different/ than the original.
-}
imap' ::
  (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) =>
  (Int -> b -> c) ->
  arr1 b ->
  arr2 c
imap' f a = run $ do
  mb <- new (size a)
  let go !i
        | i == size a = pure ()
        | otherwise = do
            x <- indexM a i
            let !b = f i x
            write mb i b
            go (i + 1)
  go 0
  unsafeFreeze mb
{-# INLINE imap' #-}

{- | Map over the elements of an array.

  Note that because a new array must be created, the resulting
  array type can be /different/ than the original.
-}
map ::
  (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) =>
  (b -> c) ->
  arr1 b ->
  arr2 c
map f a = run $ do
  mb <- new (size a)
  let go !i
        | i == size a = pure ()
        | otherwise = do
            x <- indexM a i
            write mb i (f x)
            go (i + 1)
  go 0
  unsafeFreeze mb
{-# INLINE map #-}

{- | Map strictly over the elements of an array.

  Note that because a new array must be created, the resulting
  array type can be /different/ than the original.
-}
map' ::
  (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) =>
  (b -> c) ->
  arr1 b ->
  arr2 c
map' f a = run $ do
  mb <- new (size a)
  let go !i
        | i == size a = pure ()
        | otherwise = do
            x <- indexM a i
            let !b = f x
            write mb i b
            go (i + 1)
  go 0
  unsafeFreeze mb
{-# INLINE map' #-}

-- | Convert one type of array into another.
convert ::
  (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 b) =>
  arr1 b ->
  arr2 b
convert a = map id a
{-# INLINE convert #-}

-- | Right fold over the element of an array.
foldr :: (Contiguous arr, Element arr a) => (a -> b -> b) -> b -> arr a -> b
{-# INLINE foldr #-}
foldr f z = \arr ->
  let !sz = size arr
      go !ix =
        if sz > ix
          then case index# arr ix of
            (# x #) -> f x (go (ix + 1))
          else z
   in go 0

{- | Right fold over the element of an array, lazy in the accumulator,
provides index to the step function.
-}
ifoldr :: (Contiguous arr, Element arr a) => (Int -> a -> b -> b) -> b -> arr a -> b
{-# INLINE ifoldr #-}
ifoldr f z = \arr ->
  let !sz = size arr
      go !ix =
        if sz > ix
          then case index# arr ix of
            (# x #) -> f ix x (go (ix + 1))
          else z
   in go 0

-- | Strict right fold over the elements of an array.
foldr' :: (Contiguous arr, Element arr a) => (a -> b -> b) -> b -> arr a -> b
foldr' f !z = \arr ->
  let go !ix !acc =
        if ix == -1
          then acc
          else case index# arr ix of
            (# x #) -> go (ix - 1) (f x acc)
   in go (size arr - 1) z
{-# INLINE foldr' #-}

-- | Left fold over the elements of an array.
foldl :: (Contiguous arr, Element arr a) => (b -> a -> b) -> b -> arr a -> b
foldl f z = \arr ->
  let !sz = size arr
      go !ix acc =
        if ix == sz
          then acc
          else case index# arr ix of
            (# x #) -> go (ix + 1) (f acc x)
   in go 0 z
{-# INLINE foldl #-}

-- | Strict left fold over the elements of an array.
foldl' :: (Contiguous arr, Element arr a) => (b -> a -> b) -> b -> arr a -> b
foldl' f !z = \arr ->
  let !sz = size arr
      go !ix !acc =
        if ix == sz
          then acc
          else case index# arr ix of
            (# x #) -> go (ix + 1) (f acc x)
   in go 0 z
{-# INLINE foldl' #-}

{- | Strict left fold over the elements of an array, where the accumulating
  function cares about the index of the element.
-}
ifoldl' ::
  (Contiguous arr, Element arr a) =>
  (b -> Int -> a -> b) ->
  b ->
  arr a ->
  b
ifoldl' f !z = \arr ->
  let !sz = size arr
      go !ix !acc =
        if ix == sz
          then acc
          else case index# arr ix of
            (# x #) -> go (ix + 1) (f acc ix x)
   in go 0 z
{-# INLINE ifoldl' #-}

{- | Strict right fold over the elements of an array, where the accumulating
  function cares about the index of the element.
-}
ifoldr' ::
  (Contiguous arr, Element arr a) =>
  (Int -> a -> b -> b) ->
  b ->
  arr a ->
  b
ifoldr' f !z = \arr ->
  let !sz = size arr
      go !ix !acc =
        if ix == (-1)
          then acc
          else case index# arr ix of
            (# x #) -> go (ix - 1) (f ix x acc)
   in go (sz - 1) z
{-# INLINE ifoldr' #-}

-- | Monoidal fold over the element of an array.
foldMap :: (Contiguous arr, Element arr a, Monoid m) => (a -> m) -> arr a -> m
foldMap f = \arr ->
  let !sz = size arr
      go !ix =
        if sz > ix
          then case index# arr ix of
            (# x #) -> mappend (f x) (go (ix + 1))
          else mempty
   in go 0
{-# INLINE foldMap #-}

-- | Strict monoidal fold over the elements of an array.
foldMap' ::
  (Contiguous arr, Element arr a, Monoid m) =>
  (a -> m) ->
  arr a ->
  m
foldMap' f = \arr ->
  let !sz = size arr
      go !ix !acc =
        if ix == sz
          then acc
          else case index# arr ix of
            (# x #) -> go (ix + 1) (mappend acc (f x))
   in go 0 mempty
{-# INLINE foldMap' #-}

-- | Strict left monoidal fold over the elements of an array.
foldlMap' ::
  (Contiguous arr, Element arr a, Monoid m) =>
  (a -> m) ->
  arr a ->
  m
foldlMap' = foldMap'
{-# INLINE foldlMap' #-}

-- | Strict monoidal fold over the elements of an array.
ifoldlMap' ::
  (Contiguous arr, Element arr a, Monoid m) =>
  (Int -> a -> m) ->
  arr a ->
  m
ifoldlMap' f = \arr ->
  let !sz = size arr
      go !ix !acc =
        if ix == sz
          then acc
          else case index# arr ix of
            (# x #) -> go (ix + 1) (mappend acc (f ix x))
   in go 0 mempty
{-# INLINE ifoldlMap' #-}

-- | Strict monoidal fold over the elements of an array.
ifoldlMap1' ::
  (Contiguous arr, Element arr a, Semigroup m) =>
  (Int -> a -> m) ->
  arr a ->
  m
ifoldlMap1' f = \arr ->
  let !sz = size arr
      go !ix !acc =
        if ix == sz
          then acc
          else case index# arr ix of
            (# x #) -> go (ix + 1) (acc <> f ix x)
      !(# e0 #) = index# arr 0
   in go 1 (f 0 e0)
{-# INLINE ifoldlMap1' #-}

-- | Strict right monadic fold over the elements of an array.
foldrM' ::
  (Contiguous arr, Element arr a, Monad m) =>
  (a -> b -> m b) ->
  b ->
  arr a ->
  m b
foldrM' f !z0 = \arr ->
  let !sz = size arr
      go !ix !acc1 =
        if ix >= 0
          then do
            let (# x #) = index# arr ix
            acc2 <- f x acc1
            go (ix - 1) acc2
          else pure acc1
   in go (sz - 1) z0
{-# INLINE foldrM' #-}

-- | Strict left monadic fold over the elements of an array.
foldlM' ::
  (Contiguous arr, Element arr a, Monad m) =>
  (b -> a -> m b) ->
  b ->
  arr a ->
  m b
foldlM' f !z0 = \arr ->
  let !sz = size arr
      go !ix !acc1 =
        if ix < sz
          then do
            let (# x #) = index# arr ix
            acc2 <- f acc1 x
            go (ix + 1) acc2
          else pure acc1
   in go 0 z0
{-# INLINE foldlM' #-}

-- | Strict left monadic fold over the elements of an array.
ifoldlM' ::
  (Contiguous arr, Element arr a, Monad m) =>
  (b -> Int -> a -> m b) ->
  b ->
  arr a ->
  m b
ifoldlM' f z0 = \arr ->
  let !sz = size arr
      go !ix !acc1 =
        if ix < sz
          then do
            let (# x #) = index# arr ix
            acc2 <- f acc1 ix x
            go (ix + 1) acc2
          else pure acc1
   in go 0 z0
{-# INLINE ifoldlM' #-}

-- | Drop elements that do not satisfy the predicate.
filter ::
  (Contiguous arr, Element arr a) =>
  (a -> Bool) ->
  arr a ->
  arr a
filter p arr = ifilter (const p) arr
{-# INLINE filter #-}

{- | Drop elements that do not satisfy the predicate which
  is applied to values and their indices.
-}
ifilter ::
  (Contiguous arr, Element arr a) =>
  (Int -> a -> Bool) ->
  arr a ->
  arr a
ifilter p arr = run $ do
  marr :: MutablePrimArray s Word8 <- newPrimArray sz
  let go1 :: Int -> Int -> ST s Int
      go1 !ix !numTrue =
        if ix < sz
          then do
            atIx <- indexM arr ix
            let !keep = p ix atIx
            let !keepTag = I# (dataToTag# keep)
            writePrimArray marr ix (fromIntegral keepTag)
            go1 (ix + 1) (numTrue + keepTag)
          else pure numTrue
  numTrue <- go1 0 0
  if numTrue == sz
    then pure arr
    else do
      marrTrues <- new numTrue
      let go2 !ixSrc !ixDst = when (ixDst < numTrue) $ do
            atIxKeep <- readPrimArray marr ixSrc
            if isTrue atIxKeep
              then do
                atIxVal <- indexM arr ixSrc
                write marrTrues ixDst atIxVal
                go2 (ixSrc + 1) (ixDst + 1)
              else go2 (ixSrc + 1) ixDst
      go2 0 0
      unsafeFreeze marrTrues
 where
  !sz = size arr
{-# INLINE ifilter #-}

{- | The 'mapMaybe' function is a version of 'map' which can throw out elements.
  In particular, the functional arguments returns something of type @'Maybe' b@.
  If this is 'Nothing', no element is added on to the result array. If it is
  @'Just' b@, then @b@ is included in the result array.
-}
mapMaybe ::
  forall arr1 arr2 a b.
  ( Contiguous arr1
  , Element arr1 a
  , Contiguous arr2
  , Element arr2 b
  ) =>
  (a -> Maybe b) ->
  arr1 a ->
  arr2 b
mapMaybe f arr = run $ do
  let !sz = size arr
  let go :: Int -> Int -> [b] -> ST s ([b], Int)
      go !ix !numJusts !justs =
        if ix < sz
          then do
            atIx <- indexM arr ix
            case f atIx of
              Nothing -> go (ix + 1) numJusts justs
              Just x -> go (ix + 1) (numJusts + 1) (x : justs)
          else pure (justs, numJusts)
  !(bs, !numJusts) <- go 0 0 []
  !marr <- unsafeFromListReverseMutableN numJusts bs
  unsafeFreeze marr
{-# INLINE mapMaybe #-}

{-# INLINE isTrue #-}
isTrue :: Word8 -> Bool
isTrue 0 = False
isTrue _ = True

{- | The 'catMaybes' function takes a list of 'Maybe's and returns a
  list of all the 'Just' values.
-}
catMaybes ::
  (Contiguous arr, Element arr a, Element arr (Maybe a)) =>
  arr (Maybe a) ->
  arr a
catMaybes = mapMaybe id
{-# INLINE catMaybes #-}

-- | @'replicate' n x@ is an array of length @n@ with @x@ the value of every element.
replicate :: (Contiguous arr, Element arr a) => Int -> a -> arr a
replicate n x = create (replicateMut n x)
{-# INLINE replicate #-}

-- | @'replicateMutM' n act@ performs the action n times, gathering the results.
replicateMutM ::
  (PrimMonad m, Contiguous arr, Element arr a) =>
  Int ->
  m a ->
  m (Mutable arr (PrimState m) a)
replicateMutM len act = do
  marr <- new len
  let go !ix = when (ix < len) $ do
        x <- act
        write marr ix x
        go (ix + 1)
  go 0
  pure marr
{-# INLINE replicateMutM #-}

{- | Create an array from a list. If the given length does
not match the actual length, this function has undefined
behavior.
-}
unsafeFromListN ::
  (Contiguous arr, Element arr a) =>
  -- | length of list
  Int ->
  -- | list
  [a] ->
  arr a
unsafeFromListN n l = create (unsafeFromListMutableN n l)
{-# INLINE unsafeFromListN #-}

unsafeFromListMutableN ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Int ->
  [a] ->
  m (Mutable arr (PrimState m) a)
unsafeFromListMutableN n l = do
  m <- new n
  let go !_ [] = pure m
      go !ix (x : xs) = do
        write m ix x
        go (ix + 1) xs
  go 0 l
{-# INLINE unsafeFromListMutableN #-}

{- | Create a mutable array from a list, reversing the order of
  the elements. If the given length does not match the actual length,
  this function has undefined behavior.
-}
unsafeFromListReverseMutableN ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Int ->
  [a] ->
  m (Mutable arr (PrimState m) a)
unsafeFromListReverseMutableN n l = do
  m <- new n
  let go !_ [] = pure m
      go !ix (x : xs) = do
        write m ix x
        go (ix - 1) xs
  go (n - 1) l
{-# INLINE unsafeFromListReverseMutableN #-}

{- | Create an array from a list, reversing the order of the
elements. If the given length does not match the actual length,
this function has undefined behavior.
-}
unsafeFromListReverseN ::
  (Contiguous arr, Element arr a) =>
  Int ->
  [a] ->
  arr a
unsafeFromListReverseN n l = create (unsafeFromListReverseMutableN n l)
{-# INLINE unsafeFromListReverseN #-}

-- | Map over a mutable array, modifying the elements in place.
mapMutable ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  (a -> a) ->
  Mutable arr (PrimState m) a ->
  m ()
mapMutable f !marr = do
  !sz <- sizeMut marr
  let go !ix = when (ix < sz) $ do
        a <- read marr ix
        write marr ix (f a)
        go (ix + 1)
  go 0
{-# INLINE mapMutable #-}

-- | Strictly map over a mutable array, modifying the elements in place.
mapMutable' ::
  (PrimMonad m, Contiguous arr, Element arr a) =>
  (a -> a) ->
  Mutable arr (PrimState m) a ->
  m ()
mapMutable' f !marr = do
  !sz <- sizeMut marr
  let go !ix = when (ix < sz) $ do
        a <- read marr ix
        let !b = f a
        write marr ix b
        go (ix + 1)
  go 0
{-# INLINE mapMutable' #-}

-- | Map over a mutable array with indices, modifying the elements in place.
imapMutable ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  (Int -> a -> a) ->
  Mutable arr (PrimState m) a ->
  m ()
imapMutable f !marr = do
  !sz <- sizeMut marr
  let go !ix = when (ix < sz) $ do
        a <- read marr ix
        write marr ix (f ix a)
        go (ix + 1)
  go 0
{-# INLINE imapMutable #-}

-- | Strictly map over a mutable array with indices, modifying the elements in place.
imapMutable' ::
  (PrimMonad m, Contiguous arr, Element arr a) =>
  (Int -> a -> a) ->
  Mutable arr (PrimState m) a ->
  m ()
imapMutable' f !marr = do
  !sz <- sizeMut marr
  let go !ix = when (ix < sz) $ do
        a <- read marr ix
        let !b = f ix a
        write marr ix b
        go (ix + 1)
  go 0
{-# INLINE imapMutable' #-}

{- | Map each element of the array to an action, evaluate these
  actions from left to right, and collect the results in a
  new array.
-}
traverseP ::
  ( PrimMonad m
  , Contiguous arr1
  , Element arr1 a
  , Contiguous arr2
  , Element arr2 b
  ) =>
  (a -> m b) ->
  arr1 a ->
  m (arr2 b)
traverseP f !arr = do
  let !sz = size arr
  !marr <- new sz
  let go !ix = when (ix < sz) $ do
        a <- indexM arr ix
        b <- f a
        write marr ix b
        go (ix + 1)
  go 0
  unsafeFreeze marr
{-# INLINE traverseP #-}

{- | Map each element of the array to an action, evaluate these
  actions from left to right, and collect the results in a
  new array.
-}
itraverseP ::
  ( PrimMonad m
  , Contiguous arr1
  , Element arr1 a
  , Contiguous arr2
  , Element arr2 b
  ) =>
  (Int -> a -> m b) ->
  arr1 a ->
  m (arr2 b)
itraverseP f !arr = do
  let !sz = size arr
  !marr <- new sz
  let go !ix = when (ix < sz) $ do
        a <- indexM arr ix
        b <- f ix a
        write marr ix b
        go (ix + 1)
  go 0
  unsafeFreeze marr
{-# INLINE itraverseP #-}

newtype STA v a = STA {_runSTA :: forall s. Mutable v s a -> ST s (v a)}

runSTA :: (Contiguous v, Element v a) => Int -> STA v a -> v a
runSTA !sz (STA m) = runST $ new sz >>= m
{-# INLINE runSTA #-}

{- | Map each element of the array to an action, evaluate these
  actions from left to right, and collect the results.
  For a version that ignores the results, see 'traverse_'.
-}
traverse ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  , Applicative f
  ) =>
  (a -> f b) ->
  arr1 a ->
  f (arr2 b)
traverse f = itraverse (const f)
{-# INLINE traverse #-}

{- | Map each element of the array to an action, evaluate these
  actions from left to right, and ignore the results.
  For a version that doesn't ignore the results, see 'traverse'.
-}
traverse_ ::
  (Contiguous arr, Element arr a, Applicative f) =>
  (a -> f b) ->
  arr a ->
  f ()
traverse_ f = itraverse_ (const f)

{- | Map each element of the array and its index to an action,
  evaluating these actions from left to right.
-}
itraverse ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  , Applicative f
  ) =>
  (Int -> a -> f b) ->
  arr1 a ->
  f (arr2 b)
itraverse f = \arr ->
  let !sz = size arr
      go !ix =
        if ix == sz
          then pure (STA unsafeFreeze)
          else case index# arr ix of
            (# x #) ->
              A.liftA2
                ( \b (STA m) -> STA $ \marr -> do
                    write marr ix b
                    m marr
                )
                (f ix x)
                (go (ix + 1))
   in if sz == 0
        then pure empty
        else runSTA sz <$> go 0
{-# INLINE itraverse #-}

{- | Map each element of the array and its index to an action,
  evaluate these actions from left to right, and ignore the results.
  For a version that doesn't ignore the results, see 'itraverse'.
-}
itraverse_ ::
  (Contiguous arr, Element arr a, Applicative f) =>
  (Int -> a -> f b) ->
  arr a ->
  f ()
itraverse_ f = \arr ->
  let !sz = size arr
      go !ix =
        when (ix < sz) $
          f ix (index arr ix) *> go (ix + 1)
   in go 0
{-# INLINE itraverse_ #-}

{- | 'for' is 'traverse' with its arguments flipped. For a version
  that ignores the results see 'for_'.
-}
for ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  , Applicative f
  ) =>
  arr1 a ->
  (a -> f b) ->
  f (arr2 b)
for = flip traverse
{-# INLINE for #-}

{- | 'for_' is 'traverse_' with its arguments flipped. For a version
  that doesn't ignore the results see 'for'.

  >>> for_ (C.fromList [1..4] :: PrimArray Int) print
  1
  2
  3
  4
-}
for_ ::
  (Contiguous arr, Element arr a, Applicative f) =>
  arr a ->
  (a -> f b) ->
  f ()
for_ = flip traverse_
{-# INLINE for_ #-}

{- | Monadic accumulating strict left fold over the elements on an
array.
-}
mapAccumLM' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 b
  , Element arr2 c
  , Monad m
  ) =>
  (a -> b -> m (a, c)) ->
  a ->
  arr1 b ->
  m (a, arr2 c)
{-# INLINE mapAccumLM' #-}
mapAccumLM' f a0 src = go 0 [] a0
 where
  !sz = size src
  go !ix !xs !acc =
    if ix < sz
      then do
        (!acc', !x) <- f acc (index src ix)
        go (ix + 1) (x : xs) acc'
      else
        let !xs' = unsafeFromListReverseN sz xs
         in pure (acc, xs')

mapAccum' ::
  forall arr1 arr2 a b c.
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 b
  , Element arr2 c
  , Monoid a
  ) =>
  (b -> (a, c)) ->
  arr1 b ->
  (a, arr2 c)
{-# INLINE mapAccum' #-}
mapAccum' f !src = runST $ do
  dst <- new sz
  acc <- go 0 dst mempty
  dst' <- unsafeFreeze dst
  pure (acc, dst')
 where
  !sz = size src
  go :: Int -> Mutable arr2 s c -> a -> ST s a
  go !ix !dst !accA =
    if ix < sz
      then do
        let (!accB, !x) = f (index src ix)
        write dst ix x
        go (ix + 1) dst (accA <> accB)
      else pure accA

{- | Map each element of a structure to a monadic action,
  evaluate these actions from left to right, and collect
  the results. for a version that ignores the results see
  'mapM_'.
-}
mapM ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  , Monad m
  ) =>
  (a -> m b) ->
  arr1 a ->
  m (arr2 b)
mapM f arr =
  let !sz = size arr
   in generateM sz $ \ix -> indexM arr ix >>= f
{-# INLINE mapM #-}

{- | Map each element of a structure to a monadic action,
  evaluate these actions from left to right, and ignore
  the results. For a version that doesn't ignore the results
  see 'mapM'.

  'mapM_' = 'traverse_'
-}
mapM_ ::
  (Contiguous arr, Element arr a, Element arr b, Applicative f) =>
  (a -> f b) ->
  arr a ->
  f ()
mapM_ = traverse_
{-# INLINE mapM_ #-}

{- | 'forM' is 'mapM' with its arguments flipped. For a version that
  ignores its results, see 'forM_'.
-}
forM ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  , Monad m
  ) =>
  arr1 a ->
  (a -> m b) ->
  m (arr2 b)
forM = flip mapM
{-# INLINE forM #-}

{- | 'forM_' is 'mapM_' with its arguments flipped. For a version that
  doesn't ignore its results, see 'forM'.
-}
forM_ ::
  (Contiguous arr, Element arr a, Element arr b, Applicative f) =>
  arr a ->
  (a -> f b) ->
  f ()
forM_ = flip traverse_
{-# INLINE forM_ #-}

{- | Evaluate each action in the structure from left to right
  and collect the results. For a version that ignores the
  results see 'sequence_'.
-}
sequence ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 (f a)
  , Element arr2 a
  , Applicative f
  ) =>
  arr1 (f a) ->
  f (arr2 a)
sequence = traverse id
{-# INLINE sequence #-}

{- | Evaluate each action in the structure from left to right
  and ignore the results. For a version that doesn't ignore
  the results see 'sequence'.
-}
sequence_ ::
  ( Contiguous arr
  , Element arr (f a)
  , Applicative f
  ) =>
  arr (f a) ->
  f ()
sequence_ = foldr (*>) (pure ())
{-# INLINE sequence_ #-}

{- | The sum of a collection of actions, generalizing 'concat'.

  >>> asum (C.fromList ['Just' "Hello", 'Nothing', Just "World"] :: Array String)
  Just "Hello"
-}
asum ::
  ( Contiguous arr
  , Element arr (f a)
  , A.Alternative f
  ) =>
  arr (f a) ->
  f a
asum = foldr (A.<|>) A.empty
{-# INLINE asum #-}

{- | Construct an array of the given length by applying
  the function to each index.
-}
generate ::
  (Contiguous arr, Element arr a) =>
  Int ->
  (Int -> a) ->
  arr a
generate len f = create (generateMutable len f)
{-# INLINE generate #-}

{- | Construct an array of the given length by applying
  the monadic action to each index.
-}
generateM ::
  (Contiguous arr, Element arr a, Monad m) =>
  Int ->
  (Int -> m a) ->
  m (arr a)
{-# INLINE generateM #-}
generateM !sz f =
  let go !ix =
        if ix < sz
          then
            A.liftA2
              ( \b (STA m) -> STA $ \marr -> do
                  write marr ix b
                  m marr
              )
              (f ix)
              (go (ix + 1))
          else pure $ STA unsafeFreeze
   in if sz == 0
        then pure empty
        else runSTA sz <$> go 0

{- | Construct a mutable array of the given length by applying
  the function to each index.
-}
generateMutable ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Int ->
  (Int -> a) ->
  m (Mutable arr (PrimState m) a)
generateMutable len f = generateMutableM len (pure . f)
{-# INLINE generateMutable #-}

{- | Construct a mutable array of the given length by applying
  the monadic action to each index.
-}
generateMutableM ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Int ->
  (Int -> m a) ->
  m (Mutable arr (PrimState m) a)
generateMutableM !len f = do
  marr <- new len
  let go !ix = when (ix < len) $ do
        x <- f ix
        write marr ix x
        go (ix + 1)
  go 0
  pure marr
{-# INLINE generateMutableM #-}

{- | Apply a function @n@ times to a value and construct an array
  where each consecutive element is the result of an additional
  application of this function. The zeroth element is the original value.

  @'iterateN' 5 ('+' 1) 0 = 'fromListN' 5 [0,1,2,3,4]@
-}
iterateN ::
  (Contiguous arr, Element arr a) =>
  Int ->
  (a -> a) ->
  a ->
  arr a
iterateN len f z0 = runST (iterateMutableN len f z0 >>= unsafeFreeze)
{-# INLINE iterateN #-}

{- | Apply a function @n@ times to a value and construct a mutable array
  where each consecutive element is the result of an additional
  application of this function. The zeroth element is the original value.
-}
iterateMutableN ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Int ->
  (a -> a) ->
  a ->
  m (Mutable arr (PrimState m) a)
iterateMutableN len f z0 = iterateMutableNM len (pure . f) z0
{-# INLINE iterateMutableN #-}

{- | Apply a monadic function @n@ times to a value and construct a mutable array
  where each consecutive element is the result of an additional
  application of this function. The zeroth element is the original value.
-}
iterateMutableNM ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Int ->
  (a -> m a) ->
  a ->
  m (Mutable arr (PrimState m) a)
iterateMutableNM !len f z0 = do
  marr <- new len
  -- we are strict in the accumulator because
  -- otherwise we could build up a ton of `f (f (f (f .. (f a))))`
  -- thunks for no reason.
  let go !ix !acc
        | ix <= 0 = write marr ix z0 >> go (ix + 1) z0
        | ix == len = pure ()
        | otherwise = do
            a <- f acc
            write marr ix a
            go (ix + 1) a
  go 0 z0
  pure marr
{-# INLINE iterateMutableNM #-}

-- | Execute the monad action and freeze the resulting array.
create ::
  (Contiguous arr, Element arr a) =>
  (forall s. ST s (Mutable arr s a)) ->
  arr a
create x = run (unsafeFreeze =<< x)
{-# INLINE create #-}

-- | Execute the monadic action and freeze the resulting array.
createT ::
  (Contiguous arr, Element arr a, Traversable f) =>
  (forall s. ST s (f (Mutable arr s a))) ->
  f (arr a)
createT p = runST (Prelude.mapM unsafeFreeze =<< p)
{-# INLINE createT #-}

{- | Construct an array by repeatedly applying a generator
  function to a seed. The generator function yields 'Just' the
  next element and the new seed or 'Nothing' if there are no more
  elements.

>>> unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1) 10
    <10,9,8,7,6,5,4,3,2,1>
-}

-- Unfortunately, because we don't know ahead of time when to stop,
-- we need to construct a list and then turn it into an array.
unfoldr ::
  (Contiguous arr, Element arr a) =>
  (b -> Maybe (a, b)) ->
  b ->
  arr a
unfoldr f z0 = create (unfoldrMutable f z0)
{-# INLINE unfoldr #-}

{- | Construct a mutable array by repeatedly applying a generator
  function to a seed. The generator function yields 'Just' the
  next element and the new seed or 'Nothing' if there are no more
  elements.

>>> unfoldrMutable (\n -> if n == 0 then Nothing else Just (n,n-1) 10
    <10,9,8,7,6,5,4,3,2,1>
-}

-- Unfortunately, because we don't know ahead of time when to stop,
-- we need to construct a list and then turn it into an array.
unfoldrMutable ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  (b -> Maybe (a, b)) ->
  b ->
  m (Mutable arr (PrimState m) a)
unfoldrMutable f z0 = do
  let go !sz s !xs = case f s of
        Nothing -> pure (sz, xs)
        Just (x, s') -> go (sz + 1) s' (x : xs)
  (sz, xs) <- go 0 z0 []
  unsafeFromListReverseMutableN sz xs
{-# INLINE unfoldrMutable #-}

{- | Construct an array with at most n elements by repeatedly
  applying the generator function to a seed. The generator function
  yields 'Just' the next element and the new seed or 'Nothing' if
  there are no more elements.
-}
unfoldrN ::
  (Contiguous arr, Element arr a) =>
  Int ->
  (b -> Maybe (a, b)) ->
  b ->
  arr a
unfoldrN maxSz f z0 = create (unfoldrMutableN maxSz f z0)
{-# INLINE unfoldrN #-}

{- | Construct a mutable array with at most n elements by repeatedly
  applying the generator function to a seed. The generator function
  yields 'Just' the next element and the new seed or 'Nothing' if
  there are no more elements.
-}
unfoldrMutableN ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Int ->
  (b -> Maybe (a, b)) ->
  b ->
  m (Mutable arr (PrimState m) a)
unfoldrMutableN !maxSz f z0 = do
  m <- new maxSz
  let go !ix s =
        if ix < maxSz
          then case f s of
            Nothing -> pure ix
            Just (x, s') -> do
              write m ix x
              go (ix + 1) s'
          else pure ix
  sz <- go 0 z0
  shrink m sz
{-# INLINE unfoldrMutableN #-}

-- | Convert an array to a list.
toList ::
  (Contiguous arr, Element arr a) =>
  arr a ->
  [a]
toList arr = build (\c n -> foldr c n arr)
{-# INLINE toList #-}

-- | Convert a mutable array to a list.

-- I don't think this can be expressed in terms of foldr/build,
-- so we just loop through the array.
toListMutable ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Mutable arr (PrimState m) a ->
  m [a]
toListMutable marr = do
  sz <- sizeMut marr
  let go !ix !acc =
        if ix >= 0
          then do
            x <- read marr ix
            go (ix - 1) (x : acc)
          else pure acc
  go (sz - 1) []
{-# INLINE toListMutable #-}

{- | Given an 'Int' that is representative of the length of
  the list, convert the list into a mutable array of the
  given length.

  /Note/: calls 'error' if the given length is incorrect.
-}
fromListMutableN ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Int ->
  [a] ->
  m (Mutable arr (PrimState m) a)
fromListMutableN len vs = do
  marr <- new len
  let go [] !ix =
        if ix == len
          then pure ()
          else error "Data.Primitive.Contiguous.fromListN: list length less than specified size."
      go (a : as) !ix =
        if ix < len
          then do
            write marr ix a
            go as (ix + 1)
          else error "Data.Primitive.Contiguous.fromListN: list length greater than specified size."
  go vs 0
  pure marr
{-# INLINE fromListMutableN #-}

-- | Convert a list into a mutable array of the given length.
fromListMutable ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  [a] ->
  m (Mutable arr (PrimState m) a)
fromListMutable xs = fromListMutableN (length xs) xs
{-# INLINE fromListMutable #-}

{- | Given an 'Int' that is representative of the length of
  the list, convert the list into a mutable array of the
  given length.

  /Note/: calls 'error' if the given length is incorrect.
-}
fromListN ::
  (Contiguous arr, Element arr a) =>
  Int ->
  [a] ->
  arr a
fromListN len vs = create (fromListMutableN len vs)
{-# INLINE fromListN #-}

-- | Convert a list into an array.
fromList ::
  (Contiguous arr, Element arr a) =>
  [a] ->
  arr a
fromList vs = create (fromListMutable vs)
{-# INLINE fromList #-}

-- | Modify the elements of a mutable array in-place.
modify ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  (a -> a) ->
  Mutable arr (PrimState m) a ->
  m ()
modify f marr = do
  !sz <- sizeMut marr
  let go !ix = when (ix < sz) $ do
        x <- read marr ix
        write marr ix (f x)
        go (ix + 1)
  go 0
{-# INLINE modify #-}

-- | Strictly modify the elements of a mutable array in-place.
modify' ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  (a -> a) ->
  Mutable arr (PrimState m) a ->
  m ()
modify' f marr = do
  !sz <- sizeMut marr
  let go !ix = when (ix < sz) $ do
        x <- read marr ix
        let !y = f x
        write marr ix y
        go (ix + 1)
  go 0
{-# INLINE modify' #-}

{- | Yield an array of the given length containing the values
  @x, 'succ' x, 'succ' ('succ' x)@ etc.
-}
enumFromN ::
  (Contiguous arr, Element arr a, Enum a) =>
  a ->
  Int ->
  arr a
enumFromN z0 sz = create (enumFromMutableN z0 sz)
{-# INLINE enumFromN #-}

{- | Yield a mutable array of the given length containing the values
  @x, 'succ' x, 'succ' ('succ' x)@ etc.
-}
enumFromMutableN ::
  (Contiguous arr, Element arr a, PrimMonad m, Enum a) =>
  a ->
  Int ->
  m (Mutable arr (PrimState m) a)
enumFromMutableN z0 !sz = do
  m <- new sz
  let go !ix z =
        if ix < sz
          then do
            write m ix z
            go (ix + 1) (succ z)
          else pure m
  go 0 z0
{-# INLINE enumFromMutableN #-}

{- | Lift an accumulating hash function over the elements of the array,
  returning the final accumulated hash.
-}
liftHashWithSalt ::
  (Contiguous arr, Element arr a) =>
  (Int -> a -> Int) ->
  Int ->
  arr a ->
  Int
liftHashWithSalt f s0 arr = go 0 s0
 where
  sz = size arr
  go !ix !s =
    if ix < sz
      then
        let !(# x #) = index# arr ix
         in go (ix + 1) (f s x)
      else hashIntWithSalt s ix
{-# INLINE liftHashWithSalt #-}

-- | Reverse the elements of an array.
reverse ::
  (Contiguous arr, Element arr a) =>
  arr a ->
  arr a
reverse arr = run $ do
  marr <- new (size arr)
  copy marr 0 (toSlice arr)
  reverseMutable marr
  unsafeFreeze marr
{-# INLINE reverse #-}

-- | Reverse the elements of a mutable array, in-place.
reverseMutable ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Mutable arr (PrimState m) a ->
  m ()
reverseMutable marr = do
  !sz <- sizeMut marr
  reverseSlice marr 0 (sz - 1)
{-# INLINE reverseMutable #-}

-- | Reverse the elements of a slice of a mutable array, in-place.
reverseSlice ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Mutable arr (PrimState m) a ->
  -- | start index
  Int ->
  -- | end index
  Int ->
  m ()
reverseSlice !marr !start !end = do
  let go !s !e =
        if s >= e
          then pure ()
          else do
            tmp <- read marr s
            write marr s =<< read marr e
            write marr e tmp
            go (s + 1) (e - 1)
  go start end
{-# INLINE reverseSlice #-}

{- | This function does not behave deterministically. Optimization level and
inlining can affect its results. However, the one thing that can be counted
on is that if it returns 'True', the two immutable arrays are definitely the
same. This is useful as shortcut for equality tests. However, keep in mind
that a result of 'False' tells us nothing about the arguments.
-}
same :: (ContiguousU arr) => arr a -> arr a -> Bool
same a b =
  isTrue#
    ( sameMutableArrayArray#
        (unsafeCoerce# (unlift a) :: MutableArrayArray# s)
        (unsafeCoerce# (unlift b) :: MutableArrayArray# s)
    )

hashIntWithSalt :: Int -> Int -> Int
hashIntWithSalt salt x = salt `combine` x
{-# INLINE hashIntWithSalt #-}

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
{-# INLINE combine #-}

-- | Does the element occur in the structure?
elem :: (Contiguous arr, Element arr a, Eq a) => a -> arr a -> Bool
elem a !arr =
  let !sz = size arr
      go !ix
        | ix < sz = case index# arr ix of
            !(# x #) ->
              if a == x
                then True
                else go (ix + 1)
        | otherwise = False
   in go 0
{-# INLINE elem #-}

-- | The largest element of a structure.
maximum :: (Contiguous arr, Element arr a, Ord a) => arr a -> Maybe a
maximum = maximumBy compare
{-# INLINE maximum #-}

-- | The least element of a structure.
minimum :: (Contiguous arr, Element arr a, Ord a) => arr a -> Maybe a
minimum = minimumBy compare
{-# INLINE minimum #-}

{- | The largest element of a structure with respect to the
  given comparison function.
-}
maximumBy ::
  (Contiguous arr, Element arr a) =>
  (a -> a -> Ordering) ->
  arr a ->
  Maybe a
maximumBy f arr =
  let !sz = size arr
      go !ix o =
        if ix < sz
          then case index# arr ix of
            !(# x #) -> go (ix + 1) (case f x o of GT -> x; _ -> o)
          else o
   in if sz == 0
        then Nothing
        else Just (go 0 (index arr 0))
{-# INLINE maximumBy #-}

{- | The least element of a structure with respect to the
  given comparison function.
-}
minimumBy ::
  (Contiguous arr, Element arr a) =>
  (a -> a -> Ordering) ->
  arr a ->
  Maybe a
minimumBy f arr =
  let !sz = size arr
      go !ix o =
        if ix < sz
          then case index# arr ix of
            !(# x #) -> go (ix + 1) (case f x o of GT -> o; _ -> x)
          else o
   in if sz == 0
        then Nothing
        else Just (go 0 (index arr 0))
{-# INLINE minimumBy #-}

{- | 'find' takes a predicate and an array, and returns the leftmost
  element of the array matching the prediate, or 'Nothing' if there
  is no such element.
-}
find ::
  (Contiguous arr, Element arr a) =>
  (a -> Bool) ->
  arr a ->
  Maybe a
find p = coerce . (foldMap (\x -> if p x then Just (First x) else Nothing))
{-# INLINE find #-}

{- | 'findIndex' takes a predicate and an array, and returns the index of
  the leftmost element of the array matching the prediate, or 'Nothing'
  if there is no such element.
-}
findIndex ::
  (Contiguous arr, Element arr a) =>
  (a -> Bool) ->
  arr a ->
  Maybe Int
findIndex p xs = loop 0
 where
  loop i
    | i < size xs = if p (index xs i) then Just i else loop (i + 1)
    | otherwise = Nothing
{-# INLINE findIndex #-}

-- | Swap the elements of the mutable array at the given indices.
swap ::
  (Contiguous arr, Element arr a, PrimMonad m) =>
  Mutable arr (PrimState m) a ->
  Int ->
  Int ->
  m ()
swap !marr !ix1 !ix2 = do
  atIx1 <- read marr ix1
  atIx2 <- read marr ix2
  write marr ix1 atIx2
  write marr ix2 atIx1
{-# INLINE swap #-}

{- | Extracts from an array of 'Either' all the 'Left' elements.
All the 'Left' elements are extracted in order.
-}
lefts ::
  forall arr a b.
  ( Contiguous arr
  , Element arr a
  , Element arr (Either a b)
  ) =>
  arr (Either a b) ->
  arr a
lefts !arr = create $ do
  let !sz = size arr
      go :: Int -> [a] -> Int -> ST s (Int, [a])
      go !ix !as !acc =
        if ix < sz
          then do
            indexM arr ix >>= \case
              Left a -> go (ix + 1) (a : as) (acc + 1)
              Right _ -> go (ix + 1) as acc
          else pure (acc, as)
  (len, as) <- go 0 [] 0
  unsafeFromListReverseMutableN len as
{-# INLINE lefts #-}

{- | Extracts from an array of 'Either' all the 'Right' elements.
All the 'Right' elements are extracted in order.
-}
rights ::
  forall arr a b.
  ( Contiguous arr
  , Element arr b
  , Element arr (Either a b)
  ) =>
  arr (Either a b) ->
  arr b
rights !arr = create $ do
  let !sz = size arr
      go :: Int -> [b] -> Int -> ST s (Int, [b])
      go !ix !bs !acc =
        if ix < sz
          then do
            indexM arr ix >>= \case
              Left _ -> go (ix + 1) bs acc
              Right b -> go (ix + 1) (b : bs) (acc + 1)
          else pure (acc, bs)
  (len, bs) <- go 0 [] 0
  unsafeFromListReverseMutableN len bs
{-# INLINE rights #-}

{- | Partitions an array of 'Either' into two arrays.
All the 'Left' elements are extracted, in order, to the first
component of the output. Similarly the 'Right' elements are extracted
to the second component of the output.
-}
partitionEithers ::
  forall arr a b.
  ( Contiguous arr
  , Element arr a
  , Element arr b
  , Element arr (Either a b)
  ) =>
  arr (Either a b) ->
  (arr a, arr b)
partitionEithers !arr = runST $ do
  let !sz = size arr
      go :: Int -> [a] -> [b] -> Int -> Int -> ST s (Int, Int, [a], [b])
      go !ix !as !bs !accA !accB =
        if ix < sz
          then do
            indexM arr ix >>= \case
              Left a -> go (ix + 1) (a : as) bs (accA + 1) accB
              Right b -> go (ix + 1) as (b : bs) accA (accB + 1)
          else pure (accA, accB, as, bs)
  (lenA, lenB, as, bs) <- go 0 [] [] 0 0
  arrA <- unsafeFreeze =<< unsafeFromListReverseMutableN lenA as
  arrB <- unsafeFreeze =<< unsafeFromListReverseMutableN lenB bs
  pure (arrA, arrB)
{-# INLINE partitionEithers #-}

{- | 'scanl' is similar to 'foldl', but returns an array of
  successive reduced values from the left:

  > scanl f z [x1, x2, ...] = [z, f z x1, f (f z x1) x2, ...]

  Note that

  > last (toList (scanl f z xs)) == foldl f z xs.
-}
scanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
scanl f = iscanl (const f)
{-# INLINE scanl #-}

{- | A variant of 'scanl' whose function argument takes the current
  index as an argument.
-}
iscanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (Int -> b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
iscanl f q as = internalScanl (size as + 1) f q as
{-# INLINE iscanl #-}

-- | A strictly accumulating version of 'scanl'.
scanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
scanl' f = iscanl' (const f)
{-# INLINE scanl' #-}

-- | A strictly accumulating version of 'iscanl'.
iscanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (Int -> b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
iscanl' f !q as = internalScanl' (size as + 1) f q as
{-# INLINE iscanl' #-}

-- Internal only. The first argument is the size of the array
-- argument. This function helps prevent duplication.
internalScanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  Int ->
  (Int -> b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
internalScanl !sz f !q as = create $ do
  !marr <- new sz
  let go !ix acc = when (ix < sz) $ do
        write marr ix acc
        x <- indexM as ix
        go (ix + 1) (f ix acc x)
  go 0 q
  pure marr
{-# INLINE internalScanl #-}

-- Internal only. The first argument is the size of the array
-- argument. This function helps prevent duplication.
internalScanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  Int ->
  (Int -> b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
internalScanl' !sz f !q as = create $ do
  !marr <- new sz
  let go !ix !acc = when (ix < sz) $ do
        write marr ix acc
        x <- indexM as ix
        go (ix + 1) (f ix acc x)
  go 0 q
  pure marr
{-# INLINE internalScanl' #-}

{- | A prescan.

  @prescanl f z = init . scanl f z@

  Example: @prescanl (+) 0 \<1,2,3,4\> = \<0,1,3,6\>@
-}
prescanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
prescanl f = iprescanl (const f)
{-# INLINE prescanl #-}

{- | A variant of 'prescanl' where the function argument takes
  the current index of the array as an additional argument.
-}
iprescanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (Int -> b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
iprescanl f q as = internalScanl (size as) f q as
{-# INLINE iprescanl #-}

-- | Like 'prescanl', but with a strict accumulator.
prescanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
prescanl' f = iprescanl (const f)
{-# INLINE prescanl' #-}

-- | Like 'iprescanl', but with a strict accumulator.
iprescanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (Int -> b -> a -> b) ->
  b ->
  arr1 a ->
  arr2 b
iprescanl' f !q as = internalScanl' (size as) f q as
{-# INLINE iprescanl' #-}

{- | 'zipWith' generalises 'zip' by zipping with the function
  given as the first argument, instead of a tupling function.
  For example, 'zipWith' (+) is applied to two arrays to produce
  an array of the corresponding sums.
-}
zipWith ::
  ( Contiguous arr1
  , Contiguous arr2
  , Contiguous arr3
  , Element arr1 a
  , Element arr2 b
  , Element arr3 c
  ) =>
  (a -> b -> c) ->
  arr1 a ->
  arr2 b ->
  arr3 c
zipWith f = izipWith (\_ a b -> f a b)
{-# INLINE zipWith #-}

-- | Variant of 'zipWith' that provides the index of each pair of elements.
izipWith ::
  ( Contiguous arr1
  , Contiguous arr2
  , Contiguous arr3
  , Element arr1 a
  , Element arr2 b
  , Element arr3 c
  ) =>
  (Int -> a -> b -> c) ->
  arr1 a ->
  arr2 b ->
  arr3 c
izipWith f as bs = create $ do
  let !sz = min (size as) (size bs)
  !marr <- new sz
  let go !ix = when (ix < sz) $ do
        a <- indexM as ix
        b <- indexM bs ix
        let !g = f ix a b
        write marr ix g
        go (ix + 1)
  go 0
  pure marr
{-# INLINE izipWith #-}

{- | Variant of 'zipWith' that accepts an accumulator, performing a lazy
right fold over both arrays.
-}
foldrZipWith ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (a -> b -> c -> c) ->
  c ->
  arr1 a ->
  arr2 b ->
  c
foldrZipWith f = ifoldrZipWith (\_ x y c -> f x y c)
{-# INLINE foldrZipWith #-}

{- | Variant of 'zipWith' that accepts an accumulator, performing a strict
left monadic fold over both arrays.
-}
foldlZipWithM' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  , Monad m
  ) =>
  (c -> a -> b -> m c) ->
  c ->
  arr1 a ->
  arr2 b ->
  m c
foldlZipWithM' f = ifoldlZipWithM' (\_ x y c -> f x y c)
{-# INLINE foldlZipWithM' #-}

-- | Variant of 'foldrZipWith' that provides the index of each pair of elements.
ifoldrZipWith ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (Int -> a -> b -> c -> c) ->
  c ->
  arr1 a ->
  arr2 b ->
  c
ifoldrZipWith f z = \arr1 arr2 ->
  let !sz = min (size arr1) (size arr2)
      go !ix =
        if sz > ix
          then case index# arr1 ix of
            (# x #) -> case index# arr2 ix of
              (# y #) -> f ix x y (go (ix + 1))
          else z
   in go 0
{-# INLINE ifoldrZipWith #-}

foldlZipWith' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (c -> a -> b -> c) ->
  c ->
  arr1 a ->
  arr2 b ->
  c
foldlZipWith' f = ifoldlZipWith' (\_ x y c -> f x y c)
{-# INLINE foldlZipWith' #-}

ifoldlZipWith' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (Int -> c -> a -> b -> c) ->
  c ->
  arr1 a ->
  arr2 b ->
  c
ifoldlZipWith' f !z !arr1 !arr2 =
  let !sz = min (size arr1) (size arr2)
      go !ix !acc =
        if ix == sz
          then acc
          else case index# arr1 ix of
            (# x #) -> case index# arr2 ix of
              (# y #) -> go (ix + 1) (f ix acc x y)
   in go 0 z
{-# INLINE ifoldlZipWith' #-}

-- | Variant of 'foldlZipWithM\'' that provides the index of each pair of elements.
ifoldlZipWithM' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  , Monad m
  ) =>
  (Int -> c -> a -> b -> m c) ->
  c ->
  arr1 a ->
  arr2 b ->
  m c
ifoldlZipWithM' f z = \arr1 arr2 ->
  let !sz = min (size arr1) (size arr2)
      go !ix !acc =
        if sz > ix
          then case index# arr1 ix of
            (# x #) -> case index# arr2 ix of
              (# y #) -> do
                acc' <- f ix acc x y
                go (ix + 1) acc'
          else pure acc
   in go 0 z
{-# INLINE ifoldlZipWithM' #-}

{- | 'zip' takes two arrays and returns an array of
  corresponding pairs.

  > zip [1, 2] ['a', 'b'] = [(1, 'a'), (2, 'b')]

  If one input array is shorter than the other, excess
  elements of the longer array are discarded:

  > zip [1] ['a', 'b'] = [(1, 'a')]
  > zip [1, 2] ['a'] = [(1, 'a')]
-}
zip ::
  ( Contiguous arr1
  , Contiguous arr2
  , Contiguous arr3
  , Element arr1 a
  , Element arr2 b
  , Element arr3 (a, b)
  ) =>
  arr1 a ->
  arr2 b ->
  arr3 (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

{- | Replace all locations in the input with the same value.

  Equivalent to Data.Functor.'Data.Functor.<$'.
-}
(<$) ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 b
  , Element arr2 a
  ) =>
  a ->
  arr1 b ->
  arr2 a
a <$ barr = create (replicateMut (size barr) a)
{-# INLINE (<$) #-}

{- | Sequential application.

  Equivalent to Control.Applicative.'Control.Applicative.<*>'.
-}
ap ::
  ( Contiguous arr1
  , Contiguous arr2
  , Contiguous arr3
  , Element arr1 (a -> b)
  , Element arr2 a
  , Element arr3 b
  ) =>
  arr1 (a -> b) ->
  arr2 a ->
  arr3 b
ap fs xs = create $ do
  marr <- new (szfs * szxs)
  let go1 !ix = when (ix < szfs) $ do
        f <- indexM fs ix
        go2 (ix * szxs) f 0
        go1 (ix + 1)
      go2 !off f !j = when (j < szxs) $ do
        x <- indexM xs j
        write marr (off + j) (f x)
        go2 off f (j + 1)
  go1 0
  pure marr
 where
  !szfs = size fs
  !szxs = size xs
{-# INLINE ap #-}

all :: (Contiguous arr, Element arr a) => (a -> Bool) -> arr a -> Bool
all f = foldr (\x acc -> f x && acc) True
{-# INLINE all #-}

any :: (Contiguous arr, Element arr a) => (a -> Bool) -> arr a -> Bool
any f = foldr (\x acc -> f x || acc) False
{-# INLINE any #-}
