{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | TODO better intro
--
-- WARNING: Some mutable arrays (e.g. 'SmallArray#' and 'ByteArray#') can be
-- shrunk in-place. If such an array is shrunk, slices that are backed by that
-- array may become invalidated.
--
-- WARNING: Even more weirdly, 'unsafeFreeze' really is allowed to be unsafe,
-- but it's even more unsafe than usual. The slice must be the only live
-- reference to the entire backing mutable array. (or thereabouts, I suppose you
-- could unsafe freeze two slices with the same backing)
module Data.Primitive.Contiguous.Slice
  ( Slice
  , fromArray
  , MutableSlice
  ) where

import Prelude hiding (length,read)

import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Monad.ST (runST)
import Data.Primitive.Contiguous (Contiguous(..))

data Slice arr a = Slice
  { array :: !(arr a)
  , offset :: {-# UNPACK #-} !Int
  , length :: {-# UNPACK #-} !Int
  }

fromArray :: (Contiguous arr, Element arr a) => arr a -> Slice arr a
{-# INLINE fromArray #-}
fromArray array = Slice{array,offset=0,length=size array}

data MutableSlice arr s a = MutableSlice
  { array :: !(Mutable arr s a)
  , offset :: {-# UNPACK #-} !Int
  , length :: {-# UNPACK #-} !Int
  }

fromMutableArray :: (PrimMonad m, Contiguous arr, Element arr a)
  => Mutable arr (PrimState m) a -> m (MutableSlice arr (PrimState m) a)
{-# INLINE fromMutableArray #-}
fromMutableArray array = do
  length <- sizeMutable array
  pure MutableSlice{array,offset=0,length}

instance (Contiguous arr) => Contiguous (Slice arr) where
  type Mutable (Slice arr) = MutableSlice arr
  type Element (Slice arr) = Element arr
  empty = Slice empty 0 0
  null Slice{length} = length == 0
  new length = fromMutableArray =<< new length
  replicateMutable length z = fromMutableArray =<< replicateMutable length z
  index Slice{array,offset} i = index array (i + offset)
  index# Slice{array,offset} i = index# array (i + offset)
  indexM Slice{array,offset} i = indexM array (i + offset)
  read MutableSlice{array,offset} i = read array (i + offset)
  write MutableSlice{array,offset} i x = write array (i + offset) x
  resize src@MutableSlice{array,offset,length} len'
    | len' <= length = pure MutableSlice{array,offset,length=len'}
    | otherwise = do
      dst <- new len'
      copyMutable dst 0 src 0 length
      pure dst
  size Slice{length} = length
  sizeMutable MutableSlice{length} = pure length
  unsafeFreeze MutableSlice{array,offset,length} = do
    array' <- unsafeFreeze array
    pure Slice{array=array',offset,length}
  -- if the length is longer than the slice, the output array is truncated
  freeze MutableSlice{array,offset,length} off' len'
    = fromArray <$> freeze array (offset + off') (min length len')
  thaw Slice{array,offset,length} off' len' = do
    fromMutableArray =<< thaw array (offset + off') (min length len')
  copy dst dstOff src srcOff len = do
    let MutableSlice{array=arrD,offset=offD} = dst
        Slice{array=arrS,offset=offS} = src
    copy arrD (dstOff + offD) arrS (srcOff + offS) len
  copyMutable dst dstOff src srcOff len = do
    let MutableSlice{array=arrD,offset=offD} = dst
        MutableSlice{array=arrS,offset=offS} = src
    copyMutable arrD (dstOff + offD) arrS (srcOff + offS) len
  clone Slice{array,offset,length} off len'
    = Slice{array,offset=offset+off,length=min length len'}
  cloneMutable MutableSlice{array,offset,length} off len' = do
    fromMutableArray =<< cloneMutable array (offset + off) (min length len')
  equals Slice{array=a,offset=oA,length} Slice{array=b,offset=oB,length=lenB}
    = length == lenB && loop 0 oA oB
    where
    loop !i !iA !iB =
      if i == length then True
      else index a iA == index b iB && loop (i+1) (iA+1) (iB+1)
  equalsMutable MutableSlice{array=arrA,offset=offA,length=lenA}
                MutableSlice{array=arrB,offset=offB,length=lenB}
    =  arrA `equalsMutable` arrB
    && offA == offB
    && lenA == lenB
  -- unlift
  -- lift
  singleton x = fromArray $ singleton x
  doubleton x y = fromArray $ doubleton x y
  tripleton x y z = fromArray $ tripleton x y z
  quadrupleton x y z w = fromArray $ quadrupleton x y z w
  rnf Slice{array} = rnf array `seq` ()
  run = runST
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateMutable #-}
  {-# inline index #-}
  {-# inline index# #-}
  {-# inline indexM #-}
  {-# inline read #-}
  {-# inline write #-}
  {-# inline resize #-}
  {-# inline size #-}
  {-# inline sizeMutable #-}
  {-# inline unsafeFreeze #-}
  {-# inline freeze #-}
  {-# inline thaw #-}
  {-# inline copy #-}
  {-# inline copyMutable #-}
  {-# inline clone #-}
  {-# inline cloneMutable #-}
  {-# inline equals #-}
  {-# inline equalsMutable #-}
  -- {-# inline unlift #-}
  -- {-# inline lift #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline quadrupleton #-}
  {-# inline rnf #-}
  {-# inline run #-}

-- TODO I think I'll want to add slice and sliceMutable to Contiguous, which
-- means moving this module into the big one, or making a
-- Data.Primitive.Contiguous.Class module

-- TODO freezeAll, thawAll, copyAll, copyMutableAll, cloneAll, cloneMutableAll

-- This interface is definitely strange for slices.
-- It seems like if we're going to combine array slicing and coniguous, the
-- whole class needs to be altered.

-- TODO
-- Contiguous ByteArray where Element = (~ Word8)

I think the thing that makes the most sense is to create a "competing" library to contiguous.
It can have an extremely breaking API that is oriented better to allow slicing.
We can keep using contiguous, and progressively use the newer interface, since these APIs are really only classes rather than data types.
The competitor would have `class BareArray` and `class ArraySlice`,
  an easy way to turn bare arrays into slices,
  slices would have all the general-purpose stuff,
  and bare arrays would be the only one with operations like resize.
