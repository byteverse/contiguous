{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Data.Primitive.Contiguous.Shim
  ( errorThunk
  , resizeArray
  , resizeUnliftedArray
  , replicateMutablePrimArray
  ) where

import Data.Primitive
import Data.Primitive.Unlifted.Array
import Prelude hiding (all, any, elem, filter, foldMap, foldl, foldr, map, mapM, mapM_, maximum, minimum, null, read, replicate, reverse, scanl, sequence, sequence_, traverse, zip, zipWith, (<$))

import Control.Monad.Primitive (PrimMonad (..), PrimState)
import Data.Primitive.Unlifted.Class (PrimUnlifted)

errorThunk :: a
errorThunk = error "Contiguous typeclass: unitialized element"
{-# NOINLINE errorThunk #-}

resizeArray :: (PrimMonad m) => MutableArray (PrimState m) a -> Int -> m (MutableArray (PrimState m) a)
resizeArray !src !sz = do
  let !srcSz = sizeofMutableArray src
  case compare sz srcSz of
    EQ -> pure src
    LT -> cloneMutableArray src 0 sz
    GT -> do
      dst <- newArray sz errorThunk
      copyMutableArray dst 0 src 0 sz
      pure dst
{-# INLINE resizeArray #-}

resizeUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> m (MutableUnliftedArray (PrimState m) a)
resizeUnliftedArray !src !sz = do
  let !srcSz = sizeofMutableUnliftedArray src
  case compare sz srcSz of
    EQ -> pure src
    LT -> cloneMutableUnliftedArray src 0 sz
    GT -> do
      dst <- unsafeNewUnliftedArray sz
      copyMutableUnliftedArray dst 0 src 0 sz
      pure dst
{-# INLINE resizeUnliftedArray #-}

replicateMutablePrimArray ::
  (PrimMonad m, Prim a) =>
  -- | length
  Int ->
  -- | element
  a ->
  m (MutablePrimArray (PrimState m) a)
replicateMutablePrimArray len a = do
  marr <- newPrimArray len
  setPrimArray marr 0 len a
  pure marr
{-# INLINE replicateMutablePrimArray #-}
