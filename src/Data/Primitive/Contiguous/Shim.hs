{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Data.Primitive.Contiguous.Shim
  ( errorThunk
  , resizeArray
  , resizeSmallArray
  , replicateSmallMutableArray
  , resizeUnliftedArray
  , replicateMutablePrimArray
  , clonePrimArrayShim
  , cloneMutablePrimArrayShim
  , freezePrimArrayShim
  ) where

import Control.Monad (when)
import Control.Monad.ST.Run (runPrimArrayST)
import Data.Primitive hiding (fromList,fromListN)
import Data.Primitive.Unlifted.Array
import Prelude hiding (map,all,any,foldr,foldMap,traverse,read,filter,replicate,null,reverse,foldl,foldr,zip,zipWith,scanl,(<$),elem,maximum,minimum,mapM,mapM_,sequence,sequence_)

import Data.Primitive.Unlifted.Class (PrimUnlifted)
import Control.Monad.Primitive (PrimState, PrimMonad(..))


errorThunk :: a
errorThunk = error "Contiguous typeclass: unitialized element"
{-# noinline errorThunk #-}

resizeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m (MutableArray (PrimState m) a)
resizeArray !src !sz = do
  dst <- newArray sz errorThunk
  copyMutableArray dst 0 src 0 (min sz (sizeofMutableArray src))
  pure dst
{-# inline resizeArray #-}

resizeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> m (SmallMutableArray (PrimState m) a)
resizeSmallArray !src !sz = do
  dst <- newSmallArray sz errorThunk
  copySmallMutableArray dst 0 src 0 (min sz (sizeofSmallMutableArray src))
  pure dst
{-# inline resizeSmallArray #-}

replicateSmallMutableArray :: (PrimMonad m)
  => Int
  -> a
  -> m (SmallMutableArray (PrimState m) a)
replicateSmallMutableArray len a = do
  marr <- newSmallArray len errorThunk
  let go !ix = when (ix < len) $ do
        writeSmallArray marr ix a
        go (ix + 1)
  go 0
  pure marr
{-# inline replicateSmallMutableArray #-}

resizeUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> m (MutableUnliftedArray (PrimState m) a)
resizeUnliftedArray !src !sz = do
  dst <- unsafeNewUnliftedArray sz
  copyMutableUnliftedArray dst 0 src 0 (min sz (sizeofMutableUnliftedArray src))
  pure dst
{-# inline resizeUnliftedArray #-}

replicateMutablePrimArray :: (PrimMonad m, Prim a)
  => Int -- ^ length
  -> a -- ^ element
  -> m (MutablePrimArray (PrimState m) a)
replicateMutablePrimArray len a = do
  marr <- newPrimArray len
  setPrimArray marr 0 len a
  pure marr
{-# inline replicateMutablePrimArray #-}

clonePrimArrayShim :: Prim a => PrimArray a -> Int -> Int -> PrimArray a
clonePrimArrayShim !arr !off !len = runPrimArrayST $ do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  unsafeFreezePrimArray marr
{-# inline clonePrimArrayShim #-}

cloneMutablePrimArrayShim :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Int -> m (MutablePrimArray (PrimState m) a)
cloneMutablePrimArrayShim !arr !off !len = do
  marr <- newPrimArray len
  copyMutablePrimArray marr 0 arr off len
  pure marr
{-# inline cloneMutablePrimArrayShim #-}

freezePrimArrayShim :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Int -> m (PrimArray a)
freezePrimArrayShim !src !off !len = do
  dst <- newPrimArray len
  copyMutablePrimArray dst 0 src off len
  unsafeFreezePrimArray dst
{-# inline freezePrimArrayShim #-}
