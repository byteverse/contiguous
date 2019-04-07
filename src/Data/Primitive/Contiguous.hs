{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language UnboxedTuples #-}

-- | The contiguous typeclass parameterises over a contiguous array type.
--   This allows us to have a common API to a number of contiguous
--   array types and their mutable counterparts.
module Data.Primitive.Contiguous
  (
    -- * Classes
    Contiguous(..)
  , Always

    -- * Construction
  , append
  , convert

    -- * Maps
  , map
  , map'
  , imap
  , imap'
  , mapMutable'
  , imapMutable'

    -- * Folds
  , foldr
  , foldr'
  , foldl
  , foldl'
  , foldMap
  , foldMap'
  , foldlMap'

  , ifoldl'
  , ifoldlMap'
  , ifoldlMap1'
  , foldlM'

    -- * Traversals
  , traverse
  , traverse_
  , itraverse
  , itraverse_
  , traverseP

    -- * Filters
  , filter
  , ifilter

    -- * Conversions
    -- ** Lists
  , unsafeFromListN
  , unsafeFromListReverseN

    -- * Hashing
  , liftHashWithSalt

    -- * Misc.
  , same
  ) where

import Prelude hiding (map,foldr,foldMap,traverse,read,filter)
import Control.Monad.ST (runST,ST)
import Control.Monad.Primitive
import Control.Applicative (liftA2)
import Data.Bits (xor)
import Data.Kind (Type)
import Data.Primitive
import Data.Semigroup (Semigroup,(<>))
import Data.Word (Word8)
import GHC.Exts (MutableArrayArray#,ArrayArray#,Constraint,sizeofByteArray#,sizeofArray#,sizeofArrayArray#,unsafeCoerce#,sameMutableArrayArray#,isTrue#,dataToTag#,Int(..))
import Control.DeepSeq (NFData)

import qualified Control.DeepSeq as DS

-- | A typeclass that is satisfied by all types. This is used
-- used to provide a fake constraint for 'Array' and 'SmallArray'.
class Always a
instance Always a

-- | The 'Contiguous' typeclass as an interface to a multitude of
--   contiguous structures.
class Contiguous (arr :: Type -> Type) where
  -- | The Mutable counterpart to the array.
  type family Mutable arr = (r :: Type -> Type -> Type) | r -> arr
  -- | The constraint needed to store elements in the array.
  type family Element arr :: Type -> Constraint
  -- | The empty array.
  empty :: arr a
  -- | Test whether the array is empty.
  null :: arr b -> Bool
  -- | Allocate a new mutable array of the given size.
  new :: (PrimMonad m, Element arr b) => Int -> m (Mutable arr (PrimState m) b)
  replicateM :: (PrimMonad m, Element arr b) => Int -> b -> m (Mutable arr (PrimState m) b)
  -- | Index into an array at the given index.
  index :: Element arr b => arr b -> Int -> b
  -- | Index into an array at the given index, yielding an unboxed one-tuple of the element.
  index# :: Element arr b => arr b -> Int -> (# b #)
  -- | Indexing in a monad.
  --
  --   The monad allows operations to be strict in the array
  --   when necessary. Suppose array copying is implemented like this:
  --
  --   > copy mv v = ... write mv i (v ! i) ...
  --
  --   For lazy arrays, @v ! i@ would not be not be evaluated,
  --   which means that @mv@ would unnecessarily retain a reference
  --   to @v@ in each element written.
  --
  --   With 'indexM', copying can be implemented like this instead:
  --
  --   > copy mv v = ... do
  --   >   x <- indexM v i
  --   >   write mv i x
  --
  --   Here, no references to @v@ are retained because indexing
  --   (but /not/ the elements) is evaluated eagerly.
  indexM :: (Element arr b, Monad m) => arr b -> Int -> m b
  -- | Read a mutable array at the given index.
  read :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> m b
  -- | Write to a mutable array at the given index.
  write :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> b -> m ()
  -- | Resize an array into one with the given size.
  resize :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> m (Mutable arr (PrimState m) b)
  -- | The size of the array
  size :: Element arr b => arr b -> Int
  -- | The size of the mutable array
  sizeMutable :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> m Int
  -- | Turn a mutable array into an immutable one without copying.
  --   The mutable array should not be used after this conversion.
  unsafeFreeze :: PrimMonad m => Mutable arr (PrimState m) b -> m (arr b)
  -- | Turn a mutable array into an immutable one with copying, using a slice of the mutable array.
  freeze :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> Int -> m (arr b)
  -- | Copy a slice of an immutable array into a new mutable array.
  thaw :: (PrimMonad m, Element arr b) => arr b -> Int -> Int -> m (Mutable arr (PrimState m) b)
  -- | Copy a slice of an array into a mutable array.
  copy :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -- ^ destination array
    -> Int -- ^ offset into destination array
    -> arr b -- ^ source array
    -> Int -- ^ offset into source array
    -> Int -- ^ number of elements to copy
    -> m ()
  -- | Copy a slice of a mutable array into another mutable array.
  --   In the case that the destination and source arrays are the
  --   same, the regions may overlap.
  copyMutable :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -- ^ destination array
    -> Int -- ^ offset into destination array
    -> Mutable arr (PrimState m) b -- ^ source array
    -> Int -- ^ offset into source array
    -> Int -- ^ number of elements to copy
    -> m ()
  -- | Clone a slice of an array.
  clone :: Element arr b
    => arr b
    -> Int
    -> Int
    -> arr b
  -- | Clone a slice of a mutable array.
  cloneMutable :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b
    -> Int
    -> Int
    -> m (Mutable arr (PrimState m) b)
  -- | Test the two arrays for equality.
  equals :: (Element arr b, Eq b) => arr b -> arr b -> Bool
  -- | Test the two mutable arrays for equality.
  sameMutable :: Mutable arr s a -> Mutable arr s a -> Bool
  -- | Unlift an array into an 'ArrayArray#'.
  unlift :: arr b -> ArrayArray#
  -- | Lift an 'ArrayArray#' into an array.
  lift :: ArrayArray# -> arr b
  -- | Create a singleton array.
  singleton :: Element arr a => a -> arr a
  -- | Create a doubleton array.
  doubleton :: Element arr a => a -> a -> arr a
  -- | Create a tripleton array.
  tripleton :: Element arr a => a -> a -> a -> arr a
  -- | Reduce the array and all of its elements to WHNF.
  rnf :: (NFData a, Element arr a) => arr a -> ()

instance Contiguous SmallArray where
  type Mutable SmallArray = SmallMutableArray
  type Element SmallArray = Always
  empty = mempty
  new n = newSmallArray n errorThunk
  index = indexSmallArray 
  indexM = indexSmallArrayM
  index# = indexSmallArray##
  read = readSmallArray
  write = writeSmallArray
  null a = case sizeofSmallArray a of
    0 -> True
    _ -> False
  freeze = freezeSmallArray
  size = sizeofSmallArray
  sizeMutable = return . sizeofSmallMutableArray
  unsafeFreeze = unsafeFreezeSmallArray
  thaw = thawSmallArray
  equals = (==)
  sameMutable = (==)
  singleton a = runST $ do
    marr <- newSmallArray 1 errorThunk
    writeSmallArray marr 0 a
    unsafeFreezeSmallArray marr
  doubleton a b = runST $ do
    m <- newSmallArray 2 errorThunk
    writeSmallArray m 0 a
    writeSmallArray m 1 b
    unsafeFreezeSmallArray m
  tripleton a b c = runST $ do
    m <- newSmallArray 3 errorThunk
    writeSmallArray m 0 a
    writeSmallArray m 1 b
    writeSmallArray m 2 c
    unsafeFreezeSmallArray m
  rnf !ary = 
    let !sz = sizeofSmallArray ary
        go !ix = if ix < sz
          then
            let !(# x #) = indexSmallArray## ary ix
             in DS.rnf x `seq` go (ix + 1)
          else ()
     in go 0
  clone = cloneSmallArray
  cloneMutable = cloneSmallMutableArray
  lift = fromArrayArray#
  unlift = toArrayArray#
  copy = copySmallArray
  copyMutable = copySmallMutableArray
  replicateM = replicateSmallArrayM
  resize = resizeSmallArray
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateM #-}
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
  {-# inline sameMutable #-}
  {-# inline unlift #-}
  {-# inline lift #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline rnf #-}

instance Contiguous PrimArray where
  type Mutable PrimArray = MutablePrimArray
  type Element PrimArray = Prim
  empty = mempty
  new = newPrimArray
  replicateM = replicatePrimArrayM
  index = indexPrimArray
  index# arr ix = (# indexPrimArray arr ix #)
  indexM arr ix = return (indexPrimArray arr ix)
  read = readPrimArray
  write = writePrimArray
  resize = resizeMutablePrimArray
  size = sizeofPrimArray
  sizeMutable = getSizeofMutablePrimArray
  freeze = freezePrimArray
  unsafeFreeze = unsafeFreezePrimArray
  thaw = thawPrimArray
  copy = copyPrimArray
  copyMutable = copyMutablePrimArray
  clone = clonePrimArray
  cloneMutable = cloneMutablePrimArray
  equals = (==)
  unlift = toArrayArray#
  lift = fromArrayArray#
  null (PrimArray a) = case sizeofByteArray# a of
    0# -> True
    _ -> False
  sameMutable = sameMutablePrimArray
  rnf (PrimArray !_) = ()
  singleton a = runST $ do
    marr <- newPrimArray 1
    writePrimArray marr 0 a
    unsafeFreezePrimArray marr
  doubleton a b = runST $ do
    m <- newPrimArray 2
    writePrimArray m 0 a
    writePrimArray m 1 b
    unsafeFreezePrimArray m
  tripleton a b c = runST $ do
    m <- newPrimArray 3
    writePrimArray m 0 a
    writePrimArray m 1 b
    writePrimArray m 2 c
    unsafeFreezePrimArray m
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateM #-}
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
  {-# inline sameMutable #-}
  {-# inline unlift #-}
  {-# inline lift #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline rnf #-}

instance Contiguous Array where
  type Mutable Array = MutableArray
  type Element Array = Always
  empty = mempty
  new n = newArray n errorThunk
  replicateM = newArray
  index = indexArray
  index# = indexArray##
  indexM = indexArrayM
  read = readArray
  write = writeArray
  resize = resizeArray
  size = sizeofArray
  sizeMutable = pure . sizeofMutableArray
  freeze = freezeArray
  unsafeFreeze = unsafeFreezeArray
  thaw = thawArray
  copy = copyArray
  copyMutable = copyMutableArray
  clone = cloneArray
  cloneMutable = cloneMutableArray
  equals = (==)
  unlift = toArrayArray#
  lift = fromArrayArray#
  null (Array a) = case sizeofArray# a of
    0# -> True
    _ -> False
  sameMutable = sameMutableArray
  rnf !ary = 
    let !sz = sizeofArray ary
        go !i
          | i == sz = ()
          | otherwise =
              let !(# x #) = indexArray## ary i
               in DS.rnf x `seq` go (i+1)
     in go 0
  singleton a = runST (newArray 1 a >>= unsafeFreezeArray)
  doubleton a b = runST $ do
    m <- newArray 2 a
    writeArray m 1 b
    unsafeFreezeArray m
  tripleton a b c = runST $ do
    m <- newArray 3 a
    writeArray m 1 b
    writeArray m 2 c
    unsafeFreezeArray m
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateM #-}
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
  {-# inline sameMutable #-}
  {-# inline unlift #-}
  {-# inline lift #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline rnf #-}

instance Contiguous UnliftedArray where
  type Mutable UnliftedArray = MutableUnliftedArray
  type Element UnliftedArray = PrimUnlifted
  empty = emptyUnliftedArray
  new = unsafeNewUnliftedArray
  replicateM = newUnliftedArray
  index = indexUnliftedArray
  index# arr ix = (# indexUnliftedArray arr ix #)
  indexM arr ix = return (indexUnliftedArray arr ix)
  read = readUnliftedArray
  write = writeUnliftedArray
  resize = resizeUnliftedArray
  size = sizeofUnliftedArray
  sizeMutable = pure . sizeofMutableUnliftedArray
  freeze = freezeUnliftedArray
  unsafeFreeze = unsafeFreezeUnliftedArray
  thaw = thawUnliftedArray
  copy = copyUnliftedArray
  copyMutable = copyMutableUnliftedArray
  clone = cloneUnliftedArray
  cloneMutable = cloneMutableUnliftedArray
  equals = (==)
  unlift = toArrayArray#
  lift = fromArrayArray#
  null (UnliftedArray a) = case sizeofArrayArray# a of
    0# -> True
    _ -> False
  sameMutable = sameMutableUnliftedArray
  rnf !ary = 
    let !sz = sizeofUnliftedArray ary
        go !i
          | i == sz = ()
          | otherwise =
              let x = indexUnliftedArray ary i
               in DS.rnf x `seq` go (i+1)
     in go 0
  singleton a = runST (newUnliftedArray 1 a >>= unsafeFreezeUnliftedArray)
  doubleton a b = runST $ do
    m <- newUnliftedArray 2 a
    writeUnliftedArray m 1 b
    unsafeFreezeUnliftedArray m
  tripleton a b c = runST $ do
    m <- newUnliftedArray 3 a
    writeUnliftedArray m 1 b
    writeUnliftedArray m 2 c
    unsafeFreezeUnliftedArray m
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateM #-}
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
  {-# inline sameMutable #-}
  {-# inline unlift #-}
  {-# inline lift #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline rnf #-}

errorThunk :: a
errorThunk = error "Contiguous typeclass: unitialized element"
{-# noinline errorThunk #-}

freezePrimArray :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Int -> m (PrimArray a)
freezePrimArray !src !off !len = do
  dst <- newPrimArray len
  copyMutablePrimArray dst 0 src off len
  unsafeFreezePrimArray dst
{-# inline freezePrimArray #-}

resizeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m (MutableArray (PrimState m) a)
resizeArray !src !sz = do
  dst <- newArray sz errorThunk
  copyMutableArray dst 0 src 0 (min sz (sizeofMutableArray src))
  return dst
{-# inline resizeArray #-}

resizeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> m (SmallMutableArray (PrimState m) a)
resizeSmallArray !src !sz = do
  dst <- newSmallArray sz errorThunk
  copySmallMutableArray dst 0 src 0 (min sz (sizeofSmallMutableArray src))
  return dst

resizeUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> m (MutableUnliftedArray (PrimState m) a)
resizeUnliftedArray !src !sz = do
  dst <- unsafeNewUnliftedArray sz
  copyMutableUnliftedArray dst 0 src 0 (min sz (sizeofMutableUnliftedArray src))
  return dst
{-# inline resizeUnliftedArray #-}

emptyUnliftedArray :: UnliftedArray a
emptyUnliftedArray = runST (unsafeNewUnliftedArray 0 >>= unsafeFreezeUnliftedArray)
{-# noinline emptyUnliftedArray #-}

-- | Append two arrays.
append :: (Contiguous arr, Element arr a) => arr a -> arr a -> arr a
append !a !b = runST $ do
  let !szA = size a
  let !szB = size b
  m <- new (szA + szB)
  copy m 0 a 0 szA
  copy m szA b 0 szB
  unsafeFreeze m
{-# inline append #-}

-- | Map over the elements of an array with the index.
imap :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) => (Int -> b -> c) -> arr1 b -> arr2 c
imap f a = runST $ do
  mb <- new (size a)
  let go !i
        | i == size a = return ()
        | otherwise = do
            x <- indexM a i
            write mb i (f i x)
            go (i+1)
  go 0
  unsafeFreeze mb
{-# inline imap #-}

-- | Map strictly over the elements of an array with the index.
--
--   Note that because a new array must be created, the resulting
--   array type can be /different/ than the original.
imap' :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) => (Int -> b -> c) -> arr1 b -> arr2 c
imap' f a = runST $ do
  mb <- new (size a)
  let go !i
        | i == size a = return ()
        | otherwise = do
            x <- indexM a i
            let !b = f i x
            write mb i b
            go (i + 1)
  go 0
  unsafeFreeze mb 
{-# INLINABLE imap' #-}

-- | Map over the elements of an array.
--
--   Note that because a new array must be created, the resulting
--   array type can be /different/ than the original.
map :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) => (b -> c) -> arr1 b -> arr2 c
map f a = runST $ do
  mb <- new (size a)
  let go !i
        | i == size a = return ()
        | otherwise = do
            x <- indexM a i
            write mb i (f x)
            go (i+1)
  go 0
  unsafeFreeze mb
{-# inline map #-}

-- | Map strictly over the elements of an array.
--
--   Note that because a new array must be created, the resulting
--   array type can be /different/ than the original.
map' :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) => (b -> c) -> arr1 b -> arr2 c
map' f a = runST $ do
  mb <- new (size a)
  let go !i
        | i == size a = return ()
        | otherwise = do
            x <- indexM a i
            let !b = f x
            write mb i b
            go (i+1)
  go 0
  unsafeFreeze mb
{-# inline map' #-}

-- | Convert one type of array into another.
convert :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 b) => arr1 b -> arr2 b
convert a = map id a
{-# inline convert #-}

-- | Right fold over the element of an array.
foldr :: (Contiguous arr, Element arr a) => (a -> b -> b) -> b -> arr a -> b
{-# inline foldr #-}
foldr f z arr = go 0
  where
    !sz = size arr
    go !i
      | sz > i = case index# arr i of
          (# x #) -> f x (go (i+1))
      | otherwise = z

-- | Strict left fold over the elements of an array.
foldl' :: (Contiguous arr, Element arr a) => (b -> a -> b) -> b -> arr a -> b
foldl' f !z !ary =
  let
    !sz = size ary
    go !i !acc
      | i == sz = acc
      | (# x #) <- index# ary i = go (i+1) (f acc x)
  in go 0 z
{-# inline foldl' #-}

-- | Strict left fold over the elements of an array.
ifoldl' :: (Contiguous arr, Element arr a) => (b -> Int -> a -> b) -> b -> arr a -> b
ifoldl' f !z !ary =
  let
    !sz = size ary
    go !i !acc
      | i == sz = acc
      | (# x #) <- index# ary i = go (i+1) (f acc i x)
  in go 0 z
{-# inline ifoldl' #-}

-- | Strict right fold over the elements of an array.
foldr' :: (Contiguous arr, Element arr a) => (a -> b -> b) -> b -> arr a -> b
foldr' f !z !ary =
  let
    go i !acc
      | i == -1 = acc
      | (# x #) <- index# ary i
      = go (i-1) (f x acc)
  in go (size ary - 1) z
{-# inline foldr' #-}

-- | Monoidal fold over the element of an array.
foldMap :: (Contiguous arr, Element arr a, Monoid m) => (a -> m) -> arr a -> m
foldMap f arr = go 0
  where
    !sz = size arr
    go !i
      | sz > i = case index# arr i of
          (# x #) -> mappend (f x) (go (i+1))
      | otherwise = mempty
{-# inline foldMap #-}

-- | Strict monoidal fold over the elements of an array.
foldMap' :: (Contiguous arr, Element arr a, Monoid m)
  => (a -> m) -> arr a -> m
foldMap' f !ary =
  let
    !sz = size ary
    go !i !acc
      | i == sz = acc
      | (# x #) <- index# ary i = go (i+1) (mappend acc (f x))
  in go 0 mempty
{-# inline foldMap' #-}

-- | Strict left monoidal fold over the elements of an array.
foldlMap' :: (Contiguous arr, Element arr a, Monoid m)
  => (a -> m) -> arr a -> m
foldlMap' f !ary =
  let
    !sz = size ary
    go !i !acc
      | i == sz = acc
      | (# x #) <- index# ary i = go (i+1) (mappend acc (f x))
  in go 0 mempty
{-# inline foldlMap' #-}

-- | Strict monoidal fold over the elements of an array.
ifoldlMap' :: (Contiguous arr, Element arr a, Monoid m)
  => (Int -> a -> m)
  -> arr a
  -> m
ifoldlMap' f !ary =
  let
    !sz = size ary
    go !i !acc
      | i == sz = acc
      | (# x #) <- index# ary i = go (i+1) (mappend acc (f i x))
  in go 0 mempty
{-# inline ifoldlMap' #-}

-- | Strict monoidal fold over the elements of an array.
ifoldlMap1' :: (Contiguous arr, Element arr a, Semigroup m)
  => (Int -> a -> m)
  -> arr a
  -> m
ifoldlMap1' f !ary =
  let
    !sz = size ary
    go !i !acc
      | i == sz = acc
      | (# x #) <- index# ary i = go (i+1) (acc <> f i x)
    !(# e0 #) = index# ary 0
  in go 1 (f 0 e0)
{-# inline ifoldlMap1' #-}

-- | Strict left monadic fold over the elements of an array.
foldlM' :: (Contiguous arr, Element arr a, Monad m) => (b -> a -> m b) -> b -> arr a -> m b
foldlM' f z0 arr = go 0 z0
  where
    !sz = size arr
    go !i !acc1
      | i < sz = do
          let (# x #) = index# arr i
          acc2 <- f acc1 x
          go (i + 1) acc2
      | otherwise = return acc1
{-# inline foldlM' #-}

-- | Drop elements that do not satisfy the predicate.
filter :: (Contiguous arr, Element arr a)
  => (a -> Bool)
  -> arr a
  -> arr a
filter p arr = ifilter (\_ a -> p a) arr

-- | Drop elements that do not satisfy the predicate which
--   is applied to values and their indices.
ifilter :: (Contiguous arr, Element arr a)
  => (Int -> a -> Bool)
  -> arr a
  -> arr a
ifilter p arr = runST $ do
  marr :: MutablePrimArray s Word8 <- newPrimArray sz
  let go1 :: Int -> Int -> ST s Int
      go1 !ix !numTrue = if ix < sz
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
      let go2 !ixSrc !ixDst = if ixDst < numTrue
            then do
              atIxKeep <- readPrimArray marr ixSrc
              if isTrue atIxKeep
                then do
                  atIxVal <- indexM arr ixSrc
                  write marrTrues ixDst atIxVal
                  go2 (ixSrc + 1) (ixDst + 1)
                else go2 (ixSrc + 1) ixDst
            else pure ()
      go2 0 0
      unsafeFreeze marrTrues 
  where
    !sz = size arr

{-# inline isTrue #-}
isTrue :: Word8 -> Bool
isTrue 0 = False
isTrue _ = True

thawPrimArray :: (PrimMonad m, Prim a) => PrimArray a -> Int -> Int -> m (MutablePrimArray (PrimState m) a)
thawPrimArray !arr !off !len = do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  return marr
{-# inline thawPrimArray #-}

clonePrimArray :: Prim a => PrimArray a -> Int -> Int -> PrimArray a
clonePrimArray !arr !off !len = runST $ do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  unsafeFreezePrimArray marr
{-# inline clonePrimArray #-}

cloneMutablePrimArray :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Int -> m (MutablePrimArray (PrimState m) a)
cloneMutablePrimArray !arr !off !len = do
  marr <- newPrimArray len
  copyMutablePrimArray marr 0 arr off len
  return marr
{-# inline cloneMutablePrimArray #-}

replicatePrimArrayM :: (PrimMonad m, Prim a)
  => Int -- ^ length
  -> a -- ^ element
  -> m (MutablePrimArray (PrimState m) a)
replicatePrimArrayM len a = do
  marr <- newPrimArray len
  setPrimArray marr 0 len a
  return marr
{-# inline replicatePrimArrayM #-}

replicateSmallArrayM :: (PrimMonad m)
  => Int
  -> a
  -> m (SmallMutableArray (PrimState m) a)
replicateSmallArrayM len a = do
  marr <- newSmallArray len errorThunk
  let go !ix = if ix < len
        then writeSmallArray marr ix a >> go (ix + 1)
        else return ()
  go 0
  return marr

-- | Create an array from a list. If the given length does
-- not match the actual length, this function has undefined
-- behavior.
unsafeFromListN :: (Contiguous arr, Element arr a)
  => Int -- ^ length of list
  -> [a] -- ^ list
  -> arr a
unsafeFromListN n l = runST $ do
  m <- new n
  let go !_ [] = return ()
      go !ix (x : xs) = do
        write m ix x
        go (ix+1) xs
  go 0 l
  unsafeFreeze m

-- | Create an array from a list, reversing the order of the
-- elements. If the given length does not match the actual length,
-- this function has undefined behavior.
unsafeFromListReverseN :: (Contiguous arr, Element arr a)
  => Int
  -> [a]
  -> arr a
unsafeFromListReverseN n l = runST $ do
  m <- new n
  let go !_ [] = return ()
      go !ix (x : xs) = do
        write m ix x
        go (ix-1) xs
  go (n - 1) l
  unsafeFreeze m
{-# inline unsafeFromListReverseN #-}

-- | Strictly map over a mutable array, modifying the elements in place.
mapMutable' :: (PrimMonad m, Contiguous arr, Element arr a)
  => (a -> a)
  -> Mutable arr (PrimState m) a
  -> m ()
mapMutable' f = \ !mary -> do
  !sz <- sizeMutable mary
  let
    go !i
      | i == sz = pure ()
      | otherwise = do
          a <- read mary i
          let !b = f a
          write mary i b
          go (i + 1)
  go 0
{-# inline mapMutable' #-}

-- | Strictly map over a mutable array with indices, modifying the elements in place.
imapMutable' :: (PrimMonad m, Contiguous arr, Element arr a)
  => (Int -> a -> a)
  -> Mutable arr (PrimState m) a
  -> m ()
imapMutable' f = \ !mary -> do
  !sz <- sizeMutable mary
  let
    go !i
      | i == sz = pure ()
      | otherwise = do
          a <- read mary i
          let !b = f i a
          write mary i b
          go (i + 1)
  go 0
{-# inline imapMutable' #-}

-- | Map each element of the array to an action, evaluate these
--   actions from left to right, and collect the results in a
--   new array.
traverseP :: (PrimMonad m, Contiguous arr1, Contiguous arr2, Element arr1 a, Element arr2 b)
  => (a -> m b)
  -> arr1 a
  -> m (arr2 b)
traverseP f = \ !ary ->
  let
    !sz = size ary
    go !i !mary
      | i == sz = unsafeFreeze mary
      | otherwise = do
          a <- indexM ary i
          b <- f a
          write mary i b
          go (i + 1) mary
  in do
      mary <- new sz
      go 0 mary
{-# inline traverseP #-}

newtype STA v a = STA {_runSTA :: forall s. Mutable v s a -> ST s (v a)}

runSTA :: (Contiguous v, Element v a) => Int -> STA v a -> v a
runSTA !sz = \ (STA m) -> runST $ new sz >>= \ ar -> m ar

-- | Map each element of the array to an action, evaluate these
--   actions from left to right, and collect the results.
--   For a version that ignores the results, see 'traverse_'.
traverse :: (Contiguous arr, Element arr a, Element arr b, Applicative f)
  => (a -> f b)
  -> arr a
  -> f (arr b)
traverse f = \ !ary ->
  let
    !len = size ary
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreeze mary
      | (# x #) <- index# ary i
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  write mary i b >> m mary)
               (f x) (go (i + 1))
  in if len == 0
     then pure empty
     else runSTA len <$> go 0

-- | Map each element of the array to an action, evaluate these
--   actions from left to right, and ignore the results.
--   For a version that doesn't ignore the results, see 'traverse'.
traverse_ ::
     (Contiguous arr, Element arr a, Applicative f)
  => (a -> f b)
  -> arr a
  -> f ()
traverse_ f a = go 0 where
  !sz = size a
  go !ix = if ix < sz
    then f (index a ix) *> go (ix + 1)
    else pure ()
{-# inline traverse_ #-}

-- | Map each element of the array and its index to an action,
--   evaluating these actions from left to right.
itraverse ::
     (Contiguous arr, Element arr a, Element arr b, Applicative f)
  => (Int -> a -> f b)
  -> arr a
  -> f (arr b)
{-# inline itraverse #-}
itraverse f ary =
  let !len = size ary
      go !ix
        | ix == len = pure $ STA $ \mary -> unsafeFreeze mary
        | (# x #) <- index# ary ix
        = liftA2 (\b (STA m) -> STA $ \mary ->
                   write mary ix b >> m mary)
                 (f ix x) (go (ix + 1))
   in if len == 0
        then pure empty
        else runSTA len <$> go 0

-- | Map each element of the array and its index to an action,
--   evaluate these actions from left to right, and ignore the results.
--   For a version that doesn't ignore the results, see 'itraverse'.
itraverse_ ::
     (Contiguous arr, Element arr a, Applicative f)
  => (Int -> a -> f b)
  -> arr a
  -> f ()
itraverse_ f a = go 0 where
  !sz = size a
  go !ix = if ix < sz
    then f ix (index a ix) *> go (ix + 1)
    else pure ()
{-# inline itraverse_ #-}

-- | Construct an array of the given length by applying
--   the function to each index.
generate :: (Contiguous arr, Element arr a)
  => Int
  -> (Int -> a)
  -> arr a
generate len f = runST (generateMutable len f >>= unsafeFreeze)
{-# inline generate #-}

-- | Construct a mutable array of the given length by applying
--   the function to each index.
generateMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> (Int -> a)
  -> m (Mutable arr (PrimState m) a)
generateMutable !len f = do
  marr <- new len
  let go !ix = if ix < len
        then do
          write marr ix (f ix)
          go (ix + 1)
        else return ()
  go 0
  return marr
{-# inline generateMutable #-}

iterateN :: (Contiguous arr, Element arr a)
  => Int
  -> (a -> a)
  -> a
  -> arr a
iterateN len f z0 = runST (iterateNMutable len f z0 >>= unsafeFreeze)
{-# inline iterateN #-}

iterateNMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> (a -> a)
  -> a 
  -> m (Mutable arr (PrimState m) a)
iterateNMutable !len f z0 = do
  marr <- new len
  let go !ix !acc
        | ix <= 0 = write marr ix z0
        | ix == len = return ()
        | otherwise = do
            let a = f acc
            write marr ix a
            go (ix + 1) a 
  go 0 z0
  return marr
{-# inline iterateNMutable #-}
 
-- | Lift an accumulating hash function over the elements of the array,
--   returning the final accumulated hash.
liftHashWithSalt :: (Contiguous arr, Element arr a)
  => (Int -> a -> Int)
  -> Int
  -> arr a
  -> Int
liftHashWithSalt f s0 arr = go 0 s0 where
  sz = size arr
  go !ix !s = if ix < sz
    then 
      let !(# x #) = index# arr ix
       in go (ix + 1) (f s x)
    else hashIntWithSalt s ix
{-# inline liftHashWithSalt #-}

-- | This function does not behave deterministically. Optimization level and
-- inlining can affect its results. However, the one thing that can be counted
-- on is that if it returns @True@, the two immutable arrays are definitely the
-- same. This is useful as shortcut for equality tests. However, keep in mind
-- that a result of @False@ tells us nothing about the arguments.
same :: Contiguous arr => arr a -> arr a -> Bool
same a b = isTrue# (sameMutableArrayArray# (unsafeCoerce# (unlift a) :: MutableArrayArray# s) (unsafeCoerce# (unlift b) :: MutableArrayArray# s))

hashIntWithSalt :: Int -> Int -> Int
hashIntWithSalt salt x = salt `combine` x
{-# inline hashIntWithSalt #-}

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
{-# inline combine #-}
