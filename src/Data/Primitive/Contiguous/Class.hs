{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.Contiguous.Class
  ( ContiguousSlice(..)
  , Slice(..)
  , MutableSlice(..)
  , Contiguous(..)
  , Always
  ) where


import Data.Primitive.Contiguous.Shim
import Data.Primitive hiding (fromList,fromListN)
import Data.Primitive.Unlifted.Array
import Prelude hiding (map,all,any,foldr,foldMap,traverse,read,filter,replicate,null,reverse,foldl,foldr,zip,zipWith,scanl,(<$),elem,maximum,minimum,mapM,mapM_,sequence,sequence_)


import Control.DeepSeq (NFData)
import Control.Monad.ST (runST,ST)
import Control.Monad.ST.Run (runPrimArrayST,runSmallArrayST,runUnliftedArrayST,runArrayST)
import Data.Kind (Type)
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import GHC.Exts (ArrayArray#,Constraint,sizeofByteArray#,sizeofArray#,sizeofArrayArray#,unsafeCoerce#)
import Control.Monad.Primitive (PrimState, PrimMonad(..))

import qualified Control.DeepSeq as DS

data Slice arr a = Slice
  { offset :: {-# UNPACK #-} !Int
  , length :: {-# UNPACK #-} !Int
  , base :: arr a
  }

data MutableSlice arr s a = MutableSlice
  { offsetMut :: {-# UNPACK #-} !Int
  , lengthMut :: {-# UNPACK #-} !Int
  , baseMut :: !(Mutable arr s a)
  }

-- | The 'ContiguousSlice' typeclass as an interface to a multitude of
--   contiguous structures.
class ContiguousSlice (arr :: Type -> Type) where
  -- | The Mutable counterpart to the array.
  type family Mutable arr = (r :: Type -> Type -> Type) | r -> arr
  -- | The constraint needed to store elements in the array.
  type family Element arr :: Type -> Constraint

  type family Sliced arr :: Type -> Type
  type family Unsliced arr :: Type -> Type


  ------ Construction ------
  -- | Allocate a new mutable array of the given size.
  new :: (PrimMonad m, Element arr b) => Int -> m (Mutable arr (PrimState m) b)
  -- | @'replicateMutable' n x@ is a mutable array of length @n@ with @x@ the
  -- value of every element.
  replicateMutable :: (PrimMonad m, Element arr b)
                   => Int -> b -> m (Mutable arr (PrimState m) b)
  -- | The empty array.
  empty :: arr a
  -- | Create a singleton array.
  singleton :: Element arr a => a -> arr a
  -- | Create a doubleton array.
  doubleton :: Element arr a => a -> a -> arr a
  -- | Create a tripleton array.
  tripleton :: Element arr a => a -> a -> a -> arr a
  -- | Create a quadrupleton array.
  quadrupleton :: Element arr a => a -> a -> a -> a -> arr a

  ------ Access and Update ------
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
  read :: (PrimMonad m, Element arr b)
       => Mutable arr (PrimState m) b -> Int -> m b
  -- | Write to a mutable array at the given index.
  write :: (PrimMonad m, Element arr b)
        => Mutable arr (PrimState m) b -> Int -> b -> m ()

  ------ Properties ------
  -- | Test whether the array is empty.
  null :: arr b -> Bool
  -- | The size of the array
  size :: Element arr b => arr b -> Int
  -- | The size of the mutable array
  sizeMutable :: (PrimMonad m, Element arr b)
              => Mutable arr (PrimState m) b -> m Int
  -- | Test the two arrays for equality.
  equals :: (Element arr b, Eq b) => arr b -> arr b -> Bool
  -- | Test the two mutable arrays for pointer equality.
  --   Does not check equality of elements.
  equalsMutable :: Mutable arr s a -> Mutable arr s a -> Bool

  ------ Conversion ------
  -- | Turn a mutable array into an immutable one with copying, using a slice of the mutable array.
  freeze :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b
    -> Int -- ^ offset into the array
    -> Int -- ^ length of the slice
    -> m (arr b)
  -- | Copy a slice of an immutable array into a new mutable array.
  thaw :: (PrimMonad m, Element arr b)
    => arr b
    -> Int -- ^ offset into the array
    -> Int -- ^ length of the slice
    -> m (Mutable arr (PrimState m) b)
  -- | Clone a slice of an array.
  clone :: Element arr b
    => arr b -- ^ Array to copy a slice of
    -> Int -- ^ Offset into the array
    -> Int -- ^ Length of the slice
    -> arr b
  -- | Clone a slice of a mutable array.
  cloneMutable :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -- ^ Array to copy a slice of
    -> Int -- ^ Offset into the array
    -> Int -- ^ Length of the slice
    -> m (Mutable arr (PrimState m) b)

  ------ Copy Operations ------
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
  -- | Copy a slice of an array and then insert an element into that array.
  --
  -- The default implementation performs a memset which would be unnecessary
  -- except that the garbage collector might trace the uninitialized array.
  insertSlicing :: Element arr b
    => arr b -- ^ array to copy a slice from
    -> Int -- ^ offset into source array
    -> Int -- ^ length of the slice
    -> Int -- ^ index in the output array to insert at
    -> b -- ^ element to insert
    -> arr b
  -- FIXME
  insertSlicing = undefined
  -- insertSlicing src off len0 i x = run $ do
  --   dst <- replicateMutable (len0 + 1) x
  --   copy dst 0 src off i
  --   copy dst (i + 1) src (off + i) (len0 - i)
  --   unsafeFreeze dst
  {-# inline insertSlicing #-}

  ------ Reduction ------
  -- | Reduce the array and all of its elements to WHNF.
  rnf :: (NFData a, Element arr a) => arr a -> ()
  -- | Run an effectful computation that produces an array.
  run :: (forall s. ST s (arr a)) -> arr a

class (ContiguousSlice arr) => Contiguous arr where
  -- | Resize an array into one with the given size.
  resize :: (PrimMonad m, Element arr b)
         => Mutable arr (PrimState m) b
         -> Int
         -> m (Mutable arr (PrimState m) b)
  -- | Turn a mutable array into an immutable one without copying.
  --   The mutable array should not be used after this conversion.
  unsafeFreeze :: PrimMonad m => Mutable arr (PrimState m) b -> m (arr b)
  -- | Unlift an array into an 'ArrayArray#'.
  unlift :: arr b -> ArrayArray#
  -- | Lift an 'ArrayArray#' into an array.
  lift :: ArrayArray# -> arr b


-- | A typeclass that is satisfied by all types. This is used
-- used to provide a fake constraint for 'Array' and 'SmallArray'.
class Always a where {}
instance Always a where {}

-- FIXME delete fromMutableArray
fromMutableArray :: (PrimMonad m, ContiguousSlice arr, Element arr a)
  => Mutable arr (PrimState m) a -> m (MutableSlice arr (PrimState m) a)
{-# INLINE fromMutableArray #-}
fromMutableArray baseMut = do
  lengthMut <- sizeMutable baseMut
  pure MutableSlice{offsetMut=0,lengthMut,baseMut}

instance (ContiguousSlice arr) => ContiguousSlice (Slice arr) where
  type Mutable (Slice arr) = MutableSlice arr
  type Element (Slice arr) = Element arr
  type Sliced (Slice arr) = Slice arr
  type Unsliced (Slice arr) = arr
  new len = fromMutableArray =<< new len


instance ContiguousSlice SmallArray where
  type Mutable SmallArray = SmallMutableArray
  type Element SmallArray = Always
  type Sliced SmallArray = Slice SmallArray
  type Unsliced SmallArray = SmallArray
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
  sizeMutable = (\x -> pure $! sizeofSmallMutableArray x)
  thaw = thawSmallArray
  equals = (==)
  equalsMutable = (==)
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
  quadrupleton a b c d = runST $ do
    m <- newSmallArray 4 errorThunk
    writeSmallArray m 0 a
    writeSmallArray m 1 b
    writeSmallArray m 2 c
    writeSmallArray m 3 d
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
  copy = copySmallArray
  copyMutable = copySmallMutableArray
  replicateMutable = replicateSmallMutableArray
  run = runSmallArrayST
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateMutable #-}
  {-# inline index #-}
  {-# inline index# #-}
  {-# inline indexM #-}
  {-# inline read #-}
  {-# inline write #-}
  {-# inline size #-}
  {-# inline sizeMutable #-}
  {-# inline freeze #-}
  {-# inline thaw #-}
  {-# inline copy #-}
  {-# inline copyMutable #-}
  {-# inline clone #-}
  {-# inline cloneMutable #-}
  {-# inline equals #-}
  {-# inline equalsMutable #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline quadrupleton #-}
  {-# inline rnf #-}
  {-# inline run #-}

instance Contiguous SmallArray where
  resize = resizeSmallArray
  unsafeFreeze = unsafeFreezeSmallArray
  unlift (SmallArray x) = unsafeCoerce# x
  lift x = SmallArray (unsafeCoerce# x)
  {-# inline resize #-}
  {-# inline unsafeFreeze #-}
  {-# inline unlift #-}
  {-# inline lift #-}

instance ContiguousSlice PrimArray where
  type Mutable PrimArray = MutablePrimArray
  type Element PrimArray = Prim
  type Sliced PrimArray = Slice PrimArray
  type Unsliced PrimArray = PrimArray
  empty = mempty
  new = newPrimArray
  replicateMutable = replicateMutablePrimArray
  index = indexPrimArray
  index# arr ix = (# indexPrimArray arr ix #)
  indexM arr ix = pure (indexPrimArray arr ix)
  read = readPrimArray
  write = writePrimArray
  size = sizeofPrimArray
  sizeMutable = getSizeofMutablePrimArray
  freeze = freezePrimArrayShim
  thaw = thawPrimArray
  copy = copyPrimArray
  copyMutable = copyMutablePrimArray
  clone = clonePrimArrayShim
  cloneMutable = cloneMutablePrimArrayShim
  equals = (==)
  null (PrimArray a) = case sizeofByteArray# a of
    0# -> True
    _ -> False
  equalsMutable = sameMutablePrimArray
  rnf (PrimArray !_) = ()
  singleton a = runPrimArrayST $ do
    marr <- newPrimArray 1
    writePrimArray marr 0 a
    unsafeFreezePrimArray marr
  doubleton a b = runPrimArrayST $ do
    m <- newPrimArray 2
    writePrimArray m 0 a
    writePrimArray m 1 b
    unsafeFreezePrimArray m
  tripleton a b c = runPrimArrayST $ do
    m <- newPrimArray 3
    writePrimArray m 0 a
    writePrimArray m 1 b
    writePrimArray m 2 c
    unsafeFreezePrimArray m
  quadrupleton a b c d = runPrimArrayST $ do
    m <- newPrimArray 4
    writePrimArray m 0 a
    writePrimArray m 1 b
    writePrimArray m 2 c
    writePrimArray m 3 d
    unsafeFreezePrimArray m
  insertSlicing src off len0 i x = runPrimArrayST $ do
    dst <- new (len0 + 1)
    copy dst 0 src off i
    write dst i x
    copy dst (i + 1) src (off + i) (len0 - i)
    unsafeFreeze dst
  run = runPrimArrayST
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateMutable #-}
  {-# inline index #-}
  {-# inline index# #-}
  {-# inline indexM #-}
  {-# inline read #-}
  {-# inline write #-}
  {-# inline size #-}
  {-# inline sizeMutable #-}
  {-# inline freeze #-}
  {-# inline thaw #-}
  {-# inline copy #-}
  {-# inline copyMutable #-}
  {-# inline clone #-}
  {-# inline cloneMutable #-}
  {-# inline insertSlicing #-}
  {-# inline equals #-}
  {-# inline equalsMutable #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline quadrupleton #-}
  {-# inline rnf #-}
  {-# inline run #-}

instance Contiguous PrimArray where
  resize = resizeMutablePrimArray
  unsafeFreeze = unsafeFreezePrimArray
  unlift (PrimArray x) = unsafeCoerce# x
  lift x = PrimArray (unsafeCoerce# x)
  {-# inline resize #-}
  {-# inline unsafeFreeze #-}
  {-# inline unlift #-}
  {-# inline lift #-}

instance ContiguousSlice Array where
  type Mutable Array = MutableArray
  type Element Array = Always
  type Sliced Array = Slice Array
  type Unsliced Array = Array
  empty = mempty
  new n = newArray n errorThunk
  replicateMutable = newArray
  index = indexArray
  index# = indexArray##
  indexM = indexArrayM
  read = readArray
  write = writeArray
  size = sizeofArray
  sizeMutable = (\x -> pure $! sizeofMutableArray x)
  freeze = freezeArray
  thaw = thawArray
  copy = copyArray
  copyMutable = copyMutableArray
  clone = cloneArray
  cloneMutable = cloneMutableArray
  equals = (==)
  null (Array a) = case sizeofArray# a of
    0# -> True
    _ -> False
  equalsMutable = sameMutableArray
  rnf !ary =
    let !sz = sizeofArray ary
        go !i
          | i == sz = ()
          | otherwise =
              let !(# x #) = indexArray## ary i
               in DS.rnf x `seq` go (i+1)
     in go 0
  singleton a = runArrayST (newArray 1 a >>= unsafeFreezeArray)
  doubleton a b = runArrayST $ do
    m <- newArray 2 a
    writeArray m 1 b
    unsafeFreezeArray m
  tripleton a b c = runArrayST $ do
    m <- newArray 3 a
    writeArray m 1 b
    writeArray m 2 c
    unsafeFreezeArray m
  quadrupleton a b c d = runArrayST $ do
    m <- newArray 4 a
    writeArray m 1 b
    writeArray m 2 c
    writeArray m 3 d
    unsafeFreezeArray m
  run = runArrayST
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateMutable #-}
  {-# inline index #-}
  {-# inline index# #-}
  {-# inline indexM #-}
  {-# inline read #-}
  {-# inline write #-}
  {-# inline size #-}
  {-# inline sizeMutable #-}
  {-# inline freeze #-}
  {-# inline thaw #-}
  {-# inline copy #-}
  {-# inline copyMutable #-}
  {-# inline clone #-}
  {-# inline cloneMutable #-}
  {-# inline equals #-}
  {-# inline equalsMutable #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline quadrupleton #-}
  {-# inline rnf #-}
  {-# inline run #-}

instance Contiguous Array where
  resize = resizeArray
  unsafeFreeze = unsafeFreezeArray
  unlift (Array x) = unsafeCoerce# x
  lift x = Array (unsafeCoerce# x)
  {-# inline resize #-}
  {-# inline unsafeFreeze #-}
  {-# inline unlift #-}
  {-# inline lift #-}

instance ContiguousSlice UnliftedArray where
  type Mutable UnliftedArray = MutableUnliftedArray
  type Element UnliftedArray = PrimUnlifted
  type Sliced UnliftedArray = Slice UnliftedArray
  type Unsliced UnliftedArray = UnliftedArray
  empty = emptyUnliftedArray
  new = unsafeNewUnliftedArray
  replicateMutable = newUnliftedArray
  index = indexUnliftedArray
  index# arr ix = (# indexUnliftedArray arr ix #)
  indexM arr ix = pure (indexUnliftedArray arr ix)
  read = readUnliftedArray
  write = writeUnliftedArray
  size = sizeofUnliftedArray
  sizeMutable = pure . sizeofMutableUnliftedArray
  freeze = freezeUnliftedArray
  thaw = thawUnliftedArray
  copy = copyUnliftedArray
  copyMutable = copyMutableUnliftedArray
  clone = cloneUnliftedArray
  cloneMutable = cloneMutableUnliftedArray
  equals = (==)
  null (UnliftedArray a) = case sizeofArrayArray# a of
    0# -> True
    _ -> False
  equalsMutable = sameMutableUnliftedArray
  rnf !ary =
    let !sz = sizeofUnliftedArray ary
        go !i
          | i == sz = ()
          | otherwise =
              let x = indexUnliftedArray ary i
               in DS.rnf x `seq` go (i+1)
     in go 0
  singleton a = runUnliftedArrayST (newUnliftedArray 1 a >>= unsafeFreezeUnliftedArray)
  doubleton a b = runUnliftedArrayST $ do
    m <- newUnliftedArray 2 a
    writeUnliftedArray m 1 b
    unsafeFreezeUnliftedArray m
  tripleton a b c = runUnliftedArrayST $ do
    m <- newUnliftedArray 3 a
    writeUnliftedArray m 1 b
    writeUnliftedArray m 2 c
    unsafeFreezeUnliftedArray m
  quadrupleton a b c d = runUnliftedArrayST $ do
    m <- newUnliftedArray 4 a
    writeUnliftedArray m 1 b
    writeUnliftedArray m 2 c
    writeUnliftedArray m 3 d
    unsafeFreezeUnliftedArray m
  run = runUnliftedArrayST
  {-# inline empty #-}
  {-# inline null #-}
  {-# inline new #-}
  {-# inline replicateMutable #-}
  {-# inline index #-}
  {-# inline index# #-}
  {-# inline indexM #-}
  {-# inline read #-}
  {-# inline write #-}
  {-# inline size #-}
  {-# inline sizeMutable #-}
  {-# inline freeze #-}
  {-# inline thaw #-}
  {-# inline copy #-}
  {-# inline copyMutable #-}
  {-# inline clone #-}
  {-# inline cloneMutable #-}
  {-# inline equals #-}
  {-# inline equalsMutable #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline quadrupleton #-}
  {-# inline rnf #-}
  {-# inline run #-}


instance Contiguous UnliftedArray where
  resize = resizeUnliftedArray
  unsafeFreeze = unsafeFreezeUnliftedArray
  unlift (UnliftedArray x) = x
  lift x = UnliftedArray x
  {-# inline resize #-}
  {-# inline unsafeFreeze #-}
  {-# inline unlift #-}
  {-# inline lift #-}
