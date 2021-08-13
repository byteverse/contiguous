{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

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
import Prelude hiding (length,map,all,any,foldr,foldMap,traverse,read,filter,replicate,null,reverse,foldl,foldr,zip,zipWith,scanl,(<$),elem,maximum,minimum,mapM,mapM_,sequence,sequence_)


import GHC.Exts (TYPE,RuntimeRep(UnliftedRep))
import GHC.Exts (SmallArray#,Array#)
import Control.DeepSeq (NFData)
import Control.Monad.ST (runST,ST)
import Control.Monad.ST.Run (runPrimArrayST,runSmallArrayST,runUnliftedArrayST,runArrayST)
import Data.Kind (Type)
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import GHC.Exts (ArrayArray#,Constraint,sizeofByteArray#,sizeofArray#,sizeofArrayArray#,unsafeCoerce#)
import Control.Monad.Primitive (PrimState, PrimMonad(..))

import qualified Control.DeepSeq as DS

-- | Slices of immutable arrays: packages an offset and length with a backing array.
--
-- @since 0.6.0
data Slice arr a = Slice
  { offset :: {-# UNPACK #-} !Int
  , length :: {-# UNPACK #-} !Int
  , base :: Unlifted arr a
  }

-- | Slices of mutable arrays: packages an offset and length with a mutable backing array.
--
-- @since 0.6.0
data MutableSlice arr s a = MutableSlice
  { offsetMut :: {-# UNPACK #-} !Int
  , lengthMut :: {-# UNPACK #-} !Int
  , baseMut :: !(Mutable arr s a)
  }

-- | The 'ContiguousSlice' typeclass as an interface to a multitude of
-- contiguous structures.
--
-- Some functions do not make sense on slices; for those, see 'Contiguous'.
class ContiguousSlice (arr :: Type -> Type) where
  -- | The Mutable counterpart to the array.
  type family Mutable arr = (r :: Type -> Type -> Type) | r -> arr
  -- | The constraint needed to store elements in the array.
  type family Element arr :: Type -> Constraint
  -- | The slice type of this array.
  -- The slice of a raw array type @t@ should be 'Slice t',
  -- whereas the slice of a slice should be the same slice type.
  --
  -- @since 0.6.0
  type family Sliced arr :: Type -> Type
  -- | The mutable slice type of this array.
  -- The mutable slice of a raw array type @t@ should be 'MutableSlice t',
  -- whereas the mutable slice of a mutable slice should be the same slice type.
  --
  -- @since 0.6.0
  type family SlicedMut arr :: Type -> Type -> Type


  ------ Construction ------
  -- | Allocate a new mutable array of the given size.
  new :: (PrimMonad m, Element arr b) => Int -> m (Mutable arr (PrimState m) b)
  -- | @'replicateMut' n x@ is a mutable array of length @n@ with @x@ the
  -- value of every element.
  replicateMut :: (PrimMonad m, Element arr b)
    => Int -- length
    -> b -- fill element
    -> m (Mutable arr (PrimState m) b)
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
  sizeMut :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -> m Int
  -- | Test the two arrays for equality.
  equals :: (Element arr b, Eq b) => arr b -> arr b -> Bool
  -- | Test the two mutable arrays for pointer equality.
  --   Does not check equality of elements.
  equalsMut :: Mutable arr s a -> Mutable arr s a -> Bool

  ------ Conversion ------
  -- | Create a 'Slice' of an array.
  --
  -- @O(1)@.
  --
  -- @since 0.6.0
  slice :: (Element arr a)
    => arr a -- base array
    -> Int -- offset
    -> Int -- length
    -> Sliced arr a
  -- | Create a 'MutableSlice' of a mutable array.
  --
  -- @O(1)@.
  --
  -- @since 0.6.0
  sliceMut :: (Element arr a)
    => Mutable arr s a -- base array
    -> Int -- offset
    -> Int -- length
    -> SlicedMut arr s a
  -- | Create a 'Slice' that covers the entire array.
  --
  -- @since 0.6.0
  toSlice :: (Element arr a) => arr a -> Sliced arr a
  -- | Create a 'MutableSlice' that covers the entire array.
  --
  -- @since 0.6.0
  toSliceMut :: (PrimMonad m, Element arr a)
    => Mutable arr (PrimState m) a
    -> m (SlicedMut arr (PrimState m) a)
  -- | Clone a slice of an array.
  clone :: Element arr b
    => Sliced arr b -- ^ slice to copy
    -> arr b
  default clone ::
       ( Sliced arr ~ Slice arr, Contiguous arr
       , Element arr b)
    => Sliced arr b -> arr b
  {-# INLINE clone #-}
  clone Slice{offset,length,base} = clone_ (lift base) offset length
  clone_ :: Element arr a => arr a -> Int -> Int -> arr a
  -- | Clone a slice of a mutable array.
  cloneMut :: (PrimMonad m, Element arr b)
    => SlicedMut arr (PrimState m) b -- ^ Array to copy a slice of
    -> m (Mutable arr (PrimState m) b)
  default cloneMut :: (SlicedMut arr ~ MutableSlice arr, PrimMonad m, Element arr b)
    => SlicedMut arr (PrimState m) b -> m (Mutable arr (PrimState m) b)
  {-# INLINE cloneMut #-}
  cloneMut MutableSlice{offsetMut,lengthMut,baseMut}
    = cloneMut_ baseMut offsetMut lengthMut
  cloneMut_ :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -- ^ Array to copy a slice of
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (Mutable arr (PrimState m) b)
  freeze :: (PrimMonad m, Element arr a)
    => SlicedMut arr (PrimState m) a
    -> m (arr a)
  default freeze :: (SlicedMut arr ~ MutableSlice arr, PrimMonad m, Element arr a)
    => SlicedMut arr (PrimState m) a -> m (arr a)
  {-# INLINE freeze #-}
  freeze MutableSlice{offsetMut,lengthMut,baseMut}
    = freeze_ baseMut offsetMut lengthMut
  -- | Turn a mutable array into an immutable one with copying, using a slice of
  -- the mutable array.
  freeze_ :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b
    -> Int -- ^ offset
    -> Int -- ^ length
    -> m (arr b)
  -- | Copy a slice of an immutable array into a new mutable array.
  thaw :: (PrimMonad m, Element arr b)
    => Sliced arr b
    -> m (Mutable arr (PrimState m) b)
  default thaw ::
       ( Sliced arr ~ Slice arr, Contiguous arr
       , PrimMonad m, Element arr b)
    => Sliced arr b
    -> m (Mutable arr (PrimState m) b)
  {-# INLINE thaw #-}
  thaw Slice{offset,length,base} = thaw_ (lift base) offset length
  thaw_ :: (PrimMonad m, Element arr b)
    => arr b
    -> Int -- ^ offset into the array
    -> Int -- ^ length of the slice
    -> m (Mutable arr (PrimState m) b)

  ------ Copy Operations ------
  -- | Copy a slice of an array into a mutable array.
  copy :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -- ^ destination array
    -> Int -- ^ offset into destination array
    -> Sliced arr b -- ^ source slice
    -> m ()
  default copy ::
      ( Sliced arr ~ Slice arr, Contiguous arr
      , PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -> Int -> Sliced arr b -> m ()
  {-# INLINE copy #-}
  copy dst dstOff Slice{offset,length,base} = copy_ dst dstOff (lift base) offset length
  copy_ :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -- ^ destination array
    -> Int -- ^ offset into destination array
    -> arr b -- ^ source array
    -> Int -- ^ offset into source array
    -> Int -- ^ number of elements to copy
    -> m ()
  -- | Copy a slice of a mutable array into another mutable array.
  --   In the case that the destination and source arrays are the
  --   same, the regions may overlap.
  copyMut :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -- ^ destination array
    -> Int -- ^ offset into destination array
    -> SlicedMut arr (PrimState m) b -- ^ source slice
    -> m ()
  default copyMut :: (SlicedMut arr ~ MutableSlice arr, PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) b -> Int -> SlicedMut arr (PrimState m) b -> m ()
  {-# INLINE copyMut #-}
  copyMut dst dstOff MutableSlice{offsetMut,lengthMut,baseMut} = copyMut_ dst dstOff baseMut offsetMut lengthMut
  copyMut_ :: (PrimMonad m, Element arr b)
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
  insertSlicing :: (Element arr b)
    => Sliced arr b -- ^ slice to copy from
    -> Int -- ^ index in the output array to insert at
    -> b -- ^ element to insert
    -> arr b
  default insertSlicing ::
       (Sliced arr ~ Slice arr, Element arr b, Contiguous arr)
    => Sliced arr b -> Int -> b -> arr b
  insertSlicing src i x = run $ do
    dst <- replicateMut (size src + 1) x
    copy dst 0 (slice src 0 i)
    copy dst (i + 1) (slice src i (size src - i))
    unsafeFreeze dst
  {-# inline insertSlicing #-}

  ------ Reduction ------
  -- | Reduce the array and all of its elements to WHNF.
  rnf :: (NFData a, Element arr a) => arr a -> ()
  -- | Run an effectful computation that produces an array.
  run :: (forall s. ST s (arr a)) -> arr a

-- | THe 'Contiguous' typeclass is an extension of the 'ContiguousSlice' typeclass,
-- but includes operations that make sense only on uncliced contiguous structures.
--
-- @since 0.6.0
class (ContiguousSlice arr) => Contiguous arr where
  type Unlifted arr = (r :: Type -> TYPE 'UnliftedRep) | r -> arr
  -- | Resize an array into one with the given size.
  resize :: (PrimMonad m, Element arr b)
         => Mutable arr (PrimState m) b
         -> Int
         -> m (Mutable arr (PrimState m) b)
  -- | Turn a mutable array into an immutable one without copying.
  --   The mutable array should not be used after this conversion.
  unsafeFreeze :: PrimMonad m => Mutable arr (PrimState m) b -> m (arr b)
  -- | Unlift an array into an 'ArrayArray#'.
  unlift :: arr b -> Unlifted arr b
  -- | Lift an 'ArrayArray#' into an array.
  lift :: Unlifted arr b -> arr b


-- | A typeclass that is satisfied by all types. This is used
-- used to provide a fake constraint for 'Array' and 'SmallArray'.
class Always a where {}
instance Always a where {}

-- FIXME delete fromMutableArray
fromMutableArray :: (PrimMonad m, ContiguousSlice arr, Element arr a)
  => Mutable arr (PrimState m) a -> m (MutableSlice arr (PrimState m) a)
{-# INLINE fromMutableArray #-}
fromMutableArray baseMut = do
  lengthMut <- sizeMut baseMut
  pure MutableSlice{offsetMut=0,lengthMut,baseMut}

instance (Contiguous arr) => ContiguousSlice (Slice arr) where
  type Mutable (Slice arr) = MutableSlice arr
  type Element (Slice arr) = Element arr
  type Sliced (Slice arr) = Slice arr
  type SlicedMut (Slice arr) = MutableSlice arr
  ------ Construction ------
  new len = fromMutableArray =<< new len
  replicateMut len x = fromMutableArray =<< replicateMut len x
  empty = Slice{offset=0,length=0,base=unlift empty}
  singleton a = Slice{offset=0,length=1,base=unlift $ singleton a}
  doubleton a b = Slice{offset=0,length=2,base=unlift $ doubleton a b}
  tripleton a b c = Slice{offset=0,length=3,base=unlift $ tripleton a b c}
  quadrupleton a b c d = Slice{offset=0,length=4,base=unlift $ quadrupleton a b c d}

  ------ Access and Update ------
  index Slice{offset,base} i = index (lift base) (offset + i)
  index# Slice{offset,base} i = index# (lift base) (offset + i)
  indexM Slice{offset,base} i = indexM (lift base) (offset + i)
  read MutableSlice{offsetMut,baseMut} i = read baseMut (offsetMut + i)
  write MutableSlice{offsetMut,baseMut} i = write baseMut (offsetMut + i)

  ------ Properties ------
  null Slice{length} = length == 0
  size Slice{length} = length
  sizeMut MutableSlice{lengthMut} = pure lengthMut
  equals Slice{offset=oA,length=lenA,base=a}
         Slice{offset=oB,length=lenB,base=b}
    = lenA == lenB && loop 0 oA oB
    where
    loop !i !iA !iB =
      if i == lenA then True
      else index (lift a) iA == index (lift b) iB && loop (i+1) (iA+1) (iB+1)
  equalsMut MutableSlice{offsetMut=offA,lengthMut=lenA,baseMut=a}
                MutableSlice{offsetMut=offB,lengthMut=lenB,baseMut=b}
    =  a `equalsMut` b
    && offA == offB
    && lenA == lenB

  ------ Conversion ------
  slice Slice{offset,base} off' len' = Slice
    { offset = offset + off'
    , length = len'
    , base
    }
  sliceMut MutableSlice{offsetMut,baseMut} off' len' = MutableSlice
    { offsetMut = offsetMut + off'
    , lengthMut = len'
    , baseMut
    }
  clone = id
  clone_ Slice{offset,base} off' len' =
    Slice{offset=offset+off',length=len',base}
  cloneMut xs@MutableSlice{lengthMut} = cloneMut_ xs 0 lengthMut
  cloneMut_ MutableSlice{offsetMut,baseMut} off' len' = do
    base' <- cloneMut_ baseMut (offsetMut + off') len'
    pure MutableSlice{offsetMut=0,lengthMut=len',baseMut=base'}
  freeze xs@MutableSlice{lengthMut}
    = freeze_ xs 0 lengthMut
  freeze_ MutableSlice{offsetMut,baseMut} off' len' = do
    base <- freeze_ baseMut (offsetMut + off') len'
    pure Slice{offset=0,length=len',base=unlift base}
  thaw xs@Slice{length} = thaw_ xs 0 length
  thaw_ Slice{offset,base} off' len' = do
    baseMut <- thaw_ (lift base) (offset + off') len'
    pure MutableSlice{offsetMut=0,lengthMut=len',baseMut}
  toSlice = id
  toSliceMut = pure

  ------ Copy Operations ------
  copy dst dstOff src@Slice{length} = copy_ dst dstOff src 0 length
  copy_ MutableSlice{offsetMut,baseMut} dstOff Slice{offset,base} off' len =
    copy_ baseMut (offsetMut + dstOff) (lift base) (offset + off') len
  copyMut dst dstOff src@MutableSlice{lengthMut} = copyMut_ dst dstOff src 0 lengthMut
  copyMut_ MutableSlice{offsetMut=dstOff,baseMut=dst} dstOff'
           MutableSlice{offsetMut=srcOff,baseMut=src} srcOff' len =
    copyMut_ dst (dstOff + dstOff') src (srcOff + srcOff') len
  insertSlicing Slice{offset,length,base} i x = run $ do
    dst <- replicateMut (length + 1) x
    copy_ dst 0 (lift base) offset i
    copy_ dst (i + 1) (lift base) (offset + i) (length - i)
    base' <- unsafeFreeze dst
    pure Slice{offset=0,length=length+1,base=unlift base'}

  ------ Reduction ------
  rnf !arr@Slice{length} =
    let go !ix = if ix < length
          then
            let !(# x #) = index# arr ix
             in DS.rnf x `seq` go (ix + 1)
          else ()
     in go 0
  run = runST

  ------ Everything is Inline ------
  {-# INLINE new #-}
  {-# INLINE replicateMut #-}
  {-# INLINE empty #-}
  {-# INLINE singleton #-}
  {-# INLINE doubleton #-}
  {-# INLINE tripleton #-}
  {-# INLINE quadrupleton #-}
  {-# INLINE index #-}
  {-# INLINE index# #-}
  {-# INLINE indexM #-}
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# INLINE null #-}
  {-# INLINE size #-}
  {-# INLINE sizeMut #-}
  {-# INLINE equals #-}
  {-# INLINE equalsMut #-}
  {-# INLINE slice #-}
  {-# INLINE sliceMut #-}
  {-# INLINE toSlice #-}
  {-# INLINE toSliceMut #-}
  {-# INLINE clone #-}
  {-# INLINE clone_ #-}
  {-# INLINE cloneMut #-}
  {-# INLINE cloneMut_ #-}
  {-# INLINE insertSlicing #-}
  {-# INLINE freeze #-}
  {-# INLINE freeze_ #-}
  {-# INLINE thaw #-}
  {-# INLINE thaw_ #-}
  {-# INLINE copy #-}
  {-# INLINE copy_ #-}
  {-# INLINE copyMut #-}
  {-# INLINE copyMut_ #-}
  {-# INLINE rnf #-}
  {-# INLINE run #-}


instance ContiguousSlice SmallArray where
  type Mutable SmallArray = SmallMutableArray
  type Element SmallArray = Always
  type Sliced SmallArray = Slice SmallArray
  type SlicedMut SmallArray = MutableSlice SmallArray
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
  slice base offset length = Slice{offset,length,base=unlift base}
  sliceMut baseMut offsetMut lengthMut = MutableSlice{offsetMut,lengthMut,baseMut}
  toSlice base = Slice{offset=0,length=size base,base=unlift base}
  toSliceMut baseMut = do
    lengthMut <- sizeMut baseMut
    pure MutableSlice{offsetMut=0,lengthMut,baseMut}
  freeze_ = freezeSmallArray
  size = sizeofSmallArray
  sizeMut = (\x -> pure $! sizeofSmallMutableArray x)
  thaw_ = thawSmallArray
  equals = (==)
  equalsMut = (==)
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
  clone_ = cloneSmallArray
  cloneMut_ = cloneSmallMutableArray
  copy_ = copySmallArray
  copyMut_ = copySmallMutableArray
  replicateMut = replicateSmallMutableArray
  run = runSmallArrayST

  ------ Everything is Inline ------
  {-# INLINE new #-}
  {-# INLINE replicateMut #-}
  {-# INLINE empty #-}
  {-# INLINE singleton #-}
  {-# INLINE doubleton #-}
  {-# INLINE tripleton #-}
  {-# INLINE quadrupleton #-}
  {-# INLINE index #-}
  {-# INLINE index# #-}
  {-# INLINE indexM #-}
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# INLINE null #-}
  {-# INLINE size #-}
  {-# INLINE sizeMut #-}
  {-# INLINE equals #-}
  {-# INLINE equalsMut #-}
  {-# INLINE slice #-}
  {-# INLINE sliceMut #-}
  {-# INLINE toSlice #-}
  {-# INLINE toSliceMut #-}
  {-# INLINE clone_ #-}
  {-# INLINE cloneMut_ #-}
  {-# INLINE freeze_ #-}
  {-# INLINE thaw_ #-}
  {-# INLINE copy_ #-}
  {-# INLINE copyMut_ #-}
  {-# INLINE rnf #-}
  {-# INLINE run #-}

instance Contiguous SmallArray where
  type Unlifted SmallArray = SmallArray#
  resize = resizeSmallArray
  unsafeFreeze = unsafeFreezeSmallArray
  unlift (SmallArray x) = x
  lift x = SmallArray x
  {-# inline resize #-}
  {-# inline unsafeFreeze #-}
  {-# inline unlift #-}
  {-# inline lift #-}

instance ContiguousSlice PrimArray where
  type Mutable PrimArray = MutablePrimArray
  type Element PrimArray = Prim
  type Sliced PrimArray = Slice PrimArray
  type SlicedMut PrimArray = MutableSlice PrimArray
  empty = mempty
  new = newPrimArray
  replicateMut = replicateMutablePrimArray
  index = indexPrimArray
  index# arr ix = (# indexPrimArray arr ix #)
  indexM arr ix = pure (indexPrimArray arr ix)
  read = readPrimArray
  write = writePrimArray
  size = sizeofPrimArray
  sizeMut = getSizeofMutablePrimArray
  slice base offset length = Slice{offset,length,base=unlift base}
  sliceMut baseMut offsetMut lengthMut = MutableSlice{offsetMut,lengthMut,baseMut}
  toSlice base = Slice{offset=0,length=size base,base=unlift base}
  toSliceMut baseMut = do
    lengthMut <- sizeMut baseMut
    pure MutableSlice{offsetMut=0,lengthMut,baseMut}
  freeze_ = freezePrimArrayShim
  thaw_ = thawPrimArray
  copy_ = copyPrimArray
  copyMut_ = copyMutablePrimArray
  clone_ = clonePrimArrayShim
  cloneMut_ = cloneMutablePrimArrayShim
  equals = (==)
  null (PrimArray a) = case sizeofByteArray# a of
    0# -> True
    _ -> False
  equalsMut = sameMutablePrimArray
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
  insertSlicing src i x = runPrimArrayST $ do
    dst <- new (size src + 1)
    copy dst 0 (slice src 0 i)
    write dst i x
    copy dst (i + 1) (slice src i (size src - i))
    unsafeFreeze dst
  run = runPrimArrayST

  ------ Everything is Inline ------
  {-# INLINE new #-}
  {-# INLINE replicateMut #-}
  {-# INLINE empty #-}
  {-# INLINE singleton #-}
  {-# INLINE doubleton #-}
  {-# INLINE tripleton #-}
  {-# INLINE quadrupleton #-}
  {-# INLINE index #-}
  {-# INLINE index# #-}
  {-# INLINE indexM #-}
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# INLINE null #-}
  {-# INLINE size #-}
  {-# INLINE sizeMut #-}
  {-# INLINE equals #-}
  {-# INLINE equalsMut #-}
  {-# INLINE slice #-}
  {-# INLINE sliceMut #-}
  {-# INLINE toSlice #-}
  {-# INLINE toSliceMut #-}
  {-# INLINE clone_ #-}
  {-# INLINE cloneMut_ #-}
  {-# INLINE freeze_ #-}
  {-# INLINE thaw_ #-}
  {-# INLINE copy_ #-}
  {-# INLINE copyMut_ #-}
  {-# INLINE insertSlicing #-}
  {-# INLINE rnf #-}
  {-# INLINE run #-}

newtype PrimArray# a = PrimArray# ByteArray#
instance Contiguous PrimArray where
  type Unlifted PrimArray = PrimArray#
  resize = resizeMutablePrimArray
  unsafeFreeze = unsafeFreezePrimArray
  unlift (PrimArray x) = PrimArray# x
  lift (PrimArray# x) = PrimArray x
  {-# inline resize #-}
  {-# inline unsafeFreeze #-}
  {-# inline unlift #-}
  {-# inline lift #-}

instance ContiguousSlice Array where
  type Mutable Array = MutableArray
  type Element Array = Always
  type Sliced Array = Slice Array
  type SlicedMut Array = MutableSlice Array
  empty = mempty
  new n = newArray n errorThunk
  replicateMut = newArray
  index = indexArray
  index# = indexArray##
  indexM = indexArrayM
  read = readArray
  write = writeArray
  size = sizeofArray
  sizeMut = (\x -> pure $! sizeofMutableArray x)
  slice base offset length = Slice{offset,length,base=unlift base}
  sliceMut baseMut offsetMut lengthMut = MutableSlice{offsetMut,lengthMut,baseMut}
  toSlice base = Slice{offset=0,length=size base,base=unlift base}
  toSliceMut baseMut = do
    lengthMut <- sizeMut baseMut
    pure MutableSlice{offsetMut=0,lengthMut,baseMut}
  freeze_ = freezeArray
  thaw_ = thawArray
  copy_ = copyArray
  copyMut_ = copyMutableArray
  clone Slice{offset,length,base} = clone_ (lift base) offset length
  clone_ = cloneArray
  cloneMut_ = cloneMutableArray
  equals = (==)
  null (Array a) = case sizeofArray# a of
    0# -> True
    _ -> False
  equalsMut = sameMutableArray
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

  ------ Everything is Inline ------
  {-# INLINE new #-}
  {-# INLINE replicateMut #-}
  {-# INLINE empty #-}
  {-# INLINE singleton #-}
  {-# INLINE doubleton #-}
  {-# INLINE tripleton #-}
  {-# INLINE quadrupleton #-}
  {-# INLINE index #-}
  {-# INLINE index# #-}
  {-# INLINE indexM #-}
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# INLINE null #-}
  {-# INLINE size #-}
  {-# INLINE sizeMut #-}
  {-# INLINE equals #-}
  {-# INLINE equalsMut #-}
  {-# INLINE slice #-}
  {-# INLINE sliceMut #-}
  {-# INLINE toSlice #-}
  {-# INLINE toSliceMut #-}
  {-# INLINE clone_ #-}
  {-# INLINE cloneMut_ #-}
  {-# INLINE freeze_ #-}
  {-# INLINE thaw_ #-}
  {-# INLINE copy_ #-}
  {-# INLINE copyMut_ #-}
  {-# INLINE rnf #-}
  {-# INLINE run #-}

instance Contiguous Array where
  type Unlifted Array = Array#
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
  type SlicedMut UnliftedArray = MutableSlice UnliftedArray
  empty = emptyUnliftedArray
  new = unsafeNewUnliftedArray
  replicateMut = newUnliftedArray
  index = indexUnliftedArray
  index# arr ix = (# indexUnliftedArray arr ix #)
  indexM arr ix = pure (indexUnliftedArray arr ix)
  read = readUnliftedArray
  write = writeUnliftedArray
  size = sizeofUnliftedArray
  sizeMut = pure . sizeofMutableUnliftedArray
  slice base offset length = Slice{offset,length,base=unlift base}
  sliceMut baseMut offsetMut lengthMut = MutableSlice{offsetMut,lengthMut,baseMut}
  freeze_ = freezeUnliftedArray
  toSlice base = Slice{offset=0,length=size base,base=unlift base}
  toSliceMut baseMut = do
    lengthMut <- sizeMut baseMut
    pure MutableSlice{offsetMut=0,lengthMut,baseMut}
  thaw_ = thawUnliftedArray
  copy_ = copyUnliftedArray
  copyMut_ = copyMutableUnliftedArray
  clone_ = cloneUnliftedArray
  cloneMut_ = cloneMutableUnliftedArray
  equals = (==)
  null (UnliftedArray a) = case sizeofArrayArray# a of
    0# -> True
    _ -> False
  equalsMut = sameMutableUnliftedArray
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

  ------ Everything is Inline ------
  {-# INLINE new #-}
  {-# INLINE replicateMut #-}
  {-# INLINE empty #-}
  {-# INLINE singleton #-}
  {-# INLINE doubleton #-}
  {-# INLINE tripleton #-}
  {-# INLINE quadrupleton #-}
  {-# INLINE index #-}
  {-# INLINE index# #-}
  {-# INLINE indexM #-}
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# INLINE null #-}
  {-# INLINE size #-}
  {-# INLINE sizeMut #-}
  {-# INLINE equals #-}
  {-# INLINE equalsMut #-}
  {-# INLINE slice #-}
  {-# INLINE sliceMut #-}
  {-# INLINE toSlice #-}
  {-# INLINE toSliceMut #-}
  {-# INLINE clone_ #-}
  {-# INLINE cloneMut_ #-}
  {-# INLINE freeze_ #-}
  {-# INLINE thaw_ #-}
  {-# INLINE copy_ #-}
  {-# INLINE copyMut_ #-}
  {-# INLINE rnf #-}
  {-# INLINE run #-}


newtype UnliftedArray# a = UnliftedArray# ArrayArray#
instance Contiguous UnliftedArray where
  type Unlifted UnliftedArray = UnliftedArray#
  resize = resizeUnliftedArray
  unsafeFreeze = unsafeFreezeUnliftedArray
  unlift (UnliftedArray x) = (UnliftedArray# x)
  lift (UnliftedArray# x) = UnliftedArray x
  {-# inline resize #-}
  {-# inline unsafeFreeze #-}
  {-# inline unlift #-}
  {-# inline lift #-}
