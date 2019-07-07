{-# language
        BangPatterns
      , FlexibleInstances
      , LambdaCase
      , MagicHash
      , RankNTypes
      , ScopedTypeVariables
      , TypeFamilies
      , TypeFamilyDependencies
      , UnboxedTuples
  #-}

-- | The contiguous typeclass parameterises over a contiguous array type.
--   This allows us to have a common API to a number of contiguous
--   array types and their mutable counterparts.
module Data.Primitive.Contiguous
  (
    -- * Accessors
    -- ** Length Information
    size
  , sizeMutable
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
  , replicate
  , replicateMutable
  , generate
  , generateM
  , generateMutable
  , iterateN
  , iterateMutableN
  , write
    -- ** Monadic initialisation
  , replicateMutableM
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
    -- * Modifying arrays
    -- ** Permutations
  , reverse
  , reverseMutable
  , reverseSlice

    -- ** Resizing
  , resize

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
    -- ** Comparing for equality
  , equals
  , equalsMutable
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
  , ifoldr'
  , ifoldlMap'
  , ifoldlMap1'
  , foldlM'

    -- * Traversals
  , traverse
  , traverse_
  , itraverse
  , itraverse_
  , traverseP

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
  --, postscanl
  --, ipostscanl

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
  , unlift
    -- ** Between mutable and immutable variants
  , clone
  , cloneMutable
  , copy
  , copyMutable
  , freeze
  , thaw
  , unsafeFreeze

    -- * Hashing
  , liftHashWithSalt

    -- * Forcing an array and its contents
  , rnf

    -- * Classes
  , Contiguous(Mutable,Element)
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

import Prelude hiding (map,foldr,foldMap,traverse,read,filter,replicate,null,reverse,foldl,foldr,zip,zipWith,scanl,(<$))
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Primitive
import Control.Monad.ST (runST,ST)
import Data.Bits (xor)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Primitive hiding (fromList,fromListN)
import Data.Primitive.Unlifted.Array
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import Data.Semigroup (Semigroup,(<>),First(..))
import Data.Word (Word8)
import GHC.Base (build)
import GHC.Exts (MutableArrayArray#,ArrayArray#,Constraint,sizeofByteArray#,sizeofArray#,sizeofArrayArray#,unsafeCoerce#,sameMutableArrayArray#,isTrue#,dataToTag#,Int(..))

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
  -- | @'replicateMutable' n x@ is a mutable array of length @n@ with @x@ the value of every element.
  replicateMutable :: (PrimMonad m, Element arr b) => Int -> b -> m (Mutable arr (PrimState m) b)
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
  -- | Test the two mutable arrays for pointer equality.
  --   Does not check equality of elements.
  equalsMutable :: Mutable arr s a -> Mutable arr s a -> Bool
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
  sizeMutable = pure . sizeofSmallMutableArray
  unsafeFreeze = unsafeFreezeSmallArray
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
  lift x = SmallArray (unsafeCoerce# x)
  unlift (SmallArray x) = unsafeCoerce# x
  copy = copySmallArray
  copyMutable = copySmallMutableArray
  replicateMutable = replicateSmallMutableArray
  resize = resizeSmallArray
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
  replicateMutable = replicateMutablePrimArray
  index = indexPrimArray
  index# arr ix = (# indexPrimArray arr ix #)
  indexM arr ix = pure (indexPrimArray arr ix)
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
  unlift (PrimArray x) = unsafeCoerce# x
  lift x = PrimArray (unsafeCoerce# x)
  null (PrimArray a) = case sizeofByteArray# a of
    0# -> True
    _ -> False
  equalsMutable = sameMutablePrimArray
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
  replicateMutable = newArray
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
  unlift (Array x) = unsafeCoerce# x
  lift x = Array (unsafeCoerce# x)
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
  replicateMutable = newUnliftedArray
  index = indexUnliftedArray
  index# arr ix = (# indexUnliftedArray arr ix #)
  indexM arr ix = pure (indexUnliftedArray arr ix)
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
  unlift (UnliftedArray x) = x
  lift x = UnliftedArray x
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
  pure dst
{-# inline resizeArray #-}

resizeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> m (SmallMutableArray (PrimState m) a)
resizeSmallArray !src !sz = do
  dst <- newSmallArray sz errorThunk
  copySmallMutableArray dst 0 src 0 (min sz (sizeofSmallMutableArray src))
  pure dst
{-# inline resizeSmallArray #-}

resizeUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> m (MutableUnliftedArray (PrimState m) a)
resizeUnliftedArray !src !sz = do
  dst <- unsafeNewUnliftedArray sz
  copyMutableUnliftedArray dst 0 src 0 (min sz (sizeofMutableUnliftedArray src))
  pure dst
{-# inline resizeUnliftedArray #-}

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
        | i == size a = pure ()
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
        | i == size a = pure ()
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
        | i == size a = pure ()
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
        | i == size a = pure ()
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

-- | Strict right fold over the elements of an array.
foldr' :: (Contiguous arr, Element arr a) => (a -> b -> b) -> b -> arr a -> b
foldr' f !z !ary =
  let
    go i !acc
      | i == -1 = acc
      | !(# x #) <- index# ary i
      = go (i-1) (f x acc)
  in go (size ary - 1) z
{-# inline foldr' #-}

-- | Left fold over the elements of an array.
foldl :: (Contiguous arr, Element arr a) => (b -> a -> b) -> b -> arr a -> b
foldl f z ary = go 0 z
  where
    !sz = size ary
    go !i acc
      | i == sz = acc
      | otherwise = let (# x #) = index# ary i in go (i+1) (f acc x)

-- | Strict left fold over the elements of an array.
foldl' :: (Contiguous arr, Element arr a) => (b -> a -> b) -> b -> arr a -> b
foldl' f !z !ary =
  let
    !sz = size ary
    go !i !acc
      | i == sz = acc
      | !(# x #) <- index# ary i = go (i+1) (f acc x)
  in go 0 z
{-# inline foldl' #-}

-- | Strict left fold over the elements of an array, where the accumulating
--   function cares about the index of the element.
ifoldl' :: (Contiguous arr, Element arr a) => (b -> Int -> a -> b) -> b -> arr a -> b
ifoldl' f !z !ary =
  let
    !sz = size ary
    go !i !acc
      | i == sz = acc
      | (# x #) <- index# ary i = go (i+1) (f acc i x)
  in go 0 z
{-# inline ifoldl' #-}

-- | Strict right fold over the elements of an array, where the accumulating
--   function cares about the index of the element.
ifoldr' :: (Contiguous arr, Element arr a) => (Int -> a -> b -> b) -> b -> arr a -> b
ifoldr' f !z !arr =
  let !sz = size arr
      go !i !acc = if i == (-1)
        then acc
        else let !(# x #) = index# arr i in go (i-1) (f i x acc)
   in go (sz-1) z
{-# inline ifoldr' #-}

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
      | otherwise = pure acc1
{-# inline foldlM' #-}

-- | Drop elements that do not satisfy the predicate.
filter :: (Contiguous arr, Element arr a)
  => (a -> Bool)
  -> arr a
  -> arr a
filter p arr = ifilter (const p) arr
{-# inline filter #-}

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
{-# inline ifilter #-}

-- | The 'mapMaybe' function is a version of 'map' which can throw out elements.
--   In particular, the functional arguments returns something of type @'Maybe' b@.
--   If this is 'Nothing', no element is added on to the result array. If it is
--   @'Just' b@, then @b@ is included in the result array.
mapMaybe :: forall arr1 arr2 a b. (Contiguous arr1, Element arr1 a, Contiguous arr2, Element arr2 b)
  => (a -> Maybe b)
  -> arr1 a
  -> arr2 b
mapMaybe f arr = runST $ do
  let !sz = size arr
  let go :: Int -> Int -> [b] -> ST s ([b],Int)
      go !ix !numJusts justs = if ix < sz
        then do
          atIx <- indexM arr ix
          case f atIx of
            Nothing -> go (ix+1) numJusts justs
            Just x -> go (ix+1) (numJusts+1) (x:justs)
        else pure (justs,numJusts)
  !(bs,!numJusts) <- go 0 0 []
  !marr <- unsafeFromListReverseMutableN numJusts bs
  unsafeFreeze marr
{-# inline mapMaybe #-}

{-# inline isTrue #-}
isTrue :: Word8 -> Bool
isTrue 0 = False
isTrue _ = True

-- | The 'catMaybes' function takes a list of 'Maybe's and returns a
--   list of all the 'Just' values.
catMaybes :: (Contiguous arr, Element arr a, Element arr (Maybe a))
  => arr (Maybe a)
  -> arr a
catMaybes = mapMaybe id
{-# inline catMaybes #-}

thawPrimArray :: (PrimMonad m, Prim a) => PrimArray a -> Int -> Int -> m (MutablePrimArray (PrimState m) a)
thawPrimArray !arr !off !len = do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  pure marr
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
  pure marr
{-# inline cloneMutablePrimArray #-}

-- | @'replicate' n x@ is an array of length @n@ with @x@ the value of every element.
replicate :: (Contiguous arr, Element arr a) => Int -> a -> arr a
replicate n x = create (replicateMutable n x)
{-# inline replicate #-}

-- | @'replicateMutableM' n act@ performs the action n times, gathering the results.
replicateMutableM :: (PrimMonad m, Contiguous arr, Element arr a)
  => Int
  -> m a
  -> m (Mutable arr (PrimState m) a)
replicateMutableM len act = do
  marr <- new len
  let go !ix = when (ix < len) $ do
        x <- act
        write marr ix x
        go (ix + 1)
  go 0
  pure marr
{-# inline replicateMutableM #-}

replicateMutablePrimArray :: (PrimMonad m, Prim a)
  => Int -- ^ length
  -> a -- ^ element
  -> m (MutablePrimArray (PrimState m) a)
replicateMutablePrimArray len a = do
  marr <- newPrimArray len
  setPrimArray marr 0 len a
  pure marr
{-# inline replicateMutablePrimArray #-}

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

-- | Create an array from a list. If the given length does
-- not match the actual length, this function has undefined
-- behavior.
unsafeFromListN :: (Contiguous arr, Element arr a)
  => Int -- ^ length of list
  -> [a] -- ^ list
  -> arr a
unsafeFromListN n l = create (unsafeFromListMutableN n l)
{-# inline unsafeFromListN #-}

unsafeFromListMutableN :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> [a]
  -> m (Mutable arr (PrimState m) a)
unsafeFromListMutableN n l = do
  m <- new n
  let go !_ [] = pure m
      go !ix (x : xs) = do
        write m ix x
        go (ix+1) xs
  go 0 l
{-# inline unsafeFromListMutableN #-}

-- | Create a mutable array from a list, reversing the order of
--   the elements. If the given length does not match the actual length,
--   this function has undefined behavior.
unsafeFromListReverseMutableN :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> [a]
  -> m (Mutable arr (PrimState m) a)
unsafeFromListReverseMutableN n l = do
  m <- new n
  let go !_ [] = pure m
      go !ix (x : xs) = do
        write m ix x
        go (ix-1) xs
  go (n - 1) l
{-# inline unsafeFromListReverseMutableN #-}

-- | Create an array from a list, reversing the order of the
-- elements. If the given length does not match the actual length,
-- this function has undefined behavior.
unsafeFromListReverseN :: (Contiguous arr, Element arr a)
  => Int
  -> [a]
  -> arr a
unsafeFromListReverseN n l = create (unsafeFromListReverseMutableN n l)
{-# inline unsafeFromListReverseN #-}

-- | Map over a mutable array, modifying the elements in place.
mapMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => (a -> a)
  -> Mutable arr (PrimState m) a
  -> m ()
mapMutable f = \ !mary -> do
  !sz <- sizeMutable mary
  let go !ix = when (ix < sz) $ do
        a <- read mary ix
        write mary ix (f a)
        go (ix + 1)
  go 0
{-# inline mapMutable #-}

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

-- | Map over a mutable array with indices, modifying the elements in place.
imapMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => (Int -> a -> a)
  -> Mutable arr (PrimState m) a
  -> m ()
imapMutable f = \ !mary -> do
  !sz <- sizeMutable mary
  let go !ix = when (ix < sz) $ do
        a <- read mary ix
        write mary ix (f ix a)
        go (ix + 1)
  go 0
{-# inline imapMutable #-}

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
runSTA !sz (STA m) = runST $ new sz >>= \ ar -> m ar
{-# inline runSTA #-}

-- | Map each element of the array to an action, evaluate these
--   actions from left to right, and collect the results.
--   For a version that ignores the results, see 'traverse_'.
traverse :: (Contiguous arr, Element arr a, Element arr b, Applicative f)
  => (a -> f b)
  -> arr a
  -> f (arr b)
traverse f = itraverse (const f)
{-# inline traverse #-}

-- | Map each element of the array to an action, evaluate these
--   actions from left to right, and ignore the results.
--   For a version that doesn't ignore the results, see 'traverse'.
traverse_ ::
     (Contiguous arr, Element arr a, Applicative f)
  => (a -> f b)
  -> arr a
  -> f ()
traverse_ f = itraverse_ (const f)

-- | Map each element of the array and its index to an action,
--   evaluating these actions from left to right.
itraverse ::
     (Contiguous arr, Element arr a, Element arr b, Applicative f)
  => (Int -> a -> f b)
  -> arr a
  -> f (arr b)
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
{-# inline itraverse #-}

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
  go !ix = when (ix < sz) $
    f ix (index a ix) *> go (ix + 1)
{-# inline itraverse_ #-}

-- | Construct an array of the given length by applying
--   the function to each index.
generate :: (Contiguous arr, Element arr a)
  => Int
  -> (Int -> a)
  -> arr a
generate len f = create (generateMutable len f)
{-# inline generate #-}

-- | Construct an array of the given length by applying
--   the monadic actino to each index.
generateM :: (Contiguous arr, Element arr a, Monad m)
  => Int
  -> (Int -> m a)
  -> m (arr a)
generateM !sz f =
  let go !ix = if ix < sz
        then liftA2
          (\b (STA m) -> STA $ \marr -> do
              write marr ix b
              m marr
          )
          (f ix)
          (go (ix + 1))
        else pure $ STA unsafeFreeze
  in if sz == 0
    then pure empty
    else runSTA sz <$> go 0

-- | Construct a mutable array of the given length by applying
--   the function to each index.
generateMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> (Int -> a)
  -> m (Mutable arr (PrimState m) a)
generateMutable len f = generateMutableM len (pure . f)
{-# inline generateMutable #-}

-- | Construct a mutable array of the given length by applying
--   the monadic action to each index.
generateMutableM :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> (Int -> m a)
  -> m (Mutable arr (PrimState m) a)
generateMutableM !len f = do
  marr <- new len
  let go !ix = when (ix < len) $ do
        x <- f ix
        write marr ix x
        go (ix + 1)
  go 0
  pure marr
{-# inline generateMutableM #-}

-- | Apply a function @n@ times to a value and construct an array
--   where each consecutive element is the result of an additional
--   application of this function. The zeroth element is the original value.
--
--   @'iterateN' 5 ('+' 1) 0 = 'fromListN' 5 [0,1,2,3,4]@
iterateN :: (Contiguous arr, Element arr a)
  => Int
  -> (a -> a)
  -> a
  -> arr a
iterateN len f z0 = runST (iterateMutableN len f z0 >>= unsafeFreeze)
{-# inline iterateN #-}

-- | Apply a function @n@ times to a value and construct a mutable array
--   where each consecutive element is the result of an additional
--   application of this function. The zeroth element is the original value.
iterateMutableN :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> (a -> a)
  -> a
  -> m (Mutable arr (PrimState m) a)
iterateMutableN len f z0 = iterateMutableNM len (pure . f) z0
{-# inline iterateMutableN #-}

-- | Apply a monadic function @n@ times to a value and construct a mutable array
--   where each consecutive element is the result of an additional
--   application of this function. The zeroth element is the original value.
iterateMutableNM :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> (a -> m a)
  -> a
  -> m (Mutable arr (PrimState m) a)
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
{-# inline iterateMutableNM #-}

-- | Execute the monad action and freeze the resulting array.
create :: (Contiguous arr, Element arr a)
  => (forall s. ST s (Mutable arr s a))
  -> arr a
create x = runST (unsafeFreeze =<< x)
{-# inline create #-}

-- | Execute the monadic action and freeze the resulting array.
createT :: (Contiguous arr, Element arr a, Traversable f)
  => (forall s. ST s (f (Mutable arr s a)))
  -> f (arr a)
createT p = runST (mapM unsafeFreeze =<< p)
{-# inline createT #-}

-- | Construct an array by repeatedly applying a generator
--   function to a seed. The generator function yields 'Just' the
--   next element and the new seed or 'Nothing' if there are no more
--   elements.
--
-- >>> unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1) 10
--     <10,9,8,7,6,5,4,3,2,1>

-- Unfortunately, because we don't know ahead of time when to stop,
-- we need to construct a list and then turn it into an array.
unfoldr :: (Contiguous arr, Element arr a)
  => (b -> Maybe (a,b))
  -> b
  -> arr a
unfoldr f z0 = create (unfoldrMutable f z0)
{-# inline unfoldr #-}

-- | Construct a mutable array by repeatedly applying a generator
--   function to a seed. The generator function yields 'Just' the
--   next element and the new seed or 'Nothing' if there are no more
--   elements.
--
-- >>> unfoldrMutable (\n -> if n == 0 then Nothing else Just (n,n-1) 10
--     <10,9,8,7,6,5,4,3,2,1>

-- Unfortunately, because we don't know ahead of time when to stop,
-- we need to construct a list and then turn it into an array.
unfoldrMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => (b -> Maybe (a,b))
  -> b
  -> m (Mutable arr (PrimState m) a)
unfoldrMutable f z0 = do
  let go !sz s !xs = case f s of
        Nothing -> pure (sz,xs)
        Just (x,s') -> go (sz + 1) s' (x : xs)
  (sz,xs) <- go 0 z0 []
  unsafeFromListReverseMutableN sz xs
{-# inline unfoldrMutable #-}

-- | Construct an array with at most n elements by repeatedly
--   applying the generator function to a seed. The generator function
--   yields 'Just' the next element and the new seed or 'Nothing' if
--   there are no more elements.
unfoldrN :: (Contiguous arr, Element arr a)
  => Int
  -> (b -> Maybe (a, b))
  -> b
  -> arr a
unfoldrN maxSz f z0 = create (unfoldrMutableN maxSz f z0)
{-# inline unfoldrN #-}

-- | Construct a mutable array with at most n elements by repeatedly
--   applying the generator function to a seed. The generator function
--   yields 'Just' the next element and the new seed or 'Nothing' if
--   there are no more elements.
unfoldrMutableN :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> (b -> Maybe (a, b))
  -> b
  -> m (Mutable arr (PrimState m) a)
unfoldrMutableN !maxSz f z0 = do
  m <- new maxSz
  let go !ix s = if ix < maxSz
        then case f s of
          Nothing -> pure ix
          Just (x,s') -> do
            write m ix x
            go (ix + 1) s'
        else pure ix
  sz <- go 0 z0
  case compare maxSz sz of
    EQ -> pure m
    GT -> resize m sz
    LT -> error "Data.Primitive.Contiguous.unfoldrMutableN: internal error"
{-# inline unfoldrMutableN #-}

-- | Convert an array to a list.
toList :: (Contiguous arr, Element arr a)
  => arr a
  -> [a]
toList arr = build (\c n -> foldr c n arr)
{-# inline toList #-}

-- | Convert a mutable array to a list.

-- I don't think this can be expressed in terms of foldr/build,
-- so we just loop through the array.
toListMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => Mutable arr (PrimState m) a
  -> m [a]
toListMutable marr = do
  sz <- sizeMutable marr
  let go !ix !acc = if ix >= 0
        then do
          x <- read marr ix
          go (ix - 1) (x : acc)
        else pure acc
  go (sz - 1) []
{-# inline toListMutable #-}

-- | Given an 'Int' that is representative of the length of
--   the list, convert the list into a mutable array of the
--   given length.
--
--   /Note/: calls 'error' if the given length is incorrect.
fromListMutableN :: (Contiguous arr, Element arr a, PrimMonad m)
  => Int
  -> [a]
  -> m (Mutable arr (PrimState m) a)
fromListMutableN len vs = do
  marr <- new len
  let go [] !ix = if ix == len
        then pure ()
        else error "Data.Primitive.Contiguous.fromListN: list length less than specified size."
      go (a:as) !ix = if ix < len
        then do
          write marr ix a
          go as (ix + 1)
        else error "Data.Primitive.Contiguous.fromListN: list length greater than specified size."
  go vs 0
  pure marr
{-# inline fromListMutableN #-}

-- | Convert a list into a mutable array of the given length.
fromListMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => [a]
  -> m (Mutable arr (PrimState m) a)
fromListMutable xs = fromListMutableN (length xs) xs
{-# inline fromListMutable #-}

-- | Given an 'Int' that is representative of the length of
--   the list, convert the list into a mutable array of the
--   given length.
--
--   /Note/: calls 'error' if the given length is incorrect.
fromListN :: (Contiguous arr, Element arr a)
  => Int
  -> [a]
  -> arr a
fromListN len vs = create (fromListMutableN len vs)
{-# inline fromListN #-}

-- | Convert a list into an array.
fromList :: (Contiguous arr, Element arr a)
  => [a]
  -> arr a
fromList vs = create (fromListMutable vs)
{-# inline fromList #-}

-- | Modify the elements of a mutable array in-place.
modify :: (Contiguous arr, Element arr a, PrimMonad m)
  => (a -> a)
  -> Mutable arr (PrimState m) a
  -> m ()
modify f marr = do
  !sz <- sizeMutable marr
  let go !ix = when (ix < sz) $ do
        x <- read marr ix
        write marr ix (f x)
        go (ix + 1)
  go 0
{-# inline modify #-}

-- | Strictly modify the elements of a mutable array in-place.
modify' :: (Contiguous arr, Element arr a, PrimMonad m)
  => (a -> a)
  -> Mutable arr (PrimState m) a
  -> m ()
modify' f marr = do
  !sz <- sizeMutable marr
  let go !ix = when (ix < sz) $ do
        x <- read marr ix
        let !y = f x
        write marr ix y
        go (ix + 1)
  go 0
{-# inline modify' #-}

-- | Yield an array of the given length containing the values
--   @x, 'succ' x, 'succ' ('succ' x)@ etc.
enumFromN :: (Contiguous arr, Element arr a, Enum a)
  => a
  -> Int
  -> arr a
enumFromN z0 sz = create (enumFromMutableN z0 sz)
{-# inline enumFromN #-}

-- | Yield a mutable array of the given length containing the values
--   @x, 'succ' x, 'succ' ('succ' x)@ etc.
enumFromMutableN :: (Contiguous arr, Element arr a, PrimMonad m, Enum a)
  => a
  -> Int
  -> m (Mutable arr (PrimState m) a)
enumFromMutableN z0 !sz = do
  m <- new sz
  let go !ix z = if ix < sz
        then do
          write m ix z
          go (ix + 1) (succ z)
        else pure m
  go 0 z0
{-# inline enumFromMutableN #-}

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

-- | Reverse the elements of an array.
reverse :: (Contiguous arr, Element arr a)
  => arr a
  -> arr a
reverse arr = runST $ do
  marr <- new sz
  copy marr 0 arr 0 sz
  reverseMutable marr
  unsafeFreeze marr
  where
    !sz = size arr
{-# inline reverse #-}

-- | Reverse the elements of a mutable array, in-place.
reverseMutable :: (Contiguous arr, Element arr a, PrimMonad m)
  => Mutable arr (PrimState m) a
  -> m ()
reverseMutable marr = do
  !sz <- sizeMutable marr
  reverseSlice marr 0 (sz - 1)
{-# inline reverseMutable #-}

-- | Reverse the elements of a slice of a mutable array, in-place.
reverseSlice :: (Contiguous arr, Element arr a, PrimMonad m)
  => Mutable arr (PrimState m) a
  -> Int -- ^ start index
  -> Int -- ^ end index
  -> m ()
reverseSlice !marr !start !end = do
  let go !s !e = if s >= e
        then pure ()
        else do
          tmp <- read marr s
          write marr s =<< read marr e
          write marr e tmp
          go (s+1) (e-1)
  go start end
{-# inline reverseSlice #-}

-- | This function does not behave deterministically. Optimization level and
-- inlining can affect its results. However, the one thing that can be counted
-- on is that if it returns 'True', the two immutable arrays are definitely the
-- same. This is useful as shortcut for equality tests. However, keep in mind
-- that a result of 'False' tells us nothing about the arguments.
same :: Contiguous arr => arr a -> arr a -> Bool
same a b = isTrue# (sameMutableArrayArray# (unsafeCoerce# (unlift a) :: MutableArrayArray# s) (unsafeCoerce# (unlift b) :: MutableArrayArray# s))

hashIntWithSalt :: Int -> Int -> Int
hashIntWithSalt salt x = salt `combine` x
{-# inline hashIntWithSalt #-}

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
{-# inline combine #-}

-- | 'find' takes a predicate and an array, and returns the leftmost
--   element of the array matching the prediate, or 'Nothing' if there
--   is no such predicate.
find :: (Contiguous arr, Element arr a)
  => (a -> Bool)
  -> arr a
  -> Maybe a
find p = coerce . (foldMap (\x -> if p x then Just (First x) else Nothing))
{-# inline find #-}

-- | Swap the elements of the mutable array at the given indices.
swap :: (Contiguous arr, Element arr a, PrimMonad m)
  => Mutable arr (PrimState m) a
  -> Int
  -> Int
  -> m ()
swap !marr !ix1 !ix2 = do
  atIx1 <- read marr ix1
  atIx2 <- read marr ix2
  write marr ix1 atIx2
  write marr ix2 atIx1
{-# inline swap #-}

-- | Extracts from an array of 'Either' all the 'Left' elements.
-- All the 'Left' elements are extracted in order.
lefts :: forall arr a b.
  ( Contiguous arr
  , Element arr a
  , Element arr (Either a b)
  ) => arr (Either a b)
    -> arr a
lefts !arr = create $ do
  let !sz = size arr
      go :: Int -> [a] -> Int -> ST s (Int, [a])
      go !ix !as !acc = if ix < sz
        then do
          indexM arr ix >>= \case
            Left a -> go (ix + 1) (a:as) (acc + 1)
            Right _ -> go (ix + 1) as acc
        else pure (acc, as)
  (len, as) <- go 0 [] 0
  unsafeFromListReverseMutableN len as
{-# inline lefts #-}

-- | Extracts from an array of 'Either' all the 'Right' elements.
-- All the 'Right' elements are extracted in order.
rights :: forall arr a b.
  ( Contiguous arr
  , Element arr b
  , Element arr (Either a b)
  ) => arr (Either a b)
    -> arr b
rights !arr = create $ do
  let !sz = size arr
      go :: Int -> [b] -> Int -> ST s (Int, [b])
      go !ix !bs !acc = if ix < sz
        then do
          indexM arr ix >>= \case
            Left _ -> go (ix + 1) bs acc
            Right b -> go (ix + 1) (b:bs) (acc + 1)
        else pure (acc, bs)
  (len, bs) <- go 0 [] 0
  unsafeFromListReverseMutableN len bs
{-# inline rights #-}

-- | Partitions an array of 'Either' into two arrays.
-- All the 'Left' elements are extracted, in order, to the first
-- component of the output. Similarly the 'Right' elements are extracted
-- to the second component of the output.
partitionEithers :: forall arr a b.
  ( Contiguous arr
  , Element arr a
  , Element arr b
  , Element arr (Either a b)
  ) => arr (Either a b)
    -> (arr a, arr b)
partitionEithers !arr = runST $ do
  let !sz = size arr
      go :: Int -> [a] -> [b] -> Int -> Int -> ST s (Int, Int, [a], [b])
      go !ix !as !bs !accA !accB = if ix < sz
        then do
          indexM arr ix >>= \case
            Left a -> go (ix + 1) (a:as) bs (accA + 1) accB
            Right b -> go (ix + 1) as (b:bs) accA (accB + 1)
          else pure (accA, accB, as, bs)
  (lenA, lenB, as, bs) <- go 0 [] [] 0 0
  arrA <- unsafeFreeze =<< unsafeFromListReverseMutableN lenA as
  arrB <- unsafeFreeze =<< unsafeFromListReverseMutableN lenB bs
  pure (arrA, arrB)
{-# inline partitionEithers #-}

{-
-- | ('<*>') on lifted arrays.

-- TODO: this is not very efficient.
apply ::
  ( Contiguous arr
  , Element arr (a -> b)
  , Element arr a
  , Element arr b
  ) => arr (a -> b) -> arr a -> arr b
apply f x = fromList (toList f <*> toList x)
{-# inline apply #-}
-}

-- | 'scanl' is similar to 'foldl', but returns an array of
--   successive reduced values from the left:
--
--   > scanl f z [x1, x2, ...] = [z, f z x1, f (f z x1) x2, ...]
--
--   Note that
--
--   > last (toList (scanl f z xs)) == foldl f z xs.
scanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
scanl f = iscanl (const f)
{-# inline scanl #-}

-- | A variant of 'scanl' whose function argument takes the current
--   index as an argument.
iscanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (Int -> b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
iscanl f q as = internalScanl (size as + 1) 0 0 f q as
{-# inline iscanl #-}

-- | A strictly accumulating version of 'scanl'.
scanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
scanl' f = iscanl' (const f)
{-# inline scanl' #-}

-- | A strictly accumulating version of 'iscanl'.
iscanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (Int -> b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
iscanl' f !q as = internalScanl' (size as + 1) f q as
{-# inline iscanl' #-}

-- Internal only. This function helps prevent duplication.
-- The first argument is the size of the array argument.
-- The second argument is the index at which to start reading.
-- The third argument is the index at which to start writing.
internalScanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => Int
    -> Int
    -> Int
    -> (Int -> b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
internalScanl !sz !readIx !writeIx f q as = create $ do
  !marr <- new sz
  let go !rix !wix acc = if rix < sz
        then do
          write marr wix acc
          x <- indexM as rix
          go (rix + 1) (wix + 1) (f rix acc x)
        else pure ()
  go readIx writeIx q
  pure marr
{-# inline internalScanl #-}

-- Internal only. The first argument is the size of the array
-- argument. This function helps prevent duplication.
internalScanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => Int
    -> (Int -> b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
internalScanl' !sz f !q as = create $ do
  !marr <- new sz
  let go !ix !acc = if ix < sz
        then do
          write marr ix acc
          x <- indexM as ix
          go (ix + 1) (f ix acc x)
        else pure ()
  go 0 q
  pure marr
{-# inline internalScanl' #-}

-- | A prescan.
--
--   @
--   prescanl f z = init . scanl f z
--   @
--
--   Example: @prescanl (+) 0 \<1,2,3,4\> = \<0,1,3,6\>@
prescanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
prescanl f = iprescanl (const f)
{-# inline prescanl #-}

-- | A variant of 'prescanl' where the function argument takes
--   the current index of the array as an additional argument.
iprescanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (Int -> b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
iprescanl f q as = internalScanl (size as) 0 0 f q as

-- | Like 'prescanl', but with a strict accumulator.
prescanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
prescanl' f = iprescanl (const f)
{-# inline prescanl' #-}

-- | Like 'iprescanl', but with a strict accumulator.
iprescanl' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (Int -> b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
iprescanl' f !q as = internalScanl' (size as) f q as
{-# inline iprescanl' #-}

{-
ipostscanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (Int -> b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
ipostscanl f q as =
  let go !n = if n == 0
        then empty
        else
          let !ix = 0
              !(# x #) = index# as ix
              q' = f ix q x
           in internalScanl n 1 0 f q' as
  in go (size as)

postscanl ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) => (b -> a -> b)
    -> b
    -> arr1 a
    -> arr2 b
postscanl f = ipostscanl (const f)
-}

-- | 'zipWith' generalises 'zip' by zipping with the function
--   given as the first argument, instead of a tupling function.
--   For example, 'zipWith' (+) is applied to two arrays to produce
--   an array of the corresponding sums.
zipWith ::
  ( Contiguous arr1
  , Contiguous arr2
  , Contiguous arr3
  , Element arr1 a
  , Element arr2 b
  , Element arr3 c
  ) => (a -> b -> c)
    -> arr1 a
    -> arr2 b
    -> arr3 c
zipWith f as bs = create $ do
  let !sz = min (size as) (size bs)
  !marr <- new sz
  let go !ix = when (ix < sz) $ do
        a <- indexM as ix
        b <- indexM bs ix
        let !g = f a b
        write marr ix g
        go (ix + 1)
  go 0
  pure marr
{-# inline zipWith #-}

-- | 'zip' takes two arrays and returns an array of
--   corresponding pairs.
--
--   > zip [1, 2] ['a', 'b'] = [(1, 'a'), (2, 'b')]
--
--   If one input array is shorter than the other, excess
--   elements of the longer array are discarded:
--
--   > zip [1] ['a', 'b'] = [(1, 'a')]
--   > zip [1, 2] ['a'] = [(1, 'a')]
--
zip ::
  ( Contiguous arr1
  , Contiguous arr2
  , Contiguous arr3
  , Element arr1 a
  , Element arr2 b
  , Element arr3 (a, b)
  ) => arr1 a
    -> arr2 b
    -> arr3 (a, b)
zip = zipWith (,)
{-# inline zip #-}

(<$) ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 b
  , Element arr2 a
  ) => a -> arr1 b -> arr2 a
a <$ barr = create (replicateMutable (size barr) a)
{-# inline (<$) #-}

ap ::
  ( Contiguous arr1
  , Contiguous arr2
  , Contiguous arr3
  , Element arr1 (a -> b)
  , Element arr2 a
  , Element arr3 b
  ) => arr1 (a -> b) -> arr2 a -> arr3 b
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
{-# inline ap #-}


