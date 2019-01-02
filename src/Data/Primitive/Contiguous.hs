{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Primitive.Contiguous
  ( Contiguous(..)
  , Always
  , append
  , map
  , map'
  , imap
  , mapMutable'
  , imapMutable'
  , foldr
  , foldMap
  , foldl'
  , foldr'
  , foldMap'
  , foldlM'
  , traverse
  , traverseP
  , traverse_
  , itraverse_
  , unsafeFromListN
  , unsafeFromListReverseN
  , liftHashWithSalt
  , same
  ) where

import Prelude hiding (map,foldr,foldMap,traverse,read)
import Control.Monad.ST (runST,ST)
import Control.Monad.Primitive
import Control.Applicative (liftA2)
import Data.Bits (xor)
import Data.Kind (Type)
import Data.Primitive
import GHC.Exts (MutableArrayArray#,ArrayArray#,Constraint,sizeofByteArray#,sizeofArray#,sizeofArrayArray#,unsafeCoerce#,sameMutableArrayArray#,isTrue#)
import Control.DeepSeq (NFData)

import qualified Control.DeepSeq as DS

-- | A typeclass that is satisfied by all types. This is used
-- used to provide a fake constraint for 'Array' and 'SmallArray'.
class Always a
instance Always a

-- | A contiguous array of elements.
class Contiguous (arr :: Type -> Type) where
  type family Mutable arr = (r :: Type -> Type -> Type) | r -> arr
  type family Element arr :: Type -> Constraint
  empty :: arr a
  null :: arr b -> Bool
  new :: (PrimMonad m, Element arr b) => Int -> m (Mutable arr (PrimState m) b)
  replicateM :: (PrimMonad m, Element arr b) => Int -> b -> m (Mutable arr (PrimState m) b)
  index :: Element arr b => arr b -> Int -> b
  index# :: Element arr b => arr b -> Int -> (# b #)
  indexM :: (Element arr b, Monad m) => arr b -> Int -> m b
  read :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> m b
  write :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> b -> m ()
  resize :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> m (Mutable arr (PrimState m) b)
  size :: Element arr b => arr b -> Int
  sizeMutable :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> m Int
  unsafeFreeze :: PrimMonad m => Mutable arr (PrimState m) b -> m (arr b)
  thaw :: (PrimMonad m, Element arr b) => arr b -> Int -> Int -> m (Mutable arr (PrimState m) b)
  copy :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> arr b -> Int -> Int -> m ()
  copyMutable :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> Mutable arr (PrimState m) b -> Int -> Int -> m ()
  clone :: Element arr b => arr b -> Int -> Int -> arr b
  cloneMutable :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) b -> Int -> Int -> m (Mutable arr (PrimState m) b)
  equals :: (Element arr b, Eq b) => arr b -> arr b -> Bool
  unlift :: arr b -> ArrayArray#
  lift :: ArrayArray# -> arr b
  sameMutable :: Mutable arr s a -> Mutable arr s a -> Bool
  singleton :: Element arr a => a -> arr a
  doubleton :: Element arr a => a -> a -> arr a
  tripleton :: Element arr a => a -> a -> a -> arr a
  rnf :: (NFData a, Element arr a) => arr a -> ()

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

errorThunk :: a
errorThunk = error "Contiguous typeclass: unitialized element"
{-# NOINLINE errorThunk #-}

resizeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m (MutableArray (PrimState m) a)
resizeArray !src !sz = do
  dst <- newArray sz errorThunk
  copyMutableArray dst 0 src 0 (min sz (sizeofMutableArray src))
  return dst
{-# INLINE resizeArray #-}

resizeUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> m (MutableUnliftedArray (PrimState m) a)
resizeUnliftedArray !src !sz = do
  dst <- unsafeNewUnliftedArray sz
  copyMutableUnliftedArray dst 0 src 0 (min sz (sizeofMutableUnliftedArray src))
  return dst
{-# INLINE resizeUnliftedArray #-}

emptyUnliftedArray :: UnliftedArray a
emptyUnliftedArray = runST (unsafeNewUnliftedArray 0 >>= unsafeFreezeUnliftedArray)
{-# NOINLINE emptyUnliftedArray #-}

append :: (Contiguous arr, Element arr a) => arr a -> arr a -> arr a
append !a !b = runST $ do
  let !szA = size a
  let !szB = size b
  m <- new (szA + szB)
  copy m 0 a 0 szA
  copy m szA b 0 szB
  unsafeFreeze m
{-# INLINABLE append #-}

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
{-# INLINABLE imap #-}

-- | Map over the elements of an array.
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
{-# INLINABLE map #-}

-- | Map strictly over the elements of an array.
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
{-# INLINABLE map' #-}

-- | Right fold over the element of an array.
foldr :: (Contiguous arr, Element arr a) => (a -> b -> b) -> b -> arr a -> b
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
{-# INLINABLE foldl' #-}

-- | Strict right fold over the elements of an array.
foldr' :: (Contiguous arr, Element arr a) => (a -> b -> b) -> b -> arr a -> b
foldr' f !z !ary =
  let
    go i !acc
      | i == -1 = acc
      | (# x #) <- index# ary i
      = go (i-1) (f x acc)
  in go (size ary - 1) z
{-# INLINABLE foldr' #-}

-- | Monoidal fold over the element of an array.
foldMap :: (Contiguous arr, Element arr a, Monoid m) => (a -> m) -> arr a -> m
foldMap f arr = go 0
  where
    !sz = size arr
    go !i
      | sz > i = case index# arr i of
          (# x #) -> mappend (f x) (go (i+1))
      | otherwise = mempty
{-# INLINABLE foldMap #-}

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
{-# INLINABLE foldMap' #-}

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
{-# INLINABLE foldlM' #-}

thawPrimArray :: (PrimMonad m, Prim a) => PrimArray a -> Int -> Int -> m (MutablePrimArray (PrimState m) a)
thawPrimArray !arr !off !len = do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  return marr
{-# INLINE thawPrimArray #-}

clonePrimArray :: Prim a => PrimArray a -> Int -> Int -> PrimArray a
clonePrimArray !arr !off !len = runST $ do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  unsafeFreezePrimArray marr
{-# INLINE clonePrimArray #-}

cloneMutablePrimArray :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> Int -> m (MutablePrimArray (PrimState m) a)
cloneMutablePrimArray !arr !off !len = do
  marr <- newPrimArray len
  copyMutablePrimArray marr 0 arr off len
  return marr
{-# INLINE cloneMutablePrimArray #-}

replicatePrimArrayM :: (PrimMonad m, Prim a)
  => Int -- ^ length
  -> a -- ^ element
  -> m (MutablePrimArray (PrimState m) a)
replicatePrimArrayM len a = do
  marr <- newPrimArray len
  setPrimArray marr 0 len a
  return marr
{-# INLINE replicatePrimArrayM #-}

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
{-# INLINE unsafeFromListReverseN #-}

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
{-# INLINE mapMutable' #-}

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
{-# INLINE imapMutable' #-}

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
{-# INLINE traverseP #-}

newtype STA v a = STA {_runSTA :: forall s. Mutable v s a -> ST s (v a)}

runSTA :: (Contiguous v, Element v a) => Int -> STA v a -> v a
runSTA !sz = \ (STA m) -> runST $ new sz >>= \ ar -> m ar

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
{-# INLINABLE traverse_ #-}

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
{-# INLINABLE itraverse_ #-}

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
{-# INLINABLE liftHashWithSalt #-}

-- | This function does not behave deterministically. Optimization level and
-- inlining can affect its results. However, the one thing that can be counted
-- on is that if it returns @True@, the two immutable arrays are definitely the
-- same. This is useful as shortcut for equality tests. However, keep in mind
-- that a result of @False@ tells us nothing about the arguments.
same :: Contiguous arr => arr a -> arr a -> Bool
same a b = isTrue# (sameMutableArrayArray# (unsafeCoerce# (unlift a) :: MutableArrayArray# s) (unsafeCoerce# (unlift b) :: MutableArrayArray# s))

hashIntWithSalt :: Int -> Int -> Int
hashIntWithSalt salt x = salt `combine` x

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2


