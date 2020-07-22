0.5.2: [2020.XX.XX]
-------------------
* Add `foldrZipWith` and `ifoldrZipWith`.

0.5.1: [2020.06.30]
-----------------
* Add `izipWith`.
* Compatibility with `primitive-0.7.1.0`.

0.5: [2019.07.23]
-----------------
* Add `generateM`, `reverseSlice`, `swap`, `catMaybes`,
  `zipWith`, `zip`, `lefts`, `rights`, `partitionEithers`, `elem`,
  `find`, `maximum`/`minimum`, `maximumBy`/`minimumBy`, `asum`,
  `mapM(_)`, `forM(_)`, `for(_)`, `sequence(_)`, `(<$)`, `ap`, `scanl`,
  `scanl'`, `iscanl`, `iscanl'`, `prescanl`, `prescanl'`, `iprescanl`,
  `iprescanl'`
* Re-export Array types from the `primitive` package
* Expand unit test suite to include all added functions
* Expand laws test suite to test Foldable/IsList/Traversable laws
  in addition to Functor/Applicative
* Add benchmark suite that measures allocations
* Fix performance issue with fold functions that caused huge increase
  in allocations when partially-applied. Partially-applied folds now
  perform as well as fully-applied.
* Make sure all functions are marked INLINE. Last function not marked
  as inline was `imap'`.

0.4.0.1: [2019.05.17]
---------------------
* Allow building with `primitive-0.7`. This required depending on the
  `primitive-unlifted` package to provide the removed `UnliftedArray`
  api.

0.4: [2019.05.16]
----------
* Add `convert`, `filter`, `ifilter`, `itraverse(_)` (#6), `imap'`,
  `unsafeFromListN`, `unsafeFromListReverseMutableN`, `ifoldr'`,
  `foldl`, `mapMutable`, `imapMutable`, `reverse`, `reverseMutable`,
  `replicateMutableM`, `create`, `createT`, `unsafeFromListReverseN`,
  `generate`, `generateMutable`, `iterate`, `iterateMutableN`,
  `iterateMutableNM`, `unfoldr`, `unfoldrMutable`, `toList`,
  `toListMutable`, `fromListMutableN`, `fromListMutable`, `fromListN`,
  `fromList`, `modify`, `modify'`, `enumFromN`, `enumFromMutableN`
* Refactor `replicate` functions to make more sense (#19)
* Add `Contiguous` instance for `SmallArray`
* Attempt to mark everything as inline (#18)
* Achieve 100% doc coverage, organise exports a lot more
  (mimicking vector). Various haddock fixes
* Make `toListMutable` strict in the accumulator
* Change all instances of `return` to `pure`
* Add initial test suite (some unit tests that check implementations
  against base/vector versions of the same functions)
* Export `unsafeFreeze`, `copy`, `write`, 
* Rename `sameMutable` to `equalsMutable`

0.3.3.0: [2019.03.24]
---------------------
* Add `freeze` as a method to `Contiguous`
* Add more folds
* Mark more functions as INLINEABLE

0.3.2.0: [2019.01.02]
---------------------
* Add `thaw` as a method to `Contiguous`

0.3.1.0: [2018.10.19]
---------------------
* Add `singleton`,`doubleton`,`tripleton` as methods to `Contiguous`
* Add `map'`, `imap`, `mapMutable'`, `imapMutable'`

0.3.0.0: [2018.09.06]
---------------------
* Document the need for `Always`
* Generalise API: from `ST s` to `PrimMonad m`
* Add NFData `rnf` function for deeply evaluating
  `Contiguous` arrays.
* Add function `equals`, for detecting if two arrays in memory
  are the same.
* Add hashing function.
* Make `map` able to produce a new array type.
* Add `replicate`, `null` as methods to `Contiguous`.
* Add `traverse`, `itraverse`, `traverseP`, `foldMap`

0.2.0.0: [2018.06.07]
---------------------
* Add cabal metadata: category, proper synopsis/description
* Use primitive-0.6.4.0

0.1.0.0: [2018.05.31]
---------------------
Initial version. Released on an unsuspecting world.
