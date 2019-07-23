0.5: [2019.07.23]
-----------------
* Add `generateM`, `reverseSlice`, `mapMaybe`, `catMaybes`,
  `zipWith`, `zip`, `lefts`, `rights`, `partitionEithers`, `elem`,
  `maximum`/`minimum`, `maximumBy`/`minimumBy`, `asum`, `mapM(_)`,
  `forM(_)`, `for(_)`, `sequence(_)`, `(<$)`, `ap`, `scanl`, `scanl'`,
  `iscanl`, `iscanl'`, `prescanl`, `prescanl'`, `iprescanl`,
  `iprescanl'`
* Re-export Array types from the `primitive` package
* Expand unit test suite to include all added functions
* Expand laws test suite to test Foldable/IsList/Traversable laws
  in addition to Functor/Applicative
* Add benchmark suite that measures allocations
* Fix performance issue with fold functions that caused huge increase
  in allocations when partially-applied. Partially-applied folds now
  perform as well as fully-applied.

0.4.0.1: [2019.05.17]
---------------------
* Allow building with `primitive-0.7`. This required depending on the
  `primitive-unlifted` package to provide the removed `UnliftedArray`
  api.

