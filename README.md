[![Build Status](https://travis-ci.org/TikhonJelvis/adaptive-radix-trees.svg?branch=master)](https://travis-ci.org/TikhonJelvis/adaptive-radix-trees)

A Haskell imlementation of [adaptive radix trees][art]¹. Very much a work in progress:

  * reading and inserting values seems to work based on a suite of QuickCheck tests.
  * **performance is bad**: on my current ad hoc benchmarks, it performs 2×–10× worse than `Data.IntMap`.

Current roadmap:

  * optimize—in theory, it should be *faster* than `IntMap`!
  * reduce dependencies: stop using `ByteString` for keys
    * either stick to `Int` or try to do some sort of typeclass?
  * expand operations to deletes, merges and intersections
    * end goal: have an API compatible with `Data.IntMap`

### References
¹ V. Leis, A. Kemper, and T. Neumann, “The adaptive radix tree: Artful indexing for main-memory databases,” in *ICDE*, 2013, pp. 38–49.


[art]: http://www3.informatik.tu-muenchen.de/~leis/papers/ART.pdf
