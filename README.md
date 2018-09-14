# conduit-pandas

Experiments to (try) replicate/mirror Python's `pandas` library using our Glorious Haskell language and `conduit`'s stream processing.

DISCLAIMER: THIS IS NOT A BINDING FOR PYTHON PANDAS!

## Motivation

### Pandas vs. type-safety

Pandas is widely adopted and used in the data processing community due to its expansive library capabilities and low barrier to entry thanks to Python's popularity. However there are limitations:

- Absolutely dynamic without any type safety make scripts/applications a runtime exception minefield
- Several operations (like joins) mutate record labels and create new ones with a magic suffix.
- Several functionalities require doing things the "pandas way" to exploit high performance. (Ex: map, filter)
- Datasets are loaded into memory. This is an expensive endeavor for "big data".

### Haskell + streaming > Pandas pandas ?

No idea. This repo intends to try and port common use cases that I come across to haskell land. Improvements over Pandas:

- Static typing can ensure data transformations are safe and errors are explicitly handled (ex: Missing values, parsing failures etc).
- Join results can be expressed more richly with tuples or `These` to know exactly what the results of the operation were.
- The core of the functionality and types can be vanilla Haskell and still play nicely with streaming/processing libraries.
- Streaming also means only a fraction of the dataset is ever stored in memory at a time.

## How to build+run

- Get [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

- Compile
  ```bash
  $ stack build --pedantic --ghc-options '-O2'
  ```

- Run
  ```bash
  $ time stack exec conduit-pandas-exe
  ```

## TODO

- [ ] Explore more use-cases and examples
- [ ] Add benchmarks with pandas and sqlite

## Results

### Hardware

stat|value
---|---
CPU|2.3 GHz Intel Core i5
Memory|16 GB 2133 MHz LPDDR3

### Findings

- Inner join for 1000x1000 records runs in 28 seconds, rougly 36K rows/second.

## Related reading

- [Haskell Frames benchmark](https://github.com/acowley/Frames#benchmarks)
- [Working with data in Haskell](https://www.fpcomplete.com/blog/2016/09/data-haskell)
