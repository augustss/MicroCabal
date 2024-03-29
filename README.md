# MicroCabal
This repository contains a reimplementation of a subset of Cabal.

Why a reimplementation?  Because Cabal is not a Haskell tool, it is a ghc tool.
A Haskell tool should be compilable by an implementation of Haskell2010,
which Cabal is definitely not.

The implementation assumes a Unix-like system with commands like `wget` and `tar`.

To get a consistent set of packages MicroCabal uses Stackage to find compatible packages. So in a sense, MicroCabal is more like a MicroStackage. 

