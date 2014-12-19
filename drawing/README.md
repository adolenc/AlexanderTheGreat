generateKnot function takes a series of steps and constructs a representation of tangle that satisfy those steps. See sampleKnot for example usage.

Compile with `ghc --make drawing.hs` and run with `./drawing -o drawing.svg -w 400 -h 400` to generate an image.

Requires [diagrams](http://projects.haskell.org/diagrams/) library, install it with `cabal install diagrams`.
