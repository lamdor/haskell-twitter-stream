## haskell-twitter-stream ##

### Building / Testing ###

1. `cabal sandbox init`
2. `cabal install --only-dependencies`
3. `cabal build`
4. `dist/build/haskell-twitter-stream-exec/haskell-twitter-stream-exec` to run
5. `cabal test` to run doctests

### Things it uses ###

- [conduit](http://hackage.haskell.org/package/conduit)
- [http-client](https://hackage.haskell.org/package/http-client)
- [aeson](https://hackage.haskell.org/package/aeson)
- [authenticate-oauth](https://hackage.haskell.org/package/authenticate-oauth)
- [doctest](https://github.com/sol/doctest#readme)
