module Main where

import Test.DocTest

main = doctest [ "-isrc"
               , "src/Learning/Twitter/Stream.hs"
               , "src/Learning/Twitter/Conduit.hs"
               , "src/Learning/Twitter/OAuth.hs"
               , "src/Learning/Twitter/Stats.hs"
               ]
