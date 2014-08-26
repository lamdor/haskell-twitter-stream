module Main where

import Control.Monad.Trans.Resource
import Learning.Twitter.Conduit
import Learning.Twitter.OAuth
import Learning.Twitter.Stream
import Learning.Twitter.URL


main :: IO ()
main =
  runResourceT $
    readTwitterStream twitterSampleURL twitterOAuth twitterCredential resourcePrintSink


