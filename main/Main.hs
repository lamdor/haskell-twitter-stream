module Main where

import Control.Monad.Trans.Resource
import Data.Conduit
import Learning.Twitter.Conduit
import Learning.Twitter.OAuth
import Learning.Twitter.Stream
import Learning.Twitter.URL

main :: IO ()
main = runResourceT $ do
  twitterStream <- readTwitterStream twitterSampleURL twitterOAuth twitterCredential
  twitterStream $$ resourcePrintSink
     


