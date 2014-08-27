{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.Trans.Resource
import Data.Conduit
import Learning.Twitter.Conduit
import Learning.Twitter.OAuth
import Learning.Twitter.Stats
import Learning.Twitter.Stream
import Learning.Twitter.Tweet
import Learning.Twitter.URL

type TweetStatsSource = (MonadResource m) => Source m TweetStats

main :: IO ()
main = runResourceT $ 
  (readTwitterStream twitterSampleURL twitterOAuth twitterCredential =$= decodeJSONConduit :: TweetStatsSource) =$=
  scanMappendConduit id $$
  printSink
     


