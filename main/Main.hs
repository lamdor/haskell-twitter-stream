module Main where

import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Learning.Twitter.Conduit
import           Learning.Twitter.OAuth
import           Learning.Twitter.Stats
import           Learning.Twitter.Tweet
import           Learning.Twitter.URL

main :: IO ()
main = do
  (Just twitterOAuth) <- twitterOAuthFromEnv
  (Just twitterCredential) <- twitterCredentialFromEnv

  runResourceT $
    readSamplingTweets twitterOAuth twitterCredential =$= mapToStats =$= addStatsTogether $$ printSink

  where
    readSamplingTweets = tweetSource twitterSampleURL

    mapToStats :: (MonadResource m) => Conduit Tweet m TweetStats
    mapToStats = CL.map tweetJson =$= fromJSONConduit

    addStatsTogether = scanMappendConduit id
