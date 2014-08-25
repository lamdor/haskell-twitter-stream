module Learning.Twitter.Stream where

import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Learning.Twitter.OAuth
import           Network.HTTP.Conduit

twitterSampleUrl = "https://stream.twitter.com/1.1/statuses/sample.json"
  
test_readstream :: IO ()
test_readstream = do
  request <- parseUrl twitterSampleUrl
  signedReq <- signOAuth oauth oauthCredential request
  withManager $ \manager -> do
      response <- http signedReq manager
      responseBody response $$+- CB.mapM_ (const $ return ())
  return ()
