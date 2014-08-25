{-# LANGUAGE FlexibleContexts #-}
module Learning.Twitter.Stream where

import Control.Monad.Trans.Resource
import Data.Aeson (Value)
import Data.Conduit
import Learning.Twitter.Conduit
import Learning.Twitter.OAuth
import Learning.Twitter.URL
import Network.HTTP.Conduit

readTwitterStream :: (MonadBaseControl IO m, MonadResource m) =>
                     URL -> OAuth -> Credential ->
                     Sink Value (ResourceT m) r ->
                     m r
readTwitterStream url oauth creds sink = do
  request <- parseUrl url
  signedReq <- signOAuth oauth creds request
  withManager $ \manager -> do
      response <- http signedReq manager
      responseBody response $$+- parseToJsonConduit =$ sink


test_readstream :: IO ()
test_readstream =
  runResourceT $ readTwitterStream twitterSampleURL twitterOAuth twitterCredential resourcePrintSink
