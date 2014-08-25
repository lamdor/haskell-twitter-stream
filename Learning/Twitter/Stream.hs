module Learning.Twitter.Stream where

import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Aeson.Parser
import           Data.ByteString
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Learning.Twitter.OAuth
import           Network.HTTP.Conduit

parseToJsonConduit :: MonadThrow m => Conduit ByteString m Value
parseToJsonConduit = CB.lines =$= CA.conduitParser json =$= CL.map snd

resourcePrintSink :: (Show a, MonadResource m) => Sink a m ()
resourcePrintSink = CL.mapM_ (liftIO . print)

twitterSampleUrl = "https://stream.twitter.com/1.1/statuses/sample.json"

test_readstream :: IO ()
test_readstream = do
  request <- parseUrl twitterSampleUrl
  signedReq <- signOAuth oauth oauthCredential request
  withManager $ \manager -> do
      response <- http signedReq manager
      responseBody response $$+- parseToJsonConduit =$ resourcePrintSink
