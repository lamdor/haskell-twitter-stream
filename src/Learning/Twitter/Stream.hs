{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Learning.Twitter.Stream where

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.ByteString
import Data.Conduit
import qualified Data.Conduit.List as CL
import Learning.Twitter.Conduit
import Learning.Twitter.OAuth
import Learning.Twitter.URL
import Network.HTTP.Client
import Network.HTTP.Client.TLS

readTwitterStream :: (MonadResource m) => 
                     URL -> OAuth -> Credential ->
                     m (Source m Value)
readTwitterStream url oauth creds = do
  manager   <- liftIO $ newManager tlsManagerSettings
  signedReq <- signReq url oauth creds
  response  <- liftIO $ responseOpen signedReq manager
  bytes     <- liftIO $ responseBody response
  let jsonStream = CL.sourceList [bytes] $= parseToJsonConduit
  return jsonStream
  where
   signReq url oauth creds = do
     request <- parseUrl url
     signOAuth oauth creds request
