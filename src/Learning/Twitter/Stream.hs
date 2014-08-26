{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Learning.Twitter.Stream where

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.ByteString
import Data.Conduit
import Learning.Twitter.Conduit
import Learning.Twitter.OAuth
import Learning.Twitter.URL
import Network.HTTP.Conduit

readTwitterStream :: (MonadResource m) => 
                     URL -> OAuth -> Credential ->
                     m (Source m Value)
readTwitterStream url oauth creds = do
  manager   <- liftIO $ newManager conduitManagerSettings
  signedReq <- signReq url oauth creds
  response  <- http signedReq manager
  let jsonResumableStream = responseBody response $=+ parseToJsonConduit
  (s,f)     <- unwrapResumable jsonResumableStream
  return . (addCleanupManager manager) . (addCleanup (const f)) $ s
 where
   signReq url oauth creds = do
     request <- parseUrl url
     signOAuth oauth creds request
   addCleanupManager manager = addCleanup (const (liftIO $ closeManager manager))
         

