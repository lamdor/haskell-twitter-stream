module Learning.Twitter.Stream where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Conduit
import Learning.Twitter.Conduit
import Learning.Twitter.OAuth
import Learning.Twitter.URL
import Network.HTTP.Client
import Network.HTTP.Client.TLS

readTwitterStream :: (MonadResource m) => 
                     URL -> OAuth -> Credential ->
                     Source m ByteString
readTwitterStream url oauth creds =
  bracketP openURLResponse responseClose foreverReadResponse
  where
    openURLResponse = do 
      manager   <- newManager tlsManagerSettings
      request   <- parseUrl url
      signedReq <- signOAuth oauth creds request
      responseOpen signedReq manager
    foreverReadResponse response =
      let br = responseBody response in 
       foreverSource $ liftIO $ brRead br

readTwitterStreamJSON :: (MonadResource m) => 
                         URL -> OAuth -> Credential ->
                         Source m Value
readTwitterStreamJSON url oauth creds =
  readTwitterStream url oauth creds $= parseToJsonConduit
