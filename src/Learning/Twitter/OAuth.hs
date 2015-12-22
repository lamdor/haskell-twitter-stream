module Learning.Twitter.OAuth (
    twitterOAuthFromEnv,
    twitterCredentialFromEnv,
    signOAuth,
    OAuth,
    Credential,
    ) where

import           Web.Authenticate.OAuth
import           System.Environment (lookupEnv)
import           Data.ByteString.Char8 (ByteString, pack)

twitterOAuthFromEnv :: IO (Maybe OAuth)
twitterOAuthFromEnv = do
  mkey <- lookupEnvBS "TWITTER_OAUTH_CONSUMER_KEY"
  msecret <- lookupEnvBS "TWITTER_OAUTH_CONSUMER_SECRET"
  return $ toOAuth <$> mkey <*> msecret

  where
    toOAuth key secret =
      newOAuth { oauthServerName = "twitter", oauthConsumerKey = key, oauthConsumerSecret = secret }

twitterCredentialFromEnv :: IO (Maybe Credential)
twitterCredentialFromEnv = do
  mkey <- lookupEnvBS "TWITTER_OAUTH_TOKEN"
  msecret <- lookupEnvBS "TWITTER_OAUTH_TOKEN_SECRET"
  return $ newCredential <$> mkey <*> msecret

lookupEnvBS :: String -> IO (Maybe ByteString)
lookupEnvBS name = do
  val <- lookupEnv name
  return $ fmap pack val
