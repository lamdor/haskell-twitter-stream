{-# LANGUAGE OverloadedStrings #-}
module Learning.Twitter.OAuth
       (
         twitterOAuth
       , twitterCredential
       , signOAuth -- re-export from Web.Authenticate.OAuth
       , OAuth
       , Credential
       ) where

import Web.Authenticate.OAuth

twitterOAuth :: OAuth
twitterOAuth = newOAuth { oauthServerName = "twitter"
                 , oauthConsumerKey = "" 
                 , oauthConsumerSecret = ""
                 }

twitterCredential :: Credential
twitterCredential = newCredential oauthToken oauthTokenSecret
  where oauthToken = ""
        oauthTokenSecret = ""


