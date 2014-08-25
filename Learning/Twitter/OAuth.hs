{-# LANGUAGE OverloadedStrings #-}
module Learning.Twitter.OAuth
       (
         oauth
       , oauthCredential
       , signOAuth -- re-export from Web.Authenticate.OAuth
       ) where

import Web.Authenticate.OAuth

oauth :: OAuth
oauth = newOAuth { oauthServerName = "twitter"
                 , oauthConsumerKey = "" 
                 , oauthConsumerSecret = ""
                 }

oauthCredential :: Credential
oauthCredential = newCredential oauthToken oauthTokenSecret
  where oauthToken = ""
        oauthTokenSecret = ""


