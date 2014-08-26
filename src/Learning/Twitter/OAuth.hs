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
                 , oauthConsumerKey = "7VHAjymFKvJLehEwkinSB2T6p" 
                 , oauthConsumerSecret = "beBdOx64C527xIxx1HkG9IF8e0Jl0JBz21VJ3JX5eAwxFPZYQe"
                 }

twitterCredential :: Credential
twitterCredential = newCredential oauthToken oauthTokenSecret
  where oauthToken = "14086115-2oy2mmEHjfZuSI182u7bFU7SAKKWEVITdpe9sSUXO"
        oauthTokenSecret = "lugmfaC63GrnWZYQFSAxVCsgBdXzJBx6Xk56oEcTrQdvq"


