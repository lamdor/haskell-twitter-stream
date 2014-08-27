{-# LANGUAGE OverloadedStrings #-}
module Learning.Twitter.Tweet where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Learning.Twitter.Conduit
import           Learning.Twitter.OAuth
import           Learning.Twitter.Stream
import           Learning.Twitter.URL
import           System.Locale
  
newtype Tweet = Tweet { tweetJson :: Value } deriving (Show, Eq)
  
tweetText :: Value -> Maybe T.Text
tweetText = getTextAttribute >=> getText
  where
    getTextAttribute js = js ^? key "text"
    getText (String txt) = Just txt
    getText _            = Nothing

isTweet :: Value -> Bool
isTweet = isNothing . (^? key "delete")

tweetSource :: (MonadResource m) => URL -> OAuth -> Credential -> Source m Tweet
tweetSource url oauth creds =
  readTwitterStreamJSON url oauth creds =$=
  CL.filter isTweet =$=
  fromJSONConduit =$=
  CL.map Tweet
  

newtype TwitterDateTime = TwitterDateTime { utc :: UTCTime } deriving (Show, Eq, Ord)

instance FromJSON TwitterDateTime where
  parseJSON = withText "TwitterDateTime" $ \t ->
    case parseTime defaultTimeLocale "%a %b %d %H:%M:%S +0000 %Y" (T.unpack t) of
      Just dt -> pure $ TwitterDateTime dt
      Nothing -> fail "couldn't parse TwitterDateTime"

epochTwitterDateTime :: TwitterDateTime
epochTwitterDateTime = TwitterDateTime $ UTCTime (ModifiedJulianDay 0) 0 

