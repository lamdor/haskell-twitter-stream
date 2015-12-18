{-# LANGUAGE OverloadedStrings #-}
module Learning.Twitter.Tweet where

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

-- $setup
-- >>> :set -XOverloadedStrings

newtype Tweet = Tweet { tweetJson :: Value } deriving (Show, Eq)


-- | Example
--
-- >>> decode "{\"abc\": 123, \"text\": \"hello\"}" >>= tweetText
-- Just "hello"

tweetText :: Value -> Maybe T.Text
tweetText = getTextAttribute >=> getText
  where
    getTextAttribute js = js ^? key "text"
    getText (String txt) = Just txt
    getText _            = Nothing


-- | Example
--
-- >>> isTweet <$> decode "{\"abc\": 123, \"text\": \"hello\"}"
-- Just True
-- >>> isTweet <$> decode "{\"abc\": 123, \"delete\": \"true\"}"
-- Just False

isTweet :: Value -> Bool
isTweet = isNothing . (^? key "delete")


-- | Example
--
-- >>> let tweets = runResourceT $ tweetSource twitterSampleURL twitterOAuth twitterCredential $$ CL.take 2 :: IO [Tweet]
-- >>> length <$> tweets
-- 2

tweetSource :: (MonadResource m) => URL -> OAuth -> Credential -> Source m Tweet
tweetSource url oauth creds =
  readTwitterStreamJSON url oauth creds =$=
  CL.filter isTweet =$=
  fromJSONConduit =$=
  CL.map Tweet


newtype TwitterDateTime = TwitterDateTime { utc :: UTCTime } deriving (Show, Eq, Ord)


-- | Example
--
-- >>> fromJSON $ String "Wed Aug 27 13:08:45 +0000 2008" :: Result TwitterDateTime
-- Success (TwitterDateTime {utc = 2008-08-27 13:08:45 UTC})

instance FromJSON TwitterDateTime where
  parseJSON = withText "TwitterDateTime" $ \t ->
    case parseTimeM True defaultTimeLocale "%a %b %d %H:%M:%S +0000 %Y" (T.unpack t) of
      Just dt -> pure $ TwitterDateTime dt
      Nothing -> fail "couldn't parse TwitterDateTime"

epochTwitterDateTime :: TwitterDateTime
epochTwitterDateTime = TwitterDateTime $ UTCTime (ModifiedJulianDay 0) 0
