{-# LANGUAGE OverloadedStrings #-}
module Learning.Twitter.Stats where

import Data.Aeson
import Learning.Twitter.Tweet

data TweetStats = TweetStats { numberOfTweets :: Int
                             , startDate :: TwitterDateTime } deriving (Eq, Show)

instance FromJSON TweetStats where
  parseJSON = withObject "Tweet" $ \v ->
    TweetStats <$>
    return 1   <*>
    v .: "created_at"

instance Monoid TweetStats where
  mempty = TweetStats { numberOfTweets = 0, startDate = epochTwitterDateTime }
  (TweetStats n1 s1) `mappend` (TweetStats n2 s2) =
    TweetStats { numberOfTweets = n1 + n2 , startDate = startDate' s1 s2 }
    where startDate' s1' s2' | s1' == epochTwitterDateTime = s2'
                             | s2' == epochTwitterDateTime = s1'
                             | otherwise                   = s1' `min` s2'
