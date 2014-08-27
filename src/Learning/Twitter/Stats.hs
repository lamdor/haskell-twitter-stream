module Learning.Twitter.Stats where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid

data TweetStats = TweetStats { numberOfTweets :: Int } deriving (Eq, Show)

instance FromJSON TweetStats where
  parseJSON (Object v) = TweetStats <$>
                         return 1
  parseJSON _ = mzero

instance Monoid TweetStats where
  mempty = TweetStats { numberOfTweets = 0 }
  (TweetStats n1) `mappend` (TweetStats n2) = TweetStats { numberOfTweets = n1 + n2 }


