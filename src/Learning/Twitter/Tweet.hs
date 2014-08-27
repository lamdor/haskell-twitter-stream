{-# LANGUAGE OverloadedStrings #-}
module Learning.Twitter.Tweet where

import           Control.Monad
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Text as T
  
tweetText :: Value -> Maybe T.Text
tweetText = getTextAttribute >=> getText
  where
    getTextAttribute js = js ^? key "text"
    getText (String txt) = Just txt
    getText _            = Nothing


