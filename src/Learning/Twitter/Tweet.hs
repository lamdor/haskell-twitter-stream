{-# LANGUAGE OverloadedStrings #-}
module Learning.Twitter.Tweet where

import           Control.Monad
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
  
tweetText :: (Monad m) => Conduit Value m T.Text
tweetText = CL.mapMaybe (getTextAttribute >=> getText)
  where
    getTextAttribute js = js ^? key "text"
    getText (String txt) = Just txt
    getText _            = Nothing


