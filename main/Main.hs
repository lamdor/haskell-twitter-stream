module Main where

import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Learning.Twitter.Conduit
import Learning.Twitter.OAuth
import Learning.Twitter.Stream
import Learning.Twitter.Tweet
import Learning.Twitter.URL

main :: IO ()
main = runResourceT $ 
  readTwitterStreamJSON twitterSampleURL twitterOAuth twitterCredential =$= tweetText $$ putTextSink
     


