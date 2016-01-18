module Learning.Twitter.Conduit where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import           Data.Monoid
import qualified Data.Text as T

-- $setup
-- >>> :set -XOverloadedStrings

-- Sources

-- | Example
--
-- >>> foreverSource (return 1 :: IO Int) $$ CL.take 3
-- [1,1,1]

foreverSource :: (Functor m, Monad m) => m a -> Source m a
foreverSource m =
  CL.unfoldM (const $ fmap (\a  -> Just (a,())) m) ()

-- Conduits

-- | Example
--
-- >>> CL.sourceList ["[1,2,3]"] =$= parseToJSONConduit $$ CL.consume :: IO [Value]
-- [Array [Number 1.0,Number 2.0,Number 3.0]]

parseToJSONConduit :: MonadThrow m => Conduit ByteString m Value
parseToJSONConduit = CA.conduitParser json =$= CL.map snd

-- | Example
--
-- >>> CL.sourceList ["[1,2,3]"] =$= parseToJSONConduit =$= fromJSONConduit $$ CL.consume :: IO [[Int]]
-- [[1,2,3]]

fromJSONConduit :: (Monad m, FromJSON b) => Conduit Value m b
fromJSONConduit = CL.mapM fromJSONM
  where
    fromJSONM v =
      case fromJSON v of
       Success b -> return b
       Error msg -> fail msg


-- | Example
--
-- >>> CL.sourceList [1,2,3,2,1] =$= countConduit $$ CL.consume
-- [1,2,3,4,5]

countConduit :: (Monad m) => Conduit a m Int
countConduit = scanMappendConduit (const (Sum 1)) =$= CL.map getSum


-- | Example
--
-- >>> CL.sourceList [1,2,3] =$= scanMappendConduit Sum =$= CL.map getSum $$ CL.consume
-- [1,3,6]

scanMappendConduit :: (Monad m, Monoid b) => (a -> b) -> Conduit a m b
scanMappendConduit f = void $ CL.mapAccum go mempty
  where go a b = let b' = b `mappend` f a in (b', b')

-- Sinks

-- | Example
--
-- >>> CL.sourceList [1,2] $$ printSink :: IO ()
-- 1
-- 2

printSink :: (Show a, MonadIO m) => Sink a m ()
printSink = CL.mapM_ (liftIO . print)

-- | Example
--
-- >>> CL.sourceList [T.pack "abc"] $$ putTextSink :: IO ()
-- abc

putTextSink :: (MonadIO m) => Sink T.Text m ()
putTextSink = CL.mapM_ (liftIO . putStrLn . T.unpack)
