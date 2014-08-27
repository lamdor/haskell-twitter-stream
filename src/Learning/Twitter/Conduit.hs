module Learning.Twitter.Conduit where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Monoid
import qualified Data.Text as T

-- Sources

foreverSource :: (Functor m, Monad m) => m a -> Source m a
foreverSource m =
  CL.unfoldM (const $ fmap (\a  -> Just (a,())) m) ()

-- Conduits

parseToJSONConduit :: MonadThrow m => Conduit ByteString m Value
parseToJSONConduit = CA.conduitParser json =$= CL.map snd

fromJSONConduit :: (Monad m, FromJSON b) => Conduit Value m b
fromJSONConduit = CL.mapM fromJSONM
  where
    fromJSONM v =
      case (fromJSON v) of
       Success b -> return b
       Error msg -> fail msg

countConduit :: (Monad m) => Conduit a m Int
countConduit = scanMappendConduit (const (Sum 1)) =$= CL.map getSum

scanMappendConduit :: (Monad m, Monoid b) => (a -> b) -> Conduit a m b
scanMappendConduit f = CL.scanl go mempty
  where go a b = let b' = b `mappend` f a in (b', b')
  
-- Sinks
    
printSink :: (Show a, MonadResource m) => Sink a m ()
printSink = CL.mapM_ (liftIO . print)

putTextSink :: (MonadResource m) => Sink T.Text m ()
putTextSink = CL.mapM_ (liftIO . putStrLn . T.unpack)

