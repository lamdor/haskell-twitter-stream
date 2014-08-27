module Learning.Twitter.Conduit
       (
         foreverSource
       , parseToJsonConduit
       , resourcePrintSink
       ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.ByteString
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

parseToJsonConduit :: MonadThrow m => Conduit ByteString m Value
parseToJsonConduit = CB.lines =$= CA.conduitParser json =$= CL.map snd

resourcePrintSink :: (Show a, MonadResource m) => Sink a m ()
resourcePrintSink = CL.mapM_ (liftIO . print)

foreverSource :: (Functor m, Monad m) => m a -> Source m a
foreverSource m =
  CL.unfoldM (const $ fmap (\a  -> Just (a,())) m) ()
