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

parseToJsonConduit :: MonadThrow m => Conduit ByteString m Value
parseToJsonConduit = CB.lines =$= CA.conduitParser json =$= CL.map snd

printSink :: (Show a, MonadResource m) => Sink a m ()
printSink = CL.mapM_ (liftIO . print)

putTextSink :: (MonadResource m) => Sink T.Text m ()
putTextSink = CL.mapM_ (liftIO . putStrLn . T.unpack)

foreverSource :: (Functor m, Monad m) => m a -> Source m a
foreverSource m =
  CL.unfoldM (const $ fmap (\a  -> Just (a,())) m) ()

countConduit :: (Monad m) => Conduit a m Int
countConduit = scanMappendConduit (const (Sum 1)) =$= CL.map getSum

scanMappendConduit :: (Monad m, Monoid b) => (a -> b) -> Conduit a m b
scanMappendConduit f = CL.scanl go mempty
  where go a b = let b' = b `mappend` f a in (b', b')

scanMappendConduit' :: (Monad m, Monoid b) => (a -> b) -> Conduit a m b
scanMappendConduit' f =
  awaitAndFoldLoop mempty
  where
    mappendYieldLoop b a = do
      let b' = b `mappend` (f a)
      yield b'
      awaitAndFoldLoop b'
    awaitAndFoldLoop b =
      await >>= (maybe (return ()) (mappendYieldLoop b))
  
