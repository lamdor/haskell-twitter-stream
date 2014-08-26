module Learning.Conduit where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           System.IO

source :: Monad m => Source m Int
source = do
  yield 1
  yield 2
  yield 3
  yield 4

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

conduit :: Conduit Int IO String
conduit = do
  mi1 <- await
  mi2 <- await
  case (mi1, mi2) of
   (Just i1, Just i2) -> yield $ show (i1,i2)

multiplyBy :: Monad m => Int -> Conduit Int m Int
multiplyBy i = CL.map (* i)

main = do
  source $$ (multiplyBy 2) =$ conduit =$ sink

-- Exercise: Implement sourceList in terms of yield.
sourceList :: Monad m => [a] -> Source m a
sourceList [] = return ()
sourceList (x:xs) = do
  yield x
  sourceList xs

-- Exercise: There's a helper function in the library called awaitForever. Rewrite sink above using awaitForever.
sink' :: Sink String IO ()
sink' = awaitForever $ liftIO . putStrLn

-- Exercise: Implement your own version of awaitForever.
myAwaitForever :: Monad m => (a -> Conduit a m b) -> Conduit a m b
myAwaitForever f = do
  await >>= maybe (return ()) (\v -> f v >> myAwaitForever f)
  -- mv <- await
  -- case mv of
  --  (Just v) -> f v >> myAwaitForever f
  --  Nothing  -> return ()

-- Exercise: Write a Conduit that consumes a stream of Ints. It takes the first Int from the stream, and then multiplies all subsequent Ints by that number and sends them back downstream. You should use the Data.Conduit.List.map function for this.
multiplyByFirst :: Monad m => Conduit Int m Int
multiplyByFirst = do
  await >>= maybe (return ()) (\v -> CL.map (* v))

intSourceWithCleanup :: Source IO Int
intSourceWithCleanup = do
  loop 1
  where
    loop i = do
      yieldOr i $ putStrLn $ "Terminating when yielding " ++ (show i)
      loop $ i + 1

test_intSourceWithCleanup =
  intSourceWithCleanup $$ CL.isolate 5 $= CL.mapM_ print

charFileSourceWithCleanup :: MonadResource m => Source m Char 
charFileSourceWithCleanup = do
  bracketP
    (openFile "test.txt" ReadMode)
    (\handle -> putStrLn "Closing handle" >> hClose handle)
    loop
  where
    loop handle = do
      eof <- liftIO $ hIsEOF handle
      if eof
        then return ()
        else do
          c <- liftIO $ hGetChar handle
          yield c
          loop handle

exceptionalSink :: MonadResource m => Sink Char m ()
exceptionalSink = do
  c <- await
  liftIO $ print c
  error "this throws an exception"

test_charFileSourceWithCleanup :: IO ()
test_charFileSourceWithCleanup =
  runResourceT $ charFileSourceWithCleanup $$ exceptionalSink
