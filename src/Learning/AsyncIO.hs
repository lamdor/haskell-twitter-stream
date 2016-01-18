-- | Learn us some async io: Parallel and Concurrent Programming in Haskell Chapter 8
-- Chapter 8. Overlapping Input/Output

module Learning.AsyncIO where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString as B
import Text.Printf (printf)

getURL :: String -> IO ByteString
getURL = undefined

geturls1 = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar

  forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Shovel"
    putMVar m1 r

  forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Spade"
    putMVar m2 r

  r1 <- takeMVar m1
  r2 <- takeMVar m2

  print (B.length r1, B.length r2)

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- try action; putMVar var r)
  return (Async var)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async var) = readMVar var

wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of
    Left e -> throwIO e
    Right a -> return a

geturls2 = do
  a1 <- async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async (getURL "http://www.wikipedia.org/wiki/Spade")
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

timeit :: IO a -> IO (a, Int)
timeit = undefined

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url   -- 1
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

geturls3 = do
  as <- mapM (async . timeDownload) sites
  mapM_ wait as


geturls5 = do
  m <- newEmptyMVar

  let
    download url = do
      r <- getURL url
      putMVar m (url, r)

  mapM_ (forkIO . download) sites

  (url, r) <- takeMVar m
  printf "%s was first (%d bytes)\n" url (B.length r)
  replicateM_ (Prelude.length sites - 1) (takeMVar m)

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = do
  m <- newEmptyMVar
  forkIO $ do r <- try (fmap Left (wait a)); putMVar m r
  forkIO $ do r <- try (fmap Right (wait b)); putMVar m r
  wait (Async m)

waitAny :: [Async a] -> IO a
waitAny as = do
  m <- newEmptyMVar
  let forkwait a = forkIO $ do r <- try (wait a); putMVar m r
  mapM_ forkwait as
  wait (Async m)

geturls6 = do
  let
    download url = do
      r <- getURL url
      return (url, r)

  as <- mapM (async . download) sites

  (url, r) <- waitAny as
  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait as
