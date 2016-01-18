-- | Learn us some conrrency: Parallel and Concurrent Programming in Haskell Chapter 7. Basic
-- Concurrency: Threads and MVars http://chimera.labs.oreilly.com/books/1230000000929/ch07.html
module Learning.Concurrency where

import           Control.Concurrent hiding (Chan)
import           Control.Monad
import qualified Data.Map as Map
import           Prelude hiding (lookup)
import           System.IO

forkInterleavePutChars = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 10000 (putChar 'A'))
  replicateM_ 10000 (putChar 'B')

takeAndPutMVar = do
  m <- newEmptyMVar
  forkIO $ do
    putMVar m 'x'
    putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r

-- MVar as a Simple Channel: A Logging Service
data Logger = Logger (MVar LogCommand)

data LogCommand = Message String
                | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message s -> do
          putStrLn s
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

testLogger = do
  l <- initLogger
  logMessage l "1"
  logMessage l "2"
  logMessage l "3"
  logStop l

-- MVar as a Container for Shared State

type Name = String
type PhoneNumber = String
type PhoneBook = Map.Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  m <- newMVar Map.empty
  return (PhoneBookState m)

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  let book' = Map.insert name number book
  putMVar m book'
  seq book' (return ())

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return $ Map.lookup name book

testPhoneBook = do
  s <- new
  sequence_ [ insert s ("name" ++ (show n)) (show n) | n <- [1..10000]]
  lookup s "name999" >>= print
  lookup s "unkown" >>= print


-- MVar as a Building Block: Unbounded Channels

data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

newChan   :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) a = do
 newHole <- newEmptyMVar
 oldVar <- takeMVar writeVar
 putMVar oldVar (Item a newHole)
 putMVar writeVar newHole

readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  stream <- takeMVar readVar
  Item val tail <- takeMVar stream
  putMVar readVar tail
  return val

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole <- readMVar writeVar
  newReadVar <- newMVar hole
  return (Chan newReadVar writeVar)
