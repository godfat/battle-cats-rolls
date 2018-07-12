
module Worker where

import Control.Concurrent

import Roll
import Seed
import Seeker

workStart :: [Roll] -> Int -> IO (Maybe Seed)
workStart rolls n = do
  result <- newEmptyMVar
  threads <- sequence $ dispatch rolls (seedRanges n) result
  putStrLn $ show $ length threads
  forkIO $ do
    wait threads
    tryPutMVar result Nothing >> return ()
  takeMVar result

  where
    wait :: [MVar ()] -> IO ()
    wait = mapM_ takeMVar

seedRanges :: Int -> [Seed]
seedRanges n =
  map Seed $ [min, min + step .. max] ++ [max] where
  min = fromSeed minSeed
  max = fromSeed maxSeed
  step = round $ toRational max / (toRational n / 2)

dispatch :: [Roll] -> [Seed] -> MVar (Maybe Seed) -> [IO (MVar ())]
dispatch _ (_:[]) _ = []
dispatch rolls (start:end:rest) result =
  work rolls start end result : dispatch rolls (end:rest) result

work :: [Roll] -> Seed -> Seed -> MVar (Maybe Seed) -> IO (MVar ())
work rolls startSeed endSeed result =
  forkWithMVar $ do
    case seekRange startSeed endSeed rolls of
      Nothing -> return ()
      seed@(Just _) -> tryPutMVar result seed >> return ()

forkWithMVar :: IO () -> IO (MVar ())
forkWithMVar io = do
  terminated <- newEmptyMVar
  forkFinally io (const (putMVar terminated ()))
  return terminated
