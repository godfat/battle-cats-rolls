
module Worker where

import Control.Concurrent

import Roll
import Seed
import Seeker

workStart :: [Pick] -> Int -> IO (Maybe Seed)
workStart picks n = do
  result <- newEmptyMVar
  threads <- sequence $ dispatch picks (seedRanges n) result
  -- putStrLn $ show $ length threads
  forkIO $ do
    wait threads
    tryPutMVar result Nothing >> return ()
  takeMVar result

  where
    wait :: [MVar ()] -> IO ()
    wait = mapM_ takeMVar

seedRanges :: Int -> [Seed]
seedRanges n =
  map Seed $ [min, min + step .. (max - step)] ++ [max] where
  min = fromSeed minSeed
  max = fromSeed maxSeed
  step = floor $ toRational max / (toRational n / 2)

dispatch :: [Pick] -> [Seed] -> MVar (Maybe Seed) -> [IO (MVar ())]
dispatch picks ranges result =
  map dispatchOne allRanges where
  allRanges = zip ranges (tail ranges)
  dispatchOne (start, end) = work picks start end result

work :: [Pick] -> Seed -> Seed -> MVar (Maybe Seed) -> IO (MVar ())
work picks startSeed endSeed result =
  forkWithMVar $ do
    case seekRange startSeed endSeed picks of
      Nothing -> return ()
      seed@(Just _) -> tryPutMVar result seed >> return ()

forkWithMVar :: IO () -> IO (MVar ())
forkWithMVar io = do
  terminated <- newEmptyMVar
  forkFinally io (const (putMVar terminated ()))
  return terminated
