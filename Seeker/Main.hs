
module Main where

import GHC.Conc (numCapabilities)
import Data.Word (Word32)
import Control.Applicative (empty)
import Control.Monad (join)
import Control.Arrow ((***))
import System.Posix.Signals (installHandler, sigINT, Handler(CatchOnce))

import Roll
import Seed
import Seeker
import Worker

main = do
  -- Ignore sigINT once so we can gracefully shutting it down
  -- when the parent process receives sigINT
  installHandler sigINT (CatchOnce empty) Nothing

  result <- map read <$> words <$> getContents :: IO [Word32]
  let picks = sourcePicks (buildSource result)
  workStart picks numCapabilities >>=
    sequence . fmap (putStrLn . show . join (***) fromSeed)
