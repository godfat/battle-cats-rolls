
module Main where

import GHC.Conc (numCapabilities)
import Data.Word (Word32)
import Control.Monad (join)
import Control.Arrow ((***))

import Roll
import Seed
import Seeker
import Worker

main = do
  -- putStrLn $ "Seeking in " ++ show numCapabilities ++ " cores..."
  result <- map read <$> words <$> getContents :: IO [Word32]
  let picks = sourcePicks (buildSource result)
  workStart picks numCapabilities >>=
    sequence . fmap (putStrLn . show . join (***) fromSeed)
