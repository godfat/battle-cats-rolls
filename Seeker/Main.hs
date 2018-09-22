
module Main where

import GHC.Conc (numCapabilities)
import Data.Word (Word32)

import Roll
import Seed
import Seeker
import Worker

main = do
  -- putStrLn $ "Seeking in " ++ show numCapabilities ++ " cores..."
  result <- map read <$> words <$> getContents :: IO [Word32]
  let rolls = sourceRolls (buildSource result)
  workStart rolls numCapabilities >>=
    sequence . fmap (putStrLn . show . fromSeed)
