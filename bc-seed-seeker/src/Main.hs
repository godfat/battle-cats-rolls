
module Main where

import GHC.Conc (numCapabilities)

import Seeker
import Worker

main = do
  putStrLn $ "Seeking in " ++ show numCapabilities ++ " cores..."
  workStart rollsB numCapabilities >>= putStrLn . show
