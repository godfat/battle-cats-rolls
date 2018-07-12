
module Main where

import Seeker
import Worker

main = do
  putStrLn "Seeking..."
  workStart rollsB 8 >>= putStrLn . show
