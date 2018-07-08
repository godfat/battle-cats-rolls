
module Score(Score(Rarity, Slot), scoreBase) where

import Data.Int (Int32)

scoreBase :: Int32
scoreBase = 10000

data Score =
  Rarity { begin :: Int32, end :: Int32 }
  | Slot { n :: Int32, count :: Int32 }
  deriving (Show, Eq)
