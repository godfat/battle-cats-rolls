
module Score(Score) where

import Data.Int (Int32)

base :: Int32
base = 10000

data Score =
  Rarity { begin :: Int32, end :: Int32 }
  | Slot { n :: Int32, count :: Int32 }
  | Seed { seed :: Int32 }
  deriving (Show)

instance Eq Score where
  Seed seed == Rarity begin end =
    score >= begin && score < end where
    score = seed `mod` base

  Seed seed == Slot n count =
    slot == n where
    slot = seed `mod` count

  Seed a == Seed b = a == b
  Rarity a0 a1 == Rarity b0 b1 = a0 == b0 && a1 == b1
  Slot a0 a1 == Slot b0 b1 = a0 == b0 && a1 == b1

  rarity@(Rarity _ _) == seed@(Seed _) = seed == rarity
  slot@(Slot _ _) == seed@(Seed _) = seed == slot

cycle :: [Score]
cycle = undefined

allCycles :: [[Score]]
allCycles = undefined

rolls :: [Score]
rolls = undefined
