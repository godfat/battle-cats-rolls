
module Roll(Roll(Roll), Rarity(Rarity), Slot(Slot), scoreBase) where

import Data.Int (Int32)

data Roll = Roll Rarity Slot
  deriving (Show, Eq)

data Rarity = Rarity { begin :: Int32, end :: Int32, count :: Int32 }
  deriving (Show, Eq)

newtype Slot = Slot Int32
  deriving (Show, Eq)

scoreBase :: Int32
scoreBase = 10000
