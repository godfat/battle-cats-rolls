
module Seed(Seed(Seed), toInt32, advanceSeed, matchRoll) where

import Data.Bits (xor, shiftL, shiftR)
import Data.Int (Int32)

import Roll

newtype Seed = Seed { toInt32 :: Int32 } deriving (Show, Eq)

advanceSeed :: Seed -> Seed
advanceSeed =
  Seed .
  (step (`shiftL` 15)) .
  (step (`shiftR` 17)) .
  (step (`shiftL` 13)) .
  toInt32

matchRoll :: Seed -> Roll -> Bool
matchRoll seed (Roll rarity@(Rarity _ _ count) slot) =
  matchRarity seed rarity && matchSlot (advanceSeed seed) count slot

matchRarity :: Seed -> Rarity -> Bool
matchRarity (Seed seed) (Rarity begin end _) =
  score >= begin && score < end where
  score = abs seed `mod` scoreBase

matchSlot :: Seed -> Int32 -> Slot -> Bool
matchSlot (Seed seed) count (Slot n) =
  slot == n where
  slot = abs seed `mod` count

step :: (Int32 -> Int32) -> Int32 -> Int32
step direction seed = seed `xor` (direction seed)

------------------------------------------------

seed = Seed 1745107336
tests =
  [ advanceSeed seed == Seed 2009320978
  , matchRarity seed (Rarity 7000 9000 1)
  , not $ matchRarity seed (Rarity 0 7000 1)
  , matchSlot seed 10 (Slot 6)
  , not $ matchSlot seed 10 (Slot 7)
  , matchRoll seed (Roll (Rarity 7000 9000 100) (Slot 78))
  ]

test = foldr (&&) True tests
