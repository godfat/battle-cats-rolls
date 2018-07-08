
module Seed(Seed(Seed), toInt32, advance, matchRoll) where

import Data.Bits (xor, shiftL, shiftR)
import Data.Int (Int32)
import Data.Set (Set, empty)

import Score

newtype Seed = Seed { toInt32 :: Int32 } deriving (Show, Eq)

advance :: Seed -> Seed
advance =
  Seed .
  (step (`shiftL` 15)) .
  (step (`shiftR` 17)) .
  (step (`shiftL` 13)) .
  toInt32

matchRoll :: Seed -> Score -> Bool
matchRoll (Seed seed) (Rarity begin end) =
  score >= begin && score < end where
  score = seed `mod` scoreBase

matchRoll (Seed seed) (Slot n count) =
  slot == n where
  slot = seed `mod` count

step :: (Int32 -> Int32) -> Int32 -> Int32
step direction seed = seed `xor` (direction seed)

from :: Seed
from = Seed minBound

to :: Seed
to = Seed maxBound

initSet :: Set Seed
initSet = empty

rolls :: [Score]
rolls = undefined

seed = Seed 1745107336
tests = [
  advance seed == Seed 2009320978,
  matchRoll seed (Rarity 7000 9000),
  not $ matchRoll seed (Rarity 0 7000),
  matchRoll seed (Slot 6 10),
  not $ matchRoll seed (Slot 7 10)
  ]

test = foldr (&&) True tests
