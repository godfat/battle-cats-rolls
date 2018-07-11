
module Seeker(seekStart) where

import Data.Set (Set, empty)

import Seed
import Roll

seekStart :: [Roll] -> Maybe Seed
seekStart = seek startSeed empty

seek :: Seed -> Set Seed -> [Roll] -> Maybe Seed
seek seed track input = undefined

matchSeed :: Maybe Seed -> [Roll] -> Maybe Seed
matchSeed currentSeed [] = currentSeed
matchSeed currentSeed (roll:nextRolls) = do
  seed <- currentSeed

  if matchRoll seed roll then
    matchSeed (Just (advanceSeed (advanceSeed seed))) nextRolls
  else
    Nothing

startSeed :: Seed
startSeed = Seed minBound

endSeed :: Seed
endSeed = Seed maxBound

seed = Seed (-448772753)
rare = Rarity { begin = 0, end = 7000, count = 23 }
sr = Rarity { begin = 7000, end = 9500, count = 16 }
uber = Rarity { begin = 9500, end = 10000, count = 4 }
rolls = [
  Roll rare (Slot 7), -- Tin Cat
  Roll rare (Slot 10), -- Swordsman Cat
  Roll rare (Slot 18), -- Viking Cat
  Roll rare (Slot 7),
  Roll rare (Slot 3), -- Onmyoji Cat
  Roll rare (Slot 17), -- Pirate Cat
  Roll rare (Slot 0), -- Rover Cat
  Roll rare (Slot 12), -- Witch Cat
  Roll rare (Slot 17),
  Roll sr (Slot 2) -- Surfer Cat
  ]
test = matchSeed (Just seed) rolls
