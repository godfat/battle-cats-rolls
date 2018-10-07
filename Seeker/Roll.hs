
module Roll
  ( Pick
  , Roll(Roll)
  , Rarity(Rarity, begin, end, count)
  , Slot(Slot, DualSlot)
  , Source(Source, sourcePicks)
  , scoreBase
  , buildSource) where

import Data.Word (Word32)

type Pick = Roll
data Roll = Roll Rarity Slot
  deriving (Show, Eq)

data Rarity = Rarity { begin :: Word32, end :: Word32, count :: Word32 }
  deriving (Show, Eq)

data Slot =
  Slot Word32 |
  DualSlot Word32 Word32
  deriving (Show, Eq)

scoreBase :: Word32
scoreBase = 10000

data Chance = Chance
  { rare :: Rarity
  , superRare :: Rarity
  , uber :: Rarity
  } deriving (Show, Eq)

data Source = Source
  { sourceChance :: Chance
  , sourcePicks :: [Pick]
  } deriving (Show, Eq)

buildSource :: [Word32] -> Source
buildSource (r:sr:ssr:rCount:srCount:ssrCount:picks) = Source
  { sourceChance = chance
  , sourcePicks = buildPicks chance rolls }
  where
    chance = Chance
      { rare = rare
      , superRare = superRare
      , uber = uber
      }
    rare = Rarity { begin = 0, end = r, count = rCount }
    superRare = Rarity { begin = r, end = r + sr, count = srCount }
    uber = Rarity { begin = r + sr, end = scoreBase, count = ssrCount }
    rolls = buildRolls [rare, superRare, uber] picks

buildRolls :: [Rarity] -> [Word32] -> [Roll]
buildRolls rarities [] = []
buildRolls rarities (rarity:slot:restRolls) =
  Roll (rarities !! fromEnum rarity) (Slot slot) : rest
  where
    rest = buildRolls rarities restRolls

buildPicks :: Chance -> [Roll] -> [Pick]
buildPicks chance rolls@(firstRoll:_) =
  (buildfirstPick chance firstRoll) : (buildPicksTail chance rolls)

buildfirstPick :: Chance -> Roll -> Pick
buildfirstPick chance roll@(Roll rarity _) =
  buildSinglePick (rare chance == rarity) roll

buildPicksTail :: Chance -> [Roll] -> [Pick]
buildPicksTail chance (prevRoll:rest@(currentRoll:_)) =
  pick : buildPicksTail chance rest
  where
    pick = buildSinglePick couldDupe currentRoll
    couldDupe = detectDupe chance prevRoll currentRoll

    detectDupe :: Chance -> Roll -> Roll -> Bool
    detectDupe
      chance
      (Roll prevRarity (Slot prevSlot))
      (Roll currRarity (Slot currSlot)) =
      prevRarity == currRarity &&
        rare chance == prevRarity &&
        prevSlot == dupeSlot currRarity currSlot

buildPicksTail _ _ = []

buildSinglePick :: Bool -> Roll -> Pick
buildSinglePick couldDupe roll@(Roll rarity (Slot slotCode)) =
  if couldDupe then
    Roll rarity (DualSlot slotCode (dupeSlot rarity slotCode))
  else
    roll

dupeSlot :: Rarity -> Word32 -> Word32
dupeSlot rarity slotCode =
  if slotCode < maxSlot then slotCode + 1 else 0
  where
    maxSlot = count rarity - 1
