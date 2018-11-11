
module Roll
  ( Pick
  , Roll(Roll)
  , Rarity(Rarity, begin, end, count)
  , Slot(Slot, DualSlot)
  , Source(Source, sourcePicks)
  , scoreBase
  , buildSource) where

import Data.Word (Word32)
import Data.List (genericIndex)
import Control.Monad (guard)

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
  , supa :: Rarity
  , uber :: Rarity
  , legend :: Rarity
  } deriving (Show, Eq)

data Source = Source
  { sourceChance :: Chance
  , sourcePicks :: [Pick]
  } deriving (Show, Eq)

buildSource :: [Word32] -> Source
buildSource (r:s:u:l:rCount:sCount:uCount:lCount:picks) = Source
  { sourceChance = chance
  , sourcePicks = buildPicks chance rolls }
  where
    chance = Chance
      { rare = rare
      , supa = supa
      , uber = uber
      , legend = legend
      }
    rare   = Rarity { count = rCount, begin = 0        , end = r }
    supa   = Rarity { count = sCount, begin = r        , end = r + s }
    uber   = Rarity { count = uCount, begin = r + s    , end = r + s + u }
    legend = Rarity { count = lCount, begin = r + s + u, end = scoreBase }
    rolls = buildRolls [rare, supa, uber, legend] picks

buildRolls :: [Rarity] -> [Word32] -> [Roll]
buildRolls rarities [] = []
buildRolls rarities (rarity:slot:restRolls) =
  Roll (genericIndex rarities (rarity - rarityOffset)) (Slot slot) : rest
  where
    rest = buildRolls rarities restRolls
    rarityOffset = 2

buildPicks :: Chance -> [Roll] -> [Pick]
buildPicks chance rolls@(firstRoll:_) =
  (buildfirstPick chance firstRoll) : (buildPicksTail chance rolls)

buildfirstPick :: Chance -> Roll -> Pick
buildfirstPick chance roll@(Roll rarity (Slot slotCode)) =
  if rare chance == rarity then
    buildSinglePick (Just $ dupeCode rarity slotCode) roll
  else
    roll

buildPicksTail :: Chance -> [Roll] -> [Pick]
buildPicksTail chance (previousRoll:rest@(currentRoll:_)) =
  pick : buildPicksTail chance rest
  where
    pick = buildSinglePick maybeDupe currentRoll
    maybeDupe = do
      guard $ currRarity == prevRarity &&
        currRarity == rare chance &&
        prevCode == dupeCode currRarity currCode
      return prevCode
    Roll prevRarity (Slot prevCode) = previousRoll
    Roll currRarity (Slot currCode) = currentRoll

buildPicksTail _ _ = []

buildSinglePick :: Maybe Word32 -> Roll -> Pick
buildSinglePick Nothing roll = roll
buildSinglePick (Just dupeCode) (Roll rarity (Slot slotCode)) =
  Roll rarity (DualSlot slotCode dupeCode)

dupeCode :: Rarity -> Word32 -> Word32
dupeCode rarity slotCode =
  if slotCode < maxSlot then slotCode + 1 else 0
  where
    maxSlot = count rarity - 1
