
module Roll
  ( Roll(Roll)
  , Rarity(Rarity, begin, end, count)
  , Slot(Slot)
  , Source(Source, sourceRolls)
  , scoreBase
  , buildSource) where

import Data.Word (Word32)

data Roll = Roll Rarity Slot
  deriving (Show, Eq)

data Rarity = Rarity { begin :: Word32, end :: Word32, count :: Word32 }
  deriving (Show, Eq)

newtype Slot = Slot Word32
  deriving (Show, Eq)

scoreBase :: Word32
scoreBase = 10000

data Source = Source
  { rare :: Rarity
  , superRare :: Rarity
  , uber :: Rarity
  , sourceRolls :: [Roll]
  } deriving (Show, Eq)

buildSource :: [Word32] -> Source
buildSource (r:sr:ssr:rCount:srCount:ssrCount:rolls) = Source
  { rare = rare
  , superRare = superRare
  , uber = uber
  , sourceRolls = buildRolls [rare, superRare, uber] rolls
  } where
  rare = Rarity { begin = 0, end = r, count = rCount }
  superRare = Rarity { begin = r, end = r + sr, count = srCount }
  uber = Rarity { begin = r + sr, end = scoreBase, count = ssrCount }

buildRolls :: [Rarity] -> [Word32] -> [Roll]
buildRolls rarities [] = []
buildRolls rarities (rarity:slot:restRolls) =
  Roll (rarities !! fromEnum rarity) (Slot slot) : rest
  where
    rest = buildRolls rarities restRolls
