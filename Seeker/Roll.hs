
module Roll
  ( Roll(Roll)
  , Rarity(Rarity, begin, end, count)
  , Slot(Slot)
  , Source(Source, sourceRolls)
  , scoreBase
  , buildSource) where

import Data.Int (Int32)

data Roll = Roll Rarity Slot
  deriving (Show, Eq)

data Rarity = Rarity { begin :: Int32, end :: Int32, count :: Int32 }
  deriving (Show, Eq)

newtype Slot = Slot Int32
  deriving (Show, Eq)

scoreBase :: Int32
scoreBase = 10000

data Source = Source
  { rare :: Rarity
  , superRare :: Rarity
  , uber :: Rarity
  , sourceRolls :: [Roll]
  } deriving (Show, Eq)

buildSource :: [Int32] -> Source
buildSource (r:sr:ssr:rCount:srCount:ssrCount:rolls) = Source
  { rare = rare
  , superRare = superRare
  , uber = uber
  , sourceRolls = buildRolls [rare, superRare, uber] rolls
  } where
  rare = Rarity { begin = 0, end = r, count = rCount }
  superRare = Rarity { begin = r, end = scoreBase - ssr, count = srCount }
  uber = Rarity { begin = scoreBase - ssr, end = scoreBase, count = ssrCount }

buildRolls :: [Rarity] -> [Int32] -> [Roll]
buildRolls rarities [] = []
buildRolls rarities (rarity:slot:restRolls) =
  Roll (rarities !! fromEnum rarity) (Slot slot) : rest
  where
    rest = buildRolls rarities restRolls
