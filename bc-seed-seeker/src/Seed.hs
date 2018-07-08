
module Seed(advance) where

import Data.Bits (xor, shiftL, shiftR)
import Data.Int (Int32)

step :: (Int32 -> Int32) -> Int32 -> Int32
step direction seed = seed `xor` (direction seed)

advance :: Int32 -> Int32
advance = (step (`shiftL` 15)) . (step (`shiftR` 17)) . (step (`shiftL` 13))
