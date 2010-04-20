module RandInstances()
where

import System.Random(Random(..))
import Control.Arrow(first)
import Data.Word(Word8)

instance Random Word8 where
  randomR (a,b) = first fromIntegral . randomR (fromIntegral a :: Int, fromIntegral b)
  random        = randomR (minBound, maxBound)
