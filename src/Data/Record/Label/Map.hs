{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Record.Label.Map(value) where

import qualified Data.Map          as Map
import           Data.Map          (Map)
import           Data.Record.Label ((:->), label)

value :: Ord k => k -> Map k v :-> Maybe v
value k = label (Map.lookup k) ((`Map.alter` k) . const)
