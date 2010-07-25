{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Record.Label.Tuple(first, second) where

import qualified Control.Arrow     as Arrow
import           Data.Record.Label ((:->), label)

first :: (a, b) :-> a
first = label fst (Arrow.first . const)

second :: (a, b) :-> b
second = label snd (Arrow.second . const)
