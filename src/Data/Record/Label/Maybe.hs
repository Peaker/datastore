{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Record.Label.Maybe(fromMaybe) where

import qualified Data.Maybe        as Maybe
import           Data.Record.Label ((:->), label)

fromMaybe :: a -> Maybe a :-> a
fromMaybe d = label (Maybe.fromMaybe d) (const . Just)
