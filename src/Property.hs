{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Property(Property(..), composeLabel) where

import Control.Monad(liftM)
import Data.Record.Label((:->))
import qualified Data.Record.Label as Label

data Property m a = Property {
  get :: m a,
  set :: a -> m ()
  }

composeLabel :: Monad m => Property m a -> (a :-> b) -> Property m b
composeLabel (Property getter setter) label = Property getter' setter'
  where
    getter' = Label.get label `liftM` getter
    setter' x = setter . Label.set label x =<< getter
