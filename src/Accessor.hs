{-# OPTIONS -O2 -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}

module Accessor
    (Accessor(..), get, set, composeLabel,
     modify, modify_, pureModify)
where

import Db(Db)
import Data.Record.Label((:->))
import Property(Property)
import qualified Property

data Accessor a = Accessor {
  accessorDb :: Db,
  accessorProperty :: Property IO a
  }

atProperty :: (Property IO a ->
               Property IO b) ->
              Accessor a ->
              Accessor b
atProperty f (Accessor db prop) = Accessor db (f prop)

infixl 5 `composeLabel`
composeLabel :: (a :-> b) -> Accessor a -> Accessor b
composeLabel = atProperty . Property.composeLabel

get :: Accessor a -> IO a
get = Property.get . accessorProperty

set :: Accessor a -> a -> IO ()
set = Property.set . accessorProperty

modify :: Accessor a -> (a -> IO (a, b)) -> IO b
modify = Property.modify . accessorProperty

modify_ :: Accessor a -> (a -> IO a) -> IO ()
modify_ = Property.modify_ . accessorProperty

pureModify :: Accessor a -> (a -> a) -> IO ()
pureModify = Property.pureModify . accessorProperty
