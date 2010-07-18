{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# OPTIONS -O2 -Wall #-}
module Data.Rev.Change
    (Key, Value,
     Change(Change), make,
     Dir, objectKey, oldValue, newValue)
where

import Control.Monad(liftM3)
import Data.ByteString(ByteString)
import Data.Binary(Binary(..))
import Data.Record.Label((:->), mkLabels, label)
import Data.Guid(Guid)

type Key = Guid
type Value = ByteString

-- TODO: Store the smaller of (Maybe Value) and (IRef Value)
data Change = Change {
  _objectKey :: Key,
  _oldValue :: Maybe Value,
  _newValue :: Maybe Value
  }
  deriving (Eq, Ord, Show, Read)
$(mkLabels [''Change])
objectKey :: Change :-> Key
type Dir = Change :-> Maybe Value
oldValue :: Dir
newValue :: Dir
instance Binary Change where
  get = liftM3 Change get get get
  put (Change key old new) = put key >> put old >> put new

make :: Key -> Maybe Value -> Maybe Value -> Change
make = Change