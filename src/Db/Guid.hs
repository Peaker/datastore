{-# OPTIONS -O2 -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

module Db.Guid
    (Guid(..), guidLen, newGuid)
where

import qualified Data.ByteString as SBS
import Data.ByteString.Utils(randomBS)
import Data.Binary(Binary(..))
import Data.Binary.Get(getByteString)
import Data.Binary.Put(putByteString)

newtype Guid = Guid { guidBS :: SBS.ByteString }
  deriving (Eq, Ord, Read, Show)

guidLen :: Int
guidLen = 16

newGuid :: IO Guid
newGuid = Guid `fmap` randomBS guidLen

instance Binary Guid where
  get = Guid `fmap` getByteString guidLen
  put = putByteString . guidBS
