{-# OPTIONS -O2 -Wall #-}

module Db.Guid
    (Guid(..), guidLen, newGuid, xor)
where

import qualified Data.ByteString as SBS
import Data.ByteString.Utils(randomBS, xorBS)
import Data.Binary(Binary(..))
import Data.Binary.Get(getByteString)
import Data.Binary.Put(putByteString)

newtype Guid = Guid { guidBS :: SBS.ByteString }
  deriving (Eq, Ord, Read, Show)
inGuid :: (SBS.ByteString -> SBS.ByteString) -> Guid -> Guid
inGuid f = Guid . f . guidBS
inGuid2 :: (SBS.ByteString -> SBS.ByteString -> SBS.ByteString) ->
           Guid -> Guid -> Guid
inGuid2 f = inGuid . f . guidBS

instance Binary Guid where
  get = Guid `fmap` getByteString guidLen
  put = putByteString . guidBS

guidLen :: Int
guidLen = 16

newGuid :: IO Guid
newGuid = Guid `fmap` randomBS guidLen

xor :: Guid -> Guid -> Guid
xor = inGuid2 xorBS