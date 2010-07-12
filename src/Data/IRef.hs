{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

module Data.IRef
    (IRef, guid, unsafeFromGuid, anchorIRef, bs)
where

import Data.Binary(Binary)
import Data.ByteString.UTF8(ByteString, fromString)
import Data.Guid(Guid(Guid))
import qualified Data.Guid as Guid

newtype IRef a = IRef {
  guid :: Guid
  }
  deriving (Eq, Ord, Binary, Read, Show)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef a
unsafeFromGuid = IRef

anchorIRef :: (Binary a) => String -> IRef a
anchorIRef = unsafeFromGuid . Guid . fromString

bs :: IRef a -> ByteString
bs = Guid.bs . guid
