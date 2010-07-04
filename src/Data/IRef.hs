{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

module Data.IRef
    (IRef, guid, unsafeFromGuid, anchorIRef)
where

import Data.Binary(Binary)
import Data.ByteString.UTF8(fromString)
import Data.Guid(Guid(Guid))

newtype IRef a = IRef {
  guid :: Guid
  }
  deriving (Eq, Ord, Binary)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef a
unsafeFromGuid = IRef

anchorIRef :: (Binary a) => String -> IRef a
anchorIRef = unsafeFromGuid . Guid . fromString
