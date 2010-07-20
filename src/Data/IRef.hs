{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

module Data.IRef
    (IRef, guid, unsafeFromGuid, anchor)
where

import Data.Binary(Binary)
import Data.Guid(Guid)
import qualified Data.Guid as Guid

newtype IRef a = IRef {
  guid :: Guid
  }
  deriving (Eq, Ord, Binary, Read, Show)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef a
unsafeFromGuid = IRef

anchor :: (Binary a) => String -> IRef a
anchor = unsafeFromGuid . Guid.fromString
