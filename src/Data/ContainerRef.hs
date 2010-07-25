{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ContainerRef
    (ContainerRef, guidBase, unsafeFromGuid, anchor)
where

import           Data.Binary (Binary)
import           Data.Guid   (Guid)
import qualified Data.Guid   as Guid

newtype ContainerRef a = ContainerRef { guidBase :: Guid }
  deriving (Eq, Ord, Binary, Read, Show)

unsafeFromGuid :: Guid -> ContainerRef a
unsafeFromGuid = ContainerRef

anchor :: (Binary a) => String -> ContainerRef a
anchor = unsafeFromGuid . Guid.fromString
