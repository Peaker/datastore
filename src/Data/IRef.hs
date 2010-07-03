{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TypeFamilies #-}

module Data.IRef
    (IRef, guid, unsafeFromGuid,
     Property(..), modify,
     Ref(..), new, follow,
     anchorRef, anchorIRef)
where

import Prelude hiding (read)
import Data.Binary(Binary)
import Data.Record.Label((:->))
import Data.Guid(Guid(Guid))
import Data.ByteString.UTF8(fromString)

newtype IRef a = IRef {
  guid :: Guid
  }
  deriving (Eq, Ord, Binary)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef a
unsafeFromGuid = IRef

class Property p where
  get :: p a -> IO a
  set :: p a -> a -> IO ()
  infixl 5 `composeLabel`
  composeLabel :: (a :-> b) -> p a -> p b

modify :: Property p => p a -> (a -> a) -> IO ()
modify p f = set p . f =<< get p

class Property r => Ref r where
  data Store r :: *
  newIRef :: Binary a => Store r -> a -> IO (IRef a)
  fromIRef :: Binary a => Store r -> IRef a -> r a

anchorIRef :: (Binary a) => String -> IRef a
anchorIRef = unsafeFromGuid . Guid . fromString

anchorRef :: (Ref r, Binary a) => Store r -> String -> r a
anchorRef store str = fromIRef store . unsafeFromGuid . Guid . fromString $ str

new :: (Binary a, Ref r) => Store r -> a -> IO (IRef a, r a)
new store val = do
  iref <- newIRef store val
  return (iref, fromIRef store iref)

-- Dereference the *current* value of the IRef (Will not track new
-- values of IRef, by-value and not by-name)
follow :: (Binary a, Ref r) => Store r -> r (IRef a) -> IO (r a)
follow store ref = fromIRef store `fmap` get ref
