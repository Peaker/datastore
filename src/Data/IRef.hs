{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TypeFamilies #-}

module Data.IRef
    (IRef, guid, unsafeFromGuid, anchorIRef,
     Store, lookup, insert, delete,
     StoreRef, refStore,
     get, set, modify, composeLabel,
     newIRef, fromIRef, new, follow, anchorRef)
where

import Prelude hiding (read, lookup)
import Data.Maybe(fromJust)
import Data.Binary(Binary)
import Data.Record.Label((:->))
import qualified Data.Record.Label as Label
import Data.ByteString.UTF8(ByteString, fromString)
import Data.Guid(Guid(Guid))
import qualified Data.Guid as Guid

newtype IRef a = IRef {
  guid :: Guid
  }
  deriving (Eq, Ord, Binary)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef a
unsafeFromGuid = IRef

anchorIRef :: (Binary a) => String -> IRef a
anchorIRef = unsafeFromGuid . Guid . fromString


class Store d where
  lookup :: Binary a => d -> ByteString -> IO (Maybe a)
  insert :: Binary a => d -> ByteString -> a -> IO ()
  delete :: d -> ByteString -> IO ()


readStoreIRef :: (Store d, Binary a) => d -> IRef a -> IO a
readStoreIRef store = fmap fromJust . lookup store . Guid.bs . guid

writeStoreIRef :: (Store d, Binary a) => d -> IRef a -> a -> IO ()
writeStoreIRef store = insert store . Guid.bs . guid

data StoreRef d a = StoreRef {
  refStore :: d,
  get :: IO a,
  set :: a -> IO ()
  }

modify :: StoreRef d a -> (a -> a) -> IO ()
modify p f = set p . f =<< get p

infixl 5 `composeLabel`
composeLabel :: (a :-> b) -> StoreRef d a -> StoreRef d b
composeLabel label (StoreRef store getter setter) = StoreRef store getter' setter'
  where
    getter' = Label.get label `fmap` getter
    setter' x = setter . Label.set label x =<< getter

newIRef :: (Store d, Binary a) => d -> a -> IO (IRef a)
newIRef store val = do
  newGuid <- Guid.new
  insert store (Guid.bs newGuid) val
  return (unsafeFromGuid newGuid)

fromIRef :: (Store d, Binary a) => d -> IRef a -> StoreRef d a
fromIRef store iref = StoreRef store (readStoreIRef store iref) (writeStoreIRef store iref)

new :: (Store d, Binary a) => d -> a -> IO (IRef a, StoreRef d a)
new store val = do
  iref <- newIRef store val
  return (iref, fromIRef store iref)

-- Dereference the *current* value of the IRef (Will not track new
-- values of IRef, by-value and not by-name)
follow :: (Store d, Binary a) => StoreRef d (IRef a) -> IO (StoreRef d a)
follow storeRef = fromIRef (refStore storeRef) `fmap` get storeRef

anchorRef :: (Store d, Binary a) => d -> String -> StoreRef d a
anchorRef store = fromIRef store . unsafeFromGuid . Guid . fromString
