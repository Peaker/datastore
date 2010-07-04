{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

module Data.IRef
    (IRef, guid, unsafeFromGuid, anchorIRef,
     Store(..), lookup, insert, delete,
     StoreRef, refStore,
     get, set, modify, composeLabel,
     getIRef, setIRef, newIRef, fromIRef,
     new, follow, anchorRef)
where

import Prelude hiding (read, lookup)
import Data.Maybe(fromJust)
import Data.Binary(Binary)
import Data.Binary.Utils(encodeS, decodeS)
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
  lookupBS :: d -> ByteString -> IO (Maybe ByteString)
  insertBS :: d -> ByteString -> ByteString -> IO ()
  deleteBS :: d -> ByteString -> IO ()

lookup :: (Store d, Binary a) => d -> Guid -> IO (Maybe a)
lookup store = (fmap . fmap) decodeS . lookupBS store . Guid.bs

insert :: (Store d, Binary a) => d -> Guid -> a -> IO ()
insert store key = insertBS store (Guid.bs key) . encodeS

delete :: Store d => d -> Guid -> IO ()
delete store = deleteBS store . Guid.bs

readStoreIRef :: (Store d, Binary a) => d -> IRef a -> IO a
readStoreIRef store = fmap fromJust . lookup store . guid

writeStoreIRef :: (Store d, Binary a) => d -> IRef a -> a -> IO ()
writeStoreIRef store = insert store . guid

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

getIRef :: (Store d, Binary a) => d -> IRef a -> IO a
getIRef store = get . fromIRef store

setIRef :: (Store d, Binary a) => d -> IRef a -> IO a
setIRef store = get . fromIRef store

newIRef :: (Store d, Binary a) => d -> a -> IO (IRef a)
newIRef store val = do
  newGuid <- Guid.new
  insert store newGuid val
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
