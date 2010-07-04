{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Store
    (Store(..), lookup, insert, delete,
     Ref, refStore,
     get, set, modify, composeLabel,
     getIRef, setIRef, newIRef, fromIRef,
     new, follow, anchorRef)
where

import Prelude hiding (lookup)
import Data.IRef(IRef)
import qualified Data.IRef as IRef
import Data.Maybe(fromJust)
import Data.Binary(Binary)
import Data.Binary.Utils(encodeS, decodeS)
import Data.Record.Label((:->))
import qualified Data.Record.Label as Label
import Data.ByteString.UTF8(ByteString)
import qualified Data.Guid as Guid
import Data.Guid(Guid)

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
readStoreIRef store = fmap fromJust . lookup store . IRef.guid

writeStoreIRef :: (Store d, Binary a) => d -> IRef a -> a -> IO ()
writeStoreIRef store = insert store . IRef.guid

data Ref d a = Ref {
  refStore :: d,
  get :: IO a,
  set :: a -> IO ()
  }

modify :: Ref d a -> (a -> a) -> IO ()
modify p f = set p . f =<< get p

infixl 5 `composeLabel`
composeLabel :: (a :-> b) -> Ref d a -> Ref d b
composeLabel label (Ref store getter setter) = Ref store getter' setter'
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
  return (IRef.unsafeFromGuid newGuid)

fromIRef :: (Store d, Binary a) => d -> IRef a -> Ref d a
fromIRef store iref = Ref store (readStoreIRef store iref) (writeStoreIRef store iref)

new :: (Store d, Binary a) => d -> a -> IO (IRef a, Ref d a)
new store val = do
  iref <- newIRef store val
  return (iref, fromIRef store iref)

-- Dereference the *current* value of the IRef (Will not track new
-- values of IRef, by-value and not by-name)
follow :: (Store d, Binary a) => Ref d (IRef a) -> IO (Ref d a)
follow storeRef = fromIRef (refStore storeRef) `fmap` get storeRef

anchorRef :: (Store d, Binary a) => d -> String -> Ref d a
anchorRef store = fromIRef store . IRef.anchorIRef
