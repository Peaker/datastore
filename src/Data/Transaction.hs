{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

module Data.Transaction
    (Transaction, run, Property,
     Store(..),
     lookupBS, lookup,
     insertBS, insert,
     deleteBS, delete,
     readIRef, readIRefDef,
     writeIRef,
     newIRef, newKey,
     fromIRef, fromIRefDef,
     followBy, follow,
     anchorRef, anchorRefDef)
where

import Prelude hiding (lookup)
import Control.Applicative(Applicative)
import Control.Monad(liftM)
import Control.Monad.Trans.State.Strict(StateT, runStateT, get, modify)
import Control.Monad.Trans.Reader(ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class(MonadTrans(..))
import Data.Binary(Binary)
import Data.Binary.Utils(encodeS, decodeS)
import Data.IRef(IRef)
import qualified Data.IRef as IRef
import Data.Guid(Guid)
import qualified Data.Property as Property
import Data.Monoid(mempty)
import Data.ByteString(ByteString)
import qualified Data.Map as Map
import Data.Map(Map)

type Property t m = Property.Property (Transaction t m)

type Key = Guid
type Value = Maybe ByteString -- Nothing means delete, Just means insert/modify
type Changes = Map Key Value

-- 't' is a phantom-type tag meant to make sure you run Transactions
-- with the right store
data Store t m = Store {
  storeNewKey :: m Key,
  storeLookup :: Key -> m Value,
  storeTransaction :: [(Key, Value)] -> m ()
  }

-- Define transformer stack:
newtype Transaction t m a = Transaction {
  unTransaction :: ReaderT (Store t m) (StateT Changes m) a
  } deriving (Monad, Applicative, Functor)
liftReaderT :: ReaderT (Store t m) (StateT Changes m) a -> Transaction t m a
liftReaderT = Transaction
liftStateT :: Monad m => StateT Changes m a -> Transaction t m a
liftStateT = liftReaderT . lift
liftInner :: Monad m => m a -> Transaction t m a
liftInner = Transaction . lift . lift
-- TODO: Do I need the MonadTrans instance?
-- instance MonadTrans (Transaction t m) where
--   lift = liftInner

lookupBS :: Monad m => Key -> Transaction t m Value
lookupBS bs = do
  changes <- liftStateT get
  case Map.lookup bs changes of
    Nothing -> do
      store <- liftReaderT ask
      liftInner $ storeLookup store bs
    Just res -> return res

insertBS :: Monad m => Key -> ByteString -> Transaction t m ()
insertBS key = liftStateT . modify . Map.insert key . Just

deleteBS :: Monad m => Key -> Transaction t m ()
deleteBS key = liftStateT . modify . Map.insert key $ Nothing

delete :: Monad m => Key -> Transaction t m ()
delete = deleteBS

lookup :: (Monad m, Binary a) => Key -> Transaction t m (Maybe a)
lookup = (liftM . liftM) decodeS . lookupBS

insert :: (Monad m, Binary a) => Key -> a -> Transaction t m ()
insert key = insertBS key . encodeS

readIRefDef :: (Monad m, Binary a) => Transaction t m a -> IRef a -> Transaction t m a
readIRefDef def iref =
  maybe writeDefaultRet (return . decodeS) =<< lookupBS (IRef.guid iref)
  where
    writeDefaultRet = do
      d <- def
      writeIRef iref d
      return d

readIRef :: (Monad m, Binary a) => IRef a -> Transaction t m a
readIRef iref =
  liftM decodeS $ unJust =<< lookupBS (IRef.guid iref)
  where
    unJust = maybe (fail $ show iref ++ " to inexistent object dereferenced") return

writeIRef :: (Monad m, Binary a) => IRef a -> a -> Transaction t m ()
writeIRef iref = insert (IRef.guid iref)

fromIRef :: (Monad m, Binary a) => IRef a -> Property t m a
fromIRef iref = Property.Property (readIRef iref) (writeIRef iref)

fromIRefDef :: (Monad m, Binary a) => Transaction t m a -> IRef a -> Property t m a
fromIRefDef def iref = Property.Property (readIRefDef def iref) (writeIRef iref)

newKey :: Monad m => Transaction t m Key
newKey = liftInner . storeNewKey =<< liftReaderT ask

newIRef :: (Monad m, Binary a) => a -> Transaction t m (IRef a)
newIRef val = do
  newGuid <- newKey
  insert newGuid val
  return (IRef.unsafeFromGuid newGuid)

followBy :: (Monad m, Binary a) =>
            (b -> IRef a) ->
            Property t m b ->
            Transaction t m (Property t m a)
followBy conv = liftM (fromIRef . conv) . Property.get

-- Dereference the *current* value of the IRef (Will not track new
-- values of IRef, by-value and not by-name)
follow :: (Monad m, Binary a) =>
          Property t m (IRef a) ->
          Transaction t m (Property t m a)
follow = followBy id

anchorRef :: (Monad m, Binary a) => String -> Property t m a
anchorRef = fromIRef . IRef.anchorIRef

anchorRefDef :: (Monad m, Binary a) => String -> Transaction t m a -> Property t m a
anchorRefDef name def = fromIRefDef def . IRef.anchorIRef $ name

run :: Monad m => Store t m -> Transaction t m a -> m a
run store transaction = do
  (res, changes) <- (`runStateT` mempty) . (`runReaderT` store) . unTransaction $ transaction
  storeTransaction store (Map.toList changes)
  return res
