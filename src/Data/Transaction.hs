{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

module Data.Transaction
    (Transaction, run, Property,
     Store(..),
     lookupBS, lookup,
     insertBS, insert,
     deleteBS, delete,
     readIRef, writeIRef, newIRef,
     fromIRef, followBy, follow, anchorRef)
where

import Prelude hiding (lookup)
import Control.Applicative(Applicative)
import Control.Monad(liftM)
import Control.Monad.Trans.State.Strict(StateT, runStateT, get, modify)
import Control.Monad.Trans.Reader(ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class(MonadTrans(..))
import Control.Monad.IO.Class(MonadIO(..))
import Data.Binary(Binary)
import Data.Binary.Utils(encodeS, decodeS)
import Data.IRef(IRef)
import qualified Data.IRef as IRef
import qualified Data.Guid as Guid
import qualified Data.Property as Property
import Data.Monoid(mempty)
import Data.ByteString(ByteString)
import qualified Data.Map as Map
import Data.Map(Map)

type Property t m = Property.Property (Transaction t m)

type Key = ByteString
type Value = Maybe ByteString -- Nothing means delete, Just means insert/modify
type Changes = Map Key Value

-- 't' is a phantom-type tag meant to make sure you run Transactions
-- with the right store
data Store t m = Store {
  storeLookup :: ByteString -> m (Maybe ByteString),
  storeTransaction :: [(ByteString, Maybe ByteString)] -> m ()
  }

-- Define transformer stack:
newtype Transaction t m a = Transaction {
  unTransaction :: ReaderT (Store t m) (StateT Changes m) a
  -- TODO: To get rid of MonadIO I need to somehow remove dependency
  -- on IO from newGuid/newIRef.
  } deriving (Monad, Applicative, Functor, MonadIO)
liftReaderT :: ReaderT (Store t m) (StateT Changes m) a -> Transaction t m a
liftReaderT = Transaction
liftStateT :: Monad m => StateT Changes m a -> Transaction t m a
liftStateT = liftReaderT . lift
liftInner :: Monad m => m a -> Transaction t m a
liftInner = Transaction . lift . lift
-- TODO: Do I need the MonadTrans instance?
-- instance MonadTrans (Transaction t m) where
--   lift = liftInner

lookupBS :: Monad m => ByteString -> Transaction t m (Maybe ByteString)
lookupBS bs = do
  changes <- liftStateT get
  case Map.lookup bs changes of
    Nothing -> do
      store <- liftReaderT ask
      liftInner $ storeLookup store bs
    Just res ->
      return $ res

insertBS :: Monad m => ByteString -> ByteString -> Transaction t m ()
insertBS key = liftStateT . modify . Map.insert key . Just

deleteBS :: Monad m => ByteString -> Transaction t m ()
deleteBS key = liftStateT . modify . Map.insert key $ Nothing

delete :: Monad m => ByteString -> Transaction t m ()
delete = deleteBS

lookup :: (Monad m, Binary a) => ByteString -> Transaction t m (Maybe a)
lookup = (liftM . liftM) decodeS . lookupBS

insert :: (Monad m, Binary a) => ByteString -> a -> Transaction t m ()
insert key = insertBS key . encodeS

readIRef :: (Monad m, Binary a) => IRef a -> Transaction t m a
readIRef iref =
  liftM decodeS $ unJust =<< lookupBS (IRef.bs iref)
  where
    unJust = maybe (fail $ "IRef " ++ show iref ++ " to inexistent object dereferenced") return

writeIRef :: (Monad m, Binary a) => IRef a -> a -> Transaction t m ()
writeIRef iref = insert (IRef.bs iref)

fromIRef :: (Monad m, Binary a) => IRef a -> Property t m a
fromIRef iref = Property.Property (readIRef iref) (writeIRef iref)

newIRef :: (MonadIO m, Binary a) => a -> Transaction t m (IRef a)
newIRef val = do
  newGuid <- liftInner . liftIO $ Guid.new
  insert (Guid.bs newGuid) val
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

run :: Monad m => Store t m -> Transaction t m a -> m a
run store transaction = do
  (res, changes) <- (`runStateT` mempty) . (`runReaderT` store) . unTransaction $ transaction
  storeTransaction store (Map.toList changes)
  return res
