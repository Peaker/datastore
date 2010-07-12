{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

module Data.Transaction
    (Transaction, withTransaction)
where

import Control.Applicative(Applicative)
import Control.Monad.Trans.State.Strict(StateT, evalStateT)
import Control.Monad.Trans.Reader(ReaderT, runReaderT)
import Control.Monad.Trans.Class(MonadTrans(..))
import Control.Monad.IO.Class(MonadIO)
import Data.Monoid(mempty, mappend)
import Data.Store(Store(..))
import Data.IORef(IORef, newIORef, modifyIORef, readIORef)
import Data.ByteString(ByteString)
import qualified Data.Map as Map
import Data.Map(Map)

-- Define transformer stack:
newtype Transaction d m a = Transaction {
  unTransaction :: ReaderT d (StateT (Map ByteString (Maybe ByteString)) m) a
  } deriving (MonadIO, Monad, Applicative, Functor)
liftReaderT :: ReaderT d (StateT (Map ByteString (Maybe ByteString)) m) a -> Transaction d m a
liftReaderT = Transaction
liftStateT :: Monad m => StateT (Map ByteString (Maybe ByteString)) m a -> Transaction d m a
liftStateT = liftReaderT . lift
instance MonadTrans (Transaction d) where
  lift = Transaction . lift . lift

runTransaction :: (Store d, Monad m) => d -> Transaction d m a -> m a
runTransaction store =
  storeChanges store . Map.toList =<<
  (`evalStateT` mempty) .
  (`runReaderT` store) .
  unTransaction

withTransaction :: (Monad m, Store d) => d -> (Transaction d m a -> m a) -> m a
withTransaction store func = do
  changes <- newIORef mempty
  result <- func (Transaction store changes)
  storeChanges store . Map.toList =<< readIORef changes
  return result

-- instance Store d => Store (Transaction d) where
--   lookupBS trans key = do
--     changes <- readIORef . tChanges $ trans
--     maybe (lookupBS (tStore trans) key) return $ Map.lookup key changes
--   storeChanges trans = modifyIORef (tChanges trans) . flip mappend . Map.fromList
