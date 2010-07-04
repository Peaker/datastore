{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Transaction
    (Transaction, withTransaction)
where

import Data.Monoid(mempty, mappend)
import Data.Store(Store(..))
import Data.IORef(IORef, newIORef, modifyIORef, readIORef)
import Data.ByteString(ByteString)
import qualified Data.Map as Map
import Data.Map(Map)

data Transaction d = Transaction {
  tStore :: d,
  tChanges :: IORef (Map ByteString (Maybe ByteString))
  }

withTransaction :: Store d => d -> (Transaction d -> IO a) -> IO a
withTransaction store func = do
  changes <- newIORef mempty
  result <- func (Transaction store changes)
  storeChanges store . Map.toList =<< readIORef changes
  return result

instance Store d => Store (Transaction d) where
  lookupBS trans key = do
    changes <- readIORef . tChanges $ trans
    maybe (lookupBS (tStore trans) key) return $ Map.lookup key changes
  storeChanges trans = modifyIORef (tChanges trans) . flip mappend . Map.fromList
