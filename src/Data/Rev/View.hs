{-# OPTIONS -O2 -Wall #-}
module Data.Rev.View
    (View(..), make, curVersionIRef, curVersion, move, store)
where

import Prelude hiding (lookup)
import Control.Monad(liftM)
import Control.Monad.IO.Class(MonadIO)
import Data.IRef(IRef)
import Data.Transaction(Transaction, Store(..))
import qualified Data.Transaction as Transaction
import Data.Rev.Version(Version)
import Data.Rev.VersionMap(VersionMap)
import qualified Data.Rev.VersionMap as VersionMap
import Data.Rev.Branch(Branch)
import qualified Data.Rev.Branch as Branch
import Data.Rev.Change(Change)
import qualified Data.Rev.Change as Change

data View = View { cache :: VersionMap,
                   branch :: Branch }

make :: VersionMap -> Branch -> View
make = View

curVersionIRef :: Monad m => View -> Transaction t m (IRef Version)
curVersionIRef view = Branch.curVersionIRef (branch view)

curVersion :: Monad m => View -> Transaction t m Version
curVersion view = Transaction.readIRef =<< curVersionIRef view

move :: MonadIO m => View -> IRef Version -> Transaction t m ()
move = Branch.move . branch

newVersion :: MonadIO m => View -> [Change] -> Transaction t m ()
newVersion view = Branch.newVersion (branch view)

sync :: Monad m => View -> Transaction t m ()
sync view = VersionMap.move (cache view) =<< Branch.curVersionIRef (branch view)

-- unsafe because it assumes you sync'd:
unsafeLookupBS :: Monad m => View -> Change.Key -> Transaction t m (Maybe Change.Value)
unsafeLookupBS view = VersionMap.lookup (cache view)

lookup :: Monad m => View -> Change.Key -> Transaction t m (Maybe Change.Value)
lookup view objKey = do
  sync view
  unsafeLookupBS view objKey

transaction :: MonadIO m => View -> [(Change.Key, Maybe Change.Value)] -> Transaction t m ()
transaction _    [] = return ()
transaction view changes = do
  sync view
  newVersion view =<< (mapM $ uncurry viewMakeChange) changes
  where
    viewMakeChange key value =
      flip (Change.make key) value `liftM` unsafeLookupBS view key

-- You get a store tagged however you like...
store :: MonadIO m => View -> Store t' (Transaction t m)
store view = Store {
  storeLookup = lookup view,
  storeTransaction = transaction view
  }
