{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rev.VersionMap
    (VersionMap, move, lookup, new)
where

-- | A Version Map is a large mapping of ObjectKeys to their
-- | "current-version" values. This serves as a "cache" which is
-- | redundant to the lists of changes stored in the version graph.

import Prelude hiding (lookup)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad(liftM, (<=<))
import qualified Data.Record.Label as Label
import Data.Guid(Guid)
import Data.Binary(Binary)
import qualified Data.Property as Property
import Data.IRef(IRef)
import qualified Data.IRef as IRef
import qualified Data.Transaction as Transaction
import Data.Transaction(Transaction)
import qualified Data.Guid as Guid
import Data.Rev.Change(Change)
import qualified Data.Rev.Change as Change
import Data.Rev.Version(Version(Version))
import qualified Data.Rev.Version as Version

newtype VersionMapData = VersionMapData (IRef Version)
  deriving (Binary)
newtype VersionMap = VersionMap {
  -- This key is XOR'd with object keys to yield the IRef to each
  -- object's current version ref:
  vmKey :: IRef VersionMapData
  }
  deriving (Eq, Ord, Binary)

makeKey :: VersionMap -> Change.Key -> Guid
makeKey = Guid.xor . IRef.guid . vmKey

lookup :: Monad m => VersionMap -> Change.Key -> Transaction t m (Maybe Change.Value)
lookup vm = Transaction.lookupBS . makeKey vm

applyChanges :: Monad m => VersionMap -> Change.Dir -> [Change] -> Transaction t m ()
applyChanges vm changeDir = mapM_ applyChange
  where
    applyChange change = setValue
                         (makeKey vm $ Label.get Change.objectKey change)
                         (Label.get changeDir change)
    setValue key Nothing      = Transaction.deleteBS key
    setValue key (Just value) = Transaction.insertBS key value

move :: Monad m => VersionMap -> IRef Version -> Transaction t m ()
move vm destVersionIRef = do
  let vmDataRef = Transaction.fromIRef . vmKey $ vm
  VersionMapData srcVersionIRef <- Property.get vmDataRef
  mraIRef <- Version.mostRecentAncestor srcVersionIRef destVersionIRef
  Version.walkUp applyBackward mraIRef srcVersionIRef
  Version.walkDown applyForward mraIRef destVersionIRef
  Property.set vmDataRef $ VersionMapData destVersionIRef
  where
    applyForward = apply Change.newValue
    applyBackward = apply Change.oldValue
    apply changeDir version = applyChanges vm changeDir . Version.changes $ version

new :: MonadIO m => IRef Version -> Transaction t m VersionMap
new versionIRef = do
  vm <- VersionMap `liftM` Transaction.newIRef (VersionMapData versionIRef)
  applyHistory vm =<< Transaction.readIRef versionIRef
  return vm
  where
    applyHistory vm (Version _ mbParentIRef changes) = do
      maybe (return ()) (applyHistory vm <=< Transaction.readIRef) mbParentIRef
      applyChanges vm Change.newValue changes
