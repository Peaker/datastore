{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rev.VersionMap
    (VersionMap, move, lookup, new)
where

-- | A Version Map is a large mapping of ObjectKeys to their
-- | "current-version" values. This serves as a "cache" which is
-- | redundant to the lists of changes stored in the version graph.

import Prelude hiding (lookup)
import Control.Monad(liftM, (<=<), when)
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
import Data.Rev.Version(Version)
import qualified Data.Rev.Version as Version

newtype VersionMapData = VersionMapData Version
  deriving (Binary, Eq, Ord, Show, Read)
newtype VersionMap = VersionMap {
  -- This key is XOR'd with object keys to yield the IRef to each
  -- object's current version ref:
  vmKey :: IRef VersionMapData
  }
  deriving (Eq, Ord, Binary, Show, Read)

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

move :: Monad m => VersionMap -> Version -> Transaction t m ()
move vm destVersion = do
  let vmDataRef = Transaction.fromIRef . vmKey $ vm
  VersionMapData srcVersion <- Property.get vmDataRef
  when (srcVersion /= destVersion) $ do
    mraIRef <- Version.mostRecentAncestor srcVersion destVersion
    Version.walkUp applyBackward mraIRef srcVersion
    Version.walkDown applyForward mraIRef destVersion
    Property.set vmDataRef $ VersionMapData destVersion
  where
    applyForward = apply Change.newValue
    applyBackward = apply Change.oldValue
    apply changeDir version = applyChanges vm changeDir . Version.changes $ version

new :: Monad m => Version -> Transaction t m VersionMap
new version = do
  vm <- VersionMap `liftM` Transaction.newIRef (VersionMapData version)
  applyHistory vm =<< Version.versionData version
  return vm
  where
    applyHistory vm versionData = do
      maybe (return ()) (applyHistory vm <=< Version.versionData) . Version.parent $ versionData
      applyChanges vm Change.newValue $ Version.changes versionData
