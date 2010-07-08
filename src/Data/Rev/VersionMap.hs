{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rev.VersionMap
    (VersionMap, move, lookup, new)
where

-- | A Version Map is a large mapping of ObjectKeys to their
-- | "current-version" values. This serves as a "cache" which is
-- | redundant to the lists of changes stored in the version graph.

import Prelude hiding (lookup)
import Control.Monad((<=<))
import qualified Data.Record.Label as Label
import Data.ByteString(ByteString)
import Data.ByteString.Utils(xorBS)
import Data.Binary(Binary)
import Data.Store(Store)
import Data.IRef(IRef)
import qualified Data.IRef as IRef
import qualified Data.Guid as Guid
import qualified Data.Store as Store
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

makeKey :: VersionMap -> Change.Key -> ByteString
makeKey = xorBS . Guid.bs . IRef.guid . vmKey

lookup :: Store d => d -> VersionMap -> Change.Key -> IO (Maybe Change.Value)
lookup store vm = Store.lookupBS store . makeKey vm

applyChanges :: Store d => d -> VersionMap -> Change.Dir -> [Change] -> IO ()
applyChanges store vm changeDir = mapM_ applyChange
  where
    applyChange change = setValue
                         (makeKey vm $ Label.get Change.objectKey change)
                         (Label.get changeDir change)
    setValue key Nothing      = Store.deleteBS store key
    setValue key (Just value) = Store.insertBS store key value

move :: Store d => d -> VersionMap -> IRef Version -> IO ()
move store vm destVersionIRef = do
  let vmDataRef = Store.fromIRef store . vmKey $ vm
  VersionMapData srcVersionIRef <- Store.get vmDataRef
  mraIRef <- Version.mostRecentAncestor store srcVersionIRef destVersionIRef
  Version.walkUp store applyBackward mraIRef srcVersionIRef
  Version.walkDown store applyForward mraIRef destVersionIRef
  Store.set vmDataRef $ VersionMapData destVersionIRef
  where
    applyForward = apply Change.newValue
    applyBackward = apply Change.oldValue
    apply changeDir version = applyChanges store vm changeDir . Version.changes $ version

new :: Store d => d -> IRef Version -> IO VersionMap
new store versionIRef = do
  vm <- VersionMap `fmap` Store.newIRef store (VersionMapData versionIRef)
  applyHistory vm =<< Store.getIRef store versionIRef
  return vm
  where
    applyHistory vm (Version _ mbParentIRef changes) = do
      maybe (return ()) (applyHistory vm <=< Store.getIRef store) mbParentIRef
      applyChanges store vm Change.newValue changes
