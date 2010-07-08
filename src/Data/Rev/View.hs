{-# OPTIONS -O2 -Wall #-}
module Data.Rev.View
    (View(..), make, curVersionIRef, curVersion, move)
where

import Prelude hiding (lookup)
import qualified Data.Store as Store
import Data.IRef(IRef)
import Data.Store(Store)
import Data.Rev.Version(Version)
import Data.Rev.VersionMap(VersionMap)
import qualified Data.Rev.VersionMap as VersionMap
import Data.Rev.Branch(Branch)
import qualified Data.Rev.Branch as Branch
import Data.Rev.Change(Change)
import qualified Data.Rev.Change as Change

data View d = View { store :: d,
                     cache :: VersionMap,
                     branch :: Branch }

make :: Store d => d -> VersionMap -> Branch -> View d
make = View

curVersionIRef :: Store d => View d -> IO (IRef Version)
curVersionIRef view = Branch.curVersionIRef (store view) (branch view)

curVersion :: Store d => View d -> IO Version
curVersion view = Store.getIRef (store view) =<< curVersionIRef view

move :: Store d => View d -> IRef Version -> IO ()
move view = Branch.move (store view) (branch view)

newVersion :: Store d => View d -> [Change] -> IO ()
newVersion view = Branch.newVersion (store view) (branch view)

sync :: Store d => View d -> IO ()
sync view = VersionMap.move (store view) (cache view) =<< Branch.curVersionIRef (store view) (branch view)

-- unsafe because it assumes you sync'd:
unsafeLookupBS :: Store d => View d -> Change.Key -> IO (Maybe Change.Value)
unsafeLookupBS view = VersionMap.lookup (store view) (cache view)

instance Store d => Store (View d) where
  lookupBS view objKey = sync view >> unsafeLookupBS view objKey
  storeChanges view changes = do
    sync view
    newVersion view =<< (mapM $ uncurry viewMakeChange) changes
    where
      viewMakeChange key value =
        flip (Change.make key) value `fmap` unsafeLookupBS view key
