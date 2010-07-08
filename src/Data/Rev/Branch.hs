{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rev.Branch
    (Branch, new, move, curVersionIRef, newVersion)
where

import qualified Data.Store as Store
import Data.Store(Store)
import Data.Binary(Binary)
import Data.IRef(IRef)
import Data.Rev.Change(Change)
import Data.Rev.Version(Version)
import qualified Data.Rev.Version as Version

-- | A Branch is a mutable version ptr

newtype BranchData = BranchData {
  brVersionIRef :: IRef Version
  }
  deriving (Eq, Ord, Read, Show, Binary)
newtype Branch = Branch (IRef BranchData)
  deriving (Eq, Ord, Read, Show, Binary)

new :: Store d => d -> IRef Version -> IO Branch
new store versionIRef = Branch `fmap` Store.newIRef store (BranchData versionIRef)

move :: Store d => d -> Branch -> IRef Version -> IO ()
move store (Branch dataIRef) dest = Store.setIRef store dataIRef (BranchData dest)

curVersionIRef :: Store d => d -> Branch -> IO (IRef Version)
curVersionIRef store (Branch dataIRef) = brVersionIRef `fmap` Store.getIRef store dataIRef

newVersion :: Store d => d -> Branch -> [Change] -> IO ()
newVersion store branch changes = do
  versionIRef <- curVersionIRef store branch
  move store branch =<< Version.newVersion store versionIRef changes
