{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rev.Branch
    (Branch, new, move, curVersion, newVersion)
where

import Control.Monad(liftM)
import qualified Data.Transaction as Transaction
import Data.Transaction(Transaction)
import Data.Binary(Binary)
import Data.IRef(IRef)
import Data.Rev.Change(Change)
import Data.Rev.Version(Version)
import qualified Data.Rev.Version as Version

-- | A Branch is a mutable version ptr

newtype BranchData = BranchData {
  brVersion :: Version
  }
  deriving (Eq, Ord, Read, Show, Binary)
newtype Branch = Branch (IRef BranchData)
  deriving (Eq, Ord, Read, Show, Binary)

new :: Monad m => Version -> Transaction t m Branch
new version = Branch `liftM`
              Transaction.newIRef (BranchData version)

move :: Monad m => Branch -> Version -> Transaction t m ()
move (Branch dataIRef) dest = Transaction.writeIRef dataIRef (BranchData dest)

curVersion :: Monad m => Branch -> Transaction t m Version
curVersion (Branch dataIRef) = brVersion `liftM` Transaction.readIRef dataIRef

newVersion :: Monad m => Branch -> [Change] -> Transaction t m ()
newVersion branch changes = do
  version <- curVersion branch
  move branch =<< Version.newVersion version changes
