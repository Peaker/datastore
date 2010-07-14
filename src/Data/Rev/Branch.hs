{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rev.Branch
    (Branch, new, move, curVersionIRef, newVersion)
where

import Control.Monad(liftM)
import Control.Monad.IO.Class(MonadIO)
import qualified Data.Transaction as Transaction
import Data.Transaction(Transaction)
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

new :: MonadIO m => IRef Version -> Transaction t m Branch
new versionIRef = Branch `liftM`
                  Transaction.newIRef (BranchData versionIRef)

move :: Monad m => Branch -> IRef Version -> Transaction t m ()
move (Branch dataIRef) dest = Transaction.writeIRef dataIRef (BranchData dest)

curVersionIRef :: Monad m => Branch -> Transaction t m (IRef Version)
curVersionIRef (Branch dataIRef) = brVersionIRef `liftM` Transaction.readIRef dataIRef

newVersion :: MonadIO m => Branch -> [Change] -> Transaction t m ()
newVersion branch changes = do
  versionIRef <- curVersionIRef branch
  move branch =<< Version.newVersion versionIRef changes
