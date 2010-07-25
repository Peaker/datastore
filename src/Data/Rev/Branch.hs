{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rev.Branch
    (Branch, new, move, curVersion, newVersion)
where

import           Control.Monad               (liftM)
import qualified Data.Transaction            as Transaction
import           Data.Transaction            (Transaction)
import           Data.Rev.Change             (Change)
import           Data.Rev.Version            (Version)
import qualified Data.Rev.Version            as Version
import           Data.Rev.ViewBranchInternal (BranchData(..), Branch(..), moveView)

move :: Monad m => Branch -> Version -> Transaction t m ()
move (Branch dataIRef) destVersion = do
  BranchData srcVersion views <- Transaction.readIRef dataIRef
  mapM_ (moveToDest srcVersion) views
  Transaction.writeIRef dataIRef (BranchData destVersion views)
  where
    moveToDest srcVersion view = moveView view srcVersion destVersion

curVersion :: Monad m => Branch -> Transaction t m Version
curVersion (Branch dataIRef) = _brVersion `liftM` Transaction.readIRef dataIRef

-- | A Branch is a mutable version ptr
new :: Monad m => Version -> Transaction t m Branch
new version = Branch `liftM`
              Transaction.newIRef (BranchData version [])

newVersion :: Monad m => Branch -> [Change] -> Transaction t m ()
newVersion branch changes = do
  version <- curVersion branch
  move branch =<< Version.newVersion version changes
