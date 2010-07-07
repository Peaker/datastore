{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Data.Store as Store
import Data.Store(Store)
import qualified Data.Revision as Revision
import qualified Data.Transaction as Transaction
import qualified Db
import qualified Editor.Data as Data
import qualified Editor.Anchors as Anchors
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

makeViewRef :: Store d => d -> IO (Revision.ViewRef d)
makeViewRef store = do
  versionIRef <- Revision.makeInitialVersion store
  Revision.makeView store versionIRef

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \dbStore -> do
    masterRef <- makeViewRef dbStore

    branchNameIRef <- Store.newIRef dbStore $ TextEdit.initModel "master"
    Store.set (Anchors.branches dbStore) $
      [(branchNameIRef, Revision.viewIRef masterRef)]
    Store.set (Anchors.branchSelector dbStore) Grid.initModel
    Store.set (Anchors.mainGrid dbStore) Grid.initModel
    Transaction.withTransaction masterRef $ \store -> do
      childrenRefs <- mapM (Data.makeLeafRef store . show) [1..10 :: Int]
      rootIRef <- Data.makeNodeRef store "tree root value" childrenRefs
      Store.set (Anchors.rootIRef store) rootIRef
      Store.set (Anchors.viewRootIRef store) rootIRef
      Store.set (Anchors.clipboardIRef store) =<< Store.newIRef store []
