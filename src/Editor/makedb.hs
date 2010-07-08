{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Data.Store as Store
import qualified Data.Rev.Version as Version
import qualified Data.Rev.VersionMap as VersionMap
import qualified Data.Rev.View as View
import qualified Data.Rev.Branch as Branch
import qualified Data.Transaction as Transaction
import qualified Db
import qualified Editor.Data as Data
import qualified Editor.Anchors as Anchors
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \dbStore -> do
    initialVersionIRef <- Version.makeInitialVersion dbStore

    masterNameIRef <- Store.newIRef dbStore $ TextEdit.initModel "master"
    master <- Branch.new dbStore initialVersionIRef

    versionMap <- VersionMap.new dbStore initialVersionIRef
    let view = View.make dbStore versionMap master

    -- Db-level anchors
    Store.set (Anchors.branches dbStore) [(masterNameIRef, master)]
    Store.set (Anchors.versionMap dbStore) $ versionMap
    Store.set (Anchors.branchSelector dbStore) Grid.initModel
    Store.set (Anchors.mainGrid dbStore) Grid.initModel

    Transaction.withTransaction view $ \store -> do
      childrenRefs <- mapM (Data.makeLeafRef store . show) [1..10 :: Int]
      rootIRef <- Data.makeNodeRef store "tree root value" childrenRefs
      Store.set (Anchors.rootIRef store) rootIRef
      Store.set (Anchors.focalPointIRef store) rootIRef
      Store.set (Anchors.clipboardIRef store) =<< Store.newIRef store []

