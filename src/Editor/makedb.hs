{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Data.Rev.Version as Version
import qualified Data.Rev.VersionMap as VersionMap
import qualified Data.Rev.View as View
import qualified Data.Rev.Branch as Branch
import qualified Data.Transaction as Transaction
import qualified Data.Property as Property
import qualified Db
import qualified Editor.Data as Data
import qualified Editor.Anchors as Anchors
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \db -> Transaction.run (Db.store db) $ do
    initialVersionIRef <- Version.makeInitialVersion

    masterNameIRef <- Transaction.newIRef $ TextEdit.initModel "master"
    master <- Branch.new initialVersionIRef

    versionMap <- VersionMap.new initialVersionIRef
    let view = View.make versionMap master

    -- Db-level anchors
    Property.set Anchors.branches [(masterNameIRef, master)]
    Property.set Anchors.versionMap versionMap
    Property.set Anchors.branchSelector Grid.initModel
    Property.set Anchors.mainGrid Grid.initModel

    Transaction.run (Anchors.viewStore view) $ do
      childrenRefs <- mapM (Data.makeLeafRef . show) [1..10 :: Int]
      rootIRef <- Data.makeNodeRef "tree root value" childrenRefs
      Property.set Anchors.rootIRef rootIRef
      Property.set Anchors.focalPointIRef rootIRef
      Property.set Anchors.clipboardIRef =<< Transaction.newIRef []
