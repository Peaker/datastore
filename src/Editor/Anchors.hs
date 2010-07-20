{-# LANGUAGE EmptyDataDecls #-}

module Editor.Anchors(
    clipboardIRef, rootIRef,
    focalPointIRef, branches, versionMap,
    viewGridsAnchor, dbGridsAnchor,
    dbStore, DBTag,
    viewStore, ViewTag)
where

import Control.Monad(liftM)
import Data.IRef(IRef)
import qualified Data.Transaction as Transaction
import Data.Transaction(Transaction, Store)
import Data.Rev.Branch(Branch)
import Data.Rev.VersionMap(VersionMap)
import Data.Rev.View(View)
import qualified Data.Rev.View as View
import qualified Data.Rev.Branch as Branch
import qualified Data.Rev.Version as Version
import qualified Data.Rev.VersionMap as VersionMap
import qualified Data.Property as Property
import qualified Db
import Db(Db)
import Editor.Data(ITreeD)
import qualified Editor.Data as Data
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: Monad m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

clipboardIRef :: Monad m => Transaction.Property ViewTag m (IRef [ITreeD])
clipboardIRef = Transaction.anchorRefDef "clipboard" $ Transaction.newIRef []

rootIRef :: Monad m => Transaction.Property ViewTag m ITreeD
rootIRef = Transaction.anchorRefDef "root" $
           Data.makeNodeRef "tree root value" =<<
           mapM (Data.makeLeafRef . show) [1..10 :: Int]

focalPointIRef :: Monad m => Transaction.Property ViewTag m ITreeD
focalPointIRef = Transaction.anchorRefDef "focalPoint" $ Property.get rootIRef

gridsAnchor :: Monad m => String -> String -> Transaction.Property anyTag m Grid.Model
gridsAnchor name = Transaction.containerStr . Transaction.anchorContainerDef name $ return Grid.initModel

viewGridsAnchor :: Monad m => String -> Transaction.Property ViewTag m Grid.Model
viewGridsAnchor = gridsAnchor "GUI.grids(v)"

dbGridsAnchor :: Monad m => String -> Transaction.Property DBTag m Grid.Model
dbGridsAnchor = gridsAnchor "GUI.grids(d)"

branches :: Monad m => Transaction.Property DBTag m [(IRef TextEdit.Model, Branch)]
branches = Transaction.anchorRefDef "branches" $ do
  masterNameIRef <- Transaction.newIRef $ TextEdit.initModel "master"
  initialVersionIRef <- Version.makeInitialVersion
  master <- Branch.new initialVersionIRef
  return [(masterNameIRef, master)]

versionMap :: Monad m => Transaction.Property DBTag m VersionMap
versionMap = Transaction.anchorRefDef "HEAD" $
  VersionMap.new =<< Branch.curVersion =<< (snd . head) `liftM` Property.get branches
