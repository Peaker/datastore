{-# LANGUAGE EmptyDataDecls #-}

module Editor.Anchors(
    clipboardIRef, rootIRef,
    focalPointIRef, branches,
    branchSelector, versionMap, mainGrid,
    dbStore, DBTag,
    viewStore, ViewTag)
where

import Control.Monad.IO.Class(MonadIO)
import Data.IRef(IRef)
import qualified Data.Transaction as Transaction
import Data.Transaction(Transaction, Store)
import Data.Rev.Branch(Branch)
import Data.Rev.VersionMap(VersionMap)
import Data.Rev.View(View)
import qualified Data.Rev.View as View
import qualified Db
import Db(Db)
import Editor.Data(ITreeD)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: MonadIO m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

clipboardIRef :: Monad m => Transaction.Property ViewTag m (IRef [ITreeD])
clipboardIRef = Transaction.anchorRef "clipboard"

rootIRef :: Monad m => Transaction.Property ViewTag m ITreeD
rootIRef = Transaction.anchorRef "root"

focalPointIRef :: Monad m => Transaction.Property ViewTag m ITreeD
focalPointIRef = Transaction.anchorRef "focalPoint"

branches :: Monad m => Transaction.Property DBTag m [(IRef TextEdit.Model, Branch)]
branches = Transaction.anchorRef "branches"

branchSelector :: Monad m => Transaction.Property DBTag m Grid.Model
branchSelector = Transaction.anchorRef "branchSelector"

versionMap :: Monad m => Transaction.Property DBTag m VersionMap
versionMap = Transaction.anchorRef "HEAD"

mainGrid :: Monad m => Transaction.Property DBTag m Grid.Model
mainGrid = Transaction.anchorRef "GUI.mainGrid"
