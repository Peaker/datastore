module Editor.Anchors(
    clipboardIRef, rootIRef,
    focalPointIRef, branches,
    branchSelector, versionMap, mainGrid)
where

import Data.IRef(IRef)
import qualified Data.Transaction as Transaction
import Data.Rev.Branch(Branch)
import Data.Rev.VersionMap(VersionMap)
import Editor.Data(ITreeD)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

clipboardIRef :: Monad m => Transaction.Property m (IRef [ITreeD])
clipboardIRef = Transaction.anchorRef "clipboard"

-- Revision-store key
rootIRef :: Monad m => Transaction.Property m ITreeD
rootIRef = Transaction.anchorRef "root"

-- Revision-store key
focalPointIRef :: Monad m => Transaction.Property m ITreeD
focalPointIRef = Transaction.anchorRef "focalPoint"

-- Db key
branches :: Monad m => Transaction.Property m [(IRef TextEdit.Model, Branch)]
branches = Transaction.anchorRef "branches"

branchSelector :: Monad m => Transaction.Property m Grid.Model
branchSelector = Transaction.anchorRef "branchSelector"

versionMap :: Monad m => Transaction.Property m VersionMap
versionMap = Transaction.anchorRef "HEAD"

mainGrid :: Monad m => Transaction.Property m Grid.Model
mainGrid = Transaction.anchorRef "GUI.mainGrid"
