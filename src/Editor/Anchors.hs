module Editor.Anchors(
    clipboardIRef, rootIRef,
    focalPointIRef, branches,
    branchSelector, versionMap, mainGrid)
where

import Data.Binary(Binary)
import Data.IRef(IRef)
import Data.Store(Store)
import qualified Data.Store as Store
import Data.Rev.Branch(Branch)
import Data.Rev.VersionMap(VersionMap)
import Editor.Data(ITreeD)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

makeAnchor :: (Store d, Binary a) => String -> d -> Store.Ref d a
makeAnchor = flip Store.anchorRef

clipboardIRef :: Store d => d -> Store.Ref d (IRef [ITreeD])
clipboardIRef = makeAnchor "clipboard"

-- Revision-store key
rootIRef :: Store d => d -> Store.Ref d ITreeD
rootIRef = makeAnchor "root"

-- Revision-store key
focalPointIRef :: Store d => d -> Store.Ref d ITreeD
focalPointIRef = makeAnchor "focalPoint"

-- Db key
branches :: Store d => d -> Store.Ref d [(IRef TextEdit.Model, Branch)]
branches = makeAnchor "branches"

branchSelector :: Store d => d -> Store.Ref d Grid.Model
branchSelector = makeAnchor "branchSelector"

versionMap :: Store d => d -> Store.Ref d VersionMap
versionMap = makeAnchor "HEAD"

mainGrid :: Store d => d -> Store.Ref d Grid.Model
mainGrid = makeAnchor "GUI.mainGrid"
