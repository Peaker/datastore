module Editor.Anchors(
    clipboardIRef, rootIRef,
    viewRootIRef, branches,
    branchSelector, mainGrid)
where

import Data.Binary(Binary)
import Data.IRef(IRef)
import Data.Store(Store)
import qualified Data.Store as Store
import qualified Data.Revision as Revision
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
viewRootIRef :: Store d => d -> Store.Ref d ITreeD
viewRootIRef = makeAnchor "viewroot"

-- Db key
branches :: Store d => d -> Store.Ref d [(IRef TextEdit.Model, IRef Revision.View)]
branches = makeAnchor "branches"

branchSelector :: Store d => d -> Store.Ref d Grid.Model
branchSelector = makeAnchor "branchSelector"

mainGrid :: Store d => d -> Store.Ref d Grid.Model
mainGrid = makeAnchor "GUI.mainGrid"
