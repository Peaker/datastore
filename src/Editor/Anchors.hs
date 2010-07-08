module Editor.Anchors(
    clipboardIRef, rootIRef,
    focalPointIRef, views,
    viewSelector, mainGrid)
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
focalPointIRef :: Store d => d -> Store.Ref d ITreeD
focalPointIRef = makeAnchor "viewroot"

-- Db key
views :: Store d => d -> Store.Ref d [(IRef TextEdit.Model, IRef Revision.View)]
views = makeAnchor "views"

viewSelector :: Store d => d -> Store.Ref d Grid.Model
viewSelector = makeAnchor "viewSelector"

mainGrid :: Store d => d -> Store.Ref d Grid.Model
mainGrid = makeAnchor "GUI.mainGrid"