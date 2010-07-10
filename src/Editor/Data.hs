{-# OPTIONS -O2 -Wall #-}
module Editor.Data(
    Tree(..), nodeValueRef, nodeChildrenRefs,
    ITree, ITreeD, TreeD, Data,
    makeValueRef, makeNodeRef, makeLeafRef)
where

import Data.IRef(IRef)
import Data.Store(Store)
import qualified Data.Store as Store
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import Data.Store.Tree(Tree(..), nodeValueRef, nodeChildrenRefs)

type Data = ((Grid.Model, Grid.Model), TextEdit.Model)
type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValueRef :: Store d => d -> String -> IO (IRef Data)
makeValueRef store text = Store.newIRef store ((Grid.initModel, Grid.initModel), TextEdit.initModel text)

makeNodeRef :: Store d => d -> String -> [ITreeD] -> IO ITreeD
makeNodeRef store text childrenRefs = do
  ref <- makeValueRef store text
  Store.newIRef store $ Node ref childrenRefs

makeLeafRef :: Store d => d -> String -> IO ITreeD
makeLeafRef db text = makeNodeRef db text []
