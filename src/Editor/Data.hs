{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, TemplateHaskell #-}

module Editor.Data(
    Tree(..), nodeValueRef, nodeChildrenRefs,
    ITree, ITreeD, TreeD, Data,
    makeValueRef, makeNodeRef, makeLeafRef,
    clipboardIRefRef, rootIRefRef,
    viewRootIRefRef, masterViewIRefRef)
where

import Control.Monad(liftM2)
import Data.IRef(IRef, Store, StoreRef)
import qualified Data.IRef as IRef
import Data.Binary(Binary(..))
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import Data.Record.Label((:->), mkLabels, label)
import qualified Data.Revision as Revision

type Data = ((Grid.Model, Grid.Model), TextEdit.Model)
type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)
data Tree a = Node {
  _nodeValueRef :: IRef a,
  _nodeChildrenRefs :: [ITree a]
  }
$(mkLabels [''Tree])
nodeValueRef :: Tree a :-> IRef a
nodeChildrenRefs :: Tree a :-> [IRef (Tree a)]

clipboardIRefRef :: Store d => d -> StoreRef d (IRef [ITreeD])
clipboardIRefRef store = IRef.anchorRef store "clipboard"

rootIRefRef :: Store d => d -> StoreRef d ITreeD
rootIRefRef store = IRef.anchorRef store "root"

viewRootIRefRef :: Store d => d -> StoreRef d ITreeD
viewRootIRefRef store = IRef.anchorRef store "viewroot"

masterViewIRefRef :: Store d => d -> StoreRef d (IRef Revision.View)
masterViewIRefRef store = IRef.anchorRef store "master"

instance Binary (Tree a) where
  put (Node value children) = put value >> put children
  get = liftM2 Node get get

makeValueRef :: Store d => d -> String -> IO (IRef Data)
makeValueRef store text = IRef.newIRef store ((Grid.initModel, Grid.initModel), TextEdit.initModel text)

makeNodeRef :: Store d => d -> String -> [ITreeD] -> IO ITreeD
makeNodeRef store text childrenRefs = do
  ref <- makeValueRef store text
  IRef.newIRef store $ Node ref childrenRefs

makeLeafRef :: Store d => d -> String -> IO ITreeD
makeLeafRef db text = makeNodeRef db text []
