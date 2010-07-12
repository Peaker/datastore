{-# OPTIONS -O2 -Wall #-}
module Editor.Data(
    Tree(..), nodeValueRef, nodeChildrenRefs,
    ITree, ITreeD, TreeD, Data,
    makeValueRef, makeNodeRef, makeLeafRef)
where

import Control.Monad.IO.Class(MonadIO)
import Data.IRef(IRef)
import Data.Transaction(Transaction)
import qualified Data.Transaction as Transaction
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import Data.IRef.Tree(Tree(..), nodeValueRef, nodeChildrenRefs)

type Data = ((Grid.Model, Grid.Model), TextEdit.Model)
type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValueRef :: MonadIO m => String -> Transaction m (IRef Data)
makeValueRef text = Transaction.newIRef ((Grid.initModel, Grid.initModel), TextEdit.initModel text)

makeNodeRef :: MonadIO m => String -> [ITreeD] -> Transaction m ITreeD
makeNodeRef text childrenRefs = do
  ref <- makeValueRef text
  Transaction.newIRef $ Node ref childrenRefs

makeLeafRef :: MonadIO m => String -> Transaction m ITreeD
makeLeafRef text = makeNodeRef text []
