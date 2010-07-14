{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Editor.Data(
    Data, outerGridModel, innerGridModel, textEditModel,
    Tree(..), nodeValueRef, nodeChildrenRefs,
    ITree, ITreeD, TreeD,
    makeValueRef, makeNodeRef, makeLeafRef)
where

import Control.Monad.IO.Class(MonadIO)
import Control.Monad(liftM3)
import Data.Binary(Binary(..))
import Data.IRef(IRef)
import Data.IRef.Tree(Tree(..), nodeValueRef, nodeChildrenRefs)
import Data.Transaction(Transaction)
import qualified Data.Transaction as Transaction
import Data.Record.Label((:->), mkLabels, label)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

data Data = Data {
  _outerGridModel :: Grid.Model,
  _innerGridModel :: Grid.Model,
  _textEditModel :: TextEdit.Model
  }
$(mkLabels [''Data])
outerGridModel :: Data :-> Grid.Model
innerGridModel :: Data :-> Grid.Model
textEditModel :: Data :-> TextEdit.Model

instance Binary Data where
  get = liftM3 Data get get get
  put (Data x y z) = put x >> put y >> put z

type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValueRef :: MonadIO m => String -> Transaction t m (IRef Data)
makeValueRef text = Transaction.newIRef $ Data Grid.initModel Grid.initModel (TextEdit.initModel text)

makeNodeRef :: MonadIO m => String -> [ITreeD] -> Transaction t m ITreeD
makeNodeRef text childrenRefs = do
  ref <- makeValueRef text
  Transaction.newIRef $ Node ref childrenRefs

makeLeafRef :: MonadIO m => String -> Transaction t m ITreeD
makeLeafRef text = makeNodeRef text []
