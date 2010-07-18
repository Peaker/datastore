{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Editor.Data(
    Data,
    outerGridModel, treeNodeGridModel,
    innerGridModel, textEditModel,
    isExpanded,
    Tree(..), nodeValueRef, nodeChildrenRefs,
    ITree, ITreeD, TreeD,
    makeValueRef, makeNodeRef, makeLeafRef)
where

import Control.Monad.IO.Class(MonadIO)
import Control.Monad(ap)
import Data.Binary(Binary(..))
import Data.Vector.Vector2(Vector2(..))
import Data.IRef(IRef)
import Data.IRef.Tree(Tree(..), nodeValueRef, nodeChildrenRefs)
import Data.Transaction(Transaction)
import qualified Data.Transaction as Transaction
import Data.Record.Label((:->), mkLabels, label)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

data Data = Data {
  _outerGridModel :: Grid.Model,
  _treeNodeGridModel :: Grid.Model,
  _innerGridModel :: Grid.Model,
  _textEditModel :: TextEdit.Model,
  _isExpanded :: Bool
  }
$(mkLabels [''Data])
outerGridModel :: Data :-> Grid.Model
treeNodeGridModel :: Data :-> Grid.Model
innerGridModel :: Data :-> Grid.Model
textEditModel :: Data :-> TextEdit.Model
isExpanded :: Data :-> Bool

instance Binary Data where
  get = return Data `ap` get `ap` get `ap` get `ap` get `ap` get
  put (Data a b c d e) = put a >> put b >> put c >> put d >> put e

type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValueRef :: MonadIO m => String -> Transaction t m (IRef Data)
makeValueRef text =
  Transaction.newIRef
  Data {
    _outerGridModel = Grid.initModel,
    _treeNodeGridModel = Grid.Model (Vector2 2 0),
    _innerGridModel = Grid.initModel,
    _textEditModel = TextEdit.initModel text,
    _isExpanded = True
    }

makeNodeRef :: MonadIO m => String -> [ITreeD] -> Transaction t m ITreeD
makeNodeRef text childrenRefs = do
  ref <- makeValueRef text
  Transaction.newIRef $ Node ref childrenRefs

makeLeafRef :: MonadIO m => String -> Transaction t m ITreeD
makeLeafRef text = makeNodeRef text []
