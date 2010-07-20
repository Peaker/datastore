{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Editor.Data(
    Data, gridModels, textEditModel, isExpanded,
    gridModel,
    Tree(..), nodeValue, nodeChildrenRefs,
    ITree, ITreeD, TreeD,
    makeValue, makeNodeRef, makeLeafRef)
where

import Data.Binary(Binary(..))
import Data.Binary.Utils(get3, put3)
import Data.ContainerRef(ContainerRef)
import Data.IRef(IRef)
import Data.IRef.Tree(Tree(..), nodeValue, nodeChildrenRefs)
import Data.Transaction(Transaction)
import qualified Data.Transaction as Transaction
import Data.Record.Label((:->), mkLabels, label)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

data Data = Data {
  _gridModels :: ContainerRef Grid.Model,
  _textEditModel :: TextEdit.Model,
  _isExpanded :: Bool
  }
$(mkLabels [''Data])
gridModels :: Data :-> ContainerRef Grid.Model
textEditModel :: Data :-> TextEdit.Model
isExpanded :: Data :-> Bool

gridModel :: Monad m => Grid.Model -> Data -> Transaction.Container String t m Grid.Model
gridModel defModel = Transaction.containerStr .
                     Transaction.fromContainerRefDef (return defModel) .
                     _gridModels

instance Binary Data where
  get = get3 Data
  put (Data a b c) = put3 a b c

type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValue :: Monad m => String -> Transaction t m Data
makeValue text = do
  gridModelsGuid <- Transaction.newContainerRef
  return
    Data {
      _gridModels = gridModelsGuid,
      _textEditModel = TextEdit.initModel text,
      _isExpanded = True
    }

makeNodeRef :: Monad m => String -> [ITreeD] -> Transaction t m ITreeD
makeNodeRef text childrenRefs = Transaction.newIRef . flip Node childrenRefs =<< makeValue text

makeLeafRef :: Monad m => String -> Transaction t m ITreeD
makeLeafRef text = makeNodeRef text []
