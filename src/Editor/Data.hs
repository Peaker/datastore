{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Editor.Data(
    Data, gridModels, textEditModel, isExpanded,
    gridModel,
    Tree(..), nodeValue, nodeChildrenRefs,
    ITree, ITreeD, TreeD,
    makeValue, makeNode, makeNodeRef, makeLeafRef)
where

import           Prelude                         hiding ((.), id)
import           Control.Category                ((.))
import           Data.Binary                     (Binary(..))
import           Data.Binary.Utils               (get3, put3)
import           Data.IRef                       (IRef)
import           Data.IRef.Tree                  (Tree(..), nodeValue, nodeChildrenRefs)
import           Data.Transaction                (Transaction)
import qualified Data.Transaction                as Transaction
import           Data.Record.Label               ((:->), mkLabels, label)
import qualified Data.Record.Label.Map           as Label.Map
import qualified Data.Record.Label.Maybe         as Label.Maybe
import qualified Graphics.UI.VtyWidgets.Grid     as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import           Data.Map                        (Map)
import qualified Data.Map                        as Map

data Data = Data {
  _gridModels :: Map String Grid.Model,
  _textEditModel :: TextEdit.Model,
  _isExpanded :: Bool
  }
  deriving (Show, Read, Eq, Ord)
$(mkLabels [''Data])
gridModels :: Data :-> Map String Grid.Model
textEditModel :: Data :-> TextEdit.Model
isExpanded :: Data :-> Bool

gridModel :: Grid.Model -> String -> Data :-> Grid.Model
gridModel d key = Label.Maybe.fromMaybe d . Label.Map.value key . gridModels

instance Binary Data where
  get = get3 Data
  put (Data a b c) = put3 a b c

type ITreeD = ITree Data
type TreeD = Tree Data

type ITree a = IRef (Tree a)

makeValue :: String -> Data
makeValue text =
  Data {
    _gridModels = Map.empty,
    _textEditModel = TextEdit.initModel text,
    _isExpanded = True
  }

makeNode :: String -> [ITreeD] -> TreeD
makeNode = Node . makeValue

makeNodeRef :: Monad m => String -> [ITreeD] -> Transaction t m ITreeD
makeNodeRef text childrenRefs = Transaction.newIRef $ makeNode text childrenRefs

makeLeafRef :: Monad m => String -> Transaction t m ITreeD
makeLeafRef text = makeNodeRef text []
