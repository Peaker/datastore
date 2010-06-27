{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, TemplateHaskell #-}

module Editor.Tree(
    Tree(..), nodeValueRef, nodeChildrenRefs,
    Data, Ref, makeValueRef, makeNodeRef, makeLeafRef) where

import Control.Monad(liftM2)
import Db.Ref(DBRef)
import qualified Db.Ref as Ref
import Db(Db)
import Data.Binary(Binary(..))
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import Data.Record.Label((:->), mkLabels, label)

data Tree a = Node {
  _nodeValueRef :: DBRef a,
  _nodeChildrenRefs :: [DBRef (Tree a)]
  }
$(mkLabels [''Tree])
nodeValueRef :: Tree a :-> DBRef a
nodeChildrenRefs :: Tree a :-> [DBRef (Tree a)]

instance Binary (Tree a) where
  put (Node value children) = put value >> put children
  get = liftM2 Node get get

type Data = ((Grid.Model, Grid.Model), TextEdit.Model)
type Ref = DBRef (Tree Data)

makeValueRef :: Db -> String -> IO (DBRef Data)
makeValueRef db text = Ref.new db ((Grid.initModel, Grid.initModel), TextEdit.initModel text)

makeNodeRef :: Db -> String -> [Ref] -> IO Ref
makeNodeRef db text childrenRefs = do
  ref <- makeValueRef db text
  Ref.new db $ Node ref childrenRefs

makeLeafRef :: Db -> String -> IO Ref
makeLeafRef db text = makeNodeRef db text []
