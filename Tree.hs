{-# OPTIONS -O2 -Wall #-}

module Tree(Tree(..), atValueRef, atChildrenRefs,
            Data, Ref, makeValueRef, makeNodeRef, makeLeafRef) where

import Control.Monad(liftM2)
import Ref(DBRef)
import qualified Ref
import Db(Db)
import Data.Binary(Binary(..))
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

data Tree a = Node {
  nodeValueRef :: DBRef a,
  nodeChildrenRefs :: [DBRef (Tree a)]
  }
atValueRef :: (DBRef a -> DBRef a) -> Tree a -> Tree a
atValueRef f (Node valueRef childrenRefs) = Node (f valueRef) childrenRefs
atChildrenRefs :: ([DBRef (Tree a)] -> [DBRef (Tree a)]) -> Tree a -> Tree a
atChildrenRefs f (Node valueRef childrenRefs) = Node valueRef (f childrenRefs)

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
