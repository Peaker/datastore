{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import Control.Monad(liftM2)
import qualified Db
import Db(Db)
import Data.ByteString.UTF8(fromString)
import Data.Vector.Vector2(Vector2(..))
import Data.IORef(newIORef, readIORef, writeIORef)
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import qualified Graphics.UI.VtyWidgets.Spacer as Spacer
import Graphics.UI.VtyWidgets.Widget(Widget)
import Graphics.UI.VtyWidgets.Run(runWidgetLoop)
import Ref(DBRef)
import qualified Ref
import Tree(Tree(..))

main :: IO ()
main = do
  db <- Db.open "/tmp/db.db"
  Just root <- Db.lookup db (fromString "root")
  makeWidget <- makeTreeEditMaker db root
  runWidgetLoop . const $ makeWidget

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

makeTreeEditMaker :: Db -> DBRef (Tree String) -> IO (IO (Widget (IO ())))
makeTreeEditMaker db treeRef = do
  treeNode <- Ref.read db treeRef
  makeValueEdit <- valueEditMaker treeNode
  makeGrid <- gridMaker treeNode
              makeValueEdit
  return makeGrid
  where
    gridMaker treeNode makeValueEdit = do
      outerGridModelR <- newIORef Grid.initModel
      innerGridModelR <- newIORef Grid.initModel
      treeEditMakers <- mapM (makeTreeEditMaker db) (nodeChildrenRefs treeNode)
      return $ do
        valueEdit <- makeValueEdit
        treeEdits <- sequence treeEditMakers
        outerGridModel <- readIORef outerGridModelR
        innerGridModel <- readIORef innerGridModelR
        let innerItems = map (return . (,) True) treeEdits
            innerGrid = Widget.atDisplay (indent 5) .
                        Grid.make (writeIORef innerGridModelR) innerItems $
                        outerGridModel
            outerItems = map (return . (,) True) [valueEdit, innerGrid]
            outerGrid = Grid.make (writeIORef outerGridModelR) outerItems $
                        innerGridModel
        return outerGrid

    valueEditMaker treeNode = do
      text <- getText
      cursorR <- newIORef (length text)
      return $ do
        valueEditModel <- liftM2 TextEdit.Model (readIORef cursorR) getText
        return . fmap (handleTextEditModel cursorR) $
          TextEdit.make 1 TextEdit.defaultAttr TextEdit.editingAttr valueEditModel
      where
        getText = Ref.read db (nodeValueRef treeNode)
        setText = Ref.write db (nodeValueRef treeNode)

        handleTextEditModel cursorR (TextEdit.Model cursor text) = do
          writeIORef cursorR cursor
          setText text
