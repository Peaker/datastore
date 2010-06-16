{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import Control.Monad(liftM2)
import qualified Db
import Db(Db)
import Data.Monoid(mappend)
import Data.ByteString.UTF8(fromString)
import Data.Vector.Vector2(Vector2(..))
import Data.IORef(newIORef, readIORef, writeIORef)
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Keymap(ModKey)
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
  Db.withDb "/tmp/db.db" $ \db -> do
    Just root <- Db.lookup db (fromString "root")
    makeWidget <- makeTreeEditMaker 0 db root
    runWidgetLoop . const $ makeWidget

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

quitKey :: ModKey
quitKey = ([Vty.MCtrl], Vty.KASCII 'q')

makeTreeEditMaker :: Int -> Db -> DBRef (Tree String) -> IO (IO (Widget (IO ())))
makeTreeEditMaker depth db treeRef = do
  treeNode <- Ref.read db treeRef
  makeValueEdit <- valueEditMaker treeNode
  makeGrid <- gridMaker treeNode makeValueEdit
  return makeGrid
  where
    gridMaker treeNode makeValueEdit = do
      outerGridModelR <- newIORef Grid.initModel
      innerGridModelR <- newIORef Grid.initModel
      treeEditMakers <- mapM (makeTreeEditMaker (depth + 1) db) (nodeChildrenRefs treeNode)
      return $ do
        valueEdit <- makeValueEdit
        treeEdits <- sequence treeEditMakers
        outerGridModel <- readIORef outerGridModelR
        innerGridModel <- readIORef innerGridModelR
        let innerItems = map (return . (,) True) treeEdits
            innerGrid = Widget.atDisplay (indent 5) .
                        Grid.make (writeIORef innerGridModelR) innerItems $
                        innerGridModel
            outerItems = map (return . (,) True) $
                         [valueEdit] ++
                         if null treeEdits
                         then []
                         else [innerGrid]
            outerGrid = Grid.make (writeIORef outerGridModelR) outerItems $
                        outerGridModel
            withQuit = Widget.atKeymap (`mappend`
                                        Keymap.simpleton "Quit" quitKey
                                        (ioError . userError $ "Quit")) outerGrid
        return withQuit

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
