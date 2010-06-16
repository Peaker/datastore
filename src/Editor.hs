{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import Control.Arrow(first, second)
import qualified Db
import Db(Db)
import Data.Monoid(mappend)
import Data.ByteString.UTF8(fromString)
import Data.Vector.Vector2(Vector2(..))
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
import qualified Ref
import Tree(Tree(..), Ref, atChildrenRefs, makeLeafRef)

quitKey :: ModKey
quitKey = ([Vty.MCtrl], Vty.KASCII 'q')

newChildKey :: ModKey
newChildKey = ([Vty.MCtrl], Vty.KASCII 'n')

main :: IO ()
main = do
  Db.withDb "/tmp/db.db" $ \db -> do
    Just root <- Db.lookup db (fromString "root")
    let makeWidget =
          (fmap . Widget.atKeymap)
          (`mappend`
           Keymap.simpleton "Quit" quitKey
           (ioError . userError $ "Quit")) $
          makeTreeEdit db root
    runWidgetLoop . const $ makeWidget

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

makeTreeEdit :: Db -> Ref -> IO (Widget (IO ()))
makeTreeEdit db treeRef = do
  treeNode <- Ref.read db treeRef
  let valueRef = nodeValueRef treeNode
  valueEdit <- makeValueEdit valueRef
  let childrenRefs = nodeChildrenRefs treeNode
  treeEdits <- mapM (makeTreeEdit db) childrenRefs
  let innerItems = map (return . (,) True) treeEdits
  value <- Ref.read db valueRef
  let innerGrid =
        Widget.atDisplay (indent 5) .
        Grid.make (Ref.pureModify db valueRef . first . second . const) innerItems . snd . fst $
        value
      outerItems = map (return . (,) True) $
                   [valueEdit] ++
                   if null treeEdits
                   then []
                   else [innerGrid]
      outerGrid = Grid.make (Ref.pureModify db valueRef . first . first . const) outerItems . fst . fst $
                  value
      withKeys = Widget.atKeymap
                 (`mappend`
                  Keymap.simpleton "New child node" newChildKey
                  addNewChild) outerGrid
  return withKeys
  where
    addNewChild = do
      newRef <- makeLeafRef db "<new node>"
      (Ref.pureModify db treeRef . atChildrenRefs) (++ [newRef])
    makeValueEdit valueRef = do
      return . fmap (Ref.pureModify db valueRef . second . const) .
        TextEdit.make 2 TextEdit.defaultAttr TextEdit.editingAttr . snd =<<
        Ref.read db valueRef
