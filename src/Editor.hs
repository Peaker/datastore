{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Prelude hiding ((.))
import Control.Category((.))
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
import qualified Property
import Property(Property, composeLabel)
import Data.Record.Label.Tuple(first, second)

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
  let valueProp = Ref.property db (nodeValueRef treeNode)
  valueEdit <- makeTextEdit 2 TextEdit.defaultAttr TextEdit.editingAttr
               (valueProp `composeLabel` second)
  let childrenRefs = nodeChildrenRefs treeNode
  treeEdits <- mapM (makeTreeEdit db) childrenRefs
  let innerItems = map (return . (,) True) treeEdits
  innerGrid <- Widget.atDisplay (indent 5) `fmap`
               makeGrid innerItems (valueProp `composeLabel` (second . first))
  let outerItems = map (return . (,) True) $
                   [valueEdit] ++
                   if null treeEdits
                   then []
                   else [innerGrid]
  outerGrid <- Widget.atKeymap
               (`mappend` Keymap.simpleton "New child node"
                newChildKey addNewChild)
               `fmap`
               makeGrid outerItems (valueProp `composeLabel` (first . first))
  return outerGrid
  where
    addNewChild = do
      newRef <- makeLeafRef db "<new node>"
      (Ref.pureModify db treeRef . atChildrenRefs) (++ [newRef])

makeGrid :: [[(Bool, Widget (IO ()))]] -> Property IO Grid.Model -> IO (Widget (IO ()))
makeGrid rows gridModelP =
  fmap (Grid.make (Property.set gridModelP) rows) $
  Property.get gridModelP

makeTextEdit :: Int -> Vty.Attr -> Vty.Attr -> Property IO TextEdit.Model -> IO (Widget (IO ()))
makeTextEdit maxLines defAttr editAttr textEditModelP =
  fmap (fmap (Property.set textEditModelP) .
        TextEdit.make maxLines defAttr editAttr) $
  Property.get textEditModelP
