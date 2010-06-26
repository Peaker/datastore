{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Prelude hiding ((.))
import Control.Category((.))
import qualified Config
import qualified Db
import Db(Db)
import qualified Ref
import Accessor(Accessor(..), composeLabel)
import qualified Accessor
import qualified Tree
import Data.Record.Label.Tuple(first, second)
import Data.Monoid(mempty, mappend)
import Data.Maybe(isNothing, fromMaybe)
import Data.ByteString.UTF8(fromString)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import Data.IORef(newIORef, readIORef, writeIORef)
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Overlay as Overlay
import qualified Graphics.UI.VtyWidgets.TableGrid as TableGrid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import qualified Graphics.UI.VtyWidgets.Spacer as Spacer
import Graphics.UI.VtyWidgets.Widget(Widget)
import Graphics.UI.VtyWidgets.Run(runWidgetLoop)

setViewRoot :: Db -> Tree.Ref -> IO ()
setViewRoot db ref = Db.set db (fromString "viewroot") ref

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

makeTreeEdit :: Db -> Maybe Tree.Ref -> Tree.Ref -> IO (Widget (IO ()))
makeTreeEdit db clipboard treeRef = do
  let treeA = Ref.accessor db treeRef
  valueA <- Ref.follow $ Tree.nodeValueRef `composeLabel` treeA
  makeTreeEdit'
    (second `composeLabel` valueA)
    (second . first `composeLabel` valueA)
    (first . first `composeLabel` valueA)
    (Tree.nodeChildrenRefs `composeLabel` treeA)
  where
    makeTreeEdit'
      valueTextEditModelA
      childrenGridModelA
      outerGridModelA
      childrenRefsA
      = do
        valueEdit <- makeTextEdit 2
                     TextEdit.defaultAttr TextEdit.editingAttr
                     valueTextEditModelA
        childItems <- mapM (makeTreeEdit db clipboard) =<< Accessor.get childrenRefsA
        curChildIndex <- getChildIndex (length childItems)
        childGrid <- makeGrid (map (: []) childItems) childrenGridModelA
        let childGrid' = (Widget.strongerKeys $
                          mappend
                          delNodeKeymap cutNodeKeymap
                          curChildIndex) .
                         Widget.atDisplay (indent 5) $
                         childGrid
        outerGrid <- makeGrid [[valueEdit], [childGrid']] outerGridModelA
        return .
          Widget.strongerKeys (pasteKeymap `mappend`
                               appendNewNodeKeymap `mappend`
                               setRootKeymap) $
          outerGrid
      where
        validateIndex count index
          | 0 <= index && index < count = Just index
          | otherwise = Nothing
        getChildIndex count = (validateIndex count . Vector2.snd . Grid.modelCursor) `fmap`
                              Accessor.get childrenGridModelA
        pasteKeymap =
          case clipboard of
            Nothing -> mempty
            Just cbChildRef ->
              Keymap.simpleton "Paste" Config.pasteKey $ do
                appendChild cbChildRef
                Db.del db (fromString "clipboard")
        appendNewNodeKeymap = Keymap.simpleton "Append new child node"
                              Config.appendChildKey appendNewChild
        cutNodeKeymap = if isNothing clipboard
                        then fromMaybe mempty .
                             fmap (Keymap.simpleton "Cut node" Config.cutKey . cutChild)
                        else mempty
        delNodeKeymap = fromMaybe mempty .
                        fmap (Keymap.simpleton "Del node" Config.delChildKey . delChild)
        setRootKeymap =
          Keymap.simpleton "Set root element" Config.setViewRootKey $ do
            print "Setting view root!"
            setViewRoot db treeRef
        yGridCursor = Grid.Model . Vector2 0
        appendNewChild = do
          newRef <- Tree.makeLeafRef db "NEW_NODE"
          appendChild newRef
        appendChild newRef = do
          Accessor.pureModify childrenRefsA (++ [newRef])
          childrenRefs <- Accessor.get childrenRefsA
          Accessor.set outerGridModelA $ yGridCursor 1
          Accessor.set childrenGridModelA $ yGridCursor (length childrenRefs - 1)
        cutChild index = do
          childrenRefs <- Accessor.get childrenRefsA
          Db.set db (fromString "clipboard") $ childrenRefs !! index
          delChild index
        delChild index =
          Accessor.pureModify childrenRefsA $ removeAt index

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

makeGrid :: [[Widget (IO ())]] -> Accessor Grid.Model -> IO (Widget (IO ()))
makeGrid rows gridModelA =
  fmap (Grid.make (Accessor.set gridModelA) rows) $
  Accessor.get gridModelA

makeTextEdit :: Int -> Vty.Attr -> Vty.Attr -> Accessor TextEdit.Model -> IO (Widget (IO ()))
makeTextEdit maxLines defAttr editAttr textEditModelA =
  fmap (fmap (Accessor.set textEditModelA) .
        TextEdit.make maxLines defAttr editAttr) $
  Accessor.get textEditModelA

main :: IO ()
main = do
  overlayModelRef <- newIORef $ Overlay.initModel True
  Db.withDb "/tmp/db.db" $ \db -> runWidgetLoop $ makeWidget overlayModelRef db
  where
    keymapView keymap = TableGrid.makeKeymapView keymap (keyAttr, 10) (valueAttr, 30)
    keyAttr   = Vty.def_attr
                `Vty.with_fore_color` Vty.green
                `Vty.with_back_color` Vty.blue
    valueAttr = Vty.def_attr
                `Vty.with_fore_color` Vty.red
                `Vty.with_back_color` Vty.blue
                `Vty.with_style` Vty.bold
    makeWidget overlayModelRef db size = do
      clipboard <- Db.lookup db (fromString "clipboard")
      Just rootRef <- Db.lookup db (fromString "viewroot")
      treeEdit <- makeTreeEdit db clipboard rootRef
      let treeEditWithKeys =
            Widget.strongerKeys
            (quitKeymap `mappend` goRootKeymap) $
            treeEdit
      treeEditWithKeymapView <- overlayKeymapView treeEditWithKeys
      return treeEditWithKeymapView
      where
        quitKeymap = Keymap.simpleton "Quit" Config.quitKey . ioError . userError $ "Quit"
        goRootKeymap =
          Keymap.simpleton "Go to root" Config.rootKey $ do
            Just rootRef <- Db.lookup db (fromString "root")
            Db.set db (fromString "viewroot") (rootRef :: Tree.Ref)
        overlayKeymapView w =
          Overlay.widget
          ("Keybindings: show", ([], Vty.KFun 6))
          ("Keybindings: hide", ([], Vty.KFun 6))
          (Widget.simpleDisplay . keymapView . fromMaybe mempty $
           Widget.keymap w size)
          (writeIORef overlayModelRef) w
          `fmap` readIORef overlayModelRef
