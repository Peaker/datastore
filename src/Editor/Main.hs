{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Main(main) where

import Prelude hiding ((.))
import Control.Category((.))
import Data.Record.Label.Tuple(first, second)
import qualified Data.IRef as IRef
import Data.IRef(IRef, Store, StoreRef, composeLabel)
import qualified Data.Revision as Revision
import Data.Monoid(mempty, mappend)
import Data.Maybe(fromMaybe)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import qualified Graphics.UI.VtyWidgets.Spacer as Spacer
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.Run as Run
import qualified Db
import Editor.Data(ITreeD, TreeD, Tree, Data)
import qualified Editor.Data as Data
import qualified Editor.Config as Config

setViewRoot :: Store d => d -> IRef (Tree Data) -> IO ()
setViewRoot store = IRef.set (IRef.anchorRef store "viewroot")

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

makeTreeEdit :: Store d => d -> StoreRef d [ITreeD] -> IRef TreeD -> IO (Widget (IO ()))
makeTreeEdit store clipboardRef treeIRef = do
  valueRef <- IRef.follow $ Data.nodeValueRef `composeLabel` treeRef
  makeTreeEdit'
    (second `composeLabel` valueRef)
    (second . first `composeLabel` valueRef)
    (first . first `composeLabel` valueRef)
    (Data.nodeChildrenRefs `composeLabel` treeRef)
  where
    fromIRef = IRef.fromIRef store
    treeRef = fromIRef treeIRef
    makeTreeEdit'
      valueTextEditModelRef
      childrenGridModelRef
      outerGridModelRef
      childrenIRefsRef
      = do
        valueEdit <- makeTextEdit 2
                     TextEdit.defaultAttr TextEdit.editingAttr
                     valueTextEditModelRef
        childItems <- mapM (makeTreeEdit store clipboardRef) =<< IRef.get childrenIRefsRef
        curChildIndex <- getChildIndex (length childItems)
        childGrid <- makeGrid (map (: []) childItems) childrenGridModelRef
        let childGrid' = (Widget.strongerKeys $
                          mappend
                          delNodeKeymap cutNodeKeymap
                          curChildIndex) .
                         Widget.atDisplay (indent 5) $
                         childGrid
        outerGrid <- makeGrid [[valueEdit], [childGrid']] outerGridModelRef
        clipboard <- IRef.get clipboardRef
        return .
          Widget.strongerKeys (pasteKeymap clipboard `mappend`
                               appendNewNodeKeymap `mappend`
                               setRootKeymap) $
          outerGrid
      where
        validateIndex count index
          | 0 <= index && index < count = Just index
          | otherwise = Nothing
        getChildIndex count = (validateIndex count . Vector2.snd . Grid.modelCursor) `fmap`
                              IRef.get childrenGridModelRef
        pasteKeymap [] = mempty
        pasteKeymap (cbChildRef:xs) =
          Keymap.simpleton "Paste" Config.pasteKey $ do
            appendChild cbChildRef
            IRef.set clipboardRef xs
        appendNewNodeKeymap = Keymap.simpleton "Append new child node"
                              Config.appendChildKey appendNewChild
        cutNodeKeymap = fromMaybe mempty .
                        fmap (Keymap.simpleton "Cut node" Config.cutKey . cutChild)
        delNodeKeymap = fromMaybe mempty .
                        fmap (Keymap.simpleton "Del node" Config.delChildKey . delChild)
        setRootKeymap =
          Keymap.simpleton "Set root element" Config.setViewRootKey $
            setViewRoot store treeIRef
        yGridCursor = Grid.Model . Vector2 0
        appendNewChild = do
          newRef <- Data.makeLeafRef store "NEW_NODE"
          appendChild newRef
        appendChild newRef = do
          IRef.modify childrenIRefsRef (++ [newRef])
          childrenIRefs <- IRef.get childrenIRefsRef
          IRef.set outerGridModelRef $ yGridCursor 1
          IRef.set childrenGridModelRef $ yGridCursor (length childrenIRefs - 1)
        cutChild index = do
          childrenIRefs <- IRef.get childrenIRefsRef
          IRef.modify clipboardRef (childrenIRefs !! index :)
          delChild index
        delChild index =
          IRef.modify childrenIRefsRef $ removeAt index

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

makeGrid :: [[Widget (IO ())]] -> StoreRef d Grid.Model -> IO (Widget (IO ()))
makeGrid rows gridModelRef =
  fmap (Grid.make (IRef.set gridModelRef) rows) $
  IRef.get gridModelRef

makeTextEdit :: Int -> Vty.Attr -> Vty.Attr -> StoreRef d TextEdit.Model -> IO (Widget (IO ()))
makeTextEdit maxLines defAttr editAttr textEditModelRef =
  fmap (fmap (IRef.set textEditModelRef) .
        TextEdit.make maxLines defAttr editAttr) $
  IRef.get textEditModelRef

main :: IO ()
main = Db.withDb "/tmp/db.db" $ Run.widgetLoopWithOverlay . const . makeWidget
  where
    makeWidget dbStore =
      makeWidgetForView =<< Revision.ViewRef dbStore `fmap` IRef.get (Data.masterViewIRefRef dbStore)

makeWidgetForView :: Store d => d -> IO (Widget (IO ()))
makeWidgetForView store = do
  viewRootIRef <- IRef.get viewRootIRefRef
  clipboardRef <- IRef.follow . Data.clipboardIRefRef $ store
  treeEdit <- makeTreeEdit store clipboardRef viewRootIRef
  goRootKeymap <- makeGoRootKeymap
  let treeEditWithKeys =
        Widget.strongerKeys
        (quitKeymap `mappend` goRootKeymap)
        treeEdit
  return treeEditWithKeys
  where
    viewRootIRefRef = Data.viewRootIRefRef store
    rootIRefRef = Data.rootIRefRef $ store
    quitKeymap = Keymap.simpleton "Quit" Config.quitKey . ioError . userError $ "Quit"
    makeGoRootKeymap = do
      rootIRef <- IRef.get rootIRefRef
      viewRootIRef <- IRef.get viewRootIRefRef
      return $
        if viewRootIRef == rootIRef
        then mempty
        else Keymap.simpleton "Go to root" Config.rootKey .
             IRef.set viewRootIRefRef $
             rootIRef
