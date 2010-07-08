{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Main(main) where

import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad(when)
import Data.List.Utils(safeIndex)
import Data.Record.Label.Tuple(first, second)
import Data.IRef(IRef)
import qualified Data.Store as Store
import Data.Store(Store, composeLabel)
import qualified Data.Revision as Revision
import Data.Monoid(mempty, mappend)
import Data.Maybe(fromMaybe, fromJust)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.Spacer as Spacer
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.Run as Run
import qualified Db
import Editor.Data(ITreeD, TreeD, Tree, Data)
import qualified Editor.Data as Data
import qualified Editor.Anchors as Anchors
import qualified Editor.Config as Config

setFocalPoint :: Store d => d -> IRef (Tree Data) -> IO ()
setFocalPoint store = Store.set (Store.anchorRef store "viewroot")

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

yGridCursor :: Int -> Grid.Model
yGridCursor = Grid.Model . Vector2 0

appendGridChild :: Store d => Store.Ref d Grid.Model -> Store.Ref d [a] -> a -> IO ()
appendGridChild gridModelRef valuesRef value = do
  values <- Store.get valuesRef
  Store.set valuesRef (values ++ [value])
  Store.set gridModelRef . yGridCursor $ length values

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

popCurChild :: Store d => Store.Ref d Grid.Model -> Store.Ref d [a] -> IO (Maybe a)
popCurChild gridModelRef valuesRef = do
  values <- Store.get valuesRef
  curIndex <- (Vector2.snd . Grid.modelCursor) `fmap`
              Store.get gridModelRef
  let value = curIndex `safeIndex` values
  maybe (return ()) (delChild curIndex values) value
  return value
  where
    delChild curIndex values _child = do
      Store.set valuesRef (curIndex `removeAt` values)
      when (curIndex >= length values - 1) .
        Store.modify gridModelRef . Grid.inModel . Vector2.second $ subtract 1

makeTreeEdit :: Store d => Revision.ViewRef d -> Revision.Ref d [ITreeD] -> IRef TreeD -> IO (Widget (IO ()))
makeTreeEdit viewRef clipboardRef treeIRef = do
  valueRef <- Store.follow $ Data.nodeValueRef `composeLabel` treeRef
  makeTreeEdit'
    (second `composeLabel` valueRef)
    (second . first `composeLabel` valueRef)
    (first . first `composeLabel` valueRef)
    (Data.nodeChildrenRefs `composeLabel` treeRef)
  where
    treeRef = Store.fromIRef viewRef treeIRef
    makeTreeEdit'
      valueTextEditModelRef
      childrenGridModelRef
      outerGridModelRef
      childrenIRefsRef
      = do
        valueEdit <- makeTextEdit 2
                     TextEdit.defaultAttr TextEdit.editingAttr
                     valueTextEditModelRef
        childItems <- mapM (makeTreeEdit viewRef clipboardRef) =<< Store.get childrenIRefsRef
        curChildIndex <- getChildIndex . length $ childItems
        childGrid <- makeGrid (map (: []) childItems) childrenGridModelRef
        let childGrid' = (Widget.strongerKeys $
                          mappend
                          delNodeKeymap cutNodeKeymap
                          curChildIndex) .
                         Widget.atDisplay (indent 5) $
                         childGrid
        outerGrid <- makeGrid [[valueEdit], [childGrid']] outerGridModelRef
        clipboard <- Store.get clipboardRef
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
                              Store.get childrenGridModelRef
        pasteKeymap [] = mempty
        pasteKeymap (cbChildRef:xs) =
          Keymap.simpleton "Paste" Config.pasteKey $ do
            appendChild cbChildRef
            Store.set clipboardRef xs
        appendNewNodeKeymap = Keymap.simpleton "Append new child node"
                              Config.appendChildKey appendNewChild
        cutNodeKeymap = fromMaybe mempty .
                        fmap (Keymap.simpleton "Cut node" Config.cutKey . cutChild)
        delNodeKeymap = fromMaybe mempty .
                        fmap (Keymap.simpleton "Del node" Config.delChildKey . delChild)
        setRootKeymap =
          Keymap.simpleton "Set focal point" Config.setFocalPointKey $
            setFocalPoint viewRef treeIRef
        appendNewChild = do
          -- TODO: Transaction here
          newRef <- Data.makeLeafRef viewRef "NEW_NODE"
          appendChild newRef
        appendChild newRef = do
          appendGridChild childrenGridModelRef childrenIRefsRef newRef
          Store.set outerGridModelRef $ yGridCursor 1
        cutChild index = do
          childrenIRefs <- Store.get childrenIRefsRef
          Store.modify clipboardRef (childrenIRefs !! index :)
          delChild index
        delChild index =
          Store.modify childrenIRefsRef $ removeAt index

makeGrid :: [[Widget (IO ())]] -> Store.Ref d Grid.Model -> IO (Widget (IO ()))
makeGrid rows gridModelRef =
  Grid.make (Store.set gridModelRef) rows `fmap`
  Store.get gridModelRef

makeTextEdit :: Int -> Vty.Attr -> Vty.Attr -> Store.Ref d TextEdit.Model -> IO (Widget (IO ()))
makeTextEdit maxLines defAttr editAttr textEditModelRef =
  fmap (fmap (Store.set textEditModelRef) .
        TextEdit.make "<empty>" maxLines defAttr editAttr) $
  Store.get textEditModelRef

makeChoiceWidget :: Store d =>
                    [(Widget (IO ()), k)] ->
                    Store.Ref d Grid.Model ->
                    IO (Widget (IO ()), k)
makeChoiceWidget keys gridModelRef = do
  widget <- makeGrid rows gridModelRef
  itemIndex <- (Vector2.snd .
                Grid.modelCursor) `fmap` Store.get gridModelRef
  return (widget, items !! min maxIndex itemIndex)
  where
    maxIndex = length items - 1
    rows = map (: []) widgets
    widgets = map fst keys
    items = map snd keys

main :: IO ()
main = Db.withDb "/tmp/db.db" $ Run.widgetLoopWithOverlay 20 30 . const . makeWidget
  where
    makeWidget dbStore = do
      views <- Store.get $ Anchors.views dbStore
      pairs <- mapM (pair dbStore) views
      (viewSelector, viewIRef) <- makeChoiceWidget pairs $ Anchors.viewSelector dbStore
      let viewRef = Revision.ViewRef dbStore viewIRef
      viewEdit <- Widget.strongerKeys quitKeymap
                  `fmap` makeWidgetForView dbStore viewRef
      makeGrid
        [[viewEdit,
          Widget.simpleDisplay Spacer.makeHorizontal,
          Widget.strongerKeys (delViewKeymap views dbStore)
          viewSelector]] $
        Anchors.mainGrid dbStore

    delViewKeymap [_] = mempty
    delViewKeymap _ = Keymap.simpleton "Delete View" Config.delViewKey . deleteCurView
    quitKeymap = Keymap.simpleton "Quit" Config.quitKey . ioError . userError $ "Quit"

    deleteCurView dbStore = do
      _ <- popCurChild (Anchors.viewSelector dbStore) (Anchors.views dbStore)
      return ()

    simpleTextEdit =
      makeTextEdit 1
      TextEdit.defaultAttr
      TextEdit.editingAttr

    pair dbStore (textEditModelIRef, viewIRef) = do
      textEdit <- simpleTextEdit . Store.fromIRef dbStore $ textEditModelIRef
      return (textEdit, viewIRef)

makeEditWidget :: Store d => Revision.ViewRef d -> Revision.Ref d [ITreeD] ->
                  IO (Widget (IO ()))
makeEditWidget viewRef clipboardRef = do
  rootIRef <- Store.get rootIRefRef
  focalPointIRef <- Store.get focalPointIRefRef
  Widget.strongerKeys (goRootKeymap rootIRef focalPointIRef) `fmap`
    makeTreeEdit viewRef clipboardRef focalPointIRef
  where
    focalPointIRefRef = Anchors.focalPointIRef viewRef
    rootIRefRef = Anchors.rootIRef viewRef
    goRootKeymap rootIRef focalPointIRef =
      if focalPointIRef == rootIRef
      then mempty
      else Keymap.simpleton "Go to root" Config.rootKey .
           Store.set focalPointIRefRef $
           rootIRef

makeWidgetForView :: Store d => d -> Revision.ViewRef d -> IO (Widget (IO ()))
makeWidgetForView store viewRef = do
  clipboardRef <- Store.follow . Anchors.clipboardIRef $ viewRef
  version <- Revision.viewRefVersion viewRef
  Widget.strongerKeys (keymaps version) `fmap`
    makeEditWidget viewRef clipboardRef
  where
    keymaps version = undoKeymap version `mappend` makeViewKeymap
    makeViewKeymap = Keymap.simpleton "New View" Config.makeViewKey makeView
    makeView = do
      newViewRef <- Revision.makeView store =<< Revision.viewRefVersionIRef viewRef
      textEditModelIRef <- Store.newIRef store $ TextEdit.initModel "New view"
      let viewPair = (textEditModelIRef, Revision.viewIRef newViewRef)
      appendGridChild (Anchors.viewSelector store) (Anchors.views store) viewPair
    undoKeymap version =
        if Revision.versionDepth version > 1
        then Keymap.simpleton "Undo" Config.undoKey .
             Revision.moveView viewRef .
             fromJust . Revision.versionParent $
             version
        else mempty
