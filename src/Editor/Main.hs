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
import qualified Data.Rev.View as View
import Data.Rev.View(View)
import qualified Data.Rev.Version as Version
import qualified Data.Rev.Branch as Branch
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
import Editor.Data(ITreeD, TreeD)
import qualified Editor.Data as Data
import qualified Editor.Anchors as Anchors
import qualified Editor.Config as Config

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

makeTreeEdit :: Store d =>
                View d ->
                Store.Ref (View d) [ITreeD] ->
                IRef TreeD ->
                IO (Widget (IO ()))
makeTreeEdit view clipboardRef treeIRef = do
  valueRef <- Store.follow $ Data.nodeValueRef `composeLabel` treeRef
  makeTreeEdit'
    (second `composeLabel` valueRef)
    -- HLint/haskell-src-exts doesn't know the fixity of composeLabel,
    -- so it's fixity resolver fails to parse the following without ()
    -- around the . expression. When it fails here, it avoids
    -- resolving all fixities, so it makes it complain about the above
    -- "Store.follow $ ..." expression that $ is redundant because it
    -- thinks the fixity of composeLabel is lower than $.
    ((second . first) `composeLabel` valueRef)
    ((first . first) `composeLabel` valueRef)
    (Data.nodeChildrenRefs `composeLabel` treeRef)
  where
    treeRef = Store.fromIRef view treeIRef
    makeTreeEdit'
      valueTextEditModelRef
      childrenGridModelRef
      outerGridModelRef
      childrenIRefsRef
      = do
        valueEdit <- makeTextEdit 2
                     TextEdit.defaultAttr TextEdit.editingAttr
                     valueTextEditModelRef
        childItems <- mapM (makeTreeEdit view clipboardRef) =<< Store.get childrenIRefsRef
        curChildIndex <- getChildIndex . length $ childItems
        childGrid <- makeGrid (map (: []) childItems) childrenGridModelRef
        let childGrid' = Widget.strongerKeys
                         (mappend
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
            Store.set (Anchors.focalPointIRef view) treeIRef
        appendNewChild = do
          -- TODO: Transaction here
          newRef <- Data.makeLeafRef view "NEW_NODE"
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
      versionMap <- Store.get $ Anchors.versionMap dbStore
      branches <- Store.get $ Anchors.branches dbStore
      pairs <- mapM (pair dbStore) branches
      (viewSelector, branch) <- makeChoiceWidget pairs $ Anchors.branchSelector dbStore
      let view = View.make dbStore versionMap branch
      viewEdit <- Widget.strongerKeys quitKeymap
                  `fmap` makeWidgetForView dbStore view
      makeGrid
        [[viewEdit,
          Widget.simpleDisplay Spacer.makeHorizontal,
          Widget.strongerKeys (delBranchKeymap branches dbStore)
          viewSelector]] $
        Anchors.mainGrid dbStore

    delBranchKeymap [_] = mempty
    delBranchKeymap _ = Keymap.simpleton "Delete Branch" Config.delBranchKey . deleteCurBranch
    quitKeymap = Keymap.simpleton "Quit" Config.quitKey . ioError . userError $ "Quit"

    deleteCurBranch dbStore = do
      _ <- popCurChild (Anchors.branchSelector dbStore) (Anchors.branches dbStore)
      return ()

    simpleTextEdit =
      makeTextEdit 1
      TextEdit.defaultAttr
      TextEdit.editingAttr

    pair dbStore (textEditModelIRef, versionIRef) = do
      textEdit <- simpleTextEdit . Store.fromIRef dbStore $ textEditModelIRef
      return (textEdit, versionIRef)

makeEditWidget :: Store d =>
                  View d ->
                  Store.Ref (View d) [ITreeD] ->
                  IO (Widget (IO ()))
makeEditWidget view clipboardRef = do
  rootIRef <- Store.get rootIRefRef
  focalPointIRef <- Store.get focalPointIRefRef
  Widget.strongerKeys (goRootKeymap rootIRef focalPointIRef) `fmap`
    makeTreeEdit view clipboardRef focalPointIRef
  where
    focalPointIRefRef = Anchors.focalPointIRef view
    rootIRefRef = Anchors.rootIRef view
    goRootKeymap rootIRef focalPointIRef =
      if rootIRef == focalPointIRef
      then mempty
      else Keymap.simpleton "Go to root" Config.rootKey .
           Store.set focalPointIRefRef $
           rootIRef

makeWidgetForView :: Store d => d -> View d -> IO (Widget (IO ()))
makeWidgetForView store view = do
  clipboardRef <- Store.follow . Anchors.clipboardIRef $ view
  version <- View.curVersion view
  Widget.strongerKeys (keymaps version) `fmap`
    makeEditWidget view clipboardRef
  where
    keymaps version = undoKeymap version `mappend` makeBranchKeymap
    makeBranchKeymap = Keymap.simpleton "New Branch" Config.makeBranchKey makeBranch
    makeBranch = do
      versionIRef <- Branch.new (View.store view) =<< View.curVersionIRef view
      textEditModelIRef <- Store.newIRef store $ TextEdit.initModel "New view"
      let viewPair = (textEditModelIRef, versionIRef)
      appendGridChild (Anchors.branchSelector store) (Anchors.branches store) viewPair
    undoKeymap version =
        if Version.depth version > 1
        then Keymap.simpleton "Undo" Config.undoKey .
             View.move view .
             fromJust . Version.parent $
             version
        else mempty
