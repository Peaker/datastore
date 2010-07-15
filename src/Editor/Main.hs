{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, Rank2Types #-}

module Main(main) where

import Prelude hiding ((.))
import Control.Monad.IO.Class(MonadIO)
import Control.Category((.))
import Control.Monad(when, liftM)
import Data.List.Utils(safeIndex)
import Data.IRef(IRef)
import qualified Data.Transaction as Transaction
import Data.Transaction(Transaction, Store)
import Data.Property(composeLabel)
import qualified Data.Property as Property
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
import Editor.Anchors(DBTag, ViewTag)
import qualified Editor.Config as Config

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

yGridCursor :: Int -> Grid.Model
yGridCursor = Grid.Model . Vector2 0

appendGridChild :: Monad m =>
                   Transaction.Property t m Grid.Model ->
                   Transaction.Property t m [a] ->
                   a -> Transaction t m ()
appendGridChild gridModelRef valuesRef value = do
  values <- Property.get valuesRef
  Property.set valuesRef (values ++ [value])
  Property.set gridModelRef . yGridCursor $ length values

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

popCurChild :: Monad m =>
               Transaction.Property t m Grid.Model ->
               Transaction.Property t m [a] ->
               Transaction t m (Maybe a)
popCurChild gridModelRef valuesRef = do
  values <- Property.get valuesRef
  curIndex <- (Vector2.snd . Grid.modelCursor) `liftM`
              Property.get gridModelRef
  let value = curIndex `safeIndex` values
  maybe (return ()) (delChild curIndex values) value
  return value
  where
    delChild curIndex values _child = do
      Property.set valuesRef (curIndex `removeAt` values)
      when (curIndex >= length values - 1) .
        Property.pureModify gridModelRef . Grid.inModel . Vector2.second $ subtract 1

makeGrid :: Monad m =>
            [[Widget (Transaction t m ())]] ->
            Transaction.Property t m Grid.Model ->
            Transaction t m (Widget (Transaction t m ()))
makeGrid rows gridModelRef =
  Grid.make (Property.set gridModelRef) rows `liftM`
  Property.get gridModelRef

makeTextEdit :: Monad m => Int -> Vty.Attr -> Vty.Attr ->
                Transaction.Property t m TextEdit.Model ->
                Transaction t m (Widget (Transaction t m ()))
makeTextEdit maxLines defAttr editAttr textEditModelRef =
  liftM (fmap (Property.set textEditModelRef) .
        TextEdit.make "<empty>" maxLines defAttr editAttr) $
  Property.get textEditModelRef

makeChoiceWidget :: Monad m =>
                    [(Widget (Transaction t m ()), k)] ->
                    Transaction.Property t m Grid.Model ->
                    Transaction t m (Widget (Transaction t m ()), k)
makeChoiceWidget keys gridModelRef = do
  widget <- makeGrid rows gridModelRef
  itemIndex <- (Vector2.snd .
                Grid.modelCursor) `liftM` Property.get gridModelRef
  return (widget, items !! min maxIndex itemIndex)
  where
    maxIndex = length items - 1
    rows = map (: []) widgets
    widgets = map fst keys
    items = map snd keys

makeTreeEdit :: MonadIO m => Transaction.Property ViewTag m [ITreeD] ->
                IRef TreeD ->
                Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeTreeEdit clipboardRef treeIRef = do
  valueRef <- Transaction.follow $ Data.nodeValueRef `composeLabel` treeRef
  makeTreeEdit'
    (Data.textEditModel `composeLabel` valueRef)
    -- HLint/haskell-src-exts doesn't know the fixity of composeLabel,
    -- so it's fixity resolver fails to parse the following without ()
    -- around the . expression. When it fails here, it avoids
    -- resolving all fixities, so it makes it complain about the above
    -- "Transaction.follow $ ..." expression that $ is redundant
    -- because it thinks the fixity of composeLabel is lower than $.
    (Data.innerGridModel `composeLabel` valueRef)
    (Data.outerGridModel `composeLabel` valueRef)
    (Data.nodeChildrenRefs `composeLabel` treeRef)
  where
    treeRef = Transaction.fromIRef treeIRef
    makeTreeEdit'
      valueTextEditModelRef
      childrenGridModelRef
      outerGridModelRef
      childrenIRefsRef
      = do
        valueEdit <- makeTextEdit 2
                     TextEdit.defaultAttr TextEdit.editingAttr
                     valueTextEditModelRef
        childItems <- mapM (makeTreeEdit clipboardRef) =<< Property.get childrenIRefsRef
        curChildIndex <- getChildIndex . length $ childItems
        childGrid <- makeGrid (map (: []) childItems) childrenGridModelRef
        let childGrid' = Widget.strongerKeys
                         (mappend
                          delNodeKeymap cutNodeKeymap
                          curChildIndex) .
                         Widget.atDisplay (indent 5) $
                         childGrid
        outerGrid <- makeGrid [[valueEdit], [childGrid']] outerGridModelRef
        clipboard <- Property.get clipboardRef
        return .
          Widget.strongerKeys (pasteKeymap clipboard `mappend`
                               appendNewNodeKeymap `mappend`
                               setRootKeymap) $
          outerGrid
      where
        validateIndex count index
          | 0 <= index && index < count = Just index
          | otherwise = Nothing
        getChildIndex count = (validateIndex count . Vector2.snd . Grid.modelCursor) `liftM`
                              Property.get childrenGridModelRef
        pasteKeymap [] = mempty
        pasteKeymap (cbChildRef:xs) =
          Keymap.simpleton "Paste" Config.pasteKey $ do
            appendChild cbChildRef
            Property.set clipboardRef xs
        appendNewNodeKeymap = Keymap.simpleton "Append new child node"
                              Config.appendChildKey appendNewChild
        cutNodeKeymap = fromMaybe mempty .
                        liftM (Keymap.simpleton "Cut node" Config.cutKey . cutChild)
        delNodeKeymap = fromMaybe mempty .
                        liftM (Keymap.simpleton "Del node" Config.delChildKey . delChild)
        setRootKeymap =
          Keymap.simpleton "Set focal point" Config.setFocalPointKey $
            Property.set Anchors.focalPointIRef treeIRef
        appendNewChild = do
          -- TODO: Transaction here
          newRef <- Data.makeLeafRef "NEW_NODE"
          appendChild newRef
        appendChild newRef = do
          appendGridChild childrenGridModelRef childrenIRefsRef newRef
          Property.set outerGridModelRef $ yGridCursor 1
        cutChild index = do
          childrenIRefs <- Property.get childrenIRefsRef
          Property.pureModify clipboardRef (childrenIRefs !! index :)
          delChild index
        delChild index =
          Property.pureModify childrenIRefsRef $ removeAt index

makeEditWidget :: MonadIO m =>
                  Transaction.Property ViewTag m [ITreeD] ->
                  Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeEditWidget clipboardRef = do
  rootIRef <- Property.get rootIRefRef
  focalPointIRef <- Property.get focalPointIRefRef
  Widget.strongerKeys (goRootKeymap rootIRef focalPointIRef) `liftM`
    makeTreeEdit clipboardRef focalPointIRef
  where
    focalPointIRefRef = Anchors.focalPointIRef
    rootIRefRef = Anchors.rootIRef
    goRootKeymap rootIRef focalPointIRef =
      if rootIRef == focalPointIRef
      then mempty
      else Keymap.simpleton "Go to root" Config.rootKey .
           Property.set focalPointIRefRef $
           rootIRef

-- Take a widget parameterized on transaction on views (that lives in
-- a nested transaction monad) and convert it to one parameterized on
-- the nested transaction
type MWidget m = m (Widget (m ()))
widgetDownTransaction :: MonadIO m =>
                         Store t m ->
                         MWidget (Transaction t m) ->
                         MWidget m
widgetDownTransaction store = runTrans . (liftM . fmap) runTrans
  where
    runTrans = Transaction.run store

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: MonadIO m => View -> Transaction DBTag m (Widget (Transaction DBTag m ()))
makeWidgetForView view = do
  version <- View.curVersion view
  widget <- widgetDownTransaction (Anchors.viewStore view) $ do
              clipboardP <- Transaction.follow Anchors.clipboardIRef
              makeEditWidget clipboardP
  return $ Widget.strongerKeys (keymaps version) widget
  where
    keymaps version = undoKeymap version `mappend` makeBranchKeymap
    makeBranchKeymap = Keymap.simpleton "New Branch" Config.makeBranchKey makeBranch
    makeBranch = do
      versionIRef <- Branch.new =<< View.curVersionIRef view
      textEditModelIRef <- Transaction.newIRef $ TextEdit.initModel "New view"
      let viewPair = (textEditModelIRef, versionIRef)
      appendGridChild Anchors.branchSelector Anchors.branches viewPair
    undoKeymap version =
        if Version.depth version > 1
        then Keymap.simpleton "Undo" Config.undoKey .
             View.move view .
             fromJust . Version.parent $
             version
        else mempty

main :: IO ()
main = Db.withDb "/tmp/db.db" $ Run.widgetLoopWithOverlay 20 30 . const . makeWidget . Anchors.dbStore
  where
    makeWidget dbStore = widgetDownTransaction dbStore $ do
      versionMap <- Property.get Anchors.versionMap
      branches <- Property.get Anchors.branches
      pairs <- mapM pair branches
      (viewSelector, branch) <- makeChoiceWidget pairs Anchors.branchSelector
      let view = View.make versionMap branch
      viewEdit <- Widget.strongerKeys quitKeymap
                  `liftM` makeWidgetForView view
      makeGrid
        [[viewEdit,
          Widget.simpleDisplay Spacer.makeHorizontal,
          Widget.strongerKeys (delBranchKeymap branches) viewSelector]]
        Anchors.mainGrid

    delBranchKeymap [_] = mempty
    delBranchKeymap _ = Keymap.simpleton "Delete Branch" Config.delBranchKey deleteCurBranch
    quitKeymap = Keymap.simpleton "Quit" Config.quitKey . fail $ "Quit"

    deleteCurBranch = do
      _ <- popCurChild Anchors.branchSelector Anchors.branches
      return ()

    simpleTextEdit =
      makeTextEdit 1
      TextEdit.defaultAttr
      TextEdit.editingAttr

    pair (textEditModelIRef, versionIRef) = do
      textEdit <- simpleTextEdit . Transaction.fromIRef $ textEditModelIRef
      return (textEdit, versionIRef)
