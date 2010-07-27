{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, Rank2Types #-}

module Main(main) where

import           Prelude                          hiding ((.))
import           Control.Arrow                    (first)
import           Control.Applicative              (pure)
import           Control.Category                 ((.))
import           Control.Monad                    (when, liftM)
import           Data.List.Utils                  (safeIndex)
import           Data.IRef                        (IRef)
import qualified Data.Transaction                 as Transaction
import           Data.Transaction                 (Transaction, Store)
import           Data.Property                    (composeLabel)
import qualified Data.Property                    as Property
import qualified Data.Rev.Version                 as Version
import qualified Data.Rev.Branch                  as Branch
import           Data.Rev.View                    (View)
import qualified Data.Rev.View                    as View
import           Data.Monoid                      (Monoid(..))
import           Data.Maybe                       (fromMaybe, fromJust)
import           Data.Vector.Rect                 (Rect(Rect))
import           Data.Vector.Vector2              (Vector2(..))
import qualified Graphics.Vty                     as Vty
import qualified Graphics.UI.VtyWidgets.Align     as Align
import qualified Graphics.UI.VtyWidgets.TextView  as TextView
import qualified Graphics.UI.VtyWidgets.TextEdit  as TextEdit
import qualified Graphics.UI.VtyWidgets.Box       as Box
import qualified Graphics.UI.VtyWidgets.Spacer    as Spacer
import qualified Graphics.UI.VtyWidgets.Widget    as Widget
import qualified Graphics.UI.VtyWidgets.Keymap    as Keymap
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import qualified Graphics.UI.VtyWidgets.Run       as Run
import qualified Db
import           Editor.Data                      (ITreeD, TreeD)
import qualified Editor.Data                      as Data
import qualified Editor.Anchors                   as Anchors
import           Editor.Anchors                   (DBTag, ViewTag)
import qualified Editor.Config                    as Config

widthSpace :: Int -> Display a
widthSpace width = Spacer.make . SizeRange.fixedSize $ Vector2 width 0

indent :: Int -> Display a -> Display a
indent width disp = Box.makeView Box.Horizontal [widthSpace width, disp]

appendBoxChild :: Monad m =>
                   Transaction.Property t m Box.Model ->
                   Transaction.Property t m [a] ->
                   a -> Transaction t m ()
appendBoxChild boxModelRef valuesRef value = do
  values <- Property.get valuesRef
  Property.set valuesRef (values ++ [value])
  Property.set boxModelRef . Box.Model . length $ values

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

popCurChild :: Monad m =>
               Transaction.Property t m Box.Model ->
               Transaction.Property t m [a] ->
               Transaction t m (Maybe a)
popCurChild boxModelRef valuesRef = do
  values <- Property.get valuesRef
  curIndex <- Box.modelCursor `liftM` Property.get boxModelRef
  let value = curIndex `safeIndex` values
  maybe (return ()) (delChild curIndex values) value
  return value
  where
    delChild curIndex values _child = do
      Property.set valuesRef (curIndex `removeAt` values)
      when (curIndex >= length values - 1) .
        Property.pureModify boxModelRef . Box.inModel $ subtract 1

makeBox :: Monad m =>
           Box.Orientation ->
           [Widget (Transaction t m ())] ->
           Transaction.Property t m Box.Model ->
           Transaction t m (Widget (Transaction t m ()))
makeBox orientation rows boxModelRef =
  Box.make orientation (Property.set boxModelRef) rows `liftM`
  Property.get boxModelRef

makeTextEdit :: Monad m => Int -> Vty.Attr -> Vty.Attr ->
                Transaction.Property t m TextEdit.Model ->
                Transaction t m (Widget (Transaction t m ()))
makeTextEdit maxLines defAttr editAttr textEditModelRef =
  liftM (fmap (Property.set textEditModelRef) .
        TextEdit.make "<empty>" maxLines defAttr editAttr) $
  Property.get textEditModelRef

makeChoiceWidget :: Monad m =>
                    Box.Orientation ->
                    [(Widget (Transaction t m ()), k)] ->
                    Transaction.Property t m Box.Model ->
                    Transaction t m (Widget (Transaction t m ()), k)
makeChoiceWidget orientation keys boxModelRef = do
  widget <- makeBox orientation widgets boxModelRef
  itemIndex <- Box.modelCursor `liftM` Property.get boxModelRef
  return (widget, items !! min maxIndex itemIndex)
  where
    maxIndex = length items - 1
    widgets = map fst keys
    items = map snd keys

makeChildBox :: Monad m =>
                 Int ->
                 Transaction.Property ViewTag m [ITreeD] ->
                 Transaction.Property ViewTag m Box.Model ->
                 Transaction.Property ViewTag m Box.Model ->
                 Transaction.Property ViewTag m [ITreeD] ->
                 Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeChildBox depth clipboardRef outerBoxModelRef childrenBoxModelRef childrenIRefsRef = do
  childItems <- mapM (makeTreeEdit (depth+1) clipboardRef) =<< Property.get childrenIRefsRef
  curChildIndex <- getChildIndex . length $ childItems
  childBox <- makeBox Box.Vertical childItems childrenBoxModelRef
  return .
    Widget.weakerKeys
    (mappend
     delNodeKeymap cutNodeKeymap
     curChildIndex) .
    Widget.atDisplay (indent 5) $
    childBox
  where
    cutNodeKeymap = fromMaybe mempty .
                    liftM (Keymap.simpleton "Cut node" Config.cutKey . cutChild)
    delNodeKeymap = fromMaybe mempty .
                    liftM (Keymap.simpleton "Del node" Config.delChildKey . delChild)
    cutChild index = do
      childrenIRefs <- Property.get childrenIRefsRef
      Property.pureModify clipboardRef (childrenIRefs !! index :)
      delChild index
    delChild index = do
      Property.pureModify childrenIRefsRef $ removeAt index
      isEmpty <- null `liftM` Property.get childrenIRefsRef
      when isEmpty . Property.set outerBoxModelRef . Box.Model $ 0
    getChildIndex count = (validateIndex count . Box.modelCursor) `liftM`
                          Property.get childrenBoxModelRef
    validateIndex count index
      | 0 <= index && index < count = Just index
      | otherwise = Nothing

focusableTextView :: String -> Widget a
focusableTextView =
  Widget.takesFocus .
  (Widget.atDisplay . Align.to . pure $ 0) .
  Widget.whenFocused modifyMkImage .
  Widget.simpleDisplay .
  TextView.make Vty.def_attr
  where
    modifyMkImage mkImage size =
      mkImage size `mappend`
      TermImage.rect (Rect (pure 0) size) (first (`Vty.with_back_color` Vty.blue))

makeTreeEdit :: Monad m =>
                Int -> Transaction.Property ViewTag m [ITreeD] ->
                IRef TreeD ->
                Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeTreeEdit depth clipboardRef treeIRef
  | depth >= Config.maxDepth =
    return $ Widget.strongerKeys goInKeymap $ focusableTextView "[Go deeper]"
  | otherwise = do
    valueEdit <- (Widget.atKeymap . Keymap.removeKeys)
                 [Config.expandKey, Config.collapseKey] `liftM`
                 makeTextEdit 1 TextEdit.defaultAttr TextEdit.editingAttr
                 valueTextEditModelRef
    isExpanded <- Property.get isExpandedRef
    lowRow <- if isExpanded
              then ((:[]) .
                    Widget.weakerKeys moveToParentKeymap) `liftM`
                   makeChildBox depth clipboardRef outerBoxModelRef childrenBoxModelRef childrenIRefsRef
              else return []
    cValueEdit <- makeBox Box.Horizontal
                  [collapser isExpanded,
                   Widget.simpleDisplay $ widthSpace 1,
                   valueEdit]
                  (treeNodeBoxModelRef 2) -- 2 points to valueEdit
    outerBox <- makeBox Box.Vertical (cValueEdit : lowRow) outerBoxModelRef
    clipboard <- Property.get clipboardRef
    let keymap =
          mconcat [
            pasteKeymap clipboard,
            appendNewNodeKeymap,
            setFocalPointKeymap,
            expandCollapseKeymap isExpanded
            ]
    return . Widget.weakerKeys keymap $ outerBox
    where
      goInKeymap = Keymap.simpleton "Go deeper" Config.actionKey setFocalPoint
      treeRef = Transaction.fromIRef treeIRef
      valueRef = Data.nodeValue `composeLabel` treeRef
      boxModelsContainer def k = Data.boxModel def k `composeLabel` valueRef
      valueTextEditModelRef = Data.textEditModel `composeLabel` valueRef
      childrenIRefsRef = Data.nodeChildrenRefs `composeLabel` treeRef
      isExpandedRef = Data.isExpanded `composeLabel` valueRef
      outerBoxModelRef = boxModelsContainer Box.initModel "outer"
      treeNodeBoxModelRef initValue = boxModelsContainer (Box.Model initValue) "treeNode"
      childrenBoxModelRef = boxModelsContainer Box.initModel "children"
      expandCollapseKeymap isExpanded =
        if isExpanded
        then Keymap.simpleton "Collapse" Config.collapseKey collapse
        else Keymap.simpleton "Expand" Config.expandKey expand
      collapse = Property.set isExpandedRef False
      expand = Property.set isExpandedRef True
      collapser isExpanded =
        Widget.simpleDisplay .
        TextView.make Vty.def_attr $
        if isExpanded
        then "[-]"
        else "[+]"
      pasteKeymap [] = mempty
      pasteKeymap (cbChildRef:xs) =
        Keymap.simpleton "Paste" Config.pasteKey $ do
          appendChild cbChildRef
          Property.set clipboardRef xs
      appendNewNodeKeymap = Keymap.simpleton "Append new child node"
                              Config.appendChildKey $ appendChild =<< Data.makeLeafRef ""
      moveToParentKeymap = Keymap.simpleton "Move to parent" Config.moveToParentKey .
                           Property.set outerBoxModelRef $ Box.Model 0
      setFocalPointKeymap = Keymap.simpleton "Set focal point" Config.setFocalPointKey $ setFocalPoint
      setFocalPoint = Property.pureModify Anchors.focalPointIRefs (treeIRef:)
      appendChild newRef = do
        appendBoxChild childrenBoxModelRef childrenIRefsRef newRef
        Property.set outerBoxModelRef $ Box.Model 1

makeEditWidget :: Monad m =>
                  Transaction.Property ViewTag m [ITreeD] ->
                  Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeEditWidget clipboardRef = do
  focalPointIRefs <- Property.get focalPointIRefsRef
  treeEdit <- makeTreeEdit 0 clipboardRef (foldr const Anchors.rootIRef focalPointIRefs)
  widget <-
    if not $ isAtRoot focalPointIRefs
    then makeBox Box.Vertical [goUpButton, treeEdit] (Anchors.viewBoxsAnchor "goUp")
    else return treeEdit
  return .
    Widget.strongerKeys (goUpKeymap focalPointIRefs) $
    widget
  where
    goUpButton = Widget.strongerKeys (Keymap.simpleton "Go up" Config.actionKey goUp) $
                 focusableTextView "[go up]"
    focalPointIRefsRef = Anchors.focalPointIRefs
    isAtRoot = null
    goUpKeymap focalPointIRefs =
      if isAtRoot focalPointIRefs
      then mempty
      else Keymap.simpleton "Go up" Config.goUpKey goUp
    goUp = Property.pureModify focalPointIRefsRef (drop 1)

-- Take a widget parameterized on transaction on views (that lives in
-- a nested transaction monad) and convert it to one parameterized on
-- the nested transaction
type MWidget m = m (Widget (m ()))
widgetDownTransaction :: Monad m =>
                         Store t m ->
                         MWidget (Transaction t m) ->
                         MWidget m
widgetDownTransaction store = runTrans . (liftM . fmap) runTrans
  where
    runTrans = Transaction.run store

branchSelectorBoxModel :: Monad m => Transaction.Property DBTag m Box.Model
branchSelectorBoxModel = Anchors.dbBoxsAnchor "branchSelector"

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: Monad m => View -> Transaction DBTag m (Widget (Transaction DBTag m ()))
makeWidgetForView view = do
  versionData <- Version.versionData =<< View.curVersion view
  widget <- widgetDownTransaction (Anchors.viewStore view) $
            makeEditWidget Anchors.clipboard
  return $ Widget.strongerKeys (keymaps versionData) widget
  where
    keymaps versionData = undoKeymap versionData `mappend` makeBranchKeymap
    makeBranchKeymap = Keymap.simpleton "New Branch" Config.makeBranchKey makeBranch
    makeBranch = do
      branch <- Branch.new =<< View.curVersion view
      textEditModelIRef <- Transaction.newIRef $ TextEdit.initModel "New view"
      let viewPair = (textEditModelIRef, branch)
      appendBoxChild branchSelectorBoxModel Anchors.branches viewPair
    undoKeymap versionData =
        if Version.depth versionData > 1
        then Keymap.simpleton "Undo" Config.undoKey .
             View.move view .
             fromJust . Version.parent $
             versionData
        else mempty

main :: IO ()
main = Db.withDb "/tmp/db.db" $ runDbStore . Anchors.dbStore
  where
    runDbStore store = do
      Anchors.initDB store
      Run.widgetLoopWithOverlay 20 30 . const . makeWidget $ store
    makeWidget dbStore = widgetDownTransaction dbStore $ do
      view <- Property.get Anchors.view
      branches <- Property.get Anchors.branches
      pairs <- mapM pair branches
      (branchSelector, branch) <- makeChoiceWidget Box.Vertical pairs branchSelectorBoxModel
      View.setBranch view branch
      viewEdit <- Widget.strongerKeys quitKeymap
                  `liftM` makeWidgetForView view
      makeBox Box.Horizontal
        [viewEdit,
         Widget.simpleDisplay Spacer.makeHorizontal,
         Widget.strongerKeys (delBranchKeymap branches) branchSelector] $
        Anchors.dbBoxsAnchor "main"

    pair (textEditModelIRef, version) = do
      textEdit <- simpleTextEdit . Transaction.fromIRef $ textEditModelIRef
      return (textEdit, version)

    delBranchKeymap [_] = mempty
    delBranchKeymap _ = Keymap.simpleton "Delete Branch" Config.delBranchKey deleteCurBranch
    quitKeymap = Keymap.simpleton "Quit" Config.quitKey . fail $ "Quit"

    deleteCurBranch = do
      _ <- popCurChild branchSelectorBoxModel Anchors.branches
      return ()

    simpleTextEdit =
      makeTextEdit 1
      TextEdit.defaultAttr
      TextEdit.editingAttr
