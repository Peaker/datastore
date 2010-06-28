{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Main(main) where

import Prelude hiding ((.))
import Control.Category((.))
import qualified Editor.Config as Config
import qualified Db
import Db(Db)
import qualified Db.Ref as Ref
import Db.Accessor(Accessor(..), composeLabel)
import qualified Db.Accessor as Accessor
import qualified Editor.Tree as Tree
import Data.Record.Label.Tuple(first, second)
import Data.Monoid(mempty, mappend)
import Data.Maybe(fromMaybe)
import Data.ByteString.UTF8(fromString)
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

setViewRoot :: Db -> Tree.Ref -> IO ()
setViewRoot db = Db.set db (fromString "viewroot")

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

makeTreeEdit :: Db -> [Tree.Ref] -> Tree.Ref -> IO (Widget (IO ()))
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
            [] -> mempty
            (cbChildRef:xs) ->
              Keymap.simpleton "Paste" Config.pasteKey $ do
                appendChild cbChildRef
                Db.set db (fromString "clipboard") xs
        appendNewNodeKeymap = Keymap.simpleton "Append new child node"
                              Config.appendChildKey appendNewChild
        cutNodeKeymap = fromMaybe mempty .
                        fmap (Keymap.simpleton "Cut node" Config.cutKey . cutChild)
        delNodeKeymap = fromMaybe mempty .
                        fmap (Keymap.simpleton "Del node" Config.delChildKey . delChild)
        setRootKeymap =
          Keymap.simpleton "Set root element" Config.setViewRootKey $
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
          Db.modify db (fromString "clipboard") ((childrenRefs !! index :) . fromMaybe [])
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
  Db.withDb "/tmp/db.db" $ Run.widgetLoopWithOverlay . const . makeWidget
  where
    makeWidget db = do
      Just clipboard <- Db.lookup db (fromString "clipboard")
      Just rootRef <- Db.lookup db (fromString "viewroot")
      treeEdit <- makeTreeEdit db clipboard rootRef
      let treeEditWithKeys =
            Widget.strongerKeys
            (quitKeymap `mappend` goRootKeymap)
            treeEdit
      return treeEditWithKeys
      where
        quitKeymap = Keymap.simpleton "Quit" Config.quitKey . ioError . userError $ "Quit"
        goRootKeymap =
          Keymap.simpleton "Go to root" Config.rootKey $ do
            Just rootRef <- Db.lookup db (fromString "root")
            Db.set db (fromString "viewroot") (rootRef :: Tree.Ref)
