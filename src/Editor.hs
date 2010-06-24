{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Prelude hiding ((.))
import Control.Category((.))
import qualified Db
import Data.Monoid(mappend)
import Data.ByteString.UTF8(fromString)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
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
import Accessor(Accessor(..), composeLabel)
import qualified Accessor
import qualified Tree
import Tree(Tree)
import Data.Record.Label.Tuple(first, second)

quitKey :: ModKey
quitKey = ([Vty.MCtrl], Vty.KASCII 'q')

appendChildKey :: ModKey
appendChildKey = ([Vty.MCtrl], Vty.KASCII 'n')

delChildKey :: ModKey
delChildKey = ([Vty.MCtrl], Vty.KASCII 'o')

main :: IO ()
main = do
  Db.withDb "/tmp/db.db" $ \db -> do
    Just root <- Db.lookup db (fromString "root")
    let makeWidget =
          (fmap . Widget.atKeymap)
          (`mappend` Keymap.simpleton "Quit" quitKey (ioError . userError $ "Quit")) $
          makeTreeEdit (Ref.accessor db root)
    runWidgetLoop . const $ makeWidget

indent :: Int -> Display a -> Display a
indent width disp = Grid.makeView [[Spacer.make (SizeRange.fixedSize (Vector2 width 0)), disp]]

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True  f = f
applyIf False _ = id

makeTreeEdit :: Accessor (Tree Tree.Data) -> IO (Widget (IO ()))
makeTreeEdit treeA = do
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
        childItems <- mapM (makeTreeEdit . Ref.accessor (accessorDb childrenRefsA)) =<< Accessor.get childrenRefsA
        curChildIndex <- getChildIndex
        childGrid <- makeGrid (map (: []) childItems) childrenGridModelA
        delKeymap <- delNodeKeymap
        let childGrid' = applyIf (curChildIndex < length childItems)
                         (Widget.strongerKeys delKeymap) .
                         Widget.atDisplay (indent 5) $
                         childGrid
        outerGrid <- makeGrid [[valueEdit], [childGrid']] outerGridModelA
        return (Widget.strongerKeys addNodeKeymap outerGrid)
      where
        getChildIndex = (Vector2.snd . Grid.modelCursor) `fmap`
                        Accessor.get childrenGridModelA
        addNodeKeymap = Keymap.simpleton "Append child node"
                        appendChildKey appendChild
        delNodeKeymap = (Keymap.simpleton "Del node"
                         delChildKey . delChild) `fmap` getChildIndex
        yGridCursor = Grid.Model . Vector2 0
        appendChild = do
          newRef <- Tree.makeLeafRef (accessorDb childrenRefsA) "NEW_NODE"
          Accessor.pureModify childrenRefsA (++ [newRef])
          childrenRefs <- Accessor.get childrenRefsA
          Accessor.set outerGridModelA $ yGridCursor 1
          Accessor.set childrenGridModelA $ yGridCursor (length childrenRefs - 1)
        delChild index =
          Accessor.pureModify childrenRefsA $ remove index

remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n+1) xs

makeGrid :: [[Widget (IO ())]] -> Accessor Grid.Model -> IO (Widget (IO ()))
makeGrid rows gridModelA =
  fmap (Grid.make (Accessor.set gridModelA) rows) $
  Accessor.get gridModelA

makeTextEdit :: Int -> Vty.Attr -> Vty.Attr -> Accessor TextEdit.Model -> IO (Widget (IO ()))
makeTextEdit maxLines defAttr editAttr textEditModelA =
  fmap (fmap (Accessor.set textEditModelA) .
        TextEdit.make maxLines defAttr editAttr) $
  Accessor.get textEditModelA
