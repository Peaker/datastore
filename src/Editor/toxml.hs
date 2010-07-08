{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Db
import Data.IRef(IRef)
import Data.Store(Store)
import qualified Data.Store as Store
import qualified Data.Revision as Revision
import System.IO(stdout, hPutStrLn, Handle)
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Editor.Data as Data
import qualified Editor.Anchors as Anchors

writeTreeXml :: Store d => d -> Handle -> Int -> IRef Data.TreeD -> IO ()
writeTreeXml store outFile depth iref = do
  let ref = Store.fromIRef store iref
  value <- Store.get =<< Store.follow (Data.nodeValueRef `Store.composeLabel` ref)
  childrenIRefs <- Store.get (Data.nodeChildrenRefs `Store.composeLabel` ref)
  let text = TextEdit.textEditText . snd $ value
      indent = (replicate (2 * depth) ' ' ++)
  hPutStrLn outFile . indent $ "<" ++ text ++ ">"
  mapM_ (writeTreeXml store outFile (depth + 1)) childrenIRefs
  hPutStrLn outFile . indent $ "</" ++ text ++ ">"

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \dbStore -> do
    viewRef <- (Revision.ViewRef dbStore . snd . head) `fmap` Store.get (Anchors.views dbStore)
    writeTreeXml viewRef stdout 0 =<< Store.get (Anchors.rootIRef viewRef)
