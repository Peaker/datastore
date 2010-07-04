{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Db
import Data.IRef(IRef, Store)
import qualified Data.IRef as IRef
import qualified Data.Revision as Revision
import System.IO(stdout, hPutStrLn, Handle)
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Editor.Data as Data

writeTreeXml :: Store d => d -> Handle -> Int -> IRef Data.TreeD -> IO ()
writeTreeXml store outFile depth iref = do
  let ref = IRef.fromIRef store iref
  value <- IRef.get =<< IRef.follow (Data.nodeValueRef `IRef.composeLabel` ref)
  childrenIRefs <- IRef.get (Data.nodeChildrenRefs `IRef.composeLabel` ref)
  let text = TextEdit.textEditText . snd $ value
      indent = (replicate (2 * depth) ' ' ++)
  hPutStrLn outFile . indent $ "<" ++ text ++ ">"
  mapM_ (writeTreeXml store outFile (depth + 1)) childrenIRefs
  hPutStrLn outFile . indent $ "</" ++ text ++ ">"

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \dbStore -> do
    viewRef <- Revision.ViewRef dbStore `fmap` IRef.get (Data.masterViewIRefRef dbStore)
    writeTreeXml viewRef stdout 0 =<< IRef.get (Data.rootIRefRef viewRef)
