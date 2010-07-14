{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import Control.Monad.IO.Class(MonadIO(..))
import Data.IRef(IRef)
import Data.Transaction(Transaction)
import qualified Data.Rev.View as View
import qualified Data.Transaction as Transaction
import qualified Data.Property as Property
import qualified Db
import System.IO(stdout, hPutStrLn, Handle)
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Editor.Data as Data
import qualified Editor.Anchors as Anchors
import Editor.Anchors(ViewTag, DBTag)
import qualified Data.Record.Label as Label

writeTreeXml :: MonadIO m => Handle -> Int -> IRef Data.TreeD -> Transaction ViewTag m ()
writeTreeXml outFile depth iref = do
  let ref = Transaction.fromIRef iref
  value <- Property.get =<< Transaction.follow (Data.nodeValueRef `Property.composeLabel` ref)
  childrenIRefs <- Property.get (Data.nodeChildrenRefs `Property.composeLabel` ref)
  let text = TextEdit.textEditText . Label.get Data.textEditModel $ value
      indent = (replicate (2 * depth) ' ' ++)
  liftIO . hPutStrLn outFile . indent $ "<" ++ text ++ ">"
  mapM_ (writeTreeXml outFile (depth + 1)) childrenIRefs
  liftIO . hPutStrLn outFile . indent $ "</" ++ text ++ ">"

printXml :: Transaction DBTag IO ()
printXml = do
  versionMap <- Property.get Anchors.versionMap
  branch <- (snd . head) `fmap` Property.get Anchors.branches
  let view = View.make versionMap branch
  Transaction.run (Anchors.viewStore view) $
    writeTreeXml stdout 0 =<< Property.get Anchors.rootIRef

main :: IO ()
main = Db.withDb "/tmp/db.db" $ \db -> Transaction.run (Anchors.dbStore db) printXml
