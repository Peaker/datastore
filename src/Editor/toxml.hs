{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Db
import Db(Db)
import qualified Editor.Tree as Tree
import qualified Db.Ref as Ref
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import System.IO(stdout, hPutStrLn, Handle)
import Data.ByteString.UTF8(fromString)
import qualified Data.Record.Label as Label

writeTreeXml :: Db -> Handle -> Int -> Tree.Ref -> IO ()
writeTreeXml db outFile depth ref = do
  tree <- Ref.read db ref
  value <- Ref.read db (Label.get Tree.nodeValueRef tree)
  let text = TextEdit.textEditText . snd $ value
      indent = (replicate (2 * depth) ' ' ++)
  hPutStrLn outFile . indent $ "<" ++ text ++ ">"
  mapM_ (writeTreeXml db outFile (depth + 1)) $
    Label.get Tree.nodeChildrenRefs tree
  hPutStrLn outFile . indent $ "</" ++ text ++ ">"

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \db -> do
    Just root <- Db.lookup db (fromString "root")
                 -- root :: Tree.Ref
    writeTreeXml db stdout 0 root
