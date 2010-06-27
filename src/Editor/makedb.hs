{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import Data.ByteString.UTF8(fromString)
import qualified Db
import qualified Editor.Tree as Tree

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \db -> do
    childrenRefs <- mapM (Tree.makeLeafRef db . show) [1..10 :: Int]
    rootRef <- Tree.makeNodeRef db "tree root value" childrenRefs
    Db.set db (fromString "root") rootRef
    Db.set db (fromString "viewroot") rootRef
    Db.set db (fromString "clipboard") ([] :: [Tree.Ref])
