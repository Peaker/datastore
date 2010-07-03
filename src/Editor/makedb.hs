{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Data.IRef as IRef
import qualified Db
import qualified Db.Ref as Ref
import qualified Editor.Tree as Tree

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \db -> do
    let store = Ref.store db
    childrenRefs <- mapM (Tree.makeLeafRef store . show) [1..10 :: Int]
    rootIRef <- Tree.makeNodeRef store "tree root value" childrenRefs
    IRef.set (IRef.anchorRef store "root") rootIRef
    IRef.set (IRef.anchorRef store "viewroot") rootIRef
    IRef.set (IRef.anchorRef store "clipboard") =<< IRef.newIRef store ([] :: [Tree.ITreeD])
