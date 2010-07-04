{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Data.IRef as IRef
import qualified Db
import qualified Editor.Data as Data

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \store -> do
    childrenRefs <- mapM (Data.makeLeafRef store . show) [1..10 :: Int]
    rootIRef <- Data.makeNodeRef store "tree root value" childrenRefs
    IRef.set (Data.rootIRefRef store) rootIRef
    IRef.set (Data.viewRootIRefRef store) rootIRef
    IRef.set (Data.clipboardIRefRef store) =<< IRef.newIRef store ([] :: [Data.ITreeD])
