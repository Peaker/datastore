{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Data.IRef as IRef
import Data.IRef(Store)
import qualified Data.Revision as Revision
import qualified Db
import qualified Editor.Data as Data

makeViewRef :: Store d => d -> IO (Revision.ViewRef d)
makeViewRef store = do
  versionIRef <- Revision.makeInitialVersion store
  Revision.makeView store versionIRef

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \dbStore -> do
    viewRef <- makeViewRef dbStore
    IRef.set (Data.masterViewIRefRef dbStore) $ Revision.viewIRef viewRef
    let store = viewRef
    childrenRefs <- mapM (Data.makeLeafRef store . show) [1..10 :: Int]
    rootIRef <- Data.makeNodeRef store "tree root value" childrenRefs
    IRef.set (Data.rootIRefRef store) rootIRef
    IRef.set (Data.viewRootIRefRef store) rootIRef
    IRef.set (Data.clipboardIRefRef store) =<< IRef.newIRef store ([] :: [Data.ITreeD])
