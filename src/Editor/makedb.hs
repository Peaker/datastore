{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Data.Store as Store
import Data.Store(Store)
import qualified Data.Revision as Revision
import qualified Data.Transaction as Transaction
import qualified Data.Map as Map
import qualified Db
import qualified Editor.Data as Data

makeViewRef :: Store d => d -> IO (Revision.ViewRef d)
makeViewRef store = do
  versionIRef <- Revision.makeInitialVersion store
  Revision.makeView store versionIRef

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \dbStore -> do
    masterRef <- makeViewRef dbStore
    Store.set (Data.branchesRef dbStore) $ Map.singleton "master" (Revision.viewIRef masterRef)
    Transaction.withTransaction masterRef $ \store -> do
      childrenRefs <- mapM (Data.makeLeafRef store . show) [1..10 :: Int]
      rootIRef <- Data.makeNodeRef store "tree root value" childrenRefs
      Store.set (Data.rootIRefRef store) rootIRef
      Store.set (Data.viewRootIRefRef store) rootIRef
      Store.set (Data.clipboardIRefRef store) =<< Store.newIRef store ([] :: [Data.ITreeD])
