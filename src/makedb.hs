{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Db
import Data.ByteString.UTF8(fromString)
import Tree(makeLeafRef, makeNodeRef)

main :: IO ()
main = do
  Db.withDb "/tmp/db.db" $ \db -> do
    childrenRefs <- mapM (makeLeafRef db . show) [1..10 :: Int]
    makeNodeRef db "tree root value" childrenRefs >>=
      Db.set db (fromString "root")