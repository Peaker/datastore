{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Db
import qualified Ref
import Data.ByteString.UTF8(fromString)
import Tree(Tree(..))

main :: IO ()
main = do
  Db.withDb "/tmp/db.db" $ \db -> do
    ref <- Ref.new db "hello"
    strings <- mapM (mkSubTree db . show) [1..10 :: Int]
    children <- mapM (Ref.new db) strings
    let rootKey = fromString "root"
    treeRoot <- Ref.new db $ Node ref children
    Db.set db rootKey treeRoot
  where
    mkSubTree db i = do
      ref <- Ref.new db i
      return (Node ref [])
