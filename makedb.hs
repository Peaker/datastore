{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Db
import qualified ObjectStore
import qualified Data.ByteString.Char8 as SBS8
import ByteStringUtils(encodeS)

ints :: [Int]
ints = [1..10]

main :: IO ()
main = do
  db <- Db.open "/tmp/db.db"
  keys <- mapM (ObjectStore.insert db . show) ints
  Db.insert db (SBS8.pack "root") (encodeS keys)
