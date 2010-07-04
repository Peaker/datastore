{-# OPTIONS -O2 -Wall #-}

module Db
    (Db, withDb,
     withCursor, nextKeyBS, nextKey)
where

import Data.Binary.Utils(decodeS)
import Control.Arrow(second)
import Control.Exception(bracket)
import Prelude hiding (lookup)
import qualified Database.Berkeley.Db as Berkeley
import qualified Data.IRef as IRef
import Data.ByteString(ByteString)
import Data.Binary(Binary)
import System.Directory(createDirectoryIfMissing)

data Db = Db {
  dbBerkeley :: Berkeley.Db,
  _dbEnv :: Berkeley.DbEnv
  }

type Cursor = Berkeley.DbCursor

envDir :: FilePath
envDir = "/tmp/dbenv"

open :: FilePath -> IO Db
open fileName = do
  createDirectoryIfMissing False envDir
  env <- Berkeley.dbEnv_create []
  Berkeley.dbEnv_open [Berkeley.DB_CREATE, Berkeley.DB_INIT_MPOOL] 0 env envDir
  db <- Berkeley.db_create [] env
  Berkeley.db_open [Berkeley.DB_CREATE] Berkeley.DB_BTREE 0 db Nothing fileName (Just "DB title")
  return (Db db env)

close :: Db -> IO ()
close (Db db env) = do
  Berkeley.db_close [] db
  -- TODO: waitForEmptyMap m
  Berkeley.dbEnv_close [] env

withCursor :: Db -> (Cursor -> IO a) -> IO a
withCursor db = Berkeley.db_withCursor [] (dbBerkeley db) Nothing

nextKeyBS :: Cursor -> IO (Maybe (ByteString, ByteString))
nextKeyBS = Berkeley.dbCursor_get [Berkeley.DB_NEXT]

nextKey :: Binary a => Cursor -> IO (Maybe (ByteString, a))
nextKey cursor = (fmap . fmap . second) decodeS (nextKeyBS cursor)

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb filePath = bracket (open filePath) close

instance IRef.Store Db where
  lookupBS db = Berkeley.db_get [] (dbBerkeley db) Nothing
  insertBS db = Berkeley.db_put [] (dbBerkeley db) Nothing
  deleteBS db = Berkeley.db_del [] (dbBerkeley db) Nothing
