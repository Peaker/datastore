{-# OPTIONS -O2 -Wall #-}

module Db
    (Db, -- re-export
     lookup, insert, create)
where

import Prelude hiding (lookup)
import Database.Berkeley.Db
    ( DbType(DB_BTREE),
      Db,
      DbFlag(DB_CREATE, DB_INIT_MPOOL),
      dbEnv_create,
      dbEnv_open,
      db_create,
      db_get,
      db_open,
      db_put )
import Data.ByteString(ByteString)
import System.Directory(createDirectoryIfMissing)

envDir :: FilePath
envDir = "/tmp/dbenv"

create :: FilePath -> IO Db
create fileName = do
  createDirectoryIfMissing False envDir
  dbEnv <- dbEnv_create []
  dbEnv_open [DB_CREATE, DB_INIT_MPOOL] 0 dbEnv envDir
  db <- db_create [] dbEnv
  db_open [DB_CREATE] DB_BTREE 0 db Nothing fileName (Just "DB title")
  return db

lookup :: Db -> ByteString -> IO (Maybe ByteString)
lookup db = db_get [] db Nothing

insert :: Db -> ByteString -> ByteString -> IO ()
insert db = db_put [] db Nothing

-- main = do
--   db <- create "/tmp/shit"
--   dbPut db (pack "Key") (pack "Value")
--   let bigStr = pack (replicate 1024000 'k')
--   dbPut db bigStr bigStr
--   print . isJust =<< get db bigStr
--   print . isJust =<< get db (pack "Key")
