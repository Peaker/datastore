{-# OPTIONS -O2 -Wall #-}

module Db
    (Db, -- re-export
     lookup, insert, open)
where

import Prelude hiding (lookup)
import qualified Database.Berkeley.Db as Berkeley
import Data.ByteString(ByteString)
import Data.Map(Map, (!))
import qualified Data.Map as Map
import Control.Concurrent(forkIO)
import Control.Concurrent.MVar(MVar, newMVar, readMVar, modifyMVar_)
import System.Directory(createDirectoryIfMissing)

data Db = Db {
  dbBerkeley :: Berkeley.Db,
  dbWritesInProgress :: MVar (Map ByteString (Bool, ByteString))
  }

envDir :: FilePath
envDir = "/tmp/dbenv"

open :: FilePath -> IO Db
open fileName = do
  createDirectoryIfMissing False envDir
  dbEnv <- Berkeley.dbEnv_create []
  Berkeley.dbEnv_open [Berkeley.DB_CREATE, Berkeley.DB_INIT_MPOOL] 0 dbEnv envDir
  db <- Berkeley.db_create [] dbEnv
  Berkeley.db_open [Berkeley.DB_CREATE] Berkeley.DB_BTREE 0 db Nothing fileName (Just "DB title")
  m <- newMVar Map.empty
  return (Db db m)

lookup :: Db -> ByteString -> IO (Maybe ByteString)
lookup db key = do
  inProgress <- readMVar (dbWritesInProgress db)
  case Map.lookup key inProgress of
    Nothing -> Berkeley.db_get [] (dbBerkeley db) Nothing key
    Just (_, curValue) -> return (Just curValue)

insert :: Db -> ByteString -> ByteString -> IO ()
insert db key value = do
  -- If write already in progress, schedule this write after it:
  putValue value
  where
    modifyWritesInProgress = modifyMVar_ (dbWritesInProgress db)
    putValue = modifyWritesInProgress . doPut
    doPut value' m =
      case Map.lookup key m of
        Nothing -> putKeyNow m value'
        Just _  -> return $ Map.insert key (True, value') m
    putKeyNow m value' = do
      _ <- forkIO $ do
        Berkeley.db_put [] (dbBerkeley db) Nothing key value'
        modifyWritesInProgress putFinished
      return $ Map.insert key (False, value') m
    putFinished m = do
      case m ! key of
        (False, _       ) -> return $ Map.delete key m
        (True,  curValue) -> putKeyNow m curValue
