{-# OPTIONS -O2 -Wall #-}

module Db
    (Db, open,
     lookupBS, setBS,
     lookup,   set)
where

import Prelude hiding (lookup)
import qualified Database.Berkeley.Db as Berkeley
import Data.ByteString(ByteString)
import ByteStringUtils(encodeS, decodeS)
import Data.Binary(Binary)
import Data.Map(Map, (!))
import qualified Data.Map as Map
import Control.Concurrent(forkOS)
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

lookup :: Binary a => Db -> ByteString -> IO (Maybe a)
lookup db key = (fmap . fmap) decodeS (lookupBS db key)

lookupBS :: Db -> ByteString -> IO (Maybe ByteString)
lookupBS db key = do
  inProgress <- readMVar (dbWritesInProgress db)
  case Map.lookup key inProgress of
    Nothing -> Berkeley.db_get [] (dbBerkeley db) Nothing key
    Just (_, curValue) -> return (Just curValue)

set :: Binary a => Db -> ByteString -> a -> IO ()
set db key = setBS db key . encodeS

setBS :: Db -> ByteString -> ByteString -> IO ()
setBS db key value = do
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
      _ <- forkOS $ do
        Berkeley.db_put [] (dbBerkeley db) Nothing key value'
        modifyWritesInProgress putFinished
      return $ Map.insert key (False, value') m
    putFinished m = do
      case m ! key of
        (False, _       ) -> return $ Map.delete key m
        (True,  curValue) -> putKeyNow m curValue
