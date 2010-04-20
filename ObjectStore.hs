{-# OPTIONS -O2 -Wall #-}

module ObjectStore
    (ObjectStore, IRef, create, insert, lookup, main)
where

import qualified Db
import Db(Db, create)

import Prelude hiding (lookup)
import Data.Binary(Binary, encode, decode)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import System.Random(randomIO)
import RandInstances()
import Control.Monad(replicateM)

type Guid = SBS.ByteString
type ObjectStore = Db

data IRef a = IRef {
    _irefGuid :: Guid,
    _irefObjectStore :: ObjectStore
    }

strictifyBS :: LBS.ByteString -> SBS.ByteString
strictifyBS = SBS.concat . LBS.toChunks

lazifyBS :: SBS.ByteString -> LBS.ByteString
lazifyBS = LBS.fromChunks . return

insert :: Binary a => a -> ObjectStore -> IO (IRef a)
insert x os = do
  key <- SBS.pack `fmap` replicateM 16 randomIO
  Db.insert os key . strictifyBS . encode $ x
  return (IRef key os)

lookup :: Binary a => IRef a -> IO a
lookup (IRef key os) = do
  Just bs <- Db.lookup os key
  return . decode . lazifyBS $ bs

0main :: IO ()
main = do
  print "Hello world!"
