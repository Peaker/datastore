{-# OPTIONS -O2 -Wall #-}

module ObjectStore
    (ObjectStore, IRef, insert, lookup, set)
where

import qualified Db
import Db(Db)

import Prelude hiding (lookup)
import Control.Monad(replicateM)
import qualified Data.ByteString as SBS
import Data.Binary(Binary(..))
import Data.Binary.Get(getByteString)
import Data.Binary.Put(putByteString)
import ByteStringUtils(decodeS, encodeS)
import RandInstances()
import System.Random(randomIO)

guidLen :: Int
guidLen = 16

type Guid = SBS.ByteString

type ObjectStore = Db

data IRef a = IRef {
    irefGuid :: Guid
    }

instance Binary (IRef a) where
  get = fmap IRef (getByteString guidLen)
  put = putByteString . irefGuid

insert :: Binary a => ObjectStore -> a -> IO (IRef a)
insert os x = do
  key <- SBS.pack `fmap` replicateM guidLen randomIO
  let iref = (IRef key)
  set os iref x
  return iref

set :: Binary a => ObjectStore -> IRef a -> a -> IO ()
set os (IRef key) x =
  Db.insert os key . encodeS $ x

lookup :: Binary a => ObjectStore -> IRef a -> IO a
lookup os (IRef key) = do
  Just bs <- Db.lookup os key
  return . decodeS $ bs
