{-# OPTIONS -O2 -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

module Ref
    (DBRef, new, read, write, modify, pureModify, accessor, follow)
where

import Prelude hiding (read)

import qualified Db
import Db(Db)

import Property(Property(..))
import qualified Property
import Accessor(Accessor(..))
import Control.Monad((<=<), liftM)
import qualified Data.ByteString as SBS
import Data.Binary(Binary(..))
import Data.Binary.Get(getByteString)
import Data.Binary.Put(putByteString)
import ByteStringUtils(randomBS)
import RandInstances()

guidLen :: Int
guidLen = 16

type Guid = SBS.ByteString

-- DBRef:

property :: Binary a => Db -> DBRef a -> Property IO a
property db ref = Property (read db ref) (write db ref)

accessor :: Binary a => Db -> DBRef a -> Accessor a
accessor db = Accessor db . property db

-- Convenience method (Doesn't use the setter, only the getter):
follow :: Binary a => Accessor (DBRef a) -> IO (Accessor a)
follow (Accessor db prop) =
  accessor db `liftM` Property.get prop

writeKey :: Binary a => Db -> SBS.ByteString -> a -> IO (DBRef a)
writeKey db key x = do
  Db.set db key x
  return (DBRef key)

newtype DBRef a = DBRef {
  dbrefGuid :: Guid
  }

instance Binary (DBRef a) where
  get = DBRef `fmap` getByteString guidLen
  put = putByteString . dbrefGuid

new :: Binary a => Db -> a -> IO (DBRef a)
new db x = do
  key <- randomBS guidLen
  writeKey db key x

write :: Binary a => Db -> DBRef a -> a -> IO ()
write db = do
  Db.set db . dbrefGuid

read :: Binary a => Db -> DBRef a -> IO a
read db (DBRef key) = do
  Just bs <- Db.lookup db key
  return bs

modify :: Binary a => Db -> DBRef a -> (a -> IO a) -> IO ()
modify db key f = (write db key <=< f) =<< read db key

pureModify :: Binary a => Db -> DBRef a -> (a -> a) -> IO ()
pureModify db key f = modify db key (return . f)
