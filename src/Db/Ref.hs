{-# OPTIONS -O2 -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Db.Ref
    (DBRef, new, read, write, modify, pureModify,
     accessor, follow)
where

import Prelude hiding (read)

import Data.Property(Property(..))
import qualified Data.Property as Property
import Data.Binary(Binary)
import Control.Monad((<=<), liftM)
import qualified Db
import Db(Db)
import Db.Guid(Guid(..), newGuid)
import Db.Accessor(Accessor(..))

-- DBRef:

property :: Binary a => Db -> DBRef a -> Property IO a
property db ref = Property (read db ref) (write db ref)

accessor :: Binary a => Db -> DBRef a -> Accessor a
accessor db = Accessor db . property db

-- Convenience method (Doesn't use the setter, only the getter):
follow :: Binary a => Accessor (DBRef a) -> IO (Accessor a)
follow (Accessor db prop) =
  accessor db `liftM` Property.get prop

writeKey :: Binary a => Db -> Guid -> a -> IO (DBRef a)
writeKey db key x = do
  Db.set db (guidBS key) x
  return (DBRef key)

newtype DBRef a = DBRef {
  dbrefGuid :: Guid
  }
  deriving (Eq, Ord, Binary)

new :: Binary a => Db -> a -> IO (DBRef a)
new db x = do
  key <- newGuid
  writeKey db key x

write :: Binary a => Db -> DBRef a -> a -> IO ()
write db =
  Db.set db . guidBS . dbrefGuid

read :: Binary a => Db -> DBRef a -> IO a
read db (DBRef key) = do
  Just bs <- Db.lookup db . guidBS $ key
  return bs

modify :: Binary a => Db -> DBRef a -> (a -> IO a) -> IO ()
modify db key f = (write db key <=< f) =<< read db key

pureModify :: Binary a => Db -> DBRef a -> (a -> a) -> IO ()
pureModify db key f = modify db key (return . f)
