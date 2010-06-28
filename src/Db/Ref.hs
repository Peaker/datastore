{-# OPTIONS -O2 -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Db.Ref
    (DBRef, dbrefGuid, new, read, write, modify, pureModify,
     accessor, follow)
where

import Prelude hiding (read)

import Data.Property(Property(..))
import qualified Data.Property as Property
import Data.Binary(Binary)
import Control.Monad((<=<), liftM)
import qualified Db
import Db(Db)
import Db.Guid(Guid(..))
import qualified Db.Guid as Guid
import Db.Accessor(Accessor(..))

-- DBRef:

newtype DBRef a = DBRef {
  dbrefGuid :: Guid
  }
  deriving (Eq, Ord, Binary)

property :: Binary a => Db -> DBRef a -> Property IO a
property db ref = Property (read db ref) (write db ref)

accessor :: Binary a => Db -> DBRef a -> Accessor a
accessor db = Accessor db . property db

-- Convenience method (Doesn't use the setter, only the getter):
follow :: Binary a => Accessor (DBRef a) -> IO (Accessor a)
follow (Accessor db prop) =
  accessor db `liftM` Property.get prop

new :: Binary a => Db -> a -> IO (DBRef a)
new db x = do
  key <- Guid.new
  Db.set db (guidBS key) x
  return (DBRef key)

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
