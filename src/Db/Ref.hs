{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Db.Ref(DBRef, store, unStore)
where

import Prelude hiding (read)

import qualified Data.IRef as IRef
import Data.IRef(IRef, Ref(..), Property(..))
import Data.Binary(Binary)
import Data.Maybe(fromJust)
import qualified Db
import Db(Db)
import Data.Guid(Guid(..))
import qualified Data.Guid as Guid
import qualified Data.Record.Label as Label

-- DBRef:

data DBRef a = DBRef { dbrefRead :: IO a, dbrefWrite :: a -> IO () }

readDb :: Binary a => Db -> IRef a -> IO a
readDb db = fmap fromJust . Db.lookup db . guidBS . IRef.guid

writeDb :: Binary a => Db -> IRef a -> a -> IO ()
writeDb db = Db.set db . guidBS . IRef.guid

instance Property DBRef where
  set = dbrefWrite
  get = dbrefRead
  composeLabel label (DBRef r w) = DBRef read' write'
    where
      read' = Label.get label `fmap` r
      write' x = w . Label.set label x =<< r

instance Ref DBRef where
  newtype Store DBRef = DBRefStore { unStore :: Db }
  newIRef (DBRefStore db) x = do
    iref <- IRef.unsafeFromGuid `fmap` Guid.new
    writeDb db iref x
    return iref
  fromIRef (DBRefStore db) iref = DBRef (readDb db iref) (writeDb db iref)

store :: Db -> Store DBRef
store = DBRefStore
