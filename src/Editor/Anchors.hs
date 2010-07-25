{-# LANGUAGE EmptyDataDecls #-}

module Editor.Anchors(
    clipboard, root, rootIRef,
    focalPointIRef, branches, versionMap,
    viewGridsAnchor, dbGridsAnchor,
    initDB,
    dbStore, DBTag,
    viewStore, ViewTag)
where

import Control.Monad(liftM, unless)
import Data.Binary(Binary)
import Data.IRef(IRef)
import qualified Data.IRef as IRef
import qualified Data.Transaction as Transaction
import Data.Transaction(Transaction, Store)
import Data.Rev.Branch(Branch)
import Data.Rev.VersionMap(VersionMap)
import Data.Rev.View(View)
import qualified Data.Rev.View as View
import qualified Data.Rev.Branch as Branch
import qualified Data.Rev.Version as Version
import qualified Data.Rev.VersionMap as VersionMap
import qualified Data.Property as Property
import qualified Db
import Db(Db)
import Editor.Data(ITreeD, TreeD)
import qualified Editor.Data as Data
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: Monad m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

clipboard :: Monad m => Transaction.Property ViewTag m [ITreeD]
clipboard = Transaction.anchorRefDef "clipboard" []

rootIRef :: ITreeD
rootIRef = IRef.anchor "root"

root :: Monad m => Transaction.Property ViewTag m TreeD
root = Transaction.fromIRef rootIRef

focalPointIRef :: Monad m => Transaction.Property ViewTag m ITreeD
focalPointIRef = Transaction.anchorRefDef "focalPoint" rootIRef

gridsAnchor :: Monad m => String -> String -> Transaction.Property anyTag m Grid.Model
gridsAnchor name = Transaction.containerStr . Transaction.anchorContainerDef name $ Grid.initModel

viewGridsAnchor :: Monad m => String -> Transaction.Property ViewTag m Grid.Model
viewGridsAnchor = gridsAnchor "GUI.grids(v)"

dbGridsAnchor :: Monad m => String -> Transaction.Property DBTag m Grid.Model
dbGridsAnchor = gridsAnchor "GUI.grids(d)"

branchesIRef :: IRef [(IRef TextEdit.Model, Branch)]
branchesIRef = IRef.anchor "branches"

branches :: Monad m => Transaction.Property DBTag m [(IRef TextEdit.Model, Branch)]
branches = Transaction.fromIRef branchesIRef

initRef :: (Binary a, Monad m) => IRef a -> Transaction t m a -> Transaction t m a
initRef iref act = do
  exists <- Transaction.irefExists iref
  unless exists (Property.set p =<< act)
  Property.get p
  where
    p = Transaction.fromIRef iref

versionMapIRef :: IRef VersionMap
versionMapIRef = IRef.anchor "HEAD"

versionMap :: Monad m => Transaction.Property DBTag m VersionMap
versionMap = Transaction.fromIRef versionMapIRef

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef branchesIRef $ do
      masterNameIRef <- Transaction.newIRef $ TextEdit.initModel "master"
      initialVersionIRef <- Version.makeInitialVersion
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    vm <- initRef versionMapIRef $
      VersionMap.new =<<
      Branch.curVersion =<<
      (snd . head) `liftM`
      Property.get branches
    let branch = (snd . head $ bs)
    _ <- Transaction.run (viewStore $ View.make vm branch) . initRef rootIRef $
      return $ Data.makeNode "" []
    return ()
