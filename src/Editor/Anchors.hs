{-# LANGUAGE EmptyDataDecls #-}

module Editor.Anchors(
    clipboard, root, rootIRef,
    focalPointIRef, branches, view,
    viewBoxsAnchor, dbBoxsAnchor,
    initDB,
    dbStore, DBTag,
    viewStore, ViewTag)
where

import           Control.Monad                   (unless)
import           Data.Binary                     (Binary)
import           Data.IRef                       (IRef)
import qualified Data.IRef                       as IRef
import qualified Data.Transaction                as Transaction
import           Data.Transaction                (Transaction, Store)
import           Data.Rev.Branch                 (Branch)
import           Data.Rev.View                   (View)
import qualified Data.Rev.Branch                 as Branch
import qualified Data.Rev.Version                as Version
import qualified Data.Rev.View                   as View
import qualified Data.Property                   as Property
import qualified Db
import           Db                              (Db)
import           Editor.Data                     (ITreeD, TreeD)
import qualified Editor.Data                     as Data
import qualified Graphics.UI.VtyWidgets.Box      as Box
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

boxsAnchor :: Monad m => String -> String -> Transaction.Property anyTag m Box.Model
boxsAnchor name = Transaction.containerStr . Transaction.anchorContainerDef name $ Box.initModel

viewBoxsAnchor :: Monad m => String -> Transaction.Property ViewTag m Box.Model
viewBoxsAnchor = boxsAnchor "GUI.box(v)"

dbBoxsAnchor :: Monad m => String -> Transaction.Property DBTag m Box.Model
dbBoxsAnchor = boxsAnchor "GUI.box(d)"

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

viewIRef :: IRef View
viewIRef = IRef.anchor "HEAD"

view :: Monad m => Transaction.Property DBTag m View
view = Transaction.fromIRef viewIRef

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef branchesIRef $ do
      masterNameIRef <- Transaction.newIRef $ TextEdit.initModel "master"
      initialVersionIRef <- Version.makeInitialVersion
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    curView <- initRef viewIRef $ View.new (snd . head $ bs)
    _ <- Transaction.run (viewStore curView) . initRef rootIRef $
      return $ Data.makeNode "" []
    return ()
