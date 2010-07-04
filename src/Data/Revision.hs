{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TemplateHaskell #-}
module Data.Revision
    (Change(..), Version(..), View(..), ViewRef(..),
     makeInitialVersion,
     makeView, makeVersion, makeVersionOnView,
     moveView, makeChange)
where

import Prelude hiding (lookup)
import Control.Monad(liftM3, (<=<))
import Data.Binary(Binary(..))
import Data.ByteString(ByteString)
import Data.ByteString.Utils(xorBS)
import Data.Record.Label((:->), mkLabels, label)
import qualified Data.Record.Label as Label
import qualified Data.IRef as IRef
import Data.IRef(IRef, Store)
import qualified Data.Guid as Guid

type ObjectKey = ByteString
type ObjectValue = ByteString

data Change = Change {
  _objectKey :: ObjectKey,
  _oldValue :: Maybe ObjectValue,
  _newValue :: Maybe ObjectValue
  }
  deriving (Eq, Ord, Show, Read)
$(mkLabels [''Change])
objectKey :: Change :-> ObjectKey
oldValue :: Change :-> Maybe ObjectValue
newValue :: Change :-> Maybe ObjectValue
instance Binary Change where
  get = liftM3 Change get get get
  put (Change key old new) = put key >> put old >> put new

type ChangeDir = Change :-> Maybe ObjectValue

data Version = Version {
  versionDepth :: Int,
  versionParent :: Maybe (IRef Version),
  versionChanges :: [Change]
  }
  deriving (Eq, Ord)
instance Binary Version where
  get = liftM3 Version get get get
  put (Version depth parent changes) = put depth >> put parent >> put changes


newtype View = View {
  viewVersion :: IRef Version
  }
  deriving (Eq, Ord, Binary)
data ViewRef d = ViewRef { viewRefStore :: d,
                           viewIRef :: IRef View }
  deriving (Eq, Ord)

viewKey :: ViewRef d -> ObjectKey -> ByteString
viewKey = xorBS . Guid.bs . IRef.guid . viewIRef

setViewValue :: Store d => ViewRef d -> ObjectKey -> Maybe ObjectValue -> IO ()
setViewValue viewRef objKey mbValue =
  case mbValue of
    Nothing ->    IRef.deleteBS store key
    Just value -> IRef.insertBS store key value
  where
    store = viewRefStore viewRef
    key = viewKey viewRef objKey

applyChanges :: Store d => ViewRef d -> ChangeDir -> [Change] -> IO ()
applyChanges viewRef changeDir = mapM_ applyChange
  where
    applyChange change = setViewValue viewRef
                         (Label.get objectKey change)
                         (Label.get changeDir change)

buildViewFromVersion :: Store d => ViewRef d -> Version -> IO ()
buildViewFromVersion viewRef = loop
  where
    store = viewRefStore viewRef
    loop (Version _ mbParentIRef changes) = do
      maybe (return ()) (loop <=< IRef.getIRef store) mbParentIRef
      applyChanges viewRef newValue changes

makeView :: Store d => d -> IRef Version -> IO (ViewRef d)
makeView store versionIRef = do
  viewRef <- ViewRef store `fmap` IRef.newIRef store (View versionIRef)
  buildViewFromVersion viewRef =<< IRef.getIRef store versionIRef
  return viewRef

makeInitialVersion :: Store d => d -> IO (IRef Version)
makeInitialVersion store = IRef.newIRef store $ Version 0 Nothing []

makeVersion :: Store d => d -> IRef Version -> [Change] -> IO (IRef Version)
makeVersion store versionIRef changes = do
  depth <- versionDepth `fmap` IRef.getIRef store versionIRef
  IRef.newIRef store (Version (depth+1) (Just versionIRef) changes)

makeVersionOnView :: Store d => ViewRef d -> [Change] -> IO ()
makeVersionOnView viewRef changes = do
  let viewStoreRef = IRef.fromIRef store . viewIRef $ viewRef
  View versionIRef <- IRef.get viewStoreRef
  versionIRef' <- makeVersion store versionIRef changes
  applyChanges viewRef newValue changes
  IRef.set viewStoreRef $ View versionIRef'
  where
    store = viewRefStore viewRef

makeChange :: Store d => ViewRef d -> ObjectKey -> Maybe ObjectValue -> IO Change
makeChange viewRef key value = do
  old <- IRef.lookupBS viewRef key
  return . Change key old $ value

mostRecentAncestor :: Store d => d -> IRef Version -> IRef Version -> IO (IRef Version)
mostRecentAncestor store aIRef bIRef
  | aIRef == bIRef  = return aIRef
  | otherwise       = do
    a <- IRef.getIRef store $ aIRef
    b <- IRef.getIRef store $ bIRef
    climb a b
  where
    climb
      (Version aDepth (Just aParentRef) _aChanges)
      (Version bDepth (Just bParentRef) _bChanges)
      | aDepth == bDepth  =  mostRecentAncestor store aParentRef bParentRef
      | aDepth < bDepth   =  mostRecentAncestor store aIRef bParentRef
      | aDepth > bDepth   =  mostRecentAncestor store aParentRef bIRef
    climb _ _ = fail "Given two versions without a common ancestor"

walkUp :: Store d => d -> (Version -> IO ()) -> IRef Version -> IRef Version -> IO ()
walkUp store onVersion topRef bottomRef
  | bottomRef == topRef  = return ()
  | otherwise            = do
    version <- IRef.getIRef store $ bottomRef
    onVersion version
    maybe (fail "Invalid path given, hit top") (walkUp store onVersion topRef) $
      versionParent version

-- We can't directly walkDown (we don't have references pointing
-- downwards... But we can generate a list of versions by walking up
-- and accumulating a reverse list)
versionsBetween :: Store d => d -> IRef Version -> IRef Version -> IO [Version]
versionsBetween store topRef bottomRef = accumulateWalkUp [] bottomRef
  where
    accumulateWalkUp vs curRef
      | topRef == curRef  = return vs
      | otherwise         = do
        version <- IRef.getIRef store $ curRef
        maybe (fail "Invalid path given, hit top") (accumulateWalkUp (version:vs)) $
          versionParent version

-- Implement in terms of versionsBetween
walkDown :: Store d => d -> (Version -> IO ()) -> IRef Version -> IRef Version -> IO ()
walkDown store onVersion topRef bottomRef =
  mapM_ onVersion =<< versionsBetween store topRef bottomRef

moveView :: Store d => ViewRef d -> IRef Version -> IO ()
moveView viewRef versionIRef = do
  let viewStoreRef = IRef.fromIRef store . viewIRef $ viewRef
  View viewVersionRef <- IRef.get viewStoreRef
  mraRef <- mostRecentAncestor store viewVersionRef versionIRef
  walkUp store applyBackward mraRef viewVersionRef
  walkDown store applyForward mraRef versionIRef
  IRef.set viewStoreRef $ View versionIRef
  where
    store = viewRefStore viewRef
    applyForward = apply newValue
    applyBackward = apply oldValue
    apply changeDir version = applyChanges viewRef changeDir . versionChanges $ version

singleChangeOnView :: Store d => ViewRef d -> ObjectKey -> Maybe ObjectValue -> IO ()
singleChangeOnView viewRef key value =
  makeVersionOnView viewRef . (: []) =<<
  makeChange viewRef key value

instance Store d => Store (ViewRef d) where
  lookupBS viewRef objKey = IRef.lookupBS store key
    where
      store = viewRefStore viewRef
      key = viewKey viewRef objKey
  insertBS viewRef key value = singleChangeOnView viewRef key (Just value)
  deleteBS viewRef key       = singleChangeOnView viewRef key Nothing
