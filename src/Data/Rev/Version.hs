{-# OPTIONS -O2 -Wall #-}
module Data.Rev.Version
    (Version(..),
     makeInitialVersion, newVersion, mostRecentAncestor,
     walkUp, walkDown, versionsBetween)
where

import Control.Monad(liftM3)
import Data.Binary(Binary(..))
import Data.IRef(IRef)
import qualified Data.Store as Store
import Data.Store(Store)
import Data.Rev.Change(Change)

data Version = Version {
  depth :: Int,
  parent :: Maybe (IRef Version),
  changes :: [Change]
  }
  deriving (Eq, Ord, Read, Show)
instance Binary Version where
  get = liftM3 Version get get get
  put (Version d p c) = put d >> put p >> put c

makeInitialVersion :: Store d => d -> IO (IRef Version)
makeInitialVersion store = Store.newIRef store $ Version 0 Nothing []

newVersion :: Store d => d -> IRef Version -> [Change] -> IO (IRef Version)
newVersion store versionIRef newChanges = do
  parentDepth <- depth `fmap` Store.getIRef store versionIRef
  Store.newIRef store (Version (parentDepth+1) (Just versionIRef) newChanges)

mostRecentAncestor :: Store d => d -> IRef Version -> IRef Version -> IO (IRef Version)
mostRecentAncestor store = mra
  where
    mra aIRef bIRef
      | aIRef == bIRef  = return aIRef
      | otherwise       = do
        a <- Store.getIRef store aIRef
        b <- Store.getIRef store bIRef
        climb aIRef a bIRef b
    climb
      aIRef (Version aDepth aMbParentRef _aChanges)
      bIRef (Version bDepth bMbParentRef _bChanges)
      | aDepth < bDepth   =      mra aIRef =<< nonZeroDepth bMbParentRef
      | aDepth > bDepth   = flip mra bIRef =<< nonZeroDepth aMbParentRef
      | aDepth == 0 &&
        bDepth == 0       = fail "Two versions without common ancestor given"
      | otherwise         = do -- aDepth == bDepth
        aParentRef <- nonZeroDepth aMbParentRef
        bParentRef <- nonZeroDepth bMbParentRef
        mra aParentRef bParentRef
    nonZeroDepth = maybe (fail "Non-0 depth must have a parent") return

walkUp :: Store d => d -> (Version -> IO ()) -> IRef Version -> IRef Version -> IO ()
walkUp store onVersion topRef bottomRef
  | bottomRef == topRef  = return ()
  | otherwise            = do
    version <- Store.getIRef store bottomRef
    onVersion version
    maybe (fail "Invalid path given, hit top") (walkUp store onVersion topRef) $
      parent version

-- We can't directly walkDown (we don't have references pointing
-- downwards... But we can generate a list of versions by walking up
-- and accumulating a reverse list)
versionsBetween :: Store d => d -> IRef Version -> IRef Version -> IO [Version]
versionsBetween store topRef bottomRef = accumulateWalkUp [] bottomRef
  where
    accumulateWalkUp vs curRef
      | topRef == curRef  = return vs
      | otherwise         = do
        version <- Store.getIRef store curRef
        maybe (fail "Invalid path given, hit top") (accumulateWalkUp (version:vs)) $
          parent version

-- Implement in terms of versionsBetween
walkDown :: Store d => d -> (Version -> IO ()) -> IRef Version -> IRef Version -> IO ()
walkDown store onVersion topRef bottomRef =
  mapM_ onVersion =<< versionsBetween store topRef bottomRef
