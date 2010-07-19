{-# OPTIONS -O2 -Wall #-}
module Data.Rev.Version
    (Version(..),
     makeInitialVersion, newVersion, mostRecentAncestor,
     walkUp, walkDown, versionsBetween)
where

import Control.Monad(liftM, liftM3)
import Data.Binary(Binary(..))
import Data.IRef(IRef)
import Data.Transaction(Transaction)
import qualified Data.Transaction as Transaction
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

makeInitialVersion :: Monad m => Transaction t m (IRef Version)
makeInitialVersion = Transaction.newIRef $ Version 0 Nothing []

newVersion :: Monad m => IRef Version -> [Change] -> Transaction t m (IRef Version)
newVersion versionIRef newChanges = do
  parentDepth <- depth `liftM` Transaction.readIRef versionIRef
  Transaction.newIRef $ Version (parentDepth+1) (Just versionIRef) newChanges

mostRecentAncestor :: Monad m => IRef Version -> IRef Version -> Transaction t m (IRef Version)
mostRecentAncestor = mra
  where
    mra aIRef bIRef
      | aIRef == bIRef  = return aIRef
      | otherwise       = do
        a <- Transaction.readIRef aIRef
        b <- Transaction.readIRef bIRef
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

walkUp :: Monad m => (Version -> Transaction t m ()) -> IRef Version -> IRef Version -> Transaction t m ()
walkUp onVersion topRef bottomRef
  | bottomRef == topRef  = return ()
  | otherwise            = do
    version <- Transaction.readIRef bottomRef
    onVersion version
    maybe (fail "Invalid path given, hit top") (walkUp onVersion topRef) $
      parent version

-- We can't directly walkDown (we don't have references pointing
-- downwards... But we can generate a list of versions by walking up
-- and accumulating a reverse list)
versionsBetween :: Monad m => IRef Version -> IRef Version -> Transaction t m [Version]
versionsBetween topRef bottomRef = accumulateWalkUp [] bottomRef
  where
    accumulateWalkUp vs curRef
      | topRef == curRef  = return vs
      | otherwise         = do
        version <- Transaction.readIRef curRef
        maybe (fail "Invalid path given, hit top") (accumulateWalkUp (version:vs)) $
          parent version

-- Implement in terms of versionsBetween
walkDown :: Monad m => (Version -> Transaction t m ()) -> IRef Version -> IRef Version -> Transaction t m ()
walkDown onVersion topRef bottomRef =
  mapM_ onVersion =<< versionsBetween topRef bottomRef
