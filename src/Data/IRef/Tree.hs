{-# LANGUAGE TypeOperators, TemplateHaskell #-}

module Data.IRef.Tree
    (Tree(..), nodeValue, nodeChildrenRefs)
where

import Data.Binary(Binary(..))
import Data.Binary.Utils(get2, put2)
import Data.IRef(IRef)
import Data.Record.Label((:->), mkLabels, label)

data Tree a = Node {
  _nodeValue :: a,
  _nodeChildrenRefs :: [IRef (Tree a)]
  }
  deriving (Show, Read, Eq, Ord)
$(mkLabels [''Tree])
nodeValue :: Tree a :-> a
nodeChildrenRefs :: Tree a :-> [IRef (Tree a)]

instance Binary a => Binary (Tree a) where
  put (Node value children) = put2 value children
  get = get2 Node
