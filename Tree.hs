{-# OPTIONS -O2 -Wall #-}

module Tree(Tree(..)) where

import Control.Monad(liftM2)
import Ref(DBRef)
import Data.Binary(Binary(..))

data Tree a = Node {
  nodeValueRef :: DBRef a,
  nodeChildrenRefs :: [DBRef (Tree a)]
  }

instance Binary (Tree a) where
  put (Node value children) = put value >> put children
  get = liftM2 Node get get
