{-# LANGUAGE TypeOperators, TemplateHaskell #-}

module Data.IRef.Tree
    (Tree(..), nodeValueRef, nodeChildrenRefs)
where

import Control.Monad(liftM2)
import Data.Binary(Binary(..))
import Data.IRef(IRef)
import Data.Record.Label((:->), mkLabels, label)

data Tree a = Node {
  _nodeValueRef :: IRef a,
  _nodeChildrenRefs :: [IRef (Tree a)]
  }
$(mkLabels [''Tree])
nodeValueRef :: Tree a :-> IRef a
nodeChildrenRefs :: Tree a :-> [IRef (Tree a)]

instance Binary (Tree a) where
  put (Node value children) = put value >> put children
  get = liftM2 Node get get
