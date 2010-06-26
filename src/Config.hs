{-# OPTIONS -fno-warn-missing-signatures #-}

module Config(quitKey, rootKey, appendChildKey, delChildKey, setViewRootKey) where

import qualified Graphics.Vty as Vty

quitKey = ([Vty.MCtrl], Vty.KASCII 'q')
rootKey = ([Vty.MCtrl], Vty.KASCII 'r')
appendChildKey = ([Vty.MCtrl], Vty.KASCII 'n')
delChildKey = ([Vty.MCtrl], Vty.KASCII 'o')
setViewRootKey = ([Vty.MCtrl], Vty.KASCII 'g')
