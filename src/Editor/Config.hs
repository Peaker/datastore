{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    quitKey, undoKey, rootKey, appendChildKey, delChildKey, setViewRootKey, cutKey, pasteKey)
where

import qualified Graphics.Vty as Vty

quitKey = ([Vty.MCtrl], Vty.KASCII 'q')
undoKey = ([Vty.MCtrl], Vty.KASCII 'z')
rootKey = ([Vty.MCtrl], Vty.KASCII 'r')
appendChildKey = ([Vty.MCtrl], Vty.KASCII 'n')
delChildKey = ([Vty.MCtrl], Vty.KASCII 'o')
setViewRootKey = ([Vty.MCtrl], Vty.KASCII 'g')
pasteKey = ([Vty.MCtrl], Vty.KASCII 'v')
cutKey = ([Vty.MCtrl], Vty.KASCII 'x')
