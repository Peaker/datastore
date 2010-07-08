{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    quitKey, undoKey, rootKey, appendChildKey, delChildKey,
    setFocalPointKey, cutKey, pasteKey,
    makeBranchKey, delBranchKey)
where

import qualified Graphics.Vty as Vty

quitKey = ([Vty.MCtrl], Vty.KASCII 'q')
undoKey = ([Vty.MCtrl], Vty.KASCII 'z')
rootKey = ([Vty.MCtrl], Vty.KASCII 'r')
appendChildKey = ([Vty.MCtrl], Vty.KASCII 'n')
delChildKey = ([Vty.MCtrl], Vty.KASCII 'o')
setFocalPointKey = ([Vty.MCtrl], Vty.KASCII 'g')
pasteKey = ([Vty.MCtrl], Vty.KASCII 'v')
cutKey = ([Vty.MCtrl], Vty.KASCII 'x')
makeBranchKey = ([Vty.MCtrl], Vty.KASCII 's')
delBranchKey = ([Vty.MCtrl], Vty.KASCII 'o')
