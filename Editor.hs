{-# OPTIONS -O2 -Wall #-}

module Main (main) where

import qualified Db
import qualified ObjectStore
import ObjectStore(ObjectStore, IRef)
import ByteStringUtils(decodeS)
import qualified Data.ByteString.Char8 as SBS8
import Control.Monad(liftM2)
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import Graphics.UI.VtyWidgets.Run(runWidgetLoop)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)

main :: IO ()
main = do
  db <- Db.open "/tmp/db.db"
  Just keysBS <- Db.lookup db (SBS8.pack "root")
  let irefs = decodeS keysBS :: [IRef String]
  makeTextEdits <- mapM (makeTextEditMaker db) irefs
  gridModelR <- newIORef Grid.initModel
  runWidgetLoop . const $ makeWidget gridModelR makeTextEdits

makeWidget :: IORef Grid.Model -> [IO (Widget (IO ()))] -> IO (Widget (IO ()))
makeWidget gridModelR makeTextEdits = do
  textEdits <- sequence makeTextEdits
  let items = map (return . (,) True) textEdits
  Grid.make (writeIORef gridModelR) items `fmap`
    readIORef gridModelR

makeTextEditMaker :: ObjectStore -> IRef String -> IO (IO (Widget (IO ())))
makeTextEditMaker db iref = do
  let deref = ObjectStore.lookup db iref
  text <- deref
  cursorR <- newIORef (length text)
  return $ do
    textEditModel <- liftM2 TextEdit.Model (readIORef cursorR) deref
    return . fmap (handleTextEditModel cursorR) $
      TextEdit.make 1 TextEdit.defaultAttr TextEdit.editingAttr textEditModel
  where
    handleTextEditModel cursorR (TextEdit.Model cursor text) = do
      writeIORef cursorR cursor
      ObjectStore.set db iref text
