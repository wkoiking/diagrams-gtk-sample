{-# LANGUAGE OverloadedStrings #-}

module Main where

-- base
import Data.IORef (modifyIORef, readIORef, newIORef)
import Control.Monad (when)
-- diagrams
import Diagrams.Prelude hiding (view)
-- diagrams-gtk
import Diagrams.Backend.Gtk (toGtkCoords, renderToGtk)
import Diagrams.Backend.Cairo (Cairo)
-- gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk
-- cairo
import Graphics.Rendering.Cairo (liftIO)
-- text
import Data.Text (Text)
-- safe
import Safe (headMay)

data Model = Model

initialModel :: Model
initialModel = undefined

view :: Model -> QDiagram Cairo V2 Double [Text]
view model = toGtkCoords $ undefined

updateWithClick :: Text -> Model -> Model
updateWithClick clickedObj model = undefined

updateWithKeyPress :: Text -> Model -> Model
updateWithKeyPress pressedKey model = undefined

main :: IO ()
main = do
    -- initialize GTK
    Gtk.initGUI
    window <- Gtk.windowNew
    canvas <- Gtk.drawingAreaNew
    Gtk.set window [ Gtk.containerChild := canvas ]
    Gtk.windowFullscreen window
    Gtk.widgetShowAll window

    -- initial model
    vModel <- newIORef initialModel

    -- event handlers
    window `Gtk.on` Gtk.deleteEvent $ Gtk.tryEvent $ liftIO Gtk.mainQuit
    window `Gtk.on` Gtk.keyPressEvent $ Gtk.tryEvent $ do
        key <- Gtk.eventKeyName
        liftIO $ do
            when (key == "q") Gtk.mainQuit -- quit application when "q" is pressed
            modifyIORef vModel $ updateWithKeyPress key
            Gtk.widgetQueueDraw window
    canvas `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do
        pos  <- Gtk.eventCoordinates
        liftIO $ do
            model <- readIORef vModel
            let mClickedObj = headMay $ reverse $ sample (view model) (p2 pos)
            mapM_ (modifyIORef vModel . updateWithClick) mClickedObj
            Gtk.widgetQueueDraw window
    canvas `Gtk.on` Gtk.exposeEvent $ Gtk.tryEvent $ do
        drawin <- Gtk.eventWindow
        liftIO $ do
            model <- readIORef vModel
            renderToGtk drawin $ clearValue $ view model
    Gtk.mainGUI
