{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- base
import Data.IORef (modifyIORef, readIORef, writeIORef, newIORef)
import Control.Monad (when, forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (async, wait)
-- diagrams
import Diagrams.Prelude hiding (view)
-- diagrams-gtk
import Diagrams.Backend.Gtk (toGtkCoords, renderToGtk, defaultRender)
import Diagrams.Backend.Cairo (Cairo, B)
-- gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk
-- cairo
import Graphics.Rendering.Cairo (liftIO)
-- text
import Data.Text (Text)
-- safe
import Safe (headMay)
-- time
import Data.Time
-- palette
import Data.Colour.Palette.ColorSet

type NormalDiagram = Diagram B

type GenericDiagram a = QDiagram Cairo V2 Double a

type SelectableDiagram = GenericDiagram [Text]


addTimer :: Int -> IO () -> IO ()
addTimer delay io = forever $ do
    a <- async io
    threadDelay delay
    wait a

data Model = Model
    { clockCount :: Int
    , triangleClickCount :: Int
    , squareClickCount :: Int
    }

initialModel :: Model
initialModel = Model 0 0 0

fullHDRect :: NormalDiagram
fullHDRect = rect screenWidth screenHeight # fc white

screenWidth :: Num a => a
screenWidth = 800
screenHeight :: Num a => a
screenHeight = 600

view :: Model -> SelectableDiagram
view Model{..} = toGtkCoords $ mconcat
    [ scale 50 $ center $ vsep 1
        [ value [] $ text ("Clock count: " <> show clockCount) <> phantom (rect 10 1 :: NormalDiagram)
        , value [] $ text ("Triangle click count: " <> show triangleClickCount) <> phantom (rect 10 1 :: NormalDiagram)
        , value [] $ text ("Square click count: " <> show squareClickCount) <> phantom (rect 10 1 :: NormalDiagram)
        , hsep 1
            [ triangle 1 # fc red # value ["triangle"]
            , rect 1 1 # fc blue # value ["square"]
            ]
        ]
    , sized (mkHeight screenHeight) $ center $ vcat $ replicate triangleClickCount $ hcat $ replicate triangleClickCount $ sampleCircle # fc (d3Colors2 Dark triangleClickCount) # value []
    , value [] $ fullHDRect
    ]

 where sampleText :: NormalDiagram
       sampleText =  text "a" <> phantom (rect 1 1 :: NormalDiagram)
       sampleTri :: NormalDiagram
       sampleTri = triangle 1
       sampleCircle :: NormalDiagram
       sampleCircle = circle 1

updateWithClick :: Text -> Model -> Model
updateWithClick "triangle" Model{..} = Model clockCount (triangleClickCount + 1) squareClickCount
updateWithClick "square" Model{..}   = Model clockCount triangleClickCount (squareClickCount + 1)
updateWithClick _ model              = model

updateWithTimer :: Model -> Model
updateWithTimer Model{..} = Model (clockCount + 1) triangleClickCount squareClickCount

updateWithKeyPress :: Text -> Model -> Model
updateWithKeyPress _ model   = model

main :: IO ()
main = do
    -- 編集の初期化
    vModel <- newIORef initialModel
    vRender <- newIORef $ view initialModel
    -- initialize GTK
    _ <- Gtk.initGUI
    window <- Gtk.windowNew
    canvas <- Gtk.drawingAreaNew
    Gtk.set window
        [ Gtk.containerChild := canvas
        , Gtk.windowDefaultWidth   := screenWidth
        , Gtk.windowDefaultHeight  := screenHeight
        ]

--     Gtk.windowFullscreen window
    Gtk.widgetShowAll window

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
            selectableDia <- readIORef vRender
            let mClickedObj = headMay $ reverse $ sample selectableDia $ p2 pos
            mapM_ (modifyIORef vModel . updateWithClick) mClickedObj
            Gtk.widgetQueueDraw window
    canvas `Gtk.on` Gtk.exposeEvent $ Gtk.tryEvent $ do
        drawin <- Gtk.eventWindow
        liftIO $ do
            model <- readIORef vModel
            let selectableDia = view model
--             defaultRender canvas $ clearValue $ view model
            renderToGtk drawin $ clearValue $ view model
            writeIORef vRender selectableDia

    forkIO $ addTimer 1000000 $ do
        modifyIORef vModel $ updateWithTimer
        Gtk.widgetQueueDraw window
    Gtk.mainGUI

measuringTime :: IO () -> IO ()
measuringTime io = do
     t1 <- getCurrentTime
     io
     t2 <- getCurrentTime
     let diffTime = diffUTCTime t2 t1
     putStrLn $ show diffTime
