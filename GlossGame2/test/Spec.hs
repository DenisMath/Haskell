import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Data.List
--import Graphics.Gloss.Interface.Pure.Game 
import Table_
import Space
import MainH
  

main :: IO ()
main = --do
    -- wall <- Picture (Circle 80) -- loads wall image
    play
        windowDisplay
        white
        20
        initObjList
        drawingFunc
        inputHandler
        updateFunc



