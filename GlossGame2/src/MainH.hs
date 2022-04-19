module MainH where
import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Space (Action)

class PictureClass a where
    toPicture :: a -> Picture

instance PictureClass World where
    toPicture (World l) = Pictures $ map objToPict l

data ActionR = MouseAct Float Float | KeyAct String | DefaultAct

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

initObjList :: World
initObjList= World [ObjType {coord = (0,0), imageType = "Red"}, ObjType {coord = (1,1), imageType = "Black"}]

drawingFunc :: (PictureClass a) => a -> Picture
drawingFunc = toPicture

eventToAction :: Event -> ActionR
eventToAction (EventKey (SpecialKey KeyUp) Down _ _)  = KeyAct "KeyUp"
eventToAction (EventKey (SpecialKey KeyDown) Down _ _) = KeyAct "KeyDown"
eventToAction (EventKey (SpecialKey KeyRight) Down _ _)  = KeyAct "KeyRight"
eventToAction (EventKey (SpecialKey KeyLeft) Down _ _) = KeyAct "KeyLeft"
eventToAction (EventKey (MouseButton LeftButton) Down _ (x', y')) = MouseAct x' y'
eventToAction _ = DefaultAct

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) x = x
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) x = x
inputHandler (EventKey (SpecialKey KeyRight) Down _ _)  x = x
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) x = x
inputHandler (EventKey (MouseButton LeftButton) Down _ (x', y')) (World x) = World $ defaultObj (round x') (round y'):x
inputHandler _ w = w

updateFunc :: Float -> World -> World
updateFunc _ l = l



